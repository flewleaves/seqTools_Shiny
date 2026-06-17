# python/r_bridge.py
"""R 计算引擎桥接（持久进程版）"""
import json
import os
import re
import time
import random
import threading
import subprocess
import tempfile
import shutil
import platform


class RBridge:
    def __init__(self, r_source_dir: str = None):
        if r_source_dir is None:
            script_dir = os.path.dirname(os.path.abspath(__file__))
            r_source_dir = os.path.join(script_dir, "..", "R")
        self.r_source_dir = os.path.abspath(r_source_dir)
        self.rscript_path = self._find_rscript()
        # 固定目录，避免重启丢失状态
        self._state_dir = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "..", "cache", "ai_state"
        )
        os.makedirs(self._state_dir, exist_ok=True)
        self._state_file = os.path.join(self._state_dir, "project_state.rds")
        self._state_summary_file = os.path.join(self._state_dir, "project_summary.json")
        self._state_version = 0
        self._init_r_environment()

    def _find_rscript(self) -> str:
        """自动查找 Rscript 路径（便携版优先）"""
        system = platform.system()
        project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

        # ---- 便携版优先：项目目录下的 R-Portable ----
        portable_candidates = []
        if system == "Windows":
            portable_candidates = [
                os.path.join(project_root, "R-Portable", "bin", "Rscript.exe"),
                os.path.join(project_root, "R-Portable", "Rscript.exe"),
                os.path.join(project_root, "..", "R-Portable", "bin", "Rscript.exe"),
            ]
        for candidate in portable_candidates:
            if os.path.exists(candidate):
                return candidate

        if system == "Windows":
            # Windows: 扫描常见安装路径（含便携版/自定义路径）
            for version in ["4.4.0", "4.4.1", "4.3.3", "4.3.2", "4.3.1", "4.3.0", "4.2.3", "4.2.2", "4.2.1", "4.2.0"]:
                for prefix in [r"D:\RStudioResource", r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
                    # 支持 R-x.x.x 和 直接 R4.3 两种目录结构
                    for dirname in [f"R-{version}", f"R{version.replace('.', '')[:3]}"]:
                        candidate = os.path.join(prefix, dirname, "bin", "Rscript.exe")
                        if os.path.exists(candidate):
                            return candidate
                    # 也查 prefix 直下（如 D:\RStudioResource\R4.3）
                    if os.path.exists(os.path.join(prefix, "bin", "Rscript.exe")):
                        return os.path.join(prefix, "bin", "Rscript.exe")

            # 尝试注册表
            try:
                import winreg
                with winreg.OpenKey(winreg.HKEY_LOCAL_MACHINE, r"SOFTWARE\R-core\R") as key:
                    install_path, _ = winreg.QueryValueEx(key, "InstallPath")
                    candidate = os.path.join(install_path, "bin", "Rscript.exe")
                    if os.path.exists(candidate):
                        return candidate
            except Exception:
                pass

        elif system == "Darwin":  # macOS
            for path in ["/usr/local/bin/Rscript", "/opt/homebrew/bin/Rscript", "/usr/bin/Rscript"]:
                if os.path.exists(path):
                    return path

        # 通用：PATH 中查找
        rscript = shutil.which("Rscript")
        if rscript:
            return rscript

        raise RuntimeError(
            "找不到 Rscript。请确保 R 已安装。\n"
            "常见路径: C:/Program Files/R/R-4.x.x/bin/Rscript.exe (Windows)\n"
            "          /usr/local/bin/Rscript (macOS/Linux)"
        )

    def _get_r_user_lib(self) -> str:
        """获取 R 用户库路径（非系统库，即包实际安装的位置）"""
        try:
            result = subprocess.run(
                [self.rscript_path, "-e", "cat(.libPaths(), sep=';')"],
                capture_output=True, text=True, timeout=30
            )
            libs = result.stdout.strip().split(";")
            libs = [l.strip() for l in libs if l.strip()]
            # 排除 R 系统库，取第一个非 Program Files 的路径（即用户库）
            for lib in libs:
                if "Program Files" not in lib and os.path.isdir(lib):
                    return lib.replace(os.sep, "/")
            if libs and os.path.isdir(libs[0]):
                return libs[0].replace(os.sep, "/")
        except Exception:
            pass

        # 兜底：常见 Windows 用户库路径
        username = os.getenv("USERNAME", "")
        for v in ["4.4", "4.3"]:
            for base in [
                f"C:/Users/{username}/AppData/Local/R/win-library/{v}",
                os.path.expanduser(f"~/R/win-library/{v}"),
                os.path.expanduser(f"~/AppData/Local/R/win-library/{v}"),
            ]:
                if os.path.isdir(base):
                    return base.replace(os.sep, "/")
        return ""

    def _r_path_vector(self, *paths: str) -> str:
        """把路径列表转为 R 的 c("p1", "p2") 字符串，过滤空路径"""
        valid = [p for p in paths if p]
        if not valid:
            return ""
        items = ", ".join(f'"{p}"' for p in valid)
        return items

    def _init_r_environment(self):
        """初始化：预加载核心文件，包含 RStudio 自定义库路径"""
        # 扫描项目目录及上级，自动发现 R 包库（便携版或自定义 R4.3 等）
        extra_libs = []
        for root in [os.path.dirname(self.r_source_dir),
                     os.path.join(os.path.dirname(self.r_source_dir), "..")]:
            root = os.path.abspath(root)
            if not os.path.isdir(root):
                continue
            for entry in os.listdir(root):
                full = os.path.join(root, entry)
                if not os.path.isdir(full):
                    continue
                # R-Portable：库在 library/ 子目录
                if entry.startswith("R-Portable"):
                    lib = os.path.join(full, "library")
                    if os.path.isdir(lib):
                        extra_libs.append(lib.replace(os.sep, "/"))
                # 自定义 R4.3/R4.4：目录本身就是库（含 Seurat 等子目录）
                elif entry.startswith("R4."):
                    subdirs = [d for d in os.listdir(full) if os.path.isdir(os.path.join(full, d))]
                    if any(d.startswith("Seurat") for d in subdirs):
                        extra_libs.append(full.replace(os.sep, "/"))
        lib_cmd = ""
        if extra_libs:
            lib_cmd = ".libPaths(c(" + ", ".join(f'"{p}"' for p in extra_libs) + ", .libPaths()))\n"
        self._init_script = lib_cmd + f'''
source("{os.path.join(self.r_source_dir, "core/project.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "core/import_registry.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "core/registry.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "core/engine.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "utils/tool_helpers.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "utils/ui_helpers.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "engines/normalize.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "engines/deg.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "engines/pca.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "engines/volcano.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "engines/gsea.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "engines/filter.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "utils/sc_helpers.R").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "engines/sc_engine.R").replace(os.sep, "/")}")
ImportRegistry$load_directory("{os.path.join(self.r_source_dir, "imports").replace(os.sep, "/")}")
ToolRegistry$load_directory("{os.path.join(self.r_source_dir, "tools").replace(os.sep, "/")}")
source("{os.path.join(self.r_source_dir, "skills/registry.R").replace(os.sep, "/")}")
SkillRegistry$load_directory("{os.path.join(self.r_source_dir, "skills").replace(os.sep, "/")}")
engine <- AnalysisEngine$new()
for (m in ImportRegistry$methods) {{
  engine$project$register_import(m$id, m$name, m$schema, m$run)
}}
'''

    def _start_r_proc(self):
        """启动持久 R 进程，加载环境（每次启动只做一次）"""
        self._r_proc = subprocess.Popen(
            [self.rscript_path, "--vanilla", "-"],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE, text=True, encoding="utf-8",
            errors="replace"
        )
        self._stderr_lines = []
        self._stderr_thread = threading.Thread(target=self._read_stderr, daemon=True)
        self._stderr_thread.start()

        tag = f'_R_INIT_OK_{random.randint(100000,999999)}_'
        # 单次 stdin.write：init + 就绪信号一起发
        self._r_proc.stdin.write(
            self._init_script +
            f'\ncat("\\n{tag}\\n"); flush.console()\n'
        )
        self._r_proc.stdin.flush()

        deadline = time.time() + 120
        while time.time() < deadline:
            line = self._r_proc.stdout.readline()
            if not line:
                if self._r_proc.poll() is not None:
                    raise RuntimeError(f"R 进程在初始化时退出: {self._r_proc.returncode}")
                time.sleep(0.05)
                continue
            if tag in line:
                return
        raise RuntimeError("R 进程初始化超时")

    def _read_stderr(self):
        """后台线程持续读 stderr"""
        while self._r_proc and self._r_proc.stderr:
            line = self._r_proc.stderr.readline()
            if not line:
                break
            self._stderr_lines.append(line)

    def _run_r(self, r_code: str, params: dict, state_file: str = None, summary_file: str = "") -> dict:
        """通过持久 R 进程执行工具代码"""
        if not (hasattr(self, '_r_proc') and self._r_proc and self._r_proc.poll() is None):
            self._start_r_proc()

        param_file = tempfile.mktemp(suffix=".json")
        with open(param_file, "w", encoding="utf-8") as f:
            json.dump(params, f, ensure_ascii=True)
        out_file = param_file + ".out"
        sf = (state_file or "").replace(os.sep, "/")
        tag = f'_R_DONE_{random.randint(100000,999999)}_'

        script_file = tempfile.mktemp(suffix=".R")
        with open(script_file, "w", encoding="utf-8") as f:
            f.write(r_code.format(
                param_file=param_file.replace(os.sep, "/"),
                out_file=out_file.replace(os.sep, "/"),
                summary_file=(summary_file or "").replace(os.sep, "/")
            ))

        try:
            smf = (summary_file or "").replace(os.sep, "/")
            # 只在 Shiny 推送了新状态时才重新 readRDS（检查文件 mtime）
            need_load = False
            if sf and os.path.exists(sf):
                mtime = os.path.getmtime(sf)
                last = getattr(self, '_state_mtime', 0)
                if mtime > last:
                    need_load = True
                    self._state_mtime = mtime
            wrapper_file = tempfile.mktemp(suffix=".R")
            with open(wrapper_file, "w", encoding="utf-8") as f:
                if need_load:
                    f.write(f'if (nzchar("{sf}") && file.exists("{sf}")) {{ tryCatch({{ saved <- readRDS("{sf}"); engine$project$deserialize(saved) }}, error = function(e){{}}) }};\n')
                f.write(f'source("{script_file.replace(os.sep, "/")}")\n')
                if sf:
                    # 轻量保存：读旧 results 文件（几 KB）合并，不碰 500MB 全量 RDS
                    rf = sf + ".results"
                    f.write(f'''if (nzchar("{rf}")) {{
  engine$project$meta$state_version <- (engine$project$meta$state_version %||% 0) + 1;
  if (file.exists("{rf}")) {{
    old <- tryCatch(readRDS("{rf}"), error = function(e) NULL);
    if (!is.null(old) && !is.null(old$results)) {{
      for (nm in names(old$results)) {{
        if (is.null(engine$project$results[[nm]])) engine$project$results[[nm]] <- old$results[[nm]]
      }}
    }}
  }}
  tmp <- paste0("{rf}", ".tmp");
  saveRDS(list(results=engine$project$results, meta=engine$project$meta, history=engine$project$history), tmp);
  file.rename(tmp, "{rf}");
}}\n''')
                    self._state_mtime = time.time()
                if smf:
                    f.write(f'tryCatch({{ jsonlite::write_json(engine$project$to_list(), "{smf}", auto_unbox = TRUE, pretty = FALSE) }}, error = function(e){{}});\n')
                f.write(f'cat("\\n{tag}\\n"); flush.console()\n')
            self._r_proc.stdin.write(f'source("{wrapper_file.replace(os.sep, "/")}")\n')
            self._r_proc.stdin.flush()

            # 等完成信号
            deadline = time.time() + 300
            while time.time() < deadline:
                line = self._r_proc.stdout.readline()
                if not line:
                    if self._r_proc.poll() is not None:
                        break
                    time.sleep(0.01)
                    continue
                if tag in line:
                    break
                if line.strip():
                    print(f"[R] {line.rstrip()}")

            # 收集 stderr
            time.sleep(0.2)  # 等 stderr 线程写完
            stderr_text = "".join(self._stderr_lines[-20:])  # 最近 20 行
            self._stderr_lines.clear()
            if stderr_text.strip():
                print(f"[R_ERR] {stderr_text[:500]}")
            m = re.search(r'Error[^:]*:\s*(.+)', stderr_text)
            if m:
                return {"success": False, "error": m.group(1).strip()[:300]}

            if not os.path.exists(out_file):
                if self._r_proc.poll() is not None:
                    self._start_r_proc()
                return {"success": False, "error": "R 未生成输出", "stderr": stderr_text[:300]}

            with open(out_file, "r", encoding="utf-8") as f:
                return json.load(f)

        except Exception as e:
            try: self._r_proc.kill()
            except Exception: pass
            self._r_proc = None
            return {"success": False, "error": str(e)}
        finally:
            for f in [param_file, script_file, wrapper_file, out_file]:
                if os.path.exists(f):
                    try: os.unlink(f)
                    except Exception: pass

    def _run_r_fresh(self, r_code: str, params: dict, state_file: str = None, summary_file: str = "") -> dict:
        """独立子进程 + 沙箱（execute_r 用）"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False, encoding="utf-8") as f:
            json.dump(params, f, ensure_ascii=True)
            param_file = f.name
        out_file = param_file + ".out"
        sf = (state_file or "").replace(os.sep, "/")
        state_load = ''
        if sf and os.path.exists(sf):
            state_load = f'if (nzchar("{sf}") && file.exists("{sf}")) {{ tryCatch({{ saved <- readRDS("{sf}"); engine$project$deserialize(saved) }}, error = function(e){{}}) }};'
        script = self._init_script + "\n" + state_load + "\n" + r_code.format(
            param_file=param_file.replace(os.sep, "/"),
            out_file=out_file.replace(os.sep, "/"),
            summary_file=(summary_file or "").replace(os.sep, "/")
        )
        with tempfile.NamedTemporaryFile(mode="w", suffix=".R", delete=False, encoding="utf-8") as f:
            f.write(script)
            sf_script = f.name
        try:
            result = subprocess.run(
                [self.rscript_path, "--vanilla", sf_script],
                capture_output=True, text=True, encoding="utf-8",
                errors="replace", timeout=300
            )
            if result.returncode != 0:
                return {"success": False, "error": (result.stderr or "未知错误")[:500]}
            if not os.path.exists(out_file):
                return {"success": False, "error": "R 未生成输出"}
            with open(out_file, "r", encoding="utf-8") as f:
                return json.load(f)
        except subprocess.TimeoutExpired:
            return {"success": False, "error": "执行超时"}
        finally:
            for f in [param_file, sf_script, out_file]:
                if os.path.exists(f):
                    try: os.unlink(f)
                    except Exception: pass
        """独立子进程执行 R 代码"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json",
                                         delete=False, encoding="utf-8") as f:
            json.dump(params, f, ensure_ascii=True)
            param_file = f.name
        out_file = param_file + ".out"
        sf = (state_file or "").replace(os.sep, "/")

        # 状态加载
        state_load = ''
        if sf and os.path.exists(sf):
            state_load = f'if (nzchar("{sf}") && file.exists("{sf}")) {{ tryCatch({{ saved <- readRDS("{sf}"); engine$project$deserialize(saved) }}, error = function(e){{}}) }};'

        script = self._init_script + "\n" + state_load + "\n" + r_code.format(
            param_file=param_file.replace(os.sep, "/"),
            out_file=out_file.replace(os.sep, "/"),
            summary_file=(summary_file or "").replace(os.sep, "/")
        )
        with tempfile.NamedTemporaryFile(mode="w", suffix=".R",
                                         delete=False, encoding="utf-8") as f:
            f.write(script)
            script_file = f.name
        try:
            result = subprocess.run(
                [self.rscript_path, "--vanilla", script_file],
                capture_output=True, text=True, encoding="utf-8",
                errors="replace", timeout=300
            )
            if result.returncode != 0:
                err = (result.stderr or "未知错误")[:500]
                print(f"[R_ERR] {err}")
                return {"success": False, "error": err}
            if not os.path.exists(out_file):
                return {"success": False, "error": "R 未生成输出"}
            with open(out_file, "r", encoding="utf-8") as f:
                return json.load(f)
        except subprocess.TimeoutExpired:
            return {"success": False, "error": "执行超时"}
        finally:
            for f in [param_file, script_file, out_file]:
                if os.path.exists(f):
                    try: os.unlink(f)
                    except Exception: pass

    def run_tool(self, tool_id: str, inputs: dict) -> dict:
        summary_file = self._state_summary_file.replace(os.sep, "/")
        r_code = '''
tryCatch({{
  args <- jsonlite::read_json("{param_file}", simplifyVector = TRUE)
  result <- engine$run(args$tool_id, args$inputs)
  if (isTRUE(result$success)) {{
    tryCatch({{
      st <- engine$project$to_list()
      jsonlite::write_json(st, "{summary_file}", auto_unbox = TRUE, pretty = FALSE)
    }}, error = function(e) {{}})
  }}
  # 剥掉不可序列化的对象（ggplot 等），只保留文本/数值
  clean <- function(x) {{
    if (inherits(x, "ggplot") || inherits(x, "Seurat") || is.environment(x)) "[复杂对象，请在 Shiny 查看]"
    else if (is.list(x)) lapply(x, clean)
    else x
  }}
  jsonlite::write_json(clean(result), "{out_file}", auto_unbox = TRUE, pretty = FALSE, force = TRUE)
}}, error = function(e) {{
  jsonlite::write_json(list(success = FALSE, error = conditionMessage(e)),
                       "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}})
'''
        return self._run_r(r_code, {"tool_id": tool_id, "inputs": inputs},
                          state_file=self._state_file,
                          summary_file=summary_file)

    def get_cached_state(self) -> dict:
        """读取缓存的轻量 JSON 摘要，无需启动 R 子进程（毫秒级）"""
        try:
            if os.path.exists(self._state_summary_file):
                with open(self._state_summary_file, "r", encoding="utf-8") as f:
                    summary = json.load(f)
                return {
                    "success": True,
                    "result": {
                        "data": summary,
                        "meta": {"timestamp": "", "cached": True}
                    },
                    "project_state": summary
                }
        except Exception as e:
            print(f"[get_cached_state] JSON read failed: {e}")
        # 回退到 R 子进程
        return self.run_tool("get_state", {})

    def reload_tools(self):
        """重启持久进程以加载新工具"""
        if hasattr(self, '_r_proc') and self._r_proc:
            try: self._r_proc.kill()
            except Exception: pass
            self._r_proc = None
        self._start_r_proc()

    def list_tools(self) -> list:
        r_code = '''
tryCatch({{
  tools <- engine$list_tools()
  jsonlite::write_json(list(success = TRUE, tools = tools), "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}}, error = function(e) {{
  jsonlite::write_json(list(success = FALSE, error = conditionMessage(e)),
                       "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}})
'''
        result = self._run_r(r_code, {})
        if not result.get('success'):
            print(f"[R_ERR] list_tools error: {result.get('error', 'unknown')}")
        return result.get("tools", []) if result.get("success") else []

    def _get_work_dir(self) -> str:
        """从配置文件读取工作目录，失败则返回默认值"""
        app_root = os.path.dirname(self.r_source_dir)
        config_path = os.path.join(app_root, "seqTools_config.ini")
        try:
            import configparser
            config = configparser.ConfigParser()
            for encoding in ["utf-8", "utf-8-sig", "gbk"]:
                try:
                    with open(config_path, "r", encoding=encoding) as f:
                        config.read_file(f)
                    break
                except Exception:
                    continue
            work_dir = config.get("system", "work_dir", fallback="")
            if work_dir and os.path.isdir(work_dir):
                return os.path.abspath(work_dir).replace(os.sep, "/")
        except Exception:
            pass
        # 默认工作目录
        default_ws = os.path.join(app_root, "workspace")
        if not os.path.isdir(default_ws):
            os.makedirs(default_ws, exist_ok=True)
        return os.path.abspath(default_ws).replace(os.sep, "/")

    def run_r_code(self, code: str) -> dict:
        """在沙箱中执行 R 代码。文件 I/O 限制在程序目录和工作目录内。"""
        # 将 R 代码写入临时文件，用 source() 执行（避免 eval(parse()) 的引号转义问题）
        with tempfile.NamedTemporaryFile(mode="w", suffix=".R",
                                         delete=False, encoding="utf-8") as f:
            f.write(code)
            user_script = f.name

        app_root = os.path.dirname(self.r_source_dir).replace(os.sep, "/")
        work_dir = self._get_work_dir()
        sandbox_file = os.path.join(self.r_source_dir, "utils", "sandbox.R").replace(os.sep, "/")
        code_file = user_script.replace(os.sep, "/")

        r_code = f'''
sys.source("{sandbox_file}", envir = environment())
sandbox_init(
  allowed_read  = c("{app_root}", "{work_dir}"),
  allowed_write = c("{work_dir}")
)
tryCatch({{
  source("{code_file}")
  result <- list(success = TRUE, output = "ok")
  jsonlite::write_json(result, "{{out_file}}", auto_unbox = TRUE, pretty = FALSE)
}}, error = function(e) {{
  jsonlite::write_json(list(success = FALSE, error = conditionMessage(e)),
                       "{{out_file}}", auto_unbox = TRUE, pretty = FALSE)
}})
'''
        # execute_r 的沙箱会覆写 base::file 等，必须走独立子进程，不能污染持久 R 进程
        result = self._run_r_fresh(r_code, {}, state_file=self._state_file)
        try:
            os.unlink(user_script)
        except Exception:
            pass
        return result

    def list_skills(self) -> list:
        """列出所有已注册的技能"""
        r_code = '''
tryCatch({{
  skills <- SkillRegistry$list_skills()
  jsonlite::write_json(list(success = TRUE, skills = skills), "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}}, error = function(e) {{
  jsonlite::write_json(list(success = FALSE, error = conditionMessage(e)),
                       "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}})
'''
        result = self._run_r(r_code, {})
        return result.get("skills", []) if result.get("success") else []

    def get_skill(self, skill_id: str) -> dict:
        """获取单个技能的完整定义（含步骤）"""
        r_code = '''
tryCatch({{
  args <- jsonlite::read_json("{param_file}", simplifyVector = TRUE)
  skill <- SkillRegistry$get(args$skill_id)
  if (is.null(skill)) {{
    jsonlite::write_json(list(success = FALSE, error = paste("Skill not found:", args$skill_id)),
                         "{out_file}", auto_unbox = TRUE, pretty = FALSE)
  }} else {{
    jsonlite::write_json(list(
      success = TRUE,
      skill = list(
        name = skill$name,
        display_name = skill$display_name %||% skill$name,
        description = skill$description %||% "",
        version = skill$version %||% "1.0.0",
        omics = skill$omics %||% list(),
        steps = lapply(skill$steps, function(st) list(
          id = st$id, tool = st$tool, params = st$params,
          description = st$description %||% st$tool
        ))
      )
    ), "{out_file}", auto_unbox = TRUE, pretty = FALSE)
  }}
}}, error = function(e) {{
  jsonlite::write_json(list(success = FALSE, error = conditionMessage(e)),
                       "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}})
'''
        result = self._run_r(r_code, {"skill_id": skill_id})
        return result.get("skill", {}) if result.get("success") else {}

    def run_skill(self, skill_id: str, overrides: dict) -> dict:
        """执行一个技能（所有步骤在同一 R 会话中顺序执行）"""
        r_code = '''
tryCatch({{
  args <- jsonlite::read_json("{param_file}", simplifyVector = TRUE)
  result <- SkillRegistry$execute(args$skill_id, args$overrides, engine$project)
  jsonlite::write_json(result, "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}}, error = function(e) {{
  jsonlite::write_json(list(success = FALSE, error = conditionMessage(e)),
                       "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}})
'''
        return self._run_r(r_code, {"skill_id": skill_id, "overrides": overrides}, state_file=self._state_file)

    def close(self) -> None:
        try:
            shutil.rmtree(self._state_dir, ignore_errors=True)
        except Exception:
            pass
