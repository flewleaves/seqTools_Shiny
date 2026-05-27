# python/r_bridge.py
"""R 计算引擎桥接（subprocess 版，无需 rpy2）"""
import json
import os
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
        self._state_dir = tempfile.mkdtemp(prefix="r_bridge_state_")
        self._state_file = os.path.join(self._state_dir, "project_state.rds")
        self._init_r_environment()

    def _find_rscript(self) -> str:
        """自动查找 Rscript 路径"""
        system = platform.system()

        if system == "Windows":
            # Windows: 扫描常见安装路径
            for version in ["4.4.0", "4.4.1", "4.3.3", "4.3.2", "4.3.1", "4.3.0", "4.2.3", "4.2.2", "4.2.1", "4.2.0"]:
                for prefix in [r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
                    candidate = os.path.join(prefix, f"R-{version}", "bin", "Rscript.exe")
                    if os.path.exists(candidate):
                        return candidate

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
            "常见路径: C:\Program Files\R\R-4.x.x\bin\Rscript.exe (Windows)\n"
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
        """初始化：预加载核心文件"""
        # 项目 R/lib 路径（相对脚本位置固定，不依赖 CWD）
        r_lib = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "..", "R", "lib"
        ).replace(os.sep, "/")
        r_user_lib = self._get_r_user_lib()
        # R4.3 备选库（包含 DESeq2/edgeR/limma 等）
        r4_lib = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), "..", "..", "R4.3"
        ).replace(os.sep, "/")
        if not os.path.isdir(r4_lib):
            r4_lib = "D:/RStudioResource/R4.3"
        if not os.path.isdir(r4_lib):
            r4_lib = ""
        self._init_script = f'''
.libPaths(c({self._r_path_vector(r_lib, r_user_lib, r4_lib)}, .libPaths()))
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
ImportRegistry$load_directory("{os.path.join(self.r_source_dir, "imports").replace(os.sep, "/")}")
ToolRegistry$load_directory("{os.path.join(self.r_source_dir, "tools").replace(os.sep, "/")}")
engine <- AnalysisEngine$new()
for (m in ImportRegistry$methods) {{
  engine$project$register_import(m$id, m$name, m$schema, m$run)
}}
'''

    def _run_r(self, r_code: str, params: dict, state_file: str = None) -> dict:
        """执行 R 代码，通过临时文件传递参数和接收结果"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json",
                                         delete=False, encoding="utf-8") as f:
            json.dump(params, f, ensure_ascii=True)
            param_file = f.name

        out_file = param_file + ".out"

        # 状态加载：在 tool 执行前恢复前次的 project 状态
        if state_file:
            sf = state_file.replace(os.sep, "/")
            state_preamble = f'''
state_file <- "{sf}"
if (file.exists(state_file)) {{
  tryCatch({{
    saved <- readRDS(state_file)
    engine$project$deserialize(saved)
  }}, error = function(e) {{}})
}}
'''
        else:
            state_preamble = "\nstate_file <- NULL\n"

        script = self._init_script + "\n" + state_preamble + r_code.format(
            param_file=param_file.replace(os.sep, "/"),
            out_file=out_file.replace(os.sep, "/")
        )

        with tempfile.NamedTemporaryFile(mode="w", suffix=".R",
                                         delete=False, encoding="utf-8") as f:
            f.write(script)
            script_file = f.name

        # 构建环境变量：追加 R_LIBS_USER，不覆盖
        env = os.environ.copy()
        r_lib = os.path.abspath("R/lib")
        if "R_LIBS_USER" in env and r_lib not in env["R_LIBS_USER"]:
            env["R_LIBS_USER"] = r_lib + os.pathsep + env["R_LIBS_USER"]
        else:
            env["R_LIBS_USER"] = r_lib

        try:
            result = subprocess.run(
                [self.rscript_path, "--vanilla", script_file],
                capture_output=True, text=True, encoding="utf-8",
                errors="replace", timeout=300, env=env
            )
            if result.returncode != 0 and result.stderr:
                print(f"[R_ERR] {result.stderr[:500]}")
            if result.returncode != 0:
                err = result.stderr.strip() if result.stderr else "未知 R 错误"
                return {"success": False, "error": err, "stdout": result.stdout}

            if not os.path.exists(out_file):
                return {"success": False, "error": "R 未生成输出文件"}

            with open(out_file, "r", encoding="utf-8") as f:
                return json.load(f)

        except subprocess.TimeoutExpired:
            return {"success": False, "error": "R 执行超时（300秒）"}
        except Exception as e:
            return {"success": False, "error": str(e)}
        finally:
            for f in [param_file, script_file, out_file]:
                if os.path.exists(f):
                    try:
                        os.unlink(f)
                    except Exception:
                        pass

    def run_tool(self, tool_id: str, inputs: dict) -> dict:
        r_code = '''
tryCatch({{
  args <- jsonlite::read_json("{param_file}", simplifyVector = TRUE)
  result <- engine$run(args$tool_id, args$inputs)
  if (isTRUE(result$success)) {{
    saveRDS(engine$project$serialize(), state_file)
  }}
  jsonlite::write_json(result, "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}}, error = function(e) {{
  jsonlite::write_json(list(success = FALSE, error = conditionMessage(e)),
                       "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}})
'''
        return self._run_r(r_code, {"tool_id": tool_id, "inputs": inputs}, state_file=self._state_file)

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

    def run_r_code(self, code: str) -> dict:
        """执行任意 R 代码（只读 — 可访问 engine$project 查看数据，但不保存修改）"""
        # 将 R 代码写入临时文件，用 source() 执行（避免 eval(parse()) 的引号转义问题）
        with tempfile.NamedTemporaryFile(mode="w", suffix=".R",
                                         delete=False, encoding="utf-8") as f:
            f.write(code)
            user_script = f.name

        code_file = user_script.replace(os.sep, "/")
        r_code = '''
tryCatch({
  source("CODE_FILE")
  result <- list(success = TRUE, output = "ok")
  jsonlite::write_json(result, "{out_file}", auto_unbox = TRUE, pretty = FALSE)
}, error = function(e) {
  jsonlite::write_json(list(success = FALSE, error = conditionMessage(e)),
                       "{out_file}", auto_unbox = TRUE, pretty = FALSE)
})
'''.replace("CODE_FILE", code_file)
        result = self._run_r(r_code, {}, state_file=self._state_file)
        try:
            os.unlink(user_script)
        except Exception:
            pass
        return result

    def close(self) -> None:
        try:
            shutil.rmtree(self._state_dir, ignore_errors=True)
        except Exception:
            pass
