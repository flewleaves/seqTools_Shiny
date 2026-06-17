# python/main.py
"""FastAPI main server (subprocess r_bridge version)"""
import asyncio
import sys

# Windows: force UTF-8 for stdout if possible (Python 3.7+)
try:
    sys.stdout.reconfigure(encoding='utf-8')
except Exception:
    pass

from fastapi import FastAPI, WebSocket, WebSocketDisconnect
from fastapi.responses import FileResponse, Response
from fastapi.middleware.cors import CORSMiddleware
import json
import os

from r_bridge import RBridge
from ai_client import AIClient

r_bridge = None
ai_client = None
ai_tools_schema = []

# ========== Lifespan: standard async generator (NO @asynccontextmanager) ==========
async def lifespan(app: FastAPI):
    global r_bridge, ai_client, ai_tools_schema

    # --- startup ---
    try:
        r_bridge = RBridge()
    except Exception as e:
        print(f"[WARN] R bridge init failed: {e}")
        r_bridge = None

    try:
        config = load_config()
        ai_key = config["AI"].get("key", "")
        if not ai_key:
            print("[WARN] [AI] key is empty. AI chat disabled. Edit seqTools_config.ini and restart.")
            ai_client = None
        else:
            ai_client = AIClient(
                api_key=ai_key,
                base_url=config["AI"].get("base_url", "https://api.deepseek.com"),
                model=config["AI"].get("model", "deepseek-chat")
            )
            print("[OK] AI client initialized")
    except ValueError as e:
        print(f"[WARN] AI client config error: {e}")
        ai_client = None
    except Exception as e:
        print(f"[WARN] AI client init failed: {e}")
        ai_client = None

    if r_bridge:
        try:
            tools = await asyncio.to_thread(r_bridge.list_tools)
            ai_tools_schema = build_tools_schema(tools)
            # list_tools：AI 可查看所有可用工具
            ai_tools_schema.append({
                "type": "function",
                "function": {
                    "name": "list_tools",
                    "description": "列出所有可用的分析工具（名称+说明），不确定工具名时先调这个。",
                    "parameters": {"type": "object", "properties": {}}
                }
            })
            # execute_r：仅数据探查，硬限制每会话 1 次
            ai_tools_schema.append({
                "type": "function",
                "function": {
                    "name": "execute_r",
                    "description": "【限1次/会话】只读 R 代码，仅用于查看行名/列名/表头。禁止跑分析、改数据、绕过工具失败。",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "code": {"type": "string", "description": "只读 R 代码"}
                        },
                        "required": ["code"]
                    }
                }
            })
            # 添加技能相关函数
            ai_tools_schema.extend([
                {
                    "type": "function",
                    "function": {
                        "name": "list_skills",
                        "description": "列出所有可用的分析技能（多步骤自动化工作流）",
                        "parameters": {"type": "object", "properties": {}}
                    }
                },
                {
                    "type": "function",
                    "function": {
                        "name": "get_skill_info",
                        "description": "获取某个技能的完整定义，包括所有步骤和可覆盖的参数。调用 run_skill 前先用此工具了解技能结构。",
                        "parameters": {
                            "type": "object",
                            "properties": {
                                "skill_id": {"type": "string", "description": "技能ID"}
                            },
                            "required": ["skill_id"]
                        }
                    }
                },
                {
                    "type": "function",
                    "function": {
                        "name": "run_skill",
                        "description": "执行一个技能（多步骤分析工作流）。优先使用技能而非逐个调用工具——技能更可靠、步骤更少、不易出错。覆盖参数格式: {\"step_id\": {\"param\": value}}，用于传入动态参数如contrast、deg_input",
                        "parameters": {
                            "type": "object",
                            "properties": {
                                "skill_id": {"type": "string", "description": "技能ID"},
                                "overrides_json": {"type": "string", "description": "JSON格式的参数覆盖", "default": "{}"}
                            },
                            "required": ["skill_id"]
                        }
                    }
                }
            ])
            print(f"[OK] Loaded {len(ai_tools_schema)} tools into schema (incl. execute_r + 3 skill fns)")
        except Exception as e:
            print(f"[WARN] Tool load failed: {e}")
            import traceback
            traceback.print_exc()
            ai_tools_schema = []
    else:
        ai_tools_schema = []

    yield  # startup / shutdown separator

    # --- shutdown ---
    if ai_client:
        await ai_client.close()
    if r_bridge:
        r_bridge.close()


app = FastAPI(lifespan=lifespan)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


def load_config():
    import configparser
    import os
    
    config = configparser.ConfigParser()
    
    # 程序运行目录 = main.py 的父目录（固定）
    APP_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    config_path = os.path.join(APP_ROOT, "seqTools_config.ini")
    
    for encoding in ["utf-8", "utf-8-sig", "gbk"]:
        try:
            with open(config_path, "r", encoding=encoding) as f:
                config.read_file(f)
            break
        except Exception:
            continue
    
    def get(section, key, fallback):
        val = config.get(section, key, fallback=fallback)
        return val.strip() if val and val.strip() else fallback
    
    base_url = get("AI", "base_url", "")
    # 最终兜底：如果配置文件被外部搞成空的了，给个默认值让服务能启动
    if not base_url:
        provider = get("AI", "provider", "deepseek")
        base_url = {
            "deepseek": "https://api.deepseek.com",
            "openai":   "https://api.openai.com/v1",
            "aliyun":   "https://dashscope.aliyuncs.com/compatible-mode/v1",
        }.get(provider, "https://api.deepseek.com")
        print(f"[WARN] base_url empty in config, fallback to {base_url}")

    return {
        "AI": {
            "key":      get("AI", "key",      ""),
            "base_url": base_url,
            "model":    get("AI", "model",    "deepseek-chat")
        }
    }


def build_tools_schema(tools: list) -> list:
    schema = []
    for t in tools:
        props = {}
        required = []
        schema_def = t.get("schema", {})
        if not isinstance(schema_def, dict):
            schema_def = {}

        for pname, pdef in schema_def.items():
            prop = {"type": "string", "description": pdef.get("description", pname)}

            ptype = pdef.get("type", "string")
            if ptype in ["number", "float"]:
                prop["type"] = "number"
            elif ptype in ["integer", "int"]:
                prop["type"] = "integer"
            elif ptype == "boolean":
                prop["type"] = "boolean"
            elif ptype == "select":
                if pdef.get("multiple"):
                    prop["type"] = "array"
                    prop["items"] = {"type": "string"}
                choices = pdef.get("choices", [])
                if choices and len(choices) > 0:
                    if pdef.get("multiple"):
                        prop["items"]["enum"] = choices
                    else:
                        prop["enum"] = choices
                elif not pdef.get("multiple"):
                    prop["description"] = (prop.get("description", "") + "（选项动态加载）").strip()

            if pdef.get("min") is not None:
                prop["minimum"] = pdef["min"]
            if pdef.get("max") is not None:
                prop["maximum"] = pdef["max"]

            props[pname] = prop
            if pdef.get("required"):
                required.append(pname)

        # 用 t["id"] 作为 function name，AI 调用时发回 tool_id
        schema.append({
            "type": "function",
            "function": {
                "name": t.get("id", t.get("name", "unknown")),
                "description": f"{t.get('category', '未分类')} tool: {t.get('name', t.get('id', ''))}",
                "parameters": {
                    "type": "object",
                    "properties": props,
                    "required": required
                }
            }
        })

    return schema


SYSTEM_PROMPT = """你是 seqTools 助手。第一步必须调 get_state() 看数据。

工具（run_tool）和技能（run_skill）是两回事。不确定工具名时先调 list_tools() 查。

1. get_state() → list_tools() → run_tool()。
2. 不猜工具名、不猜基因名。失败 2 次停，原样报错。
3. 完成后说"完成"。execute_r 限 1 次。
4. 缺功能 → 参考 R/tools/_example_template.R.example 格式写新工具，贴代码给用户。
5. 中文，不超过 3 句。"""


@app.get("/health")
async def health_check():
    return {"status": "ok", "tools_loaded": len(ai_tools_schema)}


@app.post("/reload")
async def reload_tools_endpoint():
    global ai_tools_schema
    try:
        tools = await asyncio.to_thread(r_bridge.list_tools)
        ai_tools_schema = build_tools_schema(tools)
        return {"success": True, "tools_count": len(ai_tools_schema)}
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.post("/sync_project")
async def sync_project(data: dict):
    """Shiny 推送项目状态到 AI 后端（通过 RDS 文件路径，避免 JSON 破坏 matrix 类型）"""
    global r_bridge
    if r_bridge is None:
        return {"success": False, "error": "r_bridge not initialized"}
    try:
        rds_path = data.get("rds_path", "")
        if not rds_path or not os.path.exists(rds_path):
            return {"success": False, "error": f"rds_path not found: {rds_path}"}
        import shutil
        # 原子写入：先写到临时文件再 rename，防止 AI 并发读到半截文件
        tmp = r_bridge._state_file + ".tmp"
        shutil.copy2(rds_path, tmp)
        os.replace(tmp, r_bridge._state_file)
        # 同步轻量摘要 JSON（供 AI get_state 快速读取）
        summary_path = data.get("summary_path", "")
        if summary_path and os.path.exists(summary_path):
            tmp2 = r_bridge._state_summary_file + ".tmp"
            shutil.copy2(summary_path, tmp2)
            os.replace(tmp2, r_bridge._state_summary_file)
        # 记录版本号，供 pull_state 跳过无变化同步
        ver = data.get("version", 0)
        r_bridge._state_version = int(ver) if ver else 0
        return {"success": True}
    except Exception as e:
        return {"success": False, "error": str(e)}


@app.get("/state_version")
async def state_version():
    """返回当前状态版本号（轻量，供 Shiny 判断是否需要拉取全量 RDS）"""
    global r_bridge
    if r_bridge is None:
        return Response(content=b"0", media_type="text/plain")
    return Response(content=str(r_bridge._state_version).encode(),
                    media_type="text/plain")


@app.get("/pull_state")
async def pull_state():
    """AI 后端返回当前项目状态 RDS 文件，供 Shiny 拉取同步"""
    global r_bridge
    if r_bridge is None or not os.path.exists(r_bridge._state_file):
        return {"success": False, "error": "No state available"}
    return FileResponse(
        r_bridge._state_file,
        media_type="application/octet-stream",
        filename="project_state.rds"
    )


@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    await websocket.accept()
    conversation = []
    cancel_event = asyncio.Event()
    chat_task = None
    execute_r_count = 0   # 硬限制：每轮最多 1 次
    tool_call_count = 0   # 硬限制：每轮最多 8 次工具调用

    async def run_chat(prompt: str):
        """后台执行完整 AI 对话 + 工具调用循环，可被外部 cancel"""
        nonlocal conversation, execute_r_count, tool_call_count
        if ai_client is None:
            await websocket.send_json({
                "type": "error",
                "error": "AI 服务未初始化：请在 seqTools_config.ini 的 [AI] 节填写 key 后重启服务。"
            })
            return

        cancel_event.clear()
        tool_call_count = 0  # 每轮用户消息重置
        conversation.append({"role": "user", "content": prompt})
        print(f"[AI] Start processing: {prompt[:50]}...")
        await websocket.send_json({"type": "thinking"})

        tool_exec_result = None
        tool_exec_info = None

        async def on_text(text: str):
            if not cancel_event.is_set():
                await websocket.send_json({"type": "stream_text", "content": text})

        async def on_tool_call(phase: str, info: dict):
            nonlocal tool_exec_result, tool_exec_info
            if cancel_event.is_set():
                return
            if phase == "start":
                await websocket.send_json({"type": "tool_start", "tool": info["name"]})
            elif phase == "execute":
                nonlocal tool_call_count
                if tool_call_count >= 8:
                    tool_exec_result = {"success": False, "error": "已达本轮工具调用上限（8次）。请直接报告用户结果或询问下一步。"}
                    return
                tool_call_count += 1
                tool_exec_info = info
                tool_name = info["name"]
                tool_args = info["arguments"]
                await websocket.send_json({"type": "tool_executing", "tool": tool_name})
                try:
                    if tool_name == "reload_tools":
                        await asyncio.to_thread(r_bridge.reload_tools)
                        tools = await asyncio.to_thread(r_bridge.list_tools)
                        global ai_tools_schema
                        ai_tools_schema = build_tools_schema(tools)
                        # rebuild skill fns after reload
                        ai_tools_schema.extend([
                            {"type":"function","function":{"name":"list_skills","description":"列出所有可用的分析技能","parameters":{"type":"object","properties":{}}}},
                            {"type":"function","function":{"name":"get_skill_info","description":"获取技能详情和步骤","parameters":{"type":"object","properties":{"skill_id":{"type":"string","description":"技能ID"}},"required":["skill_id"]}}},
                            {"type":"function","function":{"name":"run_skill","description":"执行技能（多步骤工作流）。优先使用。覆盖参数: overrides_json={\"step_id\":{\"param\":value}}","parameters":{"type":"object","properties":{"skill_id":{"type":"string","description":"技能ID"},"overrides_json":{"type":"string","description":"JSON参数覆盖","default":"{}"}},"required":["skill_id"]}}}
                        ])
                        tool_exec_result = {"success": True, "tools": tools}
                    elif tool_name == "execute_r":
                        nonlocal execute_r_count
                        if execute_r_count >= 1:
                            tool_exec_result = {"success": False, "error": "execute_r 已达上限（每会话1次）。请用 run_tool 或直接报告用户。"}
                        else:
                            execute_r_count += 1
                            r_code = tool_args.get("code", "")
                            raw_result = await asyncio.to_thread(r_bridge.run_r_code, r_code)
                            reminder = "\n\n【⚠️ execute_r 提醒】仅用于数据探索。不要重复调用。"
                            if isinstance(raw_result, dict):
                                msg = raw_result.get("message", raw_result.get("error", ""))
                                raw_result["message"] = str(msg) + reminder
                            tool_exec_result = raw_result
                    elif tool_name == "list_skills":
                        skills = await asyncio.to_thread(r_bridge.list_skills)
                        tool_exec_result = {"success": True, "skills": skills, "count": len(skills)}
                    elif tool_name == "get_skill_info":
                        sid = tool_args.get("skill_id", "")
                        skill = await asyncio.to_thread(r_bridge.get_skill, sid)
                        tool_exec_result = {"success": True, "skill": skill} if skill else {"success": False, "error": f"Skill not found: {sid}"}
                    elif tool_name == "run_skill":
                        sid = tool_args.get("skill_id", "")
                        overrides_str = tool_args.get("overrides_json", "{}")
                        try:
                            overrides = json.loads(overrides_str) if overrides_str else {}
                        except Exception:
                            overrides = {}
                        tool_exec_result = await asyncio.to_thread(r_bridge.run_skill, sid, overrides)
                    elif tool_name == "list_tools":
                        tools = await asyncio.to_thread(r_bridge.list_tools)
                        names = [f'{t.get("name","")}: {t.get("display_name","")}' for t in tools]
                        tool_exec_result = {"success": True, "tools": names, "count": len(names)}
                    elif tool_name == "get_state":
                        # 优先用缓存 JSON 摘要（毫秒级），跳过 R 子进程
                        tool_exec_result = await asyncio.to_thread(r_bridge.get_cached_state)
                    else:
                        tool_exec_result = await asyncio.to_thread(r_bridge.run_tool, tool_name, tool_args)
                except Exception as e:
                    tool_exec_result = {"success": False, "error": str(e)}
                await websocket.send_json({
                    "type": "tool_result", "tool": tool_name, "result": tool_exec_result
                })

        try:
            has_received_chunk = False
            full_response = ""

            while True:
                full_response = ""
                tool_was_called = False

                async def do_chat():
                    nonlocal full_response, has_received_chunk, tool_was_called
                    async for chunk in ai_client.stream_chat(
                        messages=conversation,
                        tools=ai_tools_schema,
                        system_prompt=SYSTEM_PROMPT,
                        on_text=on_text,
                        on_tool_call=on_tool_call
                    ):
                        if cancel_event.is_set():
                            break
                        if chunk.startswith("TEXT:"):
                            has_received_chunk = True
                            full_response += chunk[5:]
                        elif chunk.startswith("TOOL_EXECUTE:"):
                            tool_was_called = True
                            break

                await asyncio.wait_for(do_chat(), timeout=300.0)

                if cancel_event.is_set():
                    return

                if not tool_was_called:
                    break

                conversation.append({
                    "role": "assistant", "content": full_response,
                    "tool_calls": [{
                        "id": tool_exec_info.get("id", tool_exec_info["name"]),
                        "type": "function",
                        "function": {
                            "name": tool_exec_info["name"],
                            "arguments": json.dumps(tool_exec_info["arguments"])
                        }
                    }]
                })
                conversation.append({
                    "role": "tool",
                    "content": json.dumps(tool_exec_result),
                    "tool_call_id": tool_exec_info.get("id", tool_exec_info["name"])
                })
                has_received_chunk = False

            if cancel_event.is_set():
                return

            if not has_received_chunk and not full_response:
                await websocket.send_json({
                    "type": "error",
                    "error": "AI 返回空回复，可能是网络超时或 API 服务异常。"
                })
            else:
                conversation.append({"role": "assistant", "content": full_response})
                await websocket.send_json({"type": "stream_done", "full_text": full_response})
            print(f"[AI] Done, len={len(full_response)}")

        except asyncio.TimeoutError:
            await websocket.send_json({
                "type": "error", "error": "AI 请求超时（120秒），请检查网络或 API 服务状态。"
            })
        except asyncio.CancelledError:
            raise  # 让外层处理取消
        except Exception as e:
            print(f"[AI] Exception: {e}")
            await websocket.send_json({"type": "error", "error": str(e)})

    # ===================== 主循环：消息队列 + 后台接收器（避免 asyncio.wait 竞态） =====================
    message_queue = asyncio.Queue()

    async def message_receiver():
        """后台持续接收 WebSocket 消息，放入队列"""
        try:
            while True:
                msg = await websocket.receive_json()
                await message_queue.put(msg)
        except WebSocketDisconnect:
            await message_queue.put(None)
        except Exception as e:
            print(f"[WS] Receiver error: {e}")
            await message_queue.put(None)

    recv_task = asyncio.create_task(message_receiver())

    async def cancel_chat():
        """取消当前聊天任务并等待清理"""
        nonlocal chat_task
        if chat_task and not chat_task.done():
            chat_task.cancel()
            try:
                await chat_task
            except (asyncio.CancelledError, Exception):
                pass
        chat_task = None

    try:
        while True:
            msg = await message_queue.get()
            if msg is None:
                break  # WebSocket 断开

            # ========== 处理消息 ==========
            if msg["type"] == "cancel":
                print("[WS] Cancel requested")
                cancel_event.set()
                await cancel_chat()
                await websocket.send_json({"type": "stream_text", "content": "\n\n[已中断]"})
                await websocket.send_json({"type": "stream_done", "full_text": ""})
                continue

            if msg["type"] == "reset":
                conversation.clear()
                await cancel_chat()
                await websocket.send_json({"type": "stream_done", "full_text": ""})
                continue

            if msg["type"] != "chat":
                continue

            # 新消息到达时取消当前聊天（如果正在运行）
            await cancel_chat()
            chat_task = asyncio.create_task(run_chat(msg["prompt"]))

    except WebSocketDisconnect:
        print("[WS] Client disconnected")
    except Exception as e:
        print(f"[WS] Fatal error: {e}")
    finally:
        if chat_task and not chat_task.done():
            chat_task.cancel()
        if recv_task and not recv_task.done():
            recv_task.cancel()

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8765)