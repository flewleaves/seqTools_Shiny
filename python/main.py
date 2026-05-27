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
from fastapi.responses import FileResponse
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
            # 添加 execute_r 工具
            ai_tools_schema.append({
                "type": "function",
                "function": {
                    "name": "execute_r",
                    "description": "执行自定义 R 代码。可用于数据分析、画图、数据操作等。注意：每次调用执行独立的 R 子进程，但自动保存/恢复项目状态。",
                    "parameters": {
                        "type": "object",
                        "properties": {
                            "code": {
                                "type": "string",
                                "description": "要执行的 R 代码"
                            }
                        },
                        "required": ["code"]
                    }
                }
            })
            print(f"[OK] Loaded {len(ai_tools_schema)} tools into schema (incl. execute_r)")
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
                choices = pdef.get("choices", [])
                if choices and len(choices) > 0:
                    prop["enum"] = choices

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


SYSTEM_PROMPT = """你是 seqTools 生物信息学分析助手。

重要规则：
1. **必须使用可用工具函数**（通过 function calling）来操作数据，禁止在文本中虚构工具调用结果或假装执行操作。
2. 调用工具时，按需调用，不要一次性调用全部工具。
3. 首次对话必须先调用 get_state 查看工作区和数据状态，再根据数据情况选择后续分析。
4. **`execute_r` 是只读的** — 可以用它查看数据、画图、测试代码，但任何修改都不会被保存。
5. 如果用户需要修改数据（过滤、转换、新增分析等），必须在 `R/tools/` 下创建对应的工具文件（参考现有工具的格式），然后通过工具调用执行。你可以帮用户生成工具文件的代码，但需要用户确认后放入 `R/tools/` 目录。
6. **工具调用结果不直接在对话中展示** — 工具调用完成后，提示用户点击 Shiny 界面中的「同步AI结果」按钮，然后使用 `vis` 工具（结果查看）来查看结果，例如："PCA图已生成，请在 Shiny 中点击「同步AI结果」按钮，然后使用 vis('pca_plot') 查看"。不需要在对话中虚构或描述图形内容。
7. 提供中文回答，简洁专业。

输出规范：
- 每步完成后用 1-2 句总结做了什么
- 给出当前数据状态
- 最后提出下一步建议（如果还有可执行的操作）

注意事项：
- 不要暴露文件绝对路径
- 如果工具调用失败，如实报告错误，不要虚构结果
- 单个任务最多调用 5 次工具"""


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
        shutil.copy2(rds_path, r_bridge._state_file)
        return {"success": True}
    except Exception as e:
        return {"success": False, "error": str(e)}


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

    async def run_chat(prompt: str):
        """后台执行完整 AI 对话 + 工具调用循环，可被外部 cancel"""
        nonlocal conversation
        if ai_client is None:
            await websocket.send_json({
                "type": "error",
                "error": "AI 服务未初始化：请在 seqTools_config.ini 的 [AI] 节填写 key 后重启服务。"
            })
            return

        cancel_event.clear()
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
                tool_exec_info = info
                tool_name = info["name"]
                tool_args = info["arguments"]
                await websocket.send_json({"type": "tool_executing", "tool": tool_name})
                try:
                    if tool_name == "reload_tools":
                        tools = await asyncio.to_thread(r_bridge.list_tools)
                        global ai_tools_schema
                        ai_tools_schema = build_tools_schema(tools)
                        tool_exec_result = {"success": True, "tools": tools}
                    elif tool_name == "execute_r":
                        r_code = tool_args.get("code", "")
                        tool_exec_result = await asyncio.to_thread(r_bridge.run_r_code, r_code)
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

                await asyncio.wait_for(do_chat(), timeout=120.0)

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

    # ===================== 主循环：并发接收消息 + 管理聊天任务 =====================
    try:
        while True:
            # 如果聊天在运行，同时等待聊天完成或新消息
            if chat_task and not chat_task.done():
                recv_task = asyncio.create_task(websocket.receive_json())
                done, pending = await asyncio.wait(
                    [chat_task, recv_task],
                    return_when=asyncio.FIRST_COMPLETED
                )

                if chat_task in done:
                    # 聊天自然结束
                    try:
                        await chat_task
                    except asyncio.CancelledError:
                        pass
                    except Exception as e:
                        print(f"[WS] Chat error: {e}")
                    chat_task = None
                    if recv_task in done:
                        msg = recv_task.result()
                    else:
                        recv_task.cancel()
                        continue
                else:
                    # 新消息在聊天期间到达
                    msg = recv_task.result()
                    chat_task.cancel()
                    try:
                        await chat_task
                    except (asyncio.CancelledError, Exception):
                        pass
                    chat_task = None

                    # 如果是 cancel，给前端发反馈
                    if msg["type"] == "cancel":
                        cancel_event.set()
                        await websocket.send_json({"type": "stream_text", "content": "\n\n[已中断]"})
                        await websocket.send_json({"type": "stream_done", "full_text": ""})
                        continue
            else:
                chat_task = None
                msg = await websocket.receive_json()

            # ========== 处理消息 ==========
            if msg["type"] == "cancel":
                print("[WS] Cancel requested")
                cancel_event.set()
                continue

            if msg["type"] == "reset":
                conversation.clear()
                await websocket.send_json({"type": "stream_done", "full_text": ""})
                continue

            if msg["type"] != "chat":
                continue

            # 启动新的聊天任务（消息可在其运行时被 cancel）
            chat_task = asyncio.create_task(run_chat(msg["prompt"]))

    except WebSocketDisconnect:
        print("[WS] Client disconnected")
    except Exception as e:
        print(f"[WS] Fatal error: {e}")
    finally:
        if chat_task and not chat_task.done():
            chat_task.cancel()

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8765)