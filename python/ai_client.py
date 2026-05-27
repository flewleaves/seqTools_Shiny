# python/ai_client.py
"""流式 AI 客户端"""
import httpx
import json
from collections.abc import Callable, AsyncGenerator, Awaitable


class AIClient:
    def __init__(self, api_key: str, base_url: str = "https://api.deepseek.com", model: str = "deepseek-chat"):
        if not api_key or str(api_key).strip() == "":
            raise ValueError("API Key 为空...")
        if not base_url or str(base_url).strip() == "":
            raise ValueError(
                "API base_url 为空。请在 seqTools_config.ini 中配置：\n"
                "[AI]\nbase_url = https://api.xxx.com\n"
                "或在 Shiny 设置面板选择提供商并保存。"
            )
        self.api_key = str(api_key).strip()
        self.base_url = base_url.rstrip("/")
        self.model = model
        self.client = httpx.AsyncClient(timeout=60.0)  # 60 秒总超时

    async def stream_chat(
        self,
        messages: list,
        tools: list,
        system_prompt: str,
        on_text: Callable[[str], Awaitable[None]] | None = None,
        on_tool_call: Callable[[str, dict], Awaitable[None]] | None = None,
    ) -> AsyncGenerator[str, None]:
        if not self.api_key:
            yield "TEXT:[系统错误] API Key 未配置"
            return

        payload_messages = [{"role": "system", "content": system_prompt}] + messages

        try:
            async with self.client.stream(
                "POST",
                f"{self.base_url}/chat/completions",
                headers={"Authorization": f"Bearer {self.api_key}"},
                json={
                    "model": self.model,
                    "messages": payload_messages,
                    "tools": tools,
                    "stream": True,
                    "max_tokens": 4096,
                },
            ) as response:
                
                # 立即检查 HTTP 状态码
                if response.status_code != 200:
                    body = await response.aread()
                    yield f"TEXT:[HTTP {response.status_code}] {body.decode('utf-8', errors='replace')[:200]}"
                    return

                current_tool: dict | None = None

                async for line in response.aiter_lines():
                    if not line.startswith("data: "):
                        continue
                    data = line[6:]
                    if data == "[DONE]":
                        break
                    try:
                        event = json.loads(data)
                    except Exception:
                        continue

                    if "error" in event:
                        error_msg = event["error"].get("message", "未知错误")
                        yield f"TEXT:[API 错误: {error_msg}]"
                        continue

                    choices = event.get("choices", [])
                    if not choices:
                        continue

                    delta = choices[0].get("delta", {})

                    if "content" in delta and delta["content"]:
                        text = delta["content"]
                        if on_text:
                            await on_text(text)
                        yield f"TEXT:{text}"

                    if "tool_calls" in delta:
                        tc = delta["tool_calls"][0]
                        if "function" in tc:
                            func = tc["function"]
                            if "name" in func and func["name"]:
                                current_tool = {
                                    "id": tc.get("id", ""),
                                    "name": func["name"],
                                    "arguments": func.get("arguments", "")
                                }
                                if on_tool_call:
                                    await on_tool_call("start", current_tool)
                                yield f"TOOL_START:{json.dumps(current_tool)}"
                            elif "arguments" in func and current_tool:
                                current_tool["arguments"] += func["arguments"]

                    finish = choices[0].get("finish_reason")
                    if finish == "tool_calls" and current_tool:
                        try:
                            args = json.loads(current_tool["arguments"])
                        except Exception:
                            args = {}
                        exec_info = {
                            "id": current_tool.get("id", ""),
                            "name": current_tool["name"],
                            "arguments": args
                        }
                        if on_tool_call:
                            await on_tool_call("execute", exec_info)
                        yield f"TOOL_EXECUTE:{json.dumps(exec_info)}"
                        current_tool = None

        except httpx.HTTPStatusError as e:
            yield f"TEXT:[HTTP 错误 {e.response.status_code}] {e.response.text[:200]}"
        except httpx.RequestError as e:
            yield f"TEXT:[网络错误] 无法连接到 AI 服务 ({self.base_url})：{str(e)}"
        except Exception as e:
            yield f"TEXT:[系统错误] {str(e)}"

    async def close(self) -> None:
        await self.client.aclose()