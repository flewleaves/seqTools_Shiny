@echo off
cd /d "%~dp0"

echo [DEBUG MODE]
echo [1/2] 启动 Python AI 后端 (端口 8765，前台运行)...
cd python
python -m uvicorn main:app --host 0.0.0.0 --port 8765 --reload
