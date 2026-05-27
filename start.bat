@echo off
cd /d "%~dp0"

echo [1/2] 启动 Python AI 后端 (端口 8765)...
start "seqTools-Python" cmd /k "cd python && python -m uvicorn main:app --host 0.0.0.0 --port 8765"

timeout /t 4 >nul

echo [2/2] 启动 R Shiny (端口 3838)...
Rscript -e "shiny::runApp('.', port=3838, launch.browser=TRUE, host='0.0.0.0')"

echo.
echo 关闭本窗口将停止 Shiny，Python 窗口需手动关闭。
pause
