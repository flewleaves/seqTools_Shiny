@echo off
echo 正在停止 seqTools 进程...
taskkill /F /FI "WINDOWTITLE eq seqTools-Python*" 2>nul
taskkill /F /IM Rscript.exe 2>nul
taskkill /F /IM R.exe 2>nul
echo 已清理进程。
pause
