@echo off
setlocal
    set MPM_ROOT=.\.mpm\
    kotlinc -script m.kts %*
endlocal