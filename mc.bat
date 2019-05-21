@echo off
setlocal
    set MPM_ROOT=.\.mpm\
    kotlinc -script mc.kts %*
endlocal