@echo off

:PARPARSE
  set MGV_USEXHRB=Y
  if "%MG_CMP%"=="XHARBOUR" set MGV_USEXHRB=Y
  if "%MG_BCC%"==""  set MG_BCC=c:\borland\bcc58
  if "%MG_ROOT%"=="" set MG_ROOT=c:\minigui
  if "%MG_HRB%"==""  set MG_HRB=%MG_ROOT%\harbour
  if "%MG_LIB%"==""  set MG_LIB=%MG_ROOT%\lib
  if "%MG_XHRB%"=="" set MG_XHRB=c:\xharbour
  if "%MG_XLIB%"=="" set MG_XLIB=%MG_ROOT%\xlib
  if "%1"=="/x"      set MGV_USEXHRB=Y
  if "%1"=="/X"      set MGV_USEXHRB=Y
  if "%1"=="/-x"     set MGV_USEXHRB=N
  if "%1"=="/-X"     set MGV_USEXHRB=N
  if %MGV_USEXHRB%==N set MGV_HRB=%MG_HRB%
  if %MGV_USEXHRB%==N set MGV_LIB=%MG_LIB%
  if %MGV_USEXHRB%==Y set MGV_HRB=%MG_XHRB%
  if %MGV_USEXHRB%==Y set MGV_LIB=%MG_XLIB%

:PROC

  if exist hbxpp.lib del hbxpp.lib

  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MGV_HRB%\include;%MG_BCC%\include -L%MGV_LIB%;%MG_BCC%\lib dllcall.c
  if errorlevel 1 goto END

  %MG_BCC%\bin\tlib hbxpp.lib +dllcall.obj

:CLEANUP
  del dllcall.obj
  goto END

:END
  set MGV_USEXHRB=
  set MGV_HRB=
  set MGV_LIB=