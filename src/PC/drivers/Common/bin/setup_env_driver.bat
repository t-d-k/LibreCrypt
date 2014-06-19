@echo off

rem -- Items below this line should probably not need changing --

rem Set the checked DDK build environment for WXP/W2K/default
rem !!!!!!!!!!!!!!!!!
rem !!!  WARNING  !!!
rem !!!!!!!!!!!!!!!!!
rem IF CHANGING BETWEEN DEBUG/RETAIL, UPDATE setup_env_common.bat AS WELL
if %FREEOTFE_DEBUG%==1 goto DEBUG
if %FREEOTFE_CHECKED_BUILD%==1 goto DEBUG
  set FREEOTFE_CHK_FRE=fre
goto BUILD_TYPE_CONTINUE
:DEBUG
  set FREEOTFE_CHK_FRE=chk
:BUILD_TYPE_CONTINUE

call %MSDDK_DIR%\bin\SETENV.BAT %MSDDK_DIR% %FREEOTFE_CHK_FRE% %FREEOTFE_TARGET%
set INCLUDE=%ADD_INCLUDE%;%INCLUDE%

rem This should now be fully set by variables; examples of what this will end
rem up as are as follows:
rem 
rem   set FREEOTFE_OUTPUT_DIR=obj%FREEOTFE_CHK_FRE%_WXP_x86\i386
rem   set FREEOTFE_OUTPUT_DIR=obj%FREEOTFE_CHK_FRE%_wnet_amd64\amd64
rem   set FREEOTFE_OUTPUT_DIR=obj%FREEOTFE_CHK_FRE%_%FREEOTFE_TARGET%_x86\i386
set FREEOTFE_OUTPUT_DIR=obj%FREEOTFE_CHK_FRE%_%FREEOTFE_VC_OUTDIR_SEG%

