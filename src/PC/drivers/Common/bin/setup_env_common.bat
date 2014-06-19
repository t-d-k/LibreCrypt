@echo off

rem -- Configure all items in this block --

rem NOTICE: To configure for x86/AMD64, simply set the following as appropriate:
rem
rem           FREEOTFE_TARGET 
rem           FREEOTFE_VC_ENV 
rem           FREEOTFE_VC_OUTDIR_SEG 
rem 
rem         - OR - 
rem 
rem         Set this variable:
rem 
rem           FREEOTFE_CPU
rem 
rem         Note: Setting FREEOTFE_CPU overrides the variables shown above
rem set FREEOTFE_CPU=x86
set FREEOTFE_CPU=amd64


rem Build debug (1) or release (0)
rem set FREEOTFE_DEBUG=1
set FREEOTFE_DEBUG=0

rem Set to 1 for checked driver build
rem If FREEOTFE_DEBUG is set to 1, this will effectivly be set to 1 as well
rem set FREEOTFE_CHECKED_BUILD=1
set FREEOTFE_CHECKED_BUILD=0

rem Build target OS - set the following to one of: 
rem   VC++ 2005 environment:
rem     W2K   - Windows 2000 (no longer supported value?)
rem     64    - IA64
rem     AMD64 - AMD64
rem     WNET  - Windows Server 2003
rem     WXP   - Windows XP
rem   VC++ 2008 environment:
rem     WLH
rem     64
rem     x64
rem     x32-64
rem     WXP
rem IMPORTANT!!! The Windows Driver Kit (WDK) for Server 2008 (v6001.18001) CAN'T BUILD amd64/x64 DRIVERS FOR WINDOWS XP!!
rem (It comes up with: "Cannot build AMD64 bit binaries for Windows XP.  Defaulting to X86." when the WDK's setenv.bat is run)
set FREEOTFE_TARGET=WXP
rem set FREEOTFE_TARGET=AMD64
if "%FREEOTFE_CPU%" == "x86" (
  set FREEOTFE_TARGET=WXP
) else if "%FREEOTFE_CPU%" == "amd64" (
  rem VC++ 2005 - set FREEOTFE_TARGET=AMD64
  rem VC++ 2008 - set FREEOTFE_TARGET=x64
  set FREEOTFE_TARGET=x64
) else (
  rem Do nothing...
)

rem Set to one of:
rem   x86
rem   ia64
rem   amd64
rem   x86_ia64
rem   x86_amd64
rem
set FREEOTFE_VC_ENV=x86
rem set FREEOTFE_VC_ENV=amd64
if "%FREEOTFE_CPU%" == "x86" (
  set FREEOTFE_VC_ENV=x86
) else if "%FREEOTFE_CPU%" == "amd64" (
  set FREEOTFE_VC_ENV=amd64
) else (
  rem Do nothing...
)

rem The VS environment
rem VC++ environment - set to the 8.3 path if the path has brackets in it
rem e.g. "Program Files (x86)" - the brackets cause problems
rem VC++ .NET
rem set VCVARSALL=vcvars32
rem VC++ 2005
rem set VCVARSALL=vcvarsall.bat
rem VC++ 2008 on an x64 box
rem Here we use the 8.3 name for that dir in calling vcvarsall.bat
set VCVARSALL="C:\PROGRA~2\SOFT_DEV\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"


rem Architecture; VC++ outputs to a directory, and this is used to copy the resultant binary to a common location
set FREEOTFE_VC_OUTDIR_SEG=WXP_x86\i386
rem set FREEOTFE_VC_OUTDIR_SEG=wnet_amd64\amd64
if "%FREEOTFE_CPU%" == "x86" (
  set FREEOTFE_VC_OUTDIR_SEG=WXP_x86\i386
) else if "%FREEOTFE_CPU%" == "amd64" (
  rem VC++ 2005
  rem set FREEOTFE_VC_OUTDIR_SEG=wnet_amd64\amd64
  rem VC++ 2008
  rem set FREEOTFE_VC_OUTDIR_SEG=wlh_amd64\amd64
  set FREEOTFE_VC_OUTDIR_SEG=wlh_amd64\amd64
) else (
  rem Do nothing...
)


rem Project drive and directory; the base directory where the source code is
set PROJECT_DRIVE=E:
set PROJECT_DIR=%PROJECT_DRIVE%\DEV\SARAH_DEAN_EXISTING\Under_SCC\FreeOTFE\DEVELOPMENT_SRC\src\PC\drivers
set BIN_OUTPUT_DIR=%PROJECT_DRIVE%\DEV\SARAH_DEAN_EXISTING\Under_SCC\FreeOTFE\DEVELOPMENT_SRC\bin\PC\%FREEOTFE_VC_ENV%\
mkdir %BIN_OUTPUT_DIR%

rem SDK directory
rem set MSSDK_DIR=C:\MSSDK
rem VC++ 2008
set MSSDK_DIR="0"

rem DDK directory
rem set MSDDK_DIR=C:\WINDDK\3790
set MSDDK_DIR=C:\WinDDK\6001.18001

rem Additional includes
rem VC++ 2008 needs this for afxres.h; used by the .rc files
rem The includes dir gets dumped by the setenv.bat?
set ADD_INCLUDE=C:\WinDDK\6001.18001\inc\mfc42\
rem set ADD_INCLUDE=

rem -- Items below this line should probably not need changing --

rem Set the VS environment
call %VCVARSALL% %FREEOTFE_VC_ENV%

rem Set the SDK build environment
rem !!!!!!!!!!!!!!!!!
rem !!!  WARNING  !!!
rem !!!!!!!!!!!!!!!!!
if %MSSDK_DIR%=="0" goto NOMSSDK
  rem IF CHANGING BETWEEN DEBUG/RETAIL, UPDATE setup_env_driver.bat AS WELL
  if %FREEOTFE_DEBUG%==1 goto DEBUG
    rem Configure up for retail (release) build
    call %MSSDK_DIR%\SetEnv.Bat /RETAIL
  goto EXIT
:DEBUG
    rem Configure up for debug build
    call %MSSDK_DIR%\SetEnv.Bat /DEBUG
:EXIT
:NOMSSDK

rem In other .bat file
rem set INCLUDE=%ADD_INCLUDE%;%INCLUDE%
