@echo off

rem Set the build environment
call ..\..\PC\drivers\Common\bin\setup_env_common.bat



rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\..\..\cmdline_tools\DECRYPT_XOR\src

echo Building...

cl -I..\..\3rd_party\libtomcrypt\crypt-1.17 /Fe..\DECRYPT_XOR.exe main.c

cd ..
