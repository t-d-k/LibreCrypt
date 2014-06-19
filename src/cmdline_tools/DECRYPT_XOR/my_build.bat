@echo off

rem Set the build environment
call ..\..\drivers\Common\bin\setup_env_common.bat



rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\..\cmdline_tools\DECRYPT_XOR\src

echo Building...

cl -I..\..\..\3rd_party\libtomcrypt\crypt-0.94 /Fe..\DECRYPT_XOR.exe main.c

