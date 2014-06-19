@echo off

rem Set the build environment
call ..\..\drivers\Common\bin\setup_env_common.bat



rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\..\cmdline_tools\DECRYPT_TWOFISH__tlc\src

echo Building...

copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\twofish.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\twofish_tab.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\crypt.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\cbc.c .

copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\strings.c .

rem Note that we don't include "twofish_tab.c" in this list - it's #included by aes.c
cl -I..\..\..\3rd_party\libtomcrypt\crypt-0.94 /Fe..\DECRYPT_TWOFISH__tlc.exe main.c crypt.c twofish.c cbc.c strings.c

