@echo off

rem Set the build environment
call ..\..\drivers\Common\bin\setup_env_common.bat



rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\..\cmdline_tools\DECRYPT_AES__tlc\src

echo Building...

copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\aes.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\aes_tab.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\crypt.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\cbc.c .

copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\strings.c .

rem aes requires "zeromem" function
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\mem.c .

rem Note that we don't include "aes_tab.c" in this list - it's #included by aes.c
cl -I..\..\..\3rd_party\libtomcrypt\crypt-0.94 /Fe..\DECRYPT_AES__tlc.exe main.c crypt.c aes.c cbc.c strings.c mem.c

