@echo off

rem Set the build environment
call ..\..\PC\drivers\Common\bin\setup_env_common.bat



rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\..\..\cmdline_tools\DECRYPT_AES__tlc\src


copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\aes\aes.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\aes\aes_tab.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt.c .
rem copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\cbc.c .

rem copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\strings.c .

rem aes requires "zeromem" function
rem copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\mem.c .

rem Note that we don't include "aes_tab.c" in this list - it's #included by aes.c
rem cl -I..\..\..\3rd_party\libtomcrypt\crypt-0.94 /Fe..\DECRYPT_AES__tlc.exe main.c crypt.c aes.c cbc.c strings.c mem.c
cl -I..\..\..\3rd_party\libtomcrypt\crypt-1.17 -I..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\headers  /Fe..\DECRYPT_AES__tlc.exe main.c crypt.c aes.c 
cd ..


