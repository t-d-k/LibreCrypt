rem @echo off

rem Set the build environment
call ..\..\PC\drivers\Common\bin\setup_env_common.bat


echo move
rem Move into the correct src directory...
%PROJECT_DRIVE%
echo PROJECT_DRIVE %PROJECT_DRIVE%
cd %PROJECT_DIR%\..\..\cmdline_tools\DECRYPT_AES__2K__tlc\src
echo %PROJECT_DIR%\..\..\cmdline_tools\DECRYPT_AES__2K__tlc\src

echo Building...

copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\aes\aes.c .
copy ..\..\..\3rd_party\libtomcrypt\ccrypt-1.17\src\ciphers\aes\aes_tab.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\cbc.c .

copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\strings.c .

rem aes requires "zeromem" function
copy ..\..\3rd_party\libtomcrypt\crypt-0.94\mem.c .

rem Note that we don't include "aes_tab.c" in this list - it's #included by aes.c
cl -I..\..\3rd_party\libtomcrypt\crypt-1.17 /Fe..\DECRYPT_AES__2K__tlc.exe main.c crypt.c aes.c cbc.c strings.c mem.c

