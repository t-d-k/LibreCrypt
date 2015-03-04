

rem Set the build environment
call ..\cmdline_common.bat
set PROJECT_DIR=%PROJECT_BASE_DIR%DECRYPT_AES__tlc\

cd %PROJECT_DIR%src


rem copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\aes\aes.c .
rem copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\aes\aes_tab.c .
rem copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt.c .
rem rem copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\cbc.c .

rem copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\strings.c .

rem aes requires "zeromem" function
rem copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\mem.c .

rem Note that we don't include "aes_tab.c" in this list - it's #included by aes.c
rem cl -I..\..\..\3rd_party\libtomcrypt\crypt-0.94 /Fe..\DECRYPT_AES__tlc.exe main.c crypt.c aes.c cbc.c strings.c mem.c
cl -I%LTC_HDR_DIR%  /Fe..\DECRYPT_AES__tlc.exe main.c %LTC_CRYPT%crypt.c %LTC_AES%aes.c %LTC_CBC%cbc_decrypt.c ^
%LTC_MISC%error_to_string.c %LTC_CBC%cbc_start.c %LTC_CRYPT%crypt_find_cipher.c ^
%LTC_CRYPT%crypt_register_cipher.c %LTC_CRYPT%crypt_cipher_is_valid.c %LTC_CRYPT%crypt_cipher_descriptor.c

cd ..


