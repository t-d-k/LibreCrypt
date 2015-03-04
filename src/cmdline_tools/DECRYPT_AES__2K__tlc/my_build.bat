rem @echo off

rem Set the build environment
call ..\cmdline_common.bat
set PROJECT_DIR=%PROJECT_BASE_DIR%DECRYPT_AES__2K__tlc\


cd %PROJECT_DIR%src

echo Building...

rem aes requires "zeromem" function
rem copy ..\..\3rd_party\libtomcrypt\crypt-0.94\mem.c .

rem cl -I..\..\3rd_party\libtomcrypt\crypt-1.17 -I..\..\3rd_party\libtomcrypt\crypt-1.17\src\headers\ /Fe..\DECRYPT_AES__2K__tlc.exe main.c ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt.c ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\aes\aes.c ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_decrypt.c
cl -I%LTC_HDR_DIR% /Fe..\DECRYPT_AES__2K__tlc.exe main.c %LTC_CRYPT%crypt.c %LTC_AES%aes.c %LTC_CBC%cbc_decrypt.c ^
 %LTC_CBC%cbc_encrypt.c %LTC_MISC%error_to_string.c %LTC_CBC%cbc_start.c  %LTC_CRYPT%crypt_find_cipher.c ^
 %LTC_CRYPT%crypt_register_cipher.c %LTC_CRYPT%crypt_cipher_is_valid.c %LTC_CRYPT%crypt_cipher_descriptor.c

