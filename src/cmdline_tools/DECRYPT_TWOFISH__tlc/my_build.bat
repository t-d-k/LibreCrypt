rem @echo off
@echo on
rem Set the build environment
call ..\cmdline_common.bat
set PROJECT_DIR=%PROJECT_BASE_DIR%DECRYPT_TWOFISH__tlc\

cd %PROJECT_DIR%
rem call ..\..\PC\drivers\Common\bin\setup_env_common.bat


rem Move into the correct src directory...

cd %PROJECT_DIR%src

echo Building ...

cl -I%LTC_HDR_DIR% /Fe..\DECRYPT_TWOFISH__tlc.exe main.c %LTC_CRYPT%crypt.c %LTC_TWOFISH%twofish.c %LTC_TWOFISH%twofish_tab.c ^
%LTC_CBC%cbc_decrypt.c %LTC_CBC%cbc_encrypt.c %LTC_MISC%error_to_string.c %LTC_CBC%cbc_start.c  %LTC_CRYPT%crypt_find_cipher.c ^
%LTC_CRYPT%crypt_register_cipher.c %LTC_CRYPT%crypt_cipher_is_valid.c %LTC_CRYPT%crypt_cipher_descriptor.c

cd ..
