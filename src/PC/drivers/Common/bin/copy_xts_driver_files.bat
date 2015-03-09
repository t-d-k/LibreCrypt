

rem should call copy_common_driver_files.bat first so set up %THIRD_PARTY_DIR%


rem libtomcrypt library xts mode ...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\xts\xts_init.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\xts\xts_encrypt.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\xts\xts_decrypt.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\xts\xts_mult_x.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\xts\xts_done.c .

