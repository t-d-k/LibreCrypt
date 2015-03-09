

rem should call copy_common_driver_files.bat first so set up %THIRD_PARTY_DIR%


rem libtomcrypt library LRW mode ...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_start.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_encrypt.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_decrypt.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_setiv.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_process.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_done.c .

copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\misc\zeromem.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\encauth\gcm\gcm_gf_mult.c .


