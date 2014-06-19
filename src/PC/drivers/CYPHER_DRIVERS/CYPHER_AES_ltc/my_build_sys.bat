@echo off

rem Set the build environment
call ..\..\Common\bin\setup_env_common
call ..\..\Common\bin\setup_env_driver


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_AES_ltc\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\..\Common\Common\src\* .
copy ..\..\..\..\..\Common\CYPHER_DRIVERS\Common\src\* .
copy ..\..\..\Common\src\* .
copy ..\..\Common\src\* .

rem Implementation...
copy ..\..\..\..\..\Common\CYPHER_DRIVERS\CYPHER_AES_ltc\* .

rem libtomcrypt library...
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\aes\aes.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\aes\aes_tab.c .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_cipher_descriptor.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_cipher_is_valid.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_find_cipher.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_register_cipher.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_unregister_cipher.c .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_start.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_encrypt.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_decrypt.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_done.c .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_start.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_encrypt.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_decrypt.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_setiv.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_process.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\lrw\lrw_done.c .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\xts\xts_init.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\xts\xts_encrypt.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\xts\xts_decrypt.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\xts\xts_mult_x.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\xts\xts_done.c .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\zeromem.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\encauth\gcm\gcm_gf_mult.c .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherAES_ltc.sys %BIN_OUTPUT_DIR%

