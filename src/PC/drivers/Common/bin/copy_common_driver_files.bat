
rem call ..\..\Common\bin\setup_env_driver first - to set up %PROJECT_BASE_DIR%
rem TODO: fix all the copying by using builds in sep dirs and .lib files 
rem The build utility can't handle source files being in sub dirs
rem only in same or parent dirs
rem 'correct' way of handling this is with .lib files and separate builds for common files
rem also possible to use gnu make or #includes
rem see http://www.osronline.com/showThread.cfm?link=25134
rem for now copy files and delete afterwards, has problem that causes rebuild each time (timestamp changed)

copy %PROJECT_BASE_DIR%\src\Common\Common\src\FreeOTFEDebug.c .
copy %PROJECT_BASE_DIR%\src\Common\Common\src\FreeOTFElib.c .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\Common\src\FreeOTFECypherImpl.c .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\Common\src\FreeOTFECypherAPICommon.c .
copy %PROJECT_DIR%\Common\src\FreeOTFEDriverlib.c .
copy %PROJECT_DIR%\CYPHER_DRIVERS\Common\src\FreeOTFECypherDriver.c .

set THIRD_PARTY_DIR=%PROJECT_BASE_DIR%\src\3rd_party

rem Modified libtomcrypt XTS library...
copy %THIRD_PARTY_DIR%\ltc_gladman_xts\ltc_gladman_xts_init.c .
copy %THIRD_PARTY_DIR%\ltc_gladman_xts\ltc_gladman_xts_done.c .
copy %THIRD_PARTY_DIR%\ltc_gladman_xts\ltc_gladman_xts_encrypt.c .
copy %THIRD_PARTY_DIR%\ltc_gladman_xts\ltc_gladman_xts_decrypt.c .
copy %THIRD_PARTY_DIR%\ltc_gladman_xts\ltc_gladman_xts_mult_x.c .

rem not used by every project
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_cipher_descriptor.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_cipher_is_valid.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_find_cipher.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_register_cipher.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_unregister_cipher.c .

copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_start.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_encrypt.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_decrypt.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_done.c .


