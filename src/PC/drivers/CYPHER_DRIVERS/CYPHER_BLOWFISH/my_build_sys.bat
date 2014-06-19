@echo off

rem Set the build environment
call ..\..\Common\bin\setup_env_common
call ..\..\Common\bin\setup_env_driver


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_BLOWFISH\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\..\Common\Common\src\* .
copy ..\..\..\..\..\Common\CYPHER_DRIVERS\Common\src\* .
copy ..\..\..\Common\src\* .
copy ..\..\Common\src\* .

rem Implementation...
copy ..\..\..\..\..\Common\CYPHER_DRIVERS\CYPHER_BLOWFISH\* .

rem libtomcrypt library...
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\ciphers\blowfish.c .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_cipher_descriptor.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_cipher_is_valid.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_find_cipher.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_register_cipher.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\crypt\crypt_unregister_cipher.c .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_start.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_encrypt.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_decrypt.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\modes\cbc\cbc_done.c .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherBLOWFISH.sys %BIN_OUTPUT_DIR%

