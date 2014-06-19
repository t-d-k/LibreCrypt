@echo off

rem Set the build environment
call ..\..\Common\bin\setup_env_common
call ..\..\Common\bin\setup_env_driver


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_AES_Gladman\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\..\Common\Common\src\* .
copy ..\..\..\..\..\Common\CYPHER_DRIVERS\Common\src\* .
copy ..\..\..\Common\src\* .
copy ..\..\Common\src\* .

rem Implementation...
copy ..\..\..\..\..\Common\CYPHER_DRIVERS\CYPHER_AES_Gladman\* .

rem Modified libtomcrypt XTS library...
copy ..\..\..\..\..\3rd_party\ltc_gladman_xts\ltc_gladman_xts_init.c .
copy ..\..\..\..\..\3rd_party\ltc_gladman_xts\ltc_gladman_xts_done.c .
copy ..\..\..\..\..\3rd_party\ltc_gladman_xts\ltc_gladman_xts_encrypt.c .
copy ..\..\..\..\..\3rd_party\ltc_gladman_xts\ltc_gladman_xts_decrypt.c .
copy ..\..\..\..\..\3rd_party\ltc_gladman_xts\ltc_gladman_xts_mult_x.c .

rem Gladman source...
copy ..\..\..\..\..\3rd_party\AES_candidates_2nd_round_-_Gladman\aes.r2.algs\rijndael.c .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherAES_Gladman.sys %BIN_OUTPUT_DIR%

