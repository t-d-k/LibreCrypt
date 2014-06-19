@echo off

rem Set the build environment
call ..\..\Common\bin\setup_env_common
call ..\..\Common\bin\setup_env_driver


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\HASH_DRIVERS\HASH_RIPEMD\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\..\Common\Common\src\* .
copy ..\..\..\Common\src\* .
copy ..\..\Common\src\* .

rem Implementation...
copy ..\..\..\..\..\Common\HASH_DRIVERS\HASH_RIPEMD\* .

rem libtomcrypt library...
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\rmd128.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\rmd160.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\rmd256.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\rmd320.c .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFEHashRIPEMD.sys %BIN_OUTPUT_DIR%

