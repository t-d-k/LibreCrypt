@echo off

rem Set the build environment
call ..\..\Common\bin\setup_env_common
call ..\..\Common\bin\setup_env_driver


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\HASH_DRIVERS\HASH_MD\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\..\Common\Common\src\* .
copy ..\..\..\Common\src\* .
copy ..\..\Common\src\* .

rem Implementation...
copy ..\..\..\..\..\Common\HASH_DRIVERS\HASH_MD\* .

rem libtomcrypt library...
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\md2.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\md4.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\md5.c .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\misc\zeromem.c .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFEHashMD.sys %BIN_OUTPUT_DIR%

