@echo off

rem Set the build environment
call ..\..\Common\bin\setup_env_common
call ..\..\Common\bin\setup_env_driver


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\HASH_DRIVERS\HASH_SHA\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\..\Common\Common\src\* .
copy ..\..\..\Common\src\* .
copy ..\..\Common\src\* .

rem Implementation...
copy ..\..\..\..\..\Common\HASH_DRIVERS\HASH_SHA\* .

rem libtomcrypt library...
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\sha1.c   .

copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\sha2\sha224.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\sha2\sha256.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\sha2\sha384.c .
copy ..\..\..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\sha2\sha512.c .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFEHashSHA.sys %BIN_OUTPUT_DIR%

