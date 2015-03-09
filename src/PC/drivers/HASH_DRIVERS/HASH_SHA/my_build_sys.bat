@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\HASH_DRIVERS\HASH_SHA\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\copy_common_hash_driver_files.bat

rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\HASH_DRIVERS\HASH_SHA\FreeOTFEHashSHA.c .
copy %PROJECT_BASE_DIR%\src\Common\HASH_DRIVERS\HASH_SHA\FreeOTFEHashSHA.h .

rem libtomcrypt library...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\sha1.c   .

copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\sha2\sha224.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\sha2\sha256.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\sha2\sha384.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\sha2\sha512.c .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFEHashSHA.sys %BIN_OUTPUT_DIR%

del FreeOTFEHashSHA.c
del FreeOTFEHashSHA.h
del sha1.c
del sha224.c
del sha256.c
del sha384.c
del sha512.c

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\delete_common_hash_driver_files.bat
cd ..

