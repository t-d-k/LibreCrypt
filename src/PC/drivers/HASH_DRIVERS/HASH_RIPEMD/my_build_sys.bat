@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)

rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\HASH_DRIVERS\HASH_RIPEMD\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\copy_common_hash_driver_files.bat

rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\HASH_DRIVERS\HASH_RIPEMD\FreeOTFEHashRIPEMD.c .
copy %PROJECT_BASE_DIR%\src\Common\HASH_DRIVERS\HASH_RIPEMD\FreeOTFEHashRIPEMD.h .

rem libtomcrypt library...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\rmd128.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\rmd160.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\rmd256.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\rmd320.c .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFEHashRIPEMD.sys %BIN_OUTPUT_DIR%
del FreeOTFEHashRIPEMD.c
del FreeOTFEHashRIPEMD.h
del rmd128.c
del rmd160.c
del rmd256.c
del rmd320.c

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\delete_common_hash_driver_files.bat
cd ..

