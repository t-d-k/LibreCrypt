@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\HASH_DRIVERS\HASH_TIGER\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\copy_common_hash_driver_files.bat

rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\HASH_DRIVERS\HASH_TIGER\FreeOTFEHashTiger.c .
copy %PROJECT_BASE_DIR%\src\Common\HASH_DRIVERS\HASH_TIGER\FreeOTFEHashTiger.h .

rem libtomcrypt library...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\tiger.c .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFEHashTiger.sys %BIN_OUTPUT_DIR%

del tiger.c
del FreeOTFEHashTiger.c
del FreeOTFEHashTiger.h

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\delete_common_hash_driver_files.bat
cd ..

