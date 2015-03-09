@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\HASH_DRIVERS\HASH_WHIRLPOOL\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\copy_common_hash_driver_files.bat


rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\HASH_DRIVERS\HASH_WHIRLPOOL\FreeOTFEHashWhirlpool.c .
copy %PROJECT_BASE_DIR%\src\Common\HASH_DRIVERS\HASH_WHIRLPOOL\FreeOTFEHashWhirlpool.h .

rem libtomcrypt library...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\whirl\whirl.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\hashes\whirl\whirltab.c .

copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\misc\zeromem.c .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFEHashWhirlpool.sys %BIN_OUTPUT_DIR%


del whirl.c 
del whirltab.c 
del zeromem.c
del FreeOTFEHashWhirlpool.c
del FreeOTFEHashWhirlpool.h


call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\delete_common_hash_driver_files.bat

cd ..

