@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_AES_ltc\src

call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\copy_lrw_driver_files.bat
call %PROJECT_DIR%\Common\bin\copy_xts_driver_files.bat

rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_AES_ltc\FreeOTFECypherAES_ltc.h .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_AES_ltc\FreeOTFECypherAES_ltc.c .

rem libtomcrypt library...

copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\ciphers\aes\aes.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\ciphers\aes\aes_tab.c .
        

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherAES_ltc.sys %BIN_OUTPUT_DIR%

rem clean up copied files

del FreeOTFECypherAES_ltc.c
del FreeOTFECypherAES_ltc.h

del aes.c 
del aes_tab.c   

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\delete_lrw_driver_files.bat
call %PROJECT_DIR%\Common\bin\delete_xts_driver_files.bat

cd ..
