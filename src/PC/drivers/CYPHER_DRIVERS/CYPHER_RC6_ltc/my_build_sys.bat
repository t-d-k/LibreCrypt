@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_RC6_ltc\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\copy_lrw_driver_files.bat
call %PROJECT_DIR%\Common\bin\copy_xts_driver_files.bat

rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_RC6_ltc\FreeOTFECypherRC6_ltc.c .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_RC6_ltc\FreeOTFECypherRC6_ltc.h .

rem libtomcrypt library...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\ciphers\rc6.c .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherRC6_ltc.sys %BIN_OUTPUT_DIR%

del rc6.c
del FreeOTFECypherRC6_ltc.c
del FreeOTFECypherRC6_ltc.h

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
call %PROJECT_DIR%\Common\bin\delete_lrw_driver_files.bat
call %PROJECT_DIR%\Common\bin\delete_xts_driver_files.bat
cd ..

