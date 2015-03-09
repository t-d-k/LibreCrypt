@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_BLOWFISH\src

call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat


rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_BLOWFISH\FreeOTFECypherBlowfish.c .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_BLOWFISH\FreeOTFECypherBlowfish.h .

rem libtomcrypt library...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\ciphers\blowfish.c .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherBLOWFISH.sys %BIN_OUTPUT_DIR%

rem clean up copied files
del FreeOTFECypherBlowfish.c
del FreeOTFECypherBlowfish.h
del blowfish.c

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
cd ..
