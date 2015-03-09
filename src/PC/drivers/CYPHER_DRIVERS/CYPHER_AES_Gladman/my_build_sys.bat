@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_AES_Gladman\src

call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat


rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_AES_Gladman\FreeOTFECypherAES_Gladman.c .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_AES_Gladman\FreeOTFECypherAES_Gladman.h .


rem Gladman source...
copy %THIRD_PARTY_DIR%\AES_candidates_2nd_round_-_Gladman\aes.r2.algs\rijndael.c .

echo Building SYS...
rem build -gZ
build -gZ

rem Copying the binary over...

copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherAES_Gladman.sys %BIN_OUTPUT_DIR%\alternate_drivers\
echo copied FreeOTFECypherAES_Gladman.sys to %BIN_OUTPUT_DIR%

rem clean up copied files
call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat


del FreeOTFECypherAES_Gladman.c
del FreeOTFECypherAES_Gladman.h
del rijndael.c

cd ..
