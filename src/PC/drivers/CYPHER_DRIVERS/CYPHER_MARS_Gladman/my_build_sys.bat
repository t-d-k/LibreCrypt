@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_MARS_Gladman\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat

rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_MARS_Gladman\FreeOTFECypherMARS_Gladman.c .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_MARS_Gladman\FreeOTFECypherMARS_Gladman.h .

rem Gladman source...
copy %THIRD_PARTY_DIR%\AES_candidates_2nd_round_-_Gladman\aes.r2.algs\mars.c .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherMARS_Gladman.sys %BIN_OUTPUT_DIR%

del mars.c
del FreeOTFECypherMARS_Gladman.c
del FreeOTFECypherMARS_Gladman.h

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
cd ..

