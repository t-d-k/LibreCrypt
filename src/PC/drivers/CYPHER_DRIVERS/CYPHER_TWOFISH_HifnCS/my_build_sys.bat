@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_TWOFISH_HifnCS\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat

rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_TWOFISH_HifnCS\FreeOTFECypherTwofish_HifnCS.c .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_TWOFISH_HifnCS\FreeOTFECypherTwofish_HifnCS.h .

rem Hi/fn and Counterpane Systems library...
copy %THIRD_PARTY_DIR%\Twofish\twofish-optimized-c\TWOFISH2.C .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherTwofish_HifnCS.sys %BIN_OUTPUT_DIR%

del TWOFISH2.C
del FreeOTFECypherTwofish_HifnCS.c
del FreeOTFECypherTwofish_HifnCS.h

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
cd ..
