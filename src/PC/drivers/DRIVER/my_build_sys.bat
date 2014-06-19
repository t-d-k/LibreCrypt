@echo off

rem Set the build environment
call ..\Common\bin\setup_env_common.bat
call ..\Common\bin\setup_env_driver.bat


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\DRIVER\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\Common\Common\src\* .
copy ..\..\..\..\Common\CYPHER_DRIVERS\Common\src\* .
copy ..\..\..\..\Common\DRIVER\src\* .
copy ..\..\Common\src\* .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFE.sys %BIN_OUTPUT_DIR%

