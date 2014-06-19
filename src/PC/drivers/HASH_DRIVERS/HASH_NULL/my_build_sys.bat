@echo off

rem Set the build environment
call ..\..\Common\bin\setup_env_common
call ..\..\Common\bin\setup_env_driver


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\HASH_DRIVERS\HASH_NULL\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\..\Common\Common\src\* .
copy ..\..\..\Common\src\* .
copy ..\..\Common\src\* .

rem Implementation...
copy ..\..\..\..\..\Common\HASH_DRIVERS\HASH_NULL\* .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFEHashNull.sys %BIN_OUTPUT_DIR%

