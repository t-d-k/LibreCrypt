@echo off

rem Set the build environment
call ..\..\Common\bin\setup_env_common
call ..\..\Common\bin\setup_env_driver


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_CAST6_Gladman\src

rem The build utility can't handle source files not being in the same dir
copy ..\..\..\..\..\Common\Common\src\* .
copy ..\..\..\..\..\Common\CYPHER_DRIVERS\Common\src\* .
copy ..\..\..\Common\src\* .
copy ..\..\Common\src\* .

rem Implementation...
copy ..\..\..\..\..\Common\CYPHER_DRIVERS\CYPHER_CAST6_Gladman\* .

rem Gladman source...
copy ..\..\..\..\..\3rd_party\AES_candidates_1st_round_-_Gladman\cast.c .

echo Building SYS...
build -cgZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherCAST6_Gladman.sys %BIN_OUTPUT_DIR%

