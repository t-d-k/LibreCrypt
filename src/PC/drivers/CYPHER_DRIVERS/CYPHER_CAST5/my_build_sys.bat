@echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)


rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\CYPHER_DRIVERS\CYPHER_CAST5\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat

rem Implementation...
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_CAST5\FreeOTFECypherCAST5.c .
copy %PROJECT_BASE_DIR%\src\Common\CYPHER_DRIVERS\CYPHER_CAST5\FreeOTFECypherCAST5.h .

rem libtomcrypt library...
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\ciphers\cast5.c .
copy %THIRD_PARTY_DIR%\libtomcrypt\crypt-1.17\src\misc\zeromem.c .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFECypherCAST5.sys %BIN_OUTPUT_DIR%

del FreeOTFECypherCAST5.c
del FreeOTFECypherCAST5.h
del cast5.c
del zeromem.c

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
cd ..

