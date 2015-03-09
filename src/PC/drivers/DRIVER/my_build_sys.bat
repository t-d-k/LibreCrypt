rem @echo off

rem Set the build environment - if not set by build_all_x.bat 
if "%DOXBOX_FORCE_CPU%"=="" (
	call ..\Common\bin\setup_env_common.bat
	call ..\Common\bin\setup_env_driver.bat
)

rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\DRIVER\src

rem The build utility can't handle source files not being in the same dir
call %PROJECT_DIR%\Common\bin\copy_common_driver_files.bat
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\src\FreeOTFEBlockCalc.c                  .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\src\FreeOTFECallModuleFn.c               .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\src\FreeOTFEContext.c                    .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\src\FreeOTFEGenerateBlockIV.c            .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\src\FreeOTFEKDFHashSaltedPassword.c      .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\src\FreeOTFEKDFPBKDF2.c                  .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\src\FreeOTFEMACHash.c                    .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\src\FreeOTFEMACHMAC.c                    .

copy %PROJECT_BASE_DIR%\src\Common\Common\src\FreeOTFENULLGUID.c                    .

copy %PROJECT_BASE_DIR%\src\Common\DRIVER\inc\FreeOTFEBlockCalc.h                   .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\inc\FreeOTFECallModuleFn.h                .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\inc\FreeOTFEContext.h                     .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\inc\FreeOTFEGenerateBlockIV.h             .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\inc\FreeOTFEKDFHashSaltedPassword.h       .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\inc\FreeOTFEKDFPBKDF2.h                   .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\inc\FreeOTFEMACHash.h                     .
copy %PROJECT_BASE_DIR%\src\Common\DRIVER\inc\FreeOTFEMACHMAC.h                     .

copy %PROJECT_BASE_DIR%\src\Common\Common\inc\FreeOTFENULLGUID.h                    .

echo Building SYS...
build -gZ

rem Copying the binary over...
copy %FREEOTFE_OUTPUT_DIR%\FreeOTFE.sys %BIN_OUTPUT_DIR%

del FreeOTFEBlockCalc.c
del FreeOTFECallModuleFn.c
del FreeOTFEContext.c
del FreeOTFEGenerateBlockIV.c
del FreeOTFEKDFHashSaltedPassword.c
del FreeOTFEKDFPBKDF2.c
del FreeOTFEMACHash.c
del FreeOTFEMACHMAC.c 

del FreeOTFEBlockCalc.h
del FreeOTFECallModuleFn.h
del FreeOTFEContext.h
del FreeOTFEGenerateBlockIV.h
del FreeOTFEKDFHashSaltedPassword.h
del FreeOTFEKDFPBKDF2.h
del FreeOTFEMACHash.h
del FreeOTFEMACHMAC.h
del FreeOTFENULLGUID.h
del FreeOTFENULLGUID.c

call %PROJECT_DIR%\Common\bin\delete_common_driver_files.bat
cd ..
