@echo off

rem Set the build environment
rem call ..\..\PC\drivers\Common\bin\setup_env_common.bat

rem set PROJECT_DRIVE=P:
rem set PROJECT_DIR=P:\src\cmdline_tools\\

call ..\cmdline_common.bat
set PROJECT_DIR=%PROJECT_BASE_DIR%DECRYPT_TWOFISH__Hifn_Counterpane\


rem cd %PROJECT_DIR%\..\..\cmdline_tools\DECRYPT_TWOFISH__Hifn_Counterpane\src
cd %PROJECT_DIR%src

echo Building...

rem libtomcrypt library...
rem copy ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\rmd160.c .

rem Hi/fn and Counterpane Systems library...
rem copy ..\..\..\3rd_party\Twofish\twofish-optimized-c\TWOFISH2.C . 


cl -I%LTC_HDR_DIR% -I..\..\..\3rd_party\Twofish\twofish-optimized-c /Fe..\DECRYPT_TWOFISH__Hifn_Counterpane.exe main.c ..\..\..\3rd_party\libtomcrypt\crypt-1.17\src\hashes\rmd160.c ..\..\..\3rd_party\Twofish\twofish-optimized-c\TWOFISH2.C
cd ..
