@echo off

rem Set the build environment
call ..\..\drivers\Common\bin\setup_env_common.bat



rem Move into the correct src directory...
%PROJECT_DRIVE%
cd %PROJECT_DIR%\..\cmdline_tools\DECRYPT_TWOFISH__Hifn_Counterpane\src

echo Building...

rem libtomcrypt library...
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\rmd160.c .

rem Hi/fn and Counterpane Systems library...
copy ..\..\..\3rd_party\Twofish\twofish-optimized-c\TWOFISH2.C . 


cl -I..\..\..\3rd_party\libtomcrypt\crypt-0.94 -I..\..\..\3rd_party\Twofish\twofish-optimized-c /Fe..\DECRYPT_TWOFISH__Hifn_Counterpane.exe main.c rmd160.c TWOFISH2.C

