@echo off

rem Launch this batch file using a shortcut like:
rem   C:\WINDOWS\system32\cmd.exe /k g:\WINDDK\3790\bin\setenv.bat g:\WINDDK\3790 chk WXP
rem   cmd.exe /k H:\UC\OTFE_PROJECT\MY_DRIVER\menu.bat

rem Set the VS environment
call vcvars32

//rem Set the SDK build environment
//call G:\MSSDK\SetEnv.Bat



echo Building...
h:
cd H:\UC\OTFE_PROJECT\MY_DRIVER\SOLUTION\FreeOTFE\CYPHER_DRIVERS\TWOFISH_DECRYPTER__tlc\src

copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\twofish.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\twofish_tab.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\crypt.c .
copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\cbc.c .

copy ..\..\..\3rd_party\libtomcrypt\crypt-0.94\strings.c .

echo Add PC-Lint directory to path...
PATH=C:\pc_lint;%PATH%
lin env-vc7.lnt -I..\..\..\3rd_party\libtomcrypt\crypt-0.94 %1 %2 %3 %4 %5 %6 %7 %8 %9

