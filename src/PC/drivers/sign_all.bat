@echo off

set /p pfxpass= Enter pfx password:

rem first sign all x86 drivers, then all amd64
set DOXBOX_FORCE_CPU=x86
:REPEAT

rem Set the build environment
call .\Common\bin\setup_env_common.bat
rem call setup_env_driver.bat
rem FreeOTFECypherTwofish_HifnCS.sys not working yet										
cd %BIN_OUTPUT_DIR%\
signtool.exe sign /f %PROJECT_DIR%\certs\tdk.pfx /p %pfxpass% /v /t http://timestamp.verisign.com/scripts/timstamp.dll FreeOTFE.sys ^
											FreeOTFECypherAES_ltc.sys FreeOTFECypherBlowfish.sys FreeOTFECypherCAST5.sys ^
											FreeOTFECypherCAST6_Gladman.sys FreeOTFECypherDES.sys FreeOTFECypherMARS_Gladman.sys  ^
											FreeOTFECypherRC6_ltc.sys FreeOTFECypherSerpent_Gladman.sys  ^
											FreeOTFECypherTwofish_ltc.sys FreeOTFEHashMD.sys ^
											FreeOTFEHashRIPEMD.sys FreeOTFEHashSHA.sys FreeOTFEHashTiger.sys FreeOTFEHashWhirlpool.sys 
											
cd alternate_drivers\
signtool.exe sign /f %PROJECT_DIR%\certs\tdk.pfx /p %pfxpass% /v /t http://timestamp.verisign.com/scripts/timstamp.dll ^
											FreeOTFECypherAES_Gladman.sys FreeOTFECypherRC6_Gladman.sys FreeOTFECypherTwofish_Gladman.sys 

cd ..\weak_drivers\

signtool.exe sign /f %PROJECT_DIR%\certs\tdk.pfx /p %pfxpass% /v /t http://timestamp.verisign.com/scripts/timstamp.dll ^
											FreeOTFECypherNull.sys  FreeOTFECypherXOR.sys FreeOTFEHashNull.sys 
											
cd %PROJECT_DIR%

if "%DOXBOX_FORCE_CPU%" == "amd64" goto END

set DOXBOX_FORCE_CPU=amd64
goto REPEAT

:END
