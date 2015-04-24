@echo off

set /p pfxpass= Enter pfx password:

rem first sign all x86 drivers, then all amd64


rem Set the build environment
set PROJECT_DRIVE=P:
set PROJECT_BASE_DIR=%PROJECT_DRIVE%\
set PROJECT_DIR=%PROJECT_BASE_DIR%src\PC\drivers

rem call setup_env_driver.bat
rem FreeOTFECypherTwofish_HifnCS.sys not working yet										
cd %PROJECT_BASE_DIR%\bin\PC\

"C:\Program Files (x86)\Microsoft SDKs\Windows\v7.0A\Bin\signtool.exe" sign /f %PROJECT_DIR%\certs\tdk.pfx /p %pfxpass% /v /t http://timestamp.verisign.com/scripts/timstamp.dll  ^
											DoxBox.exe DoxBoxExplorer.exe ..\..\install\Output\InstallDoxBox_v61Beta.exe
											
cd %PROJECT_BASE_DIR%\src

											

