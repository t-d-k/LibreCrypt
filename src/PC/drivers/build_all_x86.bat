rem @echo off

rem ddk doesnt like changing target in same environment - so two bat files, close and reopen terminal before running other
set DOXBOX_FORCE_CPU=x86

rem these fail to call setup_env_driver.bat if dir wrong
rem Set the build environment
call .\Common\bin\setup_env_common
call .\Common\bin\setup_env_driver

echo DOXBOX_FORCE_CPU=%DOXBOX_FORCE_CPU%

rem changes drive
%PROJECT_DRIVE%
cd  %PROJECT_DIR%

rem cd DRIVER
rem call my_build_sys.bat

call DRIVER\my_build_sys.bat 
echo BIN_OUTPUT_DIR=%BIN_OUTPUT_DIR%
echo FREEOTFE_CPU=%FREEOTFE_CPU%
echo FREEOTFE_TARGET=%FREEOTFE_TARGET%

cd ..\CYPHER_DRIVERS

call CYPHER_AES_Gladman\my_build_sys.bat       && cd ..
call CYPHER_AES_ltc\my_build_sys.bat           && cd ..
call CYPHER_BLOWFISH\my_build_sys.bat          && cd ..
call CYPHER_CAST5\my_build_sys.bat             && cd ..
call CYPHER_CAST6_Gladman\my_build_sys.bat     && cd ..
call CYPHER_DES\my_build_sys.bat               && cd ..
call CYPHER_MARS_Gladman\my_build_sys.bat      && cd ..
call CYPHER_NULL\my_build_sys.bat              && cd ..
call CYPHER_RC6_Gladman\my_build_sys.bat       && cd ..
call CYPHER_RC6_ltc\my_build_sys.bat           && cd ..
call CYPHER_SERPENT_Gladman\my_build_sys.bat   && cd ..
call CYPHER_TWOFISH_Gladman\my_build_sys.bat   && cd ..
rem call CYPHER_TWOFISH_HifnCS\my_build_sys.bat    && cd .. not working yet
call CYPHER_TWOFISH_ltc\my_build_sys.bat       && cd ..
call CYPHER_XOR\my_build_sys.bat               && cd ..
cd ..\HASH_DRIVERS
call HASH_MD\my_build_sys.bat                  && cd ..
call HASH_NULL\my_build_sys.bat                && cd ..
call HASH_RIPEMD\my_build_sys.bat              && cd ..
call HASH_SHA\my_build_sys.bat                 && cd ..
call HASH_TIGER\my_build_sys.bat               && cd ..
call HASH_WHIRLPOOL\my_build_sys.bat           && cd ..
cd ..




