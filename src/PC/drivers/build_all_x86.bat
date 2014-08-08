rem @echo off

rem ddk doesnt like changing target in same dos box - so two bat files
set DOXBOX_FORCE_CPU=x86


cd DRIVER
call my_build_sys.bat
cd ..\CYPHER_DRIVERS
echo DOXBOX_FORCE_CPU=%DOXBOX_FORCE_CPU%
call CYPHER_AES_Gladman\my_build_sys.bat       && cd ..\..
call CYPHER_AES_ltc\my_build_sys.bat           && cd ..\..
call CYPHER_BLOWFISH\my_build_sys.bat          && cd ..\..
call CYPHER_CAST5\my_build_sys.bat             && cd ..\..
call CYPHER_CAST6_Gladman\my_build_sys.bat     && cd ..\..
call CYPHER_DES\my_build_sys.bat               && cd ..\..
call CYPHER_MARS_Gladman\my_build_sys.bat      && cd ..\..
call CYPHER_NULL\my_build_sys.bat              && cd ..\..
call CYPHER_RC6_Gladman\my_build_sys.bat       && cd ..\..
call CYPHER_RC6_ltc\my_build_sys.bat           && cd ..\..
call CYPHER_SERPENT_Gladman\my_build_sys.bat   && cd ..\..
call CYPHER_TWOFISH_Gladman\my_build_sys.bat   && cd ..\..
rem call CYPHER_TWOFISH_HifnCS\my_build_sys.bat    && cd ..\.. not working yet
call CYPHER_TWOFISH_ltc\my_build_sys.bat       && cd ..\..
call CYPHER_XOR\my_build_sys.bat               && cd ..\..
cd ..\HASH_DRIVERS
call HASH_MD\my_build_sys.bat                  && cd ..\..
call HASH_NULL\my_build_sys.bat                && cd ..\..
call HASH_RIPEMD\my_build_sys.bat              && cd ..\..
call HASH_SHA\my_build_sys.bat                 && cd ..\..
call HASH_TIGER\my_build_sys.bat               && cd ..\..
call HASH_WHIRLPOOL\my_build_sys.bat           && cd ..\..
cd ..




