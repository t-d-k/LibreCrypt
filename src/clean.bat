@echo off

rem Files in the drivers\Common\src, etc dir which may have been copied to
rem individual driver dirs
set COMMON_SRC_FILES=FreeOTFEDebug.c FreeOTFElib.c FreeOTFEDriverlib.c FreeOTFENULLGUID.c SDUGeneral.* SDUHexDump.* SDUEndianIntegers.* SDULinkedList.* SDUi18n.* SDUi18n_GUI.* FreeOTFECypherAPICommon.c FreeOTFECypherImpl.c FreeOTFEContext.c FreeOTFEBlockCalc.c

rem Files in the drivers\DRIVER\Common\src dir which may have been copied to
rem individual driver dirs
set COMMON_DRIVER_SRC_FILES=FreeOTFECallModuleFn.c FreeOTFEGenerateBlockIV.c FreeOTFEKDFHashSaltedPassword.c FreeOTFEKDFPBKDF2.c FreeOTFEMACHash.c FreeOTFEMACHMAC.c 

rem Files in the drivers\CYPHER_DRIVERS\Common\src, etc dir which may have been copied to
rem individual driver dirs
set COMMON_CYPHER_SRC_FILES=FreeOTFECypherDriver.c

rem Files in the drivers\HASH_DRIVERS\Common\src dir which may have been copied to
rem individual driver dirs
set COMMON_HASH_SRC_FILES=FreeOTFEHashDriver.c

rem Output files are stored in these dirs relative to the driver's
rem ...\PC\XXX_DRIVERS\XXX\src dir 
set COMMON_OUTPUT_DIRS=..\Debug ..\Release objchk_w2k_x86 objfre_w2k_x86 objchk_wxp_x86 objfre_wxp_x86 objchk_wnet_x86 objfre_wnet_x86 objchk_w2k_amd64 objfre_w2k_amd64 objchk_wxp_amd64 objfre_wxp_amd64 objchk_wnet_amd64 objfre_wnet_amd64 objchk_wlh_amd64 objfre_wlh_amd64

rem Dir which is created for each PDA project
set PDA_BUILD_DIRS="Pocket PC 2003 (ARMV4)" Debug Release


rem ======================================================
echo Cleaning up the Delphi files...

echo ...Files created during building
cd PC\gui
del /s *.dcu
del /s *.ddp
del /s *.dproj.local
del /s *.identcache
del /s *.~*
rem "xargs" is from UnxUtils; DOS port of the UNIX command, as is "rm"
dir /b /s /ad __history | xargs -n 1 rm -r
cd ..\..



rem ======================================================
echo Cleaning up the driver C files...

echo ...Files created during building
cd PC\drivers
rmdir /q /s Lint
del /s *.lnt
del /s *.log
del /s *.exe
del /s *.sys
del /s *.obj
del /s *.res
del /s *.mac
del /s *.pdb
del /s *.err
del /s *.lnt
del /s BuildLog.htm
cd ..\..


rem ------------------------------------------------------
echo ...Clean up main driver

cd PC\drivers\DRIVER\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_DRIVER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..


rem ------------------------------------------------------
echo ...Clean up hash drivers

cd PC\drivers\HASH_DRIVERS\HASH_MD\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_HASH_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFEHashMD.h
del FreeOTFEHashMD.c
rem libtomcrypt library...
del md2.c
del md4.c
del md5.c
del zeromem.c
cd ..\..\..\..\..

cd PC\drivers\HASH_DRIVERS\HASH_NULL\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_HASH_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFEHashNull.h
del FreeOTFEHashNull.c
cd ..\..\..\..\..

cd PC\drivers\HASH_DRIVERS\HASH_RIPEMD\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_HASH_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFEHashRIPEMD.h
del FreeOTFEHashRIPEMD.c
rem libtomcrypt library...
del rmd128.c
del rmd160.c
del rmd256.c
del rmd320.c
cd ..\..\..\..\..

cd PC\drivers\HASH_DRIVERS\HASH_SHA\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_HASH_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFEHashSHA.h
del FreeOTFEHashSHA.c
rem libtomcrypt library...
del sha1.c
del sha224.c
del sha256.c
del sha384.c
del sha512.c
cd ..\..\..\..\..

cd PC\drivers\HASH_DRIVERS\HASH_TIGER\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_HASH_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFEHashTiger.h
del FreeOTFEHashTiger.c
rem libtomcrypt library...
del tiger.c
cd ..\..\..\..\..

cd PC\drivers\HASH_DRIVERS\HASH_WHIRLPOOL\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_HASH_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFEHashWhirlpool.h
del FreeOTFEHashWhirlpool.c
rem libtomcrypt library...
del whirl.c
del whirltab.c
del zeromem.c
cd ..\..\..\..\..


rem ------------------------------------------------------
echo ...Clean up cypher drivers

cd PC\drivers\CYPHER_DRIVERS\CYPHER_AES_ltc\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherAES_ltc.h
del FreeOTFECypherAES_ltc.c
rem libtomcrypt library...
del aes.c
del aes_tab.c
del crypt_cipher_descriptor.c
del crypt_cipher_is_valid.c
del crypt_find_cipher.c
del crypt_register_cipher.c
del crypt_unregister_cipher.c
del cbc_start.c
del cbc_encrypt.c
del cbc_decrypt.c
del cbc_done.c
del lrw_decrypt.c
del lrw_encrypt.c
del lrw_process.c
del lrw_setiv.c
del lrw_start.c
del lrw_done.c
del gcm_gf_mult.c
del xts_decrypt.c
del xts_encrypt.c
del xts_init.c
del xts_mult_x.c
del xts_done.c
del zeromem.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_DES\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherDES.h
del FreeOTFECypherDES.c
rem libtomcrypt library...
del des.c
del crypt_cipher_descriptor.c
del crypt_cipher_is_valid.c
del crypt_find_cipher.c
del crypt_register_cipher.c
del crypt_unregister_cipher.c
del cbc_start.c
del cbc_encrypt.c
del cbc_decrypt.c
del cbc_done.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_BLOWFISH\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherBlowfish.h
del FreeOTFECypherBlowfish.c
rem libtomcrypt library...
del blowfish.c
del crypt_cipher_descriptor.c
del crypt_cipher_is_valid.c
del crypt_find_cipher.c
del crypt_register_cipher.c
del crypt_unregister_cipher.c
del cbc_start.c
del cbc_encrypt.c
del cbc_decrypt.c
del cbc_done.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_RC6_ltc\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherRC6_ltc.h
del FreeOTFECypherRC6_ltc.c
rem libtomcrypt library...
del rc6.c
del crypt_cipher_descriptor.c
del crypt_cipher_is_valid.c
del crypt_find_cipher.c
del crypt_register_cipher.c
del crypt_unregister_cipher.c
del cbc_start.c
del cbc_encrypt.c
del cbc_decrypt.c
del cbc_done.c
del lrw_decrypt.c
del lrw_encrypt.c
del lrw_process.c
del lrw_setiv.c
del lrw_start.c
del lrw_done.c
del gcm_gf_mult.c
del xts_decrypt.c
del xts_encrypt.c
del xts_init.c
del xts_mult_x.c
del xts_done.c
del zeromem.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_CAST5\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherCAST5.h
del FreeOTFECypherCAST5.c
rem libtomcrypt library...
del cast5.c
del crypt_cipher_descriptor.c
del crypt_cipher_is_valid.c
del crypt_find_cipher.c
del crypt_register_cipher.c
del crypt_unregister_cipher.c
del cbc_start.c
del cbc_encrypt.c
del cbc_decrypt.c
del cbc_done.c
del zeromem.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_NULL\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherNull.h
del FreeOTFECypherNull.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_TWOFISH_HifnCS\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherTwofish_HifnCS.h
del FreeOTFECypherTwofish_HifnCS.c
del TWOFISH2.C
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_TWOFISH_ltc\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherTwofish_ltc.h
del FreeOTFECypherTwofish_ltc.c
rem libtomcrypt library...
del twofish.c
del twofish_tab.c
del crypt_cipher_descriptor.c
del crypt_cipher_is_valid.c
del crypt_find_cipher.c
del crypt_register_cipher.c
del crypt_unregister_cipher.c
del cbc_start.c
del cbc_encrypt.c
del cbc_decrypt.c
del cbc_done.c
del lrw_decrypt.c
del lrw_encrypt.c
del lrw_process.c
del lrw_setiv.c
del lrw_start.c
del lrw_done.c
del gcm_gf_mult.c
del xts_decrypt.c
del xts_encrypt.c
del xts_init.c
del xts_mult_x.c
del xts_done.c
del zeromem.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_SERPENT_Gladman\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherSerpent_Gladman.h
del FreeOTFECypherSerpent_Gladman.c
rem Modified libtomcrypt XTS library...
del ltc_gladman_xts_init.c
del ltc_gladman_xts_done.c
del ltc_gladman_xts_encrypt.c
del ltc_gladman_xts_decrypt.c
del ltc_gladman_xts_mult_x.c
rem Gladman library...
del serpent.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_MARS_Gladman\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherMARS_Gladman.h
del FreeOTFECypherMARS_Gladman.c
rem Modified libtomcrypt XTS library...
del ltc_gladman_xts_init.c
del ltc_gladman_xts_done.c
del ltc_gladman_xts_encrypt.c
del ltc_gladman_xts_decrypt.c
del ltc_gladman_xts_mult_x.c
rem Gladman library...
del mars.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_TWOFISH_Gladman\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherTwofish_Gladman.h
del FreeOTFECypherTwofish_Gladman.c
rem Modified libtomcrypt XTS library...
del ltc_gladman_xts_init.c
del ltc_gladman_xts_done.c
del ltc_gladman_xts_encrypt.c
del ltc_gladman_xts_decrypt.c
del ltc_gladman_xts_mult_x.c
rem Gladman library...
del twofish.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_RC6_Gladman\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherRC6_Gladman.h
del FreeOTFECypherRC6_Gladman.c
rem Modified libtomcrypt XTS library...
del ltc_gladman_xts_init.c
del ltc_gladman_xts_done.c
del ltc_gladman_xts_encrypt.c
del ltc_gladman_xts_decrypt.c
del ltc_gladman_xts_mult_x.c
rem Gladman library...
del rc6.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_AES_Gladman\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherAES_Gladman.h
del FreeOTFECypherAES_Gladman.c
rem Modified libtomcrypt XTS library...
del ltc_gladman_xts_init.c
del ltc_gladman_xts_done.c
del ltc_gladman_xts_encrypt.c
del ltc_gladman_xts_decrypt.c
del ltc_gladman_xts_mult_x.c
rem Gladman library...
del rijndael.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_CAST6_Gladman\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherCAST6_Gladman.h
del FreeOTFECypherCAST6_Gladman.c
rem Gladman library...
del cast.c
cd ..\..\..\..\..

cd PC\drivers\CYPHER_DRIVERS\CYPHER_XOR\src
for %%x in (%COMMON_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_CYPHER_SRC_FILES%) do del /s %%x
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del FreeOTFECypherXOR.h
del FreeOTFECypherXOR.c
cd ..\..\..\..\..


rem ======================================================
echo Cleaning up the command line decryption utility C files...

echo ...Files created during building
cd cmdline_tools
del /s *.log
del /s *.exe
del /s *.sys
del /s *.obj
del /s *.res
del /s *.mac
del /s *.pdb
del /s *.err
del /s *.lnt
del /s BuildLog.htm
cd ..


echo ...Clean up command line decryption utilities
cd cmdline_tools\DECRYPT_AES__2K__tlc\src
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del aes.c
del aes_tab.c
del crypt.c
del cbc.c
del strings.c
del mem.c
cd ..\..\..

cd cmdline_tools\DECRYPT_AES__tlc\src
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del aes.c
del aes_tab.c
del crypt.c
del cbc.c
del strings.c
del mem.c
cd ..\..\..

cd cmdline_tools\DECRYPT_TWOFISH__Hifn_Counterpane\src
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
rem libtomcrypt library...
del rmd160.c
rem Hi/fn and Counterpane Systems library...
del TWOFISH2.C
cd ..\..\..

cd cmdline_tools\DECRYPT_TWOFISH__tlc\src
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
del twofish.c
del twofish_tab.c
del crypt.c
del cbc.c
del strings.c
cd ..\..\..

cd cmdline_tools\DECRYPT_XOR\src
for %%x in (%COMMON_OUTPUT_DIRS%) do rmdir /q /s %%x
cd ..\..\..

rem ------------------------------------------------------
echo Cleaning up the PDA files...

echo ...Files created during building PDA version

echo ...Unwanted output created for PDA version
cd ..\bin\PDA
for %%x in (%PDA_BUILD_DIRS%) do del /q /s %%x\*.exp
for %%x in (%PDA_BUILD_DIRS%) do del /q /s %%x\*.lib
for %%x in (%PDA_BUILD_DIRS%) do del /q /s %%x\*.pdb
cd ..\..\src
cd PDA
rmdir /q /s Lint
del /s *.lnt
del /s *.err
cd ..

echo ...Clean up GUI
cd PDA\gui
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..

echo ...Clean up main driver
cd PDA\drivers\DRIVER
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..

echo ...Clean up hash drivers

cd PDA\drivers\HASH_DRIVERS\HASH_MD
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\HASH_DRIVERS\HASH_NULL
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\HASH_DRIVERS\HASH_RIPEMD
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\HASH_DRIVERS\HASH_SHA
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\HASH_DRIVERS\HASH_TIGER
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\HASH_DRIVERS\HASH_WHIRLPOOL
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..


rem ------------------------------------------------------
echo ...Clean up cypher drivers

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_AES_ltc
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_DES
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_BLOWFISH
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_RC6_ltc
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_CAST5
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_NULL
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_TWOFISH_HifnCS
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_TWOFISH_ltc
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_SERPENT_Gladman
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_MARS_Gladman
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_TWOFISH_Gladman
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_RC6_Gladman
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_AES_Gladman
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_CAST6_Gladman
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..

cd PDA\drivers\CYPHER_DRIVERS\CYPHER_XOR
for %%x in (%PDA_BUILD_DIRS%) do rmdir /q /s %%x
cd ..\..\..\..



rem ------------------------------------------------------
echo Finished cleaning.
rem ------------------------------------------------------

