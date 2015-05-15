rem comon vars for cmd line tools 'my_buikld.bat' files
rem set directories

echo set paths
set PROJECT_DRIVE=P:
set PROJECT_BASE_DIR=P:\src\cmdline_tools\

rem Move into the correct drive
%PROJECT_DRIVE%

rem abs paths
set LTC_DIR=%PROJECT_BASE_DIR%..\3rd_party\libtomcrypt\crypt-1.17\src\
set LTC_MISC=%LTC_DIR%misc\
set LTC_TWOFISH=%LTC_DIR%ciphers\twofish\
set LTC_AES=%LTC_DIR%ciphers\aes\
set LTC_CBC=%LTC_DIR%modes\cbc\
set LTC_CRYPT=%LTC_DIR%misc\crypt\
set LTC_HDR_DIR=%LTC_DIR%headers


