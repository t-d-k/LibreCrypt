unit OTFEStrongDiskConsts_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes, SysUtils, Windows;


type
  EStrongDiskError = Exception;
//  EStrongDiskNotConnected = EStrongDiskError;
//  EStrongDiskError = Exception;
//  EStrongDiskNotConnected = EStrongDiskError;
//  EStrongDiskExeNotFound = EStrongDiskError;
  EStrongDiskDLLNotFound = EStrongDiskError;
  EStrongDiskBadDLL = EStrongDiskError;
  EStrongDiskDLLFailure = EStrongDiskError;

const

//  E_NOT_CONNECTED = 'StrongDisk not connected - set Active to TRUE first';
//  E_BAD_STATUS = 'StrongDisk driver returned bad status';
//  E_DRIVE_IN_USE = 'Close all windows and applications used by this drive first';
//  E_STRONGDISK_EXE_NOT_FOUND = 'The StrongDisk executable could not be found';
  E_STRONGDISK_DLL_NOT_FOUND = 'StrongDisk DLL not found';
  E_STRONGDISK_BAD_DLL = 'StrongDisk DLL did not have correct facilities!';
  E_STRONGDISK_DLL_FAILURE = 'StrongDisk DLL returned unexpected value';


  STRONGDISK_REGISTRY_ENTRY_PATH = '\SOFTWARE\PhysTechSoft\StrongDisk';
  
  STRONGDISK_EXE         = 'strdisk.exe';
  STRONGDISK_MAIN_DLL    = 'strdisk.dll';
  // Loading the support DLL is pretty horrible, but required by strdisk.dll
  // cl32.dll is a copy of Peter Gutmann et al.'s cryptlib encryption library.
  STRONGDISK_SUPPORT_DLL_CRYPTO = 'cl32.dll';
  // Again, loading the support DLL is pretty horrible, but required by strdisk.dll
  STRONGDISK_SUPPORT_DLL_LANG = 'Language.dll';


  DLL_FUNCTIONNAME_SDMOUNT       = 'SDMount';
  DLL_FUNCTIONNAME_SDDISMOUNT    = 'SDDismount';
  DLL_FUNCTIONNAME_SDDISMOUNTALL = 'SDDismountAll';
  DLL_FUNCTIONNAME_SDGETDRIVES   = 'SDGetDrives';
  DLL_FUNCTIONNAME_SDGETINFO     = 'SDGetInfo';


// ERROR CODES
SDE_OK                       =  0; // operation succeded
SDE_ERROR                    =  1; // operation failed; unexpected error
SDE_UNABLE_LOAD_DRIVER       =  2; // unable load a driver; possible causes: not sufficient system resources; the driver is not installed
SDE_NOT_ENOUGH_MEMORY        =  3; // not enough memory
SDE_ACCESS_DENIED            =  4; // failed to open or to read a file
SDE_UNMOUNT_OPEN_FILE        =  5; // unable unmount a secure drive due to open file
SDE_NOT_SDDRIVE              =  6; // the drive specified is not a StrongDisk drive
SDE_MOUNT_ON_SDDRIVE         =  7; // attempting to mount a StrongDisk image from a StrongDisk drive; StrongDisk drives don't support recursion
SDE_NOT_REGISTERED           =  8; // mounting the StrongDisk drive is not allowed by the registration
SDE_INVALID_IMAGE            =  9; // the file specified is not a StrongDisk drive image
SDE_UNSUPPORTED_ALGORITHM    = 10; // the used cryptographic library does not support an algorithm that is used in the image
SDE_INVALID_PASSWORD         = 11; // the password specified is invalid
SDE_KEYFILE_NOT_READY        = 12; // device (a drive) is not ready
SDE_KEYFILE_SHORT            = 13; // the file specified is too short to be a key
SDE_KEYFILE_NOT_FOUND        = 14; // the key file is not found
SDE_KEYFILE_ERROR            = 15; // unable to open or to read the key file
SDE_ELKEY_UNABLE_OPEN_DEVICE = 16; // unable to open an electronic key
SDE_ELKEY_UNABLE_OPEN_FILE   = 17; // unable to open key file on the electronic key
SDE_ELKEY_UNABLE_READ_FILE   = 18; // unable to read key file on the electronic key
SDE_ELKEY_ERROR              = 19; // access to electronic key failed

// SDE_INVALID_PARAMETER_1: the first parameter is invalid
SDE_INVALID_PARAMETER_1   = 1000;
SDE_INVALID_PARAMETER_2   = 1001;
SDE_INVALID_PARAMETER_3   = 1002;
SDE_INVALID_PARAMETER_4   = 1003;
SDE_INVALID_PARAMETER_5   = 1004;
SDE_INVALID_PARAMETER_6   = 1005;
SDE_INVALID_PARAMETER_7   = 1006;
SDE_INVALID_PARAMETER_8   = 1007;
SDE_INVALID_PARAMETER_9   = 1008;
SDE_INVALID_PARAMETER_10  = 1009;
// ...
SDE_INVALID_PARAMETER_100 = 1099; // the 100th parameter is invalid



// KEY MODES
SDKM_PASSWORD = 1;
SDKM_KEYFILE  = 2;
SDKM_ELKEY    = 4;

SDKM_ALL = (SDKM_PASSWORD or SDKM_KEYFILE or SDKM_ELKEY);


implementation


END.


