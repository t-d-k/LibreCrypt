unit OTFEConsts_U;
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  SysUtils; // Required for exceptions

const
  OTFE_ERR_SUCCESS          = $00; // No error
  OTFE_ERR_NOT_ACTIVE       = $01;
  OTFE_ERR_DRIVER_FAILURE   = $02;
  OTFE_ERR_USER_CANCEL      = $03;
  OTFE_ERR_WRONG_PASSWORD   = $04;
  OTFE_ERR_VOLUME_FILE_NOT_FOUND = $05;
  OTFE_ERR_INVALID_DRIVE    = $06; // You've attempted to mount a
  // volume using a drive letter
  // which is already in use
  OTFE_ERR_MOUNT_FAILURE    = $07;
  // Generic error; may be one of the other more specific ones
  OTFE_ERR_DISMOUNT_FAILURE = $08;
  // Generic error; may be one of the other more specific ones
  OTFE_ERR_FILES_OPEN       = $09;
  OTFE_ERR_STREAMING_DATA   = $0A;
  // Similar to OTFE_ERR_FILES_OPEN, used with ScramDisk NT
  OTFE_ERR_FILE_NOT_ENCRYPTED_VOLUME = $0B;
  OTFE_ERR_UNABLE_TO_LOCATE_FILE = $0C;
  OTFE_ERR_DISMOUNT_RECURSIVE = $0D; // You've attempted to dismount a
  // volume that contains a volume
  // file that is currently mounted
  OTFE_ERR_INSUFFICENT_RIGHTS = $0E; // You don't have sufficient rights
  // to perform the required operation
  // e.g. when using ScramDisk NT v3,
  // attempting to load the ScramDisk
  // driver with insufficient rights
  OTFE_ERR_NOT_W9X          = $0F; // Operation not available under
  // Windows 95/98/Me
  OTFE_ERR_NOT_WNT          = $10; // Operation not available under
  // Windows NT/2000
  OTFE_ERR_NO_FREE_DRIVE_LETTERS = $11;
  OTFE_ERR_VOLUME_FILE_FAILURE = $12; // Volume file does not contain
  // enough data, cannot read from
  // volume file


  // BestCrypt
  OTFE_ERR_UNKNOWN_ALGORITHM = $20;
  OTFE_ERR_UNKNOWN_KEYGEN    = $21;

  // ScramDisk
  OTFE_ERR_UNABLE_MOUNT_COMPRESSED = $30; // User tried to mount a volume
  // file that was compressed with
  // Windows NT/2000 compression
  // CrossCrypt
  OTFE_ERR_NO_FREE_DEVICES         = $40; // No more CrossCrypt disk devices
  // exist

  // FreeOTFE
  OTFE_ERR_HASH_FAILURE      = $50; // Hashing failed
  OTFE_ERR_CYPHER_FAILURE    = $51; // Encrypt/decrypt failed
  OTFE_ERR_MAC_FAILURE       = $52; // MAC failed
  OTFE_ERR_KDF_FAILURE       = $53; // KDF failed
  OTFE_ERR_PKCS11_SECRET_KEY_DECRYPT_FAILURE = $54; // Unable to decrypt CDB with
  // PKCS#11 secret key
  OTFE_ERR_KEYFILE_NOT_FOUND = $55; // Keyfile not found/couldn't be read

  // PANIC!
  OTFE_ERR_UNKNOWN_ERROR = $FF;



  OTFE_EXCPT_NOT_ACTIVE = 'OTFE component not connected - set Active to TRUE first';
  OTFE_EXCPT_NOT_W9X    = 'Operation not available under Windows 95/98/Me';
  OTFE_EXCPT_NOT_WNT    = 'Operation not available under Windows NT/2000';

type
  EOTFEException = Exception;

implementation

end.
