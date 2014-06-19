Notes For Developers Using TkrScramDisk and/or TEnhKrScramDisk
--------------------------------------------------------------
14th February 2000

This file contains information detailing the differences between
TkrScramDisk/TEnhKrScramDisk and the new TOTFEScramDisk component.

It is expected that some applications may require minor code changes, but these
should be minimal.

In brief, TOTFEScramDisk replaces both TkrScramDisk and TEnhKrScramDisk,
allowing for greater compatability and allows other OTFE components to be
"dropped in" should applications need to support another OTFE systems.


Full credit it due to Andy Jeffries at Kwik-Rite Development for the development
of the original TkrScramDisk, and giving me the initial idea of writing
components to support other OTFE systems.


How does TOTFEScramDisk differ from TkrScramDisk?
  * Many enhancements
  * TOTFEScramDisk is derived from TOTFE
  * WAV file steganography implemented
  * Added const for max # of slots (trivial, I know, but...)
  * Procedure:
      procedure Dismount(Drive : string; Brutal : Boolean = TRUE);
    replaced with the following two from TOTFE:
      function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
      function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
    and no longer raises an exception if the drive cannot be dismounted,
    instead the function just returns FALSE
  * Function:
      function  DismountAll(brutal : boolean) : boolean;
    replaced with the following from TOTFE:
      function  DismountAll(emergency: boolean = FALSE): string;
  * Procedure:
      procedure ClearPasswords();
    renamed:
      procedure ClearAllPasswords();
    and bug fixed such that *all* the passwords in the ScramDisk device driver
    are cleared, not just the last one entered
  * Function:
      function MountContainer(Filename: string): boolean;
    replaced with the following from TOTFE:
      function  Mount(volumeFilename: string; readonly: boolean = FALSE): char;
  * Registered to appear on the OTFE palette, together with the other OTFE
    components
  * The modified implementation of the SHA-1 algorithm by Dave Barton has
    been replaced with the more modular hash algorithm from the "Hashes" package
  * To help with backward compatability, TkrScramDisk is now a TOTFEScramDisk
  * Bugfixes and other enhancements

How does TOTFEScramDisk differ from TEnhKrScramDisk?
  * WAV file steganography implemented
  * TOTFEScramDisk is derived from TOTFE
  * Fixed bug with GetSlotInfo
  * Added const for max # of slots (trivial, I know, but...)
  * Function:
      function TScramDisk.MountPartitionsPrompted(): boolean;
    replaced with:
      function TScramDisk.MountPartitionsPrompted(): string;
    and returns a list of drives mounted on success, or an empty string on
    failure
  * Procedure:
      procedure Dismount(Drive : string; Brutal : Boolean = TRUE);
    replaced with the following two from TOTFE:
      function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
      function  Dismount(driveLetter: char; emergency: boolean = FALSE): boolean;
    and no longer raises an exception if the drive cannot be dismounted,
    instead the function just returns FALSE
  * Procedure:
      procedure ClearPasswords();
    renamed:
      procedure ClearAllPasswords();
    and bug fixed such that *all* the passwords in the ScramDisk device driver
    are cleared, not just the last one entered
  * Function:
      function MountContainer(Filename: string): boolean;
    replaced with the following from TOTFE:
      function  Mount(volumeFilename: string; readonly: boolean = FALSE): char;
  * Function:
      function  MountContainer(filename: string; readonly: boolean): boolean;
    replaced with the following from TOTFE:
      function  MountContainer(filename: string; readonly: boolean; clearPasswordsAfterwards: boolean): boolean;
    (the additional parameter determines whether the function clears all the
    driver's passwords before exiting or not)
    Use of this function is strongy discouraged, and it is recommended that
    Mount(...) be used instead.
  * Registered to appear on the OTFE palette, consistent with the other OTFE
    components
  * The modified implementation of the SHA-1 algorithm by Dave Barton has
    been replaced with the more modular hash algorithm from the "Hashes" package
  * To help with backward compatability, TEnhKrScramDisk is now a TOTFEScramDisk
  * Bugfixes and other enhancements

