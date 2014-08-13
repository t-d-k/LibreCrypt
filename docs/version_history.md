

<meta content="text/html; charset=iso-8859-1" http-equiv="Content-Type">
<meta name="keywords" content="disk encryption, security, transparent, AES, OTFE, plausible deniability, virtual drive, Linux, MS Windows, portable, USB drive, partition">
<meta name="description" content="DoxBox: An OpenSource 'on-the-fly' transparent disk encryption program for PCs. Using this software, you can create one or more &quot;virtual disks&quot; on your PC - anything written to these disks is automatically, and securely, encrypted before being stored on your computers hard drive.">

<meta name="author" content="Sarah Dean">
<meta name="copyright" content="Copyright 2004, 2005, 2006, 2007, 2008 Sarah Dean">
<meta name="ROBOTS" content="ALL">

<TITLE>Appendix A: Version History</TITLE>

<link href="./styles_common.css" rel="stylesheet" type="text/css">

<link rev="made" href="mailto:sdean12@sdean12.org">
<link rel="shortcut icon" href="./images/favicon.ico" type="image/x-icon">

<SPAN CLASS="master_link">
[![DoxBox logo](./images/FreeOTFE.gif)](http://DoxBox.squte.com/)
[DoxBox](http://DoxBox.squte.com/)
</SPAN>
<SPAN CLASS="master_title">
_OpenSource disk encryption for Windows_
</SPAN>

      
            
## Appendix A: Version History

<UL>
  * [PC Version](#level_3_heading_1)
  * [PDA Version](#level_3_heading_2)
</UL>

* * * 
<A NAME="level_3_heading_1">
### PC Version
</A>

<UL>
  * v5.21 (7th February 2010)
  <UL>
    * Added Russian language translation
    * Added translator credit to "About..." dialog
    * Minor tweaks to portableapps .com launcher
  </UL>

  * v5.20 (3rd January 2010)
  <UL>
    * Added support for using keyfiles with LUKS volumes (Hint: For ease of use, keyfiles may also be dragged and dropped onto the LUKS password entry dialog)
    * Changed disk/partition selection dialog to treat doubleclicking on a disk/partition as selecting it. Selected disk/partition properties can now be accessed via context menu item
    * When mounting a file based volume which is readonly, the readonly checkbox will automatically be selected
    * Change made such that if FreeOTFE's MRU list is disabled (this is the default), then volumes, keyfiles, etc will not appear on the MS Windows system MRU list either.
    * Added Greek language translation
    * Added Croatian language translation
    * Cosmetic display tweaks to better handle translations
  </UL>

  * v5.10 (13th October 2009)
  <UL>
    * Added volume padding option when creating new volumes
    * Added Czech translation
    * Added Japanese translation
    * Updated German and Spanish translations
    * Added option to automatically start FreeOTFE on system startup
    * Added option to let user define action carried out on clicking/doubleclicking system tray icon
    * Added options to allow/prevent newlines and tabs in passwords
    * Added "/minimize" command line switch, and modified to honor "run minimized" option when launched via a shortcut
    * Fixed bug causing hotkeys to be registered even if run with commandline arguments.
    * Fixed bug causing emergency dismount hotkey to be treated as normal dismount hotkey
    * Various cosmetic improvements
  </UL>

  * v5.00 (22nd July 2009)
  <UL>
    * Significant performance improvements made to driver code
    * Added XTS mode to Gladman cyphers (AES, MARS, RC-6, Serpent and Twofish)
    * Added RIPEMD-256 and RIPEMD-320 hash algorithms
    * Added support for dropping keyfiles onto the password entry dialog to simplify keyfile filename entry
    * Added automatic check for updates (turned _off_ by default)
    * Allow option to change the default mount type used
    * Allow various options to turn on/off confirmation/warning messages
    * Allow users to mount volumes as A: or B: if these drive letters aren't in use. Note: Selecting "Use default" for the drive to mount as will still result in using the first unused drive letter after C: - A:/B: may only be used if explicitly selected by the user
    * Added support for "refresh" multimedia key
    * Added "/create" command line option for creating new volumes
    * Added "/settings" command line parameter to read options from user specified location
    * Created U3 and portableapps.com specific installers, simplifying installation for users of these particular systems
    * Removed restriction requiring hidden volumes to begin at an offset which is a multiple of 512 bytes
    * Allow mounting either Linux LUKS or FreeOTFE partitions via the "Partition" toolbar button and "File | Mount partition..." menuitem (previously only FreeOTFE partitions could be mounted this way)
    * Fixed minor bug which prevented hidden volumes from being automatically mounted after being created
    * Fixed minor bug with "minimise to system tray" functionality
    * Fixed bug causing main window to resize continuously if toolbar not shown
  </UL>

  * v4.71 (19th April 2009)
  <UL>
    * Added "/saltlength" and "/keyiterations" command line options
    * Added additional check on saltlength when creating new volumes
    * Fixed trivial bug introduced in v4.70, causing CDB dumps to report the hash driver used twice, instead of the hash driver and cypher driver used.
    * Fixed bug introduced in v4.70, which prevented per-volume IVs from being passed to the backend driver. Only affects CBC based (i.e. very old) volumes which use per-volume IVs.
  </UL>

  * v4.70 (4th April 2009)
  <UL>
    * Spanish translation added
    * Added option to volume creation wizard to mount new volumes after creating them
    * Added support for Windows 7
    * Added autodetect functionality for PKCS#11 libraries
    * GUI and driver code refactored to allow common code to be shared between FreeOTFE and FreeOTFE Explorer
    * Fixed uninstaller bug which prevented it from cleaning out installation subdirectories
    * Fixed bug causing GUI to go into a tight loop when switching to a language for which the translation required certain specific controls to be resized
    * Fixed bug which could potentially cause FreeOTFE to crash on dismount
  </UL>

  * v4.60 (19th February 2009)
  <UL>
    * French translation added
    * Corrected fault causing exit status 102 to be returned when mounting volume from the command line with "/silent" specified
    * Various cosmetic improvements (mainly relating to translations)
  </UL>

  * v4.50 (1st January 2009)
  <UL>
    * Italian translation added
    * Added option to prevent message showing on successful mount
    * Added option to display large or small toolbar icons (Note: Large icons with captions now shown by default. This may be changed back to show small icons via the "Options" dialog)
    * Added option to save FreeOTFE settings to the Windows registry instead of a configuration file
    * Added MRU list to system tray icon's menu. (Note: The MRU list is disabled by default; enable via the "Options" dialog)
    * Added "/offset" command line option for mounting hidden volumes from the command line
    * Message catalog system to simplify translation and reduce risk of errors
    * Change to prevent changing the CWD when browsing for files via the standard open/save dialogs. This should stop FreeOTFE preventing USB drive removal when keyfiles, etc are stored on USB drives.
    * Changed the broadcast message system informing other windows of drives appearing/disappearing on mount/dismount
    * Corrected fault causing drive overwrite to fail under some circumstances with volumes over 4GB
    * Refactored some parts of the codebase to eliminate duplicated code
    * Various cosmetic tweaks to improve display layout when translated into different languages
  </UL>

  * v4.40 (20th November 2008)
  <UL>
    * German translation added
    * Added "Tools" menu item to provide a straightforward user interface to allow the user to quickly and easily copy FreeOTFE to a USB drive, and optionally configure it to run automatically when the drive is plugged in.
    * Changed default settings to display system tray icon by default
    * Improved support for dismounting drives nested within each other
    * Added "/noexit" command line option
    * Added "/password" and "/silent" command line parameters for mounting Linux volumes
    * Added greater emphasis on the fact that when overwriting a partition with encrypted data, whatever was previously stored on that partition will be overwritten. (Yes, some people did email about this!)
    * Prevent FreeOTFE from exiting when the wrong password is entered
    * Various cosmetic improvements
  </UL>

  * v4.30 (27th October 2008)
  <UL>
    * Added functionality to support language translations
    * Added "/password" and "/silent" command line parameters for mounting FreeOTFE volumes
    * Added additional option to overwrite an entire mounted volume, as well as its free space
    * Added support for optionally substituting the mounted drive letter as a parameter passed to any post-mount and pre/post-dismount autorun commands configured
    * Added support for post-mount and pre-dismount autorun commands with spaces in their paths
    * Added option to display passwords as they're entered
    * Driver development environment changed to Microsoft Visual Studio 2008 with WDK for Server 2008 v6001.18001 (although older versions can probably still be used)
    * Corrected intermittant fault causing FreeOTFE to crash on mounting some volumes
    * Corrected fault preventing hidden volumes being created on host volumes greater than 4GB
    * Corrected estimated time remaining shown on progress dialog
    * Hotkey options now saved correctly
    * Minor cosmetic improvements
  </UL>

  * v4.00 (26th June 2008)
  <UL>
    * Added support for 64 bit versions of MS Windows
    * Added support for PKCS#11 (Cryptoki) security tokens/smartcards
    * Significantly improved partition selection system
    * Added "/drive", "/keyfile" and "/lesfile" command line parameters when mounting volumes
    * Added user configurable global setting for default drive letter
    * Extensive additions, changes and improvements made to documentation
    * Various cosmetic and usability improvements
  </UL>
  * v3.00 (16th December 2007)

  <UL>
* FreeOTFE is now available both as a full installation package, as well as standard ZIP version.

    
    * Added LRW and XTS cypher mode support
    * Added post mount/pre dismount/post dismount autorun option to allow automated integrity checking, cleanup, etc

    
    * Added option to explore drive automatically after mounting
    * Made volume file
timestamp reversion optional (i.e. the timestamps on volume files are
automatically set back to their pre-mount values on dismounting;
previously it would always do this)

    
    * Simplified
password entry for mounting FreeOTFE volumes; advanced options can
easily be displayed by clicking button. (Advanced mount options can be
displayed by default by setting relevant option)

    
    * Password entry dialog is now only dismissed when the volume is
successfully mounted, or the user cancels the mount when mounting
a DoxBox

    
</UL>
  <UL>
    * Multiple driver files can now be selected for installation when manually installing drivers

    
    * "File
| Mount" now also auto detects LUKS volumes, so they can be mounted via
this menuitem as well as "File | Linux volume | Mount file...".
Similarly, dragging and dropping files onto FreeOTFE will cause it to
attempt to auto detect the type of volume used.

    
    * Manually installed drivers will now start up as soon as they've been installed, and set to startup automatically on boot
    * Removed short password warning when mounting a LUKS volume with less than 20 characters in its password
    * Command line option to mount volumes will now accept relative paths to volume files
    * Added command line options to install and uninstall drivers
    * Added command line option to return the number of mounted volumes
    * Added dialog to report all currently available hash/cyphers algorithms, together with their details (see under "Help" menu)

    
    * Improved support for multimonitor systems
    * Corrected fault with mouse RNG that would cause "insufficient random data" errors

    
    * Main development environment changed to:

    <UL>
      * CodeGear Delphi 2007
      * Microsoft Visual Studio 2005 Professional Edition
    </UL>
(although older versions can probably still be used)* Updated LibTomCrypt based cypher/hash drivers to use LibTomCrypt v1.17

    
  </UL>

  * v2.00.00 (18th March 2007)
  <UL>
    * Implemented support for Windows Vista (although
previous versions would work under Vista, they had a few minor cosmetic issues under this OS)

    
    * Added option to allow mounted volumes to be visible to all logged on users, or just the current user

    
* Added
option to associate ".vol" files with FreeOTFE, in line with the PDA
version, and defaulted open/save dialog filters. (Note that you are
still free to use **any** filename with any filename extension).

* Added ability to store FreeOTFE settings in different places, or not at all if required

    
    * Updated to automatically prompt user if they want to start portable mode if the main FreeOTFE driver isn't installed/running
* Added indicator to show portable mode activating* Added ability to combine random number generators (RNGs) when generating random data for new volumes/changing passwords* Added option to start FreeOTFE in portable mode without prompting if no installed drivers are found

    
    * Added option to suppress prompting for volume type when volume files are drag 'n' dropped onto FreeOTFE
    * Added
additional prompt when creating new volumes to make it clearer when the
minimum amount of information required had been entered
* Added option to allow user to specify where their settings should be stored (if they are to be saved)</UL>
<UL>
* Added
extra support for "odd" LUKS volumes which don't use the same cypher keylength for IV
generation as encrypting data (e.g. volumes which use Blowfish-448 for data encryption, and cbc-essiv:sha256
actually use Blowfish-256 for their ESSIV IVs - this is now supported)

    
* Added detection of (nonstandard) Tiger and Whirlpool hashes when using LUKS volumes* Removed redundant warning about drivers not being started in portable mode when they had been manually installed
    * Corrected minor cosmetic issue with status bar display* Added OS version ID to human readable CDB dumps* Moved weaker drivers (FreeOTFE4PDAHashNull.dll, FreeOTFE4PDACypherNull.dll
and FreeOTFE4PDACypherXOR.dll) into a "weak drivers" directory; most
users shouldn't use these drivers, but they're still included in the
release if needed

  </UL>

  * v1.60.00 (28th January 2006)
<UL>
* Added the following cyphers:
    <UL>
* CAST6 (Gladman library)

    </UL>
* Fixed minor bug that prevented FreeOTFE from properly dismounting volumes under certain conditions.* Corrected build error that prevented previous version from operating correctly under MS Windows 2000

    
  </UL>
  * v1.50.00 (8th January 2006)

  <UL>
    * Changed email address from sdean12@softhome.net to (finally!) sdean12@sdean12.org* Added the following cyphers:

    <UL>
      * Serpent* AES (Gladman library)
      * RC-6 (Gladman library)

      
      * Twofish (Gladman library)* MARS (Gladman library; source only pending licence clarification)

    </UL>
* Updated to allow volumes to be mounted across networks* Added ability to resize the driver control dialog
* Corrected fault that prevented LUKS partitions from being mounted* Added
support for LUKS volumes which use ESSIV, subject to the hashlength of
the ESSIV hash generating the same number or less bits as the cypher
used. Note: Although LUKS can create volumes with ESSIV - ESSIV is **not** part of the LUKS specification!

    
* Corrected errors that were displayed when a non-admin attempted to access the driver control dialog

    
  </UL>
  * v1.00.00 (20th November 2005)

  <UL>
    * Added dismount hotkeys.* Added optional system tray icon (tasktray icon), together with ability to minimize/close FreeOTFE to the system tray.
    * Added optional toolbar and statusbar* Added command line support* Improved support for MS Windows XP Themes
    * Switched from Delphi 5 to Delphi 7

    
  </UL>
  * v0.59.00 BETA (29th July 2005)

  <UL>
    * Minor change to FreeOTFE.sys device driver to fix error
introduced by compiler. Specifically, debug builds work correctly, but
for release builds the compiler handles the "%" (modulus) operator
differently for 64 bit arithmetic; calling a kernel function
("_alldvrm") that doesn't exist in Windows 2000, causing the driver to
fail to load.

    
  </UL>
  * v0.58.00 BETA (24th July 2005)

    <UL>
      * Added support for cryptlib (cl32.dll) as an RNG, if installed
      * Added support for Linux LUKS volumes
      * Removed redundant warnings when run by a user without Administrative privileges
    </UL>
  
* v0.57.9900 BETA (8th June 2005 - restricted test release)
    <UL>
      * Added support for encrypted CDs/DVDs
      * Added support for encrypting disk devices, as well as
partitions (i.e. You can now encrypt an entire disk, not just
partitions held on it)
    </UL>
  
  * v00.57.00 BETA (28th May 2005)

  <UL>
    * Compatibility with Linux volumes improved
    * Fixed bug found when creating a new volume and storing the
volume's CDB separately. (The CDB file wasn't being created before the
CDB was written to it, causing an "Unable to write critical data block"
error.)

    
  </UL>
  * v00.56.00 BETA (7th May 2005 - restricted release)
  <UL>
    * Corrected issues relating to formatting volumes

    
  </UL>
  * v00.54.00 BETA (7th May 2005 - restricted release)

  <UL>
    * Now tested under Windows 2000, as well as Windows XP.
    * Removed buffering between data transfers to increase reliability.
    * Updated to fix "Format incomplete" problems when formatting as FAT/FAT32 while running under Windows 2000.
    * Added warning to user if an attempt is made to use a volume file created with a later volume format layout ID.
    * Corrected fault with Windows XP/Windows 2000 differing
QueryDosDevice behaviour that sometimes prevented FreeOTFE listing all
cyphers/hash algorithms installed.
    * Updated to only accept drag 'n dropped files when the drivers are active.
    * Extra checks included to ensure user has at least one hash and cypher driver installed and running.
    * Fixed MB/GB selection in new volume creation.
    * More sensible saving/loading of Linux encryption settings.
    * Added prompt when exiting if volumes are still mounted.

    
  </UL>
  * v00.53.00 BETA (24th April 2005 - restricted release)
  <UL>
    * Fixed major bug causing the main driver to ignore its queue when large amounts of data were written to an encrypted volume.
    * Improved emergency dismount (dismounts faster by cancelling all queued requests to the driver).
    * Improved progress dialog's responsiveness when overwriting free
space, preventing it from appearing frozen and giving user a better
indication of progress.
    * Fixed minor cosmetic fault on the keyfile creation dialog.
    * DoxBox creation wizard now gives user more flexibility to specify the sector IV generation method used.
    * Added support for Linux dm-crypt volumes (e.g. ESSIV).
    * Added progress dialog when creating volume files.
    * Message shown when FreeOTFE exits and stops portable mode no
longer displayed, unless there was a problem stopping portable mode.

    
  </UL>
  * v00.50.00 BETA 1 (4th April 2005)

  <UL>
    * Implementation of "portable mode"* Added support for keyfiles* User passwords are no longer displayed; they are blanked out with "*" characters
    * Added option to backup and restore the CDB from volumes with a CDB
    * Added option to dump critical data block to a human readable file
    * Driver control dialog now indicates state of drivers - making it a **lot** clearer what's happening!
    * Added support for encrypted partitions
    * User can now change their passwords, and many other DoxBox properties.

    
    * When volume files
are mounted and used, their timestamps (created, last accessed and last
modified) and attributes are recorded. They are then reset on dismount.

    
    * Volume file timestamps and attributes are reset after dismounting, improving "plausible deniability"
    * Added ability to save settings when mounting Linux volumes
    * Volumes can now be mounted as removable drives (no recycle bin, files deleted directly)
    * User's password is now processed with PKCS#5 PBKDF2 (HMAC), instead of being salted and hashed.
    * Improved handling if more than one hash/cypher can be used for encryption/decryption.

    
    * Added the following cyphers:
    <UL>

      * DES

    
      * 3DES

    
      * Blowfish

    
      * RC6

    
      * CAST5

    
      * libtomcrypt version of Twofish

    
    </UL>
    * Added the following hash algorithms:
    <UL>

      * Tiger

      * Whirlpool

    </UL>
    * Added per-volume IVs
    * Rewrote most of the driver control dialog
    * Critical data block checksum now uses the HMAC of the volume details block, instead of its hash
    * Fixed
bug that could cause new volumes to be created with an extra newline
tacked onto the end of the user's password. If you have problems
mounting volumes created like this, you should still be able to mount
them by entering an extra newline (just press &lt;ENTER&gt;) at the end
of your password.
    * Fixed bug that caused VolumeFlags to be read back from the volume
incorrectly. In order to continue support for older (CDB format 1)
volumes, these volumes will still be read incorrectly, but newer (CDB
format 2) volumes are processed correctly. To update older volumes to
use the later CDB format, simply change your password via the "Tools |
Change volume/keyfile password/details..." menuitem. It is highly
recommended that you do this anyway, in order to better ensure future
compatibility

    
    <UL>

    </UL>
</UL>

  * v0.00.02 BETA 1 (11th October 2004)

  <UL>
    * Fix to correct bug with salt handling which prevented FreeOTFE
volumes from being mounted.

    
  </UL>
  * v0.00.01 BETA 1 (10th October 2004)
  <UL>
    * First public release for
compatibility testing
  </UL>
</UL>

* * * 
<A NAME="level_3_heading_2">
### PDA Version
</A>

<UL>
  * v5.21 (7th February 2010)
  <UL>
    * Added Russian language translation
    * Added translator credit to "About..." dialog
    * Fix bug in tap 'n' scribble RNG causing error message to be displayed
  </UL>

  * v5.20 (3rd January 2010)
  <UL>
    * Display layout changes made to better support translated interface
    * Added Greek language translation
    * Added Croatian language translation
    * Added language translator credits dialog
    * Added hash/cypher details dialogs
    * Changed password reading routines to better support Windows Mobie 6.5 (Microsoft changed the manner in which WM65 accepted passwords entered by the user)
  </UL>

  * v5.10 (13th October 2009)
  <UL>
    * Added Czech translation
    * Added Japanese translation
    * Updated German and Spanish translations
    * Fix bug in release procedure causing only Spanish translation to be included (Caused by longstanding fault in cabwiz.exe. Thanks a lot Microsoft :( )
  </UL>

  * v5.00 (22nd July 2009)
  <UL>
    * Optimised mount performance
    * Added functionality to support language translations
    * Added support for Linux LUKS volumes
    * Added XTS mode to Gladman cyphers (AES, MARS, RC-6, Serpent and Twofish)
    * Added RIPEMD-256 and RIPEMD-320 hash algorithms
    * Added option to volume creation wizard to mount new volumes after creating them
    * Added "/create", "/noexit" and "/silent" command line options
    * Add confirmation message when exiting "Options" dialog with "Save settings" option turned off
    * Removed redundant message shown on dismounting volumes
    * Corrected fault requiring hidden volumes to begin at an offset which is a multiple of 512 bytes
  </UL>

  * v3.76 (19th April 2009)
  <UL>
    * Corrected minor fault in locking protecting FreeOTFE4PDA's internal list of device handles
  </UL>

  * v3.75 (4th April 2009)
  <UL>
    * Added option to enable/disable hash/cypher drivers from the user interface
    * By default, the SHA and AES drivers are enabled, but all others are disabled to improve performance. (The user can easily enable others if required)
    * Driver code refactored to allow common code to be shared between FreeOTFE and FreeOTFE Explorer (no functional changes)
    * Drivers moved into "DLL" subdirectory
  </UL>

  * v3.05 (27th October 2008)
<UL>
  * Resolved issue that causing "Mount failed; the virtual storage device could not be activated at this time" error on some PDAs when mounting
  * Updated documentation
  * Development environment changed to Microsoft Visual Studio 2008 (although older versions can probably still be used)
</UL>
  * v3.00 (16th December 2007)
  <UL>
* FreeOTFE4PDA
is now available as a single .CAB file and ActiveSync installer, as well
as a standard .ZIP file, making installation to a PDA even simpler
* Added LRW and XTS cypher mode support
* Tested under Windows Mobile 6 (Note: earlier versions of FreeOTFE4PDA should also work with this OS)
</UL>
  <UL>
* Added option to revert volume file timestamps/attributes to their pre-mount values on dismounting

    
    * Optimised order of cypher/hash details returned from drivers to improve time to mount on PDA version

    
    * Updated LibTomCrypt based cypher/hash drivers to use LibTomCrypt v1.17
  
  </UL>

  * v2.11.00 (29th June 2007)

  <UL>
    * Added "\" prefix to mountpoint passed to configured explorer
application when exploring mounted volumes in order to better support
3rd party software (e.g. Resco Explorer)
    * Added automatic refresh of FreeOTFE's display if already
running when a volume is mounted via the commandline/tapping on a
".vol" file.
    * Updated to correct minor bug preventing commandline options
from mounting correctly if no mountpoint is supplied on the commandline.

    
  </UL>
  * v2.10.00 (27th May 2007)

  
  <UL>
    * Added (optional) support for Windows Mobile 5.0 softkeys
    * Documentation layout revamped
    * Added link to user guide from "Help" menu
    * Made it clearer to the user at which point they can create their new volume, and which advanced/optional steps

    
  </UL>
  * v2.00.00 (18th March 2007)
<UL>
* Added
option to change the default mountpoint, or default it based on the
volume's filename. Mount dialog "mountpoint" control moved to
"Advanced" tab on mount dialog

* Added option to specify a different "File explorer" application* Added option to associate FreeOTFE4PDA with volume files

    * Added context menu option to explore mounted volumes, in addition to existing doubletap explore

    * Added option to backup and restore the CDB from volumes with a CDB* Added option to dump critical data block to a human readable file* Added command line support* Added OS version ID to human readable CDB dumps* Moved weaker drivers (FreeOTFE4PDAHashNull.dll, FreeOTFE4PDACypherNull.dll
and FreeOTFE4PDACypherXOR.dll) into a "weak drivers" directory; most
users shouldn't use these drivers, but they're still included in the
release if needed

    </UL>
* v0.55.00 BETA (4th December 2006)<UL>
* Corrected error preventing volumes encrypted with the XOR and NULL encryption drivers operating properly* Corrected problem with opening files on some PDAs

    * Redundant drivers moved into separate directory

    * Added further information to documentation with respect to redundant drivers/speeding up the time taken to mount* Added stats report for drivers (developer use only)* FreeOTFE4PDACypherTwofish_HifnCS.dll
driver removed from PDA binary distribution (driver fails to init
correctly). Note: Existing volumes can still be mounted and used with
the remaining two Twofish implementations </UL>
* v0.50.00 BETA (11th November 2006)<UL>
* First public release</UL>
</UL>



