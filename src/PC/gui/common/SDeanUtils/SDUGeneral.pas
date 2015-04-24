unit SDUGeneral;
 // Description: Sarah Dean's General Utils
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  Controls, ExtCtrls,
  Forms, StdCtrls,
  Windows, // Required for TWIN32FindData in ConvertSFNPartToLFN, and THandle
  Classes,
  ComCtrls,  // Required in SDUEnableControl to enable/disable TRichedit controls
  Dialogs,   // Required for SDUOpenSaveDialogSetup
  Graphics,  // Required for TFont
  ShlObj,    // Required for SHChangeNotify and SHGetSpecialFolderLocation
  ActiveX,   // Required for IMalloc
  Buttons,   // Required for TBitBtn
  ActnList,  // Required for TAction
  Menus,
  SDUWinHTTP,
  SysUtils,
  zlib;

const
  // A reasonable upper limit to the number of partitions that we'll support on
  // a disk
  // Note: During texting, a Windows XP x64 with one disk, which only had a
  //       about 8 partitions on it clocked up 24 partition table entries
  //       getting returned from drive layout DIOC.
  //       This *should* be dynamically calculated, but for now we just put in
  //       a reasonably high value.
  //       Since this is only used to define the size of a struct used, we can
  //       make it reasonably large without consequence
  SDU_MAX_PARTITIONS = 75;

  FMT_DEVICENAME_HDD_PHYSICAL_DISK = '\\.\PHYSICALDRIVE%d';
  FMT_DEVICENAME_PARTITION_DEVICE  = '\Device\Harddisk%d\Partition%d';
  FMT_DEVICENAME_HDD_DEVICE        = '\Device\Harddisk%d';
  FMT_DEVICENAME_CDROM_DEVICE      = '\Device\CdRom%d';
  FMT_DEVICENAME_DRIVE_DEVICE      = '\\.\%s:';

  FILE_TYPE_DIRECTORY = 'DIRECTORY_TYPE_HERE';

type
  TSDUArrayInteger = array of Integer;
  TSDUArrayString  = array of String;

{$IF CompilerVersion >= 18.5}// Delphi 2007 defined
  // If you have Delphi 2007, use the definition in Windows.pas (i.e. uses Windows)
  // Delphi 7 doesn't have ULONGLONG
  ULONGLONG = Uint64;        //TDK CHANGE
  // ULONGLONG = int64;  // Changed from Uint64 to prevent Delphi internal error
  // c1118 in SDUFormatUnits with Uint64 under Delphi7
  // (from feedback from OracleX <oraclex@mail.ru>)
  // Note: Because it's using int64 here, overloaded
  //       functions which provide ULONGLONG and int64
  //       versions have their ULONGLONG version ifdef'd
  //       out.
{$IFEND}

  // Note: DON'T USE A PACKED RECORD HERE!
  TSDUPartitionInfo = record
    StartingOffset:      ULONGLONG;  // LARGE_INTEGER;
    PartitionLength:     ULONGLONG;  // In *bytes*. LARGE_INTEGER;
    HiddenSectors:       DWORD;
    PartitionNumber:     DWORD;
    PartitionType:       Byte;
    BootIndicator:       Boolean;
    RecognizedPartition: Boolean;
    RewritePartition:    Boolean;
    //    junk: array[0..255] of byte;  // Not needed - just padding; should be removed
  end;
  PSDUPartitionInfo = ^TSDUPartitionInfo;

  // Note: DON'T USE A PACKED RECORD HERE!
  TSDUDriveLayoutInformation = record
    PartitionCount: DWORD;
    Signature:      DWORD;
    PartitionEntry: array [0..(SDU_MAX_PARTITIONS - 1)] of TSDUPartitionInfo;
  end;
  PSDUDriveLayoutInformation = ^TSDUDriveLayoutInformation;

  TSDUMediaType = (
    Unknown,
    F5_1Pt2_512,
    F3_1Pt44_512,
    F3_2Pt88_512,
    F3_20Pt8_512,
    F3_720_512,
    F5_360_512,
    F5_320_512,
    F5_320_1024,
    F5_180_512,
    F5_160_512,
    RemovableMedia,
    FixedMedia,
    F3_120M_512,
    F3_640_512,
    F5_640_512,
    F5_720_512,
    F3_1Pt2_512,
    F3_1Pt23_1024,
    F5_1Pt23_1024,
    F3_128Mb_512,
    F3_230Mb_512,
    F8_256_128,
    F3_200Mb_512,
    F3_240M_512,
    F3_32M_512
    );

const
  TSDUMediaTypeTitle: array [TSDUMediaType] of String = (
    'Unknown',
    '5.25" floppy, with 1.2MB and 512 bytes/sector.',
    '3.5" floppy, with 1.44MB and 512 bytes/sector.',
    '3.5" floppy, with 2.88MB and 512 bytes/sector.',
    '3.5" floppy, with 20.8MB and 512 bytes/sector.',
    '3.5" floppy, with 720KB and 512 bytes/sector.',
    '5.25" floppy, with 360KB and 512 bytes/sector.',
    '5.25" floppy, with 320KB and 512 bytes/sector.',
    '5.25" floppy, with 320KB and 1024 bytes/sector.',
    '5.25" floppy, with 180KB and 512 bytes/sector.',
    '5.25" floppy, with 160KB and 512 bytes/sector.',
    'Removable media other than floppy.',
    'Fixed hard disk media.',
    '3.5" floppy, with 120MB and 512 bytes/sector.',
    '3.5" floppy, with 640MB and 512 bytes/sector.',
    '5.25" floppy, with 640KB and 512 bytes/sector.',
    '5.25" floppy, with 720KB and 512 bytes/sector.',
    '3.5" floppy, with 1.2MB and 512 bytes/sector.',
    '3.5" floppy, with 1.23MB and 1024 bytes/sector.',
    '5.25" floppy, with 1.23KB and 1024 bytes/sector.',
    '3.5" floppy, with 128MB and 512 bytes/sector.',
    '3.5" floppy, with 230MB and 512 bytes/sector.',
    '8" floppy, with 256KB and 128 bytes/sector.',
    '3.5" floppy, with 200MB and 512 bytes/sector. (HiFD).',
    '3.5" floppy, with 240MB and 512 bytes/sector. (HiFD).',
    '3.5" floppy, with 32MB and 512 bytes/sector.'
    );


resourcestring
  RS_UNKNOWN = '<unknown>';
  RS_BETA    = 'BETA';

type
  TSDUDiskGeometry = packed record
    Cylinders: LARGE_INTEGER;
    MediaType: DWORD;  // A TSDUMediaType, but must be a DWORD to pad out
    // correctly
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
    junk: array[0..255] of Byte;  // Not needed - just padding; should be removed
  end;
  PSDUDiskGeometry = ^TSDUDiskGeometry;

  TInstalledOS   = (
    osWindows95,
    osWindows98,
    osWindowsMe,
    osWindowsNT,
    osWindows2000,
    osWindowsXP,
    osWindowsServer2003,
    osWindowsServer2003R2,
    osWindowsVista,
    osWindowsServer2008,
    osWindowsServer2008R2,
    osWindows7
    );
  TCenterControl = (ccNone, ccVertical, ccHorizontal, ccBoth);
  TControlArray  = array of TControl;

  // Exceptions...
  EToolException         = Exception;
  EExceptionBadSrc       = EToolException;
  EExceptionBadDest      = EToolException;
  EExceptionBadSrcOffset = EToolException;
  EExceptionWriteError   = EToolException;
  EExceptionUserCancel   = EToolException;

  TCopyProgressCallback = procedure(progress: Int64; var cancel: Boolean) of object;

type
  TUnits_Storage = (usBytes, usKB, usMB, usGB, usTB);

  // Note: FWIW:
  //   Windows normally uses CRLF
  //   Linux   normally uses LF
  //   MacOSX  normally uses CR
  TSDUNewline_Enum = (nlCRLF, nlCR, nlLF);
  TSDUNewline      = TSDUNewline_Enum;

  TSDUBytes         = array of Byte;
  DriveLetterString = String;
  VolumeFilenameString = String;
  KeyFilenameString = String;
  FilenameString    = String;
  DriveLetterChar   = Char;
  //  PasswordString       = TSDUBytes;
  PasswordString    = Ansistring;

resourcestring
  UNITS_STORAGE_BYTES = 'bytes';
  UNITS_STORAGE_KB    = 'KB';
  UNITS_STORAGE_MB    = 'MB';
  UNITS_STORAGE_GB    = 'GB';
  UNITS_STORAGE_TB    = 'TB';

const
  SDUCR   = #$0D; // #13
  SDULF   = #$0A; // #10
  SDUCRLF = SDUCR + SDULF;

  SDUMaxByte  = 255;
  SDUMaxWORD  = 65535;
  SDUMaxDWORD = 4294967295;

  UNITS_BYTES_MULTIPLIER = 1024;

  BYTES_IN_KILOBYTE = 1024;
  BYTES_IN_MEGABYTE = 1024 * BYTES_IN_KILOBYTE;
  BYTES_IN_GIGABYTE = 1024 * BYTES_IN_MEGABYTE;

  // Note: EVN_VAR_PROC_ARCHITECTURE is set to x86 on x64 systems, if running
  //       a 32 bit process
  EVN_VAR_PROC_ARCHITECTURE = 'PROCESSOR_ARCHITECTURE';
  EVN_VAR_PROC_ARCH_W3264   = 'PROCESSOR_ARCHITEW3264';

  INSTALLED_OS_TITLE: array [TInstalledOS] of String = (
    'Windows 95',
    'Windows 98',
    'Windows Me',
    'Windows NT',
    'Windows 2000',
    'Windows XP',
    'Windows Server 2003',
    'Windows Server 2003 R2',
    'Windows Vista',
    'Windows Server 2008',
    'Windows Server 2008 R2',
    'Windows 7'
    );

  SDUNEWLINE_STRING: array [TSDUNewline] of String = (SDUCRLF, SDUCR, SDULF);
  SDUNEWLINE_TITLE: array [TSDUNewline] of String = ('CRLF', 'CR', 'LF');
  SDUNEWLINE_TITLEWITHOS: array [TSDUNewline] of String =
    ('CRLF (Windows)', 'CR (Mac)', 'LF (Linux)');
  SDUNEWLINE_OSWITHTITLE: array [TSDUNewline] of String =
    ('Windows (CRLF)', 'Mac (CR)', 'Linux (LF)');


  // Taken from:
  //
  //   ShlObj.h
  //
  // These consts may be used with:
  //   SDUGetSpecialFolderPath(...)
  // to get various system paths.
  // See:
  //   http://msdn.microsoft.com/en-us/library/bb762494(VS.85).aspx
  // for a description of each
  //
  // Versions: From: http://msdn.microsoft.com/en-us/library/bb776779(VS.85).aspx
  //
  // 4.0   All           Windows 95 and Windows NT 4.0
  // 4.7   All           Windows Internet Explorer 3.x
  // 4.71  All           Internet Explorer 4.0.
  // 4.72  All           Internet Explorer 4.01 and Windows 98.
  // 5.0   Shlwapi.dll   Internet Explorer 5 and Windows 98 SE.
  // 5.5   Shlwapi.dll   Internet Explorer 5.5 and Windows Millennium Edition (Windows Me)
  // 5.8   Comctl32.dll  Internet Explorer 5.
  // 5.81  Comctl32.dll  Windows 2000 and Windows Me.
  // 5.82  Comctl32.dll  Windows XP and Windows Vista.
  // 6.0   Shlwapi.dll   Windows XP and Windows Vista
  // 5.0   Shell32.dll   Windows 2000 and Windows Millennium Edition (Windows Me).
  // 6.0   Shell32.dll   Windows XP and Windows Vista
  SDU_CSIDL_DESKTOP         = $0000;           // <desktop>
  SDU_CSIDL_INTERNET        = $0001;           // Internet Explorer (icon on desktop)
  SDU_CSIDL_PROGRAMS        = $0002;           // Start Menu\Programs
  SDU_CSIDL_CONTROLS        = $0003;           // My Computer\Control Panel
  SDU_CSIDL_PRINTERS        = $0004;           // My Computer\Printers
  SDU_CSIDL_PERSONAL        = $0005;  // v6.0  // My Documents
  SDU_CSIDL_FAVORITES       = $0006;           // <user name>\Favorites
  SDU_CSIDL_STARTUP         = $0007;           // Start Menu\Programs\Startup
  SDU_CSIDL_RECENT          = $0008;           // <user name>\Recent
  SDU_CSIDL_SENDTO          = $0009;           // <user name>\SendTo
  SDU_CSIDL_BITBUCKET       = $000a;           // <desktop>\Recycle Bin
  SDU_CSIDL_STARTMENU       = $000b;           // <user name>\Start Menu
  SDU_CSIDL_MYDOCUMENTS     = $000c;  // v6.0  // logical "My Documents" desktop icon
  SDU_CSIDL_MYMUSIC         = $000d;           // "My Music" folder
  SDU_CSIDL_MYVIDEO         = $000e;  // v6.0  // "My Videos" folder
  SDU_CSIDL_DESKTOPDIRECTORY = $0010;           // <user name>\Desktop
  SDU_CSIDL_DRIVES          = $0011;           // My Computer
  SDU_CSIDL_NETWORK         = $0012;
  // Network Neighborhood (My Network Places)
  SDU_CSIDL_NETHOOD         = $0013;           // <user name>\nethood
  SDU_CSIDL_FONTS           = $0014;           // windows\fonts
  SDU_CSIDL_TEMPLATES       = $0015;
  SDU_CSIDL_COMMON_STARTMENU = $0016;                  // All Users\Start Menu
  SDU_CSIDL_COMMON_PROGRAMS = $0017;                   // All Users\Start Menu\Programs
  SDU_CSIDL_COMMON_STARTUP  = $0018;                   // All Users\Startup
  SDU_CSIDL_COMMON_DESKTOPDIRECTORY = $0019;           // All Users\Desktop
  SDU_CSIDL_APPDATA         = $001a;                   // v4.71 // <user name>\Application Data
  SDU_CSIDL_PRINTHOOD       = $001b;                   // <user name>\PrintHood
  SDU_CSIDL_LOCAL_APPDATA   = $001c;
  // v5.0  // <user name>\Local Settings\Applicaiton Data (non roaming)
  SDU_CSIDL_ALTSTARTUP      = $001d;           // non localized startup
  SDU_CSIDL_COMMON_ALTSTARTUP = $001e;           // non localized common startup
  SDU_CSIDL_COMMON_FAVORITES = $001f;
  SDU_CSIDL_INTERNET_CACHE  = $0020;  // v4.72
  SDU_CSIDL_COOKIES         = $0021;
  SDU_CSIDL_HISTORY         = $0022;
  SDU_CSIDL_COMMON_APPDATA  = $0023;                   // v5.0  // All Users\Application Data
  SDU_CSIDL_WINDOWS         = $0024;                   // v5.0  // GetWindowsDirectory()
  SDU_CSIDL_SYSTEM          = $0025;                   // v5.0  // GetSystemDirectory()
  SDU_CSIDL_PROGRAM_FILES   = $0026;                   // v5.0  // C:\Program Files
  SDU_CSIDL_MYPICTURES      = $0027;                   // v5.0  // C:\Program Files\My Pictures
  SDU_CSIDL_PROFILE         = $0028;                   // v5.0  // USERPROFILE
  SDU_CSIDL_SYSTEMX86       = $0029;                   // x86 system directory on RISC
  SDU_CSIDL_PROGRAM_FILESX86 = $002a;                  // x86 C:\Program Files on RISC
  SDU_CSIDL_PROGRAM_FILES_COMMON = $002b;              // v5.0  // C:\Program Files\Common
  SDU_CSIDL_PROGRAM_FILES_COMMONX86 = $002c;           // x86 Program Files\Common on RISC
  SDU_CSIDL_COMMON_TEMPLATES = $002d;                  // All Users\Templates
  SDU_CSIDL_COMMON_DOCUMENTS = $002e;                  // All Users\Documents
  SDU_CSIDL_COMMON_ADMINTOOLS = $002f;
  // v5.0  // All Users\Start Menu\Programs\Administrative Tools
  SDU_CSIDL_ADMINTOOLS      = $0030;
  // v5.0  // <user name>\Start Menu\Programs\Administrative Tools
  SDU_CSIDL_CONNECTIONS     = $0031;           // Network and Dial-up Connections
  SDU_CSIDL_COMMON_MUSIC    = $0035;  // v6.0  // All Users\My Music
  SDU_CSIDL_COMMON_PICTURES = $0036;  // v6.0  // All Users\My Pictures
  SDU_CSIDL_COMMON_VIDEO    = $0037;  // v6.0  // All Users\My Video
  SDU_CSIDL_RESOURCES       = $0038;  // Windows Vista // Resource Direcotry
  SDU_CSIDL_RESOURCES_LOCALIZED = $0039;           // Localized Resource Direcotry
  SDU_CSIDL_COMMON_OEM_LINKS = $003a;           // Links to All Users OEM specific apps
  SDU_CSIDL_CDBURN_AREA     = $003b;
  // v6.0  // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
  // unused                             $003c
  SDU_CSIDL_COMPUTERSNEARME = $003d;
  // Computers Near Me (computered from Workgroup membership)
  SDU_CSIDL_FLAG_CREATE     = $8000;
  // combine with CSIDL_ value to force folder creation in SHGetFolderPath()
  SDU_CSIDL_FLAG_DONT_VERIFY = $4000;
  // combine with CSIDL_ value to return an unverified folder path
  SDU_CSIDL_FLAG_NO_ALIAS   = $1000;
  // combine with CSIDL_ value to insure non-alias versions of the pidl
  SDU_CSIDL_FLAG_PER_USER_INIT = $0800;
  // combine with CSIDL_ value to indicate per-user init (eg. upgrade)
  SDU_CSIDL_FLAG_MASK       = $FF00;           // mask for all possible flag values


  // Constants used for SDUSelectDirectory(...)
  // Only return file system directories. If the user selects folders that are not part of the file system, the OK button is grayed.
  // Note  The OK button remains enabled for "\\server" items, as well as "\\server\share" and directory items. However, if the user selects a "\\server" item, passing the PIDL returned by SHBrowseForFolder to SHGetPathFromIDList fails.
  BIF_RETURNONLYFSDIRS = $00000001;
  // Do not include network folders below the domain level in the dialog box's tree view control.
  BIF_DONTGOBELOWDOMAIN = $00000002;
  // Include a status area in the dialog box. The callback function can set the status text by sending messages to the dialog box. This flag is not supported when BIF_NEWDIALOGSTYLE is specified.
  BIF_STATUSTEXT       = $00000004;
  // Only return file system ancestors. An ancestor is a subfolder that is beneath the root folder in the namespace hierarchy. If the user selects an ancestor of the root folder that is not part of the file system, the OK button is grayed.
  BIF_RETURNFSANCESTORS = $00000008;
  // Version 4.71. Include an edit control in the browse dialog box that allows the user to type the name of an item.
  BIF_EDITBOX          = $00000010;
  // Version 4.71. If the user types an invalid name into the edit box, the browse dialog box calls the application's BrowseCallbackProc with the BFFM_VALIDATEFAILED message. This flag is ignored if BIF_EDITBOX is not specified.
  BIF_VALIDATE         = $00000020;
  // Version 5.0. Use the new user interface. Setting this flag provides the user with a larger dialog box that can be resized. The dialog box has several new capabilities, including: drag-and-drop capability within the dialog box, reordering, shortcut menus, new folders, delete, and other shortcut menu commands.
  // Note  If Component Object Model (COM) is initialized through CoInitializeEx with the COINIT_MULTITHREADED flag set, SHBrowseForFolder fails if BIF_NEWDIALOGSTYLE is passed.
  BIF_NEWDIALOGSTYLE   = $00000040;
  // Version 5.0. The browse dialog box can display URLs. The BIF_USENEWUI and BIF_BROWSEINCLUDEFILES flags must also be set. If any of these three flags are not set, the browser dialog box rejects URLs. Even when these flags are set, the browse dialog box displays URLs only if the folder that contains the selected item supports URLs. When the folder's IShellFolder::GetAttributesOf method is called to request the selected item's attributes, the folder must set the SFGAO_FOLDER attribute flag. Otherwise, the browse dialog box will not display the URL.
  BIF_BROWSEINCLUDEURLS = $00000080;
  // 5.0. Use the new user interface, including an edit box. This flag is equivalent to BIF_EDITBOX | BIF_NEWDIALOGSTYLE.
  // Note  If COM is initialized through CoInitializeEx with the COINIT_MULTITHREADED flag set, SHBrowseForFolder fails if BIF_USENEWUI is passed.
  BIF_USENEWUI         = (BIF_EDITBOX or BIF_NEWDIALOGSTYLE);
  // Version 6.0. When combined with BIF_NEWDIALOGSTYLE, adds a usage hint to the dialog box, in place of the edit box. BIF_EDITBOX overrides this flag.
  BIF_UAHINT           = $00000100;
  // Version 6.0. Do not include the New Folder button in the browse dialog box.
  BIF_NONEWFOLDERBUTTON = $00000200;
  // Version 6.0. When the selected item is a shortcut, return the PIDL of the shortcut itself rather than its target.
  BIF_NOTRANSLATETARGETS = $00000400;
  // Only return computers. If the user selects anything other than a computer, the OK button is grayed.
  BIF_BROWSEFORCOMPUTER = $00001000;
  // Only allow the selection of printers. If the user selects anything other than a printer, the OK button is grayed.
  //    In Microsoft Windows XP and later systems, the best practice is to use a Windows XP-style dialog, setting the root of the dialog to the Printers and Faxes folder (CSIDL_PRINTERS).
  BIF_BROWSEFORPRINTER = $00002000;
  // Version 4.71. The browse dialog box displays files as well as folders.
  BIF_BROWSEINCLUDEFILES = $00004000;
  // Version 5.0. The browse dialog box can display shareable resources on remote systems. This is intended for applications that want to expose remote shares on a local system. The BIF_NEWDIALOGSTYLE flag must also be set.
  BIF_SHAREABLE        = $00008000;
  // Windows 7 and later. Allow folder junctions such as a library or a compressed file with a .zip file name extension to be browsed.
  BIF_BROWSEFILEJUNCTIONS = $00010000;

// Returns text representation of zlib compression level
function SDUZLibCompressionLevelTitle(compressionLevel: TCompressionLevel): String;
// Returns the string passed in, but with the initial letter capitalized
function SDUInitialCapital(Value: String): String;
// Get string representation of form's layout
function SDUGetFormLayout(form: TForm): String;
 // Set form's layout based on string representation of it
 // !! IMPORTANT !!
 // If there's a window layout stored, set ".Position" to poDefault before
 // calling this - otherwise it messes up the window if it was stored as
 // maximised.
 // Specifically, it shows the main window with maximised dimensions, with
 // the "Maximise" button in the top-right ready to "Normalise"
 // (non-maximise) the window, but WITH THE WINDOW SHOWN ABOUT 50 PIXELS
 // DOWN!)
procedure SDUSetFormLayout(form: TForm; layout: String);
// Convert Window message ID to string representation
function SDUWMToString(msgID: Cardinal): String;
// Convert ShowWindow(...) show state to string
function SDUShowStateToString(nCmdShow: Word): String;
// Convert from big-endian to little-endian, and vice versa
function SDUConvertEndian(const x: Word): Word; overload;
function SDUConvertEndian(const x: DWORD): DWORD; overload;
// Improved SelectDirectory(...), with "New Folder" button
function SDUSelectDirectory(hOwn: HWND; Caption: String; Root: String;
  var Path: String; uFlag: DWORD = $25): Boolean;
// As Delphi's StringOfChar(...), but operates on WideStrings
function SDUWideStringOfWideChar(Ch: Widechar; Count: Integer): WideString;
// Storage units enum to text
function SDUUnitsStorageToText(units: TUnits_Storage): String;
// Return an array containing the (translated) units
function SDUUnitsStorageToTextArr(): TSDUArrayString;
// Previously this was a const, now just returns the same as SDUUnitsStorageToTextArr(...)
function UNITS_BYTES_DENOMINATINON(): TSDUArrayString; deprecated;
// Return TRUE/FALSE, depending on whether the value passed in is odd or even
function SDUIsOddNumber(Value: Integer): Boolean;
function SDUIsEvenNumber(Value: Integer): Boolean;
 // Substitute %1, %2, %3, etc parameters in a string for values
 // format - A string containing %1, %2, %3...
 // params - The arguments to replace %1, %2, %3... with.
 // Note: "%n" can be repeated as many times as needed, and don't need to be in
 //       the same order that they appear in Args (e.g. "%2 %1 %3 %1" is valid)
 // Note: This function is "safe" in that if more arguments are passed in, it
 //       won't crash; similarly, if too few arguments are passed in, it'll just
 //       leave the "%n" in the string as "%n"
function SDUParamSubstitute(const formatStr: String; const params: array of Variant): String;
 // Get the filename of the DLL/EXE which stores the icon associated with the
 // type of file "filename", and the icon index within that DLL/EXE
 // Set "filename" to FILE_TYPE_DIRECTORY to get the icon details for a
 // directory
 // Returns TRUE/FALSE on success/failure
function SDUGetFileType_Icon(filename: String; out iconFilename: String;
  out iconIdx: Integer): Boolean;
 // Get a description of the type of file passed in
 // Set "filename" to FILE_TYPE_DIRECTORY to get the type of a directory
 // Returns '' if none found
function SDUGetFileType_Description(const filename: String): String; overload;
function SDUGetFileType_Description(const filename: String; out knownFiletype: Boolean): String;
  overload;
// CopyFile(...), but using Windows API to display "flying files" dialog while copying
function SDUFileCopy(srcFilename: String; destFilename: String): Boolean;
 // Populate the specified TComboBox with a list of removable drives
 // Note: This will clear any existing items in the TComboBox
procedure SDUPopulateRemovableDrives(cbDrive: TComboBox);
// Return text representation of partition type
function SDUPartitionType(PartitionTypeID: Byte; LongDesc: Boolean): String;
// Return last error as string
function SDUGetLastError(): String;
 // Read in the contents of the specified file
 // Note: THIS FUNCTION WILL FAIL IF FILESIZE IS > 2^(32-1); about 2GB
function SDUGetFileContent(filename: String; out content: Ansistring): Boolean;
 // Write the contents of the specified file
 // Note: This will overwrite any existing file
function SDUSetFileContent(filename: String; content: String): Boolean;
// Clear panel's caption, set bevels to bvNone
procedure SDUClearPanel(panel: TPanel);
 // Test to see if the specified bit (testBit) is set in value
 // Returns TRUE if it's set, otherwise FALSE
function SDUBitWiseTest(Value: Cardinal; testBit: Cardinal): Boolean;
 // Given a relative path, convert it to an absolute path
 // Note: May be given an absolute path
 // Note: Absolute path returned assumed relative to CWD
function SDURelativePathToAbsolute(relativePath: String): String; overload;
 // Given a relative path, convert it to an absolute path
 // Note: May be given an absolute path
 // Note: Absolute path returned assumes relativePath is relative to
 //       "relativeTo"
function SDURelativePathToAbsolute(relativePath: String; relativeTo: String): String; overload;
 // Copy file/device
 // Note: This is *not* the standard Windows copy functionality; this can copy
 //       from/to devices, etc
function SDUCopyFile(Source: String; destination: String; startOffset: Int64 = 0;
  length: Int64 = -1; blocksize: Int64 = 4096; callback: TCopyProgressCallback = nil): Boolean;
 // ----------------------------------------------------------------------------
 // compressNotDecompress - Set to TRUE to compress, FALSE to decompress
 // compressionLevel - Only used if compressNotDecompress is TRUE
 // length - Set to -1 to copy until failure
function SDUCopyFile_Compression(Source: String; destination: String;
  compressNotDecompress: Boolean; compressionLevel: TCompressionLevel = clNone;
  startOffset: Int64 = 0; length: Int64 = -1; blocksize: Int64 = 4096;
  callback: TCopyProgressCallback = nil): Boolean;
 // XOR the characters in two strings together
 // function SDUXOR(a: TSDUBytes; b: TSDUBytes): TSDUBytes;

//function SDUXOR(a: Ansistring; b: Ansistring): Ansistring;
function SDUXOR(a: TSDUBytes; b: array of Byte): TSDUBytes;
function SDUXORStr(a: Ansistring; b: Ansistring): Ansistring;
 { DONE 1 -otdk -cclean : use bytes instead of chars }
 // Calculate x! (factorial X)
function SDUFactorial(x: Integer): LARGE_INTEGER;
// Generate all permutations of the characters in "pool"
procedure SDUPermutate(pool: String; lst: TStringList);
// Center a control on it's parent
procedure SDUCenterControl(control: TControl; align: TCenterControl); overload;
procedure SDUCenterControl(Controls: TControlArray; align: TCenterControl); overload;
// Position a control relative to it's parent, percentage based (central is 50%)
procedure SDUCenterControl(control: TControl; align: TCenterControl; pcnt: Integer); overload;
procedure SDUCenterControl(Controls: TControlArray; align: TCenterControl;
  pcnt: Integer); overload;
 // double is to int as FloatTrunc(...) is to Trunc(...), but allows control as
 // to the number of decimal places that truncation begins from
function SDUFloatTrunc(X: Double; decimalPlaces: Integer): Double;
// Format ULONGLONG passed in to add thousands separator
function SDUFormatWithThousandsSeparator(const Value: ULONGLONG): String;
 // Given an amount and the units the amount is to be displayed in, this
 // function will pretty-print the value combined with the greatest denomination
 // unit it can
 // e.g. 2621440, [bytes, KB, MB, GB], and 1024 will give "2.5 MB"
 // May be used with constant units declared in this Delphi unit
function SDUFormatUnits(Value: Int64; denominations: array of String;
  multiplier: Integer = 1000; accuracy: Integer = 2): String; overload;
{$IF CompilerVersion >= 18.5}// See comment on ULONGLONG definition
function SDUFormatUnits(Value: ULONGLONG; denominations: array of String;
  multiplier: Integer = 1000; accuracy: Integer = 2): String; overload;
{$IFEND}
// As SDUFormatUnits, but assume units are bytes, KB, MB, GB, etc
function SDUFormatAsBytesUnits(Value: Int64; accuracy: Integer = 2): String; overload;
{$IF CompilerVersion >= 18.5}// See comment on ULONGLONG definition
function SDUFormatAsBytesUnits(Value: ULONGLONG; accuracy: Integer = 2): String; overload;
{$IFEND}
 // As SDUFormatAsBytesUnits, but return as:
 //   <bytes value> bytes (<units value> <units>)
 // with the <units value> part skipped if it's in bytes
function SDUFormatAsBytesAndBytesUnits(Value: ULONGLONG; accuracy: Integer = 2): String;
 // Convert the string representation of a value into it's numerical
 // representation
 // Spaces are ignored
 // e.g.
 //      "10 GB" or "10GB"       -> 1073741824
 //      "10 bytes" or "10bytes" -> 10
 //      "10"                    -> 10
 // Note: This function can't handle values with a decimal point atm
function SDUParseUnits(prettyValue: String; denominations: array of String;
  out Value: Uint64; multiplier: Integer = 1000): Boolean; overload;
{$IFDEF VER185}  // See comment on ULONGLONG definition
// As SDUParseUnits, but assume units are bytes, KB, MB, GB, etc
function SDUParseUnits(
                       prettyValue: string;
                       denominations: array of string;
                       out value: ULONGLONG;
                       multiplier: integer = 1000
                     ): boolean; overload;
{$ENDIF}
// As SDUParseUnits, but assume units are bytes, KB, MB, GB, etc
function SDUParseUnitsAsBytesUnits(prettyValue: String; out Value: uint64): Boolean; overload;
{$IFDEF VER185}   // See comment on ULONGLONG definition
function SDUParseUnitsAsBytesUnits(
                       prettyValue: string;
                       out value: ULONGLONG
                     ): boolean; overload;
{$ENDIF}
// Convert boolean value to string/char
function SDUBoolToStr(Value: Boolean; strTrue: String = 'True';
  strFalse: String = 'False'): String;
function SDUBoolToString(Value: Boolean; strTrue: String = 'True';
  strFalse: String = 'False'): String;
function SDUBooleanToStr(Value: Boolean; strTrue: String = 'True';
  strFalse: String = 'False'): String;
function SDUBooleanToString(Value: Boolean; strTrue: String = 'True';
  strFalse: String = 'False'): String;
function SDUBoolToChar(Value: Boolean; chars: String = 'TF'): Char;
function SDUBooleanToChar(Value: Boolean; chars: String = 'TF'): Char;
// Convert string/char value to boolean
function SDUStrToBool(Value: String; strTrue: String = 'True';
  strFalse: String = 'False'): Boolean;
function SDUStringToBool(Value: String; strTrue: String = 'True';
  strFalse: String = 'False'): Boolean;
function SDUStrToBoolean(Value: String; strTrue: String = 'True';
  strFalse: String = 'False'): Boolean;
function SDUStringToBoolean(Value: String; strTrue: String = 'True';
  strFalse: String = 'False'): Boolean;
function SDUCharToBool(Value: Char; chars: String = 'TF'): Boolean;
function SDUCharToBoolean(Value: Char; chars: String = 'TF'): Boolean;
// Get the Windows directory
function SDUGetWindowsDirectory(): String;
// Get temp directory
function SDUGetTempDirectory(): String;
// Get current working directory (CWD)
function SDUGetCWD(): String;
function SDUGetCurrentWorkingDirectory(): String;
 // Set current working directory (CWD)
 // Only included for completeness
procedure SDUSetCWD(newDir: String);
procedure SDUSetCurrentWorkingDirectory(newDir: String);
 // Get special directory.
 // See CSIDL_DESKTOP, etc consts in ShlObj
function SDUGetSpecialFolderPath(const CSIDL: Integer): String;
 // Check if shortcut exists
 // Returns TRUE if it exists, otherwise FALSE
 // See CSIDL_DESKTOP, etc consts in ShlObj
function SDUDoesShortcutExist(ShortcutCSIDL: Integer; ShortcutName: String): Boolean; overload;
function SDUDoesShortcutExist(ShortcutLocation: String; ShortcutName: String): Boolean; overload;
 // Delete specified shortcut
 // Returns TRUE on success, otherwise FALSE
 // See CSIDL_DESKTOP, etc consts in ShlObj
function SDUDeleteShortcut(ShortcutCSIDL: Integer; ShortcutName: String): Boolean; overload;
function SDUDeleteShortcut(ShortcutLocation: String; ShortcutName: String): Boolean; overload;
 // Create a Windows shortcut file (e.g. a desktop shortcut to an executable)
 // Note: This is only suitable for creating shortcuts to files/executables
 //       (.lnk shortcuts) - NOT URLs (WWW sites; .url files)
 // Examples of use:
 //
 //   Create simple shortcut to running executable on desktop:
 //
 //     SDUCreateShortcut(
 //                       CSIDL_DESKTOP,
 //                       'fred2',
 //                       ParamStr(0)
 //                      );
 //
 //   Create shortcut to running executable on desktop:
 //
 //     SDUCreateShortcut(
 //                       SDUGetSpecialFolderPath(CSIDL_DESKTOP),
 //                       'fred',
 //                       ParamStr(0),
 //                       '/fred',
 //                       'c:\temp',
 //                       //HotKey1.HotKey,
 //                       wsNormal,
 //                       'Comment here'
 //                       );
 //
 // See CSIDL_DESKTOP, etc consts in ShlObj
 //
function SDUCreateShortcut(ShortcutCSIDL: Integer; ShortcutName: String;
  Target: String; Parameters: String = ''; StartIn: String = '';
  // ShortcutKey: TShortCut;  - not yet implemented
  RunWindowState: TWindowState = wsNormal; Comment: String = ''): Boolean; overload;
function SDUCreateShortcut(ShortcutLocation: String; ShortcutName: String;
  Target: String; Parameters: String = ''; StartIn: String = '';
  // ShortcutKey: TShortCut;  - not yet implemented
  RunWindowState: TWindowState = wsNormal; Comment: String = ''): Boolean; overload;
 // Get the "Run" windowstate specified in the identified shortcut
 // Note: This is only suitable for checking shortcuts to files/executables
 //       (.lnk shortcuts) - NOT URLs (WWW sites; .url files)
 // See CSIDL_DESKTOP, etc consts in ShlObj
function SDUGetShortCutRunWindowState(ShortcutCSIDL: Integer;
  ShortcutName: String): TWindowState; overload;
function SDUGetShortCutRunWindowState(ShortcutLocation: String;
  ShortcutName: String): TWindowState; overload;
function SDUGetShortCutArguments(ShortcutCSIDL: Integer; ShortcutName: String): String;
 // Create a file of the specified size, filled with zeros
 // Note: This will *fail* if the file already exists
 // Returns TRUE on success, FALSE on failure
 // If the file creation was cancelled by the user, this function will return
 // FALSE, but set userCancelled to TRUE
function SDUCreateLargeFile(filename: String; size: ULONGLONG; showProgress: Boolean;
  var userCancelled: Boolean): Boolean;
 // Convert the ASCII representation of binary data into binary data
 // ASCIIrep - This must be set to a string containing the ASCII representation
 //            of binary data (e.g. "DEADBEEF010203" as bytes $DE, $AD, $BE,
 //            $EF, $01, $02, $03)
 // Note: ASCIIrep **MUST** have an **even** number of hex chars
 // Note: Whitespace in ASCIIrep is *ignored*
 // Note: This function is not case sensitive
function SDUParseASCIIToData(ASCIIrep: String; var data: String): Boolean;
// Reverse of SDUParseASCIIToData; convert to ASCII string
procedure SDUParseDataToASCII(data: String; var ASCIIrep: String);
 // On a TPageControl, determine if the one tabsheet appears *after* another
 // Returns TRUE if "aSheet" appears *after* bSheet
 // Returns FALSE if "aSheet" appears *before* bSheet
 // Returns FALSE if "aSheet" *is* "bSheet"
function SDUIsTabSheetAfter(pageCtl: TPageControl; aSheet, bSheet: TTabSheet): Boolean;
 // Get size of file
 // This is just a slightly easier to use version of GetFileSize
 // Returns file size, or -1 on error
function SDUGetFileSize(const filename: String): ULONGLONG;
// Get device name for CDROM/DVD
function SDUDeviceNameForCDROM(CDROMNumber: Cardinal): String;
// Get device name for disk
function SDUDeviceNameForDisk(DiskNumber: Cardinal): String;
// Get device name for drive
function SDUDeviceNameForDrive(driveLetter: ansichar): String;
// Get device name for partition
function SDUDeviceNameForPartition(DiskNo: Integer; PartitionNo: Integer): String;
// Get layout of disk
function SDUGetDriveLayout(physicalDiskNo: Integer;
  var driveLayout: TSDUDriveLayoutInformation): Boolean;
function SDUGetDriveLayout_Device(driveDevice: String;
  var driveLayout: TSDUDriveLayoutInformation): Boolean;
 // Get size of partition
 // Returns file size, or -1 on error
function SDUGetPartitionSize(driveletter: ansichar): ULONGLONG;
function SDUGetPartitionSize_Device(driveDevice: String): ULONGLONG;
 // Get partition information
 // Returns TRUE/FALSE on success/failure
function SDUGetPartitionInfo(driveletter: ansichar; var partInfo: TSDUPartitionInfo): Boolean;
  overload;
function SDUGetPartitionInfo(driveDevice: String; var partInfo: TSDUPartitionInfo): Boolean;
  overload;
 // Get disk geometry
 // Returns TRUE/FALSE on success/failure
function SDUGetDiskGeometry(driveletter: ansichar; var diskGeometry: TSDUDiskGeometry): Boolean;
  overload;
function SDUGetDiskGeometry(DiskNumber: Integer; var diskGeometry: TSDUDiskGeometry): Boolean;
  overload;
function SDUGetDiskGeometry(driveDevice: String; var diskGeometry: TSDUDiskGeometry): Boolean;
  overload;
// Returns installed OS
function SDUInstalledOS(): TInstalledOS;
// Returns TRUE if installed OS is Windows Vista or later
function SDUOSVistaOrLater(): Boolean;
 // Returns 32/64, depending on version of *OS* running (e.g. Windows XP x64
 // returns 64)
function SDUOSCPUSize(): Integer;
// Returns TRUE if running on 64 bit OS (e.g. Windows XP x64)
function SDUOS64bit(): Boolean;
//is the *app* 64 bit?
function SDUApp64bit(): Boolean;
// Register the specified filename extension to launch the command given
function SDUFileExtnRegCmd(fileExtn: String; menuItem: String; command: String): Boolean;
function SDUFileExtnUnregCmd(fileExtn: String; menuItem: String): Boolean;
// Get command associated with the specified file extension
function SDUFileExtnGetRegCmd(fileExtn: String; menuItem: String): String;
 // Identify if the specified executable forms any part of the command for the
 // associated file extension
function SDUFileExtnIsRegCmd(fileExtn: String; menuItem: String; executable: String): Boolean;
// Register the specified filename extension to use the specified icon
function SDUFileExtnRegIcon(fileExtn: String; filename: String; iconNum: Integer): Boolean;
function SDUFileExtnUnregIcon(fileExtn: String): Boolean;
// Return TRUE/FALSE, depending on if the file extension is registered or not
function SDUFileExtnIsRegd(fileExtn: String; menuItem: String): Boolean;
 // Get a string with the drive letters of all drives present/not present, in
 // order
 // Return value is all in uppercase
function SDUGetUsedDriveLetters(): String;
function SDUGetUnusedDriveLetters(): String;
function SDUGetNetworkDriveLetters(): String;
// Populate "output" with a pretty-printed hex display of the contents of "data"
function SDUPrettyPrintHex(data: Pointer; offset: Longint; bytes: Longint;
  output: TStringList; Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): Boolean; overload;
function SDUPrettyPrintHex(data: String; offset: Longint; bytes: Longint;
  output: TStringList; Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): Boolean; overload;
function SDUPrettyPrintHex(data: TStream; offset: Longint; bytes: Longint;
  output: TStringList; Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): Boolean; overload;
// As "SDUPrettyPrintHex(...)", but returns output as a string, instead of as a TStringList and TURE/FALSE flag
// Returns '' on error/if no data was supplied
function SDUPrettyPrintHexStr(data: Pointer; offset: Longint; bytes: Longint;
  Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): String; overload;
function SDUPrettyPrintHexStr(data: String; offset: Longint; bytes: Longint;
  Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): String; overload;
function SDUPrettyPrintHexStr(data: TStream; offset: Longint; bytes: Longint;
  Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): String; overload;
// Simple version
function SDUPrettyPrintHexStrSimple(data: String): String;
 // Setup a Open/Save dialog with supplied default filename & path
 // Fixes problem with just setting "filename" for these dialogs before
 // Execute()ing them
procedure SDUOpenSaveDialogSetup(dlg: TCommonDialog; defaultFilename: String);
// Convert short filename to LFN
function SDUConvertSFNToLFN(sfn: String): String;
// Convert LFN to short filename
function SDUConvertLFNToSFN(lfn: String): String;
// Enable/disable the specified control, with correct colors
procedure SDUEnableControl(control: TControl; enable: Boolean;
  affectAssociatedControls: Boolean = True); overload;
 //procedure SDUEnableControl(control: TAction; enable: Boolean); OVERLOAD;
 //procedure SDUEnableControl(control: TMenuItem; enable: Boolean); OVERLOAD;
 // Readonly/read-write the specified control, with correct colors
procedure SDUReadonlyControl(control: TControl; ReadOnly: Boolean;
  affectAssociatedControls: Boolean = True);
// Set "value" to the value of the command line parameter "-<parameter> value". Returns TRUE/FALSE on success/failure
function SDUCommandLineParameter(parameter: String; var Value: String): Boolean; overload;
function SDUCommandLineParameter(parameter: String; var Value: Integer): Boolean; overload;
// Returns TRUE if the specified command line switch could be found, otherwise FALSE
function SDUCommandLineSwitch(parameter: String): Boolean;
// Returns the parameter number in the command line of the specified parameter. Returns -1 on failure
function SDUCommandLineSwitchNumber(parameter: String): Integer;
// Returns the executables version numbers as set in Project|Options|Version Info
function SDUGetVersionInfo(filename: String; var majorVersion, minorVersion: Integer): Boolean;
  overload;
function SDUGetVersionInfo(filename: String;
  var majorVersion, minorVersion, revisionVersion, buildVersion: Integer): Boolean; overload;
// As SDUGetVersionInfo, but returns a nicely formatted string
function SDUGetVersionInfoString(filename: String): String;
function SDUVersionInfoToString(majorVersion: Integer; minorVersion: Integer;
  betaVersion: Integer = -1): String; overload;
function SDUVersionInfoToString(majorVersion: Integer; minorVersion: Integer;
  revisionVersion: Integer; buildVersion: Integer; betaVersion: Integer = -1): String; overload;
 // As SDUGetVersionInfo, but gets information from the PAD file at the
 // specified URL, or the XML passed in directly
function SDUGetPADFileVersionInfo(url: String; var majorVersion, minorVersion: Integer;
  userAgent: WideString = DEFAULT_HTTP_USERAGENT; ShowProgressDlg: Boolean = True): TTimeoutGet;
  overload;
function SDUGetPADFileVersionInfo(url: String;
  var majorVersion, minorVersion, revisionVersion, buildVersion: Integer;
  userAgent: WideString = DEFAULT_HTTP_USERAGENT; ShowProgressDlg: Boolean = True): TTimeoutGet;
  overload;
function SDUGetPADFileVersionInfo_XML(XML: String;
  var majorVersion, minorVersion, revisionVersion, buildVersion: Integer): Boolean;
 // As SDUGetVersionInfoString, but gets information from the PAD file at the
 // specified URL, or the XML passed in directly
function SDUGetPADFileVersionInfoString(url: String): String;
function SDUGetPADFileVersionInfoString_XML(XML: String): String;
 // Check version IDs
 // If A > B, return -1
 // If A = B, return  0
 // If A < B, return  1
function SDUVersionCompare(A_MajorVersion, A_MinorVersion: Integer;
  B_MajorVersion, B_MinorVersion: Integer): Integer; overload;
function SDUVersionCompare(A_MajorVersion, A_MinorVersion, A_RevisionVersion,
  A_BuildVersion: Integer; B_MajorVersion, B_MinorVersion, B_RevisionVersion,
  B_BuildVersion: Integer): Integer; overload;
function SDUVersionCompareWithBetaFlag(A_MajorVersion, A_MinorVersion: Integer;
  A_BetaVersion: Integer; B_MajorVersion, B_MinorVersion: Integer): Integer; overload;
function SDUVersionCompareWithBetaFlag(A_MajorVersion, A_MinorVersion,
  A_RevisionVersion, A_BuildVersion: Integer; A_BetaVersion: Integer;
  B_MajorVersion, B_MinorVersion, B_RevisionVersion, B_BuildVersion: Integer): Integer; overload;
 // Pause for the given number of ms
 // dont really want to do busy waits
 //procedure SDUPause(delayLen: Integer);
 // Execute the specified commandline and return when the command line returns
function SDUWinExecAndWait32(const cmdLine: String; cmdShow: Integer;
  workDir: String = ''; appName: String = ''): Cardinal;
function SDUWinExecNoWait32(cmdLine, params: String; cmdShow: Integer): Boolean;

function SDUWinExecAndWaitOutput(const cmdLine: String; res: TStrings;
  const workDir: String = ''): Cardinal;

// Returns the control within parentControl which has the specified tag value
function SDUGetControlWithTag(tag: Integer; parentControl: TControl): TControl;
 // Search out and return the first control found which has ".FocusControl" set
 // to the control passed in
function SDUGetControlWithFocusControl(ctrl: TControl): TWinControl;
// Display the Windows shell dialog displaying the properties for the specified item
procedure SDUShowFileProperties(const filename: String);
// Get a list of all environment variables
function SDUGetEnvironmentStrings(envStrings: TStringList): Boolean;
// Get the value of the specified environment variable
function SDUGetEnvironmentVar(envVar: String; var Value: String): Boolean;
 // Set the specified time/datestamps on the file referred to by Handle
 // Identical to "SetFileTime", but updates all 3 timestamps (created, last
 // modified and last accessed)
function SDUSetAllFileTimes(Handle: Integer; Age: Integer): Integer;
// Convert TDateTime date/timestamp to TFileTime for use with SetFileTime(...)
function SDUDateTimeToFileTime(dateTime: TDateTime): TFileTime;
// Convert TFileTime to TTimeStamp
function SDUFileTimeToTimeStamp(fileTime: TFileTime): TTimeStamp;
// Convert TFileTime to TDateTime
function SDUFileTimeToDateTime(fileTime: TFileTime): TDateTime;
// Encode date to YYYY-MM-DD format
function SDUTDateToISO8601(inDate: TDate): String;
// Encode date to HH:MM[:SS] format
function SDUTTimeToISO8601(inTime: TTime; includeSeconds: Boolean = True): String;
// Encode date to YYYY-MM-DDTHH:MM[:SS] format
function SDUTDateTimeToISO8601(inDateTime: TDateTime; includeSeconds: Boolean = True): String;
// Decode date from ISO8601 YYYY-MM-DD or YYYYMMDD format
function SDUISO8601ToTDate(inDate: String): TDate;
// Decode time from ISO8601 HH:MM[:SS] or HHMM[SS] format
function SDUISO8601ToTTime(inTime: String): TTime;
// Decode ISO8601 date/time from to TDateTime
function SDUISO8601ToTDateTime(inDateTime: String): TDateTime;
 // Counts the number of instances of theChar in theString, and returns this
 // count
function SDUCountCharInstances(theChar: Char; theString: String): Integer;
// These two functions ripped from FileCtrl.pas - see Delphi 4 source
function SDUVolumeID(DriveChar: DriveLetterChar): String;
function SDUNetworkVolume(DriveChar: DriveLetterChar): String;
{$IFDEF MSWINDOWS}
 // This calls SDUVolumeID/SDUNetworkVolume as appropriate
 // Returns '3.5" Floppy'/'Removable Disk' instead of volume label for these
 // type of drives
function SDUGetVolumeID(drive: DriveLetterChar): String;
{$ENDIF}
 // Detect if the current application is already running, and if it is, return
 // a handle to it's main window.
 // Returns 0 if the application is not currently running
function SDUDetectExistingApp(): THandle;
// Send a message to all existing apps
{$IF CompilerVersion < 18.5}
procedure SDUSendMessageExistingApp(msg: Cardinal; wParam: Integer; lParam: Integer);
  deprecated; // deprecated until it works properly with Delphi 2007; see Assert(...) in implementation
{$IFEND}
// Post a message to all existing apps
{$IF CompilerVersion < 18.5}
procedure SDUPostMessageExistingApp(msg: Cardinal; wParam: Integer; lParam: Integer);
  deprecated; // deprecated until it works properly with Delphi 2007; see Assert(...) in implementation
{$IFEND}
// Split the string supplied into two parts, before and after the split char
function SDUSplitString(wholeString: String; var firstItem: String;
  var theRest: String; splitOn: Char = ' '): Boolean;
// Split the string supplied into two parts, before and after the split char
function SDUSplitWideString(wholeString: WideString; var firstItem: WideString;
  var theRest: WideString; splitOn: Widechar = ' '): Boolean;
 // As TryStrToInt, but can handle 64 bit values
 //function SDUTryStrToInt(const S: String; out Value: Integer): Boolean; OVERLOAD;
function SDUTryStrToInt(const S: String; out Value: Uint64): Boolean; overload;
{$IFDEF VER185}  // See comment on ULONGLONG definition
function SDUTryStrToInt(const S: string; out Value: ULONGLONG): boolean; overload;
{$ENDIF}
 // As TryStrToInt, but if it fails to convert the string, return the default
 // value passed in
function SDUTryStrToIntDflt(Value: String; deflt: Integer): Integer;
// Convert a value into its binary representation
function SDUIntToBin(Value: Integer; digits: Integer = 8): String;
function SDUIntToBinary(Value: Integer; digits: Integer = 8): String;
// Convert a hex number into an integer
function SDUHexToInt(hex: String): Integer;
function SDUTryHexToInt(hex: String; var Value: Integer): Boolean;
 // Convert from int to hex representation
 // Implemented as Delphi 2007 can't truncates int64 values to 32 bits when
 // using inttohex(...)!
function SDUIntToHex(val: ULONGLONG; digits: Integer): String;
 // Convert from int to string representation
 // Implemented as Delphi 2007 truncates int64 values to 32 bits when
 // using inttostr(...)!
function SDUIntToStr(val: Int64): String; overload;
{$IF CompilerVersion >= 18.5}// See comment on ULONGLONG definition
function SDUIntToStr(val: ULONGLONG): String; overload;
{$IFEND}
// As SDUIntToStr, but with thousands seperators inserted
function SDUIntToStrThousands(val: Int64): String; overload;
{$IF CompilerVersion >= 18.5}// See comment on ULONGLONG definition
function SDUIntToStrThousands(val: ULONGLONG): String; overload;
{$IFEND}
// Save the given font's details to the registry
function SDUSaveFontToReg(rootKey: HKEY; fontKey: String; Name: String; font: TFont): Boolean;
// Load the font's details back from the registry
function SDULoadFontFromReg(rootKey: HKEY; fontKey: String; Name: String; font: TFont): Boolean;

//replaces any subst/mapping
function SDUGetFinalPath(path: String): String;

function SDUGetSystemDirectory(): String;

 { TODO 1 -otdk -cenhance : convert to using byte array when are sure all callers support it }
 //encodes as ascii for now
function SDUStringToSDUBytes(const rhs: Ansistring): TSDUBytes;
function SDUBytesToString(const Value: TSDUBytes): Ansistring;
//sets bytes to all zeros and sets length to 0
procedure SDUInitAndZeroBuffer(len: Cardinal; var val: TSDUBytes);
//sets bytes to all zeros
procedure SDUZeroBuffer(var buf: array of Byte);
procedure SDUZeroString(var buf: Ansistring);

//adds byte to array
procedure SDUAddByte(var Value: TSDUBytes; byt: Byte);
procedure SDUAddArrays(var A: TSDUBytes; const rhs: TSDUBytes);
// adds rhs to end up lhs up to 'limit' bytes from rhs
procedure SDUAddLimit(var lhs: TSDUBytes; const rhs: TSDUBytes; limit: Integer);
procedure SDUDeleteFromStart(var A: TSDUBytes; Count: Integer); //del count from start of array
 //same as setLength , but seroing any freed bytes. even if AlwaysClearFreedMemory not set
procedure SafeSetLength(var A: TSDUBytes; newLen: Integer);
procedure SDUCopy(var aTo: TSDUBytes; const aFrom: TSDUBytes);

//copies from aFrom to aTo up to limit bytes, sets length and zeroises any freed data
procedure SDUCopyLimit(var aTo: TSDUBytes; const aFrom: TSDUBytes; limit: Integer);

function SDUMapNetworkDrive(networkShare: String; useDriveLetter: Char): Boolean;

implementation

uses
  ComObj,
{$IF CompilerVersion >= 18.5}
  WideStrUtils,
{$IFEND}
  ShellAPI, // Required for SDUShowFileProperties
{$IFDEF MSWINDOWS}
{$WARN UNIT_PLATFORM OFF}// Useless warning about platform - we're already
  // protecting against that!
  FileCtrl,              // Required for TDriveType
{$WARN UNIT_PLATFORM ON}
{$ENDIF}
  registry,
  SDUDialogs,
  SDUi18n,
  SDUProgressDlg,
  SDUWindows,
  Spin64,  // Required for TSpinEdit64
  Math,    // Required for Power(...)
  Messages,
  SDUSpin64Units,
  SDUWindows64,
  xmldom,  // Required for IDOMDocument, etc
  XMLdoc,  // Required for TXMLDocument
  IOUtils; // for  TFile.ReadAllText


const
  SDU_FILE_DEVICE_FILE_SYSTEM  = $00000009;
  SDU_FILE_DEVICE_UNKNOWN      = $00000022;
  SDU_FILE_DEVICE_MASS_STORAGE = $0000002d;

  SDU_FILE_ANY_ACCESS   = 0;
  SDU_FILE_READ_ACCESS  = $0001;    // file & pipe
  SDU_FILE_WRITE_ACCESS = $0002;    // file & pipe

  SDU_METHOD_BUFFERED = 0;

  // Constants from winioctl.h (from the MS DDK)
  SDU_FILE_READ_DATA     = SDU_FILE_READ_ACCESS;
  SDU_FILE_DEVICE_CD_ROM = $00000002;
  SDU_FILE_DEVICE_DISK   = $00000007;
  SDU_FILE_DEVICE_DVD    = $00000033;

  SDU_IOCTL_DISK_BASE = SDU_FILE_DEVICE_DISK;

  // From winioctl.h
  SDU_IOCTL_DISK_GET_DRIVE_GEOMETRY =
    (((SDU_IOCTL_DISK_BASE) * $10000) or ((SDU_FILE_ANY_ACCESS) * $4000) or
    (($0000) * $4) or (SDU_METHOD_BUFFERED));

  // From winioctl.h
  SDU_IOCTL_DISK_GET_PARTITION_INFO =
    (((SDU_IOCTL_DISK_BASE) * $10000) or ((SDU_FILE_READ_ACCESS) * $4000) or
    (($0001) * $4) or (SDU_METHOD_BUFFERED));

  SDU_IOCTL_DISK_GET_DRIVE_LAYOUT =
    (((SDU_IOCTL_DISK_BASE) * $10000) or ((SDU_FILE_READ_ACCESS) * $4000) or
    (($0003) * $4) or (SDU_METHOD_BUFFERED));

  DIRECTORY_TYPE = 'Directory';

  WINDOW_LAYOUT_STATE  = 'WINDOW_LAYOUT_STATE';
  WINDOW_LAYOUT_TOP    = 'WINDOW_LAYOUT_TOP';
  WINDOW_LAYOUT_LEFT   = 'WINDOW_LAYOUT_LEFT';
  WINDOW_LAYOUT_HEIGHT = 'WINDOW_LAYOUT_HEIGHT';
  WINDOW_LAYOUT_WIDTH  = 'WINDOW_LAYOUT_WIDTH';

resourcestring
  ZLIBCOMPRESSLVL_NONE    = 'None';
  ZLIBCOMPRESSLVL_FASTEST = 'Fastest';
  ZLIBCOMPRESSLVL_DEFAULT = 'Default';
  ZLIBCOMPRESSLVL_MAX     = 'Maximum';

const
  ZLibCompressionLevelTitlePtr: array [TCompressionLevel] of Pointer =
    (@ZLIBCOMPRESSLVL_NONE, @ZLIBCOMPRESSLVL_FASTEST, @ZLIBCOMPRESSLVL_DEFAULT,
    @ZLIBCOMPRESSLVL_MAX
    );

var
  _SDUDetectExist_AppName:   array [0..255] of Char;
  _SDUDetectExist_ClassName: array [0..255] of Char;
  _SDUDetectExist_CntFound:  Integer;
  _SDUDetectExist_LastFound: HWnd;

  _SDUSendMsg_msg:    Cardinal;
  _SDUSendMsg_wParam: Integer;
  _SDUSendMsg_lParam: Integer;



procedure _SDUDetectExistWindowDetails(); forward;
function _SDUDetectExistWindowDetails_ThisClassHandle(): THandle; forward;

 // Starting from offset "offset" (zero indexed) in "data", read "bytes" bytes
 // and populate "output" with a pretty printed representation of the data
 // If "bytes" is set to -1, then this will read in *all* data from "data",
 // starting from "offset" until the end of the stream
 // !! WARNING !!
 // AFTER CALLING THIS FUNCTION, THE CURRENT POSITION IN "data" WILL BE
 // "offset"+"bytes" (or the last byte, whichever is the lesser)
 // Each line in "output" will relate to "width" bytes from "data"
 // Returns: TRUE/FALSE on success/failure
function SDUPrettyPrintHex(data: TStream; offset: Longint; bytes: Longint;
  output: TStringList; Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): Boolean;
var
  i:           Integer;
  lineHexRep:  String;
  lineCharRep: String;
  currLine:    String;
  posInLine:   Cardinal;
  x:           Byte;
  lineOffset:  Integer;
  bytesRead:   Integer;
  finished:    Boolean;
begin
  Result := True;

  data.Position := offset;

  lineOffset  := data.Position;
  lineHexRep  := '';
  lineCharRep := '';
  posInLine   := 1;

  finished := False;
  // Loop around until either we have processed "bytes" bytes from "data", or
  // "finished" is set - this is done in this way so that -1 can be specified
  // for "bytes" to indicate that processing should be carried out until the
  // end of "data" is reached
  while (((bytes = -1) or ((data.Position - offset) < bytes)) and not (finished)) do begin

    bytesRead := data.Read(x, 1);
    if (bytesRead = 0) then begin
      // If the read fails, then this is an error, unless we're supposed to be
      // processing until we run out of data.
      if (bytes <> -1) then begin
        Result := False;
      end;

      finished := True;
    end else begin
      lineHexRep := lineHexRep + inttohex(x, 2) + ' ';
      if ((x >= 32) and (x < 127)) then begin
        lineCharRep := lineCharRep + Char(x);
      end else begin
        lineCharRep := lineCharRep + '.';
      end;


      Inc(posInLine);
      if (posInLine > Width) then begin
        posInLine := 1;
        currLine  := inttohex(lineOffset, dispOffsetWidth) + ' | ' + lineHexRep +
          '| ' + lineCharRep;
        output.add(currLine);
        lineOffset  := data.Position;
        lineHexRep  := '';
        lineCharRep := '';
      end;

    end;

  end;

  if (length(lineCharRep) > 0) then begin
    // width-posInLine+1 - the +1 is because it posInLine was incremented at the end of the previous loop
    for i := 1 to (Width - posInLine + 1) do begin
      lineHexRep  := lineHexRep + '   ';
      lineCharRep := lineCharRep + ' ';
    end;
    currLine := inttohex(lineOffset, dispOffsetWidth) + ' | ' + lineHexRep + '| ' + lineCharRep;
    output.add(currLine);
  end;

end;


 // Starting from offset "offset" (zero indexed) in "data", read "bytes" bytes
 // and populate "output" with a pretty printed representation of the data
 // If "bytes" is set to -1, then this will read in *all* data from "data",
 // starting from "offset" until the end of the stream
 // Each line in "output" will relate to "width" bytes from "data"
 // Returns: TRUE/FALSE on success/failure
function SDUPrettyPrintHex(data: String; offset: Longint; bytes: Longint;
  output: TStringList; Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): Boolean; overload;
var
  dataStream: TStringStream;
begin
  dataStream := TStringStream.Create(data);
  try
    Result := SDUPrettyPrintHex(dataStream, offset, bytes, output, Width, dispOffsetWidth);
  finally
    dataStream.Free();
  end;

end;


function SDUPrettyPrintHex(data: Pointer; offset: Longint; bytes: Longint;
  output: TStringList; Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): Boolean; overload;
var
  str: String;
  i:   Integer;
begin
  str := '';
  for i := 0 to ((offset + bytes) - 1) do begin
    str := str + (PChar(data))[i];
  end;

  Result := SDUPrettyPrintHex(str, offset, bytes, output, Width, dispOffsetWidth);

end;


function SDUPrettyPrintHexStr(data: Pointer; offset: Longint; bytes: Longint;
  Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): String; overload;
var
  sl: TStringList;
begin
  Result := '';

  sl := TStringList.Create();
  try
    if SDUPrettyPrintHex(data, offset, bytes, sl, Width, dispOffsetWidth) then begin
      Result := sl.Text;
    end;

  finally
    sl.Free();
  end;

end;

function SDUPrettyPrintHexStr(data: String; offset: Longint; bytes: Longint;
  Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): String; overload;
var
  sl: TStringList;
begin
  Result := '';

  sl := TStringList.Create();
  try
    if SDUPrettyPrintHex(data, offset, bytes, sl, Width, dispOffsetWidth) then begin
      Result := sl.Text;
    end;

  finally
    sl.Free();
  end;

end;

function SDUPrettyPrintHexStr(data: TStream; offset: Longint; bytes: Longint;
  Width: Cardinal = 8; dispOffsetWidth: Cardinal = 8): String; overload;
var
  sl: TStringList;
begin
  Result := '';

  sl := TStringList.Create();
  try
    if SDUPrettyPrintHex(data, offset, bytes, sl, Width, dispOffsetWidth) then begin
      Result := sl.Text;
    end;

  finally
    sl.Free();
  end;

end;

function SDUPrettyPrintHexStrSimple(data: String): String;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to length(data) do begin
    if (Result <> '') then begin
      Result := Result + ' ';
    end;

    Result := Result + '0x' + inttohex(Ord(data[i]), 2);
  end;

end;

 // Setup a Open/Save dialog with supplied default filename & path
 // Fixes problem with just setting "filename" for these dialogs before
 // Execute()ing them
procedure SDUOpenSaveDialogSetup(dlg: TCommonDialog; defaultFilename: String);
var
  filename: String;
  initDir:  String;
begin
  // uses FileCtrl
  defaultFilename := trim(defaultFilename);
  if not (SysUtils.DirectoryExists(defaultFilename)) then begin
    filename := extractfilename(defaultFilename);
    initDir  := extractfilepath(defaultFilename);
  end else begin
    filename := '';
    initDir  := defaultFilename;
  end;

  if dlg is TOpenDialog then begin
    TOpenDialog(dlg).filename   := filename;
    TOpenDialog(dlg).initialdir := initDir;
    TOpenDialog(dlg).Options    := TOpenDialog(dlg).Options + [ofPathMustExist];
    TOpenDialog(dlg).Options    := TOpenDialog(dlg).Options + [ofFileMustExist];
  end else
  if dlg is TSaveDialog then begin
    TSaveDialog(dlg).filename   := filename;
    TSaveDialog(dlg).initialdir := initDir;
  end;

end;

 // Convert a short path & filename to it's LFN version
 // [IN] sfn - the short filename to be converted into a long filename
 // Returns - the long filename
function SDUConvertSFNToLFN(sfn: String): String;

  function SDUConvertSFNPartToLFN(sfn: String): String;
  var
    temp:         TWIN32FindData;
    searchHandle: THandle;
  begin
    searchHandle := FindFirstFile(PChar(sfn), temp);
    if searchHandle <> ERROR_INVALID_HANDLE then begin
      Result := String(temp.cFileName);
      if Result = '' then begin
        Result := String(temp.cAlternateFileName);
      end;
    end else begin
      Result := '';
    end;
    Windows.FindClose(searchHandle);
  end;

var
  lastSlash:   PChar;
  tempPathPtr: PChar;
  copySfn:     String;
begin
  sfn := SDUConvertLFNToSFN(sfn);

  Result := '';

  if not (FileExists(sfn)) and not (SysUtils.DirectoryExists(sfn)) then begin
    // Result already set to '' so just exit.
    exit;
  end;

  copySfn     := copy(sfn, 1, length(sfn));
  tempPathPtr := PChar(copySfn);
  lastSlash   := StrRScan(tempPathPtr, '\');
  while lastSlash <> nil do begin
    Result := '\' + SDUConvertSFNPartToLFN(tempPathPtr) + Result;
    if lastSlash <> nil then begin
      lastSlash^ := Char(0);
      lastSlash  := StrRScan(tempPathPtr, '\');

      // This bit is required to take into account the possibility of being
      // passed a UNC filename (e.g. \\computer_name\share_name\path\filename)
      if ((Pos('\\', tempPathPtr) = 1) and (SDUCountCharInstances('\', tempPathPtr) = 3))
      then begin
        lastSlash := nil;
      end;
    end;

  end;

  if tempPathPtr[1] = ':' then begin
    tempPathPtr[0] := upcase(tempPathPtr[0]);
  end;

  Result := tempPathPtr + Result;
end;


 // Convert a LFN to it's short version
 // [IN] lfn - the LFN to be converted into a short filename
 // Returns - the short filename
function SDUConvertLFNToSFN(lfn: String): String;
var
  sfn: String;
begin
  if not (FileExists(lfn)) and not (SysUtils.DirectoryExists(lfn)) then begin
    Result := '';
    exit;
  end;

  sfn := ExtractShortPathName(lfn);
  if sfn[2] = ':' then begin
    sfn[1] := upcase(sfn[1]);
  end;
  Result := sfn;
end;

 //procedure SDUEnableControl(control: TAction; enable: Boolean);
 //begin
 //  control.Enabled := enable;
 //end;
 //
 //procedure SDUEnableControl(control: TMenuItem; enable: Boolean);
 //begin
 //  control.Enabled := enable;
 //end;

procedure SDUEnableControl(control: TControl; enable: Boolean;
  affectAssociatedControls: Boolean = True);
var
  i: Integer;
begin
  if not (control is TPageControl) and not (control is TForm) and not (control is TPanel) then
  begin
    control.Enabled := enable;
  end;

  if (control is TEdit) or (control is TRichedit) or (control is TDateTimePicker) or
    (control is TComboBox) then begin
    if enable then begin
      TEdit(control).color := clWindow;
    end else begin
      TEdit(control).color := clBtnFace;
    end;
  end else
  if ((control is THotKey) or (control is TSpinEdit64) or (control is TSpinEdit64)) then begin
    if enable then begin
      TWinControl(control).Brush.Color := clWindow;
    end else begin
      TWinControl(control).Brush.Color := clBtnFace;
    end;
  end else
  if control is TWinControl then begin
    for i := 0 to (TWinControl(control).ControlCount - 1) do begin
      SDUEnableControl(TWinControl(control).Controls[i], enable);
    end;
  end;

  if control is TRichedit then begin
    TRichedit(control).Enabled  := True;
    TRichedit(control).ReadOnly := not (enable);
  end;

  if control is TGroupBox then begin
    if enable then begin
      TGroupBox(control).Font.Color := clWindowText;
    end else begin
      TGroupBox(control).Font.Color := clInactiveCaption;
    end;

  end;

  // Seek any controls with the same parent that have FocusControl set to this
  // component, and enabling/disabling it then as appropriate
  if affectAssociatedControls then begin
    if (control.Parent <> nil) then begin
      if (control.Parent is TWinControl) then begin
        for i := 0 to (TWinControl(control.Parent).ControlCount - 1) do begin
          if (TWinControl(control.Parent).Controls[i] is TLabel) then begin
            if (TLabel(TWinControl(control.Parent).Controls[i]).FocusControl = control) then begin
              SDUEnableControl(TWinControl(control.Parent).Controls[i], enable);
            end;
          end else
          if (TWinControl(control.Parent).Controls[i] is TStaticText) then begin
            if (TStaticText(TWinControl(control.Parent).Controls[i]).FocusControl = control) then
            begin
              SDUEnableControl(TWinControl(control.Parent).Controls[i], enable);
            end;
          end;
        end;  // for i:=0 to (TWinControl(control.Parent).ControlCount-1) do
      end;  // if (control.Parent is TWinControl) then
    end;  // if (control.Parent <> nil) then
  end;  // if affectAssociatedControls then

end;

procedure SDUReadonlyControl(control: TControl; ReadOnly: Boolean;
  affectAssociatedControls: Boolean = True);
var
  i:       Integer;
  bgColor: TColor;
  fgColor: TColor;
begin
  fgColor := clWindowText;
  if ReadOnly then begin
    bgColor := clInactiveBorder;
  end else begin
    bgColor := clWindow;
  end;

  if (control is TEdit) then begin
    TEdit(control).ReadOnly := ReadOnly;
    TEdit(control).color    := bgColor;
  end;

  if (control is TRichedit) then begin
    TRichedit(control).ReadOnly := ReadOnly;
  end;

  if (control is TComboBox) then begin
    SDUEnableControl(control, not (ReadOnly));
    // This font color change has no effect?!
    TComboBox(control).Font.color := fgColor;
  end;

  if (control is TDateTimePicker) then begin
    SDUEnableControl(control, not (ReadOnly));
    TDateTimePicker(control).color := bgColor;
  end;

  if (control is TSpinEdit64) then begin
    TSpinEdit64(control).ReadOnly := ReadOnly;
    TSpinEdit64(control).Color    := bgColor;
  end;

  if (control is TSDUSpin64Unit) then begin
    TSDUSpin64Unit(control).ReadOnly := ReadOnly;
  end;

  // Seek any controls with the same parent that have FocusControl set to this
  // component, and enabling/disabling it then as appropriate
  if affectAssociatedControls then begin
    if (control.Parent <> nil) then begin
      if (control.Parent is TWinControl) then begin
        for i := 0 to (TWinControl(control.Parent).ControlCount - 1) do begin
          if (TWinControl(control.Parent).Controls[i] is TLabel) then begin
            if (TLabel(TWinControl(control.Parent).Controls[i]).FocusControl = control) then begin
              SDUReadonlyControl(TWinControl(control.Parent).Controls[i], ReadOnly);
            end;
          end else
          if (TWinControl(control.Parent).Controls[i] is TStaticText) then begin
            if (TStaticText(TWinControl(control.Parent).Controls[i]).FocusControl = control) then
            begin
              SDUReadonlyControl(TWinControl(control.Parent).Controls[i], ReadOnly);
            end;
          end;
        end;  // for i:=0 to (TWinControl(control.Parent).ControlCount-1) do
      end;  // if (control.Parent is TWinControl) then
    end;  // if (control.Parent <> nil) then
  end;  // if affectAssociatedControls then

end;

function SDUCommandLineParameter(parameter: String; var Value: Integer): Boolean;
var
  strValue: String;
begin
  Result := SDUCommandLineParameter(parameter, strValue);
  if Result then
    Result := TryStrToInt(strValue, Value);
end;

function SDUCommandLineParameter(parameter: String; var Value: String): Boolean;
var
  i:         Integer;
  testParam: String;
begin
  Result    := False;
  parameter := uppercase(parameter);
  for i := 1 to (ParamCount - 1) do begin
    testParam := uppercase(ParamStr(i));
    if ((testParam = ('-' + parameter)) or (testParam = ('/' + parameter))) then begin
      Value  := ParamStr(i + 1);
      Result := True;
      break;
    end;
  end;

end;


function SDUCommandLineSwitch(parameter: String): Boolean;
var
  i: Integer;
begin
  Result    := False;
  parameter := uppercase(parameter);
  for i := 1 to ParamCount do begin
    if (uppercase(ParamStr(i)) = ('-' + parameter)) or (uppercase(ParamStr(i)) = ('/' + parameter))
    then begin
      Result := True;
      break;
    end;
  end;

end;

function SDUCommandLineSwitchNumber(parameter: String): Integer;
var
  i: Integer;
begin
  Result    := -1;
  parameter := uppercase(parameter);
  for i := 1 to ParamCount do begin
    if (uppercase(ParamStr(i)) = ('-' + parameter)) or (uppercase(ParamStr(i)) = ('/' + parameter))
    then begin
      Result := i;
      break;
    end;
  end;

end;

function SDUGetVersionInfo(filename: String; var majorVersion, minorVersion: Integer): Boolean;
var
  junk: Integer;
begin
  Result := SDUGetVersionInfo(filename, majorVersion, minorVersion, junk, junk);
end;

// Set filename to '' to get version info on the currently running executable
function SDUGetVersionInfo(filename: String;
  var majorVersion, minorVersion, revisionVersion, buildVersion: Integer): Boolean;
var
  vsize:     Integer;
  puLen:     Cardinal;
  dwHandle:  DWORD;
  pBlock:    Pointer;
  pVPointer: Pointer;
  tvs:       PVSFixedFileInfo;
  //  iLastError: Cardinal;
begin
  Result := False;

  if filename = '' then
    filename := Application.ExeName;

  vsize := GetFileVersionInfoSize(PChar(filename), dwHandle);

  if vsize = 0 then
    exit;

  GetMem(pBlock, vsize);
  try
    if GetFileVersionInfo(PChar(filename), dwHandle, vsize, pBlock) then begin

      VerQueryValue(pBlock, '\', pVPointer, puLen);
      if puLen > 0 then begin
        //      iLastError := GetLastError;
        //      showmessage(Format('GetFileVersionInfo failed: (%d) %s',                           [iLastError, SysErrorMessage(iLastError)]));
        tvs             := PVSFixedFileInfo(pVPointer);
        majorVersion    := tvs^.dwFileVersionMS shr 16;
        minorVersion    := tvs^.dwFileVersionMS and $ffff;
        revisionVersion := tvs^.dwFileVersionLS shr 16;
        buildVersion    := tvs^.dwFileVersionLS and $ffff;
        Result          := True;
      end;
    end;
  finally
    FreeMem(pBlock);
  end;

end;

 // Get version ID string for the specified executable
 // filename - The name of the executable to extract the version ID from.
 //            Leave blank to get version ID from current executable
function SDUGetVersionInfoString(filename: String): String;
var
  majorVersion:    Integer;
  minorVersion:    Integer;
  revisionVersion: Integer;
  buildVersion:    Integer;
begin
  Result := '';
  if SDUGetVersionInfo(filename, majorVersion, minorVersion, revisionVersion, buildVersion) then
  begin
    Result := SDUVersionInfoToString(majorVersion, minorVersion, revisionVersion,
      buildVersion, -1);
  end;

end;

function SDUVersionInfoToString(majorVersion: Integer; minorVersion: Integer;
  betaVersion: Integer = -1): String;
begin
  Result := Format('%d.%d', [majorVersion, minorVersion]);

  if (betaVersion > 0) then
    Result := Result + ' ' + RS_BETA + ' ' + IntToStr(betaVersion);
end;

function SDUVersionInfoToString(majorVersion: Integer; minorVersion: Integer;
  revisionVersion: Integer; buildVersion: Integer; betaVersion: Integer = -1): String;
begin
  // versions are normally shown as eg 6.1 so format similarly or can confuse ie not 6.01
  Result := Format('%d.%d.%.2d.%.4d', [majorVersion, minorVersion, revisionVersion,
    buildVersion]);

  if (betaVersion > 0) then
    Result := Result + ' ' + RS_BETA + ' ' + IntToStr(betaVersion);
end;

 //procedure SDUPause(delayLen: Integer);
 //var
 //  delay: TTimeStamp;
 //begin
 //  delay      := DateTimeToTimeStamp(now);
 //  delay.Time := delay.Time + delayLen;
 //  while (delay.time > DateTimeToTimeStamp(now).time) do begin
 //    // Nothing - just pause
 //  end;
 //
 //end;


 // !! WARNING !! cmdLine must contain no whitespaces, otherwise the first
 //               parameter in the CreateProcess call must be set
 // xxx - sort out that warning will sort this out later
 // This function ripped from UDDF
 // [IN] cmdLine - the command line to execute
 // [IN] cmsShow - see the description of the nCmdShow parameter of the
 //                ShowWindow function
 // [IN] workDir - the working dir of the cmdLine (default is ""; no working dir)
 // Returns: The return value of the command, or $FFFFFFFF on failure
function SDUWinExecAndWait32(const cmdLine: String; cmdShow: Integer;
  workDir: String = ''; appName: String = ''): Cardinal;
var
  zAppName:    array[0..512] of Char;
  //  zCurDir:array[0..255] of char;
  //  WorkDir:String;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  pWrkDir:     PWideChar;
  pAppName:    PWideChar;
begin
  Result := $FFFFFFFF;

  StrPCopy(zAppName, cmdLine);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);

  StartupInfo.dwFlags     := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := cmdShow;

  pWrkDir := nil;
  if workDir <> '' then
    pWrkDir := PChar(workDir);
  pAppName := nil;
  if appName <> '' then
    pAppName := PWidechar(appName);

  if CreateProcess(pAppName, zAppName,
    { pointer to command line string }
    nil,                   { pointer to process security attributes }
    nil,                   { pointer to thread security attributes }
    False,                 { handle inheritance flag }
    CREATE_NEW_CONSOLE or  { creation flags }
    NORMAL_PRIORITY_CLASS, nil,
    { pointer to new environment block }
    pWrkDir,               { pointer to current directory name }
    StartupInfo,           { pointer to STARTUPINFO }
    ProcessInfo) then      { pointer to PROCESS_INF } begin
    WaitforSingleObject(ProcessInfo.hProcess, 120000); // wait 2 minutes
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;

{from DSiWin32 utilities - public domain, changes (c) tdk
original hdr:
Executes console process in a hidden window and captures its output in a TStrings
    object.
    Totaly reworked on 2006-01-23. New code contributed by matej.
    Handles only up to 1 MB of console process output.
    @returns ID of the console process or 0 if process couldn't be started.
    @author  aoven, Lee_Nover, gabr, matej
    @since   2003-05-24

    changes made t oconform to codign stds and to be consistent with  SDUWinExecAndWait32

  [IN] cmdLine - the command line to execute
  [IN] workDir - the working dir of the cmdLine (default is ""; no working dir)
  Returns: The return value of the command, or $FFFFFFFF on failure,
 output 'res' contains cmd output
 }
function SDUWinExecAndWaitOutput(const cmdLine: String; res: TStrings;
  const workDir: String): Cardinal;
const
  BUF_SIZE = $1000;  // allow 16K for results
var
  Security:             TSecurityAttributes;
  ReadPipe:             THandle;
  WritePipe:            THandle;
  StartupInfo:          TStartUpInfo;
  ProcessInfo:          TProcessInformation;
  Buffer:               PAnsiChar;
  TotalBytesRead:       DWORD;
  BytesRead:            DWORD;
  AppRunning:           Integer;
  n:                    Integer;
  BytesLeftThisMessage: Integer;
  TotalBytesAvail:      Integer;
  zAppName:             array[0..512] of Char;
  pWrkDir:              PWideChar;
begin
  Result                  := $FFFFFFFF;
  Security.nLength        := SizeOf(TSecurityAttributes);
  Security.bInheritHandle := True;
  Security.lpSecurityDescriptor := nil;
  if CreatePipe(ReadPipe, WritePipe, @Security, 0) then begin
    Buffer := AllocMem(BUF_SIZE + 1);
    FillChar(StartupInfo, Sizeof(StartupInfo), #0);
    StartupInfo.cb          := SizeOf(StartupInfo);
    StartupInfo.hStdOutput  := WritePipe;
    StartupInfo.hStdInput   := ReadPipe;
    StartupInfo.dwFlags     := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_HIDE;
    pWrkDir                 := nil;
    if workDir <> '' then
      pWrkDir := PChar(workDir);

    StrPCopy(zAppName, cmdLine);

    if CreateProcess(nil, zAppName, @Security, @Security, True, CREATE_NO_WINDOW or
      NORMAL_PRIORITY_CLASS, nil, pWrkDir, StartupInfo, ProcessInfo) then begin
      n              := 0;
      TotalBytesRead := 0;
      res.Clear;
      repeat
        // Increase counter to prevent an endless loop if the process is dead
        Inc(n, 1);
        AppRunning := WaitForSingleObject(ProcessInfo.hProcess, 100);
        if not PeekNamedPipe(ReadPipe, @Buffer[TotalBytesRead], BUF_SIZE,
          @BytesRead, @TotalBytesAvail, @BytesLeftThisMessage) then
          break //repeat
        else
        if BytesRead > 0 then
          ReadFile(ReadPipe, Buffer[TotalBytesRead], BytesRead, BytesRead, nil);
        TotalBytesRead := TotalBytesRead + BytesRead;
      until (AppRunning <> WAIT_TIMEOUT) or (n > 150);
      Buffer[TotalBytesRead] := #0;
      OemToCharA(Buffer, Buffer);
      {$IFDEF Unicode}
      res.Text := UnicodeString(StrPas(Buffer));
      {$ELSE}
      res.Text := StrPas(Buffer);
      {$ENDIF Unicode}
    end;
    FreeMem(Buffer);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ReadPipe);
    CloseHandle(WritePipe);
  end;
end; // SDUWinExecAndWaitOutput

function SDUWinExecNoWait32(cmdLine, params: String; cmdShow: Integer): Boolean;
begin
  Result := ShellExecute(Application.Handle, 'open', PWideChar(cmdLine),
    PWideChar(params), nil, cmdShow) > 32;
end;


function SDUGetControlWithTag(tag: Integer; parentControl: TControl): TControl;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to (TWinControl(parentControl).ControlCount - 1) do begin
    if TWinControl(parentControl).Controls[i].tag = tag then begin
      Result := TWinControl(parentControl).Controls[i];
      break;
    end;
  end;

end;

 // Search out and return the first label/statictext found which has
 // ".FocusControl" set to the control passed in
function SDUGetControlWithFocusControl(ctrl: TControl): TWinControl;
var
  i:             Integer;
  parentControl: TWinControl;
begin
  Result := nil;

  parentControl := ctrl.Parent;
  for i := 0 to (parentControl.ControlCount - 1) do begin
    if (parentControl.Controls[i] is TLabel) then begin
      if (TLabel(parentControl.Controls[i]).FocusControl = ctrl) then begin
        Result := TWinControl(parentControl.Controls[i]);
        break;
      end;
    end else
    if (parentControl.Controls[i] is TStaticText) then begin
      if (TStaticText(parentControl.Controls[i]).FocusControl = ctrl) then begin
        Result := TWinControl(parentControl.Controls[i]);
        break;
      end;
    end;
  end;

end;


procedure SDUShowFileProperties(const filename: String);
var
  sei: TShellExecuteinfo;
begin
  FillChar(sei, sizeof(sei), 0);
  sei.cbSize := sizeof(sei);
  sei.lpFile := PChar(filename);
  sei.lpVerb := 'properties';
  sei.fMask  := SEE_MASK_INVOKEIDLIST;
  ShellExecuteEx(@sei);
end;

function SDUVolumeID(DriveChar: DriveLetterChar): String;
var
  OldErrorMode:      Integer;
  NotUsed, VolFlags: DWORD;
  Buf:               array [0..MAX_PATH] of Char;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Buf[0] := #$00;
    if GetVolumeInformation(PChar(DriveChar + ':\'), Buf, DWORD(sizeof(Buf)),
      nil, NotUsed, VolFlags, nil, 0) then
      SetString(Result, Buf, StrLen(Buf))
    else
      Result := '';
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

function SDUNetworkVolume(DriveChar: DriveLetterChar): String;
var
  Buf:        array [0..MAX_PATH] of Widechar;
  DriveStr:   array [0..3] of Widechar;
  BufferSize: DWORD;
begin
  BufferSize  := sizeof(Buf);
  DriveStr[0] := Widechar(UpCase(DriveChar));
  DriveStr[1] := ':';
  DriveStr[2] := #0;
  if WNetGetConnection(DriveStr, Buf, BufferSize) = WN_SUCCESS then begin
    SetString(Result, Buf, BufferSize);
    if pos(#0, Result) > 0 then begin
      Delete(Result, pos(#0, Result), length(Result) - pos(#0, Result) + 1);
    end;
    if DriveChar < 'a' then
      Result := AnsiUpperCase(Result)
    else
      Result := AnsiLowerCase(Result);
  end else
    Result := SDUVolumeID(DriveChar);
end;

{$IFDEF MSWINDOWS}
function SDUGetVolumeID(drive: DriveLetterChar): String;
var
  DriveType: TDriveType;
begin
  Result := '';

  drive     := upcase(drive);
  DriveType := TDriveType(GetDriveType(PChar(drive + ':\')));

  case DriveType of
    dtFloppy:
    begin
      if (drive = 'A') or (drive = 'B') then begin
        Result := '3.5" Floppy';
      end else begin
        Result := 'Removable Disk';
      end;
    end;
    dtFixed:
    begin
      Result := SDUVolumeID(drive);
    end;
    dtNetwork:
    begin
      Result := SDUNetworkVolume(drive);
    end;
    dtCDROM:
    begin
      Result := SDUVolumeID(drive);
    end;
    dtRAM:
    begin
      Result := SDUVolumeID(drive);
    end;
  end;

end;

{$ENDIF}

 // Populate a TStringsList with environment variables
 // [OUT] envStrings - a TStringsList to be populated with the names of all environment variables
 // Returns TRUE/FALSE on success/failure
function SDUGetEnvironmentStrings(envStrings: TStringList): Boolean;
var
  pEnvPtr, pSavePtr: PChar;
begin
  pEnvPtr  := GetEnvironmentStrings;
  pSavePtr := pEnvPtr;
  repeat
    envStrings.add(Copy(StrPas(pEnvPtr), 1, Pos('=', StrPas(pEnvPtr)) - 1));
    Inc(pEnvPtr, StrLen(pEnvPtr) + 1);
  until pEnvPtr^ = #0;

  FreeEnvironmentStrings(pSavePtr);

  Result := True;
end;


 // Get the value of the specified environment variable
 // [IN] envVar - the name of the environment variable
 // [OUT] value - set to the value of the environment variable on success
 // Returns TRUE/FALSE on success/failure
function SDUGetEnvironmentVar(envVar: String; var Value: String): Boolean;
var
  buffer:   String;
  buffSize: Integer;
  i:        Integer;
begin
  Result := False;

  SetString(buffer, nil, 1);
  buffSize := GetEnvironmentVariable(PChar(envVar), PChar(buffer), 0);
  if buffSize <> 0 then begin
    SetString(buffer, nil, buffSize);
    GetEnvironmentVariable(PChar(envVar), PChar(buffer), buffSize);
    Value := '';
    for i := 1 to buffSize - 1 do begin
      Value := Value + buffer[i];
    end;

    Result := True;
  end;

end;


 // Set the specified time/datestamps on the file referred to by Handle
 // Identical to "SetFileTime", but updates all 3 timestamps (created, last
 // modified and last accessed)
function SDUSetAllFileTimes(Handle: Integer; Age: Integer): Integer;
var
  LocalFileTime, FileTime: TFileTime;
begin
  Result := 0;
  if (DosDateTimeToFileTime(LongRec(Age).Hi, LongRec(Age).Lo, LocalFileTime) and
    LocalFileTimeToFileTime(LocalFileTime, FileTime) and SetFileTime(Handle,
    @FileTime, @FileTime, @FileTime)) then begin
    exit;
  end;
  Result := GetLastError;
end;


function SDUDateTimeToFileTime(dateTime: TDateTime): TFileTime;
var
  sysTime: TSystemTime;
begin
  try
    DateTimeToSystemTime(dateTime, sysTime);
    SystemTimeToFileTime(sysTime, Result);
    LocalFileTimeToFileTime(Result, Result);
  except
    Result.dwHighDateTime := 0;
    Result.dwLowDateTime  := 0;
  end;

end;

function SDUFileTimeToDateTime(fileTime: TFileTime): TDateTime;
var
  sysTime: TSystemTime;
begin
  try
    FileTimeToLocalFileTime(fileTime, fileTime);
    FileTimeToSystemTime(fileTime, sysTime);
    Result := SystemTimeToDateTime(sysTime);
  except
    Result := 0;
  end;

end;

function SDUFileTimeToTimeStamp(fileTime: TFileTime): TTimeStamp;
var
  dt: TDateTime;
begin
  dt     := SDUFileTimeToDateTime(fileTime);
  Result := DateTimeToTimeStamp(dt);

end;

 // Counts the number of instances of theChar in theString, and returns this
 // count
function SDUCountCharInstances(theChar: Char; theString: String): Integer;
var
  i:     Integer;
  Count: Integer;
begin
  Count := 0;
  for i := 1 to length(theString) do begin
    if theString[i] = theChar then begin
      Inc(Count);
    end;
  end;

  Result := Count;
end;



 // This is used by SDUDetectExistingApp and carries out a check on the window
 // supplied to see if it matches the current one
function _SDUDetectExistingAppCheckWindow(Handle: HWND; Temp: Longint): BOOL; stdcall;
var
  WindowName: array[0..255] of Char;
  ClassName:  array[0..255] of Char;
begin
  // Go get the windows class name
  // Is the window class the same?
  if (GetClassName(Handle, ClassName, sizeof(ClassName)) > 0) then begin
    if (StrComp(ClassName, _SDUDetectExist_ClassName) = 0) then begin
      // Get its window caption
      // Does this have the same window title?
      if (GetWindowText(Handle, WindowName, sizeof(WindowName)) > 0) then begin
        if (StrComp(WindowName, _SDUDetectExist_AppName) = 0) then begin
          Inc(_SDUDetectExist_CntFound);
          // Are the handles different?
          if (Handle <> _SDUDetectExistWindowDetails_ThisClassHandle()) then begin
            // Save it so we can bring it to the top later.
            _SDUDetectExist_LastFound := Handle;
          end;
        end;
      end;
    end;
  end;

  Result := True;
end;


 // Detect if the current application is already running, and if it is, return
 // a handle to it's main window.
 // Returns 0 if the application is not currently running
function SDUDetectExistingApp(): THandle;
begin
  // Determine what this application's name and class name is...
  _SDUDetectExistWindowDetails();

  // ...and count how many others out there are Delphi apps with this title
  _SDUDetectExist_CntFound  := 0;
  _SDUDetectExist_LastFound := 0;
  EnumWindows(@_SDUDetectExistingAppCheckWindow, 0);

  Result := _SDUDetectExist_LastFound;
end;


 // This is used by SDUSendMessageExistingApp and carries out a check on the
 // window supplied to see if it matches the current one
function _SDUSendMessageExistingAppCheckWindow(Handle: HWND; Temp: Longint): BOOL; stdcall;
var
  WindowName: array[0..255] of Char;
  ClassName:  array[0..255] of Char;
begin
  // Go get the windows class name
  // Is the window class the same?
  if (GetClassName(Handle, ClassName, sizeof(ClassName)) > 0) then begin
    if (StrComp(ClassName, _SDUDetectExist_ClassName) = 0) then begin
      // Get its window caption
      // Does this have the same window title?
      if (GetWindowText(Handle, WindowName, sizeof(WindowName)) > 0) then begin
        if StrComp(WindowName, _SDUDetectExist_AppName) = 0 then begin
          SendMessage(
            Handle,
            _SDUSendMsg_msg,
            _SDUSendMsg_wParam,
            _SDUSendMsg_lParam
            );
        end;
      end;
    end;
  end;

  Result := True;
end;


{$IF CompilerVersion < 18.5}
 // Detect if the current application is already running, and if it is, return
 // a handle to it's main window.
 // Returns 0 if the application is not currently running
procedure SDUSendMessageExistingApp(msg: Cardinal; wParam: Integer; lParam: Integer);
begin
  //    Assert(
  //           not(Application.MainFormOnTaskbar),
  //           'SDUSendMessageExistingApp doesn''t work under Delphi 2007 (and probably later version of Delphi) if Application.MainFormOnTaskbar is TRUE'
  //          );

  _SDUDetectExistWindowDetails();

  _SDUSendMsg_msg    := msg;
  _SDUSendMsg_wParam := wParam;
  _SDUSendMsg_lParam := lParam;

  EnumWindows(@_SDUSendMessageExistingAppCheckWindow, 0);

end;

{$IFEND}


 // This is used by SDUPostMessageExistingApp and carries out a check on the
 // window supplied to see if it matches the current one
function _SDUPostMessageExistingAppCheckWindow(Handle: HWND; Temp: Longint): BOOL; stdcall;
var
  WindowName: array[0..255] of Char;
  ClassName:  array[0..255] of Char;
begin
  // Go get the windows class name
  // Is the window class the same?
  if (GetClassName(Handle, ClassName, sizeof(ClassName)) > 0) then begin
    if (StrComp(ClassName, _SDUDetectExist_ClassName) = 0) then begin
      // Get its window caption
      // Does this have the same window title?
      if (GetWindowText(Handle, WindowName, sizeof(WindowName)) > 0) then begin
        if StrComp(WindowName, _SDUDetectExist_AppName) = 0 then begin
          PostMessage(
            Handle,
            _SDUSendMsg_msg,
            _SDUSendMsg_wParam,
            _SDUSendMsg_lParam
            );
        end;
      end;
    end;
  end;

  Result := True;
end;


{$IF CompilerVersion > 18.5}
 // Detect if the current application is already running, and if it is, return
 // a handle to it's main window.
 // Returns 0 if the application is not currently running
procedure SDUPostMessageExistingApp(msg: Cardinal; wParam: Integer; lParam: Integer);
begin
  //    Assert(
  //           not(Application.MainFormOnTaskbar),
  //           'SDUPostMessageExistingApp doesn''t work under Delphi 2007 (and probably later version of Delphi) if Application.MainFormOnTaskbar is TRUE'
  //          );

  _SDUDetectExistWindowDetails();

  _SDUSendMsg_msg    := msg;
  _SDUSendMsg_wParam := wParam;
  _SDUSendMsg_lParam := lParam;

  EnumWindows(@_SDUPostMessageExistingAppCheckWindow, 0);

end;

{$IFEND}

procedure _SDUDetectExistWindowDetails();
var
  classHandle: THandle;
begin
  classHandle := _SDUDetectExistWindowDetails_ThisClassHandle();

  // Determine what this application's name is...
  GetWindowText(
    classHandle,
    _SDUDetectExist_AppName,
    sizeof(_SDUDetectExist_AppName)
    );

  // ...then determine the class name for this application...
  GetClassName(
    classHandle,
    _SDUDetectExist_ClassName,
    sizeof(_SDUDetectExist_ClassName)
    );
end;

function _SDUDetectExistWindowDetails_ThisClassHandle(): THandle;
begin
  Result := Application.Handle;

{$IF CompilerVersion >= 18.5}
  // Vista fix for Delphi 2007 and later
  if ((Application.Mainform <> nil) and Application.MainFormOnTaskbar) then begin
    Result := Application.Mainform.Handle;
  end;
{$IFEND}

end;


// Split the string supplied into two parts, before and after the split char
function SDUSplitString(wholeString: String; var firstItem: String;
  var theRest: String; splitOn: Char = ' '): Boolean;
begin
  firstItem := wholeString;
  if pos(splitOn, wholeString) > 0 then begin
    firstItem := copy(wholeString, 1, (pos(splitOn, wholeString) - 1));
    theRest   := copy(wholeString, length(firstItem) + length(splitOn) + 1,
      (length(wholeString) - (length(firstItem) + length(splitOn))));
    Result    := True;
  end else begin
    theRest := '';
    Result  := (firstItem <> '');
  end;

end;

function SDUSplitWideString(wholeString: WideString; var firstItem: WideString;
  var theRest: WideString; splitOn: Widechar = ' '): Boolean;
begin
  firstItem := wholeString;
  if Pos(splitOn, wholeString) > 0 then begin
    firstItem := copy(wholeString, 1, (pos(splitOn, wholeString) - 1));
    theRest   := copy(wholeString, length(firstItem) + length(splitOn) + 1,
      (length(wholeString) - (length(firstItem) + length(splitOn))));
    Result    := True;
  end else begin
    theRest := '';
    Result  := (firstItem <> '');
  end;

end;


// Convert a hex number into an integer
function SDUHexToInt(hex: String): Integer;
begin
  Result := StrToInt('$' + hex);

end;

function SDUTryHexToInt(hex: String; var Value: Integer): Boolean;
begin
  // Strip out any spaces
  hex := StringReplace(hex, ' ', '', [rfReplaceAll]);

  // If prefixed with "0x", strip this out as well
  if (Pos('0X', uppercase(hex)) = 0) then begin
    Delete(hex, 1, 2);
  end;

  Result := TryStrToInt('$' + hex, Value);
end;

// Save the given font's details to the registry
function SDUSaveFontToReg(rootKey: HKEY; fontKey: String; Name: String; font: TFont): Boolean;
var
  LogFont:  TLogFont;
  registry: TRegistry;
begin
  GetObject(Font.Handle, SizeOf(TLogFont), @LogFont);
  registry := TRegistry.Create;
  try
    registry.LazyWrite := False;
    registry.RootKey   := rootKey;
    registry.OpenKey(FontKey, True);
    registry.WriteBinaryData(Name + 'LogFont', LogFont, SizeOf(LogFont));
    registry.WriteInteger(Name + 'FontColor', Font.Color);
    Result := True;
  finally
    registry.Free;
  end;
end;


// Load the font's details back from the registry
function SDULoadFontFromReg(rootKey: HKEY; fontKey: String; Name: String; font: TFont): Boolean;
var
  LogFont:  TLogFont;
  registry: TRegistry;
begin
  Result := False;

  registry := TRegistry.Create;
  try
    registry.RootKey := rootKey;
    if registry.OpenKey(fontKey, False) then begin
      if registry.ReadBinaryData(Name + 'LogFont', LogFont, SizeOf(LogFont)) > 0 then begin
        Font.Color  := registry.ReadInteger(Name + 'FontColor');
        Font.Handle := CreateFontIndirect(LogFont);
        Result      := True;
      end;
    end;
  finally
    registry.Free();
  end;
end;

 // Get a string with the drive letters of all drives present/not present, in
 // order
 // Return value is all in uppercase
function SDUGetNetworkDriveLetters(): String;
var
  i:          DWORD;
  dwResult:   DWORD;
  hEnum:      THANDLE;
  lpnrDrv:    PNETRESOURCE;
  lpnrDrvLoc: PNETRESOURCE;
  cEntries:   DWORD;
  cbBuffer:   DWORD;
  localName:  String;
begin
  Result := '';

  cEntries := $FFFFFFFF;
  cbBuffer := 16384;

  dwResult := WNetOpenEnum(RESOURCE_REMEMBERED, RESOURCETYPE_DISK, 0, nil, hEnum);

  if (dwResult = NO_ERROR) then begin
    repeat
      lpnrDrv  := PNETRESOURCE(GlobalAlloc(GPTR, cbBuffer));
      dwResult := WNetEnumResource(hEnum, cEntries, lpnrDrv, cbBuffer);
      if (dwResult = NO_ERROR) then begin
        lpnrDrvLoc := lpnrDrv;
        for i := 0 to (cEntries - 1) do begin
          if lpnrDrvLoc^.lpLocalName <> nil then begin
            localName := lpnrDrvLoc^.lpLocalName;
            if (length(localName) > 0) then begin
              Result := Result + lpnrDrvLoc^.lpLocalName[0];
            end;
          end;
          Inc(lpnrDrvLoc);
        end;
      end else begin
        if dwResult <> ERROR_NO_MORE_ITEMS then begin
          // Can't get drive enum
          GlobalFree(HGLOBAL(lpnrDrv));
          break;
        end;
      end;

      GlobalFree(HGLOBAL(lpnrDrv));
    until (dwResult = ERROR_NO_MORE_ITEMS);

    WNetCloseEnum(hEnum);
  end;

  { TODO 1 -otdk -cclean : warning wide->ansi }
end;


 // Get a string with the drive letters of all drives present/not present, in
 // order
 // Return value is all in uppercase
function SDUGetUsedDriveLetters(): String;
var
  DriveNum:   Integer;
  DriveBits:  set of 0..25;
  drivesList: TStringList;
  netDrives:  String;
  i:          Integer;
begin
  Result := '';

  drivesList := TStringList.Create();
  try
    drivesList.Duplicates := dupIgnore;

    // Get local drive letters (Windows XP SP1 and later don't return network
    // drives in GetLogicalDrives(...))
    Integer(DriveBits) := GetLogicalDrives();
    for DriveNum := 0 to 25 do begin
      if (DriveNum in DriveBits) then begin
        drivesList.Add(AnsiChar(DriveNum + Ord('A')));
      end;
    end;

    // Network drive letters...
    netDrives := SDUGetNetworkDriveLetters();
    // (String; index from 1)
    for i := 1 to length(netDrives) do begin
      drivesList.Add(netDrives[i]);
    end;

    drivesList.Sort();
    for i := 0 to (drivesList.Count - 1) do begin
      Result := Result + drivesList[i]; //no data loss here - just that stringlist uses widestrings
    end;
  finally
    drivesList.Free();
  end;

end;


function SDUGetUnusedDriveLetters(): String;
var
  x:                Char;
  usedDriveLetters: String;
begin
  Result := '';

  usedDriveLetters := SDUGetUsedDriveLetters();
  for x := 'A' to 'Z' do begin
    if (pos(x, usedDriveLetters) = 0) then begin
      Result := Result + x;
    end;
  end;

end;


 // Register the specified filename extension to launch the command given
 // command - This should be quoted as appropriate
function SDUFileExtnRegCmd(fileExtn: String; menuItem: String; command: String): Boolean;
var
  registry: TRegistry;
begin
  Result := True;

  if (Pos('.', fileExtn) <> 1) then begin
    fileExtn := '.' + fileExtn;
  end;

  registry := TRegistry.Create();
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    registry.Access  := KEY_WRITE;

    if registry.OpenKey('\' + fileExtn + '\shell\' + menuItem + '\command', True) then begin
      registry.WriteString('', command);
      registry.CloseKey();
      Result := True;
    end;

    // Nuke any filetype
    if registry.OpenKey('\' + fileExtn, True) then begin
      registry.DeleteValue('');
      registry.CloseKey();
      Result := True;
    end;

  finally
    registry.Free();
  end;

  // Inform Windows that an association has changed...
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

end;


function SDUFileExtnUnregCmd(fileExtn: String; menuItem: String): Boolean;
var
  registry:   TRegistry;
  okToDelete: Boolean;
  info:       TRegKeyInfo;
begin
  if (Pos('.', fileExtn) <> 1) then begin
    fileExtn := '.' + fileExtn;
  end;

  // This is a little *ugly*, but it does the job...
  // In a nutshell, this will remove the maximum amount of keys associated with
  // the file extension as it can without causing any damage 
  registry := TRegistry.Create();
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    registry.Access  := KEY_WRITE;

    Result := registry.DeleteKey('\' + fileExtn + '\shell\' + menuItem + '\command');
    if Result then begin
      Result := registry.DeleteKey('\' + fileExtn + '\shell\' + menuItem);
      if Result then begin
        // Check to see if subkeys for other applications exist under the
        // "shell" key...
        Result := registry.OpenKey('\' + fileExtn + '\shell', False);
        if Result then begin
          okToDelete      := False;
          registry.Access := KEY_READ;
          Result          := registry.GetKeyInfo(info);
          registry.Access := KEY_WRITE;
          if Result then begin
            okToDelete := (info.NumSubKeys = 0) and (info.NumValues = 0);
          end;
          registry.CloseKey();

          // If not, delete the "shell" subkey
          if okToDelete then begin
            Result := registry.DeleteKey('\' + fileExtn + '\shell');
            if Result then begin
              // Check to see if subkeys for other applications exist under the
              // menuitem key...
              Result := registry.OpenKey('\' + fileExtn, False);
              if Result then begin
                okToDelete      := False;
                registry.Access := KEY_READ;
                Result          := registry.GetKeyInfo(info);
                registry.Access := KEY_WRITE;
                if Result then begin
                  okToDelete := (info.NumSubKeys = 0) and (info.NumValues = 0);
                end;
                registry.CloseKey();

                // If not, delete the extension subkey
                if okToDelete then begin
                  Result := registry.DeleteKey('\' + fileExtn);
                end;
              end;
            end;
          end;
        end;
      end;
    end;


  finally
    registry.Free();
  end;

  // Inform Windows that an association has changed...
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

end;


function SDUFileExtnGetRegCmd(fileExtn: String; menuItem: String): String;
var
  registry: TRegistry;
begin
  Result := '';

  if (Pos('.', fileExtn) <> 1) then begin
    fileExtn := '.' + fileExtn;
  end;

  registry := TRegistry.Create();
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    registry.Access  := KEY_READ;

    if registry.OpenKey('\' + fileExtn + '\shell\' + menuItem + '\command', False) then begin
      Result := registry.ReadString('');
      registry.CloseKey();
    end;

  finally
    registry.Free();
  end;

end;

function SDUFileExtnIsRegCmd(fileExtn: String; menuItem: String; executable: String): Boolean;
var
  command: String;
begin
  command := SDUFileExtnGetRegCmd(fileExtn, menuItem);
  Result  := (Pos(uppercase(executable), uppercase(command)) > 0);

end;


// Register the specified filename extension to use the specified icon
function SDUFileExtnRegIcon(fileExtn: String; filename: String; iconNum: Integer): Boolean;
var
  registry: TRegistry;
begin
  Result := True;

  if (Pos('.', fileExtn) <> 1) then begin
    fileExtn := '.' + fileExtn;
  end;

  registry := TRegistry.Create();
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    registry.Access  := KEY_WRITE;

    if registry.OpenKey('\' + fileExtn + '\DefaultIcon', True) then begin
      registry.WriteString('', '"' + filename + '",' + IntToStr(iconNum));
      registry.CloseKey();
      Result := True;
    end;

  finally
    registry.Free();
  end;

  // Inform Windows that an association has changed...
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

end;


function SDUFileExtnUnregIcon(fileExtn: String): Boolean;
var
  registry:   TRegistry;
  okToDelete: Boolean;
  info:       TRegKeyInfo;
begin
  if (Pos('.', fileExtn) <> 1) then begin
    fileExtn := '.' + fileExtn;
  end;

  // This is a little *ugly*, but it does the job...
  // In a nutshell, this will remove the maximum amount of keys associated with
  // the file extension as it can without causing any damage
  registry := TRegistry.Create();
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    registry.Access  := KEY_WRITE;

    Result := registry.OpenKey('\' + fileExtn + '\DefaultIcon', False);
    if Result then begin
      okToDelete := False;
      Result     := registry.DeleteValue('');
      if (Result) then begin
        Result := registry.GetKeyInfo(info);
        if Result then begin
          okToDelete := (info.NumSubKeys = 0) and (info.NumValues = 0);
        end;
      end;

      registry.CloseKey();

      // If no further items under "DefaultIcon" subkey, delete it
      if okToDelete then begin
        Result := registry.DeleteKey('\' + fileExtn + '\DefaultIcon');
        if Result then begin
          // Check to see if subkeys for other applications exist under the
          // extension key...
          Result := registry.OpenKey('\' + fileExtn, False);
          if Result then begin
            okToDelete := False;
            Result     := registry.GetKeyInfo(info);
            if Result then begin
              okToDelete := (info.NumSubKeys = 0) and (info.NumValues = 0);
            end;
            registry.CloseKey();

            // If not, delete the extension subkey
            if okToDelete then begin
              Result := registry.DeleteKey('\' + fileExtn);
            end;
          end;
        end;
      end;
    end;


  finally
    registry.Free();
  end;

  // Inform Windows that an association has changed...
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);

end;


// Return TRUE/FALSE, depending on if the file extension is registered or not
function SDUFileExtnIsRegd(fileExtn: String; menuItem: String): Boolean;
var
  registry: TRegistry;
begin
  if (Pos('.', fileExtn) <> 1) then begin
    fileExtn := '.' + fileExtn;
  end;

  registry := TRegistry.Create();
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    registry.Access  := KEY_READ;

    Result := registry.KeyExists('\' + fileExtn + '\shell\' + menuItem + '\command');

  finally
    registry.Free();
  end;

end;


// Returns installed OS
function SDUInstalledOS(): TInstalledOS;
const
  SM_SERVERR2 = 98;
var
  osVersionInfoEx: SDUWindows.OSVERSIONINFOEX;
begin
  if (Win32Platform <> VER_PLATFORM_WIN32_NT) then begin
    // Windows 95/98/Me
    if (Win32MinorVersion = 0) then begin
      Result := osWindows95;
    end else
    if (Win32MinorVersion = 10) then begin
      Result := osWindows98;
    end else begin
      Result := osWindowsMe;
    end;
  end else begin
    // Windows NT based and later

    // Version numbers from:
    //   http://msdn2.microsoft.com/en-us/library/ms724834(VS.85).aspx

    // Operating system         Version number
    // -------------------------------------------
    // Windows 7                6.1
    // Windows Server 2008 R2   6.1
    // Windows Server 2008      6.0
    // Windows Vista            6.0
    // Windows Server 2003 R2   5.2
    // Windows Server 2003      5.2
    // Windows XP               5.1
    // Windows 2000             5.0

    // Fallback...
    Result := osWindowsNT;

    if (Win32MajorVersion < 5) then begin
      Result := osWindowsNT;
    end else
    if (Win32MajorVersion >= 5) then begin
      osVersionInfoEx.dwOSVersionInfoSize := sizeof(osVersionInfoEx);
      SDUGetVersionEx(osVersionInfoEx);

      if (Win32MajorVersion = 5) then begin
        // Fallback...
        Result := osWindows2000;

        if (Win32MinorVersion = 0) then begin
          Result := osWindows2000;
        end else
        if (Win32MinorVersion = 1) then begin
          Result := osWindowsXP; // On Windows XP 64 bit, Win32MinorVersion is 2 ?!
        end else
        if (Win32MinorVersion = 2) then begin
          Result := osWindowsServer2003; // On Windows XP 64 bit, Win32MinorVersion is 2 ?!

          if (GetSystemMetrics(SM_SERVERR2) <> 1) then begin
            Result := osWindowsServer2003R2;
          end;
        end;
      end else
      if (Win32MajorVersion = 6) then begin
        // Fallback
        Result := osWindowsVista;

        if (Win32MinorVersion = 0) then begin
          Result := osWindowsVista;

          if (OSVERSIONINFOEX.wProductType <> VER_NT_WORKSTATION) then begin
            Result := osWindowsServer2008;
          end;
        end else
        if (Win32MinorVersion = 1) then begin
          Result := osWindows7;

          if (OSVERSIONINFOEX.wProductType <> VER_NT_WORKSTATION) then begin
            Result := osWindowsServer2008R2;
          end;
        end;

      end else // if (Win32MajorVersion >= 6) then
      begin
        // Fallback
        Result := osWindows7;
      end;
    end;

  end;

end;


// Returns TRUE if installed OS is Windows Vista or later
function SDUOSVistaOrLater(): Boolean;
begin
  Result := (SDUInstalledOS >= osWindowsVista);
end;

 // Returns 32/64, depending on version of *OS* running (e.g. Windows XP x64
 // returns 64)
function SDUOSCPUSize(): Integer;
const
  PROC_ARCH_32_BIT = 'x86';
  //var
  //  procArch: string;
  //  isWow64: Boolean;
begin
  Result := 32;

{
  if (Result = 32) then
    begin
    procArch := trim(GetEnvironmentVariable(EVN_VAR_PROC_ARCHITECTURE));
    // If it's set to something, but not x86, assume it's a 64 bit system
    if (
        (procArch <> '') and
        (uppercase(procArch) <> uppercase(PROC_ARCH_32_BIT))
       ) then
      begin
      Result := 64;
      end;
    end;

  if (Result = 32) then
    begin
    procArch := trim(GetEnvironmentVariable(EVN_VAR_PROC_ARCH_W3264));
    // If it's set to something, but not x86, assume it's a 64 bit system
    if (
        (procArch <> '') and
        (uppercase(procArch) <> uppercase(PROC_ARCH_32_BIT))
       ) then
      begin
      Result := 64;
      end;
    end;
}

  if (Result = 32) then begin
    if SDUIsWow64Process(GetCurrentProcess()) then begin
      Result := 64;
    end;
  end;

end;

// Returns TRUE if running on 64 bit OS (e.g. Windows XP x64)
function SDUOS64bit(): Boolean;
begin
  Result := (SDUOSCPUSize() = 64);
end;

//is the *app* 64 bit?
function SDUApp64bit(): Boolean;
begin
   {$ifdef WIN32}
    result := false;
    {$ELSE}
  Result := True;
    {$ENDIF}
end;

 // Get size of file
 // This is just a slightly easier to use version of GetFileSize
 // Returns file size, or -1 on error
function SDUGetFileSize(const filename: String): ULONGLONG;
var
  fileHandle:        THandle;
  sizeLow, sizeHigh: DWORD;
begin
  Result := 0;

  // Open file and get it's size
  fileHandle := CreateFile(PChar(filename),
    // pointer to name of the file
    GENERIC_READ,             // access (read-write) mode
    (FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE),
    // share mode
    nil,                      // pointer to security attributes
    OPEN_EXISTING,            // how to create
    FILE_FLAG_RANDOM_ACCESS,  // file attributes
    0                         // handle to file with attributes to copy
    );

  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    sizeLow := GetFileSize(fileHandle, @sizeHigh);

    if not ((sizeLow = $FFFFFFFF) and (GetLastError() <> NO_ERROR)) then begin
      Result := ULONGLONG(sizeHigh) shl 32;
      Result := (Result or ULONGLONG(sizeLow));
    end;

    CloseHandle(fileHandle);
  end;
end;

 // On a TPageControl, determine if the one tabsheet appears *after* another
 // Returns TRUE if "aSheet" appears *after* bSheet
 // Returns FALSE if "aSheet" appears *before* bSheet
 // Returns FALSE if "aSheet" *is* "bSheet"
 // Returns TRUE if "aSheet" appears *after* bSheet
 // Returns FALSE if "aSheet" appears *before* bSheet
 // Returns FALSE if "aSheet" *is* "bSheet"
function SDUIsTabSheetAfter(pageCtl: TPageControl; aSheet, bSheet: TTabSheet): Boolean;
var
  i:       Integer;
  isAfter: Boolean;
  foundB:  Boolean;
begin
  isAfter := False;
  foundB  := False;

  for i := 0 to (pageCtl.PageCount - 1) do begin
    if pageCtl.Pages[i] = bSheet then begin
      foundB := True;
    end;

    if pageCtl.Pages[i] = aSheet then begin
      isAfter := not (foundB);
      break;
    end;

  end;

  Result := isAfter;
end;


procedure SDUParseDataToASCII(data: String; var ASCIIrep: String);
var
  i: Integer;
begin
  ASCIIrep := '';
  for i := 1 to length(data) do begin
    ASCIIrep := ASCIIrep + inttohex(Ord(data[i]), 2);
  end;

end;

 // Convert the ASCII representation of binary data into binary data
 // ASCIIrep - This must be set to a string containing the ASCII representation
 //            of binary data (e.g. "DEADBEEF010203" as bytes $DE, $AD, $BE,
 //            $EF, $01, $02, $03)
 // Note: ASCIIrep **MUST** have an **even** number of hex chars
 // Note: Whitespace in ASCIIrep is *ignored*
function SDUParseASCIIToData(ASCIIrep: String; var data: String): Boolean;
var
  tmpStr:        String;
  i:             Integer;
  ASCIIStripped: String;
begin
  Result := False;

  // Uppercase ASCIIrep...
  ASCIIrep := uppercase(ASCIIrep);

  // Strip whitespace from ASCIIrep...
  ASCIIStripped := '';
  for i := 1 to length(ASCIIrep) do begin
    if (
      // If it's a numeric char...
      ((Ord(ASCIIrep[i]) >= Ord('0')) and (Ord(ASCIIrep[i]) <= Ord('9'))) or
      // ...or "A" - "F"...
      ((Ord(ASCIIrep[i]) >= Ord('A')) and (Ord(ASCIIrep[i]) <= Ord('F')))) then begin
      ASCIIStripped := ASCIIStripped + ASCIIrep[i];
    end;

  end;

  // Sanity check; the input ASCII representation must have an even number of
  // chars; the length of the data must be a multiple of 8
  if ((Length(ASCIIStripped) mod 2) < 1) then begin
    data := '';

    while (Length(ASCIIStripped) > 0) do begin
      tmpStr := Copy(ASCIIStripped, 1, 2);
      Delete(ASCIIStripped, 1, 2);

      data := data + chr(SDUHexToInt(tmpStr));
    end;

    Result := True;
  end;

end;


 // ----------------------------------------------------------------------------
 // Create a file of the specified size, filled with zeros
 // Note: This will *fail* if the file already exists
 // Note: This function *could* have been implemented using SetFilePos and
 //       SetEndOfFile, which would have been a lot neater than allocating an in
 //       memory buffer, and just dumping it out to disk. However, that clobbers
 //       any progress bar, as it doens't get updated
 // Returns TRUE on success, FALSE on failure
 // If the file creation was cancelled by the user, this function will return
 // FALSE, but set userCancelled to TRUE
function SDUCreateLargeFile(filename: String; size: ULONGLONG; showProgress: Boolean;
  var userCancelled: Boolean): Boolean;
const
  BUFFER_SIZE: DWORD = (2 * 1024 * 1024); // 2MB buffer; size not *particularly*
  // important, but it will allocate this
  // amount of memory
  // Files are created by repeatedly
  // writing a buffer this size out to disk
var
  fileHandle:        THandle;
  buffer:            PChar;
  x:                 Int64;
  bytesWritten:      DWORD;
  progressDlg:       TSDUProgressDialog;
  fullChunksToWrite: Int64;
  prevCursor:        TCursor;
  driveLetter:       Char;
  diskNumber:        Integer;
  freeSpace:         Int64;
begin
  Result        := False;
  userCancelled := False;
  progressDlg   := nil;

  if (filename = '') then begin
    // No filename specified
    Result := False;
    exit;
  end else begin
    // Sanity check that there's enough storage on the drive (if a local drive)
    // 2 because we skip the drive letter
    if (Pos(':\', filename) = 2) then begin
      driveLetter := upcase(filename[1]);
      diskNumber  := Ord(driveLetter) - Ord('A') + 1;
      freeSpace   := DiskFree(diskNumber);
      if (ULONGLONG(freeSpace) < size) then begin
        // Insufficient free space - exit
        Result := False;
        exit;
      end;
    end;

  end;


  buffer := AllocMem(BUFFER_SIZE);
  try
    fileHandle := CreateFile(PChar(filename),
      // pointer to name of the file
      GENERIC_WRITE,         // access (read-write) mode
      FILE_SHARE_READ,       // share mode
      nil,                   // pointer to security attributes
      CREATE_NEW,            // how to create
      FILE_ATTRIBUTE_NORMAL, // file attributes
      0                      // handle to file with attributes to copy
      );
    if (fileHandle <> INVALID_HANDLE_VALUE) then begin
      Result := True;
      try
        prevCursor    := Screen.Cursor;
        Screen.Cursor := crAppStart;
        try
          if (showProgress) then begin
            progressDlg := TSDUProgressDialog.Create(nil);
          end;
          try

            fullChunksToWrite := (size div Int64(BUFFER_SIZE));

            // Initialize progress dialog
            if (showProgress) then begin
              progressDlg.ConfirmCancel     := False;
              progressDlg.ShowTimeRemaining := True;


              // We add 1 to cater for if the last block is less than the
              // BUFFER_SIZE. If it's exactly a multiple of BUFFER_SIZE then this
              // simply means that the dialog will never hit 100% completed - but
              // since the dialog will be free'd off at that point, it doens't
              // really matter!
              // Cast 1 to int64 to prevent silly conversion to & from integer
              progressDlg.i64Max      := fullChunksToWrite + Int64(1);
              progressDlg.i64Min      := 0;
              progressDlg.i64Position := 0;

              progressDlg.Show();
            end;


            // Write the buffer to disk as many times as required
            x := 0;
            while (x < fullChunksToWrite) do begin
              WriteFile(
                fileHandle,
                buffer[0],
                BUFFER_SIZE,
                bytesWritten,
                nil
                );

              if (bytesWritten <> BUFFER_SIZE) then begin
                Result := False;
                break;
              end;

              // Cast 1 to int64 to prevent silly conversion to & from integer
              x := x + Int64(1);


              Application.ProcessMessages();
              if (showProgress) then begin
                // Display progress...
                progressDlg.i64IncPosition();

                // Check for user cancel...
                if (progressDlg.Cancel) then begin
                  userCancelled := True;
                  Result        := False;
                  // Get out of loop...
                  break;
                end;

              end;

            end;



            if (Result) then begin
              // If the size of the file requested is not a mulitple of the buffer size,
              // write the remaining bytes
              WriteFile(
                fileHandle,
                buffer[0],
                (size mod BUFFER_SIZE),
                bytesWritten,
                nil
                );
              if (bytesWritten <> (size mod BUFFER_SIZE)) then begin
                Result := False;
              end;

            end;

          finally
            if (progressDlg <> nil) then begin
              progressDlg.Free();
            end;
          end;

        finally
          Screen.Cursor := prevCursor;
        end;

      finally
        CloseHandle(fileHandle);

        // In case of error, delete any file created
        if not (Result) then begin
          DeleteFile(filename);
        end;
      end;

    end;  // if (fileHandle<>INVALID_HANDLE_VALUE) then

  finally
    FreeMem(buffer);
  end;

end;


 // ----------------------------------------------------------------------------
 // Get special directory.
 // See CSIDL_DESKTOP, etc consts in ShlObj
function SDUGetSpecialFolderPath(const CSIDL: Integer): String;
var
  pidl:     PItemIDList;
  retVal:   array [0..MAX_PATH] of Widechar;
  ifMalloc: IMalloc;
begin
  //todo: use SHGetKnownFolderPath instead
  if Succeeded((SHGetSpecialFolderLocation(0, CSIDL, pidl))) then begin
    if (SHGetPathFromIDList(pidl, retVal)) then begin
      if Succeeded(ShGetMalloc(ifMalloc)) then begin
        ifMalloc.Free(pidl);
        ifMalloc := nil;
      end;
    end;
  end;
  Result := retVal;

end;


 // ----------------------------------------------------------------------------
 // Get the Windows directory
 // (Optimised version, from OracleX <oraclex@mail.ru>)
function SDUGetWindowsDirectory(): String;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetWindowsDirectory(PChar(Result), MAX_PATH));
end;

{
function SDUGetWindowsDirectory(): string;
var
  pathLen: integer;
  path: string;
begin
  pathLen := GetWindowsDirectory(nil, 0);

  // +1 to include the terminating NULL
  path := StringOfChar(#0, (pathLen + 1));
  GetWindowsDirectory(PChar(path), length(path));

  // Strip off any terminating NULLs and return
  Result := StrPas(PChar(path));
end;
}

 // ----------------------------------------------------------------------------
 // Get the System directory
function SDUGetSystemDirectory(): String;
var
  pathLen: Integer;
  path:    String;
begin
  pathLen := GetSystemDirectory(nil, 0);

  // +1 to include the terminating NULL
  path := StringOfChar(#0, (pathLen + 1));
  GetSystemDirectory(PChar(path), length(path));

  // Strip off any terminating NULLs and return
  Result := StrPas(PChar(path));
end;


 // ----------------------------------------------------------------------------
 // Get the temp directory
function SDUGetTempDirectory(): String;
var
  pathLen: Integer;
  path:    String;
begin
  pathLen := GetTempPath(0, nil);

  // +1 to include the terminating NULL
  path := StringOfChar(#0, (pathLen + 1));
  GetTempPath(length(path), PChar(path));

  // Strip off any terminating NULLs and return
  Result := StrPas(PChar(path));
end;


 // ----------------------------------------------------------------------------
 // Get current working directory (CWD)
function SDUGetCWD(): String;
begin
  Result := SDUGetCurrentWorkingDirectory();
end;

function SDUGetCurrentWorkingDirectory(): String;
var
  pathLen: Integer;
  path:    String;
begin
  pathLen := GetCurrentDirectory(0, nil);

  // +1 to include the terminating NULL
  path := StringOfChar(#0, (pathLen + 1));
  GetCurrentDirectory(length(path), PChar(path));

  // Strip off any terminating NULLs and return
  Result := StrPas(PChar(path));
end;


// ----------------------------------------------------------------------------
procedure SDUSetCWD(newDir: String);
begin
  SDUSetCurrentWorkingDirectory(newDir);
end;

 // Set current working directory (CWD)
 // Only included for completeness
procedure SDUSetCurrentWorkingDirectory(newDir: String);
begin
  SetCurrentDir(newDir);
end;


// ----------------------------------------------------------------------------
function _SDUGenerateShortcutFilename(ShortcutLocation: String; ShortcutName: String): String;
const
  SHORTCUT_EXTENSION = '.lnk';
begin
  Result := IncludeTrailingPathDelimiter(ShortcutLocation) + ShortcutName + SHORTCUT_EXTENSION;
end;

 // Check if shortcut exists
 // Returns TRUE if it exists, otherwise FALSE
 // See CSIDL_DESKTOP, etc consts in ShlObj
function SDUDoesShortcutExist(ShortcutCSIDL: Integer; ShortcutName: String): Boolean;
begin
  Result := SDUDoesShortcutExist(SDUGetSpecialFolderPath(ShortcutCSIDL), ShortcutName);
end;

function SDUDoesShortcutExist(ShortcutLocation: String; ShortcutName: String): Boolean;
begin
  Result := FileExists(_SDUGenerateShortcutFilename(ShortcutLocation, ShortcutName));
end;

 // Delete specified shortcut
 // Returns TRUE on success, otherwise FALSE
 // See CSIDL_DESKTOP, etc consts in ShlObj
function SDUDeleteShortcut(ShortcutCSIDL: Integer; ShortcutName: String): Boolean;
begin
  Result := SDUDeleteShortcut(SDUGetSpecialFolderPath(ShortcutCSIDL), ShortcutName);
end;

function SDUDeleteShortcut(ShortcutLocation: String; ShortcutName: String): Boolean;
begin
  Result := DeleteFile(_SDUGenerateShortcutFilename(ShortcutLocation, ShortcutName));
end;

 // Create a Windows shortcut file (e.g. a desktop shortcut to an executable)
 // Examples of use:
 //
 //   Create simple shortcut to running executable on desktop:
 //
 //     SDUCreateShortcut(
 //                       SDUGetSpecialFolderPath(CSIDL_DESKTOP),
 //                       'fred2',
 //                       ParamStr(0)
 //                      );
 //
 //   Create shortcut to running executable on desktop:
 //
 //     SDUCreateShortcut(
 //                       SDUGetSpecialFolderPath(CSIDL_DESKTOP),
 //                       'fred',
 //                       ParamStr(0),
 //                       '/fred',
 //                       'c:\temp',
 //                       //HotKey1.HotKey,
 //                       wsNormal,
 //                       'Comment here'
 //                       );
 //
 // See CSIDL_DESKTOP, etc consts in ShlObj
 //
function SDUCreateShortcut(ShortcutCSIDL: Integer; ShortcutName: String;
  Target: String; Parameters: String = ''; StartIn: String = '';
  // ShortcutKey: TShortCut;  - not yet implemented
  RunWindowState: TWindowState = wsNormal; Comment: String = ''): Boolean;
begin
  Result := SDUCreateShortcut(SDUGetSpecialFolderPath(ShortcutCSIDL),
    ShortcutName, Target, Parameters, StartIn,
    // ShortcutKey,  - not yet implemented
    RunWindowState, Comment);

end;

function SDUCreateShortcut(ShortcutLocation: String; ShortcutName: String;
  Target: String; Parameters: String = ''; StartIn: String = '';
  // ShortcutKey: TShortCut;  - not yet implemented
  RunWindowState: TWindowState = wsNormal; Comment: String = ''): Boolean;
var
  IObject:          IUnknown;
  ISLink:           IShellLink;
  IPFile:           IPersistFile;
  shortcutFilename: WideString;
  useShowCmd:       Integer;
begin
  IObject := CreateComObject(CLSID_ShellLink);
  ISLink  := IObject as IShellLink;
  IPFile  := IObject as IPersistFile;

  ISLink.SetPath(PChar(Target));

  ISLink.SetArguments(PChar(Parameters));

  if (StartIn = '') then begin
    StartIn := ExtractFilePath(Target);
  end;
  ISLink.SetWorkingDirectory(PChar(StartIn));

  useShowCmd := SW_SHOWNORMAL;
  if (RunWindowState = wsMinimized) then begin
    useShowCmd := SW_SHOWMINNOACTIVE;  // From MSDN WWW site - don't use SW_SHOWMINIMISE here
  end else
  if (RunWindowState = wsMaximized) then begin
    useShowCmd := SW_SHOWMAXIMIZED;
  end;
  ISLink.SetShowCmd(useShowCmd);

  //   ISLink.SetHotkey(ShortcutKey);

  ISLink.SetDescription(PChar(Comment));

  shortcutFilename := _SDUGenerateShortcutFilename(ShortcutLocation, ShortcutName);
  Result           := (IPFile.Save(PWChar(shortcutFilename), False) = S_OK);

end;


 // Get the "Run" windowstate specified in the identified shortcut
 // Note: This is only suitable for checking shortcuts to files/executables
 //       (.lnk shortcuts) - NOT URLs (WWW sites; .url files)
 // See CSIDL_DESKTOP, etc consts in ShlObj
function SDUGetShortCutRunWindowState(ShortcutCSIDL: Integer; ShortcutName: String): TWindowState;
begin
  Result := SDUGetShortCutRunWindowState(SDUGetSpecialFolderPath(ShortcutCSIDL), ShortcutName);
end;

function SDUGetShortCutRunWindowState(ShortcutLocation: String;
  ShortcutName: String): TWindowState;
var
  IObject:          IUnknown;
  ISLink:           IShellLink;
  IPFile:           IPersistFile;
  shortcutFilename: WideString;
  useShowCmd:       Integer;
begin
  Result := wsNormal;

  IObject := CreateComObject(CLSID_ShellLink);
  ISLink  := IObject as IShellLink;
  IPFile  := IObject as IPersistFile;

  shortcutFilename := _SDUGenerateShortcutFilename(ShortcutLocation, ShortcutName);
  if (IPFile.Load(PWChar(shortcutFilename), STGM_READ) = S_OK) then begin
    ISLink.GetShowCmd(useShowCmd);

    if (useShowCmd = SW_SHOWMINNOACTIVE) then
      // From MSDN WWW site - don't use SW_SHOWMINIMISE here
    begin
      Result := wsMinimized;
    end else
    if (useShowCmd = SW_SHOWMAXIMIZED) then begin
      Result := wsMaximized;
    end;
  end;

end;


function SDUGetShortCutArguments(ShortcutCSIDL: Integer; ShortcutName: String): String;
var
  IObject:          IUnknown;
  ISLink:           IShellLink;
  IPFile:           IPersistFile;
  shortcutFilename: WideString;
  szArgs:           array [0..255] of wchar;
  ShortcutLocation: String;
begin
  Result           := '';
  ShortcutLocation := SDUGetSpecialFolderPath(ShortcutCSIDL);
  IObject          := CreateComObject(CLSID_ShellLink);
  ISLink           := IObject as IShellLink;
  IPFile           := IObject as IPersistFile;

  shortcutFilename := _SDUGenerateShortcutFilename(ShortcutLocation, ShortcutName);
  if (IPFile.Load(PWChar(shortcutFilename), STGM_READ) = S_OK) then begin
    ISLink.GetArguments(@szArgs[0], 255);

    Result := szArgs;
  end;

end;

 // ----------------------------------------------------------------------------
 // Convert boolean value to string/char
function SDUBoolToStr(Value: Boolean; strTrue: String = 'True';
  strFalse: String = 'False'): String;
begin
  Result := SDUBooleanToStr(Value, strTrue, strFalse);
end;

function SDUBoolToString(Value: Boolean; strTrue: String = 'True';
  strFalse: String = 'False'): String; overload;
begin
  Result := SDUBooleanToStr(Value, strTrue, strFalse);
end;

function SDUBooleanToString(Value: Boolean; strTrue: String = 'True';
  strFalse: String = 'False'): String; overload;
begin
  Result := SDUBooleanToStr(Value, strTrue, strFalse);
end;

function SDUBooleanToStr(Value: Boolean; strTrue: String = 'True';
  strFalse: String = 'False'): String;
begin
  Result := strFalse;
  if Value then begin
    Result := strTrue;
  end;

end;

function SDUBoolToChar(Value: Boolean; chars: String = 'TF'): Char;
begin
  Result := SDUBooleanToChar(Value, chars);
end;

function SDUBooleanToChar(Value: Boolean; chars: String = 'TF'): Char;
begin
  // Sanity check
  if (length(chars) < 2) then begin
    raise Exception.Create('Exactly zero or two characters must be passed to BoolToChar');
  end;

  Result := chars[2];
  if Value then begin
    Result := chars[1];
  end;

end;


 // ----------------------------------------------------------------------------
 // Convert string/char value to boolean
function SDUStrToBool(Value: String; strTrue: String = 'True';
  strFalse: String = 'False'): Boolean;
begin
  Result := SDUStrToBoolean(Value, strTrue, strFalse);
end;

function SDUStringToBool(Value: String; strTrue: String = 'True';
  strFalse: String = 'False'): Boolean;
begin
  Result := SDUStrToBoolean(Value, strTrue, strFalse);
end;

function SDUStringToBoolean(Value: String; strTrue: String = 'True';
  strFalse: String = 'False'): Boolean;
begin
  Result := SDUStrToBoolean(Value, strTrue, strFalse);
end;

function SDUStrToBoolean(Value: String; strTrue: String = 'True';
  strFalse: String = 'False'): Boolean;
begin
  Result := (uppercase(Value) = uppercase(strTrue));
end;

function SDUCharToBool(Value: Char; chars: String = 'TF'): Boolean;
begin
  Result := SDUCharToBoolean(Value, chars);
end;

function SDUCharToBoolean(Value: Char; chars: String = 'TF'): Boolean;
begin
  // Sanity check
  if (length(chars) < 2) then begin
    raise Exception.Create('Exactly zero or two characters must be passed to BoolToChar');
  end;

  Result := (uppercase(Value) = uppercase(chars[1]));
end;

// ----------------------------------------------------------------------------
function SDUFloatTrunc(X: Double; decimalPlaces: Integer): Double;
var
  multiplier: Extended;
begin
  multiplier := Power(10, decimalPlaces);
  Result     := (trunc(x * multiplier)) / multiplier;
end;

 // ----------------------------------------------------------------------------
 // Format ULONGLONG passed in to add thousands separator
function SDUFormatWithThousandsSeparator(const Value: ULONGLONG): String;
var
  tmp64:     ULONGLONG;
  tmp64Zero: ULONGLONG;
  tmp64Ten:  ULONGLONG;
  tmp64Mod:  ULONGLONG;
  digitCnt:  Integer;
begin
  Result := '';

  tmp64     := Value;
  tmp64Zero := 0;
  tmp64Ten  := 10;

  if (Value = tmp64Zero) then begin
    Result := IntToStr(0);
  end else begin
    digitCnt := -1;
    while (tmp64 > tmp64Zero) do begin
      tmp64Mod := (tmp64 mod tmp64Ten);

      Inc(digitCnt);
      if ((digitCnt > 0) and ((digitCnt mod 3) = 0)) then begin
        Result := FormatSettings.ThousandSeparator + Result;
      end;

      Result := IntToStr(tmp64Mod) + Result;

      tmp64 := tmp64 div tmp64Ten;
    end;
  end;

end;

// ----------------------------------------------------------------------------
function SDUFormatUnits(Value: Int64; denominations: array of String;
  multiplier: Integer = 1000; accuracy: Integer = 2): String;
var
  z:        Double;
  useUnits: String;
  unitsIdx: Integer;
  absValue: Int64;
  unitsDiv: Int64;
begin
  absValue := abs(Value);

  // Identify the units to be used
  unitsIdx := 0;
  unitsDiv := 1;
  while ((absValue >= (unitsDiv * multiplier)) and (unitsIdx < high(denominations))) do begin
    Inc(unitsIdx);
    unitsDiv := unitsDiv * multiplier;
  end;

  useUnits := '';
  if ((unitsIdx >= low(denominations)) and (unitsIdx <= high(denominations))) then begin
    useUnits := denominations[unitsIdx];
  end else
  if (unitsIdx >= high(denominations)) then begin
    useUnits := denominations[high(denominations)];
  end;

  // If we're using the lowest denomination, don't include ".xyz"
  if (unitsIdx = 0) then begin
    accuracy := 0;
  end;

  z := SDUFloatTrunc((Value / unitsDiv), accuracy);

  Result := Format('%.' + IntToStr(accuracy) + 'f', [z]) + ' ' + useUnits;

end;

// ----------------------------------------------------------------------------
{$IF CompilerVersion >= 18.5}// See comment on ULONGLONG definition
function SDUFormatUnits(Value: ULONGLONG; denominations: array of String;
  multiplier: Integer = 1000; accuracy: Integer = 2): String;
var
  z:        Double;
  useUnits: String;
  unitsIdx: Integer;
  absValue: ULONGLONG;
  unitsDiv: ULONGLONG;
begin
  absValue := abs(Value);

  // Identify the units to be used
  unitsIdx := 0;
  unitsDiv := 1;
  while ((absValue >= (unitsDiv * multiplier)) and (unitsIdx < high(denominations))) do begin
    Inc(unitsIdx);
    unitsDiv := unitsDiv * multiplier;
  end;

  useUnits := '';
  if ((unitsIdx >= low(denominations)) and (unitsIdx <= high(denominations))) then begin
    useUnits := denominations[unitsIdx];
  end else
  if (unitsIdx >= high(denominations)) then begin
    useUnits := denominations[high(denominations)];
  end;

  // If we're using the lowest denomination, don't include ".xyz"
  if (unitsIdx = 0) then begin
    accuracy := 0;
  end;

  z := SDUFloatTrunc((Value / unitsDiv), accuracy);

  Result := Format('%.' + IntToStr(accuracy) + 'f', [z]) + ' ' + useUnits;

end;

{$IFEND}

// ----------------------------------------------------------------------------
function SDUFormatAsBytesUnits(Value: Int64; accuracy: Integer = 2): String;
begin
  Result := SDUFormatUnits(Value, SDUUnitsStorageToTextArr(), UNITS_BYTES_MULTIPLIER, accuracy);
end;

// ----------------------------------------------------------------------------
{$IF CompilerVersion >= 18.5}// See comment on ULONGLONG definition
function SDUFormatAsBytesUnits(Value: ULONGLONG; accuracy: Integer = 2): String;
begin
  Result := SDUFormatUnits(Value, SDUUnitsStorageToTextArr(), UNITS_BYTES_MULTIPLIER, accuracy);
end;

{$IFEND}

// ----------------------------------------------------------------------------
function SDUFormatAsBytesAndBytesUnits(Value: ULONGLONG; accuracy: Integer = 2): String;
var
  sizeAsUnits: String;
begin
  Result      := SDUIntToStr(Value) + ' ' + SDUUnitsStorageToText(usBytes);
  sizeAsUnits := SDUFormatAsBytesUnits(Value);
  if (Pos(SDUUnitsStorageToText(usBytes), sizeAsUnits) <= 0) then begin
    Result := Result + ' (' + sizeAsUnits + ')';
  end;

end;


// ----------------------------------------------------------------------------
function SDUParseUnits(prettyValue: String; denominations: array of String;
  out Value: uint64; multiplier: Integer = 1000): Boolean;
var
  i:               Integer;
  strNumber:       String;
  strUnits:        String;
  unitsMultiplier: Int64;
  foundMultiplier: Boolean;
begin
  Result := True;

  Value := 0;

  // Split on space, or detect boundry
  strNumber   := '';
  strUnits    := '';
  prettyValue := trim(prettyValue);
  if (Pos(' ', prettyValue) > 0) then begin
    SDUSplitString(prettyValue, strNumber, strUnits, ' ');
  end else begin
    for i := 1 to length(prettyValue) do begin
      if (((prettyValue[i] < '0') or (prettyValue[i] > '9')) and
        (prettyValue[i] <> '-')  // Allow -ve values
        ) then begin
        strUnits := Copy(prettyValue, i, (length(prettyValue) - i + 1));
        break;
      end;

      strNumber := strNumber + prettyValue[i];
    end;
  end;

  strNumber := trim(strNumber);
  strUnits  := trim(strUnits);

  unitsMultiplier := 1;
  if (strUnits <> '') then begin
    strUnits        := uppercase(strUnits);
    foundMultiplier := False;
    for i := low(denominations) to high(denominations) do begin
      if (strUnits = uppercase(denominations[i])) then begin
        foundMultiplier := True;
        break;
      end;

      unitsMultiplier := unitsMultiplier * multiplier;
    end;

    Result := Result and foundMultiplier;
  end;

  if Result then begin
    Result := SDUTryStrToInt(strNumber, Value);
  end;

  if Result then begin
    Value := Value * unitsMultiplier;
  end;

end;

// ----------------------------------------------------------------------------
{$IFDEF VER185}   // See comment on ULONGLONG definition
function SDUParseUnits(
                       prettyValue: string;
                       denominations: array of string;
                       out value: ULONGLONG;
                       multiplier: integer = 1000
                     ): boolean;
var
  i: integer;
  strNumber: string;
  strUnits: string;
  unitsMultiplier: ULONGLONG;
  foundMultiplier: boolean;
begin
  Result := TRUE;

  value := 0;

  // Split on space, or detect boundry
  strNumber := '';
  strUnits := '';
  prettyValue := trim(prettyValue);
  if (Pos(' ', prettyValue) > 0) then
    begin
    SDUSplitString(prettyValue, strNumber, strUnits, ' ');
    end
  else
    begin
    for i:=1 to length(prettyValue) do
      begin
      if (
          (prettyValue[i] < '0') or
          (prettyValue[i] > '9')
         ) then
        begin
        strUnits := Copy(prettyValue, i, (length(prettyValue) - i + 1));
        break;
        end;

      strNumber := strNumber + prettyValue[i];
      end;
    end;

  strNumber := trim(strNumber);
  strUnits := trim(strUnits);

  unitsMultiplier := 1;
  if (strUnits <> '') then
    begin
    strUnits := uppercase(strUnits);
    foundMultiplier := FALSE;
    for i:=low(denominations) to high(denominations) do
      begin
      if (strUnits = uppercase(denominations[i])) then
        begin
        foundMultiplier := TRUE;
        break;
        end;

      unitsMultiplier := unitsMultiplier * multiplier;
      end;

      Result := Result and foundMultiplier;
    end;

  if Result then
    begin
    Result := SDUTryStrToInt(strNumber, value);

    end;

  if Result then
    begin
    value := value * unitsMultiplier;
    end;


end;
{$ENDIF}

// ----------------------------------------------------------------------------
function SDUParseUnitsAsBytesUnits(prettyValue: String; out Value: uint64): Boolean;
begin
  Result := SDUParseUnits(prettyValue, SDUUnitsStorageToTextArr(), Value,
    UNITS_BYTES_MULTIPLIER);
end;


{$IFDEF VER185}  // See comment on ULONGLONG definition
function SDUParseUnitsAsBytesUnits(
                       prettyValue: string;
                       out value: ULONGLONG
                     ): boolean;
begin
  Result := SDUParseUnits(
                          prettyValue,
                          SDUUnitsStorageToTextArr(),
                          value,
                          UNITS_BYTES_MULTIPLIER
                         );
end;
{$ENDIF}

// ----------------------------------------------------------------------------
procedure SDUCenterControl(control: TControl; align: TCenterControl);
begin
  SDUCenterControl(control, align, 50);
end;

// ----------------------------------------------------------------------------
procedure SDUCenterControl(Controls: TControlArray; align: TCenterControl);
begin
  SDUCenterControl(Controls, align, 50);
end;

 // ----------------------------------------------------------------------------
 // Position a control relative to it's parent, percentage based (central is 50%)
procedure SDUCenterControl(control: TControl; align: TCenterControl; pcnt: Integer);
var
  ctrlArr: TControlArray;
begin
  SetLength(ctrlArr, 1);
  ctrlArr[0] := control;
  SDUCenterControl(ctrlArr, align, pcnt);

end;

 // ----------------------------------------------------------------------------
 // Position a control relative to it's parent, percentage based (central is 50%)
procedure SDUCenterControl(Controls: TControlArray; align: TCenterControl; pcnt: Integer);
var
  i:              Integer;
  currCtrl:       TControl;
  minLeft:        Integer;
  minTop:         Integer;
  combinedWidth:  Integer;
  combinedHeight: Integer;
begin
  if (length(Controls) = 0) then begin
    exit;
  end;

  // Determine the min X, Y of all the controls
  minLeft := Controls[low(Controls)].left;
  minTop  := Controls[low(Controls)].top;
  for i := low(Controls) to high(Controls) do begin
    currCtrl := Controls[i];

    minLeft := min(minLeft, currCtrl.left);
    minTop  := min(minTop, currCtrl.top);
  end;

  // Determine the combined width, height of all the controls
  combinedWidth  := Controls[low(Controls)].Width;
  combinedHeight := Controls[low(Controls)].Height;
  for i := low(Controls) to high(Controls) do begin
    currCtrl := Controls[i];

    combinedWidth  := max(combinedWidth, ((currCtrl.Left - minLeft) + currCtrl.Width));
    combinedHeight := max(combinedHeight, ((currCtrl.top - minTop) + currCtrl.Height));
  end;

  for i := low(Controls) to high(Controls) do begin
    currCtrl := Controls[i];

    if ((align <> ccNone) and (currCtrl <> nil)) then begin
      if (currCtrl.Parent <> nil) then begin
        if ((align = ccHorizontal) or (align = ccBoth)) then begin
          currCtrl.Left := ((currCtrl.Left - minLeft) + trunc(
            ((currCtrl.Parent.Width - combinedWidth) * (pcnt / 100))));
        end;

        if ((align = ccVertical) or (align = ccBoth)) then begin
          currCtrl.Top := ((currCtrl.Top - minTop) + trunc(
            ((currCtrl.Parent.Height - combinedHeight) * (pcnt / 100))));
        end;

      end;
    end;
  end;

end;


 // ----------------------------------------------------------------------------
 // Generate all permutations of the characters in "pool"
 // Crude, but effective; there's probably a much more efficient way of
 // implementing this.
procedure SDUPermutate(pool: String; lst: TStringList);
var
  i, j:      Integer;
  tmpLst:    TStringList;
  tmpPool:   String;
  startChar: Char;
begin
  if length(pool) = 1 then begin
    lst.add(pool);
  end else begin
    tmpLst := TStringList.Create();
    try
      for i := 1 to length(pool) do begin
        tmpPool   := pool;
        startChar := pool[i];
        Delete(tmpPool, i, 1);

        tmpLst.Clear();
        SDUPermutate(tmpPool, tmpLst);

        for j := 0 to (tmpLst.Count - 1) do begin
          lst.Add(startChar + tmpLst[j]);
        end;

      end;
    finally
      tmpLst.Free();
    end;
  end;

end;


 // ----------------------------------------------------------------------------
 // Calculate x! (factorial X)
function SDUFactorial(x: Integer): LARGE_INTEGER;
var
  i: Integer;
begin
  Result.QuadPart := 1;

  if (x = 0) then begin
    Result.QuadPart := 0;
  end;

  for i := 1 to x do begin
    Result.QuadPart := Result.QuadPart * i;

{$IFOPT Q+}
    if (Result.QuadPart <= 0) then
      begin
      raise EIntOverflow.Create('Overflow when calculating '+inttostr(i)+'!');
      end;
{$ENDIF}
  end;

end;


// ----------------------------------------------------------------------------
function SDUXOR(a: TSDUBytes; b: array of Byte): TSDUBytes;
  //function SDUXOR(a: Ansistring; b: Ansistring): Ansistring;
  { DONE 1 -otdk -cclean : use bytes instead of chars }
var
  maxlen, minlen: Integer;
  i:              Integer;
begin

  maxlen := max(length(a), length(b));
  minlen := min(length(a), length(b));
  setlength(Result, maxlen);

  for i := 0 to minlen - 1 do
    Result[i] := a[i] xor b[i];

  for i := minlen to maxlen - 1 do
    if i < length(a) then
      Result[i] := a[i]
    else
      Result[i] := b[i];
end;


// ----------------------------------------------------------------------------
function SDUXORStr(a: Ansistring; b: Ansistring): Ansistring;
var
  byteA:      Byte;
  byteB:      Byte;
  i, longest: Integer;

begin
  { DONE 1 -otdk -cclean : can simplify }
  Result := '';

  longest := max(length(a), length(b));
  for i := 1 to longest do begin
    if (i > length(a)) then begin
      byteA := 0;
    end else begin
      byteA := Ord(a[i]);
    end;

    if (i > length(b)) then begin
      byteB := 0;
    end else begin
      byteB := Ord(b[i]);
    end;

    Result := Result + ansichar(byteA xor byteB);
  end;

end;


 // ----------------------------------------------------------------------------
 // length - Set to -1 to copy until failure
function SDUCopyFile(Source: String; destination: String; startOffset: Int64 = 0;
  length: Int64 = -1; blocksize: Int64 = 4096; callback: TCopyProgressCallback = nil): Boolean;
var
  srcHandle:        THandle;
  destHandle:       THandle;
  buffer:           PChar;
  numberOfBytesRead, numberOfBytesWritten: DWORD;
  move:             DWORD;
  finished:         Boolean;
  opResult:         Boolean;
  copyCancelFlag:   Boolean;
  totalBytesCopied: Int64;
begin
  Result         := True;
  copyCancelFlag := False;

  srcHandle := CreateFile(PChar(Source), GENERIC_READ, (FILE_SHARE_READ or FILE_SHARE_WRITE),
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if srcHandle = INVALID_HANDLE_VALUE then begin
    raise EExceptionBadSrc.Create('Can''t open source');
  end;

  try
    destHandle := CreateFile(PChar(destination), CREATE_ALWAYS, FILE_SHARE_READ,
      nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

    if destHandle = INVALID_HANDLE_VALUE then begin
      raise EExceptionBadDest.Create('Can''t open destination');
    end;

    try
      move := SetFilePointer(srcHandle,  // handle of file
        startOffset,                     // number of bytes to move file pointer
        nil,                             // address of high-order word of distance to move
        FILE_BEGIN                       // how to move
        );
      if (move <> DWORD(startOffset)) then begin
        raise EExceptionBadSrcOffset.Create('Can''t open destination');
      end;

      totalBytesCopied := 0;
      if assigned(callback) then begin
        callback(totalBytesCopied, copyCancelFlag);
      end;

      buffer := AllocMem(blocksize);
      try
        finished := False;
        while not (finished) do begin
          opResult := ReadFile(srcHandle, buffer[0], blocksize, numberOfBytesRead, nil);

          // If we read was successful, and we read some bytes, we haven't
          // finished yet... 
          finished := not (opResult and (numberOfBytesRead > 0));

          if (numberOfBytesRead > 0) then begin
            // If we've got a limit to the number of bytes we should copy...
            if (length >= 0) then begin
              if ((totalBytesCopied + numberOfBytesRead) > length) then begin
                numberOfBytesRead := length - totalBytesCopied;
              end;
            end;

            opResult := WriteFile(destHandle, buffer[0], numberOfBytesRead,
              numberOfBytesWritten, nil);

            if (not (opResult) or (numberOfBytesRead <> numberOfBytesWritten)) then begin
              raise EExceptionWriteError.Create('Unable to write data to output');
            end;

            totalBytesCopied := totalBytesCopied + numberOfBytesWritten;
            if assigned(callback) then begin
              callback(totalBytesCopied, copyCancelFlag);
            end;

            if ((length >= 0) and (totalBytesCopied >= length)) then begin
              finished := True;
            end;

            // Check for user cancel
            if copyCancelFlag then begin
              raise EExceptionUserCancel.Create('User cancelled operation.');
            end;

          end;
        end;

      finally
        FreeMem(buffer);
      end;

    finally
      CloseHandle(destHandle);
    end;

  finally
    CloseHandle(srcHandle);
  end;

end;

 // ----------------------------------------------------------------------------
 // NOTICE: The format of the compressed version is:
 //
 //           *) "SDU_COMPRESS" - SDU compression magic
 //           *) ":" - Separator colon
 //           *) "1.00" - Compression version 1.00
 //           *) ":" - Separator colon
 //           *) The length of the *decompressed* data stored int64, followed by
 //           *) ":" - Separator colon
 //           *) The compressed file.
 //
 //         Note that both the int64 size of the file is a *compressed* int64
 //
 // compressNotDecompress - Set to TRUE to compress, FALSE to decompress
 // compressionLevel - Only used if compressNotDecompress is TRUE
 // length - Set to -1 to copy until failure
function SDUCopyFile_Compression(Source: String; destination: String;
  compressNotDecompress: Boolean; compressionLevel: TCompressionLevel = clNone;
  startOffset: Int64 = 0;
  // Only valid when compressing 
  length: Int64 = -1; blocksize: Int64 = 4096; callback: TCopyProgressCallback = nil): Boolean;
const
  SDU_COMPRESS_MAGIC             = 'SDU_COMPRESS';
  SDU_COMPRESS_VERSION           = '1.00';
  SDU_COMPRESS_SEPERATOR: String = ':';
var
  numberOfBytesRead, numberOfBytesWritten: DWORD;
  finished:         Boolean;
  copyCancelFlag:   Boolean;
  totalBytesCopied: Int64;
  stmFileInput:     TFileStream;
  stmFileOutput:    TFileStream;
  stmInput:         TStream;
  stmOutput:        TStream;
  stmCompress:      TCompressionStream;
  stmDecompress:    TDecompressionStream;
  junkBuffer:       array [0..1023] of Char;
begin
  Result         := True;
  copyCancelFlag := False;

  stmCompress   := nil;
  stmDecompress := nil;

  try
    stmFileInput := TFileStream.Create(Source, fmOpenRead);
  except
    stmFileInput := nil;
  end;
  if (stmFileInput = nil) then begin
    raise EExceptionBadSrc.Create('Can''t open source');
  end;

  try
    try
      try
        // Try to create new...
        stmFileOutput := TFileStream.Create(destination, fmCreate);
      except
        // Fallback to trying to open existing for write...
        stmFileOutput := TFileStream.Create(destination, fmOpenWrite);
      end;
    except
      stmFileOutput := nil;
    end;
    if (stmFileOutput = nil) then begin
      raise EExceptionBadDest.Create('Can''t open destination');
    end;

    try
      if compressNotDecompress then begin
        stmCompress := TCompressionStream.Create(compressionLevel, stmFileOutput);
        stmInput    := stmFileInput;
        stmOutput   := stmCompress;

        if (length < 0) then begin
          length := stmFileInput.Size;
        end;

        stmOutput.Write(SDU_COMPRESS_MAGIC, sizeof(SDU_COMPRESS_MAGIC));
        stmOutput.Write(SDU_COMPRESS_SEPERATOR, sizeof(SDU_COMPRESS_SEPERATOR));
        stmOutput.Write(SDU_COMPRESS_VERSION, sizeof(SDU_COMPRESS_VERSION));
        stmOutput.Write(SDU_COMPRESS_SEPERATOR, sizeof(SDU_COMPRESS_SEPERATOR));
        stmOutput.Write(length, SizeOf(length));
        stmOutput.Write(SDU_COMPRESS_SEPERATOR, sizeof(SDU_COMPRESS_SEPERATOR));

        stmInput.Position := startOffset;
      end else begin
        stmDecompress := TDecompressionStream.Create(stmFileInput);
        stmInput      := stmDecompress;
        stmOutput     := stmFileOutput;

        stmInput.Read(junkBuffer, sizeof(SDU_COMPRESS_MAGIC));
        stmInput.Read(junkBuffer, sizeof(SDU_COMPRESS_SEPERATOR));
        stmInput.Read(junkBuffer, sizeof(SDU_COMPRESS_VERSION));
        stmInput.Read(junkBuffer, sizeof(SDU_COMPRESS_SEPERATOR));
        stmInput.Read(length, SizeOf(length));
        stmInput.Read(junkBuffer, sizeof(SDU_COMPRESS_SEPERATOR));
      end;

      totalBytesCopied := 0;
      if assigned(callback) then begin
        callback(totalBytesCopied, copyCancelFlag);
      end;

      finished := False;
      while not (finished) do begin
        // If we've got a limit to the number of bytes we should copy...
        numberOfBytesRead := blockSize;
        if (length >= 0) then begin
          if ((totalBytesCopied + numberOfBytesRead) > length) then begin
            numberOfBytesRead := length - totalBytesCopied;
          end;
        end;

        if (numberOfBytesRead = 0) then begin
          break;
        end;

        numberOfBytesWritten := 0;
        try
          numberOfBytesWritten := stmOutput.CopyFrom(stmInput, numberOfBytesRead);
        except
          if (length < 0) then begin
            finished := True;
          end;

        end;

        if not (finished) then begin
          if (numberOfBytesRead <> numberOfBytesWritten) then begin
            raise EExceptionWriteError.Create('Unable to write data to output');
          end;
        end;

        totalBytesCopied := totalBytesCopied + numberOfBytesWritten;
        if assigned(callback) then begin
          callback(totalBytesCopied, copyCancelFlag);
        end;

        if ((length >= 0) and (totalBytesCopied >= length)) then begin
          finished := True;
        end;

        // Check for user cancel
        if copyCancelFlag then begin
          raise EExceptionUserCancel.Create('User cancelled operation.');
        end;

      end;

    finally
      if (compressNotDecompress and (stmCompress <> nil)) then begin
        stmCompress.Free();
      end;
      stmFileOutput.Free();
    end;

  finally
    if (not (compressNotDecompress) and (stmDecompress <> nil)) then begin
      stmDecompress.Free();
    end;
    stmFileInput.Free();
  end;

end;


 // ----------------------------------------------------------------------------
 // Get size of file
 // This is just a slightly easier to use version of GetFileSize
 // Returns file size, or -1 on error
function SDUGetFileSizeTwo(filename: String): LARGE_INTEGER;
var
  fileHandle:        THandle;
  sizeLow, sizeHigh: DWORD;
begin
  Result.QuadPart := -1;
  sizeHigh        := 0;

  // Open file and get it's size
  fileHandle := CreateFile(PChar(filename),
    // pointer to name of the file
    GENERIC_READ,             // access (read-write) mode
    (FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE),
    // share mode
    nil,                      // pointer to security attributes
    OPEN_EXISTING,            // how to create
    FILE_FLAG_RANDOM_ACCESS,  // file attributes
    0                         // handle to file with attributes to copy
    );

  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    sizeLow := SetFilePointer(fileHandle, 0, @sizeHigh, FILE_END);

    if not ((sizeLow = $FFFFFFFF) and (GetLastError() <> NO_ERROR)) then begin
      Result.HighPart := sizeHigh;
      Result.LowPart  := sizeLow;
    end;

    CloseHandle(fileHandle);
  end;

end;


 // ----------------------------------------------------------------------------
 // Get size of partition
 // Returns partition size, or -1 on error
function SDUGetPartitionSize(driveletter: ansichar): ULONGLONG;
begin
  Result := SDUGetPartitionSize_Device(SDUDeviceNameForDrive(driveLetter));
end;


// ----------------------------------------------------------------------------
function SDUGetPartitionSize_Device(driveDevice: String): ULONGLONG;
var
  partInfo: TSDUPartitionInfo;
begin
  Result := 0;

  if SDUGetPartitionInfo(driveDevice, partInfo) then begin
    Result := partInfo.PartitionLength;
  end;

end;

// ----------------------------------------------------------------------------
function SDUGetDiskGeometry(driveletter: ansichar; var diskGeometry: TSDUDiskGeometry): Boolean;
begin
  Result := SDUGetDiskGeometry(SDUDeviceNameForDrive(driveLetter), diskGeometry);
end;

// ----------------------------------------------------------------------------
function SDUGetDiskGeometry(DiskNumber: Integer; var diskGeometry: TSDUDiskGeometry): Boolean;
  overload;
begin
  Result := SDUGetDiskGeometry(SDUDeviceNameForDisk(DiskNumber), diskGeometry);
end;

 // ----------------------------------------------------------------------------
 // This can be used with (for example):
 //   \\.\C:
 //   \\.\PHYSICALDRIVE2   (i.e. Format(HDD_DISK_DEVICE_NAME_FORMAT, [<diskNo>]);
function SDUGetDiskGeometry(driveDevice: String; var diskGeometry: TSDUDiskGeometry): Boolean;
var
  fileHandle:    THandle;
  DIOCBufferOut: TSDUDiskGeometry;
  bytesReturned: DWORD;
begin
  Result := False;

  // Open file and get it's size
  fileHandle := CreateFile(PChar(driveDevice),
    // pointer to name of the file
    GENERIC_READ,             // access (read-write) mode
    (FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE),
    // share mode
    nil,                      // pointer to security attributes
    OPEN_EXISTING,            // how to create
    FILE_FLAG_RANDOM_ACCESS,  // file attributes
    0                         // handle to file with attributes to copy
    );

  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    if (DeviceIoControl(fileHandle, SDU_IOCTL_DISK_GET_DRIVE_GEOMETRY, nil,
      0, @DIOCBufferOut, sizeof(DIOCBufferOut), bytesReturned, nil)) then begin
      diskGeometry := DIOCBufferOut;
      Result       := True;
    end;

    CloseHandle(fileHandle);
  end;

end;

// ----------------------------------------------------------------------------
function SDUGetPartitionInfo(driveletter: ansichar; var partInfo: TSDUPartitionInfo): Boolean;
begin
  Result := SDUGetPartitionInfo(SDUDeviceNameForDrive(driveLetter), partInfo);
end;


// ----------------------------------------------------------------------------
function SDUGetPartitionInfo(driveDevice: String; var partInfo: TSDUPartitionInfo): Boolean;
var
  fileHandle:    THandle;
  DIOCBufferOut: TSDUPartitionInfo;
  bytesReturned: DWORD;
begin
  Result := False;

  // Open file and get it's size
  fileHandle := CreateFile(PChar(driveDevice),
    // pointer to name of the file
    GENERIC_READ,             // access (read-write) mode
    (FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE),
    // share mode
    nil,                      // pointer to security attributes
    OPEN_EXISTING,            // how to create
    FILE_FLAG_RANDOM_ACCESS,  // file attributes
    0                         // handle to file with attributes to copy
    );

  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    if (DeviceIoControl(fileHandle, SDU_IOCTL_DISK_GET_PARTITION_INFO, nil,
      0, @DIOCBufferOut, sizeof(DIOCBufferOut), bytesReturned, nil)) then begin
      partInfo := DIOCBufferOut;
      Result   := True;
    end;

    CloseHandle(fileHandle);
  end;

end;


// ----------------------------------------------------------------------------
function SDUGetDriveLayout(physicalDiskNo: Integer;
  var driveLayout: TSDUDriveLayoutInformation): Boolean;
var
  deviceName: String;
begin
  deviceName := Format(FMT_DEVICENAME_HDD_PHYSICAL_DISK, [physicalDiskNo]);
  Result     := SDUGetDriveLayout_Device(deviceName, driveLayout);
end;

// ----------------------------------------------------------------------------
function SDUGetDriveLayout_Device(driveDevice: String;
  var driveLayout: TSDUDriveLayoutInformation): Boolean;
var
  fileHandle:    THandle;
  DIOCBufferOut: TSDUDriveLayoutInformation;
  bytesReturned: DWORD;
begin
  Result := False;

  // Open file and get it's size
  fileHandle := CreateFile(PChar(driveDevice),
    // pointer to name of the file
    GENERIC_READ,             // access (read-write) mode
    (FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE),
    // share mode
    nil,                      // pointer to security attributes
    OPEN_EXISTING,            // how to create
    FILE_FLAG_RANDOM_ACCESS,  // file attributes
    0                         // handle to file with attributes to copy
    );

  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    if (DeviceIoControl(fileHandle, SDU_IOCTL_DISK_GET_DRIVE_LAYOUT, nil, 0,
      @DIOCBufferOut, sizeof(DIOCBufferOut), bytesReturned, nil)) then begin
      driveLayout := DIOCBufferOut;
      Result      := True;
    end;

    CloseHandle(fileHandle);
  end;

end;


// ----------------------------------------------------------------------------
function SDURelativePathToAbsolute(relativePath: String): String;
var
  cwd: String;
begin
  cwd    := SDUGetCurrentWorkingDirectory();
  Result := SDURelativePathToAbsolute(relativePath, cwd);
end;

// ----------------------------------------------------------------------------
function SDURelativePathToAbsolute(relativePath: String; relativeTo: String): String;
var
  concat:        String;
  pathLen:       Integer;
  tmpStr:        String;
  useRelativeTo: String;
  serverName:    String;
  shareName:     String;
  junkStr:       String;
  junkPChar:     PChar;
begin
  relativeTo   := trim(relativeTo);
  relativePath := trim(relativePath);

  Result := relativePath;

  if (Pos('\\', relativePath) = 1) then begin
    // UNC path; already absolute
  end else
  if (Pos(':', relativePath) = 2) then begin
    // X:... path; already absolute
  end else begin
    // If relativePath is a path relative to the *root* dir (i.e. it starts
    // with a "\", then process), determine the root dir of the drive/UNC share
    useRelativeTo := relativeTo;
    if (Pos('\', relativePath) = 1) then begin
      if (Pos(':', relativeTo) = 2) then begin
        useRelativeTo := relativeTo[1] + ':';
      end else
      if (Pos('\\', relativeTo) = 1) then begin
        // Assume relativeTo is "\\<servername>\<sharename>\..."
        // Strip 1st "\"
        SDUSplitString(relativeTo, junkStr, relativeTo, '\');
        // Strip 2nd "\"
        SDUSplitString(relativeTo, junkStr, relativeTo, '\');
        // Strip servername
        SDUSplitString(relativeTo, serverName, relativeTo, '\');
        // Strip sharename
        SDUSplitString(relativeTo, shareName, relativeTo, '\');

        useRelativeTo := '\\' + serverName + '\' + shareName + '\';
      end;
    end;

    // Otherwise, concatenate together, and call Windows API to de-relativize
    // the path
    // Note: The Windows API call doens't care about duplicated "\" chars
    tmpStr  := '';
    concat  := useRelativeTo + '\' + relativePath;
    pathLen := GetFullPathName(PChar(concat), 0, PChar(tmpStr), junkPChar);

    // +1 to include the terminating NULL
    tmpStr := StringOfChar(#0, (pathLen + 1));
    GetFullPathName(PChar(concat), length(tmpStr), PChar(tmpStr), junkPChar);

    // Strip off any terminating NULLs and return
    Result := StrPas(PChar(tmpStr));
  end;

end;


// ----------------------------------------------------------------------------
function SDUBitWiseTest(Value: Cardinal; testBit: Cardinal): Boolean;
begin
  Result := ((Value and testBit) = testBit);
end;

// ----------------------------------------------------------------------------
procedure SDUClearPanel(panel: TPanel);
begin
  panel.Caption    := '';
  panel.BevelInner := bvNone;
  panel.BevelOuter := bvNone;
end;

 // ----------------------------------------------------------------------------
 // Read in the contents of the specified file
 // Note: THIS FUNCTION WILL FAIL IF FILESIZE IS > 2^(32-1); about 2GB
function SDUGetFileContent(filename: String; out content: Ansistring): Boolean;
var
  fileHandle: THandle;
  bytesRead:  DWORD;
  fileSize:   ULONGLONG;
  toRead:     DWORD;
begin
  Result := False;

  fileSize   := SDUGetFileSize(filename);
  fileHandle := CreateFile(PChar(filename),
    // pointer to name of the file
    GENERIC_READ,           // access (read-write) mode
    (FILE_SHARE_READ or FILE_SHARE_WRITE),
    // share mode
    nil,                    // pointer to security attributes
    OPEN_EXISTING,          // how to create
    FILE_ATTRIBUTE_NORMAL,  // file attributes
    0                       // handle to file with attributes to copy
    );
  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    // Size buffer so ReadFile(...) can populate it
    content := StringOfChar(Ansichar(#0), filesize);

    toRead := fileSize and $FFFFFFFF;
    ReadFile(
      fileHandle,
      content[1],
      toRead,
      bytesRead,
      nil
      );

    Result := (bytesRead = toRead);

    CloseHandle(fileHandle);

    // In case of error, blow any buffer allocated
    if not (Result) then begin
      content := '';
    end;

  end;  // if (fileHandle<>INVALID_HANDLE_VALUE) then

end;


 // ----------------------------------------------------------------------------
 // Write the contents of the specified file
 // Note: This will overwrite any existing file
function SDUSetFileContent(filename: String; content: String): Boolean;
var
  fileHandle:   THandle;
  bytesWritten: DWORD;
begin
  Result := False;

  fileHandle := CreateFile(PChar(filename),
    // pointer to name of the file
    GENERIC_WRITE,          // access (read-write) mode
    FILE_SHARE_READ,        // share mode
    nil,                    // pointer to security attributes
    CREATE_NEW,             // how to create
    FILE_ATTRIBUTE_NORMAL,  // file attributes
    0                       // handle to file with attributes to copy
    );
  if (fileHandle <> INVALID_HANDLE_VALUE) then begin
    WriteFile(
      fileHandle,
      Content[1],
      length(content),
      bytesWritten,
      nil
      );

    Result := (bytesWritten = DWORD(length(content)));

    CloseHandle(fileHandle);

    // In case of error, delete any file created
    if not (Result) then begin
      DeleteFile(filename);
    end;

  end;  // if (fileHandle<>INVALID_HANDLE_VALUE) then

end;


// ----------------------------------------------------------------------------
function SDUGetLastError(): String;
begin
  Result := SysErrorMessage(GetLastError());
end;

// ----------------------------------------------------------------------------
function SDUDeviceNameForCDROM(CDROMNumber: Cardinal): String;
begin
  Result := Format(FMT_DEVICENAME_CDROM_DEVICE, [CDROMNumber]);
end;
// ----------------------------------------------------------------------------
function SDUDeviceNameForDisk(DiskNumber: Cardinal): String;
begin
  Result := Format(FMT_DEVICENAME_HDD_PHYSICAL_DISK, [DiskNumber]);
end;

// ----------------------------------------------------------------------------
function SDUDeviceNameForDrive(driveLetter: ansichar): String;
begin
  Result := Format(FMT_DEVICENAME_DRIVE_DEVICE, [upcase(driveLetter)]);
end;

 // ----------------------------------------------------------------------------
 // PartitionNo - Set to zero for entire disk, otherwise partition number
function SDUDeviceNameForPartition(DiskNo: Integer; PartitionNo: Integer): String;
begin
  Result := Format(FMT_DEVICENAME_PARTITION_DEVICE, [DiskNo, PartitionNo]);
end;

// ----------------------------------------------------------------------------
function SDUPartitionType(PartitionTypeID: Byte; LongDesc: Boolean): String;
begin
  Result := RS_UNKNOWN;

  if LongDesc then begin
    // Partition types taken (31st May 2008) from:
    //  http://www.win.tue.nl/~aeb/partitions/partition_types-1.html
    case PartitionTypeID of
      $00: Result := 'Empty';
      $01: Result := 'DOS 12-bit FAT';
      $02: Result := 'XENIX root';
      $03: Result := 'XENIX /usr';
      $04: Result := 'DOS 3.0+ 16-bit FAT (up to 32M)';
      $05: Result := 'DOS 3.3+ Extended Partition';
      $06: Result := 'DOS 3.31+ 16-bit FAT (over 32M)';
      $07: Result := 'Windows NT NTFS';  // Most likely of the below
      {
      $07: Result := 'OS/2 IFS (e.g., HPFS)';
      $07: Result := 'Windows NT NTFS';
      $07: Result := 'Advanced Unix';
      $07: Result := 'QNX2.x pre-1988 (see below under IDs 4d-4f)';
      }
      $08: Result := 'OS/2/AIX boot/SplitDrive/Commodore DOS/DELL multispan partition/QNX';
      {
      $08: Result := 'OS/2 (v1.0-1.3 only)';
      $08: Result := 'AIX boot partition';
      $08: Result := 'SplitDrive';
      $08: Result := 'Commodore DOS';
      $08: Result := 'DELL partition spanning multiple drives';
      $08: Result := 'QNX 1.x and 2.x ("qny")';
      }
      $09: Result := 'AIX data/Coherent filesystem/QNX';
      {
      $09: Result := 'AIX data partition';
      $09: Result := 'Coherent filesystem';
      $09: Result := 'QNX 1.x and 2.x ("qnz")';
      }
      $0a: Result := 'OS/2 Boot Manager/Coherent swap/OPUS';
      {
      $0a: Result := 'OS/2 Boot Manager';
      $0a: Result := 'Coherent swap partition';
      $0a: Result := 'OPUS';
      }
      $0b: Result := 'WIN95 OSR2 FAT32';
      $0c: Result := 'WIN95 OSR2 FAT32, LBA-mapped';
      $0e: Result := 'WIN95: DOS 16-bit FAT, LBA-mapped';
      $0f: Result := 'WIN95: Extended partition, LBA-mapped';
      $10: Result := 'OPUS (?)';
      $11: Result := 'Hidden FAT'; // Most useful of the below
      {
      $11: Result := 'Hidden DOS 12-bit FAT';
      $11: Result := 'Leading Edge DOS 3.x logically sectored FAT';
      }
      $12: Result := 'Configuration/diagnostics partition';
      $14: Result := 'Hidden FAT'; // Most useful of the below
      {
      $14: Result := 'Hidden DOS 16-bit FAT <32M';
      $14: Result := 'AST DOS with logically sectored FAT';
      }
      $16: Result := 'Hidden DOS 16-bit FAT >=32M';
      $17: Result := 'Hidden IFS (e.g., HPFS)';
      $18: Result := 'AST SmartSleep Partition';
      $19: Result := 'Unused';
      $1b: Result := 'Hidden WIN95 OSR2 FAT32';
      $1c: Result := 'Hidden WIN95 OSR2 FAT32, LBA-mapped';
      $1e: Result := 'Hidden WIN95 16-bit FAT, LBA-mapped';
      $20: Result := 'Unused';
      $21: Result := 'Reserved/Unused';
      {
      $21: Result := 'Reserved';
      $21: Result := 'Unused';
      }
      $22: Result := 'Unused';
      $23: Result := 'Reserved';
      $24: Result := 'NEC DOS 3.x';
      $26: Result := 'Reserved';
      $27: Result := 'PQservice/Windows RE hidden/MirOS/RouterBOOT';
      {
      $27: Result := 'PQservice';
      $27: Result := 'Windows RE hidden partition';
      $27: Result := 'MirOS partition';
      $27: Result := 'RouterBOOT kernel partition';
      }
      $2a: Result := 'AtheOS File System (AFS)';
      $2b: Result := 'SyllableSecure (SylStor)';
      $31: Result := 'Reserved';
      $32: Result := 'NOS';
      $33: Result := 'Reserved';
      $34: Result := 'Reserved';
      $35: Result := 'JFS on OS/2 or eCS';
      $36: Result := 'Reserved';
      $38: Result := 'THEOS ver 3.2 2gb partition';
      $39: Result := 'Plan 9 partition/THEOS ver 4 spanned';
      {
      $39: Result := 'Plan 9 partition';
      $39: Result := 'THEOS ver 4 spanned partition';
      }
      $3a: Result := 'THEOS ver 4 4gb partition';
      $3b: Result := 'THEOS ver 4 extended partition';
      $3c: Result := 'PartitionMagic recovery partition';
      $3d: Result := 'Hidden NetWare';
      $40: Result := 'Venix 80286';
      $41: Result := 'Linux/MINIX'; // Most likely of the below
      {
      $41: Result := 'Linux/MINIX (sharing disk with DRDOS)';
      $41: Result := 'Personal RISC Boot';
      $41: Result := 'PPC PReP (Power PC Reference Platform) Boot';
      }
      $42: Result := 'Linux swap'; // Most likely of the below
      {
      $42: Result := 'Linux swap (sharing disk with DRDOS)';
      $42: Result := 'SFS (Secure Filesystem)';
      $42: Result := 'Windows 2000 dynamic extended partition marker';
      }
      $43: Result := 'Linux native (sharing disk with DRDOS)';
      $44: Result := 'GoBack partition';
      $45: Result := 'Boot-US boot manager/Priam/EUMEL/Elan';
      {
      $45: Result := 'Boot-US boot manager';
      $45: Result := 'Priam';
      $45: Result := 'EUMEL/Elan';
      }
      $46: Result := 'EUMEL/Elan';
      $47: Result := 'EUMEL/Elan';
      $48: Result := 'EUMEL/Elan';
      $4a: Result := 'Mark Aitchison''s ALFS/THIN';  // AdaOS (Withdrawn)
      {
      $4a: Result := 'Mark Aitchison's ALFS/THIN lightweight filesystem for DOS';
      $4a: Result := 'AdaOS Aquila (Withdrawn)';
      }
      $4c: Result := 'Oberon partition';
      $4d: Result := 'QNX4.x';
      $4e: Result := 'QNX4.x 2nd part';
      $4f: Result := 'QNX4.x 3rd part/Oberon';
      {
      $4f: Result := 'QNX4.x 3rd part';
      $4f: Result := 'Oberon partition';
      }
      $50: Result := 'OnTrack Disk Manager/Lynx RTOS/Native Oberon';
      {
      $50: Result := 'OnTrack Disk Manager (older versions) RO';
      $50: Result := 'Lynx RTOS';
      $50: Result := 'Native Oberon (alt)';
      }
      $51: Result := 'OnTrack Disk Manager RW (DM6 Aux1)/Novell';
      {
      $51: Result := 'OnTrack Disk Manager RW (DM6 Aux1)';
      $51: Result := 'Novell';
      }
      $52: Result := 'CP/M / Microport SysV/AT';
      {
      $52: Result := 'CP/M';
      $52: Result := 'Microport SysV/AT';
      }
      $53: Result := 'Disk Manager 6.0 Aux3';
      $54: Result := 'Disk Manager 6.0 Dynamic Drive Overlay (DDO)';
      $55: Result := 'EZ-Drive';
      $56: Result := 'Golden Bow VFeature/DM converted to EZ-BIOS/AT&T MS-DOS';
      {
      $56: Result := 'Golden Bow VFeature Partitioned Volume.';
      $56: Result := 'DM converted to EZ-BIOS';
      $56: Result := 'AT&T MS-DOS 3.x';
      }
      $57: Result := 'DrivePro/VNDI';
      {
      $57: Result := 'DrivePro';
      $57: Result := 'VNDI Partition';
      }
      $5c: Result := 'Priam EDisk';
      $61: Result := 'SpeedStor';
      $63: Result := 'Unix System V (SCO, ISC Unix, UnixWare, ...), Mach, GNU Hurd';
      $64: Result := 'PC-ARMOUR protected/Novell Netware 286, 2.xx';
      {
      $64: Result := 'PC-ARMOUR protected partition';
      $64: Result := 'Novell Netware 286, 2.xx';
      }
      $65: Result := 'Novell Netware 386, 3.xx or 4.xx';
      $66: Result := 'Novell Netware SMS Partition';
      $67: Result := 'Novell';
      $68: Result := 'Novell';
      $69: Result := 'Novell Netware 5+, Novell Netware NSS Partition';
      $6e: Result := '??';
      $70: Result := 'DiskSecure Multi-Boot';
      $71: Result := 'Reserved';
      $72: Result := 'V7/x86';
      $73: Result := 'Reserved';
      $74: Result := 'Reserved/Scramdisk';
      {
      $74: Result := 'Reserved';
      $74: Result := 'Scramdisk partition';
      }
      $75: Result := 'IBM PC/IX';
      $76: Result := 'Reserved';
      $77: Result := 'M2FS/M2CS / VNDI';
      {
      $77: Result := 'M2FS/M2CS partition';
      $77: Result := 'VNDI Partition';
      }
      $78: Result := 'XOSL FS';
      $7e: Result := 'Unused';
      $7f: Result := 'Unused';
      $80: Result := 'MINIX until 1.4a';
      $81: Result := 'MINIX since 1.4b, early Linux/Mitac disk manager';
      {
      $81: Result := 'MINIX since 1.4b, early Linux';
      $81: Result := 'Mitac disk manager';
      }
      $82: Result := 'Prime/Solaris x86/Linux swap';
      {
      $82: Result := 'Prime';
      $82: Result := 'Solaris x86';
      $82: Result := 'Linux swap';
      }
      $83: Result := 'Linux native partition';
      $84: Result := 'OS/2 hidden C: drive/Hibernation partition';
      {
      $84: Result := 'OS/2 hidden C: drive';
      $84: Result := 'Hibernation partition';
      }
      $85: Result := 'Linux extended partition';
      $86: Result := 'Old Linux RAID superblock/FAT16 volume set';
      {
      $86: Result := 'Old Linux RAID partition superblock';
      $86: Result := 'FAT16 volume set';
      }
      $87: Result := 'NTFS volume set';
      $88: Result := 'Linux plaintext partition table';
      $8a: Result := 'Linux Kernel Partition (used by AiR-BOOT)';
      $8b: Result := 'Legacy Fault Tolerant FAT32 volume';
      $8c: Result := 'Legacy Fault Tolerant FAT32 volume using BIOS extd INT 13h';
      $8d: Result := 'Free FDISK hidden Primary DOS FAT12 partitition';
      $8e: Result := 'Linux Logical Volume Manager partition';
      $90: Result := 'Free FDISK hidden Primary DOS FAT16 partitition';
      $91: Result := 'Free FDISK hidden DOS extended partitition';
      $92: Result := 'Free FDISK hidden Primary DOS large FAT16 partitition';
      $93: Result := 'Hidden Linux native/Amoeba';
      {
      $93: Result := 'Hidden Linux native partition';
      $93: Result := 'Amoeba';
      }
      $94: Result := 'Amoeba bad block table';
      $95: Result := 'MIT EXOPC native partitions';
      $97: Result := 'Free FDISK hidden Primary DOS FAT32 partitition';
      $98: Result := 'Free FDISK hidden Primary DOS FAT32 (LBA)/Datalight ROM-DOS Super-Boot';
      {
      $98: Result := 'Free FDISK hidden Primary DOS FAT32 partitition (LBA)';
      $98: Result := 'Datalight ROM-DOS Super-Boot Partition';
      }
      $99: Result := 'DCE376 logical drive';
      $9a: Result := 'Free FDISK hidden Primary DOS FAT16 partitition (LBA)';
      $9b: Result := 'Free FDISK hidden DOS extended partitition (LBA)';
      $9f: Result := 'BSD/OS';
      $a0: Result := 'Laptop hibernation partition';
      $a1: Result := 'Laptop hibernation partition/HP Volume Expansion (SpeedStor variant)';
      {
      $a1: Result := 'Laptop hibernation partition';
      $a1: Result := 'HP Volume Expansion (SpeedStor variant)';
      }
      $a3: Result := 'HP Volume Expansion (SpeedStor variant)';
      $a4: Result := 'HP Volume Expansion (SpeedStor variant)';
      $a5: Result := 'BSD/386, 386BSD, NetBSD, FreeBSD';
      $a6: Result := 'OpenBSD/HP Volume Expansion (SpeedStor variant)';
      {
      $a6: Result := 'OpenBSD';
      $a6: Result := 'HP Volume Expansion (SpeedStor variant)';
      }
      $a7: Result := 'NeXTStep';
      $a8: Result := 'Mac OS-X';
      $a9: Result := 'NetBSD';
      $aa: Result := 'Olivetti Fat 12 1.44MB Service Partition';
      $ab: Result := 'Mac OS-X Boot partition/GO! partition';
      {
      $ab: Result := 'Mac OS-X Boot partition';
      $ab: Result := 'GO! partition';
      }
      $ae: Result := 'ShagOS filesystem';
      $af: Result := 'ShagOS swap partition/MacOS X HFS';
      {
      $af: Result := 'ShagOS swap partition';
      $af: Result := 'MacOS X HFS';
      }
      $b0: Result := 'BootStar Dummy';
      $b1: Result := 'HP Volume Expansion (SpeedStor variant)';
      $b3: Result := 'HP Volume Expansion (SpeedStor variant)';
      $b4: Result := 'HP Volume Expansion (SpeedStor variant)';
      $b6: Result :=
          'HP Volume Expansion (SpeedStor variant)/Corrupted Windows NT mirror set (master), FAT16 file system';
      {
      $b6: Result := 'HP Volume Expansion (SpeedStor variant)';
      $b6: Result := 'Corrupted Windows NT mirror set (master), FAT16 file system';
      }
      $b7: Result :=
          'Corrupted Windows NT mirror set (master), NTFS file system/BSDI BSD/386 filesystem';
      {
      $b7: Result := 'Corrupted Windows NT mirror set (master), NTFS file system';
      $b7: Result := 'BSDI BSD/386 filesystem';
      }
      $b8: Result := 'BSDI BSD/386 swap partition';
      $bb: Result := 'Boot Wizard hidden';
      $bc: Result := 'Acronis backup partition';
      $be: Result := 'Solaris 8 boot partition';
      $bf: Result := 'New Solaris x86 partition';
      $c0: Result := 'CTOS/REAL/32 secure/NTFT/DR-DOS/Novell DOS secured';
      {
      $c0: Result := 'CTOS';
      $c0: Result := 'REAL/32 secure small partition';
      $c0: Result := 'NTFT Partition';
      $c0: Result := 'DR-DOS/Novell DOS secured partition';
      }
      $c1: Result := 'DRDOS/secured (FAT-12)';
      $c2: Result := 'Unused/Hidden Linux';
      {
      $c2: Result := 'Unused';
      $c2: Result := 'Hidden Linux';
      }
      $c3: Result := 'Hidden Linux swap';
      $c4: Result := 'DRDOS/secured (FAT-16, < 32M)';
      $c5: Result := 'DRDOS/secured (extended)';
      $c6: Result := 'DRDOS/secured (FAT-16, >= 32M)/Windows NT corrupted FAT16 volume/stripe set';
      {
      $c6: Result := 'DRDOS/secured (FAT-16, >= 32M)';
      $c6: Result := 'Windows NT corrupted FAT16 volume/stripe set';
      }
      $c7: Result := 'Windows NT corrupted NTFS volume/stripe set / Syrinx boot';
      {
      $c7: Result := 'Windows NT corrupted NTFS volume/stripe set';
      $c7: Result := 'Syrinx boot';
      }
      $c8: Result := 'Reserved for DR-DOS 8.0+';
      $c9: Result := 'Reserved for DR-DOS 8.0+';
      $ca: Result := 'Reserved for DR-DOS 8.0+';
      $cb: Result := 'DR-DOS 7.04+ secured FAT32 (CHS)/';
      $cc: Result := 'DR-DOS 7.04+ secured FAT32 (LBA)/';
      $cd: Result := 'CTOS Memdump?';
      $ce: Result := 'DR-DOS 7.04+ FAT16X (LBA)/';
      $cf: Result := 'DR-DOS 7.04+ secured EXT DOS (LBA)/';
      $d0: Result := 'REAL/32 secure big/Multiuser DOS secured';
      {
      $d0: Result := 'REAL/32 secure big partition';
      $d0: Result := 'Multiuser DOS secured partition';
      }
      $d1: Result := 'Old Multiuser DOS secured FAT12';
      $d4: Result := 'Old Multiuser DOS secured FAT16 <32M';
      $d5: Result := 'Old Multiuser DOS secured extended partition';
      $d6: Result := 'Old Multiuser DOS secured FAT16 >=32M';
      $d8: Result := 'CP/M-86';
      $da: Result := 'Non-FS Data/Powercopy Backup';
      {
      $da: Result := 'Non-FS Data';
      $da: Result := 'Powercopy Backup';
      }
      $db: Result :=
          'Digital Research CP/M, Concurrent CP/M, Concurrent DOS / CTOS / KDG Telemetry SCPU boot';
      {
      $db: Result := 'Digital Research CP/M, Concurrent CP/M, Concurrent DOS';
      $db: Result := 'CTOS (Convergent Technologies OS -Unisys)';
      $db: Result := 'KDG Telemetry SCPU boot';
      }
      $dd: Result := 'Hidden CTOS Memdump?';
      $de: Result := 'Dell PowerEdge Server utilities (FAT fs)';
      $df: Result := 'DG/UX virtual disk manager partition / BootIt EMBRM';
      {
      $df: Result := 'DG/UX virtual disk manager partition';
      $df: Result := 'BootIt EMBRM';
      }
      $e0: Result := 'Reserved by STMicroelectronics for a filesystem called ST AVFS';
      $e1: Result := 'DOS access or SpeedStor 12-bit FAT extended partition';
      $e3: Result := 'DOS R/O or SpeedStor';
      $e4: Result := 'SpeedStor 16-bit FAT extended partition < 1024 cyl.';
      $e5: Result := 'Tandy MSDOS with logically sectored FAT';
      $e6: Result := 'Storage Dimensions SpeedStor';
      $e8: Result := 'LUKS';
      $eb: Result := 'BeOS BFS';
      $ec: Result := 'SkyOS SkyFS';
      $ed: Result := 'Unused';
      $ee: Result := 'Indication that this legacy MBR is followed by an EFI header';
      $ef: Result := 'Partition that contains an EFI file system';
      $f0: Result := 'Linux/PA-RISC boot loader';
      $f1: Result := 'Storage Dimensions SpeedStor';
      $f2: Result := 'DOS 3.3+ secondary partition';
      $f3: Result := 'Reserved';
      $f4: Result := 'SpeedStor large partition/Prologue single-volume partition';
      {
      $f4: Result := 'SpeedStor large partition';
      $f4: Result := 'Prologue single-volume partition';
      }
      $f5: Result := 'Prologue multi-volume partition';
      $f6: Result := 'Storage Dimensions SpeedStor';
      $f7: Result := 'Unused';
      $f9: Result := 'pCache';
      $fa: Result := 'Bochs';
      $fb: Result := 'VMware File System partition';
      $fc: Result := 'VMware Swap partition';
      $fd: Result := 'Linux raid partition with autodetect using persistent superblock';
      $fe: Result :=
          'SpeedStor > 1024 cyl./LANstep/IBM PS/2 IML/Windows NT Disk Administrator hidden partition/Linux Logical Volume Manager partition (old)';
      {
      $fe: Result := 'SpeedStor > 1024 cyl.';
      $fe: Result := 'LANstep';
      $fe: Result := 'IBM PS/2 IML (Initial Microcode Load) partition,';
      $fe: Result := 'Windows NT Disk Administrator hidden partition';
      $fe: Result := 'Linux Logical Volume Manager partition (old)';
      }
      $ff: Result := 'Xenix Bad Block Table';
    end;
  end else begin
    // Partition types taken (31st May 2008) from:
    //   Linux fdisk v1.0 
    case PartitionTypeID of
      $00: Result := 'Empty';
      $01: Result := 'FAT12';
      $02: Result := 'XENIX root';
      $03: Result := 'XENIX usr';
      $04: Result := 'Small FAT16';
      $05: Result := 'Extended';
      $06: Result := 'FAT16';
      $07: Result := 'HPFS/NTFS';
      $08: Result := 'AIX';
      $09: Result := 'AIX bootable';
      $0a: Result := 'OS/2 boot mgr';
      $0b: Result := 'FAT32';
      $0c: Result := 'FAT32 LBA';
      $0e: Result := 'FAT16 LBA';
      $0f: Result := 'Extended LBA';
      $10: Result := 'OPUS';
      $11: Result := 'Hidden FAT12';
      $12: Result := 'Compaq diag';
      $14: Result := 'Hidd Sm FAT16';
      $16: Result := 'Hidd FAT16';
      $17: Result := 'Hidd HPFS/NTFS';
      $18: Result := 'AST SmartSleep';
      $1b: Result := 'Hidd FAT32';
      $1c: Result := 'Hidd FAT32 LBA';
      $1e: Result := 'Hidd FAT16 LBA';
      $24: Result := 'NEC DOS';
      $39: Result := 'Plan 9';
      $3c: Result := 'PMagic recovery';
      $40: Result := 'Venix 80286';
      $41: Result := 'PPC PReP Boot';
      $42: Result := 'SFS';
      $4d: Result := 'QNX4.x';
      $4e: Result := 'QNX4.x 2nd part';
      $4f: Result := 'QNX4.x 3rd part';
      $50: Result := 'OnTrack DM';
      $51: Result := 'OnTrackDM6 Aux1';
      $52: Result := 'CP/M';
      $53: Result := 'OnTrackDM6 Aux3';
      $54: Result := 'OnTrack DM6';
      $55: Result := 'EZ Drive';
      $56: Result := 'Golden Bow';
      $5c: Result := 'Priam Edisk';
      $61: Result := 'SpeedStor';
      $63: Result := 'GNU HURD/SysV';
      $64: Result := 'Netware 286';
      $65: Result := 'Netware 386';
      $70: Result := 'DiskSec MltBoot';
      $75: Result := 'PC/IX';
      $80: Result := 'Minix <1.4a';
      $81: Result := 'Minix >1.4b';
      $82: Result := 'Linux swap';
      $83: Result := 'Linux';
      $84: Result := 'OS/2 hidden C:';
      $85: Result := 'Linux extended';
      $86: Result := 'NTFS volume set';
      $87: Result := 'NTFS volume set';
      $88: Result := 'Linux plaintext';
      $8e: Result := 'Linux LVM';
      $93: Result := 'Amoeba';
      $94: Result := 'Amoeba BBT';
      $9f: Result := 'BSD/OS';
      $a0: Result := 'Thinkpad hib';
      $a5: Result := 'FreeBSD';
      $a6: Result := 'OpenBSD';
      $a7: Result := 'NeXTSTEP';
      $a8: Result := 'Darwin UFS';
      $a9: Result := 'NetBSD';
      $ab: Result := 'Darwin boot';
      $b7: Result := 'BSDI fs';
      $b8: Result := 'BSDI swap';
      $bb: Result := 'Boot Wizard Hid';
      $be: Result := 'Solaris boot';
      $bf: Result := 'Solaris';
      $c1: Result := 'DRDOS/2 FAT12';
      $c4: Result := 'DRDOS/2 smFAT16';
      $c6: Result := 'DRDOS/2 FAT16';
      $c7: Result := 'Syrinx';
      $da: Result := 'Non-FS data';
      $db: Result := 'CP/M / CTOS';
      $de: Result := 'Dell Utility';
      $df: Result := 'BootIt';
      $e1: Result := 'DOS access';
      $e3: Result := 'DOS R/O';
      $e4: Result := 'SpeedStor';
      $eb: Result := 'BeOS fs';
      $ee: Result := 'EFI GPT';
      $ef: Result := 'EFI FAT';
      $f0: Result := 'Lnx/PA-RISC bt';
      $f1: Result := 'SpeedStor';
      $f2: Result := 'DOS secondary';
      $f4: Result := 'SpeedStor';
      $fd: Result := 'Lnx RAID auto';
      $fe: Result := 'LANstep';
      $ff: Result := 'XENIX BBT';
    end;
  end;

end;

// ----------------------------------------------------------------------------
function SDUIntToHex(val: ULONGLONG; digits: Integer): String;
var
  tmpVal: ULONGLONG;
  LSB:    Integer;
begin
  Result := '';

  tmpVal := val;
  while (tmpVal > 0) do begin
    lsb    := tmpVal and $FF;
    Result := inttohex(LSB, 2) + Result;
    tmpVal := tmpVal shr 8;
  end;

  if (length(Result) < digits) then begin
    Result := StringOfChar('0', (digits - length(Result))) + Result;
  end;

end;

 // ----------------------------------------------------------------------------
 // Delphi's inttostr(...) function truncates 64 bit values to 32 bits
function SDUIntToStr(val: Int64): String;
var
  tmpVal:  Int64;
  LSB:     Integer;
  tmpZero: Int64;
begin
  Result := '';

  tmpZero := 0;
  if (val = tmpZero) then begin
    Result := '0';
  end else begin
    tmpVal := val;
    while (tmpVal > 0) do begin
      lsb    := tmpVal mod 10;
      Result := IntToStr(LSB) + Result;
      tmpVal := tmpVal div 10;
    end;
  end;

end;

{$IF CompilerVersion >= 18.5}// See comment on ULONGLONG definition
function SDUIntToStr(val: ULONGLONG): String;
var
  tmpVal:  ULONGLONG;
  LSB:     Integer;
  tmpZero: ULONGLONG;
begin
  Result := '';

  tmpZero := 0;
  if (val = tmpZero) then begin
    Result := '0';
  end else begin
    tmpVal := val;
    while (tmpVal > 0) do begin
      lsb    := tmpVal mod 10;
      Result := IntToStr(LSB) + Result;
      tmpVal := tmpVal div 10;
    end;
  end;

end;

{$IFEND}


 // ----------------------------------------------------------------------------
 // As SDUIntToStr, but with thousands seperators inserted
function SDUIntToStrThousands(val: Int64): String;
var
  sVal: String;
  i:    Integer;
  ctr:  Integer;
begin
  sVal := SDUIntToStr(val);

  Result := '';
  ctr    := 0;
  for i := length(sVal) downto 1 do begin
    Inc(ctr);
    if ((ctr <> 1) and ((ctr mod 3) = 1)) then begin
      // ThousandSeparator from SysUtils
      Result := FormatSettings.ThousandSeparator + Result;
    end;

    Result := sVal[i] + Result;
  end;

end;

{$IF CompilerVersion >= 18.5}// See comment on ULONGLONG definition
function SDUIntToStrThousands(val: ULONGLONG): String;
var
  sVal: String;
  i:    Integer;
  ctr:  Integer;
begin
  sVal := SDUIntToStr(val);

  Result := '';
  ctr    := 0;
  for i := length(sVal) downto 1 do begin
    Inc(ctr);
    if ((ctr <> 1) and ((ctr mod 3) = 1)) then begin
      // ThousandSeparator from SysUtils
      Result := FormatSettings.ThousandSeparator + Result;
    end;

    Result := sVal[i] + Result;
  end;

end;

{$IFEND}

 // ----------------------------------------------------------------------------
 // Populate the specified TComboBox with a list of removable drives
 // Note: This will clear any existing items in the TComboBox
procedure SDUPopulateRemovableDrives(cbDrive: TComboBox);
var
  DriveType: TDriveType;
  drive:     DriveLetterChar;
  item:      String;
  volTitle:  String;
begin
  cbDrive.Items.Clear();

  // Skip "A" and "B"; typically floppy drives
  // Note: *Uppercase* letters
  for drive := 'C' to 'Z' do begin
    DriveType := TDriveType(GetDriveType(PwideChar(drive + ':\')));

    if ((DriveType = dtFloppy) or (DriveType = dtUnknown)) then begin
      item := drive + ':';

      volTitle := SDUVolumeID(drive);
      if (volTitle <> '') then begin
        item := item + ' [' + volTitle + ']';
      end;

      cbDrive.Items.Add(item);
    end;

  end;

end;

 // ----------------------------------------------------------------------------
 // CopyFile(...), but using Windows API to display "flying files" dialog while copying
function SDUFileCopy(srcFilename: String; destFilename: String): Boolean;
var
  fileOpStruct: TSHFileOpStruct;
begin
  Result                     := False;
  // srcFilename and destFilename *MUST* end in a double NULL for
  // SHFileOperation to operate correctly
  srcFilename                := srcFilename + #0 + #0;
  destFilename               := destFilename + #0 + #0;
  fileOpStruct.Wnd           := 0;
  fileOpStruct.wFunc         := FO_COPY;
  fileOpStruct.pFrom         := PChar(srcFilename);
  fileOpStruct.pTo           := PChar(destFilename);
  fileOpStruct.fFlags        := (FOF_NOCONFIRMATION or FOF_NOCONFIRMMKDIR);
  fileOpStruct.fAnyOperationsAborted := False;
  fileOpStruct.hNameMappings := nil;
  fileOpStruct.lpszProgressTitle := nil;

  if (SHFileOperation(fileOpStruct) = 0) then begin
    Result := not (fileOpStruct.fAnyOperationsAborted);
  end;

end;

 // ----------------------------------------------------------------------------
 // Get a description of the type of file passed in
 // knownFiletype - This will be set to TRUE/FALSE, depending on whether it's a
 //                 known filetype or not
 // Returns MS Explorer-style "<uppercase extension> File" if none found
function SDUGetFileType_Description(const filename: String): String;
var
  junk: Boolean;
begin
  Result := SDUGetFileType_Description(filename, junk);
end;

function SDUGetFileType_Description(const filename: String; out knownFiletype: Boolean): String;
var
  extn:     String;
  registry: TRegistry;
begin
  Result        := '';
  knownFiletype := False;

  extn := ExtractFileExt(filename);

  if (filename = FILE_TYPE_DIRECTORY) then begin
    extn := DIRECTORY_TYPE;
  end;

  registry := TRegistry.Create(KEY_READ);
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    if registry.OpenKeyReadOnly('\' + extn) then begin
      Result := registry.ReadString('');
      registry.CloseKey;

      // Some store a redirect to the main file type here
      if registry.OpenKeyReadOnly('\' + Result) then begin
        Result := registry.ReadString('');
        registry.CloseKey;
      end;

    end;

  finally
    registry.Free();
  end;

  // If couldn't get details, try again with lowercase
  //  - IF NOT ALREADY TRYING THIS!
  if (Result = '') then begin
    if (lowercase(filename) <> filename) then begin
      Result := SDUGetFileType_Description(lowercase(filename), knownFiletype);
    end else begin
      // Strip out "." from extension
      extn   := StringReplace(extn, '.', '', [rfReplaceAll]);
      Result := SDUParamSubstitute(_('%1 File'), [uppercase(extn)]);
    end;
  end else begin
    // Found in calling *this* routine.
    // Note: May also be set in recursive call to SDUGetFileType_Description(...)
    knownFiletype := True;
  end;

end;

 // ----------------------------------------------------------------------------
 // Returns FALSE if unable to find icon
function SDUGetFileType_Icon(filename: String; out iconFilename: String;
  out iconIdx: Integer): Boolean;
var
  extn:       String;
  registry:   TRegistry;
  typeName:   String;
  typeIcon:   String;
  iconIdxStr: String;
begin
  iconIdx := 0;

  extn := ExtractFileExt(filename);

  if (filename = FILE_TYPE_DIRECTORY) then begin
    extn := DIRECTORY_TYPE;
  end;

  registry := TRegistry.Create(KEY_READ);
  try
    registry.RootKey := HKEY_CLASSES_ROOT;
    typeIcon         := '';
    if registry.OpenKeyReadOnly('\' + extn + '\DefaultIcon') then begin
      typeIcon := registry.ReadString('');
      registry.CloseKey;
    end;

    if (typeIcon = '') then begin
      if registry.OpenKeyReadOnly('\' + extn) then begin
        typeName := registry.ReadString('');
        registry.CloseKey;

        // Some store a redirect to the main file type here
        if registry.OpenKeyReadOnly('\' + typeName + '\DefaultIcon') then begin
          typeIcon := registry.ReadString('');
          registry.CloseKey;
        end;
      end;

    end;

  finally
    registry.Free();
  end;

  if SDUSplitString(typeIcon, iconFilename, iconIdxStr, ',') then begin
    if not (TryStrToInt(iconIdxStr, iconIdx)) then begin
      iconIdx := 0;
    end;
  end;

  // Strip off any quotes around the filename (may be present if it's got
  // spaces in the path/filename)
  if (iconFilename <> '') then begin
    iconFilename := trim(iconFilename);
    iconFilename := StringReplace(iconFilename, '"', '', [rfReplaceAll]);
  end;

  // If couldn't get details, try again with lowercase
  //  - IF NOT ALREADY TRYING THIS!
  if ((iconFilename = '') and (lowercase(filename) <> filename)) then begin
    SDUGetFileType_Icon(lowercase(filename), iconFilename, iconIdx);
  end;

  Result := (iconFilename <> '');
end;


// ----------------------------------------------------------------------------
{ TODO 1 -otdk -cclean : cant see much advantage over 'format' (also cant escape %) - so replace with Format() where pos }
{replaces %1 etc in string with params - can repeat eg %1 twice}
function SDUParamSubstitute(const formatStr: String; const params: array of Variant): String;
var
  i: Integer;
begin
  Result := formatStr;

  // Do in "reverse" order in order to process %10 before %1
  for i := (length(params) - 1) downto 0 do
    Result := StringReplace(Result, '%' + IntToStr(i + 1), params[i], [rfReplaceAll]);
end;

function SDUIsOddNumber(Value: Integer): Boolean;
begin
  Result := ((Value and 1) <> 0);
end;

// ----------------------------------------------------------------------------
function SDUIsEvenNumber(Value: Integer): Boolean;
begin
  Result := ((Value and 1) = 0);
end;


 // ----------------------------------------------------------------------------
 //function SDUTryStrToInt(const S: String; out Value: Integer): Boolean;
 //begin
 //  Result := TryStrToInt(S, Value);
 //end;

function SDUTryStrToInt(const S: String; out Value: Uint64): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

{$IFDEF VER185} // See comment on ULONGLONG definition
function SDUTryStrToInt(const S: string; out Value: ULONGLONG): boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;
{$ENDIF}

 // ----------------------------------------------------------------------------
 // As TryStrToInt, but if it fails to convert the string, return the default
 // value passed in
function SDUTryStrToIntDflt(Value: String; deflt: Integer): Integer;
begin
  if not (TryStrToInt(Value, Result)) then begin
    Result := deflt;
  end;

end;

// ----------------------------------------------------------------------------
function SDUIntToBin(Value: Integer; digits: Integer = 8): String;
begin
  Result := SDUIntToBinary(Value, digits);
end;

// ----------------------------------------------------------------------------
function SDUIntToBinary(Value: Integer; digits: Integer = 8): String;
var
  currBit: Char;
begin
  Result := '';
  while ((length(Result) < digits) or (Value > 0)) do begin
    currBit := '0';
    if ((Value and 1) > 0) then begin
      currBit := '1';
    end;

    Result := currBit + Result;

    Value := (Value shr 1);
  end;

end;

function SDUUnitsStorageToText(units: TUnits_Storage): String;
begin
  Result := RS_UNKNOWN;

  case units of
    usBytes: Result := UNITS_STORAGE_BYTES;
    usKB: Result    := UNITS_STORAGE_KB;
    usMB: Result    := UNITS_STORAGE_MB;
    usGB: Result    := UNITS_STORAGE_GB;
    usTB: Result    := UNITS_STORAGE_TB;
  end;

end;

function SDUUnitsStorageToTextArr(): TSDUArrayString;
var
  i: TUnits_Storage;
begin
  SetLength(Result, 0);
  for i := low(i) to high(i) do begin
    SetLength(Result, length(Result) + 1);
    Result[length(Result) - 1] := SDUUnitsStorageToText(i);
  end;

end;

function UNITS_BYTES_DENOMINATINON(): TSDUArrayString;
begin
  Result := SDUUnitsStorageToTextArr();
end;

// ----------------------------------------------------------------------------
function SDUWideStringOfWideChar(Ch: Widechar; Count: Integer): WideString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Count do begin
    Result := Result + Ch;
  end;

end;


 // ----------------------------------------------------------------------------
 // Taken from: http://www.swissdelphicenter.ch/en/showcode.php?id=1509
function SDUSelectDirectory(hOwn: HWND; Caption: String; Root: String;
  var Path: String; uFlag: DWORD = $25): Boolean;
const
  BIF_NEWDIALOGSTYLE = $0040;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Dummy: Longword;

  function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam: Cardinal;
    lpData: Cardinal): Integer; stdcall;
  var
    PathName: array[0..MAX_PATH] of Char;
  begin
    case uMsg of
      BFFM_INITIALIZED:
        SendMessage(Hwnd, BFFM_SETSELECTION, Ord(True), Integer(lpData));
      BFFM_SELCHANGED:
      begin
        SHGetPathFromIDList(PItemIDList(lParam), @PathName);
        SendMessage(hwnd, BFFM_SETSTATUSTEXT, 0, Longint(PChar(@PathName)));
      end;
    end;
    Result := 0;
  end;

begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(hOwn, nil, POleStr(WideString(Root)),
          Dummy, RootItemIDList, Dummy);
      end;
      with BrowseInfo do begin
        hwndOwner      := hOwn;
        pidlRoot       := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle      := PChar(Caption);
        ulFlags        := uFlag;
        lpfn           := @BrowseCallbackProc;
        lParam         := Integer(PChar(Path));
      end;
      ItemIDList := ShBrowseForFolder(BrowseInfo);
      Result     := (ItemIDList <> nil);
      if Result then begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Path := StrPas(Buffer);
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;

  if Result then begin
    Result := (Path <> '');
  end;

end;


 // ----------------------------------------------------------------------------
 // Convert from big-endian to little-endian, and vice versa
function SDUConvertEndian(const x: Word): Word;
begin
  Result := (((x and $00FF) shr 8) + ((x and $FF00) shl 8));
end;

function SDUConvertEndian(const x: DWORD): DWORD;
begin
  Result := (((x and $000000FF) shr 24) + ((x and $0000FF00) shr 8) +
    ((x and $00FF0000) shl 8) + ((x and $FF000000) shl 24));
end;


// ----------------------------------------------------------------------------
function SDUGetPADFileVersionInfo_XML(XML: String;
  var majorVersion, minorVersion, revisionVersion, buildVersion: Integer): Boolean;
const
  XML_NODE_PROGRAM_NAME    = '/XML_DIZ_INFO/Program_Info/Program_Name';
  XML_NODE_PROGRAM_VERSION = '/XML_DIZ_INFO/Program_Info/Program_Version';
var
  doc: TXMLDocument;

  iXml:          IDOMDocument;
  iNode:         IDOMNode;
  DOMNodeSelect: IDOMNodeSelect;
  iNodeEx:       IDOMNodeEx;

  //  padAppID: string;

  i:          Integer;
  versionStr: String;
  stlVersion: TStringList;
begin
  doc := TXMLDocument.Create(nil);
  try
    doc.XML.Add(XML);
    doc.Active := True;

    iXml := doc.DOMDocument;
    iXml.QueryInterface(IDOMNodeSelect, DOMNodeSelect);

{
    iNode := DOMNodeSelect.selectNode(XML_NODE_PROGRAM_NAME);
    iNodeEx := GetDOMNodeEx(iNode);
    padAppID := iNodeEx.text;
}

    iNode      := DOMNodeSelect.selectNode(XML_NODE_PROGRAM_VERSION);
    iNodeEx    := GetDOMNodeEx(iNode);
    versionStr := iNodeEx.Text;

    stlVersion := TStringList.Create();
    try
      stlVersion.Delimiter     := '.';
      stlVersion.DelimitedText := versionStr;

      Result := (stlVersion.Count > 0);

      majorVersion    := 0;
      minorVersion    := 0;
      revisionVersion := 0;
      buildVersion    := 0;
      for i := 0 to (stlVersion.Count - 1) do begin
        stlVersion[i] := trim(stlVersion[i]);

        case i of
          0: Result := TryStrToInt(stlVersion[i], majorVersion);
          1: Result := TryStrToInt(stlVersion[i], minorVersion);
          2: Result := TryStrToInt(stlVersion[i], revisionVersion);
          3: Result := TryStrToInt(stlVersion[i], buildVersion);
        end;

        if not (Result) then begin
          break;
        end;

      end;

    finally
      stlVersion.Free();
    end;

  finally
    doc.Free();
  end;

end;


// ----------------------------------------------------------------------------
function SDUGetPADFileVersionInfoString_XML(XML: String): String;
var
  majorVersion:    Integer;
  minorVersion:    Integer;
  revisionVersion: Integer;
  buildVersion:    Integer;
begin
  Result := '';
  if SDUGetPADFileVersionInfo_XML(XML, majorVersion, minorVersion, revisionVersion,
    buildVersion) then begin
    Result := SDUVersionInfoToString(majorVersion, minorVersion, revisionVersion,
      buildVersion, -1);
  end;

end;


// ----------------------------------------------------------------------------
function SDUGetPADFileVersionInfo(url: String; var majorVersion, minorVersion: Integer;
  userAgent: WideString = DEFAULT_HTTP_USERAGENT; ShowProgressDlg: Boolean = True): TTimeoutGet;
var
  junk: Integer;
begin
  Result := SDUGetPADFileVersionInfo(url, majorVersion, minorVersion, junk,
    junk, userAgent, ShowProgressDlg);
end;

function SDUGetPADFileVersionInfo(url: String;
  var majorVersion, minorVersion, revisionVersion, buildVersion: Integer;
  userAgent: WideString = DEFAULT_HTTP_USERAGENT; ShowProgressDlg: Boolean = True): TTimeoutGet;
var
  xml: String;
begin

{$IFDEF FORCE_LOCAL_PAD}
     xml     := TFile.ReadAllText(url);
      Result := tgOK;
   {$ELSE}
  Result := tgFailure;
  if ShowProgressDlg then begin
    Result := SDUGetURLProgress_WithUserAgent(_('Checking for latest version...'),
      url, xml, userAgent);
  end else begin
    if SDUWinHTTPRequest_WithUserAgent(url, userAgent, xml) then begin
      Result := tgOK;
    end;
  end;

   {$ENDIF}
  if (Result = tgOK) then begin
    if not (SDUGetPADFileVersionInfo_XML(xml, majorVersion, minorVersion,
      revisionVersion, buildVersion)) then begin
      Result := tgFailure;
    end;

  end;

end;


// ----------------------------------------------------------------------------
function SDUGetPADFileVersionInfoString(url: String): String;
var
  xml: String;
begin
  Result := '';
  if SDUWinHTTPRequest(url, xml) then
    Result := SDUGetPADFileVersionInfoString_XML(xml);
end;


 // ----------------------------------------------------------------------------
 // Encode date to YYYY-MM-DD format
function SDUTDateToISO8601(inDate: TDate): String;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(inDate, AYear, AMonth, ADay);
  Result := Format('%.4d-%.2d-%.2d', [AYear, AMonth, ADay]);
end;

// Encode date to HH:MM format
function SDUTTimeToISO8601(inTime: TTime; includeSeconds: Boolean = True): String;
var
  AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeTime(inTime, AHour, AMinute, ASecond, AMilliSecond);
  Result := Format('%.2d:%.2d', [AHour, AMinute]);
  if includeSeconds then
    Result := Result + Format(':%.2d', [ASecond]);
end;

// Encode date to YYYY-MM-DDTHH:MM format
function SDUTDateTimeToISO8601(inDateTime: TDateTime; includeSeconds: Boolean = True): String;
begin
  Result := SDUTDateToISO8601(inDateTime) + 'T' + SDUTTimeToISO8601(inDateTime, includeSeconds);
end;

 // Decode date from any of the following formats:
 //   YYYY-MM-DD
 //   YYYYMMDD
function SDUISO8601ToTDate(inDate: String): TDate;
var
  AYear, AMonth, ADay: Integer;
begin
  AYear  := 1900;
  AMonth := 1;
  ADay   := 1;

  if (Pos('-', inDate) > 0) then begin
    TryStrToInt(Copy(inDate, 1, 4), AYear);
    TryStrToInt(Copy(inDate, 6, 2), AMonth);
    TryStrToInt(Copy(inDate, 9, 2), ADay);
  end else begin
    TryStrToInt(Copy(inDate, 1, 4), AYear);
    TryStrToInt(Copy(inDate, 5, 2), AMonth);
    TryStrToInt(Copy(inDate, 7, 2), ADay);
  end;

  Result := EncodeDate(AYear, AMonth, ADay);
end;

 // Decode time from any of the following formats;
 //   HH
 //   HHMM
 //   HHMMSS
 //   HH:MM
 //   HH:MM:SS
function SDUISO8601ToTTime(inTime: String): TTime;
var
  AHour, AMinute, ASecond, AMilliSecond: Integer;
  junkInt: Integer;
begin
  AHour        := 0;
  AMinute      := 0;
  ASecond      := 0;
  AMilliSecond := 0;

  // If hours only:
  //   HH
  if (length(inTime) <= 0) then begin
    // Empty string passed in - already set to 0's, so do nothing here
  end else
  if (length(inTime) <= 2) then begin
    TryStrToInt(inTime, AHour);
  end // If we have ":" separators betweem the elements, split accordingly
  else
  if (Pos(':', inTime) > 0) then begin
    // Either:
    //   HH:MM
    //   HH:MM:SS
    TryStrToInt(Copy(inTime, 1, 2), AHour);
    TryStrToInt(Copy(inTime, 4, 2), AMinute);

    // Check for seconds (HH:MM:SS)
    if (length(inTime) >= 8) then begin
      // Sanity check in case timezone information has been included, but
      // seconds omitted
      if (inTime[6] = ':') then begin
        TryStrToInt(Copy(inTime, 7, 2), ASecond);
      end;
    end;
  end else begin
    // Either:
    //   HHMM
    //   HHMMSS
    TryStrToInt(Copy(inTime, 1, 2), AHour);
    TryStrToInt(Copy(inTime, 3, 2), AMinute);

    // Check for seconds (HHMMSS)
    if (length(inTime) >= 6) then begin
      // Sanity check in case timezone information has been included, but
      // seconds omitted
      if (TryStrToInt(inTime[5], junkInt) and TryStrToInt(inTime[5], junkInt)) then begin
        TryStrToInt(Copy(inTime, 5, 2), ASecond);
      end;
    end;
  end;

  // Handle special case of midnight being represented as 24:00
  if (AHour = 24) then begin
    AHour := 0;
  end;

  Result := EncodeTime(AHour, AMinute, ASecond, AMilliSecond);
end;

 // Decode date/time from any of:
 //   YYYY-MM-DDTHH:MM
 //   YYYY-MM-DDTHH:MM:SS
 //   YYYY-MM-DDTHHMM
 //   YYYY-MM-DDTHHMMSS
 //   YYYYMMDDTHH:MM
 //   YYYYMMDDTHH:MM:SS
 //   YYYYMMDDTHHMM
 //   YYYYMMDDTHHMMSS
 //   YYYY-MM-DD HH:MM
 //   YYYY-MM-DD HH:MM:SS
 //   YYYY-MM-DD HHMM
 //   YYYY-MM-DD HHMMSS
 //   YYYYMMDD HH:MM
 //   YYYYMMDD HH:MM:SS
 //   YYYYMMDD HHMM
 //   YYYYMMDD HHMMSS
 // formats
function SDUISO8601ToTDateTime(inDateTime: String): TDateTime;
var
  date:   String;
  time:   String;
  sepPos: Integer;
begin
  date := '1900-01-01';
  time := '00:00';

  // Search for a "T" or " " acting as a date/time separator
  sepPos := Pos('T', inDateTime);
  if (sepPos = 0) then begin
    sepPos := Pos(' ', inDateTime);
  end;
  if (sepPos > 0) then begin
    // Date/time
    date := Copy(inDateTime, 1, sepPos - 1);
    time := Copy(inDateTime, sepPos + 1, (length(inDateTime) - sepPos));
  end // Not a date and time - determine if it's date-only, or time-only
  else
  if (Pos(':', inDateTime) > 0) then begin
    // Time only
    time := inDateTime;
  end else begin
    // Date only
    date := inDateTime;
  end;

  Result := SDUISO8601ToTDate(date) + SDUISO8601ToTTime(time);
end;


// ----------------------------------------------------------------------------
function _SDUVersionNumberCompare(A, B: Integer): Integer;
begin
  Result := 0;
  if (A > B) then
    Result := -1;
  if (B > A) then
    Result := 1;
end;

 // Check version IDs
 // Returns:
 //   -1 if A is later
 //   0 if they are the same
 //   1 if B is later
function SDUVersionCompare(A_MajorVersion, A_MinorVersion: Integer;
  B_MajorVersion, B_MinorVersion: Integer): Integer;
begin
  Result := _SDUVersionNumberCompare(A_MajorVersion, B_MajorVersion);
  if (Result = 0) then
    Result := _SDUVersionNumberCompare(A_MinorVersion, B_MinorVersion);
end;

function SDUVersionCompare(A_MajorVersion, A_MinorVersion, A_RevisionVersion,
  A_BuildVersion: Integer; B_MajorVersion, B_MinorVersion, B_RevisionVersion,
  B_BuildVersion: Integer): Integer;
begin
  Result := SDUVersionCompare(A_MajorVersion, A_MinorVersion, B_MajorVersion, B_MinorVersion);

  if (Result = 0) then begin
    Result := _SDUVersionNumberCompare(A_RevisionVersion, B_RevisionVersion);
  end;
  if (Result = 0) then begin
    Result := _SDUVersionNumberCompare(A_BuildVersion, B_BuildVersion);
  end;
end;


function SDUVersionCompareWithBetaFlag(A_MajorVersion, A_MinorVersion: Integer;
  A_BetaVersion: Integer; B_MajorVersion, B_MinorVersion: Integer): Integer;
begin
  Result := SDUVersionCompare(A_MajorVersion, A_MinorVersion, B_MajorVersion, B_MinorVersion);
  if (Result = 0) then begin
    if (A_BetaVersion > 0) then begin
      Result := 1;
    end;
  end;

end;

function SDUVersionCompareWithBetaFlag(A_MajorVersion, A_MinorVersion,
  A_RevisionVersion, A_BuildVersion: Integer; A_BetaVersion: Integer;
  B_MajorVersion, B_MinorVersion, B_RevisionVersion, B_BuildVersion: Integer): Integer;
begin
  Result := SDUVersionCompare(A_MajorVersion, A_MinorVersion, A_RevisionVersion,
    A_BuildVersion, B_MajorVersion, B_MinorVersion, B_RevisionVersion, B_BuildVersion);
  if (Result = 0) then begin
    if (A_BetaVersion > 0) then begin
      Result := 1;
    end;
  end;

end;

// ----------------------------------------------------------------------------
function SDUWMToString(msgID: Cardinal): String;
begin
  Result := inttohex(msgID, 8);

  case msgID of
    WM_NULL: Result                   := 'WM_NULL';
    WM_CREATE: Result                 := 'WM_CREATE';
    WM_DESTROY: Result                := 'WM_DESTROY';
    WM_MOVE: Result                   := 'WM_MOVE';
    WM_SIZE: Result                   := 'WM_SIZE';
    WM_ACTIVATE: Result               := 'WM_ACTIVATE';
    WM_SETFOCUS: Result               := 'WM_SETFOCUS';
    WM_KILLFOCUS: Result              := 'WM_KILLFOCUS';
    WM_ENABLE: Result                 := 'WM_ENABLE';
    WM_SETREDRAW: Result              := 'WM_SETREDRAW';
    WM_SETTEXT: Result                := 'WM_SETTEXT';
    WM_GETTEXT: Result                := 'WM_GETTEXT';
    WM_GETTEXTLENGTH: Result          := 'WM_GETTEXTLENGTH';
    WM_PAINT: Result                  := 'WM_PAINT';
    WM_CLOSE: Result                  := 'WM_CLOSE';
    WM_QUERYENDSESSION: Result        := 'WM_QUERYENDSESSION';
    WM_QUIT: Result                   := 'WM_QUIT';
    WM_QUERYOPEN: Result              := 'WM_QUERYOPEN';
    WM_ERASEBKGND: Result             := 'WM_ERASEBKGND';
    WM_SYSCOLORCHANGE: Result         := 'WM_SYSCOLORCHANGE';
    WM_ENDSESSION: Result             := 'WM_ENDSESSION';
    WM_SYSTEMERROR: Result            := 'WM_SYSTEMERROR';
    WM_SHOWWINDOW: Result             := 'WM_SHOWWINDOW';
    WM_CTLCOLOR: Result               := 'WM_CTLCOLOR';
    WM_WININICHANGE: Result           := 'WM_WININICHANGE/WM_WININICHANGE';
    WM_DEVMODECHANGE: Result          := 'WM_DEVMODECHANGE';
    WM_ACTIVATEAPP: Result            := 'WM_ACTIVATEAPP';
    WM_FONTCHANGE: Result             := 'WM_FONTCHANGE';
    WM_TIMECHANGE: Result             := 'WM_TIMECHANGE';
    WM_CANCELMODE: Result             := 'WM_CANCELMODE';
    WM_SETCURSOR: Result              := 'WM_SETCURSOR';
    WM_MOUSEACTIVATE: Result          := 'WM_MOUSEACTIVATE';
    WM_CHILDACTIVATE: Result          := 'WM_CHILDACTIVATE';
    WM_QUEUESYNC: Result              := 'WM_QUEUESYNC';
    WM_GETMINMAXINFO: Result          := 'WM_GETMINMAXINFO';
    WM_PAINTICON: Result              := 'WM_PAINTICON';
    WM_ICONERASEBKGND: Result         := 'WM_ICONERASEBKGND';
    WM_NEXTDLGCTL: Result             := 'WM_NEXTDLGCTL';
    WM_SPOOLERSTATUS: Result          := 'WM_SPOOLERSTATUS';
    WM_DRAWITEM: Result               := 'WM_DRAWITEM';
    WM_MEASUREITEM: Result            := 'WM_MEASUREITEM';
    WM_DELETEITEM: Result             := 'WM_DELETEITEM';
    WM_VKEYTOITEM: Result             := 'WM_VKEYTOITEM';
    WM_CHARTOITEM: Result             := 'WM_CHARTOITEM';
    WM_SETFONT: Result                := 'WM_SETFONT';
    WM_GETFONT: Result                := 'WM_GETFONT';
    WM_SETHOTKEY: Result              := 'WM_SETHOTKEY';
    WM_GETHOTKEY: Result              := 'WM_GETHOTKEY';
    WM_QUERYDRAGICON: Result          := 'WM_QUERYDRAGICON';
    WM_COMPAREITEM: Result            := 'WM_COMPAREITEM';
    WM_GETOBJECT: Result              := 'WM_GETOBJECT';
    WM_COMPACTING: Result             := 'WM_COMPACTING';
    WM_COMMNOTIFY: Result             := 'WM_COMMNOTIFY';
    WM_WINDOWPOSCHANGING: Result      := 'WM_WINDOWPOSCHANGING';
    WM_WINDOWPOSCHANGED: Result       := 'WM_WINDOWPOSCHANGED';
    WM_POWER: Result                  := 'WM_POWER';
    WM_COPYDATA: Result               := 'WM_COPYDATA';
    WM_CANCELJOURNAL: Result          := 'WM_CANCELJOURNAL';
    WM_NOTIFY: Result                 := 'WM_NOTIFY';
    WM_INPUTLANGCHANGEREQUEST: Result := 'WM_INPUTLANGCHANGEREQUEST';
    WM_INPUTLANGCHANGE: Result        := 'WM_INPUTLANGCHANGE';
    WM_TCARD: Result                  := 'WM_TCARD';
    WM_HELP: Result                   := 'WM_HELP';
    WM_USERCHANGED: Result            := 'WM_USERCHANGED';
    WM_NOTIFYFORMAT: Result           := 'WM_NOTIFYFORMAT';
    WM_CONTEXTMENU: Result            := 'WM_CONTEXTMENU';
    WM_STYLECHANGING: Result          := 'WM_STYLECHANGING';
    WM_STYLECHANGED: Result           := 'WM_STYLECHANGED';
    WM_DISPLAYCHANGE: Result          := 'WM_DISPLAYCHANGE';
    WM_GETICON: Result                := 'WM_GETICON';
    WM_SETICON: Result                := 'WM_SETICON';
    WM_NCCREATE: Result               := 'WM_NCCREATE';
    WM_NCDESTROY: Result              := 'WM_NCDESTROY';
    WM_NCCALCSIZE: Result             := 'WM_NCCALCSIZE';
    WM_NCHITTEST: Result              := 'WM_NCHITTEST';
    WM_NCPAINT: Result                := 'WM_NCPAINT';
    WM_NCACTIVATE: Result             := 'WM_NCACTIVATE';
    WM_GETDLGCODE: Result             := 'WM_GETDLGCODE';
    WM_NCMOUSEMOVE: Result            := 'WM_NCMOUSEMOVE';
    WM_NCLBUTTONDOWN: Result          := 'WM_NCLBUTTONDOWN';
    WM_NCLBUTTONUP: Result            := 'WM_NCLBUTTONUP';
    WM_NCLBUTTONDBLCLK: Result        := 'WM_NCLBUTTONDBLCLK';
    WM_NCRBUTTONDOWN: Result          := 'WM_NCRBUTTONDOWN';
    WM_NCRBUTTONUP: Result            := 'WM_NCRBUTTONUP';
    WM_NCRBUTTONDBLCLK: Result        := 'WM_NCRBUTTONDBLCLK';
    WM_NCMBUTTONDOWN: Result          := 'WM_NCMBUTTONDOWN';
    WM_NCMBUTTONUP: Result            := 'WM_NCMBUTTONUP';
    WM_NCMBUTTONDBLCLK: Result        := 'WM_NCMBUTTONDBLCLK';
    WM_NCXBUTTONDOWN: Result          := 'WM_NCXBUTTONDOWN';
    WM_NCXBUTTONUP: Result            := 'WM_NCXBUTTONUP';
    WM_NCXBUTTONDBLCLK: Result        := 'WM_NCXBUTTONDBLCLK';
    WM_INPUT: Result                  := 'WM_INPUT';
    WM_KEYFIRST: Result               := 'WM_KEYDOWN';
    WM_KEYUP: Result                  := 'WM_KEYUP';
    WM_CHAR: Result                   := 'WM_CHAR';
    WM_DEADCHAR: Result               := 'WM_DEADCHAR';
    WM_SYSKEYDOWN: Result             := 'WM_SYSKEYDOWN';
    WM_SYSKEYUP: Result               := 'WM_SYSKEYUP';
    WM_SYSCHAR: Result                := 'WM_SYSCHAR';
    WM_SYSDEADCHAR: Result            := 'WM_SYSDEADCHAR';
{$IF CompilerVersion >= 18.5}// Delphi 2007 and later
    WM_UNICHAR: Result                := 'WM_UNICHAR';
{$IFEND}
    WM_INITDIALOG: Result             := 'WM_INITDIALOG';
    WM_COMMAND: Result                := 'WM_COMMAND';
    WM_SYSCOMMAND: Result             := 'WM_SYSCOMMAND';
    WM_TIMER: Result                  := 'WM_TIMER';
    WM_HSCROLL: Result                := 'WM_HSCROLL';
    WM_VSCROLL: Result                := 'WM_VSCROLL';
    WM_INITMENU: Result               := 'WM_INITMENU';
    WM_INITMENUPOPUP: Result          := 'WM_INITMENUPOPUP';
    WM_MENUSELECT: Result             := 'WM_MENUSELECT';
    WM_MENUCHAR: Result               := 'WM_MENUCHAR';
    WM_ENTERIDLE: Result              := 'WM_ENTERIDLE';
    WM_MENURBUTTONUP: Result          := 'WM_MENURBUTTONUP';
    WM_MENUDRAG: Result               := 'WM_MENUDRAG';
    WM_MENUGETOBJECT: Result          := 'WM_MENUGETOBJECT';
    WM_UNINITMENUPOPUP: Result        := 'WM_UNINITMENUPOPUP';
    WM_MENUCOMMAND: Result            := 'WM_MENUCOMMAND';
    WM_CHANGEUISTATE: Result          := 'WM_CHANGEUISTATE';
    WM_UPDATEUISTATE: Result          := 'WM_UPDATEUISTATE';
    WM_QUERYUISTATE: Result           := 'WM_QUERYUISTATE';
    WM_CTLCOLORMSGBOX: Result         := 'WM_CTLCOLORMSGBOX';
    WM_CTLCOLOREDIT: Result           := 'WM_CTLCOLOREDIT';
    WM_CTLCOLORLISTBOX: Result        := 'WM_CTLCOLORLISTBOX';
    WM_CTLCOLORBTN: Result            := 'WM_CTLCOLORBTN';
    WM_CTLCOLORDLG: Result            := 'WM_CTLCOLORDLG';
    WM_CTLCOLORSCROLLBAR: Result      := 'WM_CTLCOLORSCROLLBAR';
    WM_CTLCOLORSTATIC: Result         := 'WM_CTLCOLORSTATIC';
    WM_MOUSEFIRST: Result             := 'WM_MOUSEMOVE';
    WM_LBUTTONDOWN: Result            := 'WM_LBUTTONDOWN';
    WM_LBUTTONUP: Result              := 'WM_LBUTTONUP';
    WM_LBUTTONDBLCLK: Result          := 'WM_LBUTTONDBLCLK';
    WM_RBUTTONDOWN: Result            := 'WM_RBUTTONDOWN';
    WM_RBUTTONUP: Result              := 'WM_RBUTTONUP';
    WM_RBUTTONDBLCLK: Result          := 'WM_RBUTTONDBLCLK';
    WM_MBUTTONDOWN: Result            := 'WM_MBUTTONDOWN';
    WM_MBUTTONUP: Result              := 'WM_MBUTTONUP';
    WM_MBUTTONDBLCLK: Result          := 'WM_MBUTTONDBLCLK';
    WM_MOUSEWHEEL: Result             := 'WM_MOUSEWHEEL';
    WM_PARENTNOTIFY: Result           := 'WM_PARENTNOTIFY';
    WM_ENTERMENULOOP: Result          := 'WM_ENTERMENULOOP';
    WM_EXITMENULOOP: Result           := 'WM_EXITMENULOOP';
    WM_NEXTMENU: Result               := 'WM_NEXTMENU';
    WM_SIZING: Result                 := 'WM_SIZING';
    WM_CAPTURECHANGED: Result         := 'WM_CAPTURECHANGED';
    WM_MOVING: Result                 := 'WM_MOVING';
    WM_POWERBROADCAST: Result         := 'WM_POWERBROADCAST';
    WM_DEVICECHANGE: Result           := 'WM_DEVICECHANGE';
    WM_IME_STARTCOMPOSITION: Result   := 'WM_IME_STARTCOMPOSITION';
    WM_IME_ENDCOMPOSITION: Result     := 'WM_IME_ENDCOMPOSITION';
    WM_IME_COMPOSITION: Result        := 'WM_IME_COMPOSITION';
    WM_IME_SETCONTEXT: Result         := 'WM_IME_SETCONTEXT';
    WM_IME_NOTIFY: Result             := 'WM_IME_NOTIFY';
    WM_IME_CONTROL: Result            := 'WM_IME_CONTROL';
    WM_IME_COMPOSITIONFULL: Result    := 'WM_IME_COMPOSITIONFULL';
    WM_IME_SELECT: Result             := 'WM_IME_SELECT';
    WM_IME_CHAR: Result               := 'WM_IME_CHAR';
    WM_IME_REQUEST: Result            := 'WM_IME_REQUEST';
    WM_IME_KEYDOWN: Result            := 'WM_IME_KEYDOWN';
    WM_IME_KEYUP: Result              := 'WM_IME_KEYUP';
    WM_MDICREATE: Result              := 'WM_MDICREATE';
    WM_MDIDESTROY: Result             := 'WM_MDIDESTROY';
    WM_MDIACTIVATE: Result            := 'WM_MDIACTIVATE';
    WM_MDIRESTORE: Result             := 'WM_MDIRESTORE';
    WM_MDINEXT: Result                := 'WM_MDINEXT';
    WM_MDIMAXIMIZE: Result            := 'WM_MDIMAXIMIZE';
    WM_MDITILE: Result                := 'WM_MDITILE';
    WM_MDICASCADE: Result             := 'WM_MDICASCADE';
    WM_MDIICONARRANGE: Result         := 'WM_MDIICONARRANGE';
    WM_MDIGETACTIVE: Result           := 'WM_MDIGETACTIVE';
    WM_MDISETMENU: Result             := 'WM_MDISETMENU';
    WM_ENTERSIZEMOVE: Result          := 'WM_ENTERSIZEMOVE';
    WM_EXITSIZEMOVE: Result           := 'WM_EXITSIZEMOVE';
    WM_DROPFILES: Result              := 'WM_DROPFILES';
    WM_MDIREFRESHMENU: Result         := 'WM_MDIREFRESHMENU';
    WM_MOUSEHOVER: Result             := 'WM_MOUSEHOVER';
    WM_MOUSELEAVE: Result             := 'WM_MOUSELEAVE';
    WM_NCMOUSEHOVER: Result           := 'WM_NCMOUSEHOVER';
    WM_NCMOUSELEAVE: Result           := 'WM_NCMOUSELEAVE';
    WM_WTSSESSION_CHANGE: Result      := 'WM_WTSSESSION_CHANGE';
    WM_TABLET_FIRST: Result           := 'WM_TABLET_FIRST';
    WM_TABLET_LAST: Result            := 'WM_TABLET_LAST';
    WM_CUT: Result                    := 'WM_CUT';
    WM_COPY: Result                   := 'WM_COPY';
    WM_PASTE: Result                  := 'WM_PASTE';
    WM_CLEAR: Result                  := 'WM_CLEAR';
    WM_UNDO: Result                   := 'WM_UNDO';
    WM_RENDERFORMAT: Result           := 'WM_RENDERFORMAT';
    WM_RENDERALLFORMATS: Result       := 'WM_RENDERALLFORMATS';
    WM_DESTROYCLIPBOARD: Result       := 'WM_DESTROYCLIPBOARD';
    WM_DRAWCLIPBOARD: Result          := 'WM_DRAWCLIPBOARD';
    WM_PAINTCLIPBOARD: Result         := 'WM_PAINTCLIPBOARD';
    WM_VSCROLLCLIPBOARD: Result       := 'WM_VSCROLLCLIPBOARD';
    WM_SIZECLIPBOARD: Result          := 'WM_SIZECLIPBOARD';
    WM_ASKCBFORMATNAME: Result        := 'WM_ASKCBFORMATNAME';
    WM_CHANGECBCHAIN: Result          := 'WM_CHANGECBCHAIN';
    WM_HSCROLLCLIPBOARD: Result       := 'WM_HSCROLLCLIPBOARD';
    WM_QUERYNEWPALETTE: Result        := 'WM_QUERYNEWPALETTE';
    WM_PALETTEISCHANGING: Result      := 'WM_PALETTEISCHANGING';
    WM_PALETTECHANGED: Result         := 'WM_PALETTECHANGED';
    WM_HOTKEY: Result                 := 'WM_HOTKEY';
    WM_PRINT: Result                  := 'WM_PRINT';
    WM_PRINTCLIENT: Result            := 'WM_PRINTCLIENT';
    WM_APPCOMMAND: Result             := 'WM_APPCOMMAND';
    WM_THEMECHANGED: Result           := 'WM_THEMECHANGED';
    WM_HANDHELDFIRST: Result          := 'WM_HANDHELDFIRST';
    WM_HANDHELDLAST: Result           := 'WM_HANDHELDLAST';
    WM_PENWINFIRST: Result            := 'WM_PENWINFIRST';
    WM_PENWINLAST: Result             := 'WM_PENWINLAST';
    WM_COALESCE_FIRST: Result         := 'WM_COALESCE_FIRST';
    WM_COALESCE_LAST: Result          := 'WM_COALESCE_LAST';
    WM_DDE_FIRST: Result              := 'WM_DDE_INITIATE';
    WM_DDE_TERMINATE: Result          := 'WM_DDE_TERMINATE';
    WM_DDE_ADVISE: Result             := 'WM_DDE_ADVISE';
    WM_DDE_UNADVISE: Result           := 'WM_DDE_UNADVISE';
    WM_DDE_ACK: Result                := 'WM_DDE_ACK';
    WM_DDE_DATA: Result               := 'WM_DDE_DATA';
    WM_DDE_REQUEST: Result            := 'WM_DDE_REQUEST';
    WM_DDE_POKE: Result               := 'WM_DDE_POKE';
    WM_DDE_EXECUTE: Result            := 'WM_DDE_EXECUTE';
{$IF CompilerVersion >= 18.5}// Delphi 2007 and later
    WM_DWMCOMPOSITIONCHANGED: Result  := 'WM_DWMCOMPOSITIONCHANGED';
    WM_DWMNCRENDERINGCHANGED: Result  := 'WM_DWMNCRENDERINGCHANGED';
    WM_DWMCOLORIZATIONCOLORCHANGED: Result := 'WM_DWMCOLORIZATIONCOLORCHANGED';
    WM_DWMWINDOWMAXIMIZEDCHANGE: Result := 'WM_DWMWINDOWMAXIMIZEDCHANGE';
{$IFEND}
    WM_APP: Result                    := 'WM_APP';
  end;

end;


 // ----------------------------------------------------------------------------
 // Convert ShowWindow(...) show state to string
function SDUShowStateToString(nCmdShow: Word): String;
begin
  Result := '<Unknown>';
  case nCmdShow of
    //    SW_FORCEMINIMIZE:   Result := 'SW_FORCEMINIMIZE';
    SW_HIDE: Result            := 'SW_HIDE';
    //    SW_MAXIMIZE:        Result := 'SW_MAXIMIZE';
    SW_MINIMIZE: Result        := 'SW_MINIMIZE';
    SW_RESTORE: Result         := 'SW_RESTORE';
    SW_SHOW: Result            := 'SW_SHOW';
    SW_SHOWDEFAULT: Result     := 'SW_SHOWDEFAULT';
    SW_SHOWMAXIMIZED: Result   := 'SW_SHOWMAXIMIZED';
    SW_SHOWMINIMIZED: Result   := 'SW_SHOWMINIMIZED';
    SW_SHOWMINNOACTIVE: Result := 'SW_SHOWMINNOACTIVE';
    SW_SHOWNA: Result          := 'SW_SHOWNA';
    SW_SHOWNOACTIVATE: Result  := 'SW_SHOWNOACTIVATE';
    SW_SHOWNORMAL: Result      := 'SW_SHOWNORMAL';
  end;

end;


 // ----------------------------------------------------------------------------
 // Get string representation of form's layout
function SDUGetFormLayout(form: TForm): String;
var
  stlLayout: TStringList;
  placement: TWindowPlacement;
begin
  // If the window's maximised, the forms top, left, width and height are set
  // to the maximised values - use this to get the normalised size and position
  placement.Length := sizeof(placement);
  GetWindowPlacement(form.Handle, @placement);

  stlLayout := TStringList.Create();
  try
    if IsIconic(form.Handle) then begin
      stlLayout.Values[WINDOW_LAYOUT_STATE] := IntToStr(Ord(wsMinimized));
    end else begin
      stlLayout.Values[WINDOW_LAYOUT_STATE] := IntToStr(Ord(form.WindowState));
    end;

    stlLayout.Values[WINDOW_LAYOUT_TOP]    := IntToStr(Ord(placement.rcNormalPosition.Top));
    stlLayout.Values[WINDOW_LAYOUT_LEFT]   := IntToStr(Ord(placement.rcNormalPosition.Left));
    stlLayout.Values[WINDOW_LAYOUT_HEIGHT] :=
      IntToStr(Ord(placement.rcNormalPosition.Bottom - placement.rcNormalPosition.Top));
    stlLayout.Values[WINDOW_LAYOUT_WIDTH]  :=
      IntToStr(Ord(placement.rcNormalPosition.Right - placement.rcNormalPosition.Left));

    stlLayout.Delimiter := ',';
    stlLayout.QuoteChar := '"';
    Result              := stlLayout.DelimitedText;
  finally
    stlLayout.Free();
  end;

end;

 // ----------------------------------------------------------------------------
 // Set form's layout based on string representation of it
 // !! IMPORTANT !!
 // If there's a window layout stored, set ".Position" to poDefault before
 // calling this - otherwise it messes up the window if it was stored as
 // maximised.
 // Specifically, it shows the main window with maximised dimensions, with
 // the "Maximise" button in the top-right ready to "Normalise"
 // (non-maximise) the window, but WITH THE WINDOW SHOWN ABOUT 50 PIXELS
 // DOWN!)
procedure SDUSetFormLayout(form: TForm; layout: String);
var
  frmState:  TWindowState;
  stlLayout: TStringList;
  placement: TWindowPlacement;
  tmpHeight: Integer;
  tmpWidth:  Integer;
begin
  // Sanity...
  if (trim(layout) = '') then begin
    exit;
  end;

  stlLayout := TStringList.Create();
  try
    stlLayout.Delimiter     := ',';
    stlLayout.QuoteChar     := '"';
    stlLayout.DelimitedText := layout;

    // If the window's maximised, the forms top, left, width and height are set
    // to the maximised values - use this to get the normalised size and position
    placement.Length := sizeof(placement);
    GetWindowPlacement(form.Handle, @placement);

    placement.rcNormalPosition.Top  :=
      SDUTryStrToIntDflt(stlLayout.Values[WINDOW_LAYOUT_TOP], placement.rcNormalPosition.Top);
    placement.rcNormalPosition.Left :=
      SDUTryStrToIntDflt(stlLayout.Values[WINDOW_LAYOUT_LEFT],
      placement.rcNormalPosition.Left);

    tmpHeight := SDUTryStrToIntDflt(stlLayout.Values[WINDOW_LAYOUT_HEIGHT],
      (placement.rcNormalPosition.Bottom - placement.rcNormalPosition.Top));
    tmpWidth  := SDUTryStrToIntDflt(stlLayout.Values[WINDOW_LAYOUT_WIDTH],
      (placement.rcNormalPosition.Right - placement.rcNormalPosition.Left));

    placement.rcNormalPosition.Bottom := placement.rcNormalPosition.Top + tmpHeight;
    placement.rcNormalPosition.Right  := placement.rcNormalPosition.Left + tmpWidth;

    // Sanity check - we're only set the window position if it doesn't go
    // off screen (partially off-screen should be OK enough though - as long
    // as the title bar's visible, the user can always drag it back)
    if ((placement.rcNormalPosition.Top >= 0) and
      (placement.rcNormalPosition.Top < Screen.Height) and
      (placement.rcNormalPosition.Left >= 0) and (placement.rcNormalPosition.Left <
      Screen.Width)) then begin
      SetWindowPlacement(form.Handle, @placement);
    end;

    frmState := TWindowState(SDUTryStrToIntDflt(
      stlLayout.Values[WINDOW_LAYOUT_STATE], Ord(form.WindowState)));
    // Special case - if minimised, call Application.Minimise(...) - otherwise
    // the window looks like it's been "minimised to the desktop" - like an
    // MDI child window with the desktop being the main window
    if (frmState = wsMinimized) then begin
      form.Visible := True;
      Application.Minimize();
    end else begin
      form.WindowState := frmState;
    end;

  finally
    stlLayout.Free();
  end;

end;

 // ----------------------------------------------------------------------------
 // Returns the string passed in, but with the initial letter capitalized
function SDUInitialCapital(Value: String): String;
begin
  Result := Value;
  if (length(Result) > 0) then begin
    Result[1] := upcase(Result[1]);
  end;

end;


// ----------------------------------------------------------------------------
function SDUZLibCompressionLevelTitle(compressionLevel: TCompressionLevel): String;
begin
  Result := LoadResString(ZLibCompressionLevelTitlePtr[compressionLevel]);
end;

//replaces any subst/mapping
function SDUGetFinalPath(path: String): String;
var
  hFile:        THandle;
  lpszFilePath: array[0..MAX_PATH] of Char;
begin
  hFile := FileOpen(path, 0);
  GetFinalPathNameByHandle(hFile, lpszFilePath, MAX_PATH, 0);
  Result := lpszFilePath;
  FileClose(hFile);
end;

//encodes as ascii for now
function SDUStringToSDUBytes(const rhs: Ansistring): TSDUBytes;
var
  len, i: Integer;
begin
  len := length(rhs);
  setlength(Result, len);
  for i := 0 to len - 1 do
    Result[i] := Byte(rhs[i + 1]);
end;

function SDUBytesToString(const Value: TSDUBytes): Ansistring;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to high(Value) do
    Result := Result + AnsiChar(Value[i]);
end;

//initialises value to all zeros
procedure SDUInitAndZeroBuffer(len: Cardinal; var val: TSDUBytes);
begin
  // setlength re-alloacates and can change reference - so even if new value is longer: still need to zero old val before resizing
  //use SafeSetLength so if change size, any freed data zeroed
  SafeSetLength(val, len);
  SDUZeroBuffer(val);
end;

//adds byte to array
procedure SDUAddByte(var Value: TSDUBytes; byt: Byte);
var
  len: Integer;
begin
  len := length(Value);
  SafeSetLength(Value, len + 1);
  { DONE 1 -otdk -ccheck : setlength can reallocate array but  AlwaysClearFreedMemory is used to zero any freed mem }
  Value[len] := byt;
end;

//adds array to array
procedure SDUAddArrays(var A: TSDUBytes; const rhs: TSDUBytes);
var
  ln: Integer;
begin
  ln := length(A) + length(rhs);
  SDUAddLimit(A, rhs, length(rhs));
  assert(length(A) = ln);
end;

// adds rhs to end of lhs, up to 'limit' bytes from rhs
procedure SDUAddLimit(var lhs: TSDUBytes; const rhs: TSDUBytes; limit: Integer);
var
  len, len_rhs, i: Integer;
begin

  len := length(lhs);

  len_rhs := min(limit, length(rhs));
  SafeSetLength(lhs, len + len_rhs);
  { DONE 1 -otdk -ccheck : setlength can realloate array but  AlwaysClearFreedMemory is used to zeroise any freed mem }
  for i := 0 to len_rhs - 1 do
    lhs[i + len] := rhs[i];
end;

//copies from aFrom to aTo, sets length and zeroises any freed data
procedure SDUCopy(var aTo: TSDUBytes; const aFrom: TSDUBytes);
begin
  SDUCopyLimit(aTo, aFrom, length(aFrom));
end;

//copies from aFrom to aTo up to limit bytes, sets length and zeroises any freed data
procedure SDUCopyLimit(var aTo: TSDUBytes; const aFrom: TSDUBytes; limit: Integer);
var
  i, newLen: Integer;
begin
  newLen := min(limit, length(aFrom));
  // if shortening - SafeSetLength overwrites data now not in array

  SafeSetLength(aTo, newLen);
  { DONE 1 -otdk -ccheck : setlength can reset ref. if inc'ing, then Mem manager zeroises. if shrotening SafeSetLength zeros deleted bytes }
  //copy  data
  for i := 0 to newLen - 1 do
    aTo[i] := aFrom[i];
end;

procedure SDUZeroBuffer(var buf: array of Byte);
var
  i: Integer;
begin
  for i := low(buf) to high(buf) do
    buf[i] := 0;
end;

procedure SDUZeroString(var buf: Ansistring);
var
  i: Integer;
begin
  for i := 1 to length(buf) do
    buf[i] := AnsiChar(#0);
  setlength(buf, 0);
end;

procedure SDUDeleteFromStart(var A: TSDUBytes; Count: Integer);
var
  len, i: Integer;
begin
  len := length(A);
  for i := 0 to len - Count - 1 do
    A[i] := A[i + Count];
  for i := len - Count to len - 1 do
    A[i] := A[0];

  SafeSetLength(A, len - Count);
  { Done 1 -otdk -ccheck : setlength doesnt zero removed bytes, but SafeSetLength does }
end;

//incs/decs length and zeroises any data
procedure SafeSetLength(var A: TSDUBytes; newLen: Integer);
var
  len, i: Integer;
  {$IFNDEF AlwaysClearFreedMemory}
  temp:   TSDUBytes;
     {$ENDIF}
begin
  len := length(A);
  // if shortening - overwrite data now not in array (FastMM4 doest do this - prob runtime keeps 'capacity' field and doesnt free it)
  for i := len - 1 downto newLen do
    A[i] := 0;

   {$IFDEF AlwaysClearFreedMemory}
     setlength(A, newLen);
     { done 1 -otdk -ccheck : setlength can reset ref. but Mem manager zeros so no need to copy and zero }
   {$ELSE}
    {AlwaysClearFreedMemory should always be set bc there are cases where data on heap but TSDUBytes
      isnt used - but in case it isnt, at least deal with TSDUBytes being re allocated}
  if newLen < len then begin
    // no reallocation so dealt with above
    setlength(A, newLen);
  end else begin
    // copy to temp array, zero old, assign temp to old (is a reference), delete temp reference
    setlength(temp, newLen); // temp array is all zeros - done by delphi runtime
    for i := 0 to len - 1 do
      temp[i] := A[i];  // newLen >= len

    for i := 0 to len - 1 do
      A[i] := 0; //wipe old data

    A    := temp;   //assigns var, is ref counted
    temp := nil;    // deletes old ref
  end;
   {$ENDIF}

  //any new data is zeroed by delphi runtime
end;

function SDUMapNetworkDrive(networkShare: String; useDriveLetter: Char): Boolean;
begin
  { TODO 3 -otdk -cfix : implement }
end;

 // ----------------------------------------------------------------------------
 // ----------------------------------------------------------------------------

end.
