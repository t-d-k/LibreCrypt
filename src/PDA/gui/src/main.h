// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _main_H
#define _main_H   1

#include <windows.h>
#include "FreeOTFE4PDAOptions.h"

// Arbitary control ID
#define IDCTRL_USERRNG 125

// About dialog settings...
#define APP_TITLE          TEXT("FreeOTFE4PDA")
#define APP_AUTHOR_CREDIT  TEXT("By Sarah Dean")
#define APP_URL            TEXT("http://www.FreeOTFE.org/")
// Set to -1 for RELEASE
#define APP_BETA_BUILD     -1

#define DOCUMENTATION_LOCALPATH_1  TEXT("docs\\mobile_site\\index.htm")
#define DOCUMENTATION_LOCALPATH_2  TEXT("docs\\index.htm")
#define DOCUMENTATION_URL          TEXT("http://www.FreeOTFE.org/docs/Main/mobile_site/index.htm")

// Colors...
#define COLOR_URL      RGB(0, 0, 255)
#define COLOR_WARNING  RGB(255, 0, 0)


#define VOL_FILE_EXTN                        TEXT("vol")
#define VOL_FILE_TYPE                        TEXT("FOTFEvolume")
#define VOL_FILE_DESC                        _("FreeOTFE4PDA volume")

#define FILE_FILTER_DESC_ALLFILES        _("All files")
#define FILE_FILTER_EXTN_ALLFILES        TEXT("*.*")
#define FILE_FILTER_DESC_VOLANDKEYFILES  _("Volume files and keyfiles")
#define FILE_FILTER_EXTN_VOLANDKEYFILES  TEXT("*.vol;*.cdb")
#define FILE_FILTER_DESC_VOLUMES         _("Volume files")
#define FILE_FILTER_EXTN_VOLUMES         TEXT("*.vol")
#define FILE_FILTER_DESC_KEYFILES        _("Keyfiles")
#define FILE_FILTER_EXTN_KEYFILES        TEXT("*.cdb")
#define FILE_FILTER_DESC_TEXT            _("Text files")
#define FILE_FILTER_EXTN_TEXT            TEXT("*.txt")
#define FILE_FILTER_DESC_CDBBACKUPS      _("CDB backups")
#define FILE_FILTER_EXTN_CDBBACKUPS      TEXT("*.cdbBackup")
#define FILE_FILTER_DESC_EXECUTABLES     _("Executable files")
#define FILE_FILTER_EXTN_EXECUTABLES     TEXT("*.exe")

#define FILE_FILTER_VOLANDKEYFILES  0x01
#define FILE_FILTER_VOLUMES         0x02
#define FILE_FILTER_KEYFILES        0x04
#define FILE_FILTER_TEXT            0x08
#define FILE_FILTER_CDBBACKUPS      0x10
#define FILE_FILTER_EXECUTABLES     0x20
#define FILE_FILTER_ALLFILES        0x40

// Open/Save file filters...
#define FILE_FILTER_FLT_VOLUMES              (FILE_FILTER_VOLUMES | FILE_FILTER_ALLFILES)
#define FILE_FILTER_CNT_VOLUMES              2
#define FILE_FILTER_DFLT_VOLUMES             0

#define FILE_FILTER_FLT_KEYFILES             (FILE_FILTER_KEYFILES | FILE_FILTER_ALLFILES)
#define FILE_FILTER_CNT_KEYFILES             2
#define FILE_FILTER_DFLT_KEYFILES            0

#define FILE_FILTER_FLT_VOLUMESANDKEYFILES   (FILE_FILTER_VOLANDKEYFILES | FILE_FILTER_VOLUMES | FILE_FILTER_KEYFILES | FILE_FILTER_ALLFILES)
#define FILE_FILTER_CNT_VOLUMESANDKEYFILES   4
#define FILE_FILTER_DFLT_VOLUMESANDKEYFILES  0

#define FILE_FILTER_FLT_TEXTFILES            (FILE_FILTER_TEXT | FILE_FILTER_ALLFILES)
#define FILE_FILTER_CNT_TEXTFILES            2
#define FILE_FILTER_DFLT_TEXTFILES           0

#define FILE_FILTER_FLT_CDBBACKUPS           (FILE_FILTER_CDBBACKUPS | FILE_FILTER_ALLFILES)
#define FILE_FILTER_CNT_CDBBACKUPS           2
#define FILE_FILTER_DFLT_CDBBACKUPS          0

#define FILE_FILTER_FLT_EXECUTABLES          (FILE_FILTER_EXECUTABLES | FILE_FILTER_ALLFILES)
#define FILE_FILTER_CNT_EXECUTABLES          2
#define FILE_FILTER_DFLT_EXECUTABLES         0



// Minimum values...
#define MIN_KEYITERATIONS  1
// (Min volume size in MB)
#define MIN_VOLUME_SIZE    2

// Spin control deltas...
#define DELTA_SALT_LENGTH      8
#define DELTA_KEY_ITERATIONS   512
#define DELTA_CYPHERKEYLENGTH  8

// Default values...
#ifdef DEBUG
//#define DISABLE_LIST_VIEW  TRUE
#define DISABLE_LIST_VIEW  FALSE

#define DEFAULT_FILENAME           TEXT("\\Storage Card\\testvol_ONE_ITER.vol")
//#define DEFAULT_FILENAME           TEXT("")
//#define DEFAULT_KEYFILE            TEXT("\\Storage Card\\testvol_ONE_ITER.CDB")
#define DEFAULT_KEYFILE            TEXT("")
#define DEFAULT_PASSWORD           TEXT("password")
#define DEFAULT_LUKS_PASSWORD      TEXT("password1234567890ABC")
#define DEFAULT_KEYITERATIONS      1
//#define DEFAULT_KEYITERATIONS      2048
#define DEFAULT_CDBATOFFSET        FALSE
#define DEFAULT_READONLY           FALSE
#else
#define DISABLE_LIST_VIEW  FALSE

#define DEFAULT_FILENAME           TEXT("")
#define DEFAULT_KEYFILE            TEXT("")
#define DEFAULT_PASSWORD           TEXT("")
#define DEFAULT_LUKS_PASSWORD      TEXT("")
#define DEFAULT_KEYITERATIONS      2048
#define DEFAULT_CDBATOFFSET        TRUE
#define DEFAULT_READONLY           FALSE
#endif

#define DEFAULT_OFFSET             0
#define DEFAULT_SALTLENGTH         256
#define DEFAULT_VOLUME_SIZE        10
#define DEFAULT_CYPHERKEYLENGTH    512
#define DEFAULT_RND_USERINPUT      FALSE
#define DEFAULT_RNG_MSCRYPTOAPI    TRUE
#define DEFAULT_CDBSTOREDINVOLUME  TRUE

#define DEFAULT_LUKS_BASEICCYPHERONHASHLENGTH  TRUE
#define DEFAULT_SIZELIMIT                      FALSE

//#define DEFAULT_HASH         TEXT("SHA-512 (512/1024)")
//#define DEFAULT_CYPHER       TEXT("AES (CBC; 256/128)")
#define DEFAULT_HASH         TEXT("SHA-512")
#define DEFAULT_CYPHER       TEXT("AES (256 bit XTS)")
#define DEFAULT_SECTORIVGEN  _("Null IV")
#define DEFAULT_VOLUMEIV           FALSE

// Executables
#define EXE_MSIE              TEXT("IEXPLORE")

#define STATS_REPORT_FILE  TEXT("FreeOTFE_Stats_Report.txt")


// Globals...
HWND      G_hWndMain;
HINSTANCE G_hInstance;
OPTIONS*  G_Options;

typedef struct _RNG_SYSTEM {
    BOOL UseUserInput;
    BOOL UseMSCryptoAPI;
} RNG_SYSTEM, *PRNG_SYSTEM;

void ExploreMountpoint(WCHAR* Mountpoint);

// Generate filter to be used in standard open/save dialogs
// Filter - Any combination of FILE_FILTER_*
WCHAR* FileFilterGenerate(int Filter);
void FileFilterFree(WCHAR* Filter);

// =========================================================================
// =========================================================================

#endif

