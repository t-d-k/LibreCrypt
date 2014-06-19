// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFE4PDARegistry_H
#define _FreeOTFE4PDARegistry_H   1

#include <windows.h>  // Required for UINT, BOOL

#define REGHIVE  HKEY_LOCAL_MACHINE

#define REGKEY_KEY_PREFIX             TEXT("FreeOTFE_%0.2d")
#define REGKEY_SEPARATOR              TEXT("\\")

#define REGKEY_DRIVERS                TEXT("\\Drivers")
#define REGKEY_DRIVERS_ACTIVE         REGKEY_DRIVERS REGKEY_SEPARATOR TEXT("Active")
#define REGNAME_DRIVERROOTKEY         TEXT("RootKey")
#define REGKEY_BUILTIN_FALLBACK       REGKEY_DRIVERS REGKEY_SEPARATOR TEXT("Builtin")

#define REGNAME_BUILTIN_PREFIX        TEXT("Prefix")
#define REGNAME_BUILTIN_DLL           TEXT("Dll")
#define REGNAME_BUILTIN_FRIENDLYNAME  TEXT("FriendlyName")
#define REGNAME_BUILTIN_ORDER         TEXT("Order")
// Ioctl is optional - if set, then once mounted, the driver will receive the
// numbered Ioctl call
#define REGNAME_BUILTIN_IOCTL         TEXT("Ioctl")
#define REGNAME_BUILTIN_ICLASS        TEXT("IClass")
#define REGNAME_BUILTIN_PROFILE       TEXT("Profile")

#define REGKEY_PROFILE              TEXT("\\System\\StorageManager\\Profiles")
#define REGNAME_PROFILE_NAME        TEXT("Name")
#define REGNAME_PROFILE_FOLDER      TEXT("Folder")
#define REGNAME_PROFILE_AUTOMOUNT   TEXT("AutoMount")
#define REGNAME_PROFILE_AUTOPART    TEXT("AutoPart")
#define REGNAME_PROFILE_AUTOFORMAT  TEXT("AutoFormat")
#define REGNAME_PROFILE_MOUNTFLAGS  TEXT("MountFlags")

// The "Hnd" key appears to be set to the same value as returned by
// ActivateDeviceEx, however this doesn't apear documented; hence we set the
// handle explicitly on the device so we can retrieve it the device there
// later
//#define REGNAME_ACTIVE_HND          TEXT("Hnd")
#define REGNAME_ACTIVE_KEY          TEXT("Key")
#define REGNAME_ACTIVE_NAME         TEXT("Name")

// We can use any prefix "decoration" (e.g. "F4P"); DSK used to exiting
// maintain Windows Mobile convention
#define DRIVER_PREFIX        TEXT("DSK")
#define DRIVER_FRIENDLYNAME  TEXT("FreeOTFE driver")
#define DRIVER_PROFILE_NAME  TEXT("FreeOTFE device")

// This struct returns details of a mounted volume
typedef struct _REGDETAILS_BUILTIN {
    WCHAR* Prefix;
    WCHAR* Dll;
    WCHAR* FriendlyName;
    DWORD Order;
    DWORD Ioctl;
    WCHAR* IClass;
    WCHAR* Profile;
} REGDETAILS_BUILTIN, *PREGDETAILS_BUILTIN;

// This struct returns details of a mounted volume
typedef struct _REGDETAILS_PROFILE {
    WCHAR* Name;
    WCHAR* Folder;
    DWORD AutoMount;
    DWORD AutoPart;
    DWORD AutoFormat;
    DWORD MountFlags;
} REGDETAILS_PROFILE, *PREGDETAILS_PROFILE;

// This struct returns active details of a mounted volume
typedef struct _REGDETAILS_ACTIVE {
    // The "Hnd" key appears to be set to the same value as returned by
    // ActivateDeviceEx, however this doesn't apear documented; hence we set the
    // handle explicitly on the device so we can retrieve it the device there
    // later
    // DWORD Hnd;

    WCHAR* Key;
    WCHAR* Name;

    WCHAR* Mountpoint;
} REGDETAILS_ACTIVE, *PREGDETAILS_ACTIVE;


BOOL RegistrySetString(HKEY hKey, LPCWSTR name, LPCWSTR value);
BOOL RegistryGetString(HKEY hKey, LPCWSTR name, LPCWSTR* value);
BOOL RegistrySetDWORD(HKEY hKey, LPCWSTR name, DWORD value);
BOOL RegistryGetDWORD(HKEY hKey, LPCWSTR name, DWORD* value);

int RegGetNextRegNumber();

BOOL RegDetailsSetAll(
    int id, 
    REGDETAILS_BUILTIN detailsBuiltin,
    REGDETAILS_PROFILE detailsProfile
);
BOOL RegDetailsGetAllByKey(
    WCHAR* builtinKey, 
    REGDETAILS_BUILTIN* detailsBuiltin,
    REGDETAILS_PROFILE* detailsProfile
);
BOOL RegDetailsGetAllByID(
    int id, 
    REGDETAILS_BUILTIN* detailsBuiltin,
    REGDETAILS_PROFILE* detailsProfile
);

BOOL RegDetailsGetBuiltinByID(int id, REGDETAILS_BUILTIN *detailsBuiltin);
BOOL RegDetailsGetBuiltinByKey(WCHAR* key, REGDETAILS_BUILTIN *detailsBuiltin);
BOOL RegDetailsGetBuiltinByHKEY(HKEY hKey, REGDETAILS_BUILTIN *detailsBuiltin);
BOOL RegDetailsGetProfileByID(
    int id, 
    REGDETAILS_PROFILE* detailsProfile
);
BOOL RegDetailsGetProfileByKey(
    WCHAR* key, 
    REGDETAILS_PROFILE* detailsProfile
);
BOOL RegDetailsGetProfileByHKEY(
    HKEY hKey, 
    REGDETAILS_PROFILE* detailsProfile
);
BOOL RegDetailsGetActiveByKey(
    WCHAR* key, 
    // The "Hnd" key appears to be set to the same value as returned by
    // ActivateDeviceEx, however this doesn't apear documented; hence we set the
    // handle explicitly on the device so we can retrieve it the device there
    // later
    //BOOL mustInclHnd,
    REGDETAILS_ACTIVE *detailsActive
);
BOOL RegDetailsGetActiveByHKEY(
    HKEY hKey, 
    // The "Hnd" key appears to be set to the same value as returned by
    // ActivateDeviceEx, however this doesn't apear documented; hence we set the
    // handle explicitly on the device so we can retrieve it the device there
    // later
    //BOOL mustInclHnd,
    REGDETAILS_ACTIVE *detailsActive
);
void RegDetailsFree(
    REGDETAILS_BUILTIN *detailsBuiltin,
    REGDETAILS_PROFILE *detailsProfile,
    REGDETAILS_ACTIVE *detailsActive
);
BOOL RegDetailsDeleteAllByID(int id, BOOL force);
BOOL RegDetailsDeleteAllByKey(WCHAR* builtinKey, BOOL force);

WCHAR* ProfileNameGenerate(int id);
void RegKeyGenerate(int id, WCHAR** keyBuiltin, WCHAR** keyProfile);


// Returns -1 on failure
int RegDetailsGetAllActive(
    int detailsActiveSize,  // Size in bytes of detailsActive buffer
    REGDETAILS_ACTIVE *detailsActive  // Actually an array of REGDETAILS_ACTIVE
);

void RegDetailsFreeAllActive(
    int cnt, // Number of elements in detailsActive array
    REGDETAILS_ACTIVE* detailsActive  // Actually an array of REGDETAILS_ACTIVE
);

BOOL RegAssociateFileExtension(
    WCHAR* FileExtension, // *Without* leading "."
    WCHAR* FileType, // Short descriptor
    WCHAR* FileDescription,
    WCHAR* OpenCommand,
    WCHAR* IconFile,
    int IconID
);
void RegDisassociateFileExtension(
    WCHAR* FileExtension, // *Without* leading "."
    WCHAR* FileType // Short descriptor
);
BOOL RegIsFileExtensionAssociated(
    WCHAR* FileExtension, // *Without* leading "."
    WCHAR* FileType // Short descriptor
);


// =========================================================================
// =========================================================================

#endif

