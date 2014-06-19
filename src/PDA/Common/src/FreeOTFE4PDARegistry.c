// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFElib.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFE4PDARegistry.h"

#include <stdlib.h>
#include <Winreg.h>

#define REGHIVE_FILEASSOC               HKEY_CLASSES_ROOT
#define REGKEY_FILEASSOC_EXTN           TEXT("\\.%s")
#define REGKEY_FILEASSOC_FILETYPE       TEXT("\\%s")
#define REGKEY_FILEASSOC_COMMAND        TEXT("\\%s\\Shell\\Open\\Command")
#define REGKEY_FILEASSOC_DEFAULTICON    TEXT("\\%s\\DefaultIcon")
#define REGVALUE_FILEASSOC_DEFAULTICON  TEXT("%s,-%d")


// =========================================================================
BOOL RegistrySetString(HKEY hKey, LPCWSTR name, LPCWSTR value)
{
    BOOL retval = FALSE;

    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Set registry [s]: NAME: %s\n"), name));
    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Set registry [s]: VALUE: %s\n"), value));
    retval = (RegSetValueEx(
                     hKey, 
                     name,
                     0,
                     REG_SZ,
                     (BYTE*)value,
                     // Note: Include the terminating NULL
                     ((wcslen(value) + 1) * sizeof(WCHAR))
                    ) == ERROR_SUCCESS);

    return retval;
}


// =========================================================================
// Note: Storage for "value" is automatically allocated; it is up to the
//       *caller* to free this off after use
BOOL RegistryGetString(HKEY hKey, LPCWSTR name, LPCWSTR* value)
{
    BOOL retval = FALSE;
    DWORD type;
    DWORD size;
    
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Get registry [s]: NAME: %s\n"), name));
    retval = (RegQueryValueEx( 
                              hKey,
                              name,
                              NULL,
                              &type,
                              NULL,
                              &size
                             ) == ERROR_SUCCESS); 

    if (retval) 
        {
        *value = malloc(size);
        retval = (RegQueryValueEx( 
                              hKey,
                              name,
                              NULL,
                              &type,
                              (LPBYTE)(*value),
                              &size
                             ) == ERROR_SUCCESS);
        if (!(retval))
            {
            free((void*)(*value));
            }
        
        }

    if (!(retval))
        {
        // Fallback to NULL
        *value = NULL;
        DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Get registry [s]: <Unable to get value>\n")));
        }
    else
        {
        DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Get registry [s]: VALUE: %ls\n"), *value));
        }

    return retval;
}


// =========================================================================
BOOL RegistrySetDWORD(HKEY hKey, LPCWSTR name, DWORD value)
{
    BOOL retval = FALSE;

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Set registry [dw]: NAME: %s\n"), name));
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Set registry [dw]: VALUE: %d\n"), value));
    retval = (RegSetValueEx(
                     hKey, 
                     name,
                     0,
                     REG_DWORD,
                     (BYTE*)&value,
                     sizeof(value)
                    ) == ERROR_SUCCESS);

    return retval;
}


// =========================================================================
BOOL RegistryGetDWORD(HKEY hKey, LPCWSTR name, DWORD* value)
{
    BOOL retval = FALSE;
    DWORD type;
    DWORD size;

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Get registry [dw]: NAME: %s\n"), name));
    retval = (RegQueryValueEx( 
                              hKey,
                              name,
                              NULL,
                              &type,
                              NULL,
                              &size
                             ) == ERROR_SUCCESS); 

    if (
        (retval) &&
        (size == sizeof(*value))
       )
        {
        retval = (RegQueryValueEx( 
                              hKey,
                              name,
                              NULL,
                              &type,
                              (LPBYTE)(value),
                              &size
                             ) == ERROR_SUCCESS);
        }

    if (!(retval))
        {
        // Fallback to 0
        *value = 0;
        DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Get registry [dw]: <Unable to get value>\n")));
        }
    else
        {
        DEBUGOUTLIB(DEBUGLEV_VERBOSE_INFO, (TEXT("Get registry [dw]: VALUE: %d\n"), *value));
        }

    return retval;
}


// =========================================================================
// Returns an identifying number that the next FreeOTFE device mounted can 
// use as part of it's unique registry key
// Returns: An ID number
int RegGetNextRegNumber()
{
    int retval;
    REGDETAILS_BUILTIN detailsBuiltin;
    REGDETAILS_PROFILE detailsProfile;
    BOOL exists;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegGetNextRegNumber\n")));

    retval = 1;
    exists = TRUE;
    while (exists)
        {
        memset(&detailsBuiltin, 0, sizeof(detailsBuiltin));
        memset(&detailsProfile, 0, sizeof(detailsProfile));

        exists = RegDetailsGetBuiltinByID(retval, &detailsBuiltin) ||
                 RegDetailsGetProfileByID(retval, &detailsProfile);

        // Ditch the details; we don't care about them, only that we
        // can/can't get them
        RegDetailsFree(&detailsBuiltin, &detailsProfile, NULL);

        if (!(exists))
            {
            DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("Next registry ID: %d\n"), retval));
            break;
            }

        retval++;
        }
    
    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegGetNextRegNumber\n")));
    return retval;
}


// =========================================================================
// Note: detailsBuiltin.Profile is *ignored* - the profile is automatically
//       generated based on the ID passed in
BOOL RegDetailsSetAll(
                      int id, 
                      REGDETAILS_BUILTIN detailsBuiltin,
                      REGDETAILS_PROFILE detailsProfile
                     )
{
    BOOL retval = TRUE;
    HKEY hKey;
    WCHAR* keyBuiltin;
    WCHAR* keyProfile;
    WCHAR* profile;
    DWORD lpdwDisposition;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsSetAll\n")));

    profile = ProfileNameGenerate(id);
    RegKeyGenerate(id, &keyBuiltin, &keyProfile);

    // Write builtin registry data...
    if (RegCreateKeyEx( 
                     REGHIVE,
                     keyBuiltin,
                     0,
                     TEXT(""),  // Class
                     0,
                     0,
                     NULL,
                     &hKey,
                     &lpdwDisposition
                    ) == ERROR_SUCCESS)
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Writing values for builtin registry key...\n")));
        retval = retval && RegistrySetString(
                                 hKey, 
                                 REGNAME_BUILTIN_PREFIX,
                                 detailsBuiltin.Prefix
                                );
        retval = retval && RegistrySetString(
                                 hKey, 
                                 REGNAME_BUILTIN_DLL,
                                 detailsBuiltin.Dll
                                );
        retval = retval && RegistrySetString(
                                 hKey, 
                                 REGNAME_BUILTIN_FRIENDLYNAME,
                                 detailsBuiltin.FriendlyName
                                );
        retval = retval && RegistrySetDWORD(
                                 hKey, 
                                 REGNAME_BUILTIN_ORDER,
                                 detailsBuiltin.Order
                                );
        retval = retval && RegistrySetDWORD(
                                 hKey, 
                                 REGNAME_BUILTIN_IOCTL,
                                 detailsBuiltin.Ioctl
                                );
        retval = retval && RegistrySetString(
                                 hKey, 
                                 REGNAME_BUILTIN_ICLASS,
                                 detailsBuiltin.IClass
                                );
        retval = retval && RegistrySetString(
                                 hKey, 
                                 REGNAME_BUILTIN_PROFILE,
                                 profile
                                );

        RegCloseKey(hKey);
        }


    // Write profile registry data...
    if (RegCreateKeyEx( 
                     REGHIVE,
                     keyProfile,
                     0,
                     TEXT(""),  // Class
                     0,
                     0,
                     NULL,
                     &hKey,
                     &lpdwDisposition
                    ) == ERROR_SUCCESS)
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Writing values for profile registry key...\n")));
        retval = retval && RegistrySetString(
                                 hKey, 
                                 REGNAME_PROFILE_NAME,
                                 detailsProfile.Name
                                );
        retval = retval && RegistrySetString(
                                 hKey, 
                                 REGNAME_PROFILE_FOLDER,
                                 detailsProfile.Folder
                                );
        retval = retval && RegistrySetDWORD(
                                 hKey, 
                                 REGNAME_PROFILE_AUTOMOUNT,
                                 detailsProfile.AutoMount
                                );
        retval = retval && RegistrySetDWORD(
                                 hKey, 
                                 REGNAME_PROFILE_AUTOPART,
                                 detailsProfile.AutoPart
                                );
        retval = retval && RegistrySetDWORD(
                                 hKey, 
                                 REGNAME_PROFILE_AUTOFORMAT,
                                 detailsProfile.AutoFormat
                                );
        retval = retval && RegistrySetDWORD(
                                 hKey, 
                                 REGNAME_PROFILE_MOUNTFLAGS,
                                 detailsProfile.MountFlags
                                );

        RegCloseKey(hKey);
        }

    // Cleanup...
    SecZeroAndFreeWCHARMemory(keyProfile);
    SecZeroAndFreeWCHARMemory(keyBuiltin);
    SecZeroAndFreeWCHARMemory(profile);
     
    if (!(retval))
        {
        // Failed; clear down any registry entries which may have been
        // created
        // Ignore return value, as there's really nothing we can do if it
        // fails; we're already returning a failure
        RegDetailsDeleteAllByID(id, TRUE);
        }

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsSetAll\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetBuiltinByID(int id, REGDETAILS_BUILTIN *detailsBuiltin)
{
    BOOL retval = FALSE;
    WCHAR* key;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsGetBuiltinByID\n")));

    RegKeyGenerate(id, &key, NULL);

    retval = RegDetailsGetBuiltinByKey(key, detailsBuiltin);

    // Cleanup...
    SecZeroAndFreeWCHARMemory(key);
     
    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsGetBuiltinByID\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetBuiltinByKey(WCHAR* key, REGDETAILS_BUILTIN *detailsBuiltin)
{
    BOOL retval = FALSE;
    HKEY hKey;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsGetBuiltinByKEY\n")));

    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("a-Opening key: %s\n"), key));
    if (RegOpenKeyEx( 
                     REGHIVE,
                     key,
                     0,
                     0,
                     &hKey
                    ) == ERROR_SUCCESS)
        {
        retval = RegDetailsGetBuiltinByHKEY(hKey, detailsBuiltin);
        RegCloseKey(hKey);
        }
     
    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsGetBuiltinByKEY\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetBuiltinByHKEY(HKEY hKey, REGDETAILS_BUILTIN *detailsBuiltin)
{
    BOOL retval = FALSE;

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_ENTER, (TEXT("RegDetailsGetBuiltinByHKEY\n")));

    if (hKey != NULL)
        {
        retval = TRUE;

        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_BUILTIN_PREFIX,
                                 &(detailsBuiltin->Prefix)
                                );
        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_BUILTIN_DLL,
                                 &(detailsBuiltin->Dll)
                                );
        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_BUILTIN_FRIENDLYNAME,
                                 &(detailsBuiltin->FriendlyName)
                                );

        retval = retval && RegistryGetDWORD(
                                 hKey, 
                                 REGNAME_BUILTIN_ORDER,
                                 &(detailsBuiltin->Order)
                                );
        retval = retval && RegistryGetDWORD(
                                 hKey, 
                                 REGNAME_BUILTIN_IOCTL,
                                 &(detailsBuiltin->Ioctl)
                                );

        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_BUILTIN_ICLASS,
                                 &(detailsBuiltin->IClass)
                                );
        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_BUILTIN_PROFILE,
                                 &(detailsBuiltin->Profile)
                                );
        
        }

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_EXIT, (TEXT("RegDetailsGetBuiltinByHKEY\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetProfileByID(
                          int id, 
                          REGDETAILS_PROFILE* detailsProfile
                         )
{
    BOOL retval = FALSE;
    WCHAR* key;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsGetProfileByID\n")));

    RegKeyGenerate(id, NULL, &key);

    retval = RegDetailsGetProfileByKey(key, detailsProfile);

    // Cleanup...
    SecZeroAndFreeWCHARMemory(key);
 
    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsGetProfileByID\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetProfileByKey(
                          WCHAR* key, 
                          REGDETAILS_PROFILE* detailsProfile
                         )
{
    BOOL retval = FALSE;
    HKEY hKey;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsGetProfileByKey\n")));

    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("b-Opening key: %s\n"), key));
    if (RegOpenKeyEx( 
                     REGHIVE,
                     key,
                     0,
                     0,
                     &hKey
                    ) == ERROR_SUCCESS)
        {
        retval = RegDetailsGetProfileByHKEY(hKey, detailsProfile);
        RegCloseKey(hKey);
        }

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsGetProfileByKey\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetProfileByHKEY(
                          HKEY hKey, 
                          REGDETAILS_PROFILE* detailsProfile
                         )
{
    BOOL retval = FALSE;

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_ENTER, (TEXT("RegDetailsGetProfileByHKEY\n")));

    if (hKey != NULL)
        {
        retval = TRUE;

        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_PROFILE_NAME,
                                 &(detailsProfile->Name)
                                );
        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_PROFILE_FOLDER,
                                 &(detailsProfile->Folder)
                                );
        retval = retval && RegistryGetDWORD(
                                 hKey, 
                                 REGNAME_PROFILE_AUTOMOUNT,
                                 &(detailsProfile->AutoMount)
                                );
        retval = retval && RegistryGetDWORD(
                                 hKey, 
                                 REGNAME_PROFILE_AUTOPART,
                                 &(detailsProfile->AutoPart)
                                );
        retval = retval && RegistryGetDWORD(
                                 hKey, 
                                 REGNAME_PROFILE_AUTOFORMAT,
                                 &(detailsProfile->AutoFormat)
                                );
        retval = retval && RegistryGetDWORD(
                                 hKey, 
                                 REGNAME_PROFILE_MOUNTFLAGS,
                                 &(detailsProfile->MountFlags)
                                );
        }
 
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_EXIT, (TEXT("RegDetailsGetProfileByHKEY\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetActiveByKey(
    WCHAR* key, 
//    BOOL mustInclHnd,
    REGDETAILS_ACTIVE* detailsActive
)
{
    BOOL retval = FALSE;
    HKEY hKey;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsGetActiveByKey\n")));

    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("c-Opening key: %s\n"), key));
    if (RegOpenKeyEx( 
                     REGHIVE,
                     key,
                     0,
                     0,
                     &hKey
                    ) == ERROR_SUCCESS)
        {
        retval = RegDetailsGetActiveByHKEY(
                                           hKey, 
                                           //mustInclHnd, 
                                           detailsActive
                                          );
        RegCloseKey(hKey);
        }

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsGetActiveByKey\n")));
    return retval;
}


// =========================================================================
// mustInclHnd - If set, those entries without a value for "Hnd" will be
//               ignored
// !NOTE!
// detailsActive->Mountpoint will be set to NULL if a FreeOTFE mountpoint
// can't be found
BOOL RegDetailsGetActiveByHKEY(
    HKEY hKey, 
//    BOOL mustInclHnd,
    REGDETAILS_ACTIVE* detailsActive
)
{
    BOOL retval = FALSE;
    REGDETAILS_BUILTIN detailsBuiltin;
    REGDETAILS_PROFILE detailsProfile;

    DEBUGOUTLIB(DEBUGLEV_VERBOSE_ENTER, (TEXT("RegDetailsGetActiveByHKEY\n")));

    if (hKey != NULL)
        {
        retval = TRUE;

        // The "Hnd" key appears to be set to the same value as returned by
        // ActivateDeviceEx, however this doesn't apear documented; hence we set the
        // handle explicitly on the device so we can retrieve it the device there
        // later
        //if (!(RegistryGetDWORD(
        //                       hKey, 
        //                       REGNAME_ACTIVE_HND,
        //                       &(detailsActive->Hnd)
        //                      )))
        //    {
        //    if (mustInclHnd) 
        //        {
        //        retval = FALSE;
        //        }
        //    }        

        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_ACTIVE_KEY,
                                 &(detailsActive->Key)
                                );
        retval = retval && RegistryGetString(
                                 hKey, 
                                 REGNAME_ACTIVE_NAME,
                                 &(detailsActive->Name)
                                );

        // Identify mountpoint, if possible.
        detailsActive->Mountpoint = NULL;
        if (RegDetailsGetAllByKey(
                                  detailsActive->Key, 
                                  &detailsBuiltin,
                                  &detailsProfile
                                 ))
            {
            if (
                (wcscmp(detailsBuiltin.Prefix, DRIVER_PREFIX) == 0) &&
                (wcscmp(detailsProfile.Name, DRIVER_PROFILE_NAME) == 0)
               )
                {
                // +1 to include terminating NULL
                detailsActive->Mountpoint = calloc(
                                                   (wcslen(detailsProfile.Folder) + 1),
                                                   sizeof(*(detailsActive->Mountpoint))
                                                  );
                if (detailsActive->Mountpoint == NULL)
                    {
                    DEBUGOUTLIB(DEBUGLEV_ERROR, (TEXT("Unable to calloc memory to store mountpoint\n")));
                    retval = FALSE;
                    }
                else
                    {
                    wcscpy(detailsActive->Mountpoint, detailsProfile.Folder);
                    }

                }

            RegDetailsFree(&detailsBuiltin, &detailsProfile, NULL);
            }
        }
 
    DEBUGOUTLIB(DEBUGLEV_VERBOSE_EXIT, (TEXT("RegDetailsGetActiveByHKEY\n")));
    return retval;
}


// =========================================================================
void RegDetailsFree(
                    REGDETAILS_BUILTIN *detailsBuiltin,
                    REGDETAILS_PROFILE *detailsProfile,
                    REGDETAILS_ACTIVE *detailsActive
                   )
{
    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsFree\n")));

    // Builtin...
    if (detailsBuiltin != NULL)
        {
        SecZeroAndFreeWCHARMemory(detailsBuiltin->Prefix);
        detailsBuiltin->Prefix = NULL;
        SecZeroAndFreeWCHARMemory(detailsBuiltin->Dll);
        detailsBuiltin->Dll = NULL;
        SecZeroAndFreeWCHARMemory(detailsBuiltin->FriendlyName);
        detailsBuiltin->FriendlyName = NULL;
        detailsBuiltin->Order = 0;
        detailsBuiltin->Ioctl = 0;
        SecZeroAndFreeWCHARMemory(detailsBuiltin->IClass);
        detailsBuiltin->IClass = NULL;
        SecZeroAndFreeWCHARMemory(detailsBuiltin->Profile);
        detailsBuiltin->Profile = NULL;

        // Belt 'n braces...
        SecZeroMemory(detailsBuiltin, sizeof(*detailsBuiltin));
        }

    // Profile...
    if (detailsProfile != NULL)
        {
        SecZeroAndFreeWCHARMemory(detailsProfile->Name);
        detailsProfile->Name = NULL;
        SecZeroAndFreeWCHARMemory(detailsProfile->Folder);
        detailsProfile->Folder = NULL;
        detailsProfile->AutoMount = 0;
        detailsProfile->AutoPart = 0;
        detailsProfile->AutoFormat = 0;
        detailsProfile->MountFlags = 0;

        // Belt 'n braces...
        SecZeroMemory(detailsProfile, sizeof(*detailsProfile));
        }

    // Active...
    if (detailsActive != NULL)
        {
        // The "Hnd" key appears to be set to the same value as returned by
        // ActivateDeviceEx, however this doesn't apear documented; hence we set the
        // handle explicitly on the device so we can retrieve it the device there
        // later
        //detailsActive->Hnd = 0;

        SecZeroAndFreeWCHARMemory(detailsActive->Key);
        detailsActive->Key = NULL;
        SecZeroAndFreeWCHARMemory(detailsActive->Name);
        detailsActive->Name = NULL;
        SecZeroAndFreeWCHARMemory(detailsActive->Mountpoint);
        detailsActive->Mountpoint = NULL;

        // Belt 'n braces...
        SecZeroMemory(detailsActive, sizeof(*detailsActive));
        }

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsFree\n")));
}


// =========================================================================
BOOL RegDetailsDeleteAllByKey(WCHAR* builtinKey, BOOL force)
{
    BOOL retval = TRUE;
    WCHAR* fullProfileKey;
    REGDETAILS_BUILTIN detailsBuiltin;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsDeleteAllByKey\n")));

    // Get corresponding profile key...
    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Getting corresponding profile key...\n")));
    fullProfileKey = NULL;
    if (RegDetailsGetBuiltinByKey(builtinKey, &detailsBuiltin))
        {
        // The "Profile" member of detailsBuiltin only lists the profile, not
        // the registry key.
        fullProfileKey = calloc(
                                wcslen(REGKEY_PROFILE) +
                                wcslen(REGKEY_SEPARATOR) + 
                                wcslen(detailsBuiltin.Profile) + 
                                1,  // +1 for terminating NULL char
                                sizeof(*fullProfileKey)
                               );

        if (fullProfileKey == NULL)
            {
            DEBUGOUTLIB(DEBUGLEV_ERROR, (TEXT("Unable to malloc storage for full profile key\n")));
            }
        else
            {
            // Build up registry key...
            wcscpy(fullProfileKey, REGKEY_PROFILE);
            wcscat(fullProfileKey, REGKEY_SEPARATOR);
            wcscat(fullProfileKey, detailsBuiltin.Profile);
            DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Profile key from builtin: %ls\n"), fullProfileKey));
            }
        }


    // Carry out deletion of builtin and profile keys...

    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Deleting builtin regkey...\n")));
    if (RegDeleteKey(
                     REGHIVE, 
                     builtinKey
                    ) != ERROR_SUCCESS)
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Failed to delete builtin registry key\n")));
        retval = FALSE;
        }

    if (
        retval ||
        force
       )
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Deleting profile regkey...\n")));
        if (RegDeleteKey(
                         REGHIVE, 
                         fullProfileKey
                        ) != ERROR_SUCCESS)
            {
            DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Failed to delete profile registry key\n")));
            retval = FALSE;
            }
        }

    SecZeroAndFreeWCHARMemory(fullProfileKey);        
    SecZeroMemory(&detailsBuiltin, sizeof(detailsBuiltin));

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsDeleteAllByKey\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsDeleteAllByID(int id, BOOL force)
{
    BOOL retval = TRUE;
    WCHAR* keyBuiltin;
    WCHAR* keyProfile;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsDeleteAllByID\n")));

    RegKeyGenerate(id, &keyBuiltin, &keyProfile);

    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Deleting builtin regkey...\n")));
    if (RegDeleteKey(
                     REGHIVE, 
                     keyBuiltin
                    ) != ERROR_SUCCESS)
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Failed to delete builtin registry key\n")));
        retval = FALSE;
        }

    if (
        retval ||
        force
       )
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Deleting profile regkey...\n")));
        if (RegDeleteKey(
                         REGHIVE, 
                         keyProfile
                        ) != ERROR_SUCCESS)
            {
            DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Failed to delete profile registry key\n")));
            retval = FALSE;
            }
        }

    SecZeroAndFreeWCHARMemory(keyBuiltin);
    SecZeroAndFreeWCHARMemory(keyProfile);

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsDeleteAllByID\n")));
    return retval;
}


// =========================================================================
void RegKeyGenerate(int id, WCHAR** keyBuiltin, WCHAR** keyProfile)
{
    WCHAR* profileName;
    WCHAR* registryRootKey;
    WCHAR* builtinKey;
    HKEY hKey;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegKeyGenerate\n")));

    profileName = ProfileNameGenerate(id);

    if (keyBuiltin != NULL)
        {
        // Get builtin key from registry
        registryRootKey = NULL;
        if (RegOpenKeyEx( 
                         REGHIVE,
                         REGKEY_DRIVERS,
                         0,
                         0,
                         &hKey
                        ) == ERROR_SUCCESS)
            {
            RegistryGetString(
                              hKey, 
                              REGNAME_DRIVERROOTKEY,
                              &registryRootKey
                             );
            }

        if (registryRootKey == NULL)
            {
            builtinKey = REGKEY_BUILTIN_FALLBACK;
            DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Using fallback rootkey: %ls\n"), builtinKey));
            }
        else
            {
            builtinKey = registryRootKey;
            DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Using driver rootkey from registry: %ls\n"), builtinKey));
            }

        *keyBuiltin = calloc(
                             (
                              wcslen(builtinKey) + 
                              wcslen(REGKEY_SEPARATOR) + 
                              wcslen(profileName) + 
                              1 // Zero terminator
                             ),
                             sizeof(WCHAR)
                            );

        wsprintf(
                 *keyBuiltin,              
                 TEXT("%s%s%s"),
                 builtinKey, 
                 REGKEY_SEPARATOR, 
                 profileName
                );

        SecZeroAndFreeWCHARMemory(registryRootKey);
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Full builtin registry keyname: %s\n"), *keyBuiltin));
        }

    if (keyProfile != NULL)
        {
        *keyProfile = calloc(
                             (
                              wcslen(REGKEY_PROFILE) + 
                              wcslen(REGKEY_SEPARATOR) + 
                              wcslen(profileName) + 
                              1 // Zero terminator
                             ),
                             sizeof(WCHAR)
                            );

        wsprintf(
                 *keyProfile,              
                 TEXT("%s%s%s"),
                 REGKEY_PROFILE, 
                 REGKEY_SEPARATOR, 
                 profileName
                );

        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Full profile registry keyname: %s\n"), *keyProfile));
        }

    SecZeroAndFreeWCHARMemory(profileName);
    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegKeyGenerate\n")));
}


// =========================================================================
WCHAR* ProfileNameGenerate(int id)
{
    WCHAR* retval;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("ProfileNameGenerate\n")));

    retval = calloc(
                    (
                      wcslen(REGKEY_KEY_PREFIX) + 
                      5 // Zero terminator and number padding (overestimate)
                    ),
                    sizeof(WCHAR) 
                   );

    wsprintf(
             retval,              
             REGKEY_KEY_PREFIX,
             id
            );

    DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Profile name: %s\n"), retval));
    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("ProfileNameGenerate\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetAllByID(
                      int id, 
                      REGDETAILS_BUILTIN* detailsBuiltin,
                      REGDETAILS_PROFILE* detailsProfile
                     )
{
    BOOL retval = TRUE;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsGetAllByID\n")));

    retval = RegDetailsGetBuiltinByID(id, detailsBuiltin);
    if (retval)
        {
        retval = RegDetailsGetProfileByID(
                                      id, 
                                      detailsProfile
                                     );
        }

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsGetAllByID\n")));
    return retval;
}


// =========================================================================
BOOL RegDetailsGetAllByKey(
                      WCHAR* builtinKey,
                      REGDETAILS_BUILTIN* detailsBuiltin,
                      REGDETAILS_PROFILE* detailsProfile
                     )
{
    BOOL retval = TRUE;
    WCHAR* fullProfileKey;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsGetAllByKey\n")));

    retval = RegDetailsGetBuiltinByKey(builtinKey, detailsBuiltin);
    if (retval)
        {
        // The "Profile" member of detailsBuiltin only lists the profile, not
        // the registry key.
        fullProfileKey = calloc(
                                wcslen(REGKEY_PROFILE) +
                                wcslen(REGKEY_SEPARATOR) + 
                                wcslen(detailsBuiltin->Profile) + 
                                1,  // +1 for terminating NULL char
                                sizeof(*fullProfileKey)
                               );

        // Build up registry key...
        wcscpy(fullProfileKey, REGKEY_PROFILE);
        wcscat(fullProfileKey, REGKEY_SEPARATOR);
        wcscat(fullProfileKey, detailsBuiltin->Profile);
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Profile key from builtin: %ls\n"), fullProfileKey));

        retval = RegDetailsGetProfileByKey(
                                           fullProfileKey, 
                                           detailsProfile
                                          );

        SecZeroAndFreeWCHARMemory(fullProfileKey);
        }

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsGetAllByKey\n")));
    return retval;
}


// =========================================================================
// Free off WCHARS associated with all of the elements in "detailsActive"
void RegDetailsFreeAllActive(
    int cnt, // Number of elements in detailsActive array
    REGDETAILS_ACTIVE* detailsActive  // Actually an array of REGDETAILS_ACTIVE
)
{
    int i;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsFreeAllActive\n")));

    if (detailsActive != NULL)
        {
        for (i = 0; i < cnt; i++)
            {
            RegDetailsFree(
                       NULL,
                       NULL,
                       &(detailsActive[i])
                      );
            }
        }

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsFreeAllActive\n")));
}


// =========================================================================
// Returns -1 on failure
// Set either (or both) of detailsActive/detailsActive to 0/NULL to just
// return a count of the number that would otherwise be returned
// If the buffer supplied is too small, this function will only fill in the
// first (detailsActiveSize / sizeof(*detailsActive)) entries - and will 
// return *that* number
// IMPORTANT: It is the callers responsibility to free off the WCHAR*s
//            populated in detailsActive (e.g. call RegDetailsFreeAllActive)
int RegDetailsGetAllActive(
    int detailsActiveSize,  // Size in bytes of detailsActive buffer
                            // On exit, this will be set to the amount of the
                            // buffer used
    REGDETAILS_ACTIVE *detailsActive  // Actually an array of REGDETAILS_ACTIVE
)
{
    int cntFound;
    HKEY hKey;
    DWORD dwIndex;
    WCHAR buffer[512];  // Just needs to be big enough for the registry key
                        // (just the final part). In practice, this is a
                        // number, so a couple of bytes should do.
    DWORD tmpBufSize;
    REGDETAILS_ACTIVE currDetailsActive;  
    BOOL returningDetails;
    WCHAR* tmpEnumFullKey;
    int bufferUsed;

    DEBUGOUTLIB(DEBUGLEV_ENTER, (TEXT("RegDetailsGetAllActive\n")));

    bufferUsed = 0;
    cntFound = -1;
    if (RegOpenKeyEx( 
                     REGHIVE,
                     REGKEY_DRIVERS_ACTIVE,
                     0,
                     0,
                     &hKey
                    ) != ERROR_SUCCESS)
        {
        DEBUGOUTLIB(DEBUGLEV_ERROR, (TEXT("Failed to open active driers key: %s\n"), REGKEY_DRIVERS_ACTIVE));
        }
    else
        {
        cntFound = 0;
        dwIndex = 0;
        tmpBufSize = sizeof(buffer);
        while (RegEnumKeyEx(
                         hKey, 
                         dwIndex, 
                         buffer,
                         &tmpBufSize,
                         NULL,
                         NULL,
                         NULL,
                         NULL
                        ) == ERROR_SUCCESS)
            {
            tmpEnumFullKey = calloc(
                                    (
                                     wcslen(REGKEY_DRIVERS_ACTIVE)+
                                     wcslen(REGKEY_SEPARATOR)+
                                     wcslen(buffer)+
                                     1 // NULL terminator
                                    ),
                                    sizeof(*tmpEnumFullKey)
                                   );
            if (tmpEnumFullKey == NULL)
                {
                DEBUGOUTLIB(DEBUGLEV_ERROR, (TEXT("Unable to calloc memory for registry key\n")));
                }
            else
                {                
                wcscpy(tmpEnumFullKey, REGKEY_DRIVERS_ACTIVE);
                wcscat(tmpEnumFullKey, REGKEY_SEPARATOR);
                wcscat(tmpEnumFullKey, buffer);
                if (RegDetailsGetActiveByKey(
                                             tmpEnumFullKey, 
                                             //TRUE, 
                                             &currDetailsActive
                                            ))
                    {
                    returningDetails = FALSE;
                    if (currDetailsActive.Mountpoint != NULL)
                        {
                        cntFound++;
                        if (
                            (detailsActiveSize > 0) &&
                            (detailsActive != NULL)
                           )
                            {
                            // If we still have enough buffer left...
                            if ((DWORD)cntFound <= (detailsActiveSize / sizeof(*detailsActive)))
                                {
                                detailsActive[(cntFound-1)] = currDetailsActive;
                                returningDetails = TRUE;
                                bufferUsed += sizeof(currDetailsActive);

                                // If the buffer can't store any more, not 
                                // point in continuing...
                                if (bufferUsed >= detailsActiveSize)
                                    {
                                    break;
                                    }

                                }
                            }
                        }

                    // Free off unused structure, if it's not being returned...
                    if (!(returningDetails))
                        {
                        RegDetailsFree(
                                       NULL,
                                       NULL,
                                       &currDetailsActive
                                      );
                        }
                    }

                SecZeroAndFreeWCHARMemory(tmpEnumFullKey);
                }


            // Setup ready for next...
            dwIndex++;
            // Reset, as changed by the call to RegEnumKeyEx(...)
            tmpBufSize = sizeof(buffer);
            }

        RegCloseKey(hKey);
        }

    DEBUGOUTLIB(DEBUGLEV_EXIT, (TEXT("RegDetailsGetAllActive\n")));
    return cntFound;
}


// =========================================================================
BOOL RegAssociateFileExtension(
    WCHAR* FileExtension, // *Without* leading "."
    WCHAR* FileType, // Short descriptor
    WCHAR* FileDescription,
    WCHAR* OpenCommand,
    WCHAR* IconFile,
    int IconID
)
{
    BOOL retval;
    HKEY hKey;
    DWORD lpdwDisposition;
    WCHAR tmpRegPath[1024];
    WCHAR tmpDefaultIconID[1024];

    retval = TRUE;

    if (retval)
        {
        _snwprintf(
                   tmpRegPath, 
                   (sizeof(tmpRegPath) / sizeof(tmpRegPath[0])),
                   REGKEY_FILEASSOC_EXTN,
                   FileExtension
                  );

        retval = (RegCreateKeyEx( 
                     REGHIVE_FILEASSOC,
                     tmpRegPath,
                     0,
                     TEXT(""),  // Class
                     0,
                     0,
                     NULL,
                     &hKey,
                     &lpdwDisposition
                    ) == ERROR_SUCCESS);
        if (retval)
            {
            retval = retval && RegistrySetString(
                                     hKey, 
                                     NULL,
                                     FileType
                                    );
            RegCloseKey(hKey);
            }
        }       

    if (retval)
        {
        _snwprintf(
                   tmpRegPath, 
                   (sizeof(tmpRegPath) / sizeof(tmpRegPath[0])),
                   REGKEY_FILEASSOC_FILETYPE,
                   FileType
                  );

        retval = (RegCreateKeyEx( 
                     REGHIVE_FILEASSOC,
                     tmpRegPath,
                     0,
                     TEXT(""),  // Class
                     0,
                     0,
                     NULL,
                     &hKey,
                     &lpdwDisposition
                    ) == ERROR_SUCCESS);
        if (retval)
            {
            retval = retval && RegistrySetString(
                                     hKey, 
                                     NULL,
                                     FileDescription
                                    );
            RegCloseKey(hKey);
            }
        }       

    if (retval)
        {
        _snwprintf(
                   tmpRegPath, 
                   (sizeof(tmpRegPath) / sizeof(tmpRegPath[0])),
                   REGKEY_FILEASSOC_COMMAND,
                   FileType
                  );

        retval = (RegCreateKeyEx( 
                     REGHIVE_FILEASSOC,
                     tmpRegPath,
                     0,
                     TEXT(""),  // Class
                     0,
                     0,
                     NULL,
                     &hKey,
                     &lpdwDisposition
                    ) == ERROR_SUCCESS);
        if (retval)
            {
            retval = retval && RegistrySetString(
                                     hKey, 
                                     NULL,
                                     OpenCommand
                                    );
            RegCloseKey(hKey);
            }
        }       

    if (retval)
        {
        if (IconFile != NULL)
            {
            _snwprintf(
                       tmpRegPath, 
                       (sizeof(tmpRegPath) / sizeof(tmpRegPath[0])),
                       REGKEY_FILEASSOC_DEFAULTICON,
                       FileType
                      );

            retval = (RegCreateKeyEx( 
                         REGHIVE_FILEASSOC,
                         tmpRegPath,
                         0,
                         TEXT(""),  // Class
                         0,
                         0,
                         NULL,
                         &hKey,
                         &lpdwDisposition
                        ) == ERROR_SUCCESS);
            }
        if (retval)
            {
            _snwprintf(
                       tmpDefaultIconID, 
                       (sizeof(tmpDefaultIconID) / sizeof(tmpDefaultIconID[0])),
                       REGVALUE_FILEASSOC_DEFAULTICON,
                       IconFile,
                       IconID
                      );

            retval = retval && RegistrySetString(
                                     hKey, 
                                     NULL,
                                     tmpDefaultIconID
                                    );
            RegCloseKey(hKey);

            SecZeroMemory(tmpDefaultIconID, sizeof(tmpDefaultIconID));
            }
        }       

    SecZeroMemory(tmpRegPath, sizeof(tmpRegPath));

    if (!(retval))
        {
        RegDisassociateFileExtension(FileExtension, FileType);
        }

    return retval;
}


// =========================================================================
void RegDisassociateFileExtension(
    WCHAR* FileExtension, // *Without* leading "."
    WCHAR* FileType // Short descriptor
)
{
    WCHAR tmpRegPath[1024];

    _snwprintf(
               tmpRegPath, 
               (sizeof(tmpRegPath) / sizeof(tmpRegPath[0])),
               REGKEY_FILEASSOC_EXTN,
               FileExtension
              );
    if (RegDeleteKey(
                     REGHIVE_FILEASSOC, 
                     tmpRegPath
                    ) != ERROR_SUCCESS)
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Failed to %s registry key\n"), tmpRegPath));
        }

    _snwprintf(
               tmpRegPath, 
               (sizeof(tmpRegPath) / sizeof(tmpRegPath[0])),
               REGKEY_FILEASSOC_FILETYPE,
               FileType
              );
    if (RegDeleteKey(
                     REGHIVE_FILEASSOC, 
                     tmpRegPath
                    ) != ERROR_SUCCESS)
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, (TEXT("Failed to %s registry key\n"), tmpRegPath));
        }

}


// =========================================================================
// Either "FileExtension" or "FileType" may have NULL passed to disregard
// Returns: TRUE if *either* of the passed in parameters are set in the
//          registry
BOOL RegIsFileExtensionAssociated(
    WCHAR* FileExtension, // *Without* leading "."
    WCHAR* FileType // Short descriptor
)
{
    BOOL retval;
    HKEY hKey;
    WCHAR tmpRegPath[1024];

    retval = FALSE;

    if (!(retval))
        {
        _snwprintf(
                   tmpRegPath, 
                   (sizeof(tmpRegPath) / sizeof(tmpRegPath[0])),
                   REGKEY_FILEASSOC_EXTN,
                   FileExtension
                  );

        if (RegOpenKeyEx( 
                         REGHIVE_FILEASSOC,
                         tmpRegPath,
                         0,
                         0,
                         &hKey
                        ) == ERROR_SUCCESS)
            {
            RegCloseKey(hKey);
            retval = TRUE;
            }
        }       

    if (!(retval))
        {
        _snwprintf(
                   tmpRegPath, 
                   (sizeof(tmpRegPath) / sizeof(tmpRegPath[0])),
                   REGKEY_FILEASSOC_FILETYPE,
                   FileType
                  );

        if (RegOpenKeyEx( 
                         REGHIVE_FILEASSOC,
                         tmpRegPath,
                         0,
                         0,
                         &hKey
                        ) == ERROR_SUCCESS)
            {
            RegCloseKey(hKey);
            retval = TRUE;
            }
        }       

    return retval;
}


// =========================================================================
// =========================================================================

