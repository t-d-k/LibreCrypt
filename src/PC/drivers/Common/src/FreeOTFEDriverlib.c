// Description: Common FreeOTFE library functions
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEDriverlib.h"
#include "FreeOTFElib.h"
#include "FreeOTFEDebug.h"


// =========================================================================
// Create device dir
NTSTATUS
CreateDeviceDir(
    IN   PWCHAR DirName,
    OUT  PHANDLE DirHandle
)
{
    NTSTATUS status = STATUS_SUCCESS;
    UNICODE_STRING devDirName;
    OBJECT_ATTRIBUTES dirObjAttribs;
    int i;  
        
    DEBUGOUTLIB(DEBUGLEV_ENTER, ("CreateDeviceDir\n"));

    DEBUGOUTLIB(DEBUGLEV_INFO, ("Doing dir name... %ls\n", DirName));
    // Note: We don't need to free devDirName, since it's buffer will be 
    // pointing to a const
    RtlInitUnicodeString(&devDirName, DirName);

    DEBUGOUTLIB(DEBUGLEV_INFO, ("Initilizing dir obj attribs...\n"));
    InitializeObjectAttributes(
            &dirObjAttribs,
            &devDirName,
            (OBJ_PERMANENT | OBJ_OPENIF),
            NULL, // Root dir not needed since it's a fully
                  // qualified name
            NULL  // Security attribs
            );
    
    
    DEBUGOUTLIB(DEBUGLEV_INFO, ("Creating dir object...\n"));
    status = ZwCreateDirectoryObject(
                DirHandle,
                DIRECTORY_ALL_ACCESS,
                &dirObjAttribs
                );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTLIB(DEBUGLEV_ERROR, ("Unable to create/open dir object\n"));
        }
    else
        {
        DEBUGOUTLIB(DEBUGLEV_INFO, ("Making dir object temporary...\n"));
        status = ZwMakeTemporaryObject(*DirHandle);
        if (!(NT_SUCCESS(status)))
            {
            DEBUGOUTLIB(DEBUGLEV_ERROR, ("Unable to make dir object temporary?!.\n"));
            }
        }
        
        
    DEBUGOUTLIB(DEBUGLEV_EXIT, ("CreateDeviceDir\n"));
    return status;
}


// =========================================================================
// Concatenate a "{zzzzzzzz-zzzz-zzzz-zzzz-zzzzzzzzzzzz}" text representation
// of the GUID given onto the end of the specified UNICODE_STRING
// Note: "unicodeString" must have sufficient space to store the text
//       representation of the GUID.
// Returns: STATUS_SUCCESS, or STATUS_BUFFER_TOO_SMALL if "unicodeString"'s
//          buffer was too small
NTSTATUS
AppendGUIDToUnicodeString(
    IN      GUID  useGUID,
    IN OUT  PUNICODE_STRING unicodeString
)
{
    NTSTATUS status = STATUS_SUCCESS;
    UNICODE_STRING unicodeRep;

    DEBUGOUTLIB(DEBUGLEV_ENTER, ("AppendGUIDToUnicodeString\n"));

    // We add on an extra WCHAR since swprintf because although the UNICODE_STRING string
    // doesn't require it, swprintf adds a terminating NULL anyway
    unicodeRep.MaximumLength = GUID_STRING_REP_UNICODE_BYTE_LENGTH + sizeof(WCHAR);
    unicodeRep.Buffer = FREEOTFE_MEMALLOC(unicodeRep.MaximumLength);    
    RtlZeroMemory(unicodeRep.Buffer, unicodeRep.MaximumLength);
    
    unicodeRep.Length = (USHORT)FREEOTFE_SWPRINTF(
                                         unicodeRep.Buffer,
                                         (unicodeRep.MaximumLength / sizeof(*(unicodeRep.Buffer))),
                                         L"{%.8X-%.4X-%.4X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X}",
                                         useGUID.Data1,
                                         useGUID.Data2,
                                         useGUID.Data3,
                                         useGUID.Data4[0],
                                         useGUID.Data4[1],
                                         useGUID.Data4[2],
                                         useGUID.Data4[3],
                                         useGUID.Data4[4],
                                         useGUID.Data4[5],
                                         useGUID.Data4[6],
                                         useGUID.Data4[7]
                                     );

    // swprintf returns the number of WCHARs, not the length in bytes
    unicodeRep.Length = unicodeRep.Length * sizeof(WCHAR);

    status = RtlAppendUnicodeStringToString(unicodeString, &unicodeRep);


    FREEOTFE_FREE(unicodeRep.Buffer);

    DEBUGOUTLIB(DEBUGLEV_EXIT, ("AppendGUIDToUnicodeString\n"));

    return status;
}


// =========================================================================
// =========================================================================


