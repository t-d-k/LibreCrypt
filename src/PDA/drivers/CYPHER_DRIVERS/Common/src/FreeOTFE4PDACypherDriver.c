// Description: FreeOTFE Cypher Device Driver
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include "FreeOTFEPlatform.h"

#include <stdio.h>

#include "SDUGeneral.h"

#include "FreeOTFE4PDACypherDriver.h"
#include "FreeOTFECypherAPICommon.h"
#include "FreeOTFEDebug.h"
#include "FreeOTFElib.h"
#include "FreeOTFE4PDAlib.h"
#include "FreeOTFECypherImpl.h"
#include "FreeOTFE4PDACypherAPI.h"


// =========================================================================
BOOL WINAPI DllMain(
  HANDLE hinstDLL, 
  DWORD dwReason, 
  LPVOID lpvReserved
)
{
    BOOL retval = TRUE;
    int majorVersion;
    int minorVersion;
    int revisionVersion;
    int buildVersion;
#if DBG
    static BOOL setDebugLevel = FALSE;
    // Default to all on
//    ULONG default_DebugLevel  = 0xFFFFFFFF;  
    // Default to all except verbose debug
    ULONG default_DebugLevel  = 
                                DEBUGLEV_ERROR |
                                DEBUGLEV_WARN  |
                                DEBUGLEV_INFO  |
                                DEBUGLEV_ENTER |
                                DEBUGLEV_EXIT;
    DWORD useDebugLevel;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("DllMain\n")));

    if (!(setDebugLevel))
        {
        useDebugLevel = ReadDebugLevelFromFile(DEBUGLEVEL_FILE);
        if (useDebugLevel == FREEOTFE_DEBUG_LEVEL_NOT_READ)
            {
            useDebugLevel = default_DebugLevel;
            }

        FreeOTFEDebugLevel = useDebugLevel;
        setDebugLevel  = TRUE;
        }

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Debug level   : %d\n"), FreeOTFEDebugLevel));
#endif

    if (!(SDUGetVersionInfo(
				            NULL,
				            &majorVersion,
				            &minorVersion, 
				            &revisionVersion, 
				            &buildVersion
				           )))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Driver version: <unable to determine>\n")));
        }
    else
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Driver version: v%02d.%02d.%02d.%04d\n"),
                                        majorVersion, 
                                        minorVersion,
                                        revisionVersion,
                                        buildVersion
                                       ));
        }
        

    switch (dwReason)
        {
        case DLL_PROCESS_ATTACH:
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("DLL_PROCESS_ATTACH\n")));
            // We really don't care about getting
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls; disable
            DisableThreadLibraryCalls(hinstDLL);
            break;
            }

        case DLL_THREAD_ATTACH:
            {
            // This should never be reached; we disable thread 
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls
            DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("DLL_THREAD_ATTACH\n")));
            break;
            }

        case DLL_THREAD_DETACH:
            {
            // This should never be reached; we disable thread 
            // DLL_THREAD_ATTACH/DLL_THREAD_DETACH calls
            DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("DLL_THREAD_DETACH\n")));
            break;
            }

        case DLL_PROCESS_DETACH:
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("DLL_PROCESS_DETACH\n")));
            break;
            }

        }

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("DllMain\n")));
    return retval;
}


// =========================================================================
// IOCTL to get cypher driver identification
DWORD
CypherIdentifyDriver(
    DIOC_CYPHER_IDENTIFYDRIVER* Buffer
)
{
    DWORD status = STATUS_SUCCESS;
    CYPHER_DRIVER_INFO_v3 tmpDriverInfo;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("IdentifyDriver\n")));

    // Populate output buffer
    ImpCypherDriverExtDetailsInit_v3(&tmpDriverInfo);

    Buffer->DriverGUID = tmpDriverInfo.DriverGUID;
    FREEOTFE_MEMZERO(Buffer->Title, sizeof(Buffer->Title));
    FREEOTFE_MEMCPY(
                  Buffer->Title,
                  tmpDriverInfo.DriverTitle,
                  strlen(tmpDriverInfo.DriverTitle)
                 );
    Buffer->VersionID = tmpDriverInfo.DriverVersionID;
    Buffer->CountCyphers = tmpDriverInfo.CypherCount;

    ImpCypherDriverExtDetailsCleardown_v3(&tmpDriverInfo);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("IdentifyDriver\n")));
    return status;
}


// =========================================================================
// IOCTL to get cyphers supported
DWORD
CypherIdentifySupported_v1(
    unsigned int BufferSize,  // In bytes
    DIOC_CYPHER_IDENTIFYSUPPORTED_v1* Buffer
)
{
    DWORD status = STATUS_SUCCESS;
    CYPHER_DRIVER_INFO_v1 tmpDriverInfo;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("IdentifySupported_v1\n")));
   
    // Check size of OUTPUT buffer
    if (BufferSize <
            sizeof(*Buffer)-sizeof(Buffer->Cyphers))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("outBuffer size wrong size (expect min: %d; got: %d)\n"),
            sizeof(*Buffer)-sizeof(Buffer->Cyphers),
            BufferSize
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

        
    // Request valid, process...

    // Setup the input parameter passed to ImpCypherIdentifySupported, so that it's aware
    // of how large the array of cypher details is
    Buffer->BufCount =
       (
         // The size of the buffer, less the array of cypher details
         BufferSize - 
           (sizeof(*Buffer)-sizeof(Buffer->Cyphers))
       ) /
       // Divide by the size of each cypher details struct to give the array length
       sizeof(Buffer->Cyphers);
       
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Request is sufficiently large to store %d cypher details\n"), Buffer->BufCount));

    ImpCypherDriverExtDetailsInit_v1(&tmpDriverInfo);

    if (Buffer->BufCount < tmpDriverInfo.CypherCount)
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("outBuffer not large enough to store enough (can store: %d, requires: )\n"),
            Buffer->BufCount,
            tmpDriverInfo.CypherCount
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        ImpCypherDriverExtDetailsCleardown_v1(&tmpDriverInfo);
        return status;
        }


    // Request valid, process...


    Buffer->BufCount = tmpDriverInfo.CypherCount;
    FREEOTFE_MEMCPY(
                  &Buffer->Cyphers[0],
                  tmpDriverInfo.CypherDetails,
                  (sizeof(CYPHER_v1) * tmpDriverInfo.CypherCount)
                 );


    ImpCypherDriverExtDetailsCleardown_v1(&tmpDriverInfo);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("IdentifySupported_v1\n")));
    return status;
}

// =========================================================================
// IOCTL to get cyphers supported
DWORD
CypherIdentifySupported_v3(
    unsigned int BufferSize,  // In bytes
    DIOC_CYPHER_IDENTIFYSUPPORTED_v3* Buffer
)
{
    DWORD status = STATUS_SUCCESS;
    CYPHER_DRIVER_INFO_v3 tmpDriverInfo;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("IdentifySupported_v3\n")));
   
    // Check size of OUTPUT buffer
    if (BufferSize <
            sizeof(*Buffer)-sizeof(Buffer->Cyphers))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("outBuffer size wrong size (expect min: %d; got: %d)\n"),
            sizeof(*Buffer)-sizeof(Buffer->Cyphers),
            BufferSize
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        return status;            
        }

        
    // Request valid, process...

    // Setup the input parameter passed to ImpCypherIdentifySupported, so that it's aware
    // of how large the array of cypher details is
    Buffer->BufCount =
       (
         // The size of the buffer, less the array of cypher details
         BufferSize - 
           (sizeof(*Buffer)-sizeof(Buffer->Cyphers))
       ) /
       // Divide by the size of each cypher details struct to give the array length
       sizeof(Buffer->Cyphers);
       
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Request is sufficiently large to store %d cypher details\n"), Buffer->BufCount));

    ImpCypherDriverExtDetailsInit_v3(&tmpDriverInfo);

    if (Buffer->BufCount < tmpDriverInfo.CypherCount)
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("outBuffer not large enough to store enough (can store: %d, requires: )\n"),
            Buffer->BufCount,
            tmpDriverInfo.CypherCount
            ));
        status = STATUS_INVALID_BUFFER_SIZE;
        ImpCypherDriverExtDetailsCleardown_v3(&tmpDriverInfo);
        return status;
        }


    // Request valid, process...


    Buffer->BufCount = tmpDriverInfo.CypherCount;
    FREEOTFE_MEMCPY(
                  &Buffer->Cyphers[0],
                  tmpDriverInfo.CypherDetails,
                  (sizeof(CYPHER_v3) * tmpDriverInfo.CypherCount)
                 );


    ImpCypherDriverExtDetailsCleardown_v3(&tmpDriverInfo);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("IdentifySupported\n")));
    return status;
}


// =========================================================================
// 
DWORD
CypherEncryptWithASCII(
    IN      GUID* CypherGUID,
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
)
{
    DWORD status;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("EncryptWithASCII\n")));

    status = ImpCypherEncryptData(
                                  CypherGUID,
                                  KeyLength,
                                  Key,
                                  KeyASCII, 
                                  IVLength, 
                                  IV,
                                  PlaintextLength,  
                                  PlaintextData,
                                  CyphertextData
                                 );

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("EncryptWithASCII\n")));

    return status;
}


// =========================================================================
// 
DWORD
CypherEncrypt(
    IN      DWORD BufferInLen,
    IN      DIOC_CYPHER_DATA_IN* BufferIn,
    IN      DWORD BufferOutLen,
    IN      DIOC_CYPHER_DATA_OUT* BufferOut
)
{
    DWORD status;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("Encrypt\n")));

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&BufferIn->Key;
    ptrInIV   = ptrInKey + ((BufferIn->KeyLength / 8) * sizeof(BufferIn->Key));
    ptrInData = ptrInIV + ((BufferIn->IVLength / 8) * sizeof(BufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Key length: %d bits\n"), BufferIn->KeyLength));
    for(i = 0; i < (BufferIn->KeyLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Key [%.2d]: %.2x\n"), i, ptrInKey[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- IV length: %d bits\n"), BufferIn->IVLength));
    for(i = 0; i < (BufferIn->IVLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- IV [%.2d]: %.2x\n"), i, ptrInIV[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data length: %d bytes\n"), BufferIn->DataLength));
    for(i = 0; i < (BufferIn->DataLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data [%.2d]: %.2x\n"), i, ptrInData[i]));
        // Only dump out the first 5 bytes...
        if (i >= 5) 
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data [..]: (Debug output truncated to only show first few bytes)\n")));
            break;        
            }
        }                                


    // Sanity checks on the request wrt the cypher

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to get cypher details...\n")));
    status = CypherGetCypherDetails_v3(
                                  &(BufferIn->CypherGUID),
                                  &CypherDetails
                                 );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Unable to locate requested cypher in driver\n")));
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Cypher details got OK.\n")));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    if ((CypherDetails.BlockSize > 0) && (((BufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n"),
                                        CypherDetails.BlockSize,
                                        (BufferIn->DataLength * 8)));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // IV length must = the blocksize
    if ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != BufferIn->IVLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n"), CypherDetails.BlockSize, BufferIn->IVLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // KeyLength must = the keysize
    if ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != BufferIn->KeyLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n"), CypherDetails.KeySizeRequired, BufferIn->KeyLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Sanity checks OK\n")));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((BufferIn->KeyLength / 4)+1);
    keyASCII = FREEOTFE_MEMALLOC(keyASCIIBufferLen);
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("keyASCIIBufferLen = %d bytes\n"), keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to convert key data to ASCIIZ rep...\n")));
    ConvertDataToASCIIRep(
                          BufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Converted OK.\n")));

    if (NT_SUCCESS(status))
        {
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = BufferIn->DataLength;
        tmpBuffer = FREEOTFE_MEMALLOC(dataLength);    

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to encrypt...\n")));
        status = ImpCypherEncryptData(
                                    &BufferIn->CypherGUID,
                                    BufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    BufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    BufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    tmpBuffer
                                    );    
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Done encryption: %d (want status: %d)\n"), status, STATUS_SUCCESS));

        FREEOTFE_MEMCPY(&BufferOut->Data, tmpBuffer, dataLength);
        SecZeroMemory(tmpBuffer, dataLength);
        FREEOTFE_FREE(tmpBuffer);
        }

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("Encrypt\n")));
    return status;
}


// =========================================================================
// 
DWORD
CypherDecryptWithASCII(
    IN      GUID* CypherGUID,
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
)
{
    DWORD status;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("DecryptWithASCII\n")));

    status = ImpCypherDecryptData(
                                  CypherGUID,
                                  KeyLength,
                                  Key,
                                  KeyASCII, 
                                  IVLength, 
                                  IV,
                                  CyphertextLength,  
                                  CyphertextData,
                                  PlaintextData
                                 );

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("DecryptWithASCII\n")));

    return status;
}


// =========================================================================
// 
DWORD
CypherDecrypt(
    IN      DWORD BufferInLen,
    IN      DIOC_CYPHER_DATA_IN* BufferIn,
    IN      DWORD BufferOutLen,
    IN      DIOC_CYPHER_DATA_OUT* BufferOut
)
{
    DWORD status;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("Decrypt\n")));

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&BufferIn->Key;
    ptrInIV   = ptrInKey + ((BufferIn->KeyLength / 8) * sizeof(BufferIn->Key));
    ptrInData = ptrInIV + ((BufferIn->IVLength / 8) * sizeof(BufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Key length: %d bits\n"), BufferIn->KeyLength));
    for(i = 0; i < (BufferIn->KeyLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Key [%.2d]: %.2x\n"), i, ptrInKey[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- IV length: %d bits\n"), BufferIn->IVLength));
    for(i = 0; i < (BufferIn->IVLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- IV [%.2d]: %.2x\n"), i, ptrInIV[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data length: %d bytes\n"), BufferIn->DataLength));
    for(i = 0; i < (BufferIn->DataLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data [%.2d]: %.2x\n"), i, ptrInData[i]));
        // Only dump out the first 5 bytes...
        if (i >= 5) 
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data [..]: (Debug output truncated to only show first few bytes)\n")));
            break;        
            }
        }                                


    // Sanity checks on the request wrt the cypher

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to get cypher details...\n")));
    status = CypherGetCypherDetails_v3(
                                    &(BufferIn->CypherGUID),
                                    &CypherDetails
                                   );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Unable to locate requested cypher in driver\n")));
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Cypher details got OK.\n")));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    if ((CypherDetails.BlockSize > 0) && (((BufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n"),
                                        CypherDetails.BlockSize,
                                        (BufferIn->DataLength * 8)));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // IV length must = the blocksize
    if ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != BufferIn->IVLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n"), CypherDetails.BlockSize, BufferIn->IVLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // KeyLength must = the keysize
    if ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != BufferIn->KeyLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n"), CypherDetails.KeySizeRequired, BufferIn->KeyLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Sanity checks OK\n")));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((BufferIn->KeyLength / 4)+1);
    keyASCII = FREEOTFE_MEMALLOC(keyASCIIBufferLen);
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("keyASCIIBufferLen = %d bytes\n"), keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to convert key data to ASCIIZ rep...\n")));
    ConvertDataToASCIIRep(
                          BufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Converted OK.\n")));

    if (NT_SUCCESS(status))
        {
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = BufferIn->DataLength;
        tmpBuffer = FREEOTFE_MEMALLOC(dataLength);    

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to decrypt...\n")));
        status = ImpCypherDecryptData(
                                    &BufferIn->CypherGUID,
                                    BufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    BufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    BufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    tmpBuffer
                                    );    
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Done decryption: %d (want status: %d)\n"), status, STATUS_SUCCESS));

        FREEOTFE_MEMCPY(&BufferOut->Data, tmpBuffer, dataLength);
        SecZeroMemory(tmpBuffer, dataLength);
        FREEOTFE_FREE(tmpBuffer);
        }

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("Decrypt\n")));
    return status;
}


// =========================================================================
// 
DWORD
CypherEncryptSectorWithASCII(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,
    IN      int SectorSize,  
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int PlaintextLength,  // In bytes
    IN      FREEOTFEBYTE* PlaintextData,
    OUT     FREEOTFEBYTE* CyphertextData
)
{
    DWORD status;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("EncryptSectorWithASCII\n")));

    status = ImpCypherEncryptSectorData(
                                  CypherGUID,
                                  SectorID,
                                  SectorSize,
                                  KeyLength,
                                  Key,
                                  KeyASCII, 
                                  IVLength, 
                                  IV,
                                  PlaintextLength,  
                                  PlaintextData,
                                  CyphertextData
                                 );

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("EncryptSectorWithASCII\n")));

    return status;
}


// =========================================================================
// 
DWORD
CypherEncryptSector(
    IN      DWORD BufferInLen,
    IN      DIOC_CYPHER_SECTOR_DATA_IN* BufferIn,
    IN      DWORD BufferOutLen,
    IN      DIOC_CYPHER_DATA_OUT* BufferOut
)
{
    DWORD status;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("Encrypt\n")));

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&BufferIn->Key;
    ptrInIV   = ptrInKey + ((BufferIn->KeyLength / 8) * sizeof(BufferIn->Key));
    ptrInData = ptrInIV + ((BufferIn->IVLength / 8) * sizeof(BufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Key length: %d bits\n"), BufferIn->KeyLength));
    for(i = 0; i < (BufferIn->KeyLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Key [%.2d]: %.2x\n"), i, ptrInKey[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- IV length: %d bits\n"), BufferIn->IVLength));
    for(i = 0; i < (BufferIn->IVLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- IV [%.2d]: %.2x\n"), i, ptrInIV[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data length: %d bytes\n"), BufferIn->DataLength));
    for(i = 0; i < (BufferIn->DataLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data [%.2d]: %.2x\n"), i, ptrInData[i]));
        // Only dump out the first 5 bytes...
        if (i >= 5) 
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data [..]: (Debug output truncated to only show first few bytes)\n")));
            break;        
            }
        }                                


    // Sanity checks on the request wrt the cypher

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to get cypher details...\n")));
    status = CypherGetCypherDetails_v3(
                                  &(BufferIn->CypherGUID),
                                  &CypherDetails
                                 );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Unable to locate requested cypher in driver\n")));
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Cypher details got OK.\n")));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    if ((CypherDetails.BlockSize > 0) && (((BufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n"),
                                        CypherDetails.BlockSize,
                                        (BufferIn->DataLength * 8)));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // IV length must = the blocksize
    if ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != BufferIn->IVLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n"), CypherDetails.BlockSize, BufferIn->IVLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // KeyLength must = the keysize
    if ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != BufferIn->KeyLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n"), CypherDetails.KeySizeRequired, BufferIn->KeyLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Sanity checks OK\n")));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((BufferIn->KeyLength / 4)+1);
    keyASCII = FREEOTFE_MEMALLOC(keyASCIIBufferLen);
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("keyASCIIBufferLen = %d bytes\n"), keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to convert key data to ASCIIZ rep...\n")));
    ConvertDataToASCIIRep(
                          BufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Converted OK.\n")));

    if (NT_SUCCESS(status))
        {
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = BufferIn->DataLength;
        tmpBuffer = FREEOTFE_MEMALLOC(dataLength);    

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to encrypt...\n")));
        status = ImpCypherEncryptData(
                                    &BufferIn->CypherGUID,
                                    BufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    BufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    BufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    tmpBuffer
                                    );    
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Done encryption: %d (want status: %d)\n"), status, STATUS_SUCCESS));

        FREEOTFE_MEMCPY(&BufferOut->Data, tmpBuffer, dataLength);
        SecZeroMemory(tmpBuffer, dataLength);
        FREEOTFE_FREE(tmpBuffer);
        }

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("EncryptSector\n")));
    return status;
}


// =========================================================================
// 
DWORD
CypherDecryptSectorWithASCII(
    IN      GUID* CypherGUID,
    IN      LARGE_INTEGER SectorID,
    IN      int SectorSize,  
    IN      int KeyLength,  // In bits
    IN      FREEOTFEBYTE* Key,
    IN      char* KeyASCII,  // ASCII representation of "Key"
    IN      int IVLength,  // In bits
    IN      FREEOTFEBYTE* IV,
    IN      int CyphertextLength,  // In bytes
    IN      FREEOTFEBYTE* CyphertextData,
    OUT     FREEOTFEBYTE* PlaintextData
)
{
    DWORD status;

    DEBUGOUTHASHDRV(DEBUGLEV_ENTER, (TEXT("DecryptSectorWithASCII\n")));

    status = ImpCypherDecryptSectorData(
                                  CypherGUID,
                                  SectorID,
                                  SectorSize,
                                  KeyLength,
                                  Key,
                                  KeyASCII, 
                                  IVLength, 
                                  IV,
                                  CyphertextLength,  
                                  CyphertextData,
                                  PlaintextData
                                 );

    DEBUGOUTHASHDRV(DEBUGLEV_EXIT, (TEXT("DecryptSectorWithASCII\n")));

    return status;
}


// =========================================================================
// 
DWORD
CypherDecryptSector(
    IN      DWORD BufferInLen,
    IN      DIOC_CYPHER_SECTOR_DATA_IN* BufferIn,
    IN      DWORD BufferOutLen,
    IN      DIOC_CYPHER_DATA_OUT* BufferOut
)
{
    DWORD status;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("DecryptSector\n")));

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&BufferIn->Key;
    ptrInIV   = ptrInKey + ((BufferIn->KeyLength / 8) * sizeof(BufferIn->Key));
    ptrInData = ptrInIV + ((BufferIn->IVLength / 8) * sizeof(BufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Key length: %d bits\n"), BufferIn->KeyLength));
    for(i = 0; i < (BufferIn->KeyLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Key [%.2d]: %.2x\n"), i, ptrInKey[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- IV length: %d bits\n"), BufferIn->IVLength));
    for(i = 0; i < (BufferIn->IVLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- IV [%.2d]: %.2x\n"), i, ptrInIV[i]));
        }                                
    DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data length: %d bytes\n"), BufferIn->DataLength));
    for(i = 0; i < (BufferIn->DataLength / 8); i++)
        { 
        DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data [%.2d]: %.2x\n"), i, ptrInData[i]));
        // Only dump out the first 5 bytes...
        if (i >= 5) 
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_VERBOSE_INFO, (TEXT("----- Data [..]: (Debug output truncated to only show first few bytes)\n")));
            break;        
            }
        }                                


    // Sanity checks on the request wrt the cypher

    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to get cypher details...\n")));
    status = CypherGetCypherDetails_v3(
                                    &(BufferIn->CypherGUID),
                                    &CypherDetails
                                   );
    if (!(NT_SUCCESS(status)))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Unable to locate requested cypher in driver\n")));
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Cypher details got OK.\n")));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    if ((CypherDetails.BlockSize > 0) && (((BufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n"),
                                        CypherDetails.BlockSize,
                                        (BufferIn->DataLength * 8)));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // IV length must = the blocksize
    if ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != BufferIn->IVLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n"), CypherDetails.BlockSize, BufferIn->IVLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    // KeyLength must = the keysize
    if ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != BufferIn->KeyLength))
        {
        DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, (TEXT("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n"), CypherDetails.KeySizeRequired, BufferIn->KeyLength));
        status = STATUS_INVALID_PARAMETER;
        return status;            
        }
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Sanity checks OK\n")));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((BufferIn->KeyLength / 4)+1);
    keyASCII = FREEOTFE_MEMALLOC(keyASCIIBufferLen);
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("keyASCIIBufferLen = %d bytes\n"), keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to convert key data to ASCIIZ rep...\n")));
    ConvertDataToASCIIRep(
                          BufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );
    DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Converted OK.\n")));

    if (NT_SUCCESS(status))
        {
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = BufferIn->DataLength;
        tmpBuffer = FREEOTFE_MEMALLOC(dataLength);    

        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("About to decrypt...\n")));
        status = ImpCypherDecryptData(
                                    &BufferIn->CypherGUID,
                                    BufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    BufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    BufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    tmpBuffer
                                    );    
        DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Done decryption: %d (want status: %d)\n"), status, STATUS_SUCCESS));

        FREEOTFE_MEMCPY(&BufferOut->Data, tmpBuffer, dataLength);
        SecZeroMemory(tmpBuffer, dataLength);
        FREEOTFE_FREE(tmpBuffer);
        }

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("DecryptSector\n")));
    return status;
}


// =========================================================================
DWORD
CypherGetCypherDetails_v1(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v1* CypherDetails
)
{
    NTSTATUS status = STATUS_NOT_FOUND;
    CYPHER_DRIVER_INFO_v1 tmpDevExt;
    unsigned int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("GetCypherDetails_v1\n")));
    
    ImpCypherDriverExtDetailsInit_v1(&tmpDevExt);

    for (i = 0; i<tmpDevExt.CypherCount; i++)
        {
        if (IsEqualGUID(&tmpDevExt.CypherDetails[i].CypherGUID, CypherGUID))
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Located relevant details\n")));
            FREEOTFE_MEMCPY(
                            CypherDetails,
                            &tmpDevExt.CypherDetails[i],
                            sizeof(CYPHER_v1)
                           );

            status = STATUS_SUCCESS;
            }
        }

    ImpCypherDriverExtDetailsCleardown_v1(&tmpDevExt);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("GetCypherDetails_v1\n")));

    return status;
}

// =========================================================================
DWORD
CypherGetCypherDetails_v3(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v3* CypherDetails
)
{
    NTSTATUS status = STATUS_NOT_FOUND;
    CYPHER_DRIVER_INFO_v3 tmpDevExt;
    unsigned int i;

    DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, (TEXT("GetCypherDetails_v3\n")));
    
    ImpCypherDriverExtDetailsInit_v3(&tmpDevExt);

    for (i = 0; i<tmpDevExt.CypherCount; i++)
        {
        if (IsEqualGUID(&tmpDevExt.CypherDetails[i].CypherGUID, CypherGUID))
            {
            DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, (TEXT("Located relevant details\n")));
            FREEOTFE_MEMCPY(
                            CypherDetails,
                            &tmpDevExt.CypherDetails[i],
                            sizeof(CYPHER_v3)
                           );

            status = STATUS_SUCCESS;
            }
        }

    ImpCypherDriverExtDetailsCleardown_v3(&tmpDevExt);

    DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, (TEXT("GetCypherDetails_v3\n")));

    return status;
}


// =========================================================================
// Included for backward compatibility
DWORD
CypherIdentifySupported(
    unsigned int BufferSize,  // In bytes
    DIOC_CYPHER_IDENTIFYSUPPORTED_v1* Buffer
)
{
    return CypherIdentifySupported_v1(BufferSize, Buffer);
}

// =========================================================================
// Included for backward compatibility
DWORD
CypherGetCypherDetails(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v1* CypherDetails
)
{
    return CypherGetCypherDetails_v1(CypherGUID, CypherDetails);
}

// =========================================================================
// =========================================================================
