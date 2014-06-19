// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFENULLGUID.h"
#include "FreeOTFEPlatform.h"
#include "LUKS.h"
#include "LUKSAFS.h"
#include "SDUGeneral.h"
#include "SDUHexDump.h"
#include "SDUEndianIntegers.h"
#include "DriverInterfaceTwo.h"
#include "DriverInterfaceHash.h"
#include "DriverInterfaceCypher.h"
#include "FreeOTFE4PDAlib.h"


// =========================================================================
// Constants...

// The Tiger hash is called "tgr" under Linux (why?!!)
#define FREEOTFE_TIGER  "TIGER"
#define LUKS_TIGER      "TGR"
// The Whirlpool hash is called "wp" under Linux (why?!!)
#define FREEOTFE_WHIRLPOOL  "WHIRLPOOL"
#define LUKS_WHIRLPOOL      "WP"

#define PLAIN_IV  "plain"
// Not defined in the LUKS spec, but supported by LUKS anyway
#define ESSIV_IV  "essiv"
// Not defined in the LUKS spec, but supported by LUKS anyway
#define BENBI_IV  "benbi"  


// =========================================================================
// Forward declarations...

BOOL LUKS_IdentifyHash(
    char* hashSpec, 
    WCHAR *hashKernelModeDeviceName,
    GUID* hashGUID
);
BOOL LUKS_IdentifyCypher(
    char* cypherName,
    int keySizeBits,
    char* cypherMode,
    BOOL baseIVCypherOnHashLength,

    WCHAR *cypherKernelModeDeviceName,
    GUID* cypherGUID,
    SECTOR_IV_GEN_METHOD* sectorIVGenMethod,
    WCHAR *IVHashKernelModeDeviceName,
    GUID* IVHashGUID,
    WCHAR *IVCypherKernelModeDeviceName,
    GUID* IVCypherGUID
);
BOOL LUKS_IdentifyCypher_SearchCyphers(
    int countImplCypher,
    DRIVER_AND_ALG* implCypher,

    char* cypherName,
    int keySizeBits,
    CYPHER_MODE useCypherMode,

    WCHAR *cypherKernelModeDeviceName,
    GUID* cypherGUID
);
void LUKS_PrettyHex(
    FILE* hFile,
    BYTE* data,
    unsigned int bytesCount
);
BOOL LUKS_StringHasNumbers(char* checkString);



// ----------------------------------------------------------------------------
BOOL LUKS_StringHasNumbers(char* checkString)
{
    unsigned int i;
    BOOL retval;

    retval = FALSE;

    for (i = 0; i < strlen(checkString); i++)
        {
        if (
            (checkString[i] >= '0') &&
            (checkString[i] <= '9')
            ) 
            {
            retval = TRUE;
            break;
            }
        }

    return retval;
}


// ----------------------------------------------------------------------------
// Determine if the specified volume is a Linux LUKS volume
BOOL LUKS_IsLUKSVolume(WCHAR* volumeFilename)
{
    BYTE rawData[LUKS_MAGICSIZE];
    BOOL retval;
    int i;
    LARGE_INTEGER tmpZero;

    retval = FALSE;

    // Read in what should be the volume's signature
    tmpZero.QuadPart = 0;
    if (SDUReadWriteFile(
                        volumeFilename,
                        tmpZero,  // LUKS volumes always start from the beginning
                        TRUE,
                        LUKS_MAGICSIZE,
                        rawData
                        ))
        {
        // Compare with LUKS volume signature
        for (i = 0; i < LUKS_MAGICSIZE; i++)
            {
            retval = (rawData[i] == LUKS_MAGIC[i]);
            if (!(retval)) 
                {
                break;
                }
            }
        }

    return retval;
}


// ----------------------------------------------------------------------------
BOOL LUKS_ReadLUKSHeader(
    WCHAR* filename, 
    LUKS_HEADER_EXT* LUKSHeaderExt
)
{
    BOOL allOK;
    LARGE_INTEGER tmpZero;
    int i;

    tmpZero.QuadPart = 0;
    allOK = SDUReadWriteFile(
                            filename,
                            tmpZero,  // LUKS volumes always start from the beginning
                            TRUE,
                            sizeof(LUKSHeaderExt->rawHeader),
                            (BYTE*)(&(LUKSHeaderExt->rawHeader))
                            );

    LUKSHeaderExt->rawHeader.version = SDULittleToBigEndianWORD(LUKSHeaderExt->rawHeader.version);
    LUKSHeaderExt->rawHeader.payload_offset = SDULittleToBigEndianDWORD(LUKSHeaderExt->rawHeader.payload_offset);
    LUKSHeaderExt->rawHeader.key_bytes = SDULittleToBigEndianDWORD(LUKSHeaderExt->rawHeader.key_bytes);
    LUKSHeaderExt->rawHeader.mk_digest_iter = SDULittleToBigEndianDWORD(LUKSHeaderExt->rawHeader.mk_digest_iter);

    for (i = 0; i < LUKS_NUMKEYS; i++)
        {
        LUKSHeaderExt->rawHeader.key_slot[i].active = SDULittleToBigEndianDWORD(LUKSHeaderExt->rawHeader.key_slot[i].active);
        LUKSHeaderExt->rawHeader.key_slot[i].iterations = SDULittleToBigEndianDWORD(LUKSHeaderExt->rawHeader.key_slot[i].iterations);
        LUKSHeaderExt->rawHeader.key_slot[i].key_material_offset = SDULittleToBigEndianDWORD(LUKSHeaderExt->rawHeader.key_slot[i].key_material_offset);
        LUKSHeaderExt->rawHeader.key_slot[i].stripes = SDULittleToBigEndianDWORD(LUKSHeaderExt->rawHeader.key_slot[i].stripes);
        }

    return allOK;
}


// ----------------------------------------------------------------------------
// If the hash and cypher members of the LUKS header passed in are populated,
// this function will populate struct with the the FreeOTFE driver details
// (e.g. driver name/GUID) that correspond to that hash/cypher
// Note: Must be called *after* ReadLUKSHeader(...)
BOOL LUKS_MapToFreeOTFE(
    BOOL baseIVCypherOnHashLength,
    LUKS_HEADER_EXT* LUKSHeader
)
{
    BOOL allOK;

    allOK = TRUE;

    // Identify the FreeOTFE hash driver to be used
    if (allOK) 
        {
        allOK = LUKS_IdentifyHash(
                                LUKSHeader->rawHeader.hash_spec, 
                                LUKSHeader->hashKernelModeDeviceName, 
                                &(LUKSHeader->hashGUID)
                                );
        }

    // Identify the FreeOTFE cypher driver to be used
    if (allOK) 
        {
        allOK = LUKS_IdentifyCypher(
                                LUKSHeader->rawHeader.cipher_name,
                                (LUKSHeader->rawHeader.key_bytes * 8),
                                LUKSHeader->rawHeader.cipher_mode,
                                baseIVCypherOnHashLength,
                                LUKSHeader->cypherKernelModeDeviceName,
                                &(LUKSHeader->cypherGUID),
                                &(LUKSHeader->sectorIVGenMethod),
                                LUKSHeader->IVHashKernelModeDeviceName,
                                &(LUKSHeader->IVHashGUID),
                                LUKSHeader->IVCypherKernelModeDeviceName,
                                &(LUKSHeader->IVCypherGUID)
                                );
        }

    return allOK;
}


// ----------------------------------------------------------------------------
BOOL LUKS_DumpLUKSDataToFile(
    MODULE_DETAILS_MAIN* mainDriver,

    WCHAR* filename, 
    unsigned char* userKey,
    BOOL baseIVCypherOnHashLength,
    WCHAR* dumpFilename
)
{
    BOOL retval;
    LUKS_HEADER_EXT LUKSHeader;
    int i;
    char hashTitle[MAX_PRETTYPRINTED_TITLE];
    char cypherTitle[MAX_PRETTYPRINTED_TITLE];
    int keySlot;
    BYTE* keyMaterial;
    char IVHashTitle[MAX_PRETTYPRINTED_TITLE];
    char IVCypherTitle[MAX_PRETTYPRINTED_TITLE];
    char sectorIVGenMethodTitle[MAX_PRETTYPRINTED_TITLE];
    char filenameAsChar[FREEOTFE_MAX_FILENAME_LENGTH * 4];

    FILE* hFile;
    WCHAR* tmpStringGUID;
    char* prettyPrintData;
    int prettyPrintDataLength;


    retval = TRUE;

    // Sanity; source file must exist
    if (retval)
        {
        retval = (
                SDUCheckFileExists(filename) &&
                (!(SDUCheckFileExists(dumpFilename)))
                );
        }


    if (retval)
        {
        hFile = _wfopen(dumpFilename, TEXT("w+"));

        driver_AddStdDumpHeader(hFile, "LUKS Dump");

        // Get the LUKS header from the volume...
        if (!(LUKS_ReadLUKSHeader(filename, &LUKSHeader))) 
            {
            if (LUKS_IsLUKSVolume(filename)) 
                {
                fprintf(hFile, "ERROR: Unable to read LUKS header?!\n");
                }
            else
                {
                fprintf(hFile, "Unable to read LUKS header; this does not appear to be a LUKS volume.\n");
                }

            }
        else
            {
            fprintf(hFile, "\n");
            fprintf(hFile, "\n");
            driver_AddStdDumpSection(hFile, "cryptsetup Style Dump");
            wcstombs(filenameAsChar, filename, sizeof(filenameAsChar));
            fprintf(hFile, "LUKS header information for %s\n", filenameAsChar);
            SecZeroMemory(filenameAsChar, sizeof(filenameAsChar));
            fprintf(hFile, "\n");
            fprintf(hFile, "Version:        %d\n", LUKSHeader.rawHeader.version);
            fprintf(hFile, "Cipher name:    %s\n", LUKSHeader.rawHeader.cipher_name);
            fprintf(hFile, "Cipher mode:    %s\n", LUKSHeader.rawHeader.cipher_mode);
            fprintf(hFile, "Hash spec:      %s\n", LUKSHeader.rawHeader.hash_spec);
            fprintf(hFile, "Payload offset: %d\n", LUKSHeader.rawHeader.payload_offset);
            fprintf(hFile, "MK bits:        %d\n", (LUKSHeader.rawHeader.key_bytes * 8));  // Convert from bytes to bits
            fprintf(hFile, "MK digest:      ");
            LUKS_PrettyHex(      
                        hFile,
                        (BYTE*)(&(LUKSHeader.rawHeader.mk_digest)),
                        LUKS_DIGESTSIZE
                        );
            fprintf(hFile, "\n");
            fprintf(hFile, "MK salt:        ");
            LUKS_PrettyHex(
                        hFile,
                        &(LUKSHeader.rawHeader.mk_digest_salt[0]),
                        (LUKS_SALTSIZE / 2)
                        );
            fprintf(hFile, "\n");
            fprintf(hFile, "                ");
            LUKS_PrettyHex(
                        hFile,
                        &(LUKSHeader.rawHeader.mk_digest_salt[(LUKS_SALTSIZE / 2)]),
                        (LUKS_SALTSIZE / 2)
                        );
            fprintf(hFile, "\n");
            fprintf(hFile, "MK iterations:  %d\n", LUKSHeader.rawHeader.mk_digest_iter);
            fprintf(hFile, "UUID:           %s\n", LUKSHeader.rawHeader.uuid);
            fprintf(hFile, "\n");

            for (i = 0; i < LUKS_NUMKEYS; i++)
                {
                if (LUKSHeader.rawHeader.key_slot[i].active != LUKS_KEY_ENABLED) 
                    {
                    fprintf(hFile, "Key Slot %d: DISABLED\n", i);
                    }
                else
                    {
                    fprintf(hFile, "Key Slot %d: ENABLED\n", i);
                    fprintf(hFile, "        Iterations:             %d\n", LUKSHeader.rawHeader.key_slot[i].iterations);
                    fprintf(hFile, "        Salt:                   ");
                    LUKS_PrettyHex(
                                hFile,
                                &(LUKSHeader.rawHeader.key_slot[i].salt[0]),
                                (LUKS_SALTSIZE / 2)
                                );
                    fprintf(hFile, "\n");
                    fprintf(hFile, "                                ");
                    LUKS_PrettyHex(
                                hFile,
                                &(LUKSHeader.rawHeader.key_slot[i].salt[(LUKS_SALTSIZE / 2)]),
                                (LUKS_SALTSIZE / 2)
                                );
                    fprintf(hFile, "\n");
                    fprintf(hFile, "        Key material offset:    %d\n", LUKSHeader.rawHeader.key_slot[i].key_material_offset);
                    fprintf(hFile, "        AF stripes:             %d\n", LUKSHeader.rawHeader.key_slot[i].stripes);
                    }
                }


            fprintf(hFile, "\n");
            fprintf(hFile, "\n");
            driver_AddStdDumpSection(hFile, "Mapped FreeOTFE Drivers");
            if (!(LUKS_MapToFreeOTFE(baseIVCypherOnHashLength, &LUKSHeader))) 
                {
                fprintf(hFile, "One or more of the following:\n");
                fprintf(hFile, "\n");
                fprintf(hFile, "  *) The hash algorithm\n");
                fprintf(hFile, "  *) The cypher\n");
                fprintf(hFile, "  *) The IV generation method\n");
                fprintf(hFile, "\n");
                fprintf(hFile, "specified in the LUKS header could not be mapped to a FreeOTFE equivalent.\n");
                fprintf(hFile, "This may be because the required FreeOTFE driver hasn''t been installed and\n");
                fprintf(hFile, "started, or because the required LUKS option is not supported in this\n");
                fprintf(hFile, "version of FreeOTFE\n");
                }
            else
                {
                strcpy(hashTitle, "ERROR: Unable to determine hash title?!");
                driver_HashPrettyprintAlgTechTitle_ByDriverAndGUID_char(
                    LUKSHeader.hashKernelModeDeviceName,
                    LUKSHeader.hashGUID,
                    hashTitle,
                    sizeof(hashTitle)
                    );

                strcpy(IVCypherTitle, "ERROR: Unable to determine cypher title?!");
                driver_CypherPrettyprintAlgTechTitle_ByDriverAndGUID_char(
                    LUKSHeader.cypherKernelModeDeviceName,
                    LUKSHeader.cypherGUID,
                    cypherTitle,
                    sizeof(cypherTitle)
                    );

                if (LUKSHeader.sectorIVGenMethod != SCTRIVGEN_ESSIV) 
                    {
                    strcpy(IVHashTitle, "IV hash n/a");
                    strcpy(IVCypherTitle, "IV cypher n/a");
                    }
                else
                    {
                    strcpy(IVHashTitle, "ERROR: Unable to determine IV hash title?!");
                    driver_HashPrettyprintAlgTechTitle_ByDriverAndGUID_char(
                        LUKSHeader.IVHashKernelModeDeviceName,
                        LUKSHeader.IVHashGUID,
                        IVHashTitle,
                        sizeof(IVHashTitle)
                        );

                    strcpy(IVCypherTitle, "ERROR: Unable to determine IV cypher title?!");
                    driver_CypherPrettyprintAlgTechTitle_ByDriverAndGUID_char(
                        LUKSHeader.IVCypherKernelModeDeviceName,
                        LUKSHeader.IVCypherGUID,
                        IVCypherTitle,
                        sizeof(IVCypherTitle)
                        );
                        }

                fprintf(hFile, "Hash pretty title         : %s\n", hashTitle);
                fprintf(hFile, "Hash driver KM name       : ");
                driver_DumpWCHAR(hFile, LUKSHeader.hashKernelModeDeviceName);
                fprintf(hFile, "\n");
                fprintf(hFile, "Hash GUID                 : ");
                GUIDToWCHAR(&(LUKSHeader.hashGUID), &tmpStringGUID);
                SDUwcstoupper(tmpStringGUID, tmpStringGUID);
                driver_DumpWCHAR(hFile, tmpStringGUID);
                SecZeroAndFreeWCHARMemory(tmpStringGUID);
                fprintf(hFile, "\n");
                fprintf(hFile, "Cypher pretty title       : %s\n", cypherTitle);
                fprintf(hFile, "Cypher driver KM name     : ");
                driver_DumpWCHAR(hFile, LUKSHeader.cypherKernelModeDeviceName);
                fprintf(hFile, "\n");
                fprintf(hFile, "Cypher GUID               : ");
                GUIDToWCHAR(&(LUKSHeader.cypherGUID), &tmpStringGUID);
                SDUwcstoupper(tmpStringGUID, tmpStringGUID);
                driver_DumpWCHAR(hFile, tmpStringGUID);
                SecZeroAndFreeWCHARMemory(tmpStringGUID);
                fprintf(hFile, "\n");
                driver_PrettyprintSectorIVGenMethod_char(
                    LUKSHeader.sectorIVGenMethod,
                    sectorIVGenMethodTitle,
                    sizeof(sectorIVGenMethodTitle)
                    );
                fprintf(hFile, "Sector IV generation      : %s\n", sectorIVGenMethodTitle);
                if (LUKSHeader.sectorIVGenMethod == SCTRIVGEN_ESSIV) 
                    {
                    fprintf(hFile, "  IV hash pretty title    : %s\n", IVHashTitle);
                    fprintf(hFile, "  IV hash driver KM name  : ");
                    driver_DumpWCHAR(hFile, LUKSHeader.IVHashKernelModeDeviceName);
                    fprintf(hFile, "\n");
                    fprintf(hFile, "  IV hash GUID            : ");
                    GUIDToWCHAR(&(LUKSHeader.IVHashGUID), &tmpStringGUID);
                    SDUwcstoupper(tmpStringGUID, tmpStringGUID);
                    driver_DumpWCHAR(hFile, tmpStringGUID);
                    SecZeroAndFreeWCHARMemory(tmpStringGUID);
                    fprintf(hFile, "\n");

                    if (baseIVCypherOnHashLength)
                        {
                        fprintf(hFile, "  IV cypher               : <based on IV hash length>\n");
                        }
                    else
                        {
                        fprintf(hFile, "  IV cypher               : <same as main cypher>\n");
                        }

                    fprintf(hFile, "  IV cypher pretty title  : %s\n", IVCypherTitle);
                    fprintf(hFile, "  IV cypher driver KM name: ");
                    driver_DumpWCHAR(hFile, LUKSHeader.IVCypherKernelModeDeviceName);
                    fprintf(hFile, "\n");
                    fprintf(hFile, "  IV cypher GUID          : ");
                    GUIDToWCHAR(&(LUKSHeader.IVCypherGUID), &tmpStringGUID);
                    SDUwcstoupper(tmpStringGUID, tmpStringGUID);
                    driver_DumpWCHAR(hFile, tmpStringGUID);
                    SecZeroAndFreeWCHARMemory(tmpStringGUID);
                    fprintf(hFile, "\n");
                    }
                }


            fprintf(hFile, "\n");
            fprintf(hFile, "\n");
            driver_AddStdDumpSection(hFile, "Master Key");
            fprintf(hFile, "User supplied password   : %s\n", userKey);

            keyMaterial = NULL;
            if (!(LUKS_RecoverMasterKey(
                                        mainDriver,

                                        filename,
                                        userKey,
                                        baseIVCypherOnHashLength,
                                        &LUKSHeader,
                                        &keySlot,
                                        &keyMaterial
                                        )))
                {
                fprintf(hFile, "No master key could be recovered with the specified password.\n");
                }
            else
                {
                prettyPrintDataLength = SDUPrettyPrintHexBufReq(
                                            LUKSHeader.rawHeader.key_bytes,
                                            8
                                            );
                prettyPrintData = malloc(prettyPrintDataLength);
                if (prettyPrintData == NULL)
                    {
                    fprintf(hFile, "Unable to allocate memory to store prettyprinted master key\n");
                    }
                else
                    {
                    SDUPrettyPrintHex(
                                    keyMaterial,
                                    0,
                                    LUKSHeader.rawHeader.key_bytes,
                                    prettyPrintData,
                                    prettyPrintDataLength,
                                    8
                                    );            
                    fprintf(hFile, "Password unlocks key slot: %d\n", keySlot);
                    fprintf(hFile, "Recovered master key     :\n");
                    fprintf(hFile, "%s", prettyPrintData);

                    SecZeroAndFreeMemory(prettyPrintData, prettyPrintDataLength);
                    }

                }

            SecZeroAndFreeMemory(keyMaterial, LUKSHeader.rawHeader.key_bytes);
            }

        retval = TRUE;

        fclose(hFile);
        }


    return retval;
}


// ----------------------------------------------------------------------------
// This function is similar to SDUPrettyPrintHex from the SDUGeneral unit, but
// formats slightly differently
// This special function is required to generate the same format output as
// cryptsetup produces, in order to make diffing the output easier.
void LUKS_PrettyHex(
    FILE* hFile,
    BYTE* data,
    unsigned int bytesCount
)
{
    unsigned int i;

    for (i = 0; i < bytesCount; i++)
        {
        // Yeah, I know - this isn't too nice. There's always going to be an extra
        // space at the end of the line. Don't blame me, this is how
        // cryptosetup-luks does things - I'm just replicating it here so that the
        // output ban be diffed easily
        fprintf(hFile, "%.2x ", data[i]);
        }

}


// ----------------------------------------------------------------------------
BOOL LUKS_RecoverMasterKey(
    MODULE_DETAILS_MAIN* mainDriver,

    WCHAR* filename,
    unsigned char* userPassword,
    BOOL baseIVCypherOnHashLength,

    LUKS_HEADER_EXT* LUKSHeader,
    int* keySlot,
    BYTE** keyMaterial  // CALLER is responsible for freeing off the pointer this is set to 
                        // Buffer will be (LUKSHeader->key_bytes) long 
)
{
    BOOL allOK;
    BYTE* pwd_PBKDF2ed;
    int pwd_PBKDF2edLength;
    BOOL foundValid;
    int i;
    BYTE* generatedCheck;
    BYTE* masterKey;
    int splitDataLength;
    BYTE* splitData;
    int tmpVolumeFlags;
    LARGE_INTEGER tmpZero;
    LARGE_INTEGER encryptedDataStart;


    allOK = TRUE;

    *keySlot = -1;

    splitDataLength = 0;
    splitData = NULL;
    pwd_PBKDF2ed = NULL;
    masterKey = NULL;
    generatedCheck = NULL;

    tmpZero.QuadPart = 0;

    // Get the LUKS header from the volume...
    if (allOK) 
        {
        allOK = LUKS_ReadLUKSHeader(filename, LUKSHeader);
        }

    // Identify the FreeOTFE options to be used...
    if (allOK) 
        {
        allOK = LUKS_MapToFreeOTFE(baseIVCypherOnHashLength, LUKSHeader);
        }

    if (allOK)
        {
        masterKey = malloc(LUKSHeader->rawHeader.key_bytes);
        allOK = (masterKey != NULL);
        }

    if (allOK) 
        {
        pwd_PBKDF2edLength = LUKSHeader->rawHeader.key_bytes;
        pwd_PBKDF2ed = malloc(pwd_PBKDF2edLength);
        allOK = (pwd_PBKDF2ed != NULL);
        }

    if (allOK) 
        {
        generatedCheck = malloc(sizeof(LUKSHeader->rawHeader.mk_digest));
        allOK = (generatedCheck != NULL);
        }

    if (allOK) 
        {
        // For each keyslot...
        for (i = 0; i < LUKS_NUMKEYS; i++)
            {
            if (LUKSHeader->rawHeader.key_slot[i].active == LUKS_KEY_ENABLED) 
                {
                // Overwrite and free off any previous...
                SecZeroAndFreeMemory(splitData, splitDataLength);

                splitDataLength = (LUKSHeader->rawHeader.key_bytes * LUKSHeader->rawHeader.key_slot[i].stripes);
                splitData = malloc(splitDataLength);
                if (splitData == NULL)
                    {
                    allOK = FALSE;
                    break;
                    }

                // PBKDF2 the user's password with the salt
                if (!(driver_DeriveKey(
                                mainDriver,

                                KDF_PBKDF2,
                                LUKSHeader->hashKernelModeDeviceName,
                                LUKSHeader->hashGUID,
                                LUKSHeader->cypherKernelModeDeviceName,
                                LUKSHeader->cypherGUID,
                                userPassword,
                                strlen(userPassword),
                                LUKSHeader->rawHeader.key_slot[i].salt,  
                                (LUKS_SALTSIZE * 8),
                                LUKSHeader->rawHeader.key_slot[i].iterations,
                                (LUKSHeader->rawHeader.key_bytes * 8),  // In *bits*
                                pwd_PBKDF2ed
                                )))
                    {
                    allOK = FALSE;
                    break;
                    }


                //{$IFDEF FREEOTFE_DEBUG}
                //  DebugMsg('LUKS derived key follows:');
                //  DebugMsgBinary(pwd_PBKDF2ed);
                //{$ENDIF}

                tmpVolumeFlags = 0;

                encryptedDataStart.QuadPart = (LUKS_SECTOR_SIZE * LUKSHeader->rawHeader.key_slot[i].key_material_offset);  // Offset within volume where encrypted data starts
                if (!(driver_ReadWritePlaintextToVolume(
                                mainDriver,

                                TRUE,  // Read, NOT write

                                filename,
                                pwd_PBKDF2ed,
                                (LUKSHeader->rawHeader.key_bytes * 8),
                                LUKSHeader->sectorIVGenMethod,
                                LUKSHeader->IVHashKernelModeDeviceName,
                                LUKSHeader->IVHashGUID,
                                LUKSHeader->IVCypherKernelModeDeviceName,
                                LUKSHeader->IVCypherGUID,
                                LUKSHeader->cypherKernelModeDeviceName,
                                LUKSHeader->cypherGUID,
                                tmpVolumeFlags,
                                
                                tmpZero,  // Offset from within mounted volume from where to read/write data
                                splitDataLength,  // Length of data to read/write. In bytes
                                splitData,  // Data to read/write

                                encryptedDataStart,  // Offset within volume where encrypted data starts
                                tmpZero
                                )))
                    {
                    allOK = FALSE;
                    break;
                    }


                //{$IFDEF FREEOTFE_DEBUG}
                //  DebugMsg('LUKS decrypted split data follows:');
                //  DebugMsgBinary(splitData);
                //{$ENDIF}


                // Merge the split data
                if (!(LUKSAFS_AFMerge(
                                splitData,
                                splitDataLength,  // In bytes
                                LUKSHeader->rawHeader.key_slot[i].stripes,
                                LUKSHeader->hashKernelModeDeviceName,
                                LUKSHeader->hashGUID,
                                LUKSHeader->rawHeader.key_bytes,
                                masterKey
                                ))) 
                    {
                    allOK = FALSE;
                    break;
                    }


                //{$IFDEF FREEOTFE_DEBUG}
                //  DebugMsg('LUKS AF merged data follows:');
                //  DebugMsgBinary(masterKey);
                //{$ENDIF}


                // Generate PBKDF2 of the merged data
                if (!(driver_DeriveKey(
                            mainDriver,

                            KDF_PBKDF2,
                            LUKSHeader->hashKernelModeDeviceName,
                            LUKSHeader->hashGUID,
                            LUKSHeader->cypherKernelModeDeviceName,
                            LUKSHeader->cypherGUID,
                            masterKey,
                            LUKSHeader->rawHeader.key_bytes,
                            LUKSHeader->rawHeader.mk_digest_salt,  
                            (LUKS_SALTSIZE * 8),
                            LUKSHeader->rawHeader.mk_digest_iter,
                            (sizeof(LUKSHeader->rawHeader.mk_digest) * 8),
                            generatedCheck
                            )))
                    {
                    allOK = FALSE;
                    break;
                    }

                //{$IFDEF FREEOTFE_DEBUG}
                //  DebugMsg('LUKS generated pbkdf2 checksum of recovered key follows:');
                //  DebugMsgBinary(generatedCheck);
                //{$ENDIF}

                // Check if the PBKDF2 of the merged data matches that of the MK diget
                foundValid = TRUE;
                if (memcmp(
                           generatedCheck, 
                           LUKSHeader->rawHeader.mk_digest,
                           sizeof(LUKSHeader->rawHeader.mk_digest)
                          ) != 0)
                    {
                    foundValid = FALSE;
                    break;
                    }

                if (foundValid) 
                    {
                    // Valid keyslot found; break out of loop
                    *keyMaterial = malloc(LUKSHeader->rawHeader.key_bytes);
                    if (*keyMaterial == NULL)
                        {
                        // Do nothing; keyslot not set, so this function will just 
                        // return FALSE anyway
                        }
                    else
                        {
                        *keySlot = i;
                        memcpy(*keyMaterial, masterKey, LUKSHeader->rawHeader.key_bytes);
                        }

                    break;
                    }

                }

            }

        }

    SecZeroAndFreeMemory(generatedCheck, sizeof(LUKSHeader->rawHeader.mk_digest));
    SecZeroAndFreeMemory(splitData, splitDataLength);
    SecZeroAndFreeMemory(pwd_PBKDF2ed, pwd_PBKDF2edLength);
    SecZeroAndFreeMemory(masterKey, LUKSHeader->rawHeader.key_bytes);

    allOK = (allOK && ((*keySlot) != -1));

    return allOK;
}


// ----------------------------------------------------------------------------
BOOL LUKS_IdentifyHash(
    char* hashSpec, 
    WCHAR *hashKernelModeDeviceName,
    GUID* hashGUID
)
{
    BOOL allOK;
    DRIVER_AND_ALG currImpl;
    unsigned int i;
    char normalisedTitleLUKSified[MAX_PRETTYPRINTED_TITLE];
    char uppercaseHashSpec[LUKS_HASHSPEC_L];

    BOOL foundMatching;
    BOOL gotImplDetails;

    int countImplCypher = 0;
    DRIVER_AND_ALG* implCypher = NULL;  // Array of options
    int countImplHash = 0;
    DRIVER_AND_ALG* implHash = NULL;  // Array of options
    int implIdx;
    char normalisedTitle[MAX_PRETTYPRINTED_TITLE];
    char tmpTitle[MAX_PRETTYPRINTED_TITLE];
    char* tmpPtr;
    int charIdx;

    allOK = TRUE;
    foundMatching = FALSE;

    gotImplDetails = FALSE;

    // Standardise to uppercase; removes any potential problems with case
    // sensitivity
    SDUstrtoupper(uppercaseHashSpec, hashSpec);

    // Obtain details of all hashes...
    if (allOK)
        {
        gotImplDetails = driver_GetAllAlgorithmDriverOptions(
                                &countImplHash,
                                &implHash,

                                &countImplCypher,
                                &implCypher,

                                TRUE
                                );
        }


    // Locate the specific FreeOTFE hash to use...
    if (allOK) 
        {
        for (
            implIdx = 0; 
            implIdx < countImplHash; 
            implIdx++
            )
            {
            currImpl = implHash[implIdx];

            // Strip out any "-" or whitespace in the hash's title; LUKS hashes
            // don't have these characters in their names
            memset(normalisedTitle, 0, sizeof(normalisedTitle));
            charIdx = 0;
            for (i = 0; i < strlen(currImpl.Hash.Title); i++)
                {
                if ( 
                    ((currImpl.Hash.Title)[i] != ' ') &&
                    ((currImpl.Hash.Title)[i] != '-')
                    )
                    {
                    normalisedTitle[charIdx] = (currImpl.Hash.Title)[i];
                    charIdx++;
                    }
                }
            strcpy(tmpTitle, normalisedTitle);
            SDUstrtoupper(normalisedTitle, tmpTitle);

            // The Tiger hash is called "tgr" under Linux (why?!!)
            // Because of this weirdness, we have to carry out an additional
            // check for LUKS volumes encrypted with "tgr" (Tiger) cypher
            // Note: The tiger hash isn't covered by the LUKS specification; it's
            //       support here (and in the Linux implementation) is an
            //       *optional* extension to the standard.
            // Note: Tiger isn't included in the v2.6.11.7 Linux kernel, but is
            //       included in the v2.6.19 kernel
            //
            // Examples of Tiger hash specs:
            //   tgr128
            //   tgr160
            //   tgr190
            strcpy(normalisedTitleLUKSified, normalisedTitle);

            if (strstr(normalisedTitleLUKSified, FREEOTFE_TIGER) != NULL) 
                {
                // If there's no numbers in our FreeOTFE's hash driver's title, add
                // on the hash length in bits; see the example above of this hash's
                // hashspec under Linux
                if (!(LUKS_StringHasNumbers(normalisedTitleLUKSified))) 
                    {
                    sprintf(tmpTitle, "%s%d", normalisedTitleLUKSified, currImpl.Hash.Length);
                    strcpy(normalisedTitleLUKSified, tmpTitle);
                    }

                tmpPtr = SDUstrreplace(
                                normalisedTitleLUKSified,
                                FREEOTFE_TIGER,
                                LUKS_TIGER
                                );
                strcpy(normalisedTitleLUKSified, tmpPtr);
                SecZerocharMemory(tmpPtr);
                SDUstrtoupper(normalisedTitleLUKSified, normalisedTitleLUKSified);
                }

            // A similar thing happens with Whirlpool...
            // Examples of Whirlpool hash specs:
            //   wp256
            //   wp384
            //   wp512
            if (strstr(normalisedTitleLUKSified, FREEOTFE_WHIRLPOOL) != NULL) 
                {
                // If there's no numbers in our FreeOTFE's hash driver's title, add
                // on the hash length in bits; see the example above of this hash's
                // hashspec under Linux
                if (!(LUKS_StringHasNumbers(normalisedTitleLUKSified))) 
                    {
                    sprintf(tmpTitle, "%s%d", normalisedTitleLUKSified, currImpl.Hash.Length);
                    strcpy(normalisedTitleLUKSified, tmpTitle);
                    }

                tmpPtr = SDUstrreplace(
                                normalisedTitleLUKSified,
                                FREEOTFE_WHIRLPOOL,
                                LUKS_WHIRLPOOL
                                );
                strcpy(normalisedTitleLUKSified, tmpPtr);
                SecZerocharMemory(tmpPtr);
                SDUstrtoupper(normalisedTitleLUKSified, normalisedTitleLUKSified);
                }

            // Check current hash implementation details against volume's
            if (
                (strcmp(normalisedTitle, uppercaseHashSpec) == 0) ||
                (strcmp(normalisedTitleLUKSified, uppercaseHashSpec) == 0) 
               )
                {
                // Valid hash found
                *hashGUID = currImpl.DriverImplAlgGUID;
                wcscpy(hashKernelModeDeviceName, currImpl.DriverFilename);
                foundMatching = TRUE;
                break;
                }

            }
        }

    if (gotImplDetails) 
        {
        driver_FreeAllAlgorithmDriverOptions(
                    &countImplHash,
                    &implHash,

                    &countImplCypher,
                    &implCypher
                    );
        }


    return (allOK && foundMatching);
}


// ----------------------------------------------------------------------------
BOOL LUKS_IdentifyCypher_SearchCyphers(
    int countImplCypher,
    DRIVER_AND_ALG* implCypher,

    char* cypherName,
    int keySizeBits,
    CYPHER_MODE useCypherMode,

    WCHAR *cypherKernelModeDeviceName,
    GUID* cypherGUID
)
{
    BOOL allOK;
    DRIVER_AND_ALG currImpl;
    BOOL foundMatch;
    int implIdx;
    char uppercaseCypherName[LUKS_CIPHERNAME_L];
    char uppercaseCurr[MAX_PRETTYPRINTED_TITLE];

    allOK = TRUE;
    foundMatch = FALSE;


    SDUstrtoupper(uppercaseCypherName, cypherName);

    // Locate the specific FreeOTFE cypher to use...
    if (allOK) 
        {
        for (
            implIdx = 0; 
            implIdx < countImplCypher; 
            implIdx++
            )
            {
            currImpl = implCypher[implIdx];

            SDUstrtoupper(uppercaseCurr, currImpl.Cypher.Title);

            // Check current cypher implementation details against volume's
            if (
                (strcmp(uppercaseCurr, uppercaseCypherName) == 0) &&
                (currImpl.Cypher.KeySizeRequired == keySizeBits) &&
                (currImpl.Cypher.Mode == useCypherMode)
                ) 
                {
                // Valid cypher found
                *cypherGUID = currImpl.DriverImplAlgGUID;
                wcscpy(cypherKernelModeDeviceName, currImpl.DriverFilename);
                foundMatch = TRUE;
                break;
                }

            }
        }


    return (allOK && foundMatch);
}


// ----------------------------------------------------------------------------
BOOL LUKS_IdentifyCypher(
    char* cypherName,
    int keySizeBits,
    char* cypherMode,
    BOOL baseIVCypherOnHashLength,

    WCHAR *cypherKernelModeDeviceName,
    GUID* cypherGUID,
    SECTOR_IV_GEN_METHOD* sectorIVGenMethod,
    WCHAR *IVHashKernelModeDeviceName,
    GUID* IVHashGUID,
    WCHAR *IVCypherKernelModeDeviceName,
    GUID* IVCypherGUID
)
{
    BOOL allOK;
    CYPHER_MODE currCypherMode;
    CYPHER_MODE useCypherMode;
    char* IVGenMethod;
    HASH IVHashDetails;
    char ucCypherMode[LUKS_CIPHERMODE_L];
    char ucCypherName[LUKS_CIPHERNAME_L];
    char tmpUCtestMode[LUKS_CIPHERMODE_L];
    char* tmpPtr;
    WCHAR strMode_WCHAR[10];  // Enough to hold any cypher mode as a string
    char strMode_char[10];  // Enough to hold any cypher mode as a string

    BOOL gotImplDetails;

    int countImplCypher = 0;
    DRIVER_AND_ALG* implCypher = NULL;  // Array of options
    int countImplHash = 0;
    DRIVER_AND_ALG* implHash = NULL;  // Array of options


    allOK = TRUE;

    gotImplDetails = FALSE;

    // Standardise to uppercase; removes any potential problems with case
    // sensitivity
    SDUstrtoupper(ucCypherMode, cypherMode);
    SDUstrtoupper(ucCypherName, cypherName);


    // Detect IV generation method
    // Examples: ecb
    //           cbc-plain
    //           lrw-plain
    //           cbc-essiv:sha256
    // The stuff before the "-" is the cypher mode; the stuff after is the IV
    // generation
    // Note: ESSIV is NOT specified in the LUKS specification, but LUKS implements
    //       this anyway as "-essiv:<hash>"
    if (allOK) 
        {
        // Fallback to none... (e.g. ECB)
        *sectorIVGenMethod = SCTRIVGEN_NONE;
        wcscpy(IVHashKernelModeDeviceName, TEXT(""));
        *IVHashGUID = FREEOTFE_NULL_GUID;

        tmpPtr = strstr(ucCypherMode, "-");
        if (tmpPtr != NULL)
            {
            tmpPtr[0] = 0;  // Replace "-" with terminating NULL for ucCypherMode
            // tick on a char to point to IV generation method
            IVGenMethod = &(tmpPtr[1]);

            // Search for "-plain" and strip off cypher mode
            SDUstrtoupper(tmpUCtestMode, PLAIN_IV);
            tmpPtr = strstr(IVGenMethod, tmpUCtestMode);
            if (tmpPtr != NULL)
                {
                *sectorIVGenMethod = SCTRIVGEN_32BIT_SECTOR_ID;
                }

            // Search for "-essiv" and strip off cypher mode
            SDUstrtoupper(tmpUCtestMode, ESSIV_IV);
            tmpPtr = strstr(IVGenMethod, tmpUCtestMode);
            if (tmpPtr != NULL)
                {
                *sectorIVGenMethod = SCTRIVGEN_ESSIV;

                // Get the hash
                tmpPtr = strstr(IVGenMethod, ":");
                allOK = (tmpPtr != NULL);
                if (allOK)
                    {
                    tmpPtr = &(tmpPtr[1]);  // Move on 1 char
                    allOK = LUKS_IdentifyHash(
                                              tmpPtr,
                                              IVHashKernelModeDeviceName,
                                              IVHashGUID
                                             );
                    }

                }

            // Search for "-benbi" and strip off cypher mode
            SDUstrtoupper(tmpUCtestMode, BENBI_IV);
            tmpPtr = strstr(IVGenMethod, tmpUCtestMode);
            if (tmpPtr != NULL)
                {
                *sectorIVGenMethod = SCTRIVGEN_NONE;

                // Get the hash
                tmpPtr = strstr(IVGenMethod, ":");
                // Optional/not needed for "benbi"?
                if (tmpPtr != NULL)
                    {
                    tmpPtr = &(tmpPtr[1]);  // Move on 1 char
                    allOK = LUKS_IdentifyHash(
                                tmpPtr,
                                IVHashKernelModeDeviceName,
                                IVHashGUID
                                );
                    }
                }

            }
        }

    // Determine cypher mode
    useCypherMode = CYPHER_MODE_UNKNOWN;
    if (allOK)
        {
        for (
            currCypherMode = _CYPHER_MODE_FIRST; 
            currCypherMode <= _CYPHER_MODE_LAST;
            currCypherMode++
            )
            {
            // Convert cypher mode to simple char text representation, in uppercase
            driver_CypherPrettyprintAlgMode(currCypherMode, strMode_WCHAR);
            memset(strMode_char, 0, sizeof(strMode_char));
            wcstombs(strMode_char, strMode_WCHAR, wcslen(strMode_WCHAR));
            SDUstrtoupper(strMode_char, strMode_char);

            if (strcmp(strMode_char, ucCypherMode) == 0)
                {
                useCypherMode = currCypherMode;
                break;
                }

            }

        allOK = (useCypherMode != CYPHER_MODE_UNKNOWN);
        }

    // Obtain details of all cyphers...
    if (allOK)
        {
        gotImplDetails = driver_GetAllAlgorithmDriverOptions(
                                &countImplHash,
                                &implHash,

                                &countImplCypher,
                                &implCypher,

                                TRUE
                                );
        }


    // Given the information we've determined, identify the FreeOTFE driver to
    // map the details to
    if (allOK)
        {
        allOK = LUKS_IdentifyCypher_SearchCyphers(
                                countImplCypher,
                                implCypher,
                                cypherName,
                                keySizeBits,
                                useCypherMode,
                                cypherKernelModeDeviceName,
                                cypherGUID
                                );
        }

    // Sort out the IV cypher driver
    if (allOK)
        {
        wcscpy(IVCypherKernelModeDeviceName, cypherKernelModeDeviceName);
        *IVCypherGUID = *cypherGUID;

        // Because of a bug in dm-crypt, the cypher used to generate ESSIV IVs may
        // ***NOT*** be the same cypher as used for the bulk encryption of data
        // This is because it passes the IV hash's length to the cypher when it
        // creates the ESSIV cypher - as a result, the ESSIV cypher may have a
        // different keylength to the bulk encryption cypher
        if (
            (baseIVCypherOnHashLength) &&
            (*sectorIVGenMethod == SCTRIVGEN_ESSIV)
            ) 
            {
            allOK = driver_HashGetImplDetails(
                                IVHashKernelModeDeviceName,
                                IVHashGUID,
                                &IVHashDetails
                                );

            if (allOK)
                {
                allOK = LUKS_IdentifyCypher_SearchCyphers(
                                    countImplCypher,
                                    implCypher,
                                    cypherName,
                                    IVHashDetails.Length,
                                    useCypherMode,
                                    IVCypherKernelModeDeviceName,
                                    IVCypherGUID
                                    );
                }
            }

        }

    if (gotImplDetails) 
        {
        driver_FreeAllAlgorithmDriverOptions(
                        &countImplHash,
                        &implHash,

                        &countImplCypher,
                        &implCypher
                        );
        }

    return allOK;
}


// =========================================================================
// =========================================================================
