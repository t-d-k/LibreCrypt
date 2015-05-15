/*
licence:gpl
(c) tdk
project to test FreeOTFECypherAES_Gladman.c under doxbox\src\Common\CYPHER_DRIVERS\CYPHER_AES_Gladman\ 
and integration with 3rd party code 
tests cipher modes and test vectors 


fns to test - not v1

// Cypher driver init function
// devExt - The device extension to have it's "Driver identification and
//          cyphers supported" members initialized
ImpCypherDriverExtDetailsInit_v3(
 

// Cypher driver cleardown function
// devExt - The device extension to have it's "Driver identification and
//          cyphers supported" members cleared down
ImpCypherDriverExtDetailsCleardown_v3(


// Encryption function
// Note: CyphertextLength must be set to the size of the CyphertextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
ImpCypherEncryptData(

// Decryption function
// Note: PlaintextLength must be set to the size of the PlaintextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
ImpCypherDecryptData(

// Encryption function
// Note: CyphertextLength must be set to the size of the CyphertextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
ImpCypherEncryptSectorData(


// Decryption function
// Note: PlaintextLength must be set to the size of the PlaintextData buffer on
//       entry; on exit, this will be set to the size of the buffer used.
ImpCypherDecryptSectorData(


// LRW uses big endian
LARGE_INTEGER__To__INTEGER_128_BigEndian(

// XTS uses little endian
LARGE_INTEGER__To__INTEGER_128_LittleEndian(

*/

// aes_gladman.cpp : Defines the entry point for the console application.
//
#ifdef FOTFE_PC_DRIVER
#error compile with FOTFE_CMD_LINE only
#endif

#ifdef FOTFE_PDA
#error compile with FOTFE_CMD_LINE only
#endif

#include <assert.h>

#include "FreeOTFECypherAES_Gladman.h"
#include "FreeOTFECypherAPICommon.h"
#include "FreeOTFECypherImpl.h"

// =========================================================================
NTSTATUS
GetCypherDetails_v3(
    IN     GUID* CypherGUID,
    IN OUT CYPHER_v3* CypherDetails
)
{
    NTSTATUS status ;//= STATUS_NOT_FOUND;
    CYPHER_DRIVER_INFO_v3 tmpDevExt;
    unsigned int i;

 //   DEBUGOUTCYPHERDRV(DEBUGLEV_ENTER, ("GetCypherDetails_v3\n"));
    
    // This is a bit of a hack to get the details...
    ImpCypherDriverExtDetailsInit_v3(&tmpDevExt);
    for (i = 0; i<tmpDevExt.CypherCount; i++)
        {
        if (IsEqualGUID(&tmpDevExt.CypherDetails[i].CypherGUID, CypherGUID))
            {
          //  DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Located relevant details\n"));
            RtlCopyMemory(
                          CypherDetails,
                          &tmpDevExt.CypherDetails[i],
                          sizeof(CYPHER_v3)
                         );

            status = STATUS_SUCCESS;
            }
        }

    ImpCypherDriverExtDetailsCleardown_v3(&tmpDevExt);


   // DEBUGOUTCYPHERDRV(DEBUGLEV_EXIT, ("GetCypherDetails_v3\n"));

    return status;
}



// =========================================================================
// 
void
FreeOTFECypher_EncryptSector( const char key[],const char plain[],char act_cyphertext[],GUID cyphGuid)
{
    NTSTATUS status = STATUS_SUCCESS;
    
    PDIOC_CYPHER_SECTOR_DATA_IN DIOCBufferIn;
    PDIOC_CYPHER_DATA_OUT DIOCBufferOut;
    CYPHER_v3 CypherDetails;
    char* keyASCII;
    int keyASCIIBufferLen;
    int dataLength;
    FREEOTFEBYTE* tmpBuffer;
    FREEOTFEBYTE* ptrInKey;   // Pointer to where the key starts in the input buffer
    FREEOTFEBYTE* ptrInIV;    // Pointer to where the IV starts in the input buffer
    FREEOTFEBYTE* ptrInData;  // Pointer to where the data starts in the input buffer
    int i;
	int InputBufferLength = 237,OutputBufferLength=10;

    DIOCBufferIn = (PDIOC_CYPHER_SECTOR_DATA_IN)malloc(sizeof(DIOC_CYPHER_SECTOR_DATA_IN)+256);
	DIOCBufferOut = (PDIOC_CYPHER_DATA_OUT)malloc(64);
	
	DIOCBufferIn->CypherGUID = cyphGuid;

    DIOCBufferIn->KeyLength = 64 *  8;  // Bits -> Bytes
    DIOCBufferIn->IVLength= AES_BLOCK_SIZE;  // Bits -> Bytes
    DIOCBufferIn->DataLength = 64 ;  // Already counted in bytes

	DIOCBufferIn->SectorID.QuadPart =0;
    DIOCBufferIn->SectorSize =16;
	
	for( i = 0;i<AES_BLOCK_SIZE / 8; i++){ DIOCBufferIn->IV[i] = 0;}
	for( i = 0;i<(DIOCBufferIn->KeyLength /8); i++){ DIOCBufferIn->Key[i] = key[i];}
	for( i = 0;i<(DIOCBufferIn->DataLength ); i++){ DIOCBufferIn->Data[i] = plain[i];}	 

    // Actual 
    InputBufferLength  =            
             sizeof(DIOC_CYPHER_SECTOR_DATA_IN)
              - sizeof(DIOCBufferIn->Key)
              - sizeof(DIOCBufferIn->IV)
              - sizeof(DIOCBufferIn->Data)
              + (DIOCBufferIn->KeyLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->IVLength / 8)  // Bits -> Bytes
              + (DIOCBufferIn->DataLength)  // Already counted in bytes
            ;      


    // size of OUTPUT buffer
    // NOTE THAT THIS USES THE DataLength MEMBER OF THE INPUT BUFFER - THE
    // OUTPUT IS THE SAME SIZE AS THE INPUT
    OutputBufferLength =      sizeof(DIOC_CYPHER_DATA_OUT)+(DIOCBufferIn->DataLength * sizeof(DIOCBufferOut->Data))-sizeof(DIOCBufferOut->Data);

    // Request valid, process...

    // Since the variable length members of the input buffer can't be accessed
    // directly (their locations vary, depending on the size of earlier members)
    // we calculate pointers so they can be accessed easily...
    ptrInKey  = (FREEOTFEBYTE*)&DIOCBufferIn->Key;
    ptrInIV   = ptrInKey + ((DIOCBufferIn->KeyLength / 8) * sizeof(DIOCBufferIn->Key));
    ptrInData = ptrInIV + ((DIOCBufferIn->IVLength / 8) * sizeof(DIOCBufferIn->IV));

    // NOTE: THIS IS ONLY DUMPED OUT IF DEBUG IS NOT ENABLED; ONLY ON
    //       DEBUG BUILDS.
    //       Normal release builds cause DEBUGOUTCYPHERDRV to be a NOP, so
    //       this debug code gets removed
    
    
    // Sanity checks on the request wrt the cypher

    
    status = GetCypherDetails_v3(
                              &(DIOCBufferIn->CypherGUID),
                              &CypherDetails
                             );
    assert(NT_SUCCESS(status)); // "Unable to locate requested cypher in driver\n"

    // DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Cypher details got OK.\n"));

    // Input/output buffer must be a multiple of the blocksize
    // Output buffer length not explicitly checked, as we already made sure
    // that it's big enough to store the input data
    // Note: Multiply DataLength by 8 to get bits
    assert (!((CypherDetails.BlockSize > 0) && (((DIOCBufferIn->DataLength * 8) % CypherDetails.BlockSize) != 0)));
        
      //  DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Length of data IN must be a multiple of the blocksize (bs: %d bits; bufSize: %d bits\n",
        
    // IV length must = the blocksize
    assert(! ((CypherDetails.BlockSize > 0) && (CypherDetails.BlockSize != DIOCBufferIn->IVLength)));
        
       // DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("The cypher blocksize is greater than 0; the IV length MUST equal the cypher's blocksize (cypher blocksize: %d bits; supplied IV was: %d bits\n", CypherDetails.BlockSize, DIOCBufferIn->IVLength));
        
    // KeyLength must = the keysize
    assert(! ((CypherDetails.KeySizeRequired >= 0) && (CypherDetails.KeySizeRequired != DIOCBufferIn->KeyLength)));
        
      //  DEBUGOUTCYPHERDRV(DEBUGLEV_ERROR, ("Keylength must = the cypher's keysize (cypher keysize: %d bits; supplied keysize: %d bits\n", CypherDetails.KeySize, DIOCBufferIn->KeyLength));
        
  //  DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("Sanity checks OK\n"));

    // ASCII representation of "Key".
    // Number of chars/nibbles
    // +1 to include NULL terminator
    // This is included as an optimisation for
    // ciphers which use the NIST AES API
    keyASCIIBufferLen = ((DIOCBufferIn->KeyLength / 4)+1);
    keyASCII = (char*)FREEOTFE_MEMALLOC(keyASCIIBufferLen);    
 //   DEBUGOUTCYPHERDRV(DEBUGLEV_INFO, ("keyASCIIBufferLen = %d bytes\n", keyASCIIBufferLen));

    // Convert the data passed in to it's ASCII representation
    ConvertDataToASCIIRep(
                          DIOCBufferIn->KeyLength,  // In bits
                          ptrInKey,
                          keyASCII
                         );

    assert (NT_SUCCESS(status));
        
        // Create a tmp buffer; this is done so that we don't pass both
        // the DIOC input and output buffers (the *same* buffer!) to the
        // implementation function
        // (Also backup dataLength for the same reason)
        dataLength = DIOCBufferIn->DataLength;
     //   tmpBuffer = (char*)FREEOTFE_MEMALLOC(dataLength);    
	//	SecZeroMemory(tmpBuffer, dataLength);
    SecZeroMemory(act_cyphertext, dataLength);
        status = ImpCypherEncryptSectorData(
                                    &DIOCBufferIn->CypherGUID,
                                    DIOCBufferIn->SectorID,
                                    DIOCBufferIn->SectorSize,
                                    DIOCBufferIn->KeyLength,  // In bits
                                    ptrInKey,
                                    keyASCII,
                                    DIOCBufferIn->IVLength,  // In bits
                                    ptrInIV,
                                    DIOCBufferIn->DataLength,  // In bytes
                                    ptrInData,
                                    act_cyphertext
                                    );    


      

    SecZeroMemory(keyASCII, keyASCIIBufferLen);
    FREEOTFE_FREE(keyASCII);
   
  assert(status == STATUS_SUCCESS);  
}


// use ascii for now, to match main gui _tmain(int argc, _TCHAR* argv[])
int  main(int argc, char* argv[])
{
	GUID expGuid = {0,0,0,{0,0,0,0,0,13,0,1}};
	GUID cyphGuid = {0,0,0,{0,0,0,0,13,0,0,8}};
	// unsigned char keyAscii[] = "password";
	unsigned char iv[AES_BLOCK_SIZE / 8];
	unsigned char key[64]		= {1,2,3,0,0,	0,0,0,0,0,	0,0,0,0,0,	0, 
								   0,0,0,0,0,   0,0,0,0,0,	0,0,0,0,0,	0,
								   0,0,0,0,0,   0,0,0,0,0,	0,0,0,0,0,	0,
								   0,0,0,0,0,   0,0,0,0,0,	0,0,0,0,0,	0,	
	};
	unsigned char plaintext[64] = "1234567890abcdefghijklmnopqrstuvwxyz";
	unsigned char cyphertext[64] = {  116, 237,  55,  50,  61,	217, 148, 134, 119, 100,	211, 214, 115, 198, 136,	108,
									  165,  11, 240, 109,  10,	212,  69, 244,  41, 162,	101,  99, 152, 193,  72,		72, 
									  103,  65, 197, 232, 251,	 66, 144,  79, 188,  67,     14, 225, 181, 112,   9,		60, 
									  169, 103, 234,  36, 144,	165, 105, 234, 157, 167,	183, 224, 193, 160,  49,		241};

	unsigned char act_cyphertext[64];
	LARGE_INTEGER sectorID;
	int i;
	int keyLengthBits;

	CYPHER_DRIVER_INFO_v3 devExt;
	NTSTATUS res;
	res = ImpCypherDriverExtDetailsInit_v3(&devExt);
	assert(!res);
	assert(!memcmp(&devExt.DriverGUID,&expGuid,sizeof(expGuid)));
	assert(!strncmp(devExt.DriverTitle,"AES (Gladman library)",256));
	assert(devExt.DriverVersionID == 83886080);
	assert(devExt.CypherCount == 6);
	assert(!memcmp(&devExt.CypherDetails->CypherGUID,&cyphGuid,sizeof(cyphGuid)));
	assert(!strncmp(devExt.CypherDetails->Title,"AES",5));
	assert(devExt.CypherDetails->Mode == CYPHER_MODE_XTS);
	assert(devExt.CypherDetails->KeySizeUnderlying == 256);
	assert(devExt.CypherDetails->BlockSize == 128);
	assert(devExt.CypherDetails->VersionID == 83886080);
	assert(devExt.CypherDetails->KeySizeRequired == 512);

	keyLengthBits = devExt.CypherDetails->KeySizeRequired;
	assert(keyLengthBits/8 == 64);//prereq
	res =ImpCypherDriverExtDetailsCleardown_v3(&devExt);
assert(!res);
assert(devExt.CypherDetails == NULL);
assert(devExt.CypherCount == 0);
sectorID.HighPart = 1;
sectorID.LowPart = 0;
for( i = 0;i<AES_BLOCK_SIZE / 8; i++){ iv[i] = 0;}

FreeOTFECypher_EncryptSector(key,plaintext,act_cyphertext,cyphGuid);

  for( i = 0;i<(64 ); i++){
	 // sprintf(&debug[0],"%s, %d",&debug[0],act_cyphertext[i]);
	 assert( act_cyphertext[i] = cyphertext[i]);  
  }	
  // puts(debug);
	return 0;
}

