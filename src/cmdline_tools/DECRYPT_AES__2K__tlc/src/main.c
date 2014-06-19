// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include <stdio.h>

#include <windows.h>  // Required for LARGE_INTEGER

//#include <windows.h> // Required for definition of "byte"

//#define CBC 1
//#include <mycrypt.h>
//#include <mycrypt_cipher.h>
#include <mycrypt_custom.h>


#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#define SECTOR_SIZE 512






// Dump out to "stream" a pretty-printed hex dump of the memory from "data"
// Not perfect, but hey, it'll do for now...
// If dataLength%columns != 0, you may get the ASCII on the last line in
// the wrong place
//
// stream - the stream to which the pretty-printed dump should be sent
// data - pointer to the data to be pretty-printed
// dataLength - amount of data to be dumped out (in bytes)
// columns - number of columns to be used in pretty-printing the hexdump
// indent - pointer to a string to be printed at the start of each line
void HexDump(FILE *stream, const unsigned char *data, int dataLength, int columns, char *indent)
{
  int i;
  int j;

  // Print column numbers
  fprintf(stream, "%s   | ", indent);
  for(j=0; j<columns; j++)
    {
    fprintf(stream, "%02x ", j);
    }
  fprintf(stream, "  ASCII\n");

  // Print line under column numbers
  fprintf(stream, "%s---+-", indent);
  for(j=0; j<columns; j++)
    {
    fprintf(stream, "---");
    }
  fprintf(stream, "-------\n");

  // Print out nicely formatted data
  for(i=0; i<dataLength; i = i + columns)
    {
    fprintf(stream, "%s%02x | ", indent, i);

    // Hex representation
    for(j=0; j<columns; j++)
      {
      fprintf(stream, "%02x ", data[i+j]);
      }

    fprintf(stream, "  ");

    // ASCII representation (where possible)
    for(j=0; j<columns; j++)
      {
      // Print char if it's printable, otherwise print a "."
      if (((data[i+j])>=' ') && ((data[i+j])<='~'))
        {
        fprintf(stream, "%c", data[i+j]);
        }
      else
        {
        fprintf(stream, ".");
        }

      }
    fprintf(stream, "\n");
    }

}




void main(void)
{
    FILE *inFile;
    FILE *outFile;
      
    unsigned char inBuf[SECTOR_SIZE];
    unsigned char outBuf[SECTOR_SIZE];
      
    unsigned char* inBufPtr;
    unsigned char* outBufPtr;
      
    unsigned int x;
    unsigned int i;
      
    int cipher;

    // 20 * 8 = 160 bits
    // - it should be 20, not 32
    unsigned char key[32];
    unsigned char* IV;

    LARGE_INTEGER sectorID;
      
    unsigned char tmpkey[32];
    unsigned char tmpstore[32];
      
      
    symmetric_CBC cbc;
    int errnum;
      
    // Blank out key; ensures data beyond end of buffer is zero'd
    memset(key, 0, sizeof(key));
    // This is the hex representation of the SHA512 hashed password, "passwordpasswordpasswordpassword"
    // i.e.:
    //   B587A6D48AC712CC E8D384E39962EA72 1427ACEF200750B3 4BE5BBB2A56FCD9C 
    //   56EBEB2386DBFBE8 ED287E252183D4A0 1CCFE20A46B3E08E F12322EEF89A28DB
    key[ 0] = 0xB5;
    key[ 1] = 0x87;
    key[ 2] = 0xA6;
    key[ 3] = 0xD4;
    key[ 4] = 0x8A;
    key[ 5] = 0xC7;
    key[ 6] = 0x12;
    key[ 7] = 0xCC;
    key[ 8] = 0xE8;
    key[ 9] = 0xD3;
    key[10] = 0x84;
    key[11] = 0xE3;
    key[12] = 0x99;
    key[13] = 0x62;
    key[14] = 0xEA;
    key[15] = 0x72;
    key[16] = 0x14;
    key[17] = 0x27;
    key[18] = 0xAC;
    key[19] = 0xEF;
    key[20] = 0x20;
    key[21] = 0x07;
    key[22] = 0x50;
    key[23] = 0xB3;
    key[24] = 0x4B;
    key[25] = 0xE5;
    key[26] = 0xBB;
    key[27] = 0xB2;
    key[28] = 0xA5;
    key[29] = 0x6F;
    key[30] = 0xCD;
    key[31] = 0x9C;
    key[32] = 0x56;
    key[33] = 0xEB;
    key[34] = 0xEB;
    key[35] = 0x23;
    key[36] = 0x86;
    key[37] = 0xDB;
    key[38] = 0xFB;
    key[39] = 0xE8;
    key[40] = 0xED;
    key[41] = 0x28;
    key[42] = 0x7E;
    key[43] = 0x25;
    key[44] = 0x21;
    key[45] = 0x83;
    key[46] = 0xD4;
    key[47] = 0xA0;
    key[48] = 0x1C;
    key[49] = 0xCF;
    key[50] = 0xE2;
    key[51] = 0x0A;
    key[52] = 0x46;
    key[53] = 0xB3;
    key[54] = 0xE0;
    key[55] = 0x8E;
    key[56] = 0xF1;
    key[57] = 0x23;
    key[58] = 0x22;
    key[59] = 0xEE;
    key[60] = 0xF8;
    key[61] = 0x9A;
    key[62] = 0x28;
    key[63] = 0xDB;


    sectorID.QuadPart = 0;
      
    // Initialize cipher
    cipher = register_cipher(&aes_desc);
    if (cipher == -1)
        {
        printf ("Could not register cipher\n");
        exit (-1);
        }
    else
        {    
        cipher = find_cipher("aes");
        if (cipher == -1)
            {
            printf ("Could not find cipher\n");
            exit(-1);
            }        
        }
  

      
    IV = (unsigned char*)malloc((size_t)aes_desc.block_length);
    if (IV == NULL)
        {
        printf("Unable to malloc IV\n");
        exit(-1);
        }

//    ppppppppppppppppppp
        
        // Key setup; 2000 times

        
        // "tmpkey" is the hash of the user's key (i.e. it's "key")
        memcpy(tmpkey, key, 32);
        tmpkey[0] ^= 1;

        // encrypt "key" 2000 times using "tmpkey"
        i = 2000;


printf("ka\n");                
            while(i > 0) {
                // Just in case of badly behaved cypher implementation...
                memset(tmpstore, 0, sizeof(tmpstore));


        // Reinitialize the IV to zero
        memset(IV, 0, aes_desc.block_length);
        // Start a CBC session
        // 32 - the keysize in bytes
        // Note: AES256, so only the 1st 32 bytes (256 bit = 8*32) of the hash are used
        if ((errnum = cbc_start(cipher, IV, tmpkey, 32, 0, &cbc)) != CRYPT_OK)
            {
            printf ("CBC Setup: %s\n", error_to_string(errnum));
            exit (-1);
            }

        // Reinitialize the IV to zero
                if ((errnum = cbc_encrypt(&key[0], &tmpstore[0], &cbc)) != CRYPT_OK)
                    {
                    printf ("CBC result: %s\n", error_to_string(errnum));
                    exit (-1);
                    } 

        // Reinitialize the IV to zero
        memset(IV, 0, aes_desc.block_length);
        // Start a CBC session
        // 32 - the keysize in bytes
        // Note: AES256, so only the 1st 32 bytes (256 bit = 8*32) of the hash are used
        if ((errnum = cbc_start(cipher, IV, tmpkey, 32, 0, &cbc)) != CRYPT_OK)
            {
            printf ("CBC Setup: %s\n", error_to_string(errnum));
            exit (-1);
            }

                if ((errnum = cbc_encrypt(&key[16], &tmpstore[16], &cbc)) != CRYPT_OK)
                    {
                    printf ("CBC result: %s\n", error_to_string(errnum));
                    exit (-1);
                    } 

				memcpy(&key[16], &tmpstore[ 8], 8);
				memcpy(&key[ 8], &tmpstore[16], 8);
				memcpy(&key[ 0], &tmpstore[ 0], 8);
				memcpy(&key[24], &tmpstore[24], 8);


				i--;
			}

        printf("jiji\n");
//ppppppppppppppppppp


    inFile = fopen("inFile.dat", "rb");
    if (inFile == NULL)
        {
        printf("Unable to open inFile file\n");
        exit(-1);
        }
    outFile = fopen("outFile.dat", "wb");
    if (outFile == NULL)
        {
        printf("Unable to open outFile file\n");
        exit(-1);
        }
      
    printf("Sector size: %d\n", SECTOR_SIZE);
      

    // Main loop
printf("x\n");
    while (!feof(inFile)) 
        {
printf("a\n");
        // Reinitialize the IV to zero
        memset(IV, 0, aes_desc.block_length);
        // Cast the IV (unsigned cast) as a ptr to a DWORD, then dereference to set
        *(DWORD*)IV                 = sectorID.LowPart;
        // Cast the IV (unsigned cast) as a ptr to a DWORD, then dereference to set
        // The IV is offset by sizeof(DWORD), since the IV is in unsigned chars,
        // before it is cast
        *(DWORD*)(IV+sizeof(DWORD)) = sectorID.HighPart;

        printf("Processing sector: %.8x:%.8x\n", sectorID.HighPart, sectorID.LowPart);
        

        // Start a CBC session
        // 32 - the keysize in bytes
        if ((errnum = cbc_start(cipher, IV, key, 32, 0, &cbc)) != CRYPT_OK)
            {
            printf ("CBC Setup: %s\n", error_to_string(errnum));
            exit (-1);
            }
        
        x = fread(inBuf, 1, SECTOR_SIZE, inFile);
        
        if (x>0)
            {    
            printf("Bytes read: %d\n", x);
            // Pad out, if needed
            if (x<SECTOR_SIZE)
                {
                // Zero out any part of the unused buffer
                // This *shouldn't* be needed - it depends on the cypher implementation,
                // and whether it reads beyond the data supplied to it
                memset(&inBuf[x], 0, (SECTOR_SIZE-x));
                }

            // This *shouldn't* be needed - it depends on the cypher implementation,
            // and whether it reads from the output buffer before writing to it
            memset(outBuf, 0, SECTOR_SIZE);

                
            inBufPtr = inBuf;
            outBufPtr = outBuf;

            // Slight cast weirdness to please PC-Lint
            x = (unsigned int)(int)(SECTOR_SIZE / aes_desc.block_length);
            for (i = 0; i < x; i++)
                {
                if ((errnum = cbc_decrypt(inBufPtr, outBufPtr, &cbc)) != CRYPT_OK)
                    {
                    printf ("CBC result: %s\n", error_to_string(errnum));
                    exit (-1);
                    } 
                
                inBufPtr  += aes_desc.block_length;
                outBufPtr += aes_desc.block_length;
                }

            HexDump(stdout, (const unsigned char*)outBuf, 64, 8, (char *)"");

            fwrite(outBuf, 1, SECTOR_SIZE, outFile);

            // Increment sector ID, ready for next...
            sectorID.QuadPart++;

            //exit(1);
            }
        }  // while (!feof(inFile))
      
printf("c\n");
      
    free(IV);
      
    fclose(inFile);
    fclose(outFile);  
}


