// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include <stdio.h>


//#include <windows.h> // Required for definition of "byte"

#define CBC 1
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
      
      
    symmetric_CBC cbc;
    int errnum;
      
    // Blank out key; ensures data beyond end of buffer is zero'd
    memset(key, 0, sizeof(key));
    // This is the hex representation of the RMD160 hashed password, "password"
    key[ 0] = 0x2C;
    key[ 1] = 0x08;
    key[ 2] = 0xE8;
    key[ 3] = 0xF5;
    key[ 4] = 0x88;
    key[ 5] = 0x47;
    key[ 6] = 0x50;
    key[ 7] = 0xA7;
    key[ 8] = 0xB9;
    key[ 9] = 0x9F;
    key[10] = 0x6F;
    key[11] = 0x2F;
    key[12] = 0x34;
    key[13] = 0x2F;
    key[14] = 0xC6;
    key[15] = 0x38;
    key[16] = 0xDB;
    key[17] = 0x25;
    key[18] = 0xFF;
    key[19] = 0x31;

    // This is the hex representation of the RMD160 hashed password, "password1234567890ABC"
    key[ 0] = 0xFA;
    key[ 1] = 0xFE;
    key[ 2] = 0x56;
    key[ 3] = 0xC3;
    key[ 4] = 0xBA;
    key[ 5] = 0xB4;
    key[ 6] = 0xCD;
    key[ 7] = 0x21;
    key[ 8] = 0x6B;
    key[ 9] = 0xA0;
    key[10] = 0x24;
    key[11] = 0x74;
    key[12] = 0xAC;
    key[13] = 0x15;
    key[14] = 0x7E;
    key[15] = 0xA5;
    key[16] = 0x55;
    key[17] = 0xFA;
    key[18] = 0x57;
    key[19] = 0x11;


      
    // Initialize cipher
    cipher = register_cipher(&twofish_desc);
    if (cipher == -1)
        {
        printf ("Could not register cipher\n");
        exit (-1);
        }
    else
        {    
        cipher = find_cipher("twofish");
        if (cipher == -1)
            {
            printf ("Could not find cipher\n");
            exit (-1);
            }        
        }
      

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
      
      
    IV = (unsigned char*)malloc((size_t)twofish_desc.block_length);
    if (IV == NULL)
        {
        printf("Unable to malloc IV\n");
        exit(-1);
        }

    // Main loop
    while (!feof(inFile)) 
        {
        // Reinitialize the IV to zero
        memset(IV, 0, twofish_desc.block_length);


        // Start a CBC session
        // 24 - they keysize, which must be ROUNDED UP TO THE NEAREST MULTIPLE OF 64
        // With a RMD160 hash, this is 160 bits; rounded up to 192 bits (24 bytes)
        if ((errnum = cbc_start(cipher, IV, key, 24, 0, &cbc)) != CRYPT_OK)
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

            // Slight cast werirdness to please PC-Lint
            x = (unsigned int)(int)(SECTOR_SIZE / twofish_desc.block_length);
            for (i = 0; i < x; i++)
                {
                if ((errnum = cbc_decrypt(inBufPtr, outBufPtr, &cbc)) != CRYPT_OK)
                    {
                    printf ("CBC result: %s\n", error_to_string(errnum));
                    exit (-1);
                    } 
                    
                inBufPtr  += twofish_desc.block_length;
                outBufPtr += twofish_desc.block_length;
                }

            HexDump(stdout, (const unsigned char*)outBuf, 64, 8, (char *)"");

            fwrite(outBuf, 1, SECTOR_SIZE, outFile);
            //exit(1);
            }

        }  // while (!feof(inFile))
      
  
  free(IV);
  
  fclose(inFile);
  fclose(outFile);  
}


