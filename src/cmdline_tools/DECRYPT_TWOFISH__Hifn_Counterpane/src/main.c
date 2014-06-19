// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include <stdio.h>


#include <windows.h> // Required for definition of "byte"


#include "AES.H"

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
void HexDump(FILE *stream, unsigned char *data, int dataLength, int columns, char *indent)
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




char convertNibbleToASCII(unsigned int nibble)
{
    char retval = 0x00;

    if (nibble<0x0A)
        {
        retval = (int)'0'+nibble;
        }
    else
        {
        retval = (int)'A'+nibble-0x0A;
        }

    return retval;
}


void convertDataToASCIIRep(
  unsigned char* inKeyData,
  unsigned int inKeyLength, // in Bits
  char** outASCIIKey
)
{
    int i;
    unsigned int nibble;
    char* ASCIIKey;

    *outASCIIKey = (char*)malloc((inKeyLength/4)+2);

    for (i=0; i<(inKeyLength/8); i++)
        {
        nibble = (inKeyData[i] & 0xF0) >> 4;
        (*outASCIIKey)[(i*2)] = convertNibbleToASCII(nibble);
        nibble = inKeyData[i] & 0x0F;
        (*outASCIIKey)[((i*2)+1)] = convertNibbleToASCII(nibble);
        }
        
    (*outASCIIKey)[(inKeyLength/4)]   = 0;
    (*outASCIIKey)[(inKeyLength/4)+1] = 0;      
}




void main(void)
{
    FILE *inFile;
    FILE *outFile;
      
    char inBuf[SECTOR_SIZE];
    char outBuf[SECTOR_SIZE];
    int x;
    int i;
      
      
    char *password = "password";
    // 20 * 8 = 160 bits
    //char *keyMaterial;
    char keyMaterial[10240];
      
    // This is the ASCII representation of the hashed password, "password"
    char *passwordHashASCII = NULL;
    // "2C08E8F5884750A7B99F6F2F342FC638DB25FF31";
    char nibbleKeyMaterial[500];
      
     
    cipherInstance cipher;
    keyInstance keyInst;
      
      

    for (i = 0; i<sizeof(keyMaterial); i++)
        {
        keyMaterial[i] = 0x00;
        }
    // This is the ASCII representation of the RMD160 hashed password, "password"
    keyMaterial[ 0] = 0x2C;
    keyMaterial[ 1] = 0x08;
    keyMaterial[ 2] = 0xE8;
    keyMaterial[ 3] = 0xF5;
    keyMaterial[ 4] = 0x88;
    keyMaterial[ 5] = 0x47;
    keyMaterial[ 6] = 0x50;
    keyMaterial[ 7] = 0xA7;
    keyMaterial[ 8] = 0xB9;
    keyMaterial[ 9] = 0x9F;
    keyMaterial[10] = 0x6F;
    keyMaterial[11] = 0x2F;
    keyMaterial[12] = 0x34;
    keyMaterial[13] = 0x2F;
    keyMaterial[14] = 0xC6;
    keyMaterial[15] = 0x38;
    keyMaterial[16] = 0xDB;
    keyMaterial[17] = 0x25;
    keyMaterial[18] = 0xFF;
    keyMaterial[19] = 0x31;

    // Garbage
    keyMaterial[20] = 0xFF;
    keyMaterial[21] = 0x0F;
    keyMaterial[22] = 0x00;
    keyMaterial[23] = 0xF0;
    keyMaterial[24] = 0xFF;
    keyMaterial[25] = 0x0F;
    keyMaterial[26] = 0x00;
    keyMaterial[27] = 0xF0;
    keyMaterial[28] = 0xFF;
    keyMaterial[29] = 0x0F;
    keyMaterial[30] = 0x00;
    keyMaterial[31] = 0xF0;
    keyMaterial[32] = 0xFF;
    keyMaterial[33] = 0x0F;
    keyMaterial[34] = 0x00;
    keyMaterial[35] = 0xF0;
      

    convertDataToASCIIRep(
                          (char*)&keyMaterial,
                          160,  // in Bits
                          &passwordHashASCII
                         );

    printf("password as ASCII: %s\n", passwordHashASCII);

    inFile = fopen("inFile.dat", "rb");
    outFile = fopen("outFile.dat", "wb");
      
    printf("Sector size: %d\n", SECTOR_SIZE);
      
        
    while (!feof(inFile)) 
        {
        memset(&cipher, 0, sizeof(cipher));
        memset(&keyInst, 0, sizeof(keyInst));

        if (cipherInit(&cipher, MODE_CBC, NULL) != TRUE)
            {
            printf("Could not cipherInit\n");
            exit(-1);
            }


        if (makeKey(&keyInst, DIR_DECRYPT, 160, passwordHashASCII) != TRUE)
            {
            printf("Could not makeKey\n");
            exit(-1);
            }
                
        //makeKey(&keyInst, DIR_DECRYPT, 160, NULL);
        //for(i = 0; i<=9; i++)
        //    {
        //    keyInst.key32[i] = keyMaterial[i*2] << 24;
        //    keyInst.key32[i] = keyInst.key32[i] & (keyMaterial[(i*2)+1] << 16);
        //    keyInst.key32[i] = keyInst.key32[i] & (keyMaterial[(i*2)+2] << 8);
        //    keyInst.key32[i] = keyInst.key32[i] & (keyMaterial[(i*2)+3]);
        //    }
        //reKey(&keyInst);

        // Reinitialize the IV to zero
        memset(cipher.iv32, 0, BLOCK_SIZE/32);
            
            
        x = fread(&inBuf, 1, SECTOR_SIZE, inFile);
            
        if (x>0)
            {    
            printf("x = %d\n", x);
            // Pad out
            if (x<SECTOR_SIZE)
                {
                // Zero out any part of the unused buffer
                // This *shouldn't* be needed - it depends on the cypher implementation,
                // and whether it reads beyond the data supplied to it
                memset(&inBuf[x], 0, (SECTOR_SIZE-x));
                }

            for(i = 1; i<=SECTOR_SIZE; i++)
                {
                // -1 because the buffer indexes from 0, not 1
                //outBuf[i-1] = 0;
                }
                    
            blockDecrypt(&cipher, &keyInst, (byte*)&inBuf, SECTOR_SIZE * 8, (byte*)&outBuf);
            //blockEncrypt(&cipher, &keyInst, (byte*)&inBuf, SECTOR_SIZE * 8, (byte*)&outBuf);
            HexDump(stdout, outBuf, 64, 8, "");


            fwrite(&outBuf, 1, SECTOR_SIZE, outFile);
            //exit(1);
            }
        }  // while (!feof(inFile))
  
  
  fclose(inFile);
  fclose(outFile);  
}


