// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#include <stdio.h>

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
    unsigned char keyIdx;
    unsigned int keyBytes;

      
    // Blank out key; ensures data beyond end of buffer is zero'd
    memset(key, 0, sizeof(key));
    key[ 0] = 'p';
    key[ 1] = 'a';
    key[ 2] = 's';
    key[ 3] = 's';
    key[ 4] = 'w';
    key[ 5] = 'o';
    key[ 6] = 'r';
    key[ 7] = 'd';

    keyBytes = 8;  // In *bytes*

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
    while (!feof(inFile)) 
        {
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

            inBufPtr = inBuf;
            outBufPtr = outBuf;

            if (keyBytes<1)
                {
                memcpy(outBuf, inBuf, SECTOR_SIZE);
                }
            else
                {
                keyIdx = 0;
                for (i = 0; i < SECTOR_SIZE; i++)
                    {
                    outBufPtr[i] = inBufPtr[i] ^ key[keyIdx];

                    // Increment key ptr, ready for next...
                    keyIdx++;
                    if (keyIdx >= keyBytes)
                        {
                        keyIdx = 0;
                        }

                    }

                }

            HexDump(stdout, (const unsigned char*)outBuf, 64, 8, (char *)"");

            fwrite(outBuf, 1, SECTOR_SIZE, outFile);
            //exit(1);
            }

        }  // while (!feof(inFile))
  
  
  fclose(inFile);
  fclose(outFile);  
}


