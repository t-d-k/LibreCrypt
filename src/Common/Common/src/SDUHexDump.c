
#include <stdio.h>


// =========================================================================
// Returns the *estimated* number of characters that a call to 
// SDUPrettyPrintHex(...) will require to prettyprint data.          
unsigned int SDUPrettyPrintHexBufReq(
  const unsigned int bytes,  // Size of data to be prettyprinted (in bytes)
  const int width // Number of bytes per line (typically 8 or 16)
)
{
  int estLines;
  int estBytesPerLine;

  estLines = (bytes / width) + 1;
  estBytesPerLine = (width * 4) + 8 + 6 + 3 + 4;
  /*
                             ^ 2 chars + space (hex rep) + 1 char (ASCII rep)
                                  ^ offset
                                      ^ 2 x column dividers and their spacing
                                              ^ newline
  */

  return (estLines * estBytesPerLine);
}


// =========================================================================
// Starting from offset "offset" (zero indexed) in "data", read "bytes" bytes
// and populate "output" with a null terminated pretty printed representation
// of data
// Each line in "output" will relate to "width" bytes from "data"
// data - Pointer to the data to be prettyprinted
// offset - Offset from the start of data to begin prettyprinting from
// bytes - Number of bytes from data to be prettyprinted
// output - Pointer to a buffer to receive the prettprinted output
//          This must be at *least* SDUPrettyPrintHexBufReq bytes long
// outputLength - Length of output buffer
// Returns: 0 on success, nonzero on failure (e.g. output buffer too small)
int SDUPrettyPrintHex(
  const unsigned char *data,  // Data to be prettyprinted
  const unsigned int offset,  // Offset within data from which to start dump
  const unsigned int bytes,  // Size of data in bytes
  char* output,
  unsigned int outputLength,
  const int width // Number of bytes per line (typically 8 or 16)
)
{
  int allOK;
  unsigned int inputOffset;
  unsigned int outputOffset;
  int i;


  allOK = 0;

  if (SDUPrettyPrintHexBufReq(
                              bytes,
                              width
                             ) > outputLength)
    {
    allOK = 1;
    return allOK;
    }


  inputOffset = offset;
  outputOffset = 0;


  while (inputOffset < (offset+bytes))
    {
    // Display offset 
    outputOffset += sprintf(
                            &(output[outputOffset]),
                            "%.08X | ",
                            inputOffset
                           );

    // Hex representation 
    for (i = 0; i < width; i++)
      {
      // >= because we index from 0 
      if ((inputOffset+i) >= (offset+bytes))
        {
        outputOffset += sprintf(
                                &(output[outputOffset]),
                                "   "
                               );
        }
      else
        {
        outputOffset += sprintf(
                                &(output[outputOffset]),
                                "%.02X ",
                                data[(inputOffset+i)]
                               );
        }

      }


    outputOffset += sprintf(
                            &(output[outputOffset]),
                            "| "
                           );

    // ASCII representation 
    for (i = 0; i < width; i++)
      {
      // >= because we index from 0 
      if ((inputOffset+i) >= (offset+bytes))
        {
        outputOffset += sprintf(
                                &(output[outputOffset]),
                                " "
                               );
        }
      else
        {
        if (
            (data[(inputOffset + i)] >= 32) &&
            (data[(inputOffset + i)] < 127)
           )
          {
          outputOffset += sprintf(
                                  &(output[outputOffset]),
                                  "%c",
                                  data[(inputOffset + i)]
                                 );
          }
        else
          {
          outputOffset += sprintf(
                                  &(output[outputOffset]),
                                  "."
                                 );
          }
        }

      }


    outputOffset += sprintf(
                            &(output[outputOffset]),
                            "\n"
                           );

    inputOffset += width;
    }

  output[outputOffset] = 0;

  return allOK;
}


// =========================================================================
// As SDUPrettyPrintHex(...), but automatically allocates memory for dump
// Note: CALLER is responsible for freeing off the buffer returned.
// Returns: Buffer containing prettyprinted hex dump, or NULL on failure
char* SDUPrettyPrintHexAlloc(
  const unsigned char *data,  // Data to be prettyprinted
  const unsigned int offset,  // Offset within data from which to start dump
  const unsigned int bytes,  // Size of data in bytes
  const int width // Number of bytes per line (typically 8 or 16)
)
{
  char* retval;
  unsigned int bufSize;

  bufSize = SDUPrettyPrintHexBufReq(bytes, width);
  retval = malloc(bufSize);
  if (retval != NULL)
    {
    if (SDUPrettyPrintHex(
                      data,
                      offset,
                      bytes,
                      retval,
                      bufSize,
                      width
                     ) != 0)
      {
      free(retval);
      retval = NULL;
      }
    }

  return retval;
}


// =========================================================================
// =========================================================================

