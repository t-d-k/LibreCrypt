
#ifndef _SDUHexDump_H
#define _SDUHexDump_H   1

// =========================================================================
// Returns the *estimated* number of characters that a call to 
// SDUPrettyPrintHex(...) will require to prettyprint data.          
unsigned int SDUPrettyPrintHexBufReq(
  const unsigned int bytes,  // Size of data to be prettyprinted (in bytes)
  const int width // Number of bytes per line (typically 8 or 16)
);

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
);

// As SDUPrettyPrintHex(...), but automatically allocates memory for dump
// Note: CALLER is responsible for freeing off the buffer returned.
// Returns: Buffer containing prettyprinted hex dump, or NULL on failure
char* SDUPrettyPrintHexAlloc(
  const unsigned char *data,  // Data to be prettyprinted
  const unsigned int offset,  // Offset within data from which to start dump
  const unsigned int bytes,  // Size of data in bytes
  const int width // Number of bytes per line (typically 8 or 16)
);


// =========================================================================
// =========================================================================

#endif

