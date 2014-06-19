
// Copyright in this code is held by Dr B. R. Gladman but free direct or
// derivative use is permitted subject to acknowledgement of its origin.
// Dr B. R. Gladman                               .   25th January 2000.

// AES_CD = 1 if AES CD is to be used for reference, 0 otherwise

#define AES_CD  0

// Set to 1 to output dots during long tests, 0 otherwise

#define KEEP_USER_HAPPY 0

// Test vector and output file locations and names

char *aes_path = "e:\\testvals\\";				// path for AES CD test vector files
char *loc_path = "d:\\cpp\\aes2\\testvals\\";	// path for local test vector files
char *out_path = "d:\\cpp\\aes2\\outvals\\";	// path for local output files

char *aes_name[6] =                             // AES test vector file names
{ "\\ecb_vk.txt",  "\\ecb_vt.txt",  "\\ecb_e_m.txt",
  "\\ecb_d_m.txt", "\\cbc_e_m.txt", "\\cbc_d_m.txt"
};

#if AES_CD
#  define   ref_path    aes_path
#else
#  define   ref_path    loc_path
#endif

