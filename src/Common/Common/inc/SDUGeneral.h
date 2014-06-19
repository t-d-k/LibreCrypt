// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _SDUGeneral_H
#define _SDUGeneral_H   1

#include <windows.h>  // Required for WCHAR

// =========================================================================
// Macros...

#define SDULengthOf(x)  (sizeof(x) / sizeof(x[0]))


// =========================================================================
// Globals...


// Global buffer used by SDUParamSubstitute(...) if no output buffer passed in
// 20K buffer should be enough for most purposes
char SDUParamSubstitute_GlobalBuffer[1024*20];
WCHAR SDUParamSubstitute_GlobalBufferW[1024*20];

extern const char SDU_EMPTY_STRING[];
extern const WCHAR SDU_EMPTY_STRINGW[];


// =========================================================================
// Consts...

#define PATH_SEPARATOR_CHAR   '\\'

#define FAILED_SETFILEPOINTER     0xFFFFFFFF


// =========================================================================
// Enums...
typedef enum _SDU_OS_VER_PDA {
    SDU_OS_VER_PDA_CE10     = 1,
    SDU_OS_VER_PDA_CE20     = 2,
    SDU_OS_VER_PDA_CE21     = 3,
//    SDU_OS_VER_PDA_CE30     = 4,
    SDU_OS_VER_PDA_WM2002   = 4,
    SDU_OS_VER_PDA_WM2003   = 5,
    SDU_OS_VER_PDA_WM2003SE = 6,
    SDU_OS_VER_PDA_WM2005   = 7,  // aka WM5
    SDU_OS_VER_PDA_CROSSBOW = 8,  // aka WM6
    SDU_OS_VER_PDA_UNKNOWN  = 9999
} SDU_OS_VER_PDA, *PSDU_OS_VER_PDA;


// =========================================================================
// Functions...

void SDUParsePath(
    const WCHAR* fullPath, 
    WCHAR** uncMachine,
    WCHAR** drive,
    WCHAR** path,
    WCHAR** filename,
    WCHAR** fileExtension
);

// Copy the executable's *path* from the specified to the buffer supplied
// Note: The trailing "\" is *not* included
void SDUCpyExeDir(
    WCHAR* buffer
);



void* SDUUnpack_LARGE_INTEGER(const void* ptr, LARGE_INTEGER* val);
void* SDUUnpack_DWORD(const void* ptr, DWORD* val);
void* SDUUnpack_char(const void* ptr, char* val);
void* SDUPack_LARGE_INTEGER(const void* ptr, LARGE_INTEGER val);
void* SDUPack_DWORD(const void* ptr, DWORD val);
void* SDUPack_char(const void* ptr, char val);

BOOL SDUGetVersionInfoShortFmtAlloc(
					   LPWSTR filename,
					   LPWSTR* lpBuffer
					  );
BOOL SDUGetVersionInfoShortFmt(
					   LPWSTR filename,
					   LPWSTR lpBuffer,
					   DWORD nSize
					  );
BOOL SDUGetVersionInfoFmtAlloc(
					   LPWSTR filename,
					   LPWSTR* lpBuffer
					  );
BOOL SDUGetVersionInfoFmt(
					   LPWSTR filename,
					   LPWSTR lpBuffer,
					   DWORD nSize
					  );
BOOL SDUGetVersionInfoShort(
					   LPWSTR Filename,
					   DWORD* Version
					  );
BOOL SDUGetVersionInfo(
					   LPWSTR filename,
					   int* majorVersion,
					   int* minorVersion, 
					   int* revisionVersion, 
					   int* buildVersion
					  );

void SDUTextSetBold(
    HWND hWnd,
    int ctrlID,
    BOOL UseBold
);

void SDUTextSetItalic(
    HWND hWnd,
    int ctrlID,
    BOOL UseItalic
);

void SDUTextSetUnderline(
    HWND hWnd,
    int ctrlID,
    BOOL UseUnderline
);


// Draw text as per DrawText(...), but truncate the string displayed and
// append "..." onto it if it will not fit into the boundrys supplied
// This is implemented for systems where DT_WORD_ELLIPSIS and DT_PATH_ELLIPSIS
// are not supported (e.g. WinCE)
int SDUDrawTextEllipses(
    HDC hdc,
    WCHAR* Text,
    int TextLength,
    LPRECT TextRect,
    UINT uFormat
);

void SDUSetListViewType(HWND hWnd, DWORD newStyle);
void SDUSetWndStyle(HWND hWnd, BOOL SetNotUnset, DWORD Style);
int SDUListView_GetItemTextLength(HWND hWnd, int item, int subItem);

// Return the value of the specified command line parameter "-<parameter>
// value"
// Returns NULL on failure
WCHAR* SDUCommandLineParameter_Name(const WCHAR* parameter);
// Return the value of the command line parameter indicated by the specified
// index
// Returns NULL on failure
WCHAR* SDUCommandLineParameter_Idx(const int parameterIdx);
// Returns TRUE if the specified command line switch could be found, otherwise
// FALSE
BOOL SDUCommandLineSwitch(const WCHAR* parameter);
// Returns the parameter number in the command line of the specified parameter
// Returns -1 on failure
int SDUCommandLineSwitchNumber(const WCHAR* parameter);
// This function only included in the header file for debug purposes
WCHAR* _SDUParamTok(WCHAR* cmdLine);

// Identify which version of Windows Mobile is running
SDU_OS_VER_PDA SDUGetOSVersion_PDA();

// Given a path and filename, this returns the last item in PathAndFilename
// i.e. The filename
// Note: If PathAndFilename is a directory, and ends in a "\", it'll 
//       return a pointer to an empty string
WCHAR* SDUFilenameFromPathAndFilename(WCHAR* PathAndFilename);

// Set s1 to s2, uppercasing all letters
// s1 may be the same as a2, to do an in-place uppercase
char* SDUstrtoupper(char *s1, const char *s2);
WCHAR* SDUwcstoupper(WCHAR *s1, const WCHAR *s2);

// Search and replace one string with another
// Returns a new string; the caller is responsible for freeing this off
char* SDUstrreplace(
    char* String,
    char* OldString,
    char* NewString
);

// ReadNotWrite - Set to TRUE to read, FALSE to write
BOOL SDUReadWriteFile(
    WCHAR* Filename,
    LARGE_INTEGER Offset,
    BOOL ReadNotWrite,
    int BufferSize,
    BYTE* Buffer
);


// Replace all instances of OldString in Text with NewString
// Length - Set to the length of the Text buffer
void SDUStringReplaceN(char* Text, size_t Length, char* OldString, char* NewString);
void SDUStringReplaceNW(WCHAR* Text, size_t Length, WCHAR* OldString, WCHAR* NewString);

// Replace OldString in Text with NewString.
// Note: This function modifies Text
void SDUStringReplace(char* Text, char* OldString, char* NewString);
void SDUStringReplaceW(WCHAR* Text, WCHAR* OldString, WCHAR* NewString);

// Substitute %1, %2, %3, etc parameters in a string for values
// Note: "%n" can be repeated as many times as needed, and don't need to be in
//       the same order that they appear in Args (e.g. "%2 %1 %3 %1" is valid)
// Note: This function is "safe" in that if more arguments are passed in, it
//       won't crash; similarly, if too few arguments are passed in, it'll just
//       leave the "%n" in the string as "%n"
//       HOWEVER - the list of substitution parameters *must* end in either "NULL" 
//       or "NULL, NULL"
// Important: The versions of these functions which *don't* use 
//            OutBuffer/OutBufferSize will use a GLOBAL buffer - as a result, 
//            this version is NOT THREADSAFE
// OutBuffer - This will be populated a copy of FormatStr, expanded out
// OutBufferSize - This should be set to the size of the OutBuffer buffer
// FormatStr - A string containing %1, %2, %3, which will be substituted for 
//             the parameters following it
//             The variable length parameters passed following "FormatStr" must
//             be pairs of values; the first of each pair being a sprintf
//             format string which determines how the value is to be
//             interpreted, and the second of each pair is the value to be
//             substituted into "FormatStr". The pairs of values *must* be
//             terminated with NULL (or NULL, NULL if wanted)
//             e.g.:
//               SDUParamSubstitute(
//                   "This is a string: %1, a character: %2, and a number: %3\n",
//                   "%s", "A STRING HERE",
//                   "%c", 'Z',
//                   "%i", 42,
//                   NULL
//                  );
// Returns: OutBuffer
char* SDUParamSubstitute(const char* FormatStr, ...);
char* SDUParamSubstituteBuf(char* OutBuffer, const int OutBufferSize, const char* FormatStr, ...);
char* SDUParamSubstituteBufList(char* OutBuffer, const int OutBufferSize, const char* FormatStr, va_list vl);
WCHAR* SDUParamSubstituteW(const WCHAR* FormatStr, ...);
WCHAR* SDUParamSubstituteBufW(WCHAR* OutBuffer, const int OutBufferSize, const WCHAR* FormatStr, ...);
WCHAR* SDUParamSubstituteBufListW(WCHAR* OutBuffer, const int OutBufferSize, const WCHAR* FormatStr, va_list vl);


BOOL
SDUCheckFileExists(
    WCHAR* Filename
);

// =========================================================================
// =========================================================================

#endif

