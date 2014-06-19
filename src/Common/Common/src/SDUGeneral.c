// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


#include "SDUGeneral.h"
#include <Wingdi.h>
#include <commctrl.h>  // Only used for LVS_TYPEMASK
#include <stdio.h>
#include <wchar.h>
#include <tchar.h>  // Required for _T macro
#include <ctype.h>  // Required for toupper(...)
#ifndef WINCE
#include "FreeOTFEPlatform.h"  // Required to define FOTFE_PC_DRIVER if needed
#ifndef FOTFE_PC_DRIVER
// This should only be included for PC builds - but *not* for kernel drivers
#pragma comment (lib, "version.lib")  // Requird for GetFileVersionInfo and GetFileVersionInfoSize
#endif
#endif

// Does swprintf(...) take 3 or 4 parameters - i.e. the second parameter is
// the size of the first parameter (Buffer)
#ifdef WINCE
#define SWPRINTF_TAKES_SIZE 0
#else
#define SWPRINTF_TAKES_SIZE 1
#endif

// Not clear where this is defined?!
#ifdef WINCE        
#define INVALID_FILE_ATTRIBUTES 0xFFFFFFFF
#endif


const int MAX_SUBS = 10;

// Note: These are *NOT* translatable - just simple empty strings
const char SDU_EMPTY_STRING[] = "";
const WCHAR SDU_EMPTY_STRINGW[] = TEXT("");


// =========================================================================
void SDUParsePath(
    const WCHAR* fullPath, 
    WCHAR** uncMachine,
    WCHAR** drive,
    WCHAR** path,
    WCHAR** filename,
    WCHAR** fileExtension
)
{
    WCHAR* pos;
    int i;
    int tmpLen;

    *uncMachine    = NULL;
    *drive         = NULL;
    *path          = NULL;
    *filename      = NULL;
    *fileExtension = NULL;


    // Identify UNC path/drive letter and path
    if (wcslen(fullPath) >= 3)
        {
        // Check if starts with "\\"; UNC path
        // e.g. \\<machine>\path\...
        if (wcsncmp(fullPath, TEXT("\\\\"), 2) == 0)
            {
            *uncMachine = (WCHAR*)&(fullPath[2]);

            // Seek following path...
            pos = &((*uncMachine)[1]); // Skip "\\"
            while (wcsncmp(pos, TEXT("\0"), 1) != 0)
                {
                if (
                    (wcsncmp(pos, TEXT("\\"), 1) == 0) ||
                    (wcsncmp(pos, TEXT("/"), 1) == 0)
                   )
                    {
                    *path = pos;
                    break;
                    }

                pos++;
                }
            }
        // Check if second char is ":"; DOS drive letter
        // e.g. C:\path\...
        else if (wcsncmp(&(fullPath[1]), TEXT(":"), 1) == 0)
            {
            *drive = (WCHAR*)fullPath;
            *path = (WCHAR*)&(fullPath[2]); // Move past drive letter and ":"
            }
        else 
        // Assume that the input is *just* a path
            {
            *path = (WCHAR*)fullPath;
            }
        }
    else if (wcslen(fullPath) >= 1)
    // Assume that the input is *just* a path
        {
        *path = (WCHAR*)fullPath;
        }


    // Process filename...
    if (*path != NULL)
        {
        // Seek backwards to pick up the filename extension and filename
        tmpLen = wcslen(*path);
        for (i = (tmpLen - 1); i >= 0; i--)
            {
            if (
                (wcsncmp(&((*path)[i]), TEXT("\\"), 1) == 0) ||
                (wcsncmp(&((*path)[i]), TEXT("/"), 1) == 0)
                )
                {
                // If the character we found wasn't the last character, we
                // do have a filename we can use
                if (i != (tmpLen - 1))
                    {
                    *filename = &((*path)[i+1]);
                    }

                break;
                }
            }
        }


    // Process filename extension...
    if (*filename != NULL)
        {
        // Seek backwards to pick up the filename extension and filename
        tmpLen = wcslen(*filename);
        for (i = (tmpLen - 1); i >= 0; i--)
            {
            if (wcsncmp(&((*filename)[i]), TEXT("."), 1) == 0)                 
                {
                // If the character we found wasn't the last character, we
                // do have a filename we can use
                if (i != (tmpLen - 1))
                    {
                    *fileExtension = &((*filename)[i+1]);
                    }

                break;
                }
            }
        }

}


// =========================================================================
// Copy the executable's *path* from the specified to the buffer supplied
// Note: The trailing "\" is *not* included
void SDUCpyExeDir(
    WCHAR* buffer
)
{
    WCHAR exeFilename[4096];   // Some arbitary max length for buffer
    WCHAR* uncMachine;
    WCHAR* drive;
    WCHAR* path;
    WCHAR* tmpFilename;
    WCHAR* fileExtension;

    // Check for a local copy of the documentation under the 
    // executable's directory
    GetModuleFileName(
                      NULL, 
                      exeFilename, 
                      (sizeof(exeFilename) / sizeof(exeFilename[0]))
                     ); 
    SDUParsePath(
                 exeFilename, 
                 &uncMachine, 
                 &drive, 
                 &path, 
                 &tmpFilename, 
                 &fileExtension
                );
    // Terminate "path"
    // -1 so we don't include trailing slash
    if (tmpFilename != NULL)
        {
        *(tmpFilename-1) = (WCHAR)NULL;
        }
    // Note: No "\" after the first %s - the above only terminated on the filename,
    //       not the trailing slash after the path
    wsprintf(
             buffer, 
             TEXT("%s"), 
             exeFilename  // i.e. From the start of the exe path; it could be a
                          //      UNC path, drive letter + path, or just path
            );

}


// =========================================================================
void* SDUUnpack_LARGE_INTEGER(const void* ptr, LARGE_INTEGER* val)
{
    void *tmpPtr;
    int i;
    unsigned char x;

    tmpPtr = (void*)ptr;
    val->QuadPart = 0;
    for (i = 0; i < 8; i++)
        {
        tmpPtr = SDUUnpack_char(tmpPtr, (char*)&x);
        val->QuadPart = (val->QuadPart << 8) + x;
        }

    return tmpPtr;
}


// =========================================================================
void* SDUUnpack_DWORD(const void* ptr, DWORD* val)
{
    void *tmpPtr;
    int i;
    unsigned char x;

    tmpPtr = (void*)ptr;
    *val = 0;
    for (i = 0; i < 4; i++)
        {
        tmpPtr = SDUUnpack_char(tmpPtr, (char*)&x);
        *val = (*val << 8) + x;
        }

    return tmpPtr;
}


// =========================================================================
void* SDUUnpack_char(const void* ptr, char* val)
{
    *val = (*((char*)ptr));
    return ((char*)ptr + 1);
}


// =========================================================================
void* SDUPack_LARGE_INTEGER(const void* ptr, LARGE_INTEGER val)
{
    void *tmpPtr;

    tmpPtr = (void *)ptr;

    tmpPtr = SDUPack_DWORD(tmpPtr, val.HighPart);
    tmpPtr = SDUPack_DWORD(tmpPtr, val.LowPart);

    return tmpPtr;
}


// =========================================================================
void* SDUPack_DWORD(const void* ptr, DWORD val)
{
    void *tmpPtr;
    unsigned char x;

    tmpPtr = (void *)ptr;

    x = (unsigned char)((val >> 24) & 0xFF);
    tmpPtr = SDUPack_char(tmpPtr, (char)x);
    x = (unsigned char)((val >> 16) & 0xFF);
    tmpPtr = SDUPack_char(tmpPtr, (char)x);
    x = (unsigned char)((val >> 8) & 0xFF);
    tmpPtr = SDUPack_char(tmpPtr, (char)x);
    x = (unsigned char)(val & 0xFF);
    tmpPtr = SDUPack_char(tmpPtr, (char)x);

    return tmpPtr;
}


// =========================================================================
void* SDUPack_char(const void* ptr, char val)
{
    *((char*)ptr) = val;
    return ((char*)ptr + 1);
}


// =========================================================================
// Set filename to NULL to get version info on the current process
// Note: Caller must free(lpBuffer) after use
BOOL SDUGetVersionInfoShortFmtAlloc(
					   LPWSTR filename,
					   LPWSTR* lpBuffer
					  )
{
    BOOL retval = FALSE;
    const int VERSION_STR_SIZE = 256;

    *lpBuffer = (LPWSTR)malloc(VERSION_STR_SIZE * sizeof(*lpBuffer));
    if (SDUGetVersionInfoShortFmt(
                             filename,
                             *lpBuffer,
                             VERSION_STR_SIZE
                             ))
        {
        retval = TRUE;
        }
    else
        {
        free(*lpBuffer);
        *lpBuffer = NULL;
        }

    return retval;
}


// =========================================================================
// Set filename to NULL to get version info on the current process
// Note: nSize must be the max number of *characters*
BOOL SDUGetVersionInfoShortFmt(
					   LPWSTR filename,
					   LPWSTR lpBuffer,
					   DWORD nSize
					  )
{
    BOOL retval = FALSE;
    DWORD Version;

    if (SDUGetVersionInfoShort(filename, &Version))
        {
        if (_snwprintf(
                       lpBuffer, 
                       nSize,
                       TEXT("v%d.%0.2d.%0.4d"), 
                       (Version & 0xFF000000) / 0x00FF0000,
                       (Version & 0x00FF0000) / 0x0000FF00,
                       (Version & 0x0000FFFF)
                      ) >= 0)
/*
        if (_snwprintf_(
                       lpBuffer, 
                       nSize,
                       nSize,
                       TEXT("v%d.%0.2d.%0.4d"), 
                       (Version & 0xFF000000) / 0x00FF0000,
                       (Version & 0x00FF0000) / 0x0000FF00,
                       (Version & 0x0000FFFF)
                      ) >= 0)
*/
            {
            retval = TRUE;
            }
        }

    return retval;   
}


// =========================================================================
// Set filename to NULL to get version info on the current process
// Note: Caller must free(lpBuffer)  after use
BOOL SDUGetVersionInfoFmtAlloc(
					   LPWSTR filename,
					   LPWSTR* lpBuffer
					  )
{
    BOOL retval = FALSE;
    const int VERSION_STR_SIZE = 256;

    *lpBuffer = (LPWSTR)malloc(VERSION_STR_SIZE);
    if (SDUGetVersionInfoFmt(
                             filename,
                             *lpBuffer,
                             VERSION_STR_SIZE
                             ))
        {
        retval = TRUE;
        }
    else
        {
        free(*lpBuffer);
        *lpBuffer = NULL;
        }

    return retval;
}


// =========================================================================
// Set filename to NULL to get version info on the current process
BOOL SDUGetVersionInfoFmt(
					   LPWSTR filename,
					   LPWSTR lpBuffer,
					   DWORD nSize
					  )
{
    BOOL retval = FALSE;
    int majorVersion;
    int minorVersion;
    int revisionVersion;
    int buildVersion;

    if (SDUGetVersionInfo(
                          filename,
                          &majorVersion,
                          &minorVersion, 
                          &revisionVersion, 
                          &buildVersion
                         ))
        {
        if (_snwprintf(
                       lpBuffer, 
                       nSize,
                       TEXT("v%d.%0.2d.%0.2d.%0.4d"), 
                       majorVersion, 
                       minorVersion, 
                       revisionVersion, 
                       buildVersion
                      ) >= 0)
/*
        if (_snwprintf_s(
                       lpBuffer, 
                       nSize,
                       nSize,
                       TEXT("v%d.%0.2d.%0.2d.%0.4d"), 
                       majorVersion, 
                       minorVersion, 
                       revisionVersion, 
                       buildVersion
                      ) >= 0)
*/
            {
            retval = TRUE;
            }
        }

    return retval;   
}


// =========================================================================
// Set filename to NULL to get version info on the current process
// This sets Version to the version ID in the form: 0xAABBCCCC
// Where: 
//   AA - Major version
//   BB - Minor version
//   CC - Revision version
BOOL SDUGetVersionInfoShort(
					   LPWSTR Filename,
					   DWORD* Version
					  )
{
    BOOL retval;
    int majorVersion;
    int minorVersion;
    int revisionVersion;
    int buildVersion;

    retval = SDUGetVersionInfo(
                               Filename,
                               &majorVersion,
                               &minorVersion,
                               &revisionVersion,
                               &buildVersion
                              );
    if (retval)
        {
        *Version = (
                    (majorVersion << 24) +
                    (minorVersion << 16) +
                    revisionVersion
                   );
        }

    return retval;
}


// =========================================================================
// Set filename to NULL to get version info on the current process
BOOL SDUGetVersionInfo(
					   LPWSTR filename,
					   int* majorVersion,
					   int* minorVersion, 
					   int* revisionVersion, 
					   int* buildVersion
					  )
{
    BOOL retval = FALSE;
    int vsize;
    UINT puLen;
    DWORD dwHandle;
    void* pBlock;
    void* pVPointer = NULL;
    VS_FIXEDFILEINFO* tvs;
    // Twice MAX_PATH should be sufficient for most purposes...
    WCHAR bufFilename[sizeof(WCHAR) * (2 * MAX_PATH)];

    // Autodetermine filename if not passed in
    if (filename == NULL)
        {
        if (GetModuleFileName( 
                              NULL,
                              bufFilename, 
                              sizeof(bufFilename)
                             ) != 0)
            {
            filename = bufFilename;
            }
        }

    if (filename != NULL)
        {
        vsize = GetFileVersionInfoSize(filename, &dwHandle);
        if (vsize > 0) 
            {
            pBlock = malloc(vsize);

            if (GetFileVersionInfo(filename, dwHandle, vsize, pBlock))
                {
                if (VerQueryValue(pBlock, _T("\\"), &pVPointer, &puLen))
                    {
                    if (puLen > 0)
                        {
                        tvs = (VS_FIXEDFILEINFO*)pVPointer;
                        *majorVersion    = tvs->dwFileVersionMS >> 16;
                        *minorVersion    = tvs->dwFileVersionMS & 0xFFFF;
                        *revisionVersion = tvs->dwFileVersionLS >> 16;
                        *buildVersion    = tvs->dwFileVersionLS & 0xFFFF;
                        retval = TRUE;
                        }
                    }
                }

            free(pBlock);
            }
        }

    return retval;
}


// =========================================================================
void SDUTextSetBold(
    HWND hWnd,
    int ctrlID,
    BOOL UseBold
)
{
    HFONT hFont;
    LOGFONT lFont;

    // Set welcome title to bold
    hFont = (HFONT)SendMessage(
                GetDlgItem(hWnd, ctrlID),
                WM_GETFONT,
                0,
                0
               );
    if (hFont != NULL)
        {
        if (GetObject(hFont, sizeof(lFont), &lFont) != 0)
            {
            lFont.lfWeight = FW_BOLD;
            hFont = CreateFontIndirect(&lFont);
            SendMessage(
                        GetDlgItem(hWnd, ctrlID),
                        WM_SETFONT,
                        (WPARAM)hFont,
                        TRUE
                       );
            }
        }
}


// =========================================================================
void SDUTextSetItalic(
    HWND hWnd,
    int ctrlID,
    BOOL UseItalics
)
{
    HFONT hFont;
    LOGFONT lFont;

    // Set welcome title to bold
    hFont = (HFONT)SendMessage(
                GetDlgItem(hWnd, ctrlID),
                WM_GETFONT,
                0,
                0
               );
    if (hFont != NULL)
        {
        if (GetObject(hFont, sizeof(lFont), &lFont) != 0)
            {
            lFont.lfItalic = UseItalics;
            hFont = CreateFontIndirect(&lFont);
            SendMessage(
                        GetDlgItem(hWnd, ctrlID),
                        WM_SETFONT,
                        (WPARAM)hFont,
                        TRUE
                       );
            }
        }
}


// =========================================================================
void SDUTextSetUnderline(
    HWND hWnd,
    int ctrlID,
    BOOL UseUnderline
)
{
    HFONT hFont;
    LOGFONT lFont;

    // Set welcome title to bold
    hFont = (HFONT)SendMessage(
                GetDlgItem(hWnd, ctrlID),
                WM_GETFONT,
                0,
                0
               );
    if (hFont != NULL)
        {
        if (GetObject(hFont, sizeof(lFont), &lFont) != 0)
            {
            lFont.lfUnderline = UseUnderline;
            hFont = CreateFontIndirect(&lFont);
            SendMessage(
                        GetDlgItem(hWnd, ctrlID),
                        WM_SETFONT,
                        (WPARAM)hFont,
                        TRUE
                       );
            }
        }
}


// =========================================================================
// Draw text as per DrawText(...), but truncate the string displayed and
// append "..." onto it if it will not fit into the boundrys supplied
// This is implemented for systems where DT_WORD_ELLIPSIS and DT_PATH_ELLIPSIS
// are not supported (e.g. WinCE)
// Returns: The height of the text displayed
#define SDU_ELLIPSES  TEXT("...")
int SDUDrawTextEllipses(
    HDC hdc,
    WCHAR* Text,
    int TextLength,
    LPRECT TextRect,
    UINT uFormat
)
{
    int retval;
    SIZE textSize;
    int bufferSize;
    WCHAR* buffer;
    int useTextLength;

    retval = 0;

    // +1 for NULL terminator
    bufferSize = (wcslen(Text) + wcslen(SDU_ELLIPSES) + 1) * sizeof(*buffer);
    buffer = (WCHAR*)malloc(bufferSize);
    if (buffer != NULL)
        {
        useTextLength = TextLength;
        memset(buffer, 0, bufferSize);
        wcsncpy(
                  buffer, 
                  Text,
                  useTextLength
                 );
/*
        wcsncpy_s(
                  buffer, 
                  (bufferSize / sizeof(*buffer)),
                  Text,
                  useTextLength
                 );
*/

        GetTextExtentPoint32(
                             hdc, 
                             buffer,
                             wcslen(buffer),
                             &textSize
                            );
        if (textSize.cx > (TextRect->right - TextRect->left))
            {
            wcscat(buffer, SDU_ELLIPSES);
//            wcscat_s(buffer, (bufferSize / sizeof(WORD)), SDU_ELLIPSES);
            GetTextExtentPoint32(
                                 hdc, 
                                 buffer,
                                 wcslen(buffer),
                                 &textSize
                                );
            }

        while (
               (textSize.cx > (TextRect->right - TextRect->left)) &&
               (useTextLength > 0)
              )
            {
            useTextLength--;
            memset(buffer, 0, bufferSize);
            wcsncpy(
                      buffer, 
                      Text, 
                      useTextLength
                     );
            wcscat(buffer, SDU_ELLIPSES);
/*
            wcsncpy_s(
                      buffer, 
                      (bufferSize / sizeof(WORD)), 
                      Text, 
                      useTextLength
                     );
            wcscat_s(buffer, (bufferSize / sizeof(WORD)), SDU_ELLIPSES);
*/
            GetTextExtentPoint32(
                                 hdc, 
                                 buffer,
                                 wcslen(buffer),
                                 &textSize
                                );
            }

        retval = DrawText(
                          hdc,
                          buffer,
                          wcslen(buffer),
                          TextRect,
                          uFormat
                         );

        free(buffer);
        }

    return retval;
}


// =========================================================================
// Set listview style; one of:
//   LVS_ICON      - Large icons with text underneath them. Bit like Windows
//                   desktop icons in layout
//   LVS_REPORT    - Grid with header. Subitems displayed as separate columns.
//                   This is the only one which can do full row select
//   LVS_SMALLICON - Just a list, no header. Only first item (no subitems)
//                   displayed
//   LVS_LIST      - Just a list, no header. Only first item (no subitems)
//                   displayed
// To get the listview to display a report view with large icons, supply
// normal sized (i.e. 32x32) icons as the listview's "small icons" imagelist
void SDUSetListViewType(HWND hWnd, DWORD newStyle)
{
    DWORD style;

    style = GetWindowLong(hWnd, GWL_STYLE);

    if((style & LVS_TYPEMASK) != newStyle)
        {
        SetWindowLong(
                      hWnd, 
                      GWL_STYLE, 
                      ((style & ~LVS_TYPEMASK) | newStyle)
                     );
        } 

}


// =========================================================================
// SetNotUnset - Set to TRUE to set the style specified, FALSE to remove it
void SDUSetWndStyle(HWND hWnd, BOOL SetNotUnset, DWORD Style)
{
    DWORD wndStyle;

    wndStyle = GetWindowLong(hWnd, GWL_STYLE);

    if (SetNotUnset)
        {
        wndStyle |= Style;
        }
    else
        {
        wndStyle &= ~Style;
        }

    SetWindowLong(
                  hWnd, 
                  GWL_STYLE, 
                  wndStyle
                 );
}


// =========================================================================
// The one MS missed out... Get the length of a listview items's text in
// characters
int SDUListView_GetItemTextLength(HWND hWnd, int item, int subItem)
{
    LVITEM listItem;
    WCHAR* buffer;
    int testCharCount;
    int v;

    memset(&listItem, 0, sizeof(listItem));
    listItem.iItem = item;
    listItem.iSubItem = subItem;
    listItem.mask = LVIF_TEXT;

    testCharCount = 5;
    buffer = (WCHAR*)malloc((testCharCount + 1) * sizeof(*buffer));
    listItem.pszText = buffer;
    listItem.cchTextMax = testCharCount;
    v = SendMessage(hWnd, LVM_GETITEMTEXT, item, (LPARAM)&listItem);

    testCharCount = 1;
    while (v >= (testCharCount - 1))
        {
        testCharCount += 5;
        buffer = (WCHAR*)realloc(buffer, (testCharCount + 1) * sizeof(*buffer));
        listItem.pszText = buffer;
        listItem.cchTextMax = testCharCount;
        v = SendMessage(hWnd, LVM_GETITEMTEXT, item, (LPARAM)&listItem);
        }

    if (buffer != NULL)
        {
        free(buffer);
        }

    return v; // listItem.cchTextMax;
}


// =========================================================================
// Tokenize a command line and it's parameters
// Analagous to strtok(...) in it's operation
WCHAR* _SDUParamTok(WCHAR* cmdLine)
{
    static WCHAR* ptr;
    WCHAR* start;
    WCHAR* end;
    WCHAR* tmp;
    WCHAR endOfParamChar;

    if (cmdLine != NULL)
        {
        ptr = cmdLine;
        }

    if (ptr == NULL)
        {
        return NULL;
        }

    start = NULL;
    tmp = ptr;
    while (*tmp != 0)
        {
        if (*tmp != ' ')
            {
            start = tmp;
            break;
            }

        tmp++;
        }

    if (start == NULL)
        {
        ptr = NULL;
        return NULL;
        }

    endOfParamChar = ' ';
    if (*start == '"')
        {
        start = &(start[1]);
        endOfParamChar = '"';    
        }

    end = start;
    tmp = start;
    while (
           (*end != 0) &&
           (*end != endOfParamChar)
           )
        {
        end++;
        }

    if (*end == 0)
        {
        ptr = NULL;
        }
    else
        {
        ptr = &(end[1]);
        }

    if (
        (start == end) &&
        (endOfParamChar != '"')
       )
        {
        ptr = NULL;
        start = NULL;
        }

    *end = 0;

    return start;
}


// =========================================================================
// Return the value of the command line parameter indicated by the specified
// index (zero based index)
// IMPORTANT: It is the *callers* responsibility to free off the value returned
// Returns NULL on failure
WCHAR* SDUCommandLineParameter_Idx(const int parameterIdx)
{
    WCHAR* origCmdLine;
    WCHAR* useCmdLine;
    WCHAR* currParam;
    int currParamIdx;
    WCHAR* retval;

    retval = NULL;

    origCmdLine = GetCommandLine();
    // +1 to include NULL terminator
    useCmdLine = calloc((wcslen(origCmdLine) + 1), sizeof(*useCmdLine));
    if (useCmdLine != NULL)
        {
        wcscpy(useCmdLine, origCmdLine);

        currParamIdx = 0;
        currParam = _SDUParamTok(useCmdLine);
        while (
               (currParamIdx < parameterIdx) &&
               (currParam != NULL)
              )
            {
            currParamIdx++;
            currParam = _SDUParamTok(NULL);
            }

        if (currParam != NULL)
            {
            retval = calloc((wcslen(currParam) + 1), sizeof(*retval));
            if (retval != NULL)
                {
                wcscpy(retval, currParam);
                }
            }

        free(useCmdLine);
        }

    return retval;
}

// =========================================================================
// Returns TRUE if the specified command line switch could be found, otherwise
// FALSE
BOOL SDUCommandLineSwitch(const WCHAR* parameter)
{
    BOOL retval;

    retval = FALSE;

    if (SDUCommandLineSwitchNumber(parameter) >= 0)
        {
        retval = TRUE;
        }

    return retval;
}



// =========================================================================
// Return the value of the specified command line parameter "-<parameter>
// value"
// IMPORTANT: It is the *callers* responsibility to free off the value returned
// Returns NULL on failure
WCHAR* SDUCommandLineParameter_Name(const WCHAR* parameter)
{
    WCHAR* retval;
    int switchParamIdx;

    retval = NULL;

    switchParamIdx = SDUCommandLineSwitchNumber(parameter);
    if (switchParamIdx >= 0)
        {
        retval = SDUCommandLineParameter_Idx(switchParamIdx+1);
        }

    return retval;
}


// =========================================================================
// Returns the parameter number in the command line of the specified parameter.
// Returns -1 on failure
int SDUCommandLineSwitchNumber(const WCHAR* parameter)
{
    int retval;
    WCHAR* currParam;
    int currParamIdx;

    retval = -1;

    currParamIdx = 0;
    currParam = SDUCommandLineParameter_Idx(currParamIdx);
    while (currParam != NULL)
        {
        // >2 to allow 
        if (wcslen(currParam) > 1)
            {
            if (
                (currParam[0] == '/') ||
                (currParam[0] == '-')
               )
                {
                if (wcscmp(parameter, &(currParam[1])) == 0)
                    {
                    retval = currParamIdx;
                    break;
                    }
                }
            }

        free(currParam);                    
        currParamIdx++;
        currParam = SDUCommandLineParameter_Idx(currParamIdx);
        }

    if (currParam != NULL)
        {
        free(currParam);
        }

    return retval;
}


// =========================================================================
// Identify which version of Windows Mobile is running
SDU_OS_VER_PDA SDUGetOSVersion_PDA()
{
    SDU_OS_VER_PDA retval;
    OSVERSIONINFO osVer;

    retval = SDU_OS_VER_PDA_UNKNOWN;

    //  VER_PLATFORM_WIN32_CE
    if (GetVersionEx(&osVer))
        {
        if (
            (osVer.dwMajorVersion >= 5) &&
            (osVer.dwMinorVersion >= 2)
           )
            {
            // aka WM6
            retval = SDU_OS_VER_PDA_CROSSBOW;
            }
        else if (osVer.dwMajorVersion >= 5)
            {
            // aka WM5
            retval = SDU_OS_VER_PDA_WM2005;
            }
        else if (
            (osVer.dwMajorVersion >= 4) &&
            (osVer.dwMinorVersion >= 2)
           )
            {
            retval = SDU_OS_VER_PDA_WM2003SE;
            }
        else if (osVer.dwMajorVersion >= 4)
            {
            retval = SDU_OS_VER_PDA_WM2003;
            }
        else if (osVer.dwMajorVersion >= 3)
            {
            retval = SDU_OS_VER_PDA_WM2002;  // SDU_OS_VER_PDA_CE30;
            }
        else if (
                 (osVer.dwMajorVersion >= 2) &&
                 (osVer.dwMinorVersion >= 1)
                )
            {
            retval = SDU_OS_VER_PDA_CE21;
            }
        else if (osVer.dwMajorVersion >= 2)
            {
            retval = SDU_OS_VER_PDA_CE20;
            }
        else 
            {
            retval = SDU_OS_VER_PDA_CE10;
            }
        }

    return retval;
}


// =========================================================================
// Given a path and filename, this returns the last item in PathAndFilename
// i.e. The filename
// Note: If PathAndFilename is a directory, and ends in a "\", it'll 
//       return a pointer to an empty string
WCHAR* SDUFilenameFromPathAndFilename(WCHAR* PathAndFilename)
{
    WCHAR* retval;
    int i;

    retval = NULL;
    if (PathAndFilename != NULL)
        {
        retval = PathAndFilename;
        // -1 to ignore terminating NULL
        for(i = (((int)wcslen(PathAndFilename)) - 1); i >= 0; i--)
            {
            if (PathAndFilename[i] == PATH_SEPARATOR_CHAR)
                {
                retval = &(PathAndFilename[i+1]);
                break;
                }
            }
        }

    return retval;
}


// =========================================================================
char* SDUstrtoupper(char *s1, const char *s2)
{
    unsigned int i;

    for (i = 0; i < strlen(s2); i++)
        {
        s1[i] = toupper(s2[i]);
        }
    // Set terminating NULL
    s1[strlen(s2)] = 0;

    return s1;
}

WCHAR* SDUwcstoupper(WCHAR *s1, const WCHAR *s2)
{
    unsigned int i;

    for (i = 0; i < wcslen(s2); i++)
        {
        s1[i] = towupper(s2[i]);
        }
    // Set terminating NULL
    s1[wcslen(s2)] = 0;

    return s1;
}


// =========================================================================
char* SDUstrreplace(
    char* String,
    char* OldString,
    char* NewString
)
{
    int replaceCnt;
    char* pos;
    char* retval;
    char* lastPos;

    replaceCnt = 0;
    pos = String;
    while (pos != NULL)
        {
        pos = strstr(pos, OldString);
        if (pos != NULL)
            {
            pos = &(pos[1]);
            replaceCnt++;
            }

        }

    retval = calloc(
                    sizeof(*retval),
                    (
                     strlen(String) +
                     ((strlen(NewString) - strlen(OldString)) * replaceCnt) +
                     // +1 for terminating NULL
                     1
                    )
                   );

    if (retval != NULL)
        {
        replaceCnt = 0;
        pos = String;
        while (pos != NULL)
            {
            lastPos = pos;
            pos = strstr(lastPos, OldString);

            if (pos == NULL)
                {
                strcat(retval, lastPos);
                }
            else
                {
                strncat(retval, lastPos, (pos - lastPos));
                strcat(retval, NewString);
                pos = &(pos[strlen(OldString)]);
                }
            }
        }

    return retval;
}


// =========================================================================
// ReadNotWrite - Set to TRUE to read, FALSE to write
BOOL SDUReadWriteFile(
    WCHAR* Filename,
    LARGE_INTEGER Offset,
    BOOL ReadNotWrite,
    int BufferSize,
    BYTE* Buffer
)
{
    BOOL retval = FALSE;
    DWORD bytesTransferred;
    HANDLE fileHandle;
    BOOL filePosOK;
    DWORD newFilePos;
    DWORD dwDesiredAccess;


    if (ReadNotWrite) 
        {
        dwDesiredAccess = GENERIC_READ;
//        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Read data from file:\n")));
        }
    else
        {
        dwDesiredAccess = GENERIC_WRITE;
//        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("Write data to file:\n")));
        }
//    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("  Filename: %ls\n"), Filename));
//    DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("  Offset  : 0x%0.8x\n"), Offset.QuadPart));

    fileHandle = CreateFile(
                            Filename,
                            dwDesiredAccess,  
                            (FILE_SHARE_READ | FILE_SHARE_WRITE),  
                            NULL,
                            OPEN_EXISTING, 
                            FILE_ATTRIBUTE_NORMAL, 
                            NULL
                            ); 
    if (fileHandle == INVALID_HANDLE_VALUE) 
        { 
//        DEBUGOUTGUI(DEBUGLEV_ERROR, (TEXT("Unable to open file\n")));
        } 
    else
        {
        newFilePos = SetFilePointer( 
                                    fileHandle, 
                                    Offset.LowPart, 
                                    &(Offset.HighPart), 
                                    FILE_BEGIN
                                    );
        filePosOK = TRUE;
        if (newFilePos == FAILED_SETFILEPOINTER) 
            {
            filePosOK = (GetLastError() == NO_ERROR);
            }
        if (filePosOK) 
            {
            if (ReadNotWrite)
                {
                retval = ReadFile( 
                                    fileHandle,
                                    Buffer, 
                                    BufferSize, 
                                    &bytesTransferred, 
                                    NULL
                                    );
                }
            else
                {
                retval = WriteFile( 
                                    fileHandle,
                                    Buffer, 
                                    BufferSize, 
                                    &bytesTransferred, 
                                    NULL
                                    );
                }
            }

        CloseHandle(fileHandle);
        }

    if (retval)
        {
//        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("CDB write OK\n")));
        }
    else
        {
//        DEBUGOUTGUI(DEBUGLEV_INFO, (TEXT("CDB write FAILED.\n")));
        }

    return retval;
}


// Replace all instances of OldString in Text with NewString
// Length - Set to the length of the Text buffer
void SDUStringReplaceN(char* Text, size_t Length, char* OldString, char* NewString)
{
  char* currFound;
  BOOLEAN finished;  
  char* tmpHead;
  char* tmpInput;
  
  tmpInput = calloc(strlen(Text) + 1, sizeof(*tmpInput));
  strcpy(tmpInput, Text);

  strcpy(Text, "");

  tmpHead = tmpInput;

  finished = FALSE;
  while (!(finished))
    {
    currFound = strstr(tmpHead, OldString);
    if (currFound == NULL)
      {
      finished = TRUE;
      // -1 to include terminating NULL char
      strncat(Text, tmpHead, ((Length - strlen(Text)) - 1));
      }
    else
      {
      // -1 to include terminating NULL char
      strncat(Text, tmpHead, min(
                                ((currFound - tmpHead) / sizeof(char)),
                                ((Length - strlen(Text)) - 1)
                               ));
      // -1 to include terminating NULL char
      strncat(Text, NewString, min(
                                strlen(NewString),
                                ((Length - strlen(Text)) - 1)
                               ));
      tmpHead += (currFound - tmpHead);
      tmpHead += strlen(OldString);
      }
    }
    
}

// Replace OldString in Text with NewString.
// Note: This function modifies Text
void SDUStringReplace(char* Text, char* OldString, char* NewString)
{
  SDUStringReplaceN(Text, 99999, OldString, NewString);
}

// Length - Length of "Text" buffer in WCHAR
void SDUStringReplaceNW(WCHAR* Text, size_t Length, WCHAR* OldString, WCHAR* NewString)
{
  WCHAR* currFound;
  BOOLEAN finished;  
  WCHAR* tmpHead;
  WCHAR* tmpInput;
  
  tmpInput = calloc(wcslen(Text) + 1, sizeof(*tmpInput));
  wcscpy(tmpInput, Text);

  wcscpy(Text, TEXT(""));

  tmpHead = tmpInput;

  finished = FALSE;
  while (!(finished))
    {
    currFound = wcsstr(tmpHead, OldString);
    if (currFound == NULL)
      {
      finished = TRUE;
      // -1 to include terminating NULL WCHAR
      wcsncat(Text, tmpHead, ((Length - wcslen(Text)) - 1));
      }
    else
      {
      // -1 to include terminating NULL WCHAR
      wcsncat(Text, tmpHead, min(
                                (unsigned int)((currFound - tmpHead)),
                                ((Length - wcslen(Text)) - 1)
                               ));
      // -1 to include terminating NULL WCHAR
      wcsncat(Text, NewString, min(
                                wcslen(NewString),
                                ((Length - wcslen(Text)) - 1)
                               ));
      tmpHead += (currFound - tmpHead);
      tmpHead += wcslen(OldString);
      }
    }
    
}

// Replace OldString in Text with NewString.
// Note: This function modifies Text
void SDUStringReplaceW(WCHAR* Text, WCHAR* OldString, WCHAR* NewString)
{
  SDUStringReplaceNW(Text, 99999, OldString, NewString);
}



// Substitute %1, %2, %3, etc parameters in a string for values
// Note: "%n" can be repeated as many times as needed, and don't need to be in
//       the same order that they appear in Args (e.g. "%2 %1 %3 %1" is valid)
// Note: This function is "safe" in that if more arguments are passed in, it
//       won't crash; similarly, if too few arguments are passed in, it'll just
//       leave the "%n" in the string as "%n"
//       HOWEVER - the list of substitution parameters *must* end in NULL, NULL
// OutBuffer - This will be populated a copy of FormatStr, expanded out
// OutBufferSize - This should be set to the size of the OutBuffer buffer
// FormatStr - A string containing %1, %2, %3...
// ... - Additional parameters as format string/value pairs, terminated with NULL (or NULL, NULL)
//       e.g. "%s", "this is a string",
//            "%c", 'Z',
//            "%i", 42,
//            NULL
// Returns: OutBuffer
char* SDUParamSubstitute(const char* FormatStr, ...)
{
  va_list vl;
  char* retval;
  
  va_start(vl, FormatStr);
  retval = SDUParamSubstituteBufList(
                            SDUParamSubstitute_GlobalBuffer,
                            sizeof(SDUParamSubstitute_GlobalBuffer),
                            FormatStr,
                            vl
                           );
  va_end(vl);  
  
  return retval;
}

WCHAR* SDUParamSubstituteW(const WCHAR* FormatStr, ...)
{
  va_list vl;
  WCHAR* retval;
  
  va_start(vl, FormatStr);
  retval = SDUParamSubstituteBufListW(
                            SDUParamSubstitute_GlobalBufferW,
                            (sizeof(SDUParamSubstitute_GlobalBufferW) / sizeof(WCHAR)),
                            FormatStr,
                            vl
                           );
  va_end(vl);  
  
  return retval;
}


char* SDUParamSubstituteBuf(char* OutBuffer, const int OutBufferSize, const char* FormatStr, ...)
{
  va_list vl;
  char* retval;
  
  va_start(vl, FormatStr);
  retval = SDUParamSubstituteBufList(
                            OutBuffer,
                            OutBufferSize,
                            FormatStr,
                            vl
                           );
  va_end(vl);  
  
  return retval;
}

WCHAR* SDUParamSubstituteBufW(WCHAR* OutBuffer, const int OutBufferSize, const WCHAR* FormatStr, ...)
{
  va_list vl;
  WCHAR* retval;
  
  va_start(vl, FormatStr);
  retval = SDUParamSubstituteBufListW(
                            OutBuffer,
                            OutBufferSize,
                            FormatStr,
                            vl
                           );
  va_end(vl);  
  
  return retval;
}


char* SDUParamSubstituteBufList(char* OutBuffer, const int OutBufferSize, const char* FormatStr, va_list vl)
{
  int i;
  char* currArgFmt;
  int replaceIdx;
  char paramAsString[512];  // Must be big enough to hold string representation of parameter
  char paramType;
  char argValChar;
  char* argValString;
  int argValInt;
  float argValFloat;
  char paramIdxAsString[1024];
  char replaceIdxStr[1024];
  
  replaceIdx = 1;
  
  strcpy(OutBuffer, FormatStr);
  for (i = 1; i <= MAX_SUBS; i++)
    {
    currArgFmt = va_arg(vl, char*);
    if (currArgFmt == NULL) 
      {
      break;
      }
      
    paramType = currArgFmt[strlen(currArgFmt)-1];
    strcpy(paramAsString, "");
    switch (paramType)
      {
      case 'c': 
        {
        argValChar = va_arg(vl, char);
        sprintf(paramAsString, currArgFmt, argValChar);
        break;
        }

      case 's':
        {
        argValString = va_arg(vl, char*);
        sprintf(paramAsString, currArgFmt, argValString);
        break;
        }
        
      case 'i': 
        {
        argValInt = va_arg(vl, int);
        sprintf(paramAsString, currArgFmt, argValInt);
        break;
        }

      case 'f': 
        {
        argValFloat = va_arg(vl, float);
        sprintf(paramAsString, currArgFmt, argValFloat);
        break;
        }
      }
    
    memset(paramIdxAsString, 0, sizeof(paramIdxAsString));
    _itoa(replaceIdx, paramIdxAsString, 10);
    strcpy(replaceIdxStr, "%");
    strcat(replaceIdxStr, paramIdxAsString);
    SDUStringReplace(OutBuffer, replaceIdxStr, paramAsString);
    
    replaceIdx++;
    }
  
  return OutBuffer;
}

WCHAR* SDUParamSubstituteBufListW(WCHAR* OutBuffer, const int OutBufferSize, const WCHAR* FormatStr, va_list vl)
{
  int i;
  WCHAR* currArgFmt;
  int replaceIdx;
  WCHAR paramAsString[512];  // Must be big enough to hold string representation of parameter
  WCHAR paramType;
  WCHAR argValChar;
  WCHAR* argValString;
  int argValInt;
  float argValFloat;
  WCHAR paramIdxAsString[1024];
  WCHAR replaceIdxStr[1024];
  
  replaceIdx = 1;
  
  wcscpy(OutBuffer, FormatStr);
  for (i = 1; i <= MAX_SUBS; i++)
    {
    currArgFmt = va_arg(vl, WCHAR*);
    if (currArgFmt == NULL) 
      {
      break;
      }
      
    paramType = currArgFmt[wcslen(currArgFmt)-1];
    wcscpy(paramAsString, TEXT(""));
    switch (paramType)
      {
      case 'c': 
        {
        argValChar = va_arg(vl, WCHAR);
#if (SWPRINTF_TAKES_SIZE == 1)
        swprintf(
                 paramAsString, 
                 (sizeof(paramAsString) / sizeof(WCHAR)), 
                 currArgFmt, 
                 argValChar
                );
#else
        swprintf(
                 paramAsString, 
                 currArgFmt, 
                 argValChar
                );
#endif
        break;
        }

      case 's':
        {
        argValString = va_arg(vl, WCHAR*);
#if (SWPRINTF_TAKES_SIZE == 1)
        swprintf(
                 paramAsString, 
                 (sizeof(paramAsString) / sizeof(WCHAR)), 
                 currArgFmt, 
                 argValString
                );
#else
        swprintf(
                 paramAsString, 
                 currArgFmt, 
                 argValString
                );
#endif
        break;
        }
        
      case 'i': 
      case 'd': // Delphi style
        {
        argValInt = va_arg(vl, int);
#if (SWPRINTF_TAKES_SIZE == 1)
        swprintf(
                 paramAsString, 
                 (sizeof(paramAsString) / sizeof(WCHAR)), 
                 currArgFmt, 
                 argValInt
                );
#else
        swprintf(
                 paramAsString, 
                 currArgFmt, 
                 argValInt
                );
#endif
        break;
        }

      case 'f': 
        {
        argValFloat = va_arg(vl, float);
#if (SWPRINTF_TAKES_SIZE == 1)
        swprintf(
                 paramAsString, 
                 (sizeof(paramAsString) / sizeof(WCHAR)), 
                 currArgFmt, 
                 argValFloat
                );
#else
        swprintf(
                 paramAsString, 
                 currArgFmt, 
                 argValFloat
                );
#endif
        break;
        }
      }
    
    memset(paramIdxAsString, 0, sizeof(paramIdxAsString));
    _itow(replaceIdx, paramIdxAsString, 10);
    wcscpy(replaceIdxStr, TEXT("%"));
    wcscat(replaceIdxStr, paramIdxAsString);
    SDUStringReplaceW(OutBuffer, replaceIdxStr, paramAsString);
    
    replaceIdx++;
    }
  
  return OutBuffer;
}

// =========================================================================
// Return TRUE if the file exists, otherwise FALSE
BOOL
SDUCheckFileExists(
    WCHAR* Filename
)
{
    BOOL retval = FALSE;

    if (Filename != NULL)
        {
        if (wcslen(Filename) > 0)
            {
            retval = (GetFileAttributes(Filename) != INVALID_FILE_ATTRIBUTES);
            }
        }

    return retval;
}


// =========================================================================
// =========================================================================
