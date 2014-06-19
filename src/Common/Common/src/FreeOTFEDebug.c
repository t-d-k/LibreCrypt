// Description: FreeOTFE Debug Library
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//

#include "FreeOTFEDebug.h"

#include "FreeOTFEPlatform.h"

#if DBG

// Note: These should be set after reading the registry
// Note: These should be "bitmap values" indicating warn/error/info/enter/exit, etc
FreeOTFEDebugLevel = 0;
FreeOTFEDebugIndent = 0;
FreeOTFEDebugLoopTmp = 0;


#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
#include <stdarg.h>   // xxx - junk if not needed Needed for va_list, etc
#include <windows.h>

#define MAX_DEBUG_LINE_LENGTH 512
void SendComm(WCHAR* AZ, ...)
{
    DWORD dwWritten;
    HANDLE hComm;
    char BUF_1[MAX_DEBUG_LINE_LENGTH];
    WCHAR WBUF[MAX_DEBUG_LINE_LENGTH];
//    char BUF_2[MAX_DEBUG_LINE_LENGTH * 2];
    va_list ap;

    memset(BUF_1, 0, sizeof(BUF_1));
    va_start(ap, AZ);
    vswprintf(WBUF, AZ, ap);
    va_end(ap);
    wcstombs(BUF_1, WBUF, wcslen(WBUF));

//    memset(BUF_2, 0, sizeof(BUF_2));
//    sprintf(BUF_2, "%0.8x: ", level);
//    strcat(BUF_2, BUF_1);

    hComm = CreateFile(
                       DEBUG_FILE,
                       (GENERIC_READ | GENERIC_WRITE),  
                       0,  
                       0,  
                       OPEN_ALWAYS,
                       FILE_FLAG_WRITE_THROUGH, 
                       0
                      ); 

    if (hComm != INVALID_HANDLE_VALUE) 
        { 
        SetFilePointer(hComm, 0, NULL, FILE_END);
        WriteFile(hComm, BUF_1, strlen(BUF_1), &dwWritten, NULL); 
        //WriteFile(hComm, BUF_2, strlen(BUF_2), &dwWritten, NULL); 
        CloseHandle(hComm); 
        }

}

#endif
#endif

