// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


#ifndef _FreeOTFEDebug_H
#define _FreeOTFEDebug_H   1

#include "FreeOTFEPlatform.h"

//#define DBG 0


// =========================================================================
// Debug related

#if DBG
// For checked kernels, define a macro to print out informational
// messages.
//
// Used for errors
#define DEBUGLEV_ERROR             ((ULONG)0x00000001)
// Used for warnings
#define DEBUGLEV_WARN              ((ULONG)0x00000002)
// Used for information
#define DEBUGLEV_INFO              ((ULONG)0x00000004)
// Used when entering a function
#define DEBUGLEV_ENTER             ((ULONG)0x00000008)
// Used when exiting a function
#define DEBUGLEV_EXIT              ((ULONG)0x00000010)

// Used for errors
#define DEBUGLEV_VERBOSE_ERROR     ((ULONG)0x00001000)
// Used for warnings
#define DEBUGLEV_VERBOSE_WARN      ((ULONG)0x00002000)
// Used for information
#define DEBUGLEV_VERBOSE_INFO      ((ULONG)0x00004000)
// Used when entering a function
#define DEBUGLEV_VERBOSE_ENTER     ((ULONG)0x00008000)
// Used when exiting a function
#define DEBUGLEV_VERBOSE_EXIT      ((ULONG)0x00010000)


#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
#include <windef.h>
// File from which the debug level will be read from
#define DEBUGLEVEL_FILE   TEXT("FreeOTFE4PDADebugLevel.txt")
void SendComm(WCHAR* AZ, ...);
#endif

extern int FreeOTFEDebugLevel;
extern int FreeOTFEDebugIndent;
extern int FreeOTFEDebugLoopTmp;

#if (defined(FOTFE_PDA) || defined(FOTFE_PC_DLL))
// WinCE (Win32) debug
// From dbgapi.h:
//#define DEBUGMSG(cond,printf_exp)   \
//   ((void)((cond)?(NKDbgPrintfW printf_exp),1:0))
#define DEBUG_SEND  NKDbgPrintfW
//#define DEBUG_SEND  SendComm
#ifdef FOTFE_PDA
//#define DEBUG_FILE TEXT("\\Storage card\\__OUTPUT.txt")
//#define DEBUG_FILE TEXT("\\Storage Card\\__OUTPUT.txt")
#define DEBUG_FILE TEXT("\\My Documents\\__OUTPUT_PDA_driver.txt")
//#define DEBUG_FILE TEXT("\\My Documents\\__OUTPUT_PDA_gui.txt")
#endif
#ifdef FOTFE_PC_DLL
#define DEBUG_FILE TEXT("\\__OUTPUT_PDA_driver.txt")
#endif
#else
// Kernel debug
#define DEBUG_SEND  DbgPrint
#endif



#define DEBUGOUT(DRIVER,LEVEL,STRING)                    \
            if (FreeOTFEDebugLevel & (LEVEL))            \
                {                                        \
                DEBUG_SEND(DRIVER);                      \
                DEBUG_SEND((TEXT(": ")));                \
                if (                                     \
                    (LEVEL & DEBUGLEV_VERBOSE_ERROR) ||  \
                    (LEVEL & DEBUGLEV_VERBOSE_WARN)  ||  \
                    (LEVEL & DEBUGLEV_VERBOSE_INFO)  ||  \
                    (LEVEL & DEBUGLEV_VERBOSE_ENTER) ||  \
                    (LEVEL & DEBUGLEV_VERBOSE_EXIT)      \
                   )                                     \
                    {                                    \
                    DEBUG_SEND((TEXT("(v)")));           \
                    }                                    \
                if (                                     \
                    (LEVEL & DEBUGLEV_EXIT) ||           \
                    (LEVEL & DEBUGLEV_VERBOSE_EXIT)      \
                   )                                     \
                    {                                    \
                    FreeOTFEDebugIndent--;               \
                    if (FreeOTFEDebugIndent < 0)         \
                        {                                \
                        FreeOTFEDebugIndent = 0;         \
                        }                                \
                    }                                    \
                for (FreeOTFEDebugLoopTmp = 0;           \
                     FreeOTFEDebugLoopTmp<=FreeOTFEDebugIndent;  \
                     FreeOTFEDebugLoopTmp++)             \
                    {                                    \
                    DEBUG_SEND((TEXT("  ")));            \
                    }                                    \
                if (                                     \
                    (LEVEL & DEBUGLEV_ERROR) ||          \
                    (LEVEL & DEBUGLEV_VERBOSE_ERROR)     \
                   )                                     \
                    {                                    \
                    DEBUG_SEND((TEXT("ERROR: ")));       \
                    }                                    \
                else if (                                \
                    (LEVEL & DEBUGLEV_WARN) ||           \
                    (LEVEL & DEBUGLEV_VERBOSE_WARN)      \
                   )                                     \
                    {                                    \
                    DEBUG_SEND((TEXT("WARNING: ")));     \
                    }                                    \
                else if (                                \
                    (LEVEL & DEBUGLEV_ENTER) ||          \
                    (LEVEL & DEBUGLEV_VERBOSE_ENTER)     \
                   )                                     \
                    {                                    \
                    DEBUG_SEND((TEXT(">>> ")));          \
                    FreeOTFEDebugIndent++;               \
                    }                                    \
                else if (                                \
                    (LEVEL & DEBUGLEV_EXIT) ||           \
                    (LEVEL & DEBUGLEV_VERBOSE_EXIT)      \
                   )                                     \
                    {                                    \
                    DEBUG_SEND((TEXT("<<< ")));          \
                    }                                    \
                                                         \
                DEBUG_SEND STRING;                       \
                }

// Notice that the last "DEBUG_SEND" doesn't have brackets

#define DEBUGOUTGUI(LEVEL,STRING) \
                DEBUGOUT((TEXT("FreeOTFEGUI")), LEVEL, STRING)
//  SendComm STRING

#ifdef FOTFE_PDA
#define DEBUGOUTMAINDRV(LEVEL,STRING) \
  SendComm STRING
#endif
#ifdef FOTFE_PC_DLL
#define DEBUGOUTMAINDRV(LEVEL,STRING) \
  SendComm STRING
#endif
#ifdef FOTFE_PC_DRIVER
#define DEBUGOUTMAINDRV(LEVEL,STRING) \
                DEBUGOUT((TEXT("FreeOTFE")), LEVEL, STRING)
#endif

// MAC debug disabled as only needed when debugging this functionality
//#define DEBUGOUTMACDRV(LEVEL,STRING) \
//                DEBUGOUT(("FreeOTFEMAC"), LEVEL, STRING)
#define DEBUGOUTMACDRV(LEVEL,STRING)   ((void)0)

// KDF debug disabled as only needed when debugging this functionality
//#define DEBUGOUTKDFDRV(LEVEL,STRING) \
//                DEBUGOUT(("FreeOTFEKDF"), LEVEL, STRING)
#define DEBUGOUTKDFDRV(LEVEL,STRING)   ((void)0)

//#define DEBUGOUTLIB(LEVEL,STRING) \
//                DEBUGOUT((TEXT("FreeOTFElib")), LEVEL, STRING)
//  SendComm STRING
#define DEBUGOUTLIB(LEVEL,STRING)   ((void)0)

#define DEBUGOUTCYPHERDRV(LEVEL,STRING) \
  ((void)0)
//                DEBUGOUT((TEXT("FreeOTFECypherDrv")), LEVEL, STRING)

#define DEBUGOUTCYPHERIMPL(LEVEL,STRING) \
  ((void)0)
//                DEBUGOUT((TEXT("FreeOTFECypherImpl")), LEVEL, STRING)

#define DEBUGOUTHASHDRV(LEVEL,STRING) \
  ((void)0)
//                DEBUGOUT((TEXT("FreeOTFEHashDrv")), LEVEL, STRING)

#define DEBUGOUTHASHIMPL(LEVEL,STRING) \
  ((void)0)
//                DEBUGOUT((TEXT("FreeOTFEHashImpl")), LEVEL, STRING)

#define DEBUGOUTIVDRV(LEVEL,STRING) \
  ((void)0)
//                DEBUGOUT((TEXT("FreeOTFEIVDrv")), LEVEL, STRING)

#define DEBUGOUTIVIMPL(LEVEL,STRING) \
  ((void)0)
//                DEBUGOUT((TEXT("FreeOTFEIVImpl")), LEVEL, STRING)

#define DEBUGOUTDRV(LEVEL,STRING) \
                DEBUGOUT((TEXT("FreeOTFEdrv")), LEVEL, STRING)
//  SendComm STRING

#else

// Debug disabled; stub out all debug output...

#define DEBUGOUT(DRIVER,LEVEL,STRING)    ((void)0)

#define DEBUGOUTGUI(LEVEL,STRING)        ((void)0)
#define DEBUGOUTMAINDRV(LEVEL,STRING)    ((void)0)
#define DEBUGOUTMACDRV(LEVEL,STRING)     ((void)0)
#define DEBUGOUTKDFDRV(LEVEL,STRING)     ((void)0)
#define DEBUGOUTLIB(LEVEL,STRING)        ((void)0)
#define DEBUGOUTCYPHERDRV(LEVEL,STRING)  ((void)0)
#define DEBUGOUTCYPHERIMPL(LEVEL,STRING) ((void)0)
#define DEBUGOUTHASHDRV(LEVEL,STRING)    ((void)0)
#define DEBUGOUTHASHIMPL(LEVEL,STRING)   ((void)0)
#define DEBUGOUTIVDRV(LEVEL,STRING)      ((void)0)
#define DEBUGOUTIVIMPL(LEVEL,STRING)     ((void)0)
#define DEBUGOUTDRV(LEVEL,STRING)        ((void)0)

#endif



// =========================================================================
// =========================================================================

#endif

