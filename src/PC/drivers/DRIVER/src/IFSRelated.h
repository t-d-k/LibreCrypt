// Description: Various NTIFS Related
//
// -----------------------------------------------------------------------------
//


#ifndef _IFSRelated_H
#define _IFSRelated_H   1


#include <ntverp.h>  // Needed for VER_PRODUCTBUILD

// Structs from MSDN WWW site:
#define TOKEN_SOURCE_LENGTH 8

typedef struct _TOKEN_SOURCE {
  CHAR  SourceName[TOKEN_SOURCE_LENGTH];
  LUID  SourceIdentifier;
} TOKEN_SOURCE, *PTOKEN_SOURCE;


typedef struct _TOKEN_CONTROL {
  LUID  TokenId;
  LUID  AuthenticationId;
  LUID  ModifiedId;
  TOKEN_SOURCE  TokenSource;
} TOKEN_CONTROL, *PTOKEN_CONTROL;


typedef struct _SECURITY_CLIENT_CONTEXT {
  SECURITY_QUALITY_OF_SERVICE SecurityQos;
  PACCESS_TOKEN               ClientToken;
  BOOLEAN                     DirectlyAccessClientToken;
  BOOLEAN                     DirectAccessEffectiveOnly;
  BOOLEAN                     ServerIsRemote;
  TOKEN_CONTROL               ClientTokenControl;
} SECURITY_CLIENT_CONTEXT, *PSECURITY_CLIENT_CONTEXT;

NTSTATUS
  SeCreateClientSecurity(
    IN PETHREAD  ClientThread,
    IN PSECURITY_QUALITY_OF_SERVICE  ClientSecurityQos,
    IN BOOLEAN  ServerIsRemote,
    OUT PSECURITY_CLIENT_CONTEXT  ClientContext
    );

NTSTATUS
  SeImpersonateClientEx(
    IN PSECURITY_CLIENT_CONTEXT  ClientContext,
    IN PETHREAD  ServerThread  OPTIONAL
    ); 

typedef enum _TOKEN_TYPE {
  TokenPrimary = 1,
  TokenImpersonation
} TOKEN_TYPE;
typedef TOKEN_TYPE *PTOKEN_TYPE;

NTKERNELAPI
TOKEN_TYPE
SeTokenType (
    IN PACCESS_TOKEN Token
);

NTKERNELAPI
VOID
PsRevertToSelf();

/*
   From the OSR Online WWW site:

   http://www.osronline.com/article.cfm?id=33 

   "* PsDereferencePrimaryToken – this has been converted from a macro (in the
      Windows 2000 IFS Kit) to a function (in the Windows XP IFS Kit).   Thus,
      if your file system or file system filter driver utilizes this function
      you will need to recompile to ensure you are using the correct
      implementation of this operation.

    * PsDereferenceImpersonationToken – this has been converted from a macro
      (in the Windows 2000 IFS Kit) to a function (in the Windows XP IFS Kit).
      Thus, if your file system or file system filter driver utilizes this
      function you will need to recompile to ensure you are using the correct
      implementation of this operation."

    However, we always use the Windows 2000 macro; this seems to work OK under
    Windows XP, and means that we don't need separate Windows 2000/Windows XP builds of
    the main FreeOTFE driver.

    To be correct; comment out the "if FALSE" line, and uncomment the line following
    it.
*/

#if FALSE
// #if (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
VOID
PsDereferenceImpersonationToken (
    IN PACCESS_TOKEN ImpersonationToken
);

NTKERNELAPI
VOID
PsDereferencePrimaryToken (
    IN PACCESS_TOKEN PrimaryToken
);

#else

#define PsDereferenceImpersonationToken(T)  {  \
    if (ARGUMENT_PRESENT(T)) {                 \
        ObDereferenceObject(T);                \
    }                                          \
}

#define PsDereferencePrimaryToken(T) (ObDereferenceObject((T)))

#endif

/*
NTKERNELAPI
VOID
  SeDeleteClientSecurity(
    IN PSECURITY_CLIENT_CONTEXT  ClientContext
    );
*/

#define SeDeleteClientSecurity(C)  {                        \
    if (SeTokenType((C)->ClientToken) == TokenPrimary) {    \
        PsDereferencePrimaryToken((C)->ClientToken);        \
    } else {                                                \
        PsDereferenceImpersonationToken((C)->ClientToken);  \
    }                                                       \
}

// =========================================================================
// From:
//   http://www.osronline.com/showThread.cfm?link=154

NTSYSAPI
NTSTATUS
NTAPI
ZwFsControlFile(
    IN   HANDLE hFile,
    IN   HANDLE hEvent OPTIONAL,
    IN   PIO_APC_ROUTINE IoApcRoutine OPTIONAL,
    IN   PVOID IoApcContext OPTIONAL,
    OUT  PIO_STATUS_BLOCK pIoStatusBlock,
    IN   ULONG FileSystemControlCode,
    IN   PVOID InBuffer OPTIONAL,
    IN   ULONG InBufferLength,
    OUT  PVOID OutBuffer OPTIONAL,
    IN   ULONG OutBufferLength
);


// =========================================================================
// =========================================================================

#endif

