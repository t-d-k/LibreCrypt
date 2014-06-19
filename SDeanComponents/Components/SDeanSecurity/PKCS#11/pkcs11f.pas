unit pkcs11f;

// Written: 19/02/08
//
// This file is derived from the RSA Security Inc. PKCS #11 Cryptographic Token
// Interface (Cryptoki), and is a straight port of the RSA source (pkcs11f.h)


interface

// xxx - lplp - cdecl/stdcall on all the imported functions? assumed cdecl here
// xxx - lplp - cdecl/stdcall on all the imported functions? assumed cdecl here
// xxx - lplp - cdecl/stdcall on all the imported functions? assumed cdecl here
// xxx - lplp - cdecl/stdcall on all the imported functions? assumed cdecl here
// xxx - lplp - cdecl/stdcall on all the imported functions? assumed cdecl here

uses
  pkcs11t;

// This header file contains pretty much everything about all the
// Cryptoki function prototypes.  Because this information is
// used for more than just declaring function prototypes, the
// order of the functions appearing herein is important, and
// should not be altered.

// General-purpose

// C_Initialize initializes the Cryptoki library.
type C_Initialize = function
(
  pInitArgs: CK_VOID_PTR   // if this is not NULL_PTR, it gets
                           // cast to CK_C_INITIALIZE_ARGS_PTR
                           // and dereferenced
): CK_RV; cdecl;
type CK_C_Initialize = ^C_Initialize;

// C_Finalize indicates that an application is done with the
// Cryptoki library.
type C_Finalize = function
(
  pReserved:  CK_VOID_PTR   // reserved.  Should be NULL_PTR
): CK_RV; cdecl;
type CK_C_Finalize = ^C_Finalize;


// C_GetInfo returns general information about Cryptoki. 
type C_GetInfo = function
(
  pInfo:  CK_INFO_PTR   // location that receives information
): CK_RV; cdecl;
type CK_C_GetInfo = ^C_GetInfo;


// C_GetFunctionList returns the function list. 
type C_GetFunctionList = function
(
  ppFunctionList:  CK_FUNCTION_LIST_PTR_PTR // receives pointer to
                                            // function list
): CK_RV; cdecl;
type CK_C_GetFunctionList = ^C_GetFunctionList;



// Slot and token management 

// C_GetSlotList obtains a list of slots in the system. 
type C_GetSlotList = function
(
  tokenPresent:  CK_BBOOL;       // only slots with tokens?
  pSlotList:     CK_SLOT_ID_PTR; // receives array of slot IDs
  pulCount:      CK_ULONG_PTR    // receives number of slots
): CK_RV; cdecl;
type CK_C_GetSlotList = ^C_GetSlotList;


// C_GetSlotInfo obtains information about a particular slot in
// the system. 
type C_GetSlotInfo = function
(
  slotID:  CK_SLOT_ID;       // the ID of the slot
  pInfo:   CK_SLOT_INFO_PTR  // receives the slot information
): CK_RV; cdecl;
type CK_C_GetSlotInfo = ^C_GetSlotInfo;


// C_GetTokenInfo obtains information about a particular token
// in the system. 
type C_GetTokenInfo = function
(
  slotID:  CK_SLOT_ID;        // ID of the token's slot
  pInfo:   CK_TOKEN_INFO_PTR  // receives the token information
): CK_RV; cdecl;
type CK_C_GetTokenInfo = ^C_GetTokenInfo;


// C_GetMechanismList obtains a list of mechanism types
// supported by a token.
type C_GetMechanismList = function
(
  slotID:          CK_SLOT_ID;            // ID of token's slot
  pMechanismList:  CK_MECHANISM_TYPE_PTR; // gets mech. array
  pulCount:        CK_ULONG_PTR           // gets # of mechs.
): CK_RV; cdecl;
type CK_C_GetMechanismList = ^C_GetMechanismList;


// C_GetMechanismInfo obtains information about a particular
// mechanism possibly supported by a token.
type C_GetMechanismInfo = function
(
  slotID:  CK_SLOT_ID;            // ID of the token's slot
  // "mechType" as "type" is a Delphi reserved word
  mechType:    CK_MECHANISM_TYPE;     // type of mechanism
  pInfo:   CK_MECHANISM_INFO_PTR  // receives mechanism info
): CK_RV; cdecl;
type CK_C_GetMechanismInfo = ^C_GetMechanismInfo;


// C_InitToken initializes a token.
type C_InitToken = function
// pLabel changed from CK_CHAR_PTR to CK_UTF8CHAR_PTR for v2.10
(
  slotID:    CK_SLOT_ID;      // ID of the token's slot
  pPin:      CK_UTF8CHAR_PTR; // the SO's initial PIN
  ulPinLen:  CK_ULONG;        // length in bytes of the PIN
  pLabel:    CK_UTF8CHAR_PTR  // 32-byte token label (blank padded)
): CK_RV; cdecl;
type CK_C_InitToken = ^C_InitToken;


// C_InitPIN initializes the normal user's PIN. 
type C_InitPIN = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  pPin:      CK_UTF8CHAR_PTR;   // the normal user's PIN
  ulPinLen:  CK_ULONG           // length in bytes of the PIN
): CK_RV; cdecl;
type CK_C_InitPIN = ^C_InitPIN;


// C_SetPIN modifies the PIN of the user who is logged in.
type C_SetPIN = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  pOldPin:   CK_UTF8CHAR_PTR;   // the old PIN
  ulOldLen:  CK_ULONG;          // length of the old PIN
  pNewPin:   CK_UTF8CHAR_PTR;   // the new PIN
  ulNewLen:  CK_ULONG           // length of the new PIN
): CK_RV; cdecl;
type CK_C_SetPIN = ^C_SetPIN;



// Session management 

// C_OpenSession opens a session between an application and a
// token. 
type C_OpenSession = function
(
  slotID:        CK_SLOT_ID;            // the slot's ID
  flags:         CK_FLAGS;              // from CK_SESSION_INFO
  pApplication:  CK_VOID_PTR;           // passed to callback
  Notify:        CK_NOTIFY;             // callback function
  phSession:     CK_SESSION_HANDLE_PTR  // gets session handle
): CK_RV; cdecl;
type CK_C_OpenSession = ^C_OpenSession;


// C_CloseSession closes a session between an application and a
// token. 
type C_CloseSession = function
(
  hSession:  CK_SESSION_HANDLE // the session's handle
): CK_RV; cdecl;
type CK_C_CloseSession = ^C_CloseSession;


// C_CloseAllSessions closes all sessions with a token. 
type C_CloseAllSessions = function
(
  slotID:  CK_SLOT_ID     // the token's slot
): CK_RV; cdecl;
type CK_C_CloseAllSessions = ^C_CloseAllSessions;


// C_GetSessionInfo obtains information about the session.
type C_GetSessionInfo = function
(
  hSession:  CK_SESSION_HANDLE;   // the session's handle
  pInfo:     CK_SESSION_INFO_PTR  // receives session info
): CK_RV; cdecl;
type CK_C_GetSessionInfo = ^C_GetSessionInfo;


// C_GetOperationState obtains the state of the cryptographic operation
// in a session.
type C_GetOperationState = function
(
  hSession:             CK_SESSION_HANDLE; // session's handle
  pOperationState:      CK_BYTE_PTR;       // gets state
  pulOperationStateLen: CK_ULONG_PTR       // gets state length
): CK_RV; cdecl;
type CK_C_GetOperationState = ^C_GetOperationState;


// C_SetOperationState restores the state of the cryptographic
// operation in a session. 
type C_SetOperationState = function
(
  hSession:             CK_SESSION_HANDLE; // session's handle
  pOperationState:      CK_BYTE_PTR;       // holds state
  ulOperationStateLen:  CK_ULONG;          // holds state length
  hEncryptionKey:       CK_OBJECT_HANDLE;  // en/decryption key
  hAuthenticationKey:   CK_OBJECT_HANDLE   // sign/verify key
): CK_RV; cdecl;
type CK_C_SetOperationState = ^C_SetOperationState;


// C_Login logs a user into a token. 
type C_Login = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  userType:  CK_USER_TYPE;      // the user type
  pPin:      CK_UTF8CHAR_PTR;   // the user's PIN
  ulPinLen:  CK_ULONG           // the length of the PIN
): CK_RV; cdecl;
type CK_C_Login = ^C_Login;


// C_Logout logs a user out from a token. 
type C_Logout = function
(
  hSession: CK_SESSION_HANDLE // the session's handle
): CK_RV; cdecl;
type CK_C_Logout = ^C_Logout;



// Object management 

// C_CreateObject creates a new object. 
type C_CreateObject = function
(
  hSession:    CK_SESSION_HANDLE; // the session's handle
  pTemplate:   CK_ATTRIBUTE_PTR;  // the object's template
  ulCount:     CK_ULONG;          // attributes in template
  phObject:    CK_OBJECT_HANDLE_PTR  // gets new object's handle.
): CK_RV; cdecl;
type CK_C_CreateObject = ^C_CreateObject;


// C_CopyObject copies an object, creating a new object for the
// copy.
type C_CopyObject = function
(
  hSession:     CK_SESSION_HANDLE;    // the session's handle
  hObject:      CK_OBJECT_HANDLE;     // the object's handle
  pTemplate:    CK_ATTRIBUTE_PTR;     // template for new object
  ulCount:      CK_ULONG;             // attributes in template
  phNewObject:  CK_OBJECT_HANDLE_PTR  // receives handle of copy
): CK_RV; cdecl;
type CK_C_CopyObject = ^C_CopyObject;


// C_DestroyObject destroys an object. 
type C_DestroyObject = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  hObject:   CK_OBJECT_HANDLE   // the object's handle
): CK_RV; cdecl;
type CK_C_DestroyObject = ^C_DestroyObject;


// C_GetObjectSize gets the size of an object in bytes. 
type C_GetObjectSize = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  hObject:   CK_OBJECT_HANDLE;  // the object's handle
  pulSize:   CK_ULONG_PTR       // receives size of object
): CK_RV; cdecl;
type CK_C_GetObjectSize = ^C_GetObjectSize;


// C_GetAttributeValue obtains the value of one or more object
// attributes.
type C_GetAttributeValue = function
(
  hSession:   CK_SESSION_HANDLE; // the session's handle
  hObject:    CK_OBJECT_HANDLE;  // the object's handle
  pTemplate:  CK_ATTRIBUTE_PTR;  // specifies attrs; gets vals
  ulCount:    CK_ULONG           // attributes in template
): CK_RV; cdecl;
type CK_C_GetAttributeValue = ^C_GetAttributeValue;


// C_SetAttributeValue modifies the value of one or more object
// attributes 
type C_SetAttributeValue = function
(
  hSession:   CK_SESSION_HANDLE; // the session's handle
  hObject:    CK_OBJECT_HANDLE;  // the object's handle
  pTemplate:  CK_ATTRIBUTE_PTR;  // specifies attrs and values
  ulCount:    CK_ULONG           // attributes in template
): CK_RV; cdecl;
type CK_C_SetAttributeValue = ^C_SetAttributeValue;


// C_FindObjectsInit initializes a search for token and session
// objects that match a template.
type C_FindObjectsInit = function
(
  hSession:   CK_SESSION_HANDLE; // the session's handle
  pTemplate:  CK_ATTRIBUTE_PTR;  // attribute values to match
  ulCount:    CK_ULONG           // attrs in search template
): CK_RV; cdecl;
type CK_C_FindObjectsInit = ^C_FindObjectsInit;


// C_FindObjects continues a search for token and session
// objects that match a template, obtaining additional object
// handles. 
type C_FindObjects = function
(
 hSession:          CK_SESSION_HANDLE;    // session's handle
 phObject:          CK_OBJECT_HANDLE_PTR; // gets obj. handles
 ulMaxObjectCount:  CK_ULONG;             // max handles to get
 pulObjectCount:    CK_ULONG_PTR          // actual # returned
): CK_RV; cdecl;
type CK_C_FindObjects = ^C_FindObjects;


// C_FindObjectsFinal finishes a search for token and session
// objects.
type C_FindObjectsFinal = function
(
  hSession: CK_SESSION_HANDLE // the session's handle
): CK_RV; cdecl;
type CK_C_FindObjectsFinal = ^C_FindObjectsFinal;



// Encryption and decryption 

// C_EncryptInit initializes an encryption operation. 
type C_EncryptInit = function
(
  hSession:    CK_SESSION_HANDLE; // the session's handle
  pMechanism:  CK_MECHANISM_PTR;  // the encryption mechanism
  hKey:        CK_OBJECT_HANDLE   // handle of encryption key
): CK_RV; cdecl;
type CK_C_EncryptInit = ^C_EncryptInit;


// C_Encrypt encrypts single-part data.
type C_Encrypt = function
(
  hSession:            CK_SESSION_HANDLE; // session's handle
  pData:               CK_BYTE_PTR;       // the plaintext data
  ulDataLen:           CK_ULONG;          // bytes of plaintext
  pEncryptedData:      CK_BYTE_PTR;       // gets ciphertext
  pulEncryptedDataLen: CK_ULONG_PTR       // gets c-text size
): CK_RV; cdecl;
type CK_C_Encrypt = ^C_Encrypt;


// C_EncryptUpdate continues a multiple-part encryption
// operation.
type C_EncryptUpdate = function
(
  hSession:           CK_SESSION_HANDLE; // session's handle
  pPart:              CK_BYTE_PTR;       // the plaintext data
  ulPartLen:          CK_ULONG;          // plaintext data len
  pEncryptedPart:     CK_BYTE_PTR;       // gets ciphertext
  pulEncryptedPartLen:CK_ULONG_PTR       // gets c-text size
): CK_RV; cdecl;
type CK_C_EncryptUpdate = ^C_EncryptUpdate;


// C_EncryptFinal finishes a multiple-part encryption
// operation.
type C_EncryptFinal = function
(
  hSession:                CK_SESSION_HANDLE; // session handle
  pLastEncryptedPart:      CK_BYTE_PTR;       // last c-text
  pulLastEncryptedPartLen: CK_ULONG_PTR       // gets last size
): CK_RV; cdecl;
type CK_C_EncryptFinal = ^C_EncryptFinal;


// C_DecryptInit initializes a decryption operation.
type C_DecryptInit = function
(
  hSession:    CK_SESSION_HANDLE; // the session's handle
  pMechanism:  CK_MECHANISM_PTR;  // the decryption mechanism
  hKey:        CK_OBJECT_HANDLE   // handle of decryption key
): CK_RV; cdecl;
type CK_C_DecryptInit = ^C_DecryptInit;


// C_Decrypt decrypts encrypted data in a single part.
type C_Decrypt = function
(
  hSession:           CK_SESSION_HANDLE; // session's handle
  pEncryptedData:     CK_BYTE_PTR;       // ciphertext
  ulEncryptedDataLen: CK_ULONG;          // ciphertext length
  pData:              CK_BYTE_PTR;       // gets plaintext
  pulDataLen:         CK_ULONG_PTR       // gets p-text size
): CK_RV; cdecl;
type CK_C_Decrypt = ^C_Decrypt;


// C_DecryptUpdate continues a multiple-part decryption
// operation. 
type C_DecryptUpdate = function
(
  hSession:            CK_SESSION_HANDLE; // session's handle
  pEncryptedPart:      CK_BYTE_PTR;       // encrypted data
  ulEncryptedPartLen:  CK_ULONG;          // input length
  pPart:               CK_BYTE_PTR;       // gets plaintext
  pulPartLen:          CK_ULONG_PTR       // p-text size
): CK_RV; cdecl;
type CK_C_DecryptUpdate = ^C_DecryptUpdate;


// C_DecryptFinal finishes a multiple-part decryption
// operation.
type C_DecryptFinal = function
(
  hSession:       CK_SESSION_HANDLE; // the session's handle
  pLastPart:      CK_BYTE_PTR;       // gets plaintext
  pulLastPartLen: CK_ULONG_PTR       // p-text size
): CK_RV; cdecl;
type CK_C_DecryptFinal = ^C_DecryptFinal;



// Message digesting 

// C_DigestInit initializes a message-digesting operation. 
type C_DigestInit = function
(
  hSession:   CK_SESSION_HANDLE; // the session's handle
  pMechanism: CK_MECHANISM_PTR  // the digesting mechanism
): CK_RV; cdecl;
type CK_C_DigestInit = ^C_DigestInit;


// C_Digest digests data in a single part.
type C_Digest = function
(
  hSession:     CK_SESSION_HANDLE; // the session's handle
  pData:        CK_BYTE_PTR;       // data to be digested
  ulDataLen:    CK_ULONG;          // bytes of data to digest
  pDigest:      CK_BYTE_PTR;       // gets the message digest
  pulDigestLen: CK_ULONG_PTR       // gets digest length
): CK_RV; cdecl;
type CK_C_Digest = ^C_Digest;


// C_DigestUpdate continues a multiple-part message-digesting
// operation.
type C_DigestUpdate = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  pPart:     CK_BYTE_PTR;       // data to be digested
  ulPartLen: CK_ULONG           // bytes of data to be digested
): CK_RV; cdecl;
type CK_C_DigestUpdate = ^C_DigestUpdate;


// C_DigestKey continues a multi-part message-digesting
// operation, by digesting the value of a secret key as part of
// the data already digested.
type C_DigestKey = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  hKey:      CK_OBJECT_HANDLE   // secret key to digest
): CK_RV; cdecl;
type CK_C_DigestKey = ^C_DigestKey;


// C_DigestFinal finishes a multiple-part message-digesting
// operation.
type C_DigestFinal = function
(
  hSession:     CK_SESSION_HANDLE; // the session's handle
  pDigest:      CK_BYTE_PTR;       // gets the message digest
  pulDigestLen: CK_ULONG_PTR       // gets byte count of digest
): CK_RV; cdecl;
type CK_C_DigestFinal = ^C_DigestFinal;



// Signing and MACing

// C_SignInit initializes a signature (private key encryption)
// operation, where the signature is (will be) an appendix to
// the data, and plaintext cannot be recovered from the
//signature.
type C_SignInit = function
(
  hSession:    CK_SESSION_HANDLE; // the session's handle
  pMechanism:  CK_MECHANISM_PTR;  // the signature mechanism
  hKey:        CK_OBJECT_HANDLE   // handle of signature key
): CK_RV; cdecl;
type CK_C_SignInit = ^C_SignInit;


// C_Sign signs (encrypts with private key) data in a single
// part, where the signature is (will be) an appendix to the
// data, and plaintext cannot be recovered from the signature.
type C_Sign = function
(
  hSession:        CK_SESSION_HANDLE; // the session's handle
  pData:           CK_BYTE_PTR;       // the data to sign
  ulDataLen:       CK_ULONG;          // count of bytes to sign
  pSignature:      CK_BYTE_PTR;       // gets the signature
  pulSignatureLen: CK_ULONG_PTR       // gets signature length
): CK_RV; cdecl;
type CK_C_Sign = ^C_Sign;


// C_SignUpdate continues a multiple-part signature operation,
// where the signature is (will be) an appendix to the data,
// and plaintext cannot be recovered from the signature.
type C_SignUpdate = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  pPart:     CK_BYTE_PTR;       // the data to sign
  ulPartLen: CK_ULONG           // count of bytes to sign
): CK_RV; cdecl;
type CK_C_SignUpdate = ^C_SignUpdate;


// C_SignFinal finishes a multiple-part signature operation:
// returning the signature.
type C_SignFinal = function
(
  hSession:        CK_SESSION_HANDLE; // the session's handle
  pSignature:      CK_BYTE_PTR;       // gets the signature
  pulSignatureLen: CK_ULONG_PTR       // gets signature length
): CK_RV; cdecl;
type CK_C_SignFinal = ^C_SignFinal;


// C_SignRecoverInit initializes a signature operation, where
// the data can be recovered from the signature.
type C_SignRecoverInit = function
(
  hSession:   CK_SESSION_HANDLE; // the session's handle
  pMechanism: CK_MECHANISM_PTR;  // the signature mechanism
  hKey:       CK_OBJECT_HANDLE   // handle of the signature key
): CK_RV; cdecl;
type CK_C_SignRecoverInit = ^C_SignRecoverInit;


// C_SignRecover signs data in a single operation, where the
// data can be recovered from the signature. 
type C_SignRecover = function
(
  hSession:        CK_SESSION_HANDLE; // the session's handle
  pData:           CK_BYTE_PTR;       // the data to sign
  ulDataLen:       CK_ULONG;          // count of bytes to sign
  pSignature:      CK_BYTE_PTR;       // gets the signature
  pulSignatureLen: CK_ULONG_PTR       // gets signature length
): CK_RV; cdecl;
type CK_C_SignRecover = ^C_SignRecover;



// Verifying signatures and MACs

// C_VerifyInit initializes a verification operation, where the
// signature is an appendix to the data, and plaintext cannot
//  cannot be recovered from the signature (e.g. DSA).
type C_VerifyInit = function
(
  hSession:    CK_SESSION_HANDLE; // the session's handle
  pMechanism:  CK_MECHANISM_PTR;  // the verification mechanism
  hKey:        CK_OBJECT_HANDLE   // verification key
): CK_RV; cdecl;
type CK_C_VerifyInit = ^C_VerifyInit;


// C_Verify verifies a signature in a single-part operation, 
// where the signature is an appendix to the data, and plaintext
// cannot be recovered from the signature. 
type C_Verify = function
(
  hSession:       CK_SESSION_HANDLE; // the session's handle
  pData:          CK_BYTE_PTR;       // signed data
  ulDataLen:      CK_ULONG;          // length of signed data
  pSignature:     CK_BYTE_PTR;       // signature
  ulSignatureLen: CK_ULONG           // signature length
): CK_RV; cdecl;
type CK_C_Verify = ^C_Verify;


// C_VerifyUpdate continues a multiple-part verification
// operation, where the signature is an appendix to the data,
// and plaintext cannot be recovered from the signature.
type C_VerifyUpdate = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  pPart:     CK_BYTE_PTR;       // signed data
  ulPartLen: CK_ULONG           // length of signed data
): CK_RV; cdecl;
type CK_C_VerifyUpdate = ^C_VerifyUpdate;


// C_VerifyFinal finishes a multiple-part verification
// operation, checking the signature. 
type C_VerifyFinal = function
(
  hSession:       CK_SESSION_HANDLE; // the session's handle
  pSignature:     CK_BYTE_PTR;       // signature to verify
  ulSignatureLen: CK_ULONG           // signature length
): CK_RV; cdecl;
type CK_C_VerifyFinal = ^C_VerifyFinal;


// C_VerifyRecoverInit initializes a signature verification
// operation, where the data is recovered from the signature.
type C_VerifyRecoverInit = function
(
  hSession:    CK_SESSION_HANDLE; // the session's handle
  pMechanism:  CK_MECHANISM_PTR;  // the verification mechanism
  hKey:        CK_OBJECT_HANDLE   // verification key
): CK_RV; cdecl;
type CK_C_VerifyRecoverInit = ^C_VerifyRecoverInit;


// C_VerifyRecover verifies a signature in a single-part
// operation, where the data is recovered from the signature. 
type C_VerifyRecover = function
(
  hSession:        CK_SESSION_HANDLE; // the session's handle
  pSignature:      CK_BYTE_PTR;       // signature to verify
  ulSignatureLen:  CK_ULONG;          // signature length
  pData:           CK_BYTE_PTR;       // gets signed data
  pulDataLen:      CK_ULONG_PTR       // gets signed data len
): CK_RV; cdecl;
type CK_C_VerifyRecover = ^C_VerifyRecover;



// Dual-function cryptographic operations 

// C_DigestEncryptUpdate continues a multiple-part digesting
// and encryption operation. 
type C_DigestEncryptUpdate = function
(
  hSession:            CK_SESSION_HANDLE; // session's handle
  pPart:               CK_BYTE_PTR;       // the plaintext data
  ulPartLen:           CK_ULONG;          // plaintext length
  pEncryptedPart:      CK_BYTE_PTR;       // gets ciphertext
  pulEncryptedPartLen: CK_ULONG_PTR       // gets c-text length
): CK_RV; cdecl;
type CK_C_DigestEncryptUpdate = ^C_DigestEncryptUpdate;


// C_DecryptDigestUpdate continues a multiple-part decryption and
// digesting operation. 
type C_DecryptDigestUpdate = function
(
  hSession:            CK_SESSION_HANDLE; // session's handle
  pEncryptedPart:      CK_BYTE_PTR;       // ciphertext
  ulEncryptedPartLen:  CK_ULONG;          // ciphertext length
  pPart:               CK_BYTE_PTR;       // gets plaintext
  pulPartLen:          CK_ULONG_PTR       // gets plaintext len
): CK_RV; cdecl;
type CK_C_DecryptDigestUpdate = ^C_DecryptDigestUpdate;


// C_SignEncryptUpdate continues a multiple-part signing and
// encryption operation.
type C_SignEncryptUpdate = function
(
  hSession:            CK_SESSION_HANDLE; // session's handle
  pPart:               CK_BYTE_PTR;       // the plaintext data
  ulPartLen:           CK_ULONG;          // plaintext length
  pEncryptedPart:      CK_BYTE_PTR;       // gets ciphertext
  pulEncryptedPartLen: CK_ULONG_PTR       // gets c-text length
): CK_RV; cdecl;
type CK_C_SignEncryptUpdate = ^C_SignEncryptUpdate;


// C_DecryptVerifyUpdate continues a multiple-part decryption and
// verify operation. 
type C_DecryptVerifyUpdate = function
(
  hSession:            CK_SESSION_HANDLE; // session's handle
  pEncryptedPart:      CK_BYTE_PTR;       // ciphertext
  ulEncryptedPartLen:  CK_ULONG;          // ciphertext length
  pPart:               CK_BYTE_PTR;       // gets plaintext
  pulPartLen:          CK_ULONG_PTR       // gets p-text length
): CK_RV; cdecl;
type CK_C_DecryptVerifyUpdate = ^C_DecryptVerifyUpdate;



// Key management

// C_GenerateKey generates a secret key, creating a new key
// object.
type C_GenerateKey = function
(
  hSession:    CK_SESSION_HANDLE;    // the session's handle
  pMechanism:  CK_MECHANISM_PTR;     // key generation mech.
  pTemplate:   CK_ATTRIBUTE_PTR;     // template for new key
  ulCount:     CK_ULONG;             // # of attrs in template
  phKey:       CK_OBJECT_HANDLE_PTR  // gets handle of new key
): CK_RV; cdecl;
type CK_C_GenerateKey = ^C_GenerateKey;


// C_GenerateKeyPair generates a public-key/private-key pair, 
// creating new key objects. 
type C_GenerateKeyPair = function
(
  hSession:                    CK_SESSION_HANDLE;    // session
                                                     // handle
  pMechanism:                  CK_MECHANISM_PTR;     // key-gen
                                                     // mech.
  pPublicKeyTemplate:          CK_ATTRIBUTE_PTR;     // template
                                                     // for pub.
                                                     // key
  ulPublicKeyAttributeCount:   CK_ULONG;             // # pub.
                                                     // attrs.
  pPrivateKeyTemplate:         CK_ATTRIBUTE_PTR;     // template
                                                     // for priv.
                                                     // key
  ulPrivateKeyAttributeCount:  CK_ULONG;             // # priv.
                                                     // attrs.
  phPublicKey:                 CK_OBJECT_HANDLE_PTR; // gets pub.
                                                     // key
                                                     // handle
  phPrivateKey:                CK_OBJECT_HANDLE_PTR  // gets
                                                     // priv. key
                                                     // handle
): CK_RV; cdecl;
type CK_C_GenerateKeyPair = ^C_GenerateKeyPair;


// C_WrapKey wraps (i.e., encrypts) a key.
type C_WrapKey = function
(
  hSession:        CK_SESSION_HANDLE; // the session's handle
  pMechanism:      CK_MECHANISM_PTR;  // the wrapping mechanism
  hWrappingKey:    CK_OBJECT_HANDLE;  // wrapping key
  hKey:            CK_OBJECT_HANDLE;  // key to be wrapped
  pWrappedKey:     CK_BYTE_PTR;       // gets wrapped key
  pulWrappedKeyLen:CK_ULONG_PTR       // gets wrapped key size
): CK_RV; cdecl;
type CK_C_WrapKey = ^C_WrapKey;


// C_UnwrapKey unwraps (decrypts) a wrapped key, creating a new
// key object.
type C_UnwrapKey = function
(
  hSession:          CK_SESSION_HANDLE;    // session's handle
  pMechanism:        CK_MECHANISM_PTR;     // unwrapping mech.
  hUnwrappingKey:    CK_OBJECT_HANDLE;     // unwrapping key
  pWrappedKey:       CK_BYTE_PTR;          // the wrapped key
  ulWrappedKeyLen:   CK_ULONG;             // wrapped key len
  pTemplate:         CK_ATTRIBUTE_PTR;     // new key template
  ulAttributeCount:  CK_ULONG;             // template length
  phKey:             CK_OBJECT_HANDLE_PTR  // gets new handle
): CK_RV; cdecl;
type CK_C_UnwrapKey = ^C_UnwrapKey;


// C_DeriveKey derives a key from a base key, creating a new key
// object.
type C_DeriveKey = function
(
  hSession:          CK_SESSION_HANDLE;    // session's handle
  pMechanism:        CK_MECHANISM_PTR;     // key deriv. mech.
  hBaseKey:          CK_OBJECT_HANDLE;     // base key
  pTemplate:         CK_ATTRIBUTE_PTR;     // new key template
  ulAttributeCount:  CK_ULONG;             // template length
  phKey:             CK_OBJECT_HANDLE_PTR  // gets new handle
): CK_RV; cdecl;
type CK_C_DeriveKey = ^C_DeriveKey;



// Random number generation

// C_SeedRandom mixes additional seed material into the token's
// random number generator.
type C_SeedRandom = function
(
  hSession:  CK_SESSION_HANDLE; // the session's handle
  pSeed:     CK_BYTE_PTR;       // the seed material
  ulSeedLen: CK_ULONG           // length of seed material
): CK_RV; cdecl;
type CK_C_SeedRandom = ^C_SeedRandom;


// C_GenerateRandom generates random data. 
type C_GenerateRandom = function
(
  hSession:    CK_SESSION_HANDLE; // the session's handle
  RandomData:  CK_BYTE_PTR;       // receives the random data
  ulRandomLen: CK_ULONG           // # of bytes to generate
): CK_RV; cdecl;
type CK_C_GenerateRandom = ^C_GenerateRandom;



// Parallel function management

// C_GetFunctionStatus is a legacy function; it obtains an
// updated status of a function running in parallel with an
// application.
type C_GetFunctionStatus = function
(
  hSession: CK_SESSION_HANDLE // the session's handle
): CK_RV; cdecl;
type CK_C_GetFunctionStatus = ^C_GetFunctionStatus;


// C_CancelFunction is a legacy function; it cancels a function
// running in parallel.
type C_CancelFunction = function
(
  hSession: CK_SESSION_HANDLE // the session's handle
): CK_RV; cdecl;
type CK_C_CancelFunction = ^C_CancelFunction;



// Functions added in for Cryptoki Version 2.01 or later 

// C_WaitForSlotEvent waits for a slot event (token insertion,
// removal, etc.) to occur. 
type C_WaitForSlotEvent = function
(
  flags:        CK_FLAGS; // blocking/nonblocking flag
  pSlot:  CK_SLOT_ID_PTR; // location that receives the slot ID
  pRserved:  CK_VOID_PTR  // reserved.  Should be NULL_PTR
): CK_RV; cdecl;
type CK_C_WaitForSlotEvent = ^C_WaitForSlotEvent;





type CK_FUNCTION_LIST = packed record
  version: CK_VERSION;  // Cryptoki version

  // Pile all the function pointers into the CK_FUNCTION_LIST.
  // pkcs11f.h has all the information about the Cryptoki
  // function prototypes.
  CK_C_Initialize: C_Initialize;
  CK_C_Finalize: C_Finalize;
  CK_C_GetInfo: C_GetInfo;
  CK_C_GetFunctionList: C_GetFunctionList;
  CK_C_GetSlotList: C_GetSlotList;
  CK_C_GetSlotInfo: C_GetSlotInfo;
  CK_C_GetTokenInfo: C_GetTokenInfo;
  CK_C_GetMechanismList: C_GetMechanismList;
  CK_C_GetMechanismInfo: C_GetMechanismInfo;
  CK_C_InitToken: C_InitToken;
  CK_C_InitPIN: C_InitPIN;
  CK_C_SetPIN: C_SetPIN;
  CK_C_OpenSession: C_OpenSession;
  CK_C_CloseSession: C_CloseSession;
  CK_C_CloseAllSessions: C_CloseAllSessions;
  CK_C_GetSessionInfo: C_GetSessionInfo;
  CK_C_GetOperationState: C_GetOperationState;
  CK_C_SetOperationState: C_SetOperationState;
  CK_C_Login: C_Login;
  CK_C_Logout: C_Logout;
  CK_C_CreateObject: C_CreateObject;
  CK_C_CopyObject: C_CopyObject;
  CK_C_DestroyObject: C_DestroyObject;
  CK_C_GetObjectSize: C_GetObjectSize;
  CK_C_GetAttributeValue: C_GetAttributeValue;
  CK_C_SetAttributeValue: C_SetAttributeValue;
  CK_C_FindObjectsInit: C_FindObjectsInit;
  CK_C_FindObjects: C_FindObjects;
  CK_C_FindObjectsFinal: C_FindObjectsFinal;
  CK_C_EncryptInit: C_EncryptInit;
  CK_C_Encrypt: C_Encrypt;
  CK_C_EncryptUpdate: C_EncryptUpdate;
  CK_C_EncryptFinal: C_EncryptFinal;
  CK_C_DecryptInit: C_DecryptInit;
  CK_C_Decrypt: C_Decrypt;
  CK_C_DecryptUpdate: C_DecryptUpdate;
  CK_C_DecryptFinal: C_DecryptFinal;
  CK_C_DigestInit: C_DigestInit;
  CK_C_Digest: C_Digest;
  CK_C_DigestUpdate: C_DigestUpdate;
  CK_C_DigestKey: C_DigestKey;
  CK_C_DigestFinal: C_DigestFinal;
  CK_C_SignInit: C_SignInit;
  CK_C_Sign: C_Sign;
  CK_C_SignUpdate: C_SignUpdate;
  CK_C_SignFinal: C_SignFinal;
  CK_C_SignRecoverInit: C_SignRecoverInit;
  CK_C_SignRecover: C_SignRecover;
  CK_C_VerifyInit: C_VerifyInit;
  CK_C_Verify: C_Verify;
  CK_C_VerifyUpdate: C_VerifyUpdate;
  CK_C_VerifyFinal: C_VerifyFinal;
  CK_C_VerifyRecoverInit: C_VerifyRecoverInit;
  CK_C_VerifyRecover: C_VerifyRecover;
  CK_C_DigestEncryptUpdate: C_DigestEncryptUpdate;
  CK_C_DecryptDigestUpdate: C_DecryptDigestUpdate;
  CK_C_SignEncryptUpdate: C_SignEncryptUpdate;
  CK_C_DecryptVerifyUpdate: C_DecryptVerifyUpdate;
  CK_C_GenerateKey: C_GenerateKey;
  CK_C_GenerateKeyPair: C_GenerateKeyPair;
  CK_C_WrapKey: C_WrapKey;
  CK_C_UnwrapKey: C_UnwrapKey;
  CK_C_DeriveKey: C_DeriveKey;
  CK_C_SeedRandom: C_SeedRandom;
  CK_C_GenerateRandom: C_GenerateRandom;
  CK_C_GetFunctionStatus: C_GetFunctionStatus;
  CK_C_CancelFunction: C_CancelFunction;
  CK_C_WaitForSlotEvent: C_WaitForSlotEvent;
end;


implementation

END.

