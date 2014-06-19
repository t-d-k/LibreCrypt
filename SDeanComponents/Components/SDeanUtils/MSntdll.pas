unit MSntdll;

interface

uses
  Windows;

type

  // Note: ULONG defined in unit "Windows"
  USHORT = Word;
  NTSTATUS = DWORD;

const
  STATUS_SUCCESS = 0;

type

(*
typedef struct _LSA_UNICODE_STRING {
  USHORT Length;
  USHORT MaximumLength;
  PWSTR Buffer;
} LSA_UNICODE_STRING, 
 *PLSA_UNICODE_STRING, 
 UNICODE_STRING, 
 *PUNICODE_STRING;
*)
TUNICODE_STRING = packed record
  Length: USHORT;
  MaximumLength: USHORT;
  Buffer: PWideChar;
end;
PUNICODE_STRING = ^TUNICODE_STRING;

(*
typedef struct _OBJECT_ATTRIBUTES {
    ULONG  Length;
    HANDLE  RootDirectory;
    PUNICODE_STRING  ObjectName;
    ULONG  Attributes;
    PVOID  SecurityDescriptor;
    PVOID  SecurityQualityOfService;
} OBJECT_ATTRIBUTES, *POBJECT_ATTRIBUTES;
typedef CONST OBJECT_ATTRIBUTES *PCOBJECT_ATTRIBUTES;
*)
TOBJECT_ATTRIBUTES = packed record
  Length: ULONG;
  RootDirectory: THandle;
  ObjectName: PUNICODE_STRING;
  Attributes: ULONG;
  SecurityDescriptor: Pointer;
  SecurityQualityOfService: Pointer;
end;
POBJECT_ATTRIBUTES = ^TOBJECT_ATTRIBUTES;

const
  OBJ_INHERIT            = $00000002;
  OBJ_PERMANENT          = $00000010;
  OBJ_EXCLUSIVE          = $00000020;
  OBJ_CASE_INSENSITIVE   = $00000040;
  OBJ_OPENIF             = $00000080;
  OBJ_OPENLINK           = $00000100;
//  OBJ_KERNEL_HANDLE 	   = $
//  OBJ_FORCE_ACCESS_CHECK = $
  OBJ_VALID_ATTRIBUTES   = $000001F2;

type
TOBJECT_DIRECTORY_INFORMATION = packed record
  ObjectName: TUNICODE_STRING;
  ObjectTypeName: TUNICODE_STRING;
  Data: array [0..0] of byte;
end;
POBJECT_DIRECTORY_INFORMATION = ^TOBJECT_DIRECTORY_INFORMATION;
//OBJECT_DIRECTORY_INFORMATION = TOBJECT_DIRECTORY_INFORMATION;



(*
VOID RtlInitUnicodeString(
    PUNICODE_STRING DestinationString,
    PCWSTR SourceString
);
*)
{$IFDEF _NTDLL_DYNAMIC}
type
TRtlInitUnicodeString = procedure(DestinationString: PUNICODE_STRING; SourceString: PWideChar); stdcall;
var
RtlInitUnicodeString: TRtlInitUnicodeString;
{$ELSE}
procedure RtlInitUnicodeString(DestinationString: PUNICODE_STRING; SourceString: PWideChar); stdcall;
{$EXTERNALSYM RtlInitUnicodeString}
{$ENDIF}


(*
NTSTATUS WINAPI NtOpenDirectoryObject(
  __out  PHANDLE DirectoryHandle,
  __in   ACCESS_MASK DesiredAccess,
  __in   POBJECT_ATTRIBUTES ObjectAttributes
);
*)
{$IFDEF _NTDLL_DYNAMIC}
type
TNtOpenDirectoryObject = function(
  DirectoryHandle: PHandle;
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES
): NTSTATUS; stdcall;
var
NtOpenDirectoryObject: TNtOpenDirectoryObject;
{$ELSE}
function NtOpenDirectoryObject(
  DirectoryHandle: PHandle;
  DesiredAccess: ACCESS_MASK;
  ObjectAttributes: POBJECT_ATTRIBUTES
): NTSTATUS; stdcall;
{$EXTERNALSYM NtOpenDirectoryObject}
{$ENDIF}

const
  DIRECTORY_QUERY               = $0001;
  DIRECTORY_TRAVERSE            = $0002;
  DIRECTORY_CREATE_OBJECT       = $0004;
  DIRECTORY_CREATE_SUBDIRECTORY = $0008;
  DIRECTORY_ALL_ACCESS          = (STANDARD_RIGHTS_REQUIRED or $F);

  SYMBOLIC_LINK_QUERY      = $0001;
  SYMBOLIC_LINK_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or $1);


(*
NTSTATUS WINAPI NtQueryDirectoryObject(
  __in       HANDLE DirectoryHandle,
  __out_opt  PVOID Buffer,
  __in       ULONG Length,
  __in       BOOLEAN ReturnSingleEntry,
  __in       BOOLEAN RestartScan,
  __inout    PULONG Context,
  __out_opt  PULONG ReturnLength
);
*)
{$IFDEF _NTDLL_DYNAMIC}
type
TNtQueryDirectoryObject = function(
  DirectoryHandle: THandle;
  Buffer: Pointer;
  Length: ULONG;
  ReturnSingleEntry: BOOLEAN;
  RestartScan: boolean;
  Context: PULONG;
  ReturnLength: PULONG
): NTSTATUS; stdcall;
var
NtQueryDirectoryObject: TNtQueryDirectoryObject;
{$ELSE}
function NtQueryDirectoryObject(
  DirectoryHandle: THandle;
  Buffer: Pointer;
  Length: ULONG;
  ReturnSingleEntry: BOOLEAN;
  RestartScan: boolean;
  Context: PULONG;
  ReturnLength: PULONG
): NTSTATUS; stdcall;
{$EXTERNALSYM NtQueryDirectoryObject}
{$ENDIF}

(*
NTSTATUS WINAPI NtOpenSymbolicLinkObject(
  __out  PHANDLE LinkHandle,
  __in   ACCESS_MASK DesiredAccess,
  __in   POBJECT_ATTRIBUTES ObjectAttributes
);
*)
{$IFDEF _NTDLL_DYNAMIC}
TNtOpenSymbolicLinkObject = function(
  LinkHandle: PHANDLE;
  DesiredAccess: ACCESS_MASK;
  ObjectAttribute: POBJECT_ATTRIBUTES
): NTSTATUS; stdcall;
var
NtOpenSymbolicLinkObject = TNtOpenSymbolicLinkObject;
{$ELSE}
function NtOpenSymbolicLinkObject(
  LinkHandle: PHANDLE;
  DesiredAccess: ACCESS_MASK;
  ObjectAttribute: POBJECT_ATTRIBUTES
): NTSTATUS; stdcall;
{$EXTERNALSYM NtOpenSymbolicLinkObject}
{$ENDIF}

type
  TACCESS_MASK = DWORD;
  PACCESS_MASK = ^TACCESS_MASK;

const
  DELETE                           = $00010000;
  READ_CONTROL                     = $00020000;
  WRITE_DAC                        = $00040000;
  WRITE_OWNER                      = $00080000;
  SYNCHRONIZE                      = $00100000;

  STANDARD_RIGHTS_REQUIRED         = $000F0000;

  STANDARD_RIGHTS_READ             = READ_CONTROL;
  STANDARD_RIGHTS_WRITE            = READ_CONTROL;
  STANDARD_RIGHTS_EXECUTE          = READ_CONTROL;

  STANDARD_RIGHTS_ALL              = $001F0000;

  SPECIFIC_RIGHTS_ALL              = $0000FFFF;



(*
NTSTATUS WINAPI NtQuerySymbolicLinkObject(
  __in       HANDLE LinkHandle,
  __inout    PUNICODE_STRING LinkTarget,
  __out_opt  PULONG ReturnedLength
);
*)
{$IFDEF _NTDLL_DYNAMIC}
type
TNtQuerySymbolicLinkObject = function(
  LinkHandle: THandle;
  LinkTarget: PUNICODE_STRING;
  ReturnedLengt: PULONG
): NTSTATUS; stdcall;
var
NtQuerySymbolicLinkObject = TNtQuerySymbolicLinkObject;
{$ELSE}
function NtQuerySymbolicLinkObject(
  LinkHandle: THandle;
  LinkTarget: PUNICODE_STRING;
  ReturnedLengt: PULONG
): NTSTATUS; stdcall;
{$EXTERNALSYM NtQuerySymbolicLinkObject}
{$ENDIF}

(*
NTSTATUS NtClose(
    HANDLE Handle
);
*)
{$IFDEF _NTDLL_DYNAMIC}
type
TNtClose = function(
  Handle: THandle
): NTSTATUS; stdcall;
var
NtClose = TNtClose;
{$ELSE}
function NtClose(
  Handle: THandle
): NTSTATUS; stdcall;
{$EXTERNALSYM NtClose}
{$ENDIF}


(*
VOID
  InitializeObjectAttributes(
    OUT POBJECT_ATTRIBUTES  InitializedAttributes,
    IN PUNICODE_STRING  ObjectName,
    IN ULONG  Attributes,
    IN HANDLE  RootDirectory,
    IN PSECURITY_DESCRIPTOR  SecurityDescriptor
    );
*)
{$IFDEF _NTDLL_DYNAMIC}
type
TInitializeObjectAttributes = procedure(
    InitializedAttributes: POBJECT_ATTRIBUTES;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    RootDirectory: THandle;
    SecurityDescriptor: PSECURITY_DESCRIPTOR
);
var
InitializeObjectAttributes: TInitializeObjectAttributes;
{$ELSE}
procedure InitializeObjectAttributes(
    InitializedAttributes: POBJECT_ATTRIBUTES;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    RootDirectory: THandle;
    SecurityDescriptor: PSECURITY_DESCRIPTOR
);
{$ENDIF}

function NT_SUCCESS(status: NTSTATUS): boolean;

// Allocate memory for, and populate a UNICODE_STRING structure
// Note: Memory allocated *must* be free'd off by called; e.g. by calling FreeRtlInitUnicodeString(...)
procedure AllocRtlInitUnicodeString(DestinationString: PUNICODE_STRING; SourceString: WideString);
procedure FreeRtlInitUnicodeString(StringToFree: PUNICODE_STRING);

function UNICODE_STRINGToWideString(inStr: TUNICODE_STRING): WideString;


implementation

uses
  SysUtils;

const
  ntdll = 'ntdll.dll';

{$IFDEF _NTDLL_DYNAMIC}
var
  hLibNtDLL: THandle;
{$ELSE}
{$ENDIF}

{$IFDEF _NTDLL_DYNAMIC}
{$ELSE}
procedure RtlInitUnicodeString; external ntdll name 'RtlInitUnicodeString';
function NtOpenDirectoryObject; external ntdll name 'NtOpenDirectoryObject';
function NtQueryDirectoryObject; external ntdll name 'NtQueryDirectoryObject';
function NtOpenSymbolicLinkObject; external ntdll name 'NtOpenSymbolicLinkObject';
function NtQuerySymbolicLinkObject; external ntdll name 'NtQuerySymbolicLinkObject';
function NtClose; external ntdll name 'NtClose';
{$ENDIF}


function NT_SUCCESS(status: NTSTATUS): boolean;
begin
  Result := (status = STATUS_SUCCESS);
end;


procedure AllocRtlInitUnicodeString(DestinationString: PUNICODE_STRING; SourceString: WideString);
var
  newString: PWideChar;
begin
  newString := AllocMem((length(SourceString) + 1)* sizeof(newString^));
  CopyMemory(newstring, PWideString(SourceString), (length(SourceString) * sizeof(newString^)) );
  newString[length(SourceString)] := #0;
  RtlInitUnicodeString(DestinationString, newString);
end;

procedure FreeRtlInitUnicodeString(StringToFree: PUNICODE_STRING);
begin
  StringToFree.Length := 0;
  StringToFree.MaximumLength := 0;
  FreeMem(StringToFree.Buffer);
  StringToFree.Buffer := nil;
end;


function UNICODE_STRINGToWideString(inStr: TUNICODE_STRING): WideString;
var
  retval: WideString;
  i: integer;
begin
  retval := '';
  // -1 because index from 0
  for i:=0 to ((inStr.Length div 2) - 1) do
    begin
    retval := retval + inStr.Buffer[i];
    end;

  Result := retval;
end;

procedure  InitializeObjectAttributes(
    InitializedAttributes: POBJECT_ATTRIBUTES;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    RootDirectory: THandle;
    SecurityDescriptor: PSECURITY_DESCRIPTOR
);
begin
  InitializedAttributes.Length := sizeof(InitializedAttributes^);
  InitializedAttributes.Attributes := Attributes;
  InitializedAttributes.ObjectName := ObjectName;
  InitializedAttributes.RootDirectory := RootDirectory;
  InitializedAttributes.SecurityDescriptor := SecurityDescriptor;
  InitializedAttributes.SecurityQualityOfService := nil;

end;

{$IFDEF _NTDLL_DYNAMIC}
initialization
  hLibNtDLL := LoadLibrary(ntdll);

  @RtlInitUnicodeString       := 0;
  @NtOpenDirectoryObject      := 0;
  @NtQueryDirectoryObject     := 0;
  @NtOpenSymbolicLinkObject   := 0;
  @NtQuerySymbolicLinkObject  := 0;
  @NtClose                    := 0;
  @InitializeObjectAttributes := 0;
  if (hLibNtDLL <> 0) then
    begin
    @RtlInitUnicodeString       := GetProcAddress(hLibNtDLL, 'RtlInitUnicodeString');
    @NtOpenDirectoryObject      := GetProcAddress(hLibNtDLL, 'NtOpenDirectoryObject');
    @NtQueryDirectoryObject     := GetProcAddress(hLibNtDLL, 'NtQueryDirectoryObject');
    @NtOpenSymbolicLinkObject   := GetProcAddress(hLibNtDLL, 'NtOpenSymbolicLinkObject');
    @NtQuerySymbolicLinkObject  := GetProcAddress(hLibNtDLL, 'NtQuerySymbolicLinkObject');
    @NtClose                    := GetProcAddress(hLibNtDLL, 'NtClose');
    @InitializeObjectAttributes := GetProcAddress(hLibNtDLL, 'InitializeObjectAttributes');
    end;

finalization
  if (hLibNtDLL <> 0) then
    begin
    FreeLibrary(hLibNtDLL);
    end;

{$ELSE}
{$ENDIF}

END.

