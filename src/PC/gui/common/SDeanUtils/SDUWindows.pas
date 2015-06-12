unit SDUWindows;
{ extra windows functions}
interface

uses
  Windows;

type
  POSVersionInfoExA = ^TOSVersionInfoExA;
  POSVersionInfoExW = ^TOSVersionInfoExW;
  POSVersionInfoEx = POSVersionInfoExA;
  _OSVERSIONINFOEXA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance string for PSS usage }
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  {$EXTERNALSYM _OSVERSIONINFOEXA}
  _OSVERSIONINFOEXW = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of WideChar; { Maintenance string for PSS usage }
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved: BYTE;
  end;
  {$EXTERNALSYM _OSVERSIONINFOEXW}
  _OSVERSIONINFOEX = _OSVERSIONINFOEXA;
  TOSVersionInfoExA = _OSVERSIONINFOEXA;
  TOSVersionInfoExW = _OSVERSIONINFOEXW;
  TOSVersionInfoEx = TOSVersionInfoExA;
  OSVERSIONINFOEXA = _OSVERSIONINFOEXA;
  {$EXTERNALSYM OSVERSIONINFOEXA}
  {$EXTERNALSYM OSVERSIONINFOEX}
  OSVERSIONINFOEXW = _OSVERSIONINFOEXW;
  {$EXTERNALSYM OSVERSIONINFOEXW}
  {$EXTERNALSYM OSVERSIONINFOEX}
  OSVERSIONINFOEX = OSVERSIONINFOEXA;

const
  VER_NT_WORKSTATION              = $0000001;
  {$EXTERNALSYM VER_NT_WORKSTATION}
  VER_NT_DOMAIN_CONTROLLER        = $0000002;
  {$EXTERNALSYM VER_NT_DOMAIN_CONTROLLER}
  VER_NT_SERVER                   = $0000003;
  {$EXTERNALSYM VER_NT_SERVER}

function SDUGetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall;
{$EXTERNALSYM SDUGetVersionEx}
function SDUGetVersionExA(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall;
{$EXTERNALSYM SDUGetVersionExA}
function SDUGetVersionExW(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall;
{$EXTERNALSYM SDUGetVersionExW}


implementation

function SDUGetVersionEx; external kernel32 name 'GetVersionExA';
function SDUGetVersionExA; external kernel32 name 'GetVersionExA';
function SDUGetVersionExW; external kernel32 name 'GetVersionExW';

end.



