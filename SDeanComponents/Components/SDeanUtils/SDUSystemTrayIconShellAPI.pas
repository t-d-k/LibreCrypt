unit SDUSystemTrayIconShellAPI;
// Description: Shell API functions relating to tasktray Icon
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows,
  Messages;

// ------------------------------------
// All versions of MS Windows
type
  // Ripped from Delphi's ShellAPI.pas, in case they update to support shell v5
  // struct
  PNotifyIconData_v1A = ^TNotifyIconData_v1A;
  PNotifyIconData_v1W = ^TNotifyIconData_v1W;
  PNotifyIconData_v1 = PNotifyIconData_v1A;
  {$EXTERNALSYM _NOTIFYICONDATA_v1A}
  _NOTIFYICONDATA_v1A = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..63] of AnsiChar;
  end;
  {$EXTERNALSYM _NOTIFYICONDATA_v1W}
  _NOTIFYICONDATA_v1W = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..63] of WideChar;
  end;
  {$EXTERNALSYM _NOTIFYICONDATA_v1}
  _NOTIFYICONDATA_v1 = _NOTIFYICONDATA_v1A;
  TNotifyIconData_v1A = _NOTIFYICONDATA_v1A;
  TNotifyIconData_v1W = _NOTIFYICONDATA_v1W;
  TNotifyIconData_v1 = TNotifyIconData_v1A;
  {$EXTERNALSYM NOTIFYICONDATA_v1A}
  NOTIFYICONDATA_v1A = _NOTIFYICONDATA_v1A;
  {$EXTERNALSYM NOTIFYICONDATA_v1W}
  NOTIFYICONDATA_v1W = _NOTIFYICONDATA_v1W;
  {$EXTERNALSYM NOTIFYICONDATA_v1}
  NOTIFYICONDATA_v1 = NOTIFYICONDATA_v1A;

const
  NOTIFYICONDATA_V1_SIZE = sizeof(TNotifyIconData_v1);

  
// ------------------------------------
// MS Windows 2000 and later
type
  // Shell v2 struct
  // Based on MSDN definition of NOTIFYICONDATA structure
  TTimeoutVersion = record
    case boolean of
      TRUE:  (uTimeout: UINT);
      FALSE: (uVersion: UINT)
  end;

  PNotifyIconData_v2A = ^TNotifyIconData_v2A;
  PNotifyIconData_v2W = ^TNotifyIconData_v2W;
  PNotifyIconData_v2 = PNotifyIconData_v2A;
  {$EXTERNALSYM _NOTIFYICONDATA_v2A}
  _NOTIFYICONDATA_v2A = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of AnsiChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of AnsiChar;
    TimeoutVersion: TTimeoutVersion;  // Union
    szInfoTitle: array [0..63] of AnsiChar;
    dwInfoFlags: DWORD;
  end;
  {$EXTERNALSYM _NOTIFYICONDATA_v2W}
  _NOTIFYICONDATA_v2W = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of WideChar;
    TimeoutVersion: TTimeoutVersion;  // Union
    szInfoTitle: array [0..63] of WideChar;
    dwInfoFlags: DWORD;
  end;
  {$EXTERNALSYM _NOTIFYICONDATA_v2}
  _NOTIFYICONDATA_v2 = _NOTIFYICONDATA_v2A;
  TNotifyIconData_v2A = _NOTIFYICONDATA_v2A;
  TNotifyIconData_v2W = _NOTIFYICONDATA_v2W;
  TNotifyIconData_v2 = TNotifyIconData_v2A;
  {$EXTERNALSYM NOTIFYICONDATA_v2A}
  NOTIFYICONDATA_v2A = _NOTIFYICONDATA_v2A;
  {$EXTERNALSYM NOTIFYICONDATA_v2W}
  NOTIFYICONDATA_v2W = _NOTIFYICONDATA_v2W;
  {$EXTERNALSYM NOTIFYICONDATA_v2}
  NOTIFYICONDATA_v2 = NOTIFYICONDATA_v2A;

const
  NOTIFYICONDATA_V2_SIZE = sizeof(TNotifyIconData_v2);


// ------------------------------------
// MS Windows XP SP-?? and later
type
  // Shell v5 struct
  // Based on MSDN definition of NOTIFYICONDATA structure
  PNotifyIconData_v5A = ^TNotifyIconData_v5A;
  PNotifyIconData_v5W = ^TNotifyIconData_v5W;
  PNotifyIconData_v5 = PNotifyIconData_v5A;
  {$EXTERNALSYM _NOTIFYICONDATA_v5A}
  _NOTIFYICONDATA_v5A = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of AnsiChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of AnsiChar;
    TimeoutVersion: TTimeoutVersion;  // Union
    szInfoTitle: array [0..63] of AnsiChar;
    dwInfoFlags: DWORD;
    guidItem: TGUID;
  end;

  {$EXTERNALSYM _NOTIFYICONDATA_v5W}
  _NOTIFYICONDATA_v5W = record
    cbSize: DWORD;
    hWnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of WideChar;
    dwState: DWORD;
    dwStateMask: DWORD;
    szInfo: array [0..255] of WideChar;
    TimeoutVersion: TTimeoutVersion;  // Union
    szInfoTitle: array [0..63] of WideChar;
    dwInfoFlags: DWORD;
    guidItem: TGUID;
  end;
  {$EXTERNALSYM _NOTIFYICONDATA_v5}
  _NOTIFYICONDATA_v5 = _NOTIFYICONDATA_v5A;
  TNotifyIconData_v5A = _NOTIFYICONDATA_v5A;
  TNotifyIconData_v5W = _NOTIFYICONDATA_v5W;
  TNotifyIconData_v5 = TNotifyIconData_v5A;
  {$EXTERNALSYM NOTIFYICONDATA_v5A}
  NOTIFYICONDATA_v5A = _NOTIFYICONDATA_v5A;
  {$EXTERNALSYM NOTIFYICONDATA_v5W}
  NOTIFYICONDATA_v5W = _NOTIFYICONDATA_v5W;
  {$EXTERNALSYM NOTIFYICONDATA_v5}
  NOTIFYICONDATA_v5 = NOTIFYICONDATA_v5A;

const
  NOTIFYICONDATA_V5_SIZE = sizeof(TNotifyIconData_v5);


const
  // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/shell/reference/structures/notifyicondata.asp
  {$EXTERNALSYM NIM_ADD}
  NIM_ADD         = $00000000;
  {$EXTERNALSYM NIM_MODIFY}
  NIM_MODIFY      = $00000001;
  {$EXTERNALSYM NIM_DELETE}
  NIM_DELETE      = $00000002;
  {$EXTERNALSYM NIM_SETFOCUS}
  NIM_SETFOCUS    = $00000003;
  {$EXTERNALSYM NIM_SETVERSION}
  NIM_SETVERSION  = $00000004;

  
  {$EXTERNALSYM NIF_ICON}
  NIF_ICON    = $00000002;  // The hIcon member is valid.
  {$EXTERNALSYM NIF_MESSAGE}
  NIF_MESSAGE = $00000001;  // The uCallbackMessage member is valid.
  {$EXTERNALSYM NIF_TIP}
  NIF_TIP     = $00000004;  // The szTip member is valid.
  {$EXTERNALSYM NIF_STATE}
  NIF_STATE   = $00000008;  // The dwState and dwStateMask members are valid.
  {$EXTERNALSYM NIF_INFO}
  NIF_INFO    = $00000010;  // Use a balloon ToolTip instead of a standard ToolTip. The szInfo, uTimeout, szInfoTitle, and dwInfoFlags members are valid.
  {$EXTERNALSYM NIF_GUID}
  NIF_GUID    = $00000020;  // Reserved.

  {$EXTERNALSYM NIS_HIDDEN}
  NIS_HIDDEN     = $00000001;
  {$EXTERNALSYM NIS_SHAREDICON}
  NIS_SHAREDICON = $00000002;

  {$EXTERNALSYM NOTIFYICON_OLDVERSION}
  NOTIFYICON_OLDVERSION = $00000000;
  {$EXTERNALSYM NOTIFYICON_VERSION}
  NOTIFYICON_VERSION    = $00000003;

  // Based on MSDN definition of NOTIFYICONDATA structure
  {$EXTERNALSYM NIIF_ERROR}
  NIIF_ERROR     = $00000003;  // An error icon.
  {$EXTERNALSYM NIIF_INFO}
  NIIF_INFO      = $00000001;  // An information icon.
  {$EXTERNALSYM NIIF_NONE}
  NIIF_NONE      = $00000000;  // No icon.
//  {$EXTERNALSYM NIIF_USER}
//  NIIF_USER      = $???;  // Windows XP Service Pack 2 (SP2) and later. Use the icon identified in hIcon as the notification balloon's title icon.
  {$EXTERNALSYM NIIF_WARNING}
  NIIF_WARNING   = $00000002;  // A warning icon.
  {$EXTERNALSYM NIIF_ICON_MASK}
  NIIF_ICON_MASK = $0000000F;  // Version 6.0. Reserved.
  {$EXTERNALSYM NIIF_NOSOUND}
  NIIF_NOSOUND   = $00000010;  // Version 6.0. Do not play the associated sound. Applies only to balloon ToolTips.


  {$EXTERNALSYM NINF_KEY }
  NINF_KEY             = $00000001;
  {$EXTERNALSYM NIN_BALLOONSHOW}
  NIN_BALLOONSHOW      = WM_USER + 2;  // Sent when the balloon is shown (balloons are queued).
  {$EXTERNALSYM NIN_BALLOONHIDE}
  NIN_BALLOONHIDE      = WM_USER + 3;  // Sent when the balloon disappears—when the icon is deleted, for example. This message is not sent if the balloon is dismissed because of a timeout or mouse click by the user.
  {$EXTERNALSYM NIN_BALLOONTIMEOUT}
  NIN_BALLOONTIMEOUT   = WM_USER + 4;  // Sent when the balloon is dismissed because of a timeout.
  {$EXTERNALSYM NIN_BALLOONUSERCLICK}
  NIN_BALLOONUSERCLICK = WM_USER + 5;  // Sent when the balloon is dismissed because the user clicked the mouse.
  {$EXTERNALSYM NIN_SELECT}
  NIN_SELECT           = WM_USER + 0;
  {$EXTERNALSYM NIN_KEYSELECT}
  NIN_KEYSELECT        = NIN_SELECT or NINF_KEY;

{$EXTERNALSYM Shell_NotifyIcon}
function Shell_NotifyIcon(dwMessage: DWORD; lpData: PNotifyIconData_v5): BOOL; stdcall;
{$EXTERNALSYM Shell_NotifyIconA}
function Shell_NotifyIconA(dwMessage: DWORD; lpData: PNotifyIconData_v5A): BOOL; stdcall;
{$EXTERNALSYM Shell_NotifyIconW}
function Shell_NotifyIconW(dwMessage: DWORD; lpData: PNotifyIconData_v5W): BOOL; stdcall;


const
{$IFDEF LINUX}
  shell32 = 'libshell32.borland.so';
{$ELSE}
  shell32 = 'shell32.dll';
{$ENDIF}




implementation

function Shell_NotifyIcon; external shell32 name 'Shell_NotifyIconA';
function Shell_NotifyIconA; external shell32 name 'Shell_NotifyIconA';
function Shell_NotifyIconW; external shell32 name 'Shell_NotifyIconW';

END.



