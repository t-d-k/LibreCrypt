unit SDUDialogs;

// Note: In order to use this, need XP Manifest in your application
// See also: TaskDialogIndirect(...) and TASKDIALOGCONFIG!!!
// msg boxes fns moved to lcDialogs

interface

uses
  Dialogs,
  Windows;
 (*
const
  // Consts for use with Vista TaskDialogs
  // Taken from CommCtrl.h, Vista SDK
  SDVISTA_TDCBF_OK_BUTTON     = $0001;
  SDVISTA_TDCBF_YES_BUTTON    = $0002;
  SDVISTA_TDCBF_NO_BUTTON     = $0004;
  SDVISTA_TDCBF_CANCEL_BUTTON = $0008;
  SDVISTA_TDCBF_RETRY_BUTTON  = $0010;
  SDVISTA_TDCBF_CLOSE_BUTTON  = $0020;

  // Taken from WinUser.h, Vista SDK:
  //   #define MAKEINTRESOURCEW(i) ((LPWSTR)((ULONG_PTR)((WORD)(i))))
  // Note: We do ***NOT*** use MAKEINTRESOURCE here as Delphi's version of this
  //       doesn't cast to a WORD (16 bit), hence doesn't truncate it before
  //       converting to PWChar - we manually expand out the C version's macro
  // Taken from CommCtrl.h, Vista SDK
  //   #define TD_WARNING_ICON         MAKEINTRESOURCEW(-1)
  SDVISTA_TD_WARNING_ICON     = PWChar(WORD(-1));
  //   #define TD_ERROR_ICON           MAKEINTRESOURCEW(-2)
  SDVISTA_TD_ERROR_ICON       = PWChar(WORD(-2));
  //   #define TD_INFORMATION_ICON     MAKEINTRESOURCEW(-3)
  SDVISTA_TD_INFORMATION_ICON = PWChar(WORD(-3));
  //   #define TD_SHIELD_ICON          MAKEINTRESOURCEW(-4)
  SDVISTA_TD_SHIELD_ICON      = PWChar(WORD(-4));


  SDVISTA_TD_ICON_BLANK       = PWChar(WORD(0));
  SDVISTA_TD_ICON_WARNING     = SDVISTA_TD_WARNING_ICON;
  SDVISTA_TD_ICON_QUESTION    = PWChar(IDI_QUESTION);  // Not official!
                                                       // Not in the Vista SDK!
  SDVISTA_TD_ICON_ERROR       = SDVISTA_TD_ERROR_ICON;
  SDVISTA_TD_ICON_INFORMATION = SDVISTA_TD_INFORMATION_ICON;
//  SDVISTA_TD_ICON_BLANK_AGAIN = MAKEINTRESOURCEW(0);
  SDVISTA_TD_ICON_SHIELD      = SDVISTA_TD_SHIELD_ICON;

{
  // Consts from: http://weblogs.foxite.com/stuartdunkeld/archive/2006/05/23/1570.aspx
  SDVISTA_TD_ICON_BLANK       = 100;
  SDVISTA_TD_ICON_WARNING     = 101;
  SDVISTA_TD_ICON_QUESTION    = 102;
  SDVISTA_TD_ICON_ERROR       = 103;
  SDVISTA_TD_ICON_INFORMATION = 104;
  SDVISTA_TD_ICON_BLANK_AGAIN = 105;
  SDVISTA_TD_ICON_SHIELD      = 106;
}

{
  // Consts from: http://www.delphi-forum.de/viewtopic.php?p=404545
  SDVISTA_TD_ICON_BLANK       = PWChar(32512);
  SDVISTA_TD_ICON_WARNING     = PWChar(32515);
  SDVISTA_TD_ICON_QUESTION    = PWChar(32514);
  SDVISTA_TD_ICON_ERROR       = PWChar(32513);
  SDVISTA_TD_ICON_INFORMATION = PWChar(32516);
  SDVISTA_TD_ICON_BLANK_AGAIN = PWChar(32517);
  SDVISTA_TD_ICON_SHIELD      = PWChar(32518);
}

                *)
 type
{these dialogs just don't change working dir if PreserveCWD is set}
  TSDUOpenDialog = class(TOpenDialog)
  private
    FPreserveCWD: boolean;
  public
    function Execute(): boolean; override;
  published
    property PreserveCWD: boolean read FPreserveCWD write FPreserveCWD default TRUE;
  end;

  TSDUSaveDialog = class(TSaveDialog)
  private
    FPreserveCWD: boolean;
  public
    function Execute(): boolean; override;
  published
    property PreserveCWD: boolean read FPreserveCWD write FPreserveCWD default TRUE;
  end;

procedure Register;

//var
//  // If set, then any messages displayed will have any single CRLFs stripped
//  // out, while double CRLFs (SDU_CRLF+SDUCRLF) will be preserved under
//  // Windows Vista and later
//  // This allows messages to have CRLFs included in them for pre-Vista systems,
//  // in order to break up long lines - but on Vista, which does more sensible
//  // word wrapping, such CRLFs will be removed
//  SDUDialogsStripSingleCRLF: boolean = TRUE;

  // set to true to suppress all dialogs (ege for testing)
//  G_SuppressDialogs : boolean = False;
 (*
// Display confirmation dialog with OK/Cancel or Yes/No buttons.
// Returns TRUE if the user selects OK or Yes.
function SDUConfirmOK(msg: string): boolean;
function SDUConfirmYN(msg: string): boolean;
function SDUWarnOK(msg: string): boolean;
function SDUWarnYN(msg: string): boolean;
function SDUErrorOK(msg: string): boolean;
function SDUErrorYN(msg: string): boolean;

// function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: UINT): Integer; stdcall;
function SDUMessageBox(
  hWnd: HWND;
  Content: string;
  WindowTitle: string;
  Flags: longint
): integer;

// function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
//   Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
function SDUMessageDlg(
  Content: string;
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  HelpCtx: Longint
): integer; overload;

function SDUMessageDlg(
  Content: string;
  DlgType: TMsgDlgType
): integer; overload;

function SDUMessageDlg(
  Content: string
): integer; overload;

function SDUVistaTaskDialog(
  WindowTitle: WideString;
  MainInstruction: WideString;
  Content: WideString;
  CommonButtons: integer;
  Icon: PWChar
): integer; overload;

function SDUVistaTaskDialog(
  hWndParent: HWND;
  WindowTitle: WideString;
  MainInstruction: WideString;
  Content: WideString;
  CommonButtons: integer;
  Icon: PWChar
): integer; overload;

function SDUVistaTaskDialog(
  hWndParent: HWND;
  hInst: THandle;
  WindowTitle: WideString;
  MainInstruction: WideString;
  Content: WideString;
  CommonButtons: integer;
  Icon: PWChar
): integer; overload;
              *)

implementation

uses
  Classes,
  Controls,  // Required for mrXXX
//  Consts,
//  RTLConsts,
  Forms, // Required for Application
  SDUGeneral,
  SysUtils;  // Required for StringReplace

(*
resourcestring

// Ugly hack to prevent:
//   [Pascal Error] E2201 Need imported data reference ($G) to access 'SMsgDlgInformation' from unit 'SDUDialogs'
// compiler error; copy of consts from "Consts.pas"
  SDU_SMsgDlgWarning      = 'Warning';
  SDU_SMsgDlgError        = 'Error';
  SDU_SMsgDlgInformation  = 'Information';
  SDU_SMsgDlgConfirm      = 'Confirm';

  // These are included, just so that they get included in any .mo
  // (translation) file. Otherwise these are not used anywhere - Delphi's
  // "Consts.pas" holds the ones actually used, but they don't get extracted
  // to .mo files; hence including a copy here
  SDU_SMsgDlgYes      = '&Yes';
  SDU_SMsgDlgNo       = '&No';
  SDU_SMsgDlgOK       = 'OK';
  SDU_SMsgDlgCancel   = 'Cancel';
  SDU_SMsgDlgHelp     = '&Help';
  SDU_SMsgDlgHelpNone = 'No help available';
  SDU_SMsgDlgHelpHelp = 'Help';
  SDU_SMsgDlgAbort    = '&Abort';
  SDU_SMsgDlgRetry    = '&Retry';
  SDU_SMsgDlgIgnore   = '&Ignore';
  SDU_SMsgDlgAll      = '&All';
  SDU_SMsgDlgNoToAll  = 'N&o to All';
  SDU_SMsgDlgYesToAll = 'Yes to &All';

const
  // The filename of the DLL
  COMCTL32_LIB_DLL = 'COMCTL32.DLL';

  DLL_FUNCTIONNAME_TaskDialog = 'TaskDialog';
             *)
  (*
type
  // Library function definition
  // From CommCtrl.h, Vista SDK
  //  WINCOMMCTRLAPI HRESULT WINAPI TaskDialog(
  //    __in_opt HWND hwndParent,
  //    __in_opt HINSTANCE hInstance,
  //    __in_opt PCWSTR pszWindowTitle,
  //    __in_opt PCWSTR pszMainInstruction,
  //    __in_opt PCWSTR pszContent,
  //    TASKDIALOG_COMMON_BUTTON_FLAGS dwCommonButtons,
  //    __in_opt PCWSTR pszIcon,
  //    __out_opt int *pnButton
  //    );
  TFnTaskDialog = function(
     hWndParent: HWND;
     hInstance: THandle;
     pszWindowTitle: PWChar;
     pszMainInstruction: PWChar;
     pszContent: PWChar;
     CommonButtons: Integer;
     pszIcon: PWChar;
     pnButton: PInteger
   ): HRESULT; stdcall;


var

  _SDUTaskDialog_hLib: THandle;
  _SDUTaskDialog_lib_TaskDialog: TFnTaskDialog;


// Forward declarations...
function _SDUTaskDialog_LoadDLL(): boolean; forward;
function _SDUTaskDialog_GetDLLProcAddresses(): boolean; forward;
procedure _SDUTaskDialog_UnloadDLL(); forward;

function _SDUDialogs_StripSingleNewlines(msg: string): string; forward;
       *)

// ----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUOpenDialog]);
  RegisterComponents('SDeanUtils', [TSDUSaveDialog]);
end;
     (*
// ----------------------------------------------------------------------------
function _SDUTaskDialog_LoadDLL(): boolean;
begin
  result := FALSE;

  // If the lib is already loaded, just return TRUE
  if (_SDUTaskDialog_hLib <> 0) then
    begin
    result := TRUE;
    end
  else
    begin
    _SDUTaskDialog_hLib := LoadLibrary(COMCTL32_LIB_DLL);
    if (_SDUTaskDialog_hLib <> 0) then
      begin
      // DLL loaded, get function addresses
      result := _SDUTaskDialog_GetDLLProcAddresses();
      if not(result) then
        begin
        // Unload DLL; there was a problem
        _SDUTaskDialog_UnloadDLL();
        end;
      end;

    end;
end;


// ----------------------------------------------------------------------------
procedure _SDUTaskDialog_UnloadDLL();
begin
  if (_SDUTaskDialog_hLib <> 0) then
    begin
    FreeLibrary(_SDUTaskDialog_hLib);
    _SDUTaskDialog_hLib := 0;
    end;

end;


// ----------------------------------------------------------------------------
function _SDUTaskDialog_GetDLLProcAddresses(): boolean;
begin
  result := TRUE;

  if (_SDUTaskDialog_hLib = 0) then     begin
    result := FALSE;
  end;

  if (result) then    begin
    @_SDUTaskDialog_lib_TaskDialog := GetProcAddress(_SDUTaskDialog_hLib, ''+DLL_FUNCTIONNAME_TaskDialog+'');
    result := (@_SDUTaskDialog_lib_TaskDialog <> nil);
  end;

end;


// ----------------------------------------------------------------------------
function SDUVistaTaskDialog(
  WindowTitle: WideString;
  MainInstruction: WideString;
  Content: WideString;
  CommonButtons: integer;
  Icon: PWChar
): integer;  overload;
begin
  Result := SDUVistaTaskDialog(
                              0,
                              WindowTitle,
                              MainInstruction,
                              Content,
                              CommonButtons,
                              Icon
                             );
end;


// ----------------------------------------------------------------------------
function SDUVistaTaskDialog(
  hWndParent: HWND;
  WindowTitle: WideString;
  MainInstruction: WideString;
  Content: WideString;
  CommonButtons: integer;
  Icon: PWChar
): integer;  overload;
begin
  Result := SDUVistaTaskDialog(
                              0,
                              0, // Application.Handle - We use zero here
                                 // to allow use of the standard predefined
                                 // icons  
                              WindowTitle,
                              MainInstruction,
                              Content,
                              CommonButtons,
                              Icon
                             );
end;


// ----------------------------------------------------------------------------
function SDUVistaTaskDialog(
  hWndParent: HWND;
  hInst: THandle;
  WindowTitle: WideString;
  MainInstruction: WideString;
  Content: WideString;
  CommonButtons: integer;
  Icon: PWChar
): integer; overload;
var
  allOK: boolean;
  olderMsgDlgType: TMsgDlgType;
  olderMsgDlgButtons: TMsgDlgButtons;
  ansistrContent: string;
  title: WideString;
begin
  result := 0;

  allOK := _SDUTaskDialog_LoadDLL();

  if SDUDialogsStripSingleCRLF then
    begin
    Content := _SDUDialogs_StripSingleNewlines(Content);
    end;

  if (allOK) then
    begin
    // Use application title if title not supplied
    title := WindowTitle;
    if (title = '') then
      begin
      title := Application.Title;
      end;

    _SDUTaskDialog_lib_TaskDialog(
                   hWndParent,
                   hInst,
                   PWChar(title),
                   PWChar(MainInstruction),
                   PWChar(Content),
                   CommonButtons,
                   Icon,
                   @result
                  );

    _SDUTaskDialog_UnloadDLL();
    end
  else
    begin
    // Degrade gracefully on non-Vista systems...

    // Degrade icon...
    if (Icon = SDVISTA_TD_ICON_BLANK) then
      begin
      olderMsgDlgType := mtCustom;
      end
    else if (Icon = SDVISTA_TD_ICON_WARNING) then
      begin
      olderMsgDlgType := mtWarning;
      end
    else if (Icon = SDVISTA_TD_ICON_QUESTION) then
      begin
      olderMsgDlgType := mtConfirmation;
      end
    else if (Icon = SDVISTA_TD_ICON_ERROR) then
      begin
      olderMsgDlgType := mtError;
      end
    else if (Icon = SDVISTA_TD_ICON_INFORMATION) then
      begin
      olderMsgDlgType := mtInformation;
      end
//    else if (Icon = SDVISTA_TD_ICON_BLANK_AGAIN) then
//      begin
//      olderMsgDlgType := mtCustom;
//      end;
    else if (Icon = SDVISTA_TD_ICON_SHIELD) then
      begin
      olderMsgDlgType := mtInformation;
      end
    else
      begin
      olderMsgDlgType := mtCustom;
      end;

    // Degrade buttons...
    olderMsgDlgButtons:= [];
    if ((CommonButtons and SDVISTA_TDCBF_OK_BUTTON) = SDVISTA_TDCBF_OK_BUTTON) then
      begin
      olderMsgDlgButtons := olderMsgDlgButtons + [mbOK];
      end
    else if ((CommonButtons and SDVISTA_TDCBF_YES_BUTTON) = SDVISTA_TDCBF_YES_BUTTON) then
      begin
      olderMsgDlgButtons := olderMsgDlgButtons + [mbYes];
      end
    else if ((CommonButtons and SDVISTA_TDCBF_NO_BUTTON) = SDVISTA_TDCBF_NO_BUTTON) then
      begin
      olderMsgDlgButtons := olderMsgDlgButtons + [mbNo];
      end
    else if ((CommonButtons and SDVISTA_TDCBF_CANCEL_BUTTON) = SDVISTA_TDCBF_CANCEL_BUTTON) then
      begin
      olderMsgDlgButtons := olderMsgDlgButtons + [mbCancel];
      end
    else if ((CommonButtons and SDVISTA_TDCBF_RETRY_BUTTON) = SDVISTA_TDCBF_RETRY_BUTTON) then
      begin
      olderMsgDlgButtons := olderMsgDlgButtons + [mbRetry];
      end
    else if ((CommonButtons and SDVISTA_TDCBF_CLOSE_BUTTON) = SDVISTA_TDCBF_CLOSE_BUTTON) then
      begin
      olderMsgDlgButtons := olderMsgDlgButtons + [mbAbort];  // Should be cancel?
      end;

    ansistrContent:= Content;

    result := MessageDlg(ansistrContent, olderMsgDlgType, olderMsgDlgButtons, 0);

    // Map buttonpress...
    case result of
      mrOk:
        begin
        result := IDOK;
        end;

      mrCancel:
        begin
        result := IDCANCEL;
        end;

      mrYes:
        begin
        result := IDYES;
        end;

      mrNo:
        begin
        result := IDNO;
        end;

      mrAbort:
        begin
        result := IDABORT;
        end;

      mrRetry:
        begin
        result := IDRETRY;
        end;

      mrIgnore:
        begin
        result := IDIGNORE;
        end;

      mrAll:
        begin
        result := IDOK; // No real mapping...
        end;

      mrNoToAll:
        begin
        result := IDNO;
        end;

      mrYesToAll:
        begin
        result := IDYES;
        end;

      else
        begin
        result := 0;
        end;

      end;

    end;

end;


// ----------------------------------------------------------------------------
function SDUMessageBox(
  hWnd: HWND;
  Content: string;
  WindowTitle: string;
  Flags: longint
): integer;
var
  allOK: boolean;
  buttons: integer;
  icon: PWChar;
  widestrWindowTitle: WideString;
  widestrContent: WideString;
begin

  allOK := _SDUTaskDialog_LoadDLL();

  if SDUDialogsStripSingleCRLF then
    begin
    Content := _SDUDialogs_StripSingleNewlines(Content);
    end;

  if (allOK) then
    begin     
    // Map buttons...
    buttons := 0;
    if ((Flags and MB_ABORTRETRYIGNORE) = MB_ABORTRETRYIGNORE) then
      begin
      // No "Abort" or "Ignore", we just use the closest here...
      buttons := (
                  SDVISTA_TDCBF_CANCEL_BUTTON or
                  SDVISTA_TDCBF_RETRY_BUTTON or
                  SDVISTA_TDCBF_OK_BUTTON
                 );
      end
    else if ((Flags and MB_OK) = MB_OK) then
      begin
      buttons := SDVISTA_TDCBF_OK_BUTTON;
      end
    else if ((Flags and MB_OKCANCEL) = MB_OKCANCEL) then
      begin
      buttons := SDVISTA_TDCBF_OK_BUTTON or SDVISTA_TDCBF_CANCEL_BUTTON;
      end
    else if ((Flags and MB_RETRYCANCEL) = MB_RETRYCANCEL) then
      begin
      buttons := SDVISTA_TDCBF_RETRY_BUTTON or SDVISTA_TDCBF_CANCEL_BUTTON;
      end
    else if ((Flags and MB_YESNO) = MB_YESNO) then
      begin
      buttons := SDVISTA_TDCBF_YES_BUTTON or SDVISTA_TDCBF_NO_BUTTON;
      end
    else if ((Flags and MB_YESNOCANCEL) = MB_YESNOCANCEL) then
      begin
      buttons := (
                  SDVISTA_TDCBF_YES_BUTTON or
                  SDVISTA_TDCBF_NO_BUTTON or
                  SDVISTA_TDCBF_CANCEL_BUTTON
                 );
      end;


    // Map icon...
    icon := SDVISTA_TD_ICON_BLANK;
    if ((Flags and MB_ICONWARNING) = MB_ICONWARNING) then
      begin
      icon := SDVISTA_TD_ICON_WARNING;
      end
    else if ((Flags and MB_ICONERROR) = MB_ICONERROR) then
      begin
      icon := SDVISTA_TD_ICON_ERROR;
      end
    else if ((Flags and MB_ICONINFORMATION) = MB_ICONINFORMATION) then
      begin
      icon := SDVISTA_TD_ICON_INFORMATION;
      end
    else if ((Flags and MB_ICONQUESTION) = MB_ICONQUESTION) then
      begin
      icon := SDVISTA_TD_ICON_QUESTION;
      end
    else if ((Flags and MB_ICONEXCLAMATION) = MB_ICONEXCLAMATION) then
      begin
      icon := SDVISTA_TD_ICON_INFORMATION;  // Nearest we've got
      end
    else if ((Flags and MB_ICONHAND) = MB_ICONHAND) then
      begin
      icon := SDVISTA_TD_ICON_ERROR;  // Nearest we've got...
      end
    else if ((Flags and MB_ICONASTERISK) = MB_ICONASTERISK) then
      begin
      icon := SDVISTA_TD_ICON_INFORMATION; // Nearest we've got...
      end
    else if ((Flags and MB_ICONSTOP) = MB_ICONSTOP) then
      begin
      icon := SDVISTA_TD_ICON_ERROR;  // Nearest we've got...
      end
    else if ((Flags and MB_USERICON) = MB_USERICON) then
      begin
      icon := SDVISTA_TD_ICON_BLANK; // Not supported by MessageBox(...) anyway...
      end;

    // Convert to widestrings...
    widestrWindowTitle     := WindowTitle;
    widestrContent         := Content;

    result := SDUVistaTaskDialog(
                                hWnd,
                                widestrWindowTitle,
                                '',
                                widestrContent,
                                buttons,
                                icon
                               );

    // No need to map return value; MessageBox uses IDOK, IDCANCEL, etc anyway

    _SDUTaskDialog_UnloadDLL();
    end
  else
    begin
    // Fallback...
    result := MessageBox(hWnd, PChar(Content), PChar(WindowTitle), Flags);
    end;

end;


// ----------------------------------------------------------------------------
function SDUMessageDlg(          
  Content: string;
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  HelpCtx: Longint
): integer;
const
  // This taken from CreateMessageDialog(...) in Dialogs.pas
  SDCaptions: array[TMsgDlgType] of Pointer = (@SDU_SMsgDlgWarning, @SDU_SMsgDlgError,
    @SDU_SMsgDlgInformation, @SDU_SMsgDlgConfirm, nil);
var
  allOK: boolean;
  widestrWindowTitle: WideString;
  widestrContent: WideString;
  TDButtons: integer;
  TBIcon: PWChar;
begin
//  if G_SuppressDialogs then exit;

  allOK := _SDUTaskDialog_LoadDLL();

  if SDUDialogsStripSingleCRLF then     begin
    Content := _SDUDialogs_StripSingleNewlines(Content);
    end;

  if (allOK) then
    begin
    // Map buttons...
    TDButtons := 0;
    if (mbYes in Buttons) then
      begin
      TDButtons := TDButtons or SDVISTA_TDCBF_YES_BUTTON;
      end;
    if (mbNo in Buttons) then
      begin
      TDButtons := TDButtons or SDVISTA_TDCBF_NO_BUTTON;
      end;
    if (mbOK in Buttons) then
      begin
      TDButtons := TDButtons or SDVISTA_TDCBF_OK_BUTTON;
      end;
    if (mbCancel in Buttons) then
      begin
      TDButtons := TDButtons or SDVISTA_TDCBF_CANCEL_BUTTON;
      end;
    if (mbAbort in Buttons) then
      begin
      TDButtons := TDButtons or SDVISTA_TDCBF_CLOSE_BUTTON;  // Should be cancel?
      end;
    if (mbRetry in Buttons) then
      begin
      TDButtons := TDButtons or SDVISTA_TDCBF_RETRY_BUTTON;
      end;
    if (mbIgnore in Buttons) then
      begin
      // Not supported
      end;
    if (mbAll in Buttons) then
      begin
      // Not supported
      end;
    if (mbNoToAll in Buttons) then
      begin
      // Not supported
      end;
    if (mbYesToAll in Buttons) then
      begin
      // Not supported
      end;
    if (mbHelp in Buttons) then
      begin
      // Not supported
      end;

    // Map icon...
    case DlgType of
      mtWarning:
        begin
        TBIcon := SDVISTA_TD_ICON_WARNING;
        end;

      mtError:
        begin
        TBIcon := SDVISTA_TD_ICON_ERROR;
        end;

      mtInformation:
        begin
        TBIcon := SDVISTA_TD_ICON_INFORMATION;
        end;

      mtConfirmation:
        begin
        TBIcon := SDVISTA_TD_ICON_QUESTION;
        end;

      mtCustom:
        begin
        TBIcon := SDVISTA_TD_ICON_BLANK;
        end;

      else
        begin
        TBIcon := SDVISTA_TD_ICON_BLANK;
        end;

      end;

    // Convert to widestrings...
    widestrContent         := Content;

    // This taken from CreateMessageDialog(...) in Dialogs.pas
    if DlgType <> mtCustom then
      begin
      widestrWindowTitle := LoadResString(SDCaptions[DlgType])
      end
    else
      begin
      widestrWindowTitle := Application.Title;
      end;

    result := SDUVistaTaskDialog(
                                0,
                                widestrWindowTitle,
                                '',
                                widestrContent,
                                TDButtons,
                                TBIcon
                               );

    // No need to map return value; MessageBox uses IDOK, IDCANCEL, etc anyway

    _SDUTaskDialog_UnloadDLL();
    end
  else
    begin
    // Fallback...
    result := MessageDlg(Content, DlgType, Buttons, HelpCtx);
    end;

end;


// ----------------------------------------------------------------------------
function SDUMessageDlg(
  Content: string;
  DlgType: TMsgDlgType
): integer;
begin
  Result := SDUMessageDlg(Content, DlgType, [mbOK], 0);
end;


// ----------------------------------------------------------------------------
function SDUMessageDlg(
  Content: string
): integer;
begin
  Result := SDUMessageDlg(Content, mtInformation, [mbOK], 0);
end;


// ----------------------------------------------------------------------------
// Convert CRLFs to spaces (or just strip them out), while preserving
// CRLF+CRLFs
function _SDUDialogs_StripSingleNewlines(msg: string): string;
const
  DBLCRLF_MARKER = #13;
begin
  if SDUOSVistaOrLater() then
    begin
    // Replace double-CRLFs with a special flag marker to preserve them
    msg := StringReplace(msg, SDUCRLF+SDUCRLF, DBLCRLF_MARKER, [rfReplaceAll]);

    // Replace any remaining CRLFs with a space character, unless they were
    // already next to a space character, in which case just strip the CRLF out.
    msg := StringReplace(msg, SDUCRLF+' ', ' ', [rfReplaceAll]);
    msg := StringReplace(msg, ' '+SDUCRLF, ' ', [rfReplaceAll]);
    msg := StringReplace(msg, SDUCRLF, ' ', [rfReplaceAll]);

    // Revert double-CRLFs
    msg := StringReplace(msg, DBLCRLF_MARKER, SDUCRLF+SDUCRLF, [rfReplaceAll]);
    end;

  Result := msg;
end;

// ----------------------------------------------------------------------------
// Display confirmation dialog with OK/Cancel
// Returns TRUE if the user selects OK
function SDUConfirmOK(msg: string): boolean;
begin
  Result := (SDUMessageDlg(msg, mtConfirmation, [mbOK, mbCancel], 0) = mrOK);
end;


// ----------------------------------------------------------------------------
// Display confirmation dialog with Yes/No buttons.
// Returns TRUE if the user selects Yes.
function SDUConfirmYN(msg: string): boolean;
begin
  Result := (SDUMessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;


// ----------------------------------------------------------------------------
// Display confirmation dialog with OK/Cancel
// Returns TRUE if the user selects OK
function SDUWarnOK(msg: string): boolean;
begin
  Result := (SDUMessageDlg(msg, mtWarning, [mbOK, mbCancel], 0) = mrOK);
end;


// ----------------------------------------------------------------------------
// Display confirmation dialog with Yes/No buttons.
// Returns TRUE if the user selects Yes.
function SDUWarnYN(msg: string): boolean;
begin
  Result := (SDUMessageDlg(msg, mtWarning, [mbYes, mbNo], 0) = mrYes);
end;


// ----------------------------------------------------------------------------
// Display confirmation dialog with OK/Cancel
// Returns TRUE if the user selects OK
function SDUErrorOK(msg: string): boolean;
begin
  Result := (SDUMessageDlg(msg, mtError, [mbOK, mbCancel], 0) = mrOK);
end;


// ----------------------------------------------------------------------------
// Display confirmation dialog with Yes/No buttons.
// Returns TRUE if the user selects Yes.
function SDUErrorYN(msg: string): boolean;
begin
  Result := (SDUMessageDlg(msg, mtError, [mbYes, mbNo], 0) = mrYes);
end;
 *)

// ----------------------------------------------------------------------------
function TSDUOpenDialog.Execute(): boolean;
var
  oldCWD: string;
begin
  oldCWD := SDUGetCWD();

  Result := inherited Execute();

  if fPreserveCWD then
    SDUSetCWD(oldCWD);


end;


// ----------------------------------------------------------------------------
function TSDUSaveDialog.Execute(): boolean;
var
  oldCWD: string;
begin
  oldCWD := SDUGetCWD();

  Result := inherited Execute();

  if fPreserveCWD then    SDUSetCWD(oldCWD);


end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------


END.


