unit lcDialogs;

 // Note: In order to use this, need XP Manifest in your application
 // See also: TaskDialogIndirect(...) and TASKDIALOGCONFIG!!!

interface

uses
  Dialogs,
  Windows;

var
  // If set, then any messages displayed will have any single CRLFs stripped
  // out, while double CRLFs (SDU_CRLF+SDUCRLF) will be preserved under
  // Windows Vista and later
  // This allows messages to have CRLFs included in them for pre-Vista systems,
  // in order to break up long lines - but on Vista, which does more sensible
  // word wrapping, such CRLFs will be removed
  GSDUDialogsStripSingleCRLF: Boolean = True;

 // set to true to suppress all dialogs (ege for testing)
 //  G_SuppressDialogs : boolean = False;

 // Display confirmation dialog with OK/Cancel or Yes/No buttons.
 // Returns TRUE if the user selects OK or Yes.
function SDUConfirmOK(msg: String): Boolean;
function SDUConfirmYN(msg: String): Boolean;
function SDUWarnOK(msg: String): Boolean;
function SDUWarnYN(msg: String): Boolean;
function SDUErrorOK(msg: String): Boolean;
function SDUErrorYN(msg: String): Boolean;

// function MessageBox(hWnd: HWND; lpText, lpCaption: PChar; uType: UINT): Integer; stdcall;
function SDUMessageBox(
  hWnd: HWND;
  Content: String;
  WindowTitle: String;
  Flags: Longint): Integer;

 // function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
 //   Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
function SDUMessageDlg(
  Content: String;
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  HelpCtx: Longint): Integer; overload;

function SDUMessageDlg(Content: String;
  DlgType: TMsgDlgType): Integer; overload;

function SDUMessageDlg(Content: String): Integer; overload;


implementation

uses
  Classes,
  Controls,  // Required for mrXXX
             //  Consts,
             //  RTLConsts,
  Forms,     // Required for Application
  SDUGeneral,
  SysUtils,   // Required for StringReplace
  UITypes;


resourcestring

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

//type



function _SDUDialogs_StripSingleNewlines(msg: String): String; forward;



// ----------------------------------------------------------------------------
function SDUMessageBox(
  hWnd: HWND;
  Content: String;
  WindowTitle: String;
  Flags: Longint): Integer;
begin
  if GSDUDialogsStripSingleCRLF then
    Content := _SDUDialogs_StripSingleNewlines(Content);

  // do enhancements here
  Result := MessageBox(hWnd, PChar(Content), PChar(WindowTitle), Flags);
end;

// ----------------------------------------------------------------------------
function SDUMessageDlg(
  Content: String;
  DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons;
  HelpCtx: Longint): Integer;
begin
  //  if G_SuppressDialogs then exit;

  if GSDUDialogsStripSingleCRLF then
    Content := _SDUDialogs_StripSingleNewlines(Content);

  // do enhancements here
  Result := MessageDlg(Content, DlgType, Buttons, HelpCtx);
end;


// ----------------------------------------------------------------------------
function SDUMessageDlg(
  Content: String;
  DlgType: TMsgDlgType): Integer;
begin
  Result := SDUMessageDlg(Content, DlgType, [mbOK], 0);
end;


// ----------------------------------------------------------------------------
function SDUMessageDlg(
  Content: String): Integer;
begin
  Result := SDUMessageDlg(Content, mtInformation, [mbOK], 0);
end;


 // ----------------------------------------------------------------------------
 // Convert CRLFs to spaces (or just strip them out), while preserving
 // CRLF+CRLFs
function _SDUDialogs_StripSingleNewlines(msg: String): String;
const
  DBLCRLF_MARKER = #13;
begin
  if SDUOSVistaOrLater() then begin
    // Replace double-CRLFs with a special flag marker to preserve them
    msg := StringReplace(msg, SDUCRLF + SDUCRLF, DBLCRLF_MARKER, [rfReplaceAll]);

    // Replace any remaining CRLFs with a space character, unless they were
    // already next to a space character, in which case just strip the CRLF out.
    msg := StringReplace(msg, SDUCRLF + ' ', ' ', [rfReplaceAll]);
    msg := StringReplace(msg, ' ' + SDUCRLF, ' ', [rfReplaceAll]);
    msg := StringReplace(msg, SDUCRLF, ' ', [rfReplaceAll]);

    // Revert double-CRLFs
    msg := StringReplace(msg, DBLCRLF_MARKER, SDUCRLF + SDUCRLF, [rfReplaceAll]);
  end;

  Result := msg;
end;

 // ----------------------------------------------------------------------------
 // Display confirmation dialog with OK/Cancel
 // Returns TRUE if the user selects OK
function SDUConfirmOK(msg: String): Boolean;
begin
  Result := (SDUMessageDlg(msg, mtConfirmation, [mbOK, mbCancel], 0) = mrOk);
end;


 // ----------------------------------------------------------------------------
 // Display confirmation dialog with Yes/No buttons.
 // Returns TRUE if the user selects Yes.
function SDUConfirmYN(msg: String): Boolean;
begin
  Result := (SDUMessageDlg(msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;


 // ----------------------------------------------------------------------------
 // Display confirmation dialog with OK/Cancel
 // Returns TRUE if the user selects OK
function SDUWarnOK(msg: String): Boolean;
begin
  Result := (SDUMessageDlg(msg, mtWarning, [mbOK, mbCancel], 0) = mrOk);
end;


 // ----------------------------------------------------------------------------
 // Display confirmation dialog with Yes/No buttons.
 // Returns TRUE if the user selects Yes.
function SDUWarnYN(msg: String): Boolean;
begin
  Result := (SDUMessageDlg(msg, mtWarning, [mbYes, mbNo], 0) = mrYes);
end;


 // ----------------------------------------------------------------------------
 // Display confirmation dialog with OK/Cancel
 // Returns TRUE if the user selects OK
function SDUErrorOK(msg: String): Boolean;
begin
  Result := (SDUMessageDlg(msg, mtError, [mbOK, mbCancel], 0) = mrOk);
end;


 // ----------------------------------------------------------------------------
 // Display confirmation dialog with Yes/No buttons.
 // Returns TRUE if the user selects Yes.
function SDUErrorYN(msg: String): Boolean;
begin
  Result := (SDUMessageDlg(msg, mtError, [mbYes, mbNo], 0) = mrYes);
end;


end.
