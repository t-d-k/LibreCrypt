unit Main;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SDUSystemTrayIcon, Menus, ActnList;

type
  TForm1 = class(TForm)
    SDUSystemTrayIcon1: TSDUSystemTrayIcon;
    PopupMenu1: TPopupMenu;
    miDisplay: TMenuItem;
    miExit: TMenuItem;
    pbExit: TButton;
    ActionList1: TActionList;
    actExit: TAction;
    actDisplay: TAction;
    GroupBox1: TGroupBox;
    pbAppMinimise: TButton;
    pbClose: TButton;
    pbWSMinimise: TButton;
    pbHide: TButton;
    GroupBox2: TGroupBox;
    ckDisplaySystemTrayIconIcon: TCheckBox;
    ckCloseToIcon: TCheckBox;
    ckMinToIcon: TCheckBox;
    GroupBox3: TGroupBox;
    pbChildModal: TButton;
    pbChildNonModal: TButton;
    procedure UpdateSystemTrayIconSettings(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure pbAppMinimiseClick(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actDisplayExecute(Sender: TObject);
    procedure pbWSMinimiseClick(Sender: TObject);
    procedure pbHideClick(Sender: TObject);
    procedure pbChildNonModalClick(Sender: TObject);
    procedure pbChildModalClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    // Flag to indicate that the login session is about to end
    EndSessionFlag: boolean;

    procedure WMEndSession(var msg: TWMEndSession); message WM_ENDSESSION;
    procedure WMQueryEndSession(var msg: TWMQueryEndSession); message WM_QUERYENDSESSION;

  public
    procedure AppMin(Sender: TObject);

  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
{$IFDEF GEXPERTS}
  DbugIntf,
{$ELSE}
  SDULogger_U,
{$ENDIF}
  SDUGeneral,
  SecondForm;


{$IFNDEF GEXPERTS}
  {$IFDEF SDUSystemTrayIcon_DEBUG}
procedure SendDebug(x: string);
var
  logObj: TSDULogger;
begin
  logObj:= TSDULogger.Create('C:\SDUSystemTrayIconDebug.txt');
  try
    logObj.LogMessage(x);
  finally
    logObj.Free();
  end;
end;
  {$ELSE}
procedure SendDebug(x: string);
begin
  // Do nothing
end;
  {$ENDIF}
{$ENDIF}


// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
//       Functions required for close/minimize to SystemTrayIcon follow
// --------------------------------------------------------------------------
// --------------------------------------------------------------------------

// Function required for close to system tray icon
procedure TForm1.WMQueryEndSession(var msg: TWMQueryEndSession);
begin
  SendDebug('MAIN APP WMQueryEndSession');
  // Default to allowing shutdown
  msg.Result := 1;
  EndSessionFlag := (msg.Result = 1);
  SendDebug('MAIN APP WMQueryEndSession');
  inherited;
  EndSessionFlag := (msg.Result = 1);
  
end;


// Function required for close to system tray icon
procedure TForm1.WMEndSession(var msg: TWMEndSession);
begin
  SendDebug('MAIN APP WMEndSession');
  EndSessionFlag := msg.EndSession;

end;


// Function required for close to system tray icon
procedure TForm1.FormCreate(Sender: TObject);
begin
  EndSessionFlag := FALSE;

  // This next lines are *NOT* needed - only used for the test app
  self.Caption := Application.Title;
  UpdateSystemTrayIconSettings(nil);
  Application.OnMinimize := AppMin;

end;


// Function required for close to system tray icon
// Note: This is *only* required in your application is you intend to call
//       Close() to hide/close the window. Calling Hide() instead is preferable
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SendDebug('FormClose');

  // This code segment is required to handle the case when Close() is called
  // programatically
  // Only carry out this action if closing to SystemTrayIcon
  // Note the MainForm test; if it was the main form, setting Action would have
  // no effect
  if (
      not(EndSessionFlag) and
      SDUSystemTrayIcon1.Active and
      ckCloseToIcon.checked and
{$IFNDEF VER180}
      (
       // If Application.MainFormOnTaskbar is set, use the form name,
       // otherwise check exactly
       (
        Application.MainFormOnTaskbar and
        (application.mainform <> nil) and
        (application.mainform.name = self.name)
       ) or
       (
        not(Application.MainFormOnTaskbar) and
        (application.mainform <> self)
       )
      )
{$ELSE}
      (Application.MainForm <> self)
{$ENDIF}
     ) then
    begin
    SendDebug('Setting FormClose Action');
    Action := caMinimize;
    end;

end;


// Function required for close to system tray icon
// Note: This is *only* required in your application is you intend to call
//       Close() to hide/close the window. Calling Hide() instead is preferable
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SendDebug('FormCloseQuery');

  // This code segment is required to handle the case when Close() is called
  // programatically
  // Only carry out this action if closing to SystemTrayIcon
  // Note the MainForm test; if it was not the main form, setting Action would
  // in FormClose would do the minimise
  SendDebug('FormCloseQuery SDUSystemTrayIcon1.Active: '+booltostr(SDUSystemTrayIcon1.Active));
  if (
      not(EndSessionFlag) and
      SDUSystemTrayIcon1.Active and
      ckCloseToIcon.checked and
{$IFNDEF VER180}
      (
       // If Application.MainFormOnTaskbar is set, use the form name,
       // otherwise check exactly
       (
        Application.MainFormOnTaskbar and
        (application.mainform <> nil) and
        (application.mainform.name = self.name)
       ) or
       (
        not(Application.MainFormOnTaskbar) and
        (application.mainform = self)
       )
      )
{$ELSE}
      (Application.MainForm = self)
{$ENDIF}
     ) then
    begin
    SendDebug('FormCloseQuery setting CanClose to FALSE; minimizing to SystemTrayIcon');
    CanClose := FALSE;
    SDUSystemTrayIcon1.DoMinimizeToIcon();
    end;

end;


// Function required for both close and minimize to system tray icon
// Procedure to display the form after minimize/close
procedure TForm1.actDisplayExecute(Sender: TObject);
begin
  SendDebug('actDisplayExecute');
  SDUSystemTrayIcon1.DoRestore();

end;


// Function required for both close and minimize to system tray icon
procedure TForm1.actExitExecute(Sender: TObject);
begin
  // Either:
  //   Application.Terminate();
  // Or:
  SDUSystemTrayIcon1.Active := FALSE;
  Close();

end;


// --------------------------------------------------------------------------
// --------------------------------------------------------------------------
//                           Test functions follow
// --------------------------------------------------------------------------
// --------------------------------------------------------------------------


procedure TForm1.UpdateSystemTrayIconSettings(Sender: TObject);
begin
  SDUSystemTrayIcon1.Active         := ckDisplaySystemTrayIconIcon.checked;
  SDUSystemTrayIcon1.MinimizeToIcon := ckMinToIcon.checked;

end;


procedure TForm1.pbChildNonModalClick(Sender: TObject);
var
  dlg: TForm2;
begin
  dlg:= TForm2.Create(nil);
  try
    dlg.Show();
  finally
    // Note: We don't free dlg - we just leave it non-modal
  end;

end;


procedure TForm1.pbChildModalClick(Sender: TObject);
var
  dlg: TForm2;
begin
  dlg:= TForm2.Create(nil);
  try
    dlg.ShowModal();
  finally
    dlg.Free()
  end;

end;


procedure TForm1.pbAppMinimiseClick(Sender: TObject);
begin
  Application.Minimize();

end;

procedure TForm1.pbWSMinimiseClick(Sender: TObject);
begin
  WindowState := wsminimized;

end;


procedure TForm1.pbHideClick(Sender: TObject);
begin
  Hide();

end;


procedure TForm1.pbCloseClick(Sender: TObject);
begin
  Close();

end;


procedure TForm1.AppMin(Sender: TObject);
begin
  SendDebug('AppMin called');

end;


END.



