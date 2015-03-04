unit SDUSystemTrayIcon;
// Description: System Tray Icon
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

// This just uses the later structures for "TNotifyIconData_...", which should
// be harmless with earlier versions - the shell just ignores the extra data
// Note: To get the version ID of the shell, use 'shell32.dll' and the version
//       ID lib code from SDUGeneral

uses
  Menus,
  SysUtils,  // Required for Exceptions
  Windows,  // Required for hIcon
  Graphics,  // Required for TIcon
  Controls,  // Required for TImageList
  ExtCtrls,  // Required for TTimer
  Classes,  // Required for TComponent
  Messages,  // Required for TMessage
  imgList;  // Required for TChangeLink

type
  // Exceptions...
  ESDUSystemTrayIconError = Exception;
  ESDUSystemTrayIconNotActive = ESDUSystemTrayIconError;

  TSDUSystemTrayIconBubbleIcon = (shbiNone, shbiInfo, shbiWarning, shbiError);

{$IFDEF WIN32}
  WParameter = LongInt;
{$ELSE}
  WParameter = Word;
{$ENDIF}
  LParameter = LongInt;

  TSDUSystemTrayIcon = class(TComponent)
  private
    // Various which may be set by user
    FActive: boolean;

    FMinimizeToIcon: boolean;

    FPopupMenuMenu: TPopupMenu;
    FTip: Ansistring;

    FIcon: TIcon;

    FAnimationIcons: TImageList;
    FAnimateIcon: boolean;

    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnContextPopup: TContextPopupEvent;
    FOnBalloonShow: TNotifyEvent;
    FOnBalloonHide: TNotifyEvent;
    FOnBalloonTimeout: TNotifyEvent;
    FOnBalloonUserClick: TNotifyEvent;

    // Internal state
    FAnimationChangeLink: TChangeLink;

    FhWindow: HWND;

    FAnimationPos: integer;
    FAnimationTimer: TTimer;

    FOldAppProc: Pointer;
    FOldWindowProc: Pointer;
    FNewAppProc: Pointer;
    FNewWindowProc: Pointer;

    FSystemShuttingDown: boolean;
    FActiveBeforeQES: boolean;

  protected
    procedure SetActive(status: boolean);
    procedure SetAnimateIcon(animate: boolean);
    procedure SetTip(tip: Ansistring);
    procedure SetIcon(icon: TIcon);
    procedure SetAnimationIcons(Value: TImageList);
    function  GetAnimationDelay(): integer;
    procedure SetAnimationDelay(delay: integer);

    procedure TimerFired(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure AddOrRemoveTrayIcon();
    procedure UpdateTrayIcon(); overload;
    procedure UpdateTrayIcon(dwMessage: cardinal); overload;

    procedure TrayIconCallback(var msg: TMessage);
    procedure AnimationListChanged(Sender: TObject);

    procedure CheckActive();

    procedure NewAppProc(var msg: TMessage);
    procedure NewWindowProc(var msg: TMessage);

    procedure TrapWindowMessages();
    procedure TrapWindowMessages_AppProc();
    procedure TrapWindowMessages_WindowProc();
    procedure UntrapWindowMessages();
    procedure UntrapWindowMessages_AppProc();
    procedure UntrapWindowMessages_WindowProc();

    procedure Loaded(); override;
    procedure DebugWM(wmFrom: string; msg: TMessage);
    // AppWM - Set to TRUE for app WM, FALSE for window WM
    function  ProcessWM(AppProcNotWindowProc: boolean; var msg: TMessage): boolean;
    
    function  IsOnMainForm(): boolean;
    function  TheMainHandle(): THandle;

  public
    // Procedures made public for developer convenience
    procedure DoMinimizeToIcon();
    procedure DoRestore();

  published
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure BubbleMessage(Title: AnsiChar; Info: AnsiChar; Icon: TSDUSystemTrayIconBubbleIcon = shbiNone; Timeout: integer = 15000);

    property Active: boolean read FActive write SetActive;

    property MinimizeToIcon: boolean read FMinimizeToIcon write FMinimizeToIcon;

    property PopupMenu: TPopupMenu read FPopupMenuMenu write FPopupMenuMenu;

    property Tip: Ansistring read FTip write SetTip;
    property Icon: TIcon read FIcon write SetIcon;

    property AnimationIcons: TImageList read FAnimationIcons write SetAnimationIcons;
    property AnimateIcon: boolean read FAnimateIcon write SetAnimateIcon;
    property AnimationDelay: integer read GetAnimationDelay write SetAnimationDelay;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnContextPopup: TContextPopupEvent read FOnContextPopup write FOnContextPopup;
    property OnBalloonShow: TNotifyEvent read FOnBalloonShow write FOnBalloonShow;
    property OnBalloonHide: TNotifyEvent read FOnBalloonHide write FOnBalloonHide;
    property OnBalloonTimeout: TNotifyEvent read FOnBalloonTimeout write FOnBalloonTimeout;
    property OnBalloonUserClick: TNotifyEvent read FOnBalloonUserClick write FOnBalloonUserClick;

  end;

procedure Register;

implementation

uses
{$IFDEF SDUSystemTrayIcon_DEBUG}
{$IFDEF GEXPERTS}
  DbugIntf,  
{$ELSE}
  SDULogger_U,
{$ENDIF}
{$ENDIF}
  Forms,
  SDUGeneral,
  SDUSystemTrayIconShellAPI;

const
  EXCPT_SYSTEMTRAYICON_NOT_ACTIVE = 'System tray icon must be set to Active before use.';

  WM_SDU_SYSTEMTRAYICON = WM_USER + 1;


{$IFDEF SDUSystemTrayIcon_DEBUG}
  {$IFNDEF GEXPERTS}
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
  {$ENDIF}
{$ELSE}
procedure SendDebug(x: string);
begin
  // Do nothing
end;
{$ENDIF}


// ----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUSystemTrayIcon]);
end;


// ----------------------------------------------------------------------------
constructor TSDUSystemTrayIcon.Create(AOwner : TComponent);
begin
  inherited;

  FActive:= FALSE;

  FMinimizeToIcon := FALSE;

  FPopupMenuMenu:= nil;
  FTip:= '';

  FIcon:= TIcon.Create();
  FAnimationIcons := nil;
  FAnimateIcon:= FALSE;

  FOnClick:= nil;
  FOnDblClick:= nil;

  // Internal state
  // Delphi 6 and later moved AllocateHWnd to "Classes"
{$IFDEF VER130}
  FhWindow:= AllocateHWnd(self.TrayIconCallback);  // Allocate a new window, specifying
                                                   // the callback function for window
                                                   // messages
{$ELSE}
  FhWindow:= Classes.AllocateHWnd(self.TrayIconCallback);  // Allocate a new window, specifying
                                                           // the callback function for window
                                                           // messages
{$ENDIF}

  FAnimationTimer:= TTimer.Create(self);
  FAnimationTimer.Enabled := FALSE;
  FAnimationTimer.OnTimer := TimerFired;
  FAnimationTimer.Interval := 1000;
  FAnimationPos:= 0;
  FAnimationChangeLink := TChangeLink.Create();
  FAnimationChangeLink.OnChange := self.AnimationListChanged;

  FOldAppProc    := nil;
  FOldWindowProc := nil;
  FNewAppProc    := nil;
  FNewWindowProc := nil;

  FSystemShuttingDown := FALSE;
  FActiveBeforeQES := Active;

end;


// ----------------------------------------------------------------------------
destructor TSDUSystemTrayIcon.Destroy;
begin
  Active := FALSE;

  if not(csDesigning in ComponentState) then
    begin
    UntrapWindowMessages();
    end;

  // Delphi 6 and later moved AllocateHWnd to "Classes"
{$IFDEF VER130}
  DeAllocateHWnd(FhWindow);
{$ELSE}
  Classes.DeAllocateHWnd(FhWindow);
{$ENDIF}

  FAnimationTimer.Enabled := FALSE;
  FAnimationTimer.Free();

  FAnimationChangeLink.Free();
  FIcon.Free();

  inherited;
end;


// ----------------------------------------------------------------------------
// Note: This cannot be done in the constructor
procedure TSDUSystemTrayIcon.Loaded();
//var
//  showWindowStatus: integer;
//  si: TStartupInfo;
begin
  inherited;

{
// THIS COMMENTED OUT CODE HAS NO EFFECT WHEN UNCOMMENTED - THE MAINFORM ISN'T
// SETUP WHEN THIS EXECUTES

  if not(csDesigning in ComponentState) then
    begin
    if ((CmdShow = SW_SHOWMINIMIZED) or (CmdShow = SW_SHOWMINNOACTIVE)) then
      begin
      SendDebug('Start minimized!');

SendDebug('Start xxminimized!');
if Application.MainForm = nil then
  begin
  SendDebug('MAINFORM NIL!');
  end;
SendDebug('owner: '+inttostr(integer(owner)));
SendDebug('Application.MainForm: '+inttostr(integer(Application.MainForm)));

  if (Application.MainForm = owner) then
    begin
SendDebug('Start zzminimized!');
    showWindowStatus := CmdShow;
    if (showWindowStatus = SW_SHOWDEFAULT) then
      begin
      GetStartupInfo(si);
      showWindowStatus := si.wShowWindow;
      end;

    if (
        (showWindowStatus = SW_MINIMIZE) or
        (showWindowStatus = SW_SHOWMINIMIZED) or
        (showWindowStatus = SW_SHOWMINNOACTIVE)
       ) then
      begin
SendDebug('Start minimized!');
      Showwindow(TheMainHandle(), SW_HIDE);
      Application.ShowMainForm := FALSE;
      Application.MainForm.visible := FALSE;
//      Hide();
SendDebug('SETTING HIDDEN ------------------');
      end;
    end;

//      MinimizeToIcon();
      end;
    end;
}

end;


// ----------------------------------------------------------------------------
function TSDUSystemTrayIcon.TheMainHandle(): THandle;
var
  useHandle: THandle;
begin
  useHandle := Application.Handle;
// Delphi 2007 and later only
{$IFDEF VER180}
  if (
      Application.MainFormOnTaskBar and
      Assigned(Application.MainForm) and
      Application.MainForm.HandleAllocated
     ) then
  begin
    useHandle := Application.MainFormHandle;
  end;
//  useHandle := Application.Handle;
{$ENDIF}

  Result := useHandle;
end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.DoMinimizeToIcon();
begin
  SendDebug('DoMinimizeToIcon');

  if Active then
    begin
    // Set the window procedure back to the old window procedure
    if (Owner <> nil) then
      begin
      if (Owner is TWinControl) then
        begin
        // If our owner is the main form, it's an Application.Minimize
        if IsOnMainForm() then
          begin
          Application.MainForm.Visible := FALSE;
          ShowWindow(TheMainHandle(), SW_HIDE);
//          ShowWindow(Application.Handle, SW_HIDE);
//          ShowWindow(Application.MainForm.Handle, SW_HIDE);
//          ShowWindow(Application.MainFormHandle, SW_HIDE);
          //  Application.ShowMainForm := FALSE;  NO! We don't do this - may be wanted by app...
          end
        else
          // Otherwise, just hide the owner
          begin
          ShowWindow(TWinControl(Owner).Handle, SW_HIDE);
          end;
        end;
      end;
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.DoRestore();
begin
  SendDebug('DoRestore');
  if (Owner <> nil) then
    begin
    if (Owner is TForm) then
      begin
      TForm(Owner).Show();
      TForm(Owner).WindowState := wsNormal;
      end;
    end;

  Application.Restore();

  // Only requrid if we're the main form
  if IsOnMainForm() then
    begin
    ShowWindow(TheMainHandle(), SW_RESTORE);
    end;

  //Application.ShowMainForm := TRUE;  NO! We didn't do this on minimise...

  Application.BringToFront();

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.CheckActive();
begin
  if not(Active) then
    begin
    raise ESDUSystemTrayIconNotActive.Create(EXCPT_SYSTEMTRAYICON_NOT_ACTIVE);
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.AnimationListChanged(Sender: TObject);
begin
  FAnimationPos := 0;
  
end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.TrayIconCallback(var msg: TMessage);
var
  csrPos: TPoint;
  bHandled: boolean;
begin
  DebugWM('TrayIconCallback', msg);

  case msg.Msg of
    WM_QUERYENDSESSION:
      begin
      // As far as our (invisible) window is concerned; the system can
      // shutdown at any time
      msg.Result := 1;
      end;

    WM_SDU_SYSTEMTRAYICON:
      begin
      case msg.LParam of
        // From:
        // http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/platform/shell/reference/functions/shell_notifyicon.asp
        //
        // "*) If a user selects a notify icon's shortcut menu with the keyboard, the version 5.0 Shell sends the associated application a WM_CONTEXTMENU message. Earlier versions send WM_RBUTTONDOWN and WM_RBUTTONUP messages.
        //  *) If a user selects a notify icon with the keyboard and activates it with the SPACEBAR or ENTER key, the version 5.0 Shell sends the associated application an NIN_KEYSELECT notification. Earlier versions send WM_RBUTTONDOWN and WM_RBUTTONUP messages.
        //  *) If a user selects a notify icon with the mouse and activates it with the ENTER key, the version 5.0 Shell sends the associated application an NIN_SELECT notification. Earlier versions send WM_RBUTTONDOWN and WM_RBUTTONUP messages."
        WM_RBUTTONUP,
        WM_CONTEXTMENU,
        NIN_KEYSELECT,
        NIN_SELECT:
          begin
          GetCursorPos(csrPos);  // Get the cursor position on screen

          bHandled := False;
          if Assigned(FOnContextPopup) then
            begin
            FOnContextPopup(Self, csrPos, bHandled);
            end;
          msg.Result := Ord(bHandled);

          if not(bHandled) then
            begin
            if Assigned(PopupMenu) then
              begin
              if PopupMenu.AutoPopup then
                begin
                SetForegroundWindow(TheMainHandle());  // Set the foreground window
                                                          // as the application - see MS KB Q135788
                Application.ProcessMessages();
                // ? - SendCancelMode(nil);  
                PopupMenu.PopupComponent := Self;
                PopupMenu.Popup(csrPos.X, csrPos.Y);  // Popup TPopupMenu
                PostMessage(TheMainHandle(), WM_NULL, 0, 0);  // Post a WM_NULL message; this
                                                                 // will make sure the menu closes
                                                                 // when it loses focus - see MS KB Q135788
                msg.Result := 1;
                end;

              end;
            end;
          end;


        WM_LBUTTONDBLCLK:
          begin
          if Assigned(OnDblClick) then
            begin
            OnDblClick(self);
            end;
          end;

        WM_LBUTTONDOWN:
          begin
          if Assigned(OnClick) then
            begin
            OnClick(self);
            end;
          end;


        NIN_BALLOONSHOW:
          begin
          if Assigned(OnBalloonShow) then
            begin
            OnBalloonShow(self);
            end;
          end;

        NIN_BALLOONHIDE:
          begin
          if Assigned(OnBalloonHide) then
            begin
            OnBalloonHide(self);
            end;
          end;

        NIN_BALLOONTIMEOUT:
          begin
          if Assigned(OnBalloonTimeout) then
            begin
            OnBalloonTimeout(self);
            end;
          end;

        NIN_BALLOONUSERCLICK:
          begin
          if Assigned(OnBalloonUserClick) then
            begin
            OnBalloonUserClick(self);
            end;
          end;


        //WM_LBUTTONDOWN    : showmessage('WM_LBUTTONDOWN');
        //WM_LBUTTONUP      : showmessage('WM_LBUTTONUP');
        //WM_MBUTTONCLK     : showmessage('WM_MBUTTONCLK');
        //WM_MBUTTONDBLCLK  : showmessage('WM_MBUTTONDBLCLK');
        //WM_MBUTTONDOWN    : showmessage('WM_MBUTTONDOWN');
        //WM_MBUTTONUP      : showmessage('WM_MBUTTONUP');
        //WM_RBUTTONDBLCLK  : showmessage('WM_RBUTTONDBLCLK');
        //WM_RBUTTONDOWN    : showmessage('WM_RBUTTONDOWN');
        //WM_RBUTTONUP      : showmessage('WM_RBUTTONUP');
        //WM_MOUSEMOVE      : showmessage('WM_MOUSEMOVE');
        //WM_MOUSEWHEEL     : showmessage('WM_MOUSEWHEEL');
        //WM_RBUTTONDBLCLK  : showmessage('WM_RBUTTONDBLCLK');
        //WM_RBUTTONDOWN    : showmessage('WM_RBUTTONDOWN');
        //etc...

        end;
      end;  // WM_SDU_SYSTEMTRAYICON case

    end;  // case msg.Msg of

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.SetActive(status: boolean);
begin
  if (status <> FActive) then
    begin
    SendDebug('System tray icon component state changing to: '+SDUBoolToStr(status));
    FActive := status;

    if not(csdesigning in ComponentState) then
      begin
      AddOrRemoveTrayIcon();

      if status then
        begin
        TrapWindowMessages();
        end
      else
        begin
        UntrapWindowMessages();
        end;

      FAnimationPos := 0;
      FAnimationTimer.Enabled := (Active and AnimateIcon);
      end;

    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.SetAnimateIcon(animate: boolean);
begin
  FAnimateIcon := animate;
  if (
      Active and
      not(csdesigning in ComponentState)
     ) then
    begin
    FAnimationTimer.Enabled := animate;

    UpdateTrayIcon();
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.SetTip(tip: Ansistring);
begin
  FTip := tip;
  if (
      Active and
      not(csdesigning in ComponentState)
     ) then
    begin
    UpdateTrayIcon();
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.SetIcon(icon: TIcon);
begin
  if (icon <> nil) then
    begin
    FIcon.Assign(icon);
    if (
        Active and
        not(csdesigning in ComponentState)
       ) then
      begin
      UpdateTrayIcon();
      end;
    end;

end;


// ----------------------------------------------------------------------------
// Taken from ComCtrls.pas (TListView)
procedure TSDUSystemTrayIcon.SetAnimationIcons(Value: TImageList);
begin
  if (AnimationIcons <> Value) then
    begin
    if (AnimationIcons <> nil) then
      begin
      AnimationIcons.UnRegisterChanges(FAnimationChangeLink);
      end;

    FAnimationIcons := Value;

    if (AnimationIcons <> nil) then
      begin
      AnimationIcons.RegisterChanges(FAnimationChangeLink);
      AnimationIcons.FreeNotification(Self);
      end
    else
      begin
      Active := FALSE;
      end;

    if (
        Active and
        not(csdesigning in ComponentState)
       ) then
      begin
      UpdateTrayIcon();
      end;
    end;

end;


// ----------------------------------------------------------------------------
function TSDUSystemTrayIcon.GetAnimationDelay(): integer;
begin
  Result := FAnimationTimer.Interval;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.SetAnimationDelay(delay: integer);
begin
  FAnimationTimer.Interval := delay;
  if (
      Active and
      not(csdesigning in ComponentState)
     ) then
    begin
    UpdateTrayIcon();
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.TimerFired(Sender: TObject);
begin
  inc(FAnimationPos);
  if (FAnimationPos > (AnimationIcons.Count-1)) then
    begin
    FAnimationPos := 0;
    end;

  if (
      Active and
      not(csdesigning in ComponentState)
     ) then
    begin
    UpdateTrayIcon();
    end;

end;


// ----------------------------------------------------------------------------
// Taken from ComCtrls.pas
procedure TSDUSystemTrayIcon.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (
      (Operation = opRemove) and
      (AComponent = AnimationIcons)
     ) then
    begin
    AnimationIcons := nil;
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.AddOrRemoveTrayIcon();
begin
  if Active then
    begin
    UpdateTrayIcon(NIM_ADD);
    end
  else
    begin
    UpdateTrayIcon(NIM_DELETE);
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.UpdateTrayIcon();
begin
  UpdateTrayIcon(NIM_MODIFY);

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.UpdateTrayIcon(dwMessage: cardinal);
var
 { TODO 1 -otdk -crefactor : use _NOTIFYICONDATA_v5W for widechar }
  trayIcon: TNotifyIconData_v2;
  tmpIcon: TIcon;
begin
  tmpIcon:= TIcon.Create();
  try
    if (
        Active or
        (dwMessage = NIM_DELETE)
       ) then
      begin
      trayIcon.cbSize := sizeof(trayIcon);  // Size of struct
      trayIcon.hWnd:= FhWindow;  // Handle to window which will receive callbacks
      trayIcon.uID:= 0;
      trayIcon.uFlags:= NIF_ICON or NIF_TIP or NIF_MESSAGE;
      trayIcon.uCallbackMessage := WM_SDU_SYSTEMTRAYICON;  // The message the icon answers to

      // Fallback to application's icon
      trayIcon.hIcon:= Application.Icon.Handle;

      if not(AnimateIcon) then
        begin
        if not(FIcon.Empty) then
          begin
          trayIcon.hIcon:= FIcon.Handle;
          end;
        end
      else
        begin
        if Assigned(FAnimationIcons) then
          begin
          // Sanity check...
          if (FAnimationPos > (FAnimationIcons.Count-1)) then
            begin
            FAnimationPos := 0;
            end;

          if (FAnimationPos <= (FAnimationIcons.Count-1)) then
            begin
            FAnimationIcons.GetIcon(FAnimationPos, tmpIcon);
            trayIcon.hIcon:= tmpIcon.Handle;
            end;

          end;

        end;

      // From the Microsoft help:
      //   "Pointer to a null-terminated string with the text for a standard
      //   ToolTip. It can have a maximum of 64 characters including the
      //   terminating NULL.
      //   For Version 5.0 and later, szTip can have a maximum of 128
      //   characters, including the terminating NULL."

      StrPLCopy(trayIcon.szTip, fTip, (sizeof(trayIcon.szTip)-1));

      Shell_NotifyIcon(dwMessage, @trayIcon);
      end;

  finally
    tmpIcon.Free();
  end;

end;


// ----------------------------------------------------------------------------
procedure TSDUSystemTrayIcon.BubbleMessage(Title: AnsiChar; Info: AnsiChar; Icon: TSDUSystemTrayIconBubbleIcon; Timeout: integer);
var
  trayIcon: TNotifyIconData_v2;
begin
  CheckActive();

  trayIcon.cbSize := sizeof(trayIcon);  // Size of struct
  trayIcon.hWnd:= FhWindow;  // Handle to window which will receive callbacks
  trayIcon.uID:= 0;
  trayIcon.uFlags:= NIF_INFO;

  case Icon of
    shbiNone:    trayIcon.dwInfoFlags := NIIF_NONE;
    shbiInfo:    trayIcon.dwInfoFlags := NIIF_INFO;
    shbiWarning: trayIcon.dwInfoFlags := NIIF_WARNING;
    shbiError:   trayIcon.dwInfoFlags := NIIF_ERROR;
  else
    trayIcon.dwInfoFlags := NIIF_NONE;
  end;


  StrPLCopy(trayIcon.szInfoTitle, Title, (sizeof(trayIcon.szInfoTitle)-1));
  StrPLCopy(trayIcon.szInfo, Info, (sizeof(trayIcon.szInfo)-1));

//  trayIcon.TimeoutVersion.uTimeout := Timeout;
  trayIcon.TimeoutVersion.uTimeout := Timeout or NOTIFYICON_VERSION;
  Shell_NotifyIcon(NIM_MODIFY, @trayIcon);

end;


// ----------------------------------------------------------------------------
// See: Trapping Windows Messages in Delphi
// http://community.borland.com/article/0,1410,16487,00.html
procedure TSDUSystemTrayIcon.TrapWindowMessages();
begin
  TrapWindowMessages_AppProc();
  TrapWindowMessages_WindowProc();
end;

procedure TSDUSystemTrayIcon.TrapWindowMessages_AppProc();
begin
  // Set the new app procedure for the control and remember the old app procedure.
  // Delphi 6 and later moved MakeObjectInstance to "Classes"
{$IFDEF VER130}
  FNewAppProc := Forms.MakeObjectInstance(self.NewAppProc);
{$ELSE}
  FNewAppProc := Classes.MakeObjectInstance(self.NewAppProc);
{$ENDIF}


  FOldAppProc := Pointer(SetWindowLong(
                                       Application.Handle,
                                       GWL_WNDPROC,
                                       LongInt(FNewAppProc)
                                      ));

end;

procedure TSDUSystemTrayIcon.TrapWindowMessages_WindowProc();
begin
  // Set the new app procedure for the control and remember the old window procedure.
  if (Owner <> nil) then
    begin
    if (Owner is TWinControl) then
      begin
      // Set the new app procedure for the control and remember the old app procedure.
      // Delphi 6 and later moved MakeObjectInstance to "Classes"
{$IFDEF VER130}
      FNewWindowProc := Forms.MakeObjectInstance(self.NewWindowProc);
{$ELSE}
      FNewWindowProc := Classes.MakeObjectInstance(self.NewWindowProc);
{$ENDIF}

      FOldWindowProc := Pointer(SetWindowLong(
                                              TWinControl(Owner).Handle,
                                              GWL_WNDPROC,
                                              LongInt(FNewWindowProc)
                                             ));
      end;
    end;
    
end;


// ----------------------------------------------------------------------------
// See: Trapping Windows Messages in Delphi
// http://community.borland.com/article/0,1410,16487,00.html
procedure TSDUSystemTrayIcon.UntrapWindowMessages();
begin
  UntrapWindowMessages_AppProc();
  UntrapWindowMessages_WindowProc();
end;

procedure TSDUSystemTrayIcon.UntrapWindowMessages_AppProc();
begin
  // Set the app procedure back to the old app procedure
  if (FOldAppProc <> nil) then
    begin
    SetWindowLong(
                  Application.Handle,
                  GWL_WNDPROC,
                  LongInt(FOldAppProc)
                 );
    if (FNewAppProc <> nil) then
      begin
      // Delphi 6 and later moved FreeObjectInstance to "Classes"
{$IFDEF VER130}
      Forms.FreeObjectInstance(FNewAppProc);
{$ELSE}
      Classes.FreeObjectInstance(FNewAppProc);
{$ENDIF}
      end;
    end;

  FOldAppProc := nil;
  FNewAppProc := nil;
    
end;

procedure TSDUSystemTrayIcon.UntrapWindowMessages_WindowProc();
begin
  // Set the window procedure back to the old window procedure
  if (FOldWindowProc <> nil) then
    begin
    if (Owner <> nil) then
      begin
      if (Owner is TWinControl) then
        begin
        SetWindowLong(
                      TWinControl(Owner).Handle,
                      GWL_WNDPROC,
                      LongInt(FOldWindowProc)
                     );

        if (FNewWindowProc <> nil) then
          begin
          // Delphi 6 and later moved FreeObjectInstance to "Classes"
{$IFDEF VER130}
          Forms.FreeObjectInstance(FNewWindowProc);
{$ELSE}
          Classes.FreeObjectInstance(FNewWindowProc);
{$ENDIF}
          end;
        end;
      end;
    end;

  FOldWindowProc := nil;
  FNewWindowProc := nil;

end;


// ----------------------------------------------------------------------------
// Dump debug information out about certain window messaes received
procedure TSDUSystemTrayIcon.DebugWM(wmFrom: string; msg: TMessage);
var
  si: TStartupInfo;
  showWindowStatus: integer;
begin
  wmFrom := '------------- '+wmFrom+' -------------';


  // Debug out the window message details
  case msg.Msg of
    WM_ENDSESSION:
      begin
      SendDebug(wmFrom);
      SendDebug('WM_ENDSESSION');
      SendDebug(' - Shuting down: '+SDUBoolToStr(TWMEndSession(msg).EndSession));
      end;

    WM_SHOWWINDOW:
      begin
      SendDebug(wmFrom);
      showWindowStatus := msg.LParam;
      if (msg.LParam = SW_SHOWDEFAULT) then
        begin
        GetStartupInfo(si);
        showWindowStatus := si.wShowWindow;
        end;

      SendDebug('WM_SHOWWINDOW');
      SendDebug(' - showWindowStatus: '+inttostr(showWindowStatus));
      SendDebug('    - SW_MINIMIZE       : '+SDUBoolToStr(showWindowStatus = SW_MINIMIZE));
      SendDebug('    - SW_SHOWMINIMIZED  : '+SDUBoolToStr(showWindowStatus = SW_SHOWMINIMIZED));
      SendDebug('    - SW_SHOWMINNOACTIVE: '+SDUBoolToStr(showWindowStatus = SW_SHOWMINNOACTIVE));
      end;

    WM_SIZE:
      begin
      SendDebug(wmFrom);
      SendDebug('WM_SIZE');
      SendDebug(' - SIZE_MINIMIZED: '+SDUBoolToStr(msg.WParam = SIZE_MINIMIZED));
      end;

    WM_SYSCOMMAND:
      begin
      SendDebug(wmFrom);
      SendDebug('WM_SYSCOMMAND');
      SendDebug(' - SC_CLOSE   : '+SDUBoolToStr(msg.WParam = SC_CLOSE));
      SendDebug(' - SC_MINIMIZE: '+SDUBoolToStr(msg.WParam = SC_MINIMIZE));
      SendDebug(' - SC_ICON    : '+SDUBoolToStr(msg.WParam = SC_ICON));
      end;

//    CM_RECREATEWND:
//      begin
//      SendDebug(wmFrom);
//      SendDebug('CM_RECREATEWND');
//      end;

    WM_NCPAINT,
    WM_GETTEXT,
    WM_ERASEBKGND,
    WM_PAINT,
    WM_NCHITTEST,
    WM_SETCURSOR,
    WM_MOUSEMOVE,
    WM_MOUSEACTIVATE:
      begin
      // Ignore
      end;

    else
      begin
      SendDebug(wmFrom);
      SendDebug(SDUWMToString(msg.Msg));
      end;

  end;

end;


// ----------------------------------------------------------------------------
function TSDUSystemTrayIcon.ProcessWM(AppProcNotWindowProc: boolean; var msg: TMessage): boolean;
var
//  si: TStartupInfo;
//  showWindowStatus: integer;
  minOrClose: boolean;
  intercept: boolean;
begin
  minOrClose := FALSE;
  intercept := FALSE;

  // Process the message of your choice here
  case msg.Msg of

{
    WM_SHOWWINDOW: begin
      if (Msg.lParam = 0) and (Msg.wParam = 1) then
      begin
        // Show the taskbar icon (Windows may have shown it already)
        ShowWindow(TheMainHandle(), SW_RESTORE);
        // Bring the taskbar icon and the main form to the foreground
        SetForegroundWindow(TheMainHandle());
        SetForegroundWindow((Owner as TWinControl).Handle);
      end;
    end;
}
{
    WM_WINDOWPOSCHANGED: begin
      // Bring any modal forms owned by the main form to the foreground
      if Assigned(Screen.ActiveControl) then
        SetFocus(Screen.ActiveControl.Handle);
    end;
}

{
    WM_SHOWWINDOW:
      begin
      showWindowStatus := msg.LParam;
      if (msg.LParam = SW_SHOWDEFAULT) then
        begin
        GetStartupInfo(si);
        showWindowStatus := si.wShowWindow;
        end;

      minOrClose := (
                     (showWindowStatus = SW_MINIMIZE) or
                     (showWindowStatus = SW_SHOWMINIMIZED) or
                     (showWindowStatus = SW_SHOWMINNOACTIVE)
                    );
      minOrClose := FALSE;
      end;

    WM_WINDOWPOSCHANGED:
      begin
      end;
}

    WM_SIZE:
      begin
      minOrClose := MinimizeToIcon and (msg.WParam = SIZE_MINIMIZED);
      end;


{
    WM_CLOSE:
      begin
      minOrClose := CloseToIcon;
      intercept := CloseToIcon;
      end;
}
{
    WM_SYSCOMMAND:
      begin
      minOrClose := (
                     (msg.WParam = SC_CLOSE) or
                     (msg.WParam = SC_MINIMIZE) or
                     (msg.WParam = SC_ICON)
                    );

      intercept := (msg.WParam = SC_CLOSE);
      minOrClose := FALSE;
      intercept := FALSE;
      end;
}

// Enabling this causes the main menu to disappear!
//    WM_DESTROY:
//      begin
//      UntrapWindowMessages_WindowProc();
//      FReceivedWM_DESTROY := TRUE;
//      end;

//    WM_WINDOWPOSCHANGING:
//      begin
//      if FReceivedWM_DESTROY then
//        begin
//        TrapWindowMessages_WindowProc();
//        end;
//      end;

  end;

  if minOrClose then
    begin
    DoMinimizeToIcon();
    end;

  Result := intercept or minOrClose;
//  Result := FALSE;

  if Result then
    begin
    SendDebug('+++ Window message processed +++');
    end;

end;


// ----------------------------------------------------------------------------
// Application window message received
procedure TSDUSystemTrayIcon.NewAppProc(var msg: TMessage);
var
  handled: boolean;
begin
  DebugWM('NewAppProc', msg);
  handled := FALSE;
  if IsOnMainForm() then
    begin
    handled := ProcessWM(TRUE, msg);
    end;

  if handled then
    begin
    msg.Result := 1;
    end
  else
    begin
    // Call the old Window procedure to allow processing of the message.
    msg.Result := CallWindowProc(
                                 FOldAppProc,
                                 Application.Handle,
                                 msg.Msg,
                                 msg.WParam,
                                 msg.LParam
                                );
    end;

end;


// ----------------------------------------------------------------------------
function TSDUSystemTrayIcon.IsOnMainForm(): boolean;
begin
  Result :=
      (owner <> nil) and
{$IF CompilerVersion >= 18.5}
      (
       // If Application.MainFormOnTaskbar is set, use the form name,
       // otherwise check exactly
       (
        Application.MainFormOnTaskbar and
        (application.mainform <> nil) and
        (application.mainform.name = owner.name)
       ) or
       (
        not(Application.MainFormOnTaskbar) and
//        (application.mainform = owner)
        (application.mainform <> nil) and
        (application.mainform.name = owner.name)
       )
      )
{$ELSE}
        (application.mainform = owner)
{$IFEND}
      ;

end;


// ----------------------------------------------------------------------------
// Window window message received
procedure TSDUSystemTrayIcon.NewWindowProc(var msg: TMessage);
var
  handled: boolean;
begin
  DebugWM('NewWindowProc', msg);
  handled := FALSE;
  // Note: This is NOT the same as IsOnMainForm(...) in the previous version
{$IF CompilerVersion >= 18.5}
  if  (owner <> nil) and
      (
       // If Application.MainFormOnTaskbar is set, use the form name,
       // otherwise check exactly
       (
        Application.MainFormOnTaskbar and
        (application.mainform <> nil) and
        (application.mainform.name = owner.name)
       ) or
       (
        not(Application.MainFormOnTaskbar) and
//        (application.mainform <> owner)
        (application.mainform <> nil) and
        (application.mainform.name <> self.name)
       )
      ) then
{$ELSE}
  if ((owner <> nil) and (application.mainform <> owner)) then
{$IFEND}
    begin
    handled := ProcessWM(FALSE, msg);
    end;

  if handled then
    begin
    msg.Result := 1;
    end
  else
    begin
    // Call the old Window procedure to allow processing of the message.
    msg.Result := CallWindowProc(
                                 FOldWindowProc,
                                 TWinControl(Owner).Handle,
                                 msg.Msg,
                                 msg.WParam,
                                 msg.LParam
                                );
    end;

end;


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

END.



