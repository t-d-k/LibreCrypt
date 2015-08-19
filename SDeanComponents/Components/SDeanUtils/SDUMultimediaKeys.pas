unit SDUMultimediaKeys;

// Multimedia keys component
// Set "MultimediaKeyControl" as appropriate, and then set Active to TRUE.
// Note: If MultimediaKeyControl is set to nil, multimedia keystokes won't be
///      restricted to a single control

// !!! WARNING !!!
// Not guaranteed to work correctly if multiple instances of this component
// are used in different threads; SetWindowsHookEx(...) uses GetCurrentThread
// to setup a hook for the thread - but it only has one SDMultimediaKeysHook

interface

uses
  Classes, Controls, Messages, Windows,
  ShellAPI;

type
  TSDUMultimediaKeysThreadCallback = procedure (cmd: WORD; uDevice: WORD; dwKeys : WORD) of object;
  TSDUMultimediaKeyEvent = procedure (Sender: TObject; var MultimediaKey: Word; Shift: TShiftState) of object;

  TSDUMultimediaKeysThread = class(TThread)
  protected
    procedure SyncMethod();
  public
    cmd: WORD;
    uDevice: WORD;
    dwKeys: WORD;
    Callback: TSDUMultimediaKeysThreadCallback;

    procedure AfterConstruction(); override;
    destructor Destroy(); override;
    
    procedure Execute(); override;
  end;

  TSDUMultimediaKeys = class(TComponent)
  private
    FActive: boolean;
    FMultimediaKeyControl: TWinControl;

    FOnMultimediaKeypress: TSDUMultimediaKeyEvent;

    FOnBrowserBackward: TNotifyEvent;
    FOnBrowserForward: TNotifyEvent;
    FOnBrowserRefresh: TNotifyEvent;
    FOnBrowserStop: TNotifyEvent;
    FOnBrowserSearch: TNotifyEvent;
    FOnBrowserFavorites: TNotifyEvent;
    FOnBrowserHome: TNotifyEvent;
    FOnVolumeMute: TNotifyEvent;
    FOnVolumeDown: TNotifyEvent;
    FOnVolumeUp: TNotifyEvent;
    FOnMediaNextTrack: TNotifyEvent;
    FOnMediaPreviousTrack: TNotifyEvent;
    FOnMediaStop: TNotifyEvent;
    FOnMediaPlayPause: TNotifyEvent;
    FOnLaunchMail: TNotifyEvent;
    FOnLaunchMediaSelect: TNotifyEvent;
    FOnLaunchApp1: TNotifyEvent;
    FOnLaunchApp2: TNotifyEvent;
    FOnBassDown: TNotifyEvent;
    FOnBassBoost: TNotifyEvent;
    FOnBassUp: TNotifyEvent;
    FOnTrebleDown: TNotifyEvent;
    FOnTrebleUp: TNotifyEvent;

  protected
    procedure ResetMultimediaKeyControlHandle();

    function  GetEventForKey(cmd: WORD): TNotifyEvent;

    procedure KickOffThread(cmd: WORD; uDevice: WORD; dwKeys : WORD);
    procedure ThreadCallback(cmd: WORD; uDevice: WORD; dwKeys : WORD);

  public
    // Cached MultimediaKeyControl.Handle required as the MultimediaKeyControl may be destroyed
    // before we are!
    // Exposed as public to allow the hook callback to check it
    MultimediaKeyControlHandle: THandle;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    procedure SetActive(newActive: boolean);
    procedure SetControl(newControl: TWinControl);

    function  WMAppCommand(lParam: DWORD): boolean;

    procedure BeforeDestruction(); override;

  published
    property Active: boolean read FActive write SetActive;
    property MultimediaKeyControl: TWinControl read FMultimediaKeyControl write SetControl;
//    property Hook: HHOOK read FHook;

    property OnMultimediaKeypress: TSDUMultimediaKeyEvent read FOnMultimediaKeypress write FOnMultimediaKeypress;

    property OnBrowserBackward: TNotifyEvent read FOnBrowserBackward write FOnBrowserBackward;
    property OnBrowserForward: TNotifyEvent read FOnBrowserForward write FOnBrowserForward;
    property OnBrowserRefresh: TNotifyEvent read FOnBrowserRefresh write FOnBrowserRefresh;
    property OnBrowserStop: TNotifyEvent read FOnBrowserStop write FOnBrowserStop;
    property OnBrowserSearch: TNotifyEvent read FOnBrowserSearch write FOnBrowserSearch;
    property OnBrowserFavorites: TNotifyEvent read FOnBrowserFavorites write FOnBrowserFavorites;
    property OnBrowserHome: TNotifyEvent read FOnBrowserHome write FOnBrowserHome;
    property OnVolumeMute: TNotifyEvent read FOnVolumeMute write FOnVolumeMute;
    property OnVolumeDown: TNotifyEvent read FOnVolumeDown write FOnVolumeDown;
    property OnVolumeUp: TNotifyEvent read FOnVolumeUp write FOnVolumeUp;
    property OnMediaNextTrack: TNotifyEvent read FOnMediaNextTrack write FOnMediaNextTrack;
    property OnMediaPreviousTrack: TNotifyEvent read FOnMediaPreviousTrack write FOnMediaPreviousTrack;
    property OnMediaStop: TNotifyEvent read FOnMediaStop write FOnMediaStop;
    property OnMediaPlayPause: TNotifyEvent read FOnMediaPlayPause write FOnMediaPlayPause;
    property OnLaunchMail: TNotifyEvent read FOnLaunchMail write FOnLaunchMail;
    property OnLaunchMediaSelect: TNotifyEvent read FOnLaunchMediaSelect write FOnLaunchMediaSelect;
    property OnLaunchApp1: TNotifyEvent read FOnLaunchApp1 write FOnLaunchApp1;
    property OnLaunchApp2: TNotifyEvent read FOnLaunchApp2 write FOnLaunchApp2;
    property OnBassDown: TNotifyEvent read FOnBassDown write FOnBassDown;
    property OnBassBoost: TNotifyEvent read FOnBassBoost write FOnBassBoost;
    property OnBassUp: TNotifyEvent read FOnBassUp write FOnBassUp;
    property OnTrebleDown: TNotifyEvent read FOnTrebleDown write FOnTrebleDown;
    property OnTrebleUp: TNotifyEvent read FOnTrebleUp write FOnTrebleUp;

  end;



function WMAppCommandHook(nCode: Integer; wParam: DWORD; lParam: DWORD): Longint; stdcall;

procedure Register;

implementation

uses
  Contnrs,
  SDUGeneral,
  SysUtils;


  const
  // From winuser.h:
  FAPPCOMMAND_MOUSE                  = $8000;
  FAPPCOMMAND_KEY                    = 0;
  FAPPCOMMAND_OEM                    = $1000;
  FAPPCOMMAND_MASK                   = $F000;

  // From winuser.h:
  APPCOMMAND_BROWSER_BACKWARD        = 1;
  APPCOMMAND_BROWSER_FORWARD         = 2;
  APPCOMMAND_BROWSER_REFRESH         = 3;
  APPCOMMAND_BROWSER_STOP            = 4;
  APPCOMMAND_BROWSER_SEARCH          = 5;
  APPCOMMAND_BROWSER_FAVORITES       = 6;
  APPCOMMAND_BROWSER_HOME            = 7;
  APPCOMMAND_VOLUME_MUTE             = 8;
  APPCOMMAND_VOLUME_DOWN             = 9;
  APPCOMMAND_VOLUME_UP               = 10;
  APPCOMMAND_MEDIA_NEXTTRACK         = 11;
  APPCOMMAND_MEDIA_PREVIOUSTRACK     = 12;
  APPCOMMAND_MEDIA_STOP              = 13;
  APPCOMMAND_MEDIA_PLAY_PAUSE        = 14;
  APPCOMMAND_LAUNCH_MAIL             = 15;
  APPCOMMAND_LAUNCH_MEDIA_SELECT     = 16;
  APPCOMMAND_LAUNCH_APP1             = 17;
  APPCOMMAND_LAUNCH_APP2             = 18;
  APPCOMMAND_BASS_DOWN               = 19;
  APPCOMMAND_BASS_BOOST              = 20;
  APPCOMMAND_BASS_UP                 = 21;
  APPCOMMAND_TREBLE_DOWN             = 22;
  APPCOMMAND_TREBLE_UP               = 23;

var
  SDMultimediaKeysHook: HHOOK;
  SDMultimediaKeysObjs: TObjectList;


// ----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUMultimediaKeys]);
end;


// ----------------------------------------------------------------------------
constructor TSDUMultimediaKeys.Create(AOwner: TComponent);
begin
  inherited;

  FActive:= FALSE;
  FMultimediaKeyControl:= nil;
  MultimediaKeyControlHandle := 0;

  FOnMultimediaKeypress  := nil;
    
  FOnBrowserBackward     := nil;
  FOnBrowserForward      := nil;
  FOnBrowserRefresh      := nil;
  FOnBrowserStop         := nil;
  FOnBrowserSearch       := nil;
  FOnBrowserFavorites    := nil;
  FOnBrowserHome         := nil;
  FOnVolumeMute          := nil;
  FOnVolumeDown          := nil;
  FOnVolumeUp            := nil;
  FOnMediaNextTrack      := nil;
  FOnMediaPreviousTrack  := nil;
  FOnMediaStop           := nil;
  FOnMediaPlayPause      := nil;
  FOnLaunchMail          := nil;
  FOnLaunchMediaSelect   := nil;
  FOnLaunchApp1          := nil;
  FOnLaunchApp2          := nil;
  FOnBassDown            := nil;
  FOnBassBoost           := nil;
  FOnBassUp              := nil;
  FOnTrebleDown          := nil;
  FOnTrebleUp            := nil;

end;


// ----------------------------------------------------------------------------
destructor TSDUMultimediaKeys.Destroy();
begin
  inherited;
end;


// ----------------------------------------------------------------------------
procedure TSDUMultimediaKeys.BeforeDestruction();
begin
  Active := FALSE;
  inherited;
end;


// ----------------------------------------------------------------------------
function WMAppCommandHook(nCode: Integer; wParam: DWORD; lParam: DWORD): Longint; stdcall;
var
  i: integer;
  retval: Longint;
  handled: boolean;
  currObj: TSDUMultimediaKeys;
begin
  handled := FALSE;
  if (nCode = HSHELL_APPCOMMAND) then
    begin
    for i:=0 to (SDMultimediaKeysObjs.count - 1) do
      begin
      currObj := TSDUMultimediaKeys(SDMultimediaKeysObjs[i]);
      if (
          (wParam = currObj.MultimediaKeyControlHandle) or
          (currObj.MultimediaKeyControlHandle = 0)
         ) then
        begin
        currObj.WMAppCommand(lParam);
        handled := TRUE;
        break;
        end;
      end;
    end;

  if handled then
    begin
    retval := 1;
    end
  else
    begin
    retval := CallNextHookEx(
                             SDMultimediaKeysHook,
                             nCode,
                             wParam,
                             lParam
                            );
    end;

  Result := retval;
end;


// ----------------------------------------------------------------------------
procedure TSDUMultimediaKeys.SetActive(newActive: boolean);
begin
  if (Active <> newActive) then
    begin
    if newActive then
      begin
      // Update DropControlHandle - the handle could have been changed since
      // the control was assigned
      ResetMultimediaKeyControlHandle();

      // If not restricted to a certain control, add to end, so any more
      // instances of this component relating to a specific control get the
      // event first
      if (MultimediaKeyControlHandle = 0) then
        begin
        SDMultimediaKeysObjs.Add(self);
        end
      else
        begin
        SDMultimediaKeysObjs.Insert(0, self);
        end;

      // If the thread's hook hasn't been setup yet; set it up now
      if (SDMultimediaKeysHook = 0) then
        begin
        SDMultimediaKeysHook := SetWindowsHookEx(
                                WH_SHELL,
                                @WMAppCommandHook,
                                0,
                                GetCurrentThreadID
                               );
        end;
          
      end
    else
      begin
      SDMultimediaKeysObjs.Delete(SDMultimediaKeysObjs.IndexOf(self));
      if (SDMultimediaKeysObjs.Count <= 0) then
        begin
        UnhookWindowsHookEx(SDMultimediaKeysHook);
        SDMultimediaKeysHook := 0;
        end;
      end;

    FActive := newActive;
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUMultimediaKeys.SetControl(newControl: TWinControl);
var
  prevActive: boolean;
begin
  prevActive := Active;

  Active := FALSE;

  FMultimediaKeyControl := newControl;

  ResetMultimediaKeyControlHandle();

  Active := prevActive;

end;


// ----------------------------------------------------------------------------
procedure TSDUMultimediaKeys.ResetMultimediaKeyControlHandle();
begin
  if (FMultimediaKeyControl = nil) then
    begin
    MultimediaKeyControlHandle:= 0;
    end
  else
    begin
    MultimediaKeyControlHandle:= FMultimediaKeyControl.Handle;
    end;

end;


// ----------------------------------------------------------------------------
// Returns TRUE if handled, otherwise FALSE
function TSDUMultimediaKeys.WMAppCommand(lParam: DWORD): boolean;

  function GET_APPCOMMAND_LPARAM(lParam: Integer): Word;
  begin
    Result := HiWord(lParam) and not FAPPCOMMAND_MASK;
  end;

  function GET_DEVICE_LPARAM(lParam: Integer): Word;
  begin
    Result := HiWord(lParam) and FAPPCOMMAND_MASK;
  end;

  function GET_FLAGS_LPARAM(lParam: Integer): Word;
  begin
    Result := LoWord(lParam);
  end;

  function GET_KEYSTATE_LPARAM(lParam: Integer): Word;
  begin
    Result := GET_FLAGS_LPARAM(lParam);
  end;

var
  cmd: WORD;
  uDevice: WORD;
  dwKeys: WORD;
begin
  cmd:= GET_APPCOMMAND_LPARAM(LParam);
  uDevice:= GET_DEVICE_LPARAM(LParam);
  dwKeys:= GET_KEYSTATE_LPARAM(LParam);

  // Because we're still handling the Windows message, we kick off pass
  // the list of files/dirs dropped over to a thread, and complete
  // handling the WM.
  // The thread simply sync's back with us, passing back the list of
  // files/dirs dropped, and we fire off events as appropriate
  // This is more complex that you'd expect, but if we call the events
  // directly from here, and those events carry out actions such as
  // displaying messageboxes to the user, things start to crash and the
  // user's application will do "odd things"
  KickOffThread(cmd, uDevice, dwKeys);

  Result := TRUE;
end;

procedure TSDUMultimediaKeys.KickOffThread(cmd: WORD; uDevice: WORD; dwKeys : WORD);
var
  thread: TSDUMultimediaKeysThread;
begin
  thread:= TSDUMultimediaKeysThread.Create(TRUE);

  thread.cmd := cmd;
  thread.uDevice := uDevice;
  thread.dwKeys := dwKeys;
  thread.Callback:= ThreadCallback;

  thread.FreeOnTerminate := TRUE;

  thread.Resume();
end;


// ----------------------------------------------------------------------------
function TSDUMultimediaKeys.GetEventForKey(cmd: WORD): TNotifyEvent;
var
  retval: TNotifyEvent;
begin
  retval := nil;

  case cmd of
    APPCOMMAND_BROWSER_BACKWARD:     retval := FOnBrowserBackward;
    APPCOMMAND_BROWSER_FORWARD:      retval := FOnBrowserForward;
    APPCOMMAND_BROWSER_REFRESH:      retval := FOnBrowserRefresh;
    APPCOMMAND_BROWSER_STOP:         retval := FOnBrowserStop;
    APPCOMMAND_BROWSER_SEARCH:       retval := FOnBrowserSearch;
    APPCOMMAND_BROWSER_FAVORITES:    retval := FOnBrowserFavorites;
    APPCOMMAND_BROWSER_HOME:         retval := FOnBrowserHome;
    APPCOMMAND_VOLUME_MUTE:          retval := FOnVolumeMute;
    APPCOMMAND_VOLUME_DOWN:          retval := FOnVolumeDown;
    APPCOMMAND_VOLUME_UP:            retval := FOnVolumeUp;
    APPCOMMAND_MEDIA_NEXTTRACK:      retval := FOnMediaNextTrack;
    APPCOMMAND_MEDIA_PREVIOUSTRACK:  retval := FOnMediaPreviousTrack;
    APPCOMMAND_MEDIA_STOP:           retval := FOnMediaStop;
    APPCOMMAND_MEDIA_PLAY_PAUSE:     retval := FOnMediaPlayPause;
    APPCOMMAND_LAUNCH_MAIL:          retval := FOnLaunchMail;
    APPCOMMAND_LAUNCH_MEDIA_SELECT:  retval := FOnLaunchMediaSelect;
    APPCOMMAND_LAUNCH_APP1:          retval := FOnLaunchApp1;
    APPCOMMAND_LAUNCH_APP2:          retval := FOnLaunchApp2;
    APPCOMMAND_BASS_DOWN:            retval := FOnBassDown;
    APPCOMMAND_BASS_BOOST:           retval := FOnBassBoost;
    APPCOMMAND_BASS_UP:              retval := FOnBassUp;
    APPCOMMAND_TREBLE_DOWN:          retval := FOnTrebleDown;
    APPCOMMAND_TREBLE_UP:            retval := FOnTrebleUp;
  end;

  Result := retval;
end;


// ----------------------------------------------------------------------------
procedure TSDUMultimediaKeys.ThreadCallback(cmd: WORD; uDevice: WORD; dwKeys : WORD);
var
  event: TNotifyEvent;
  shiftState: TShiftState;
begin
  shiftState := [];

  if ((dwKeys and MK_CONTROL) = MK_CONTROL) then
    begin
    shiftState := shiftState + [ssCtrl];
    end;
  if ((dwKeys and MK_LBUTTON) = MK_LBUTTON) then
    begin
    shiftState := shiftState + [ssLeft];
    end;
  if ((dwKeys and MK_MBUTTON) = MK_MBUTTON) then
    begin
    shiftState := shiftState + [ssMiddle];
    end;
  if ((dwKeys and MK_RBUTTON) = MK_RBUTTON) then
    begin
    shiftState := shiftState + [ssRight];
    end;
  if ((dwKeys and MK_SHIFT) = MK_SHIFT) then
    begin
    shiftState := shiftState + [ssShift];
    end;
{
  if ((dwKeys and MK_XBUTTON1) = MK_XBUTTON1) then
    begin
    shiftState := shiftState + [???];
    end;
  if ((dwKeys and MK_XBUTTON2) = MK_XBUTTON2) then
    begin
    shiftState := shiftState + [???];
    end;
}

  if assigned(FOnMultimediaKeypress) then
    begin
    FOnMultimediaKeypress(self, cmd, shiftState);
    end;

  event := GetEventForKey(cmd);
  if assigned(event) then
    begin
    event(self);
    end;

end;


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
procedure TSDUMultimediaKeysThread.SyncMethod();
begin
  if Assigned(Callback) then
    begin
    Callback(cmd, uDevice, dwKeys);
    end;
end;

procedure TSDUMultimediaKeysThread.AfterConstruction();
begin
  inherited;
end;

destructor TSDUMultimediaKeysThread.Destroy();
begin
  inherited;
end;

procedure TSDUMultimediaKeysThread.Execute();
begin
  Synchronize(SyncMethod);
end;


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

initialization
  SDMultimediaKeysHook := 0;
  SDMultimediaKeysObjs:= TObjectList.Create();
  SDMultimediaKeysObjs.OwnsObjects := FALSE;

finalization
  SDMultimediaKeysObjs.Free();

END.

