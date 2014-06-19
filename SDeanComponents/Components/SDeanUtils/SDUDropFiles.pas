unit SDUDropFiles;

// File/Directory Drag Drop component
// Set "DropControl" as appropriate, and then set Active to TRUE.
// Note: If DropControl is set to nil, the parent control (e.g. frame/form)
//       will become the drop target)

// !!! WARNING !!!
// Not guaranteed to work correctly if multiple instances of this component
// are used in different threads; SetWindowsHookEx(...) uses GetCurrentThread
// to setup a hook for the thread - but it only has one SDFileDropHook

interface

uses
  Classes, Controls, Messages, Windows,
  ShellAPI;

type
  TNotifyFileDirectoryDropEvent = procedure (Sender: TObject; DropItem: string; DropPoint: TPoint) of object;
  TNotifyItemsDropEvent = procedure (Sender: TObject; DropItems: TStringList; DropPoint: TPoint) of object;

  TSDUDropFilesThreadCallback = procedure (DropItems: TStringList; DropPoint: TPoint) of object;

  TSDUDropFilesThread = class(TThread)
  protected
    procedure SyncMethod();
  public
    ItemsDropped: TStringList;
    DropPoint: TPoint;
    Callback: TSDUDropFilesThreadCallback;

    procedure AfterConstruction(); override;
    destructor Destroy(); override;
    
    procedure Execute(); override;
  end;

  TSDUDropFiles = class(TComponent)
  private
    FActive: boolean;
    FDropControl: TWinControl;

    FOnItemsDrop: TNotifyItemsDropEvent;
    FOnFileDrop: TNotifyFileDirectoryDropEvent;
    FOnDirectoryDrop: TNotifyFileDirectoryDropEvent;
  protected
    procedure ResetDropControlHandle();
    
    procedure DoOnItemsDrop(items: TStringList; dropPoint: TPoint);
    procedure DoOnFileDrop(filename: string; dropPoint: TPoint);
    procedure DoOnDirectoryDrop(dirname: string; dropPoint: TPoint);

    procedure KickOffThread(itemsDropped: TStringList; dropPoint: TPoint);
    procedure ThreadCallback(itemsDropped: TStringList; dropPoint: TPoint);

  public
    // Cached DropControl.Handle required as the DropControl may be destroyed
    // before we are!
    // Exposed as public to allow the hook callback to check it
    DropControlHandle: THandle;

    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    procedure SetActive(newActive: boolean);
    procedure SetControl(newControl: TWinControl);

    function  WMDropFiles(var msg: TMsg): boolean;

    procedure BeforeDestruction(); override;

  published
    property Active: boolean read FActive write SetActive;
    property DropControl: TWinControl read FDropControl write SetControl;
//    property Hook: HHOOK read FHook;

    // This event is called *once* for all items dropped
    // This even will be fired *before* OnFileDrop/OnDirectoryDrop.
    // The TStringList it gets passed can be modified to add/remove/modify
    // the calls made to OnFileDrop/OnDirectoryDrop
    property OnItemsDrop: TNotifyItemsDropEvent read FOnItemsDrop write FOnItemsDrop;
    // These events are called once for *each* item dropped
    property OnFileDrop: TNotifyFileDirectoryDropEvent read FOnFileDrop write FOnFileDrop;
    property OnDirectoryDrop: TNotifyFileDirectoryDropEvent read FOnDirectoryDrop write FOnDirectoryDrop;

  end;

function WMDropFilesHook(nCode: Integer; wParam: Longint; var Msg: TMsg): Longint; stdcall;

// Clear and populate "Filenames" with the filenames associated with hDrop
procedure DropFilenamesToList(hDrop: HDROP; DropFilenames: TStringList);

procedure Register;

implementation

uses
  Contnrs,
  SDUGeneral,
  SysUtils;

var
  SDFileDropHook: HHOOK;
  SDFileDropObjs: TObjectList;


// ----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUDropFiles]);
end;

// ----------------------------------------------------------------------------
// Clear and populate "Filenames" with the filenames associated with hDrop
procedure DropFilenamesToList(hDrop: HDROP; DropFilenames: TStringList);
var
  i: integer;
  cntDropped: integer;
  filenameLen: integer;
  filename: string;
begin
  DropFilenames.Clear();
  
  cntDropped := DragQueryFile(
                           hDrop,
                           $FFFFFFFF,
                           nil,
                           0
                          );

  for i:=0 to (cntDropped - 1) do
    begin
    filenameLen := DragQueryFile(
                             hDrop,
                             i,
                             nil,
                             0
                            );
    // Increment filenameLen to allow for terminating NULL
    inc(filenameLen);
    filename := StringOfChar(#0, filenameLen);
    filenameLen := DragQueryFile(
                             hDrop,
                             i,
                             PChar(filename),
                             filenameLen
                            );

    // Strip off the terminating NULL
    filename := Copy(filename, 1, filenameLen);

    DropFilenames.Add(filename);
    end;

end;

// ----------------------------------------------------------------------------
constructor TSDUDropFiles.Create(AOwner: TComponent);
begin
  inherited;

  FActive:= FALSE;
  FDropControl:= nil;
  DropControlHandle := 0;

  FOnFileDrop      := nil;
  FOnDirectoryDrop := nil;
  FOnItemsDrop     := nil;
end;


// ----------------------------------------------------------------------------
destructor TSDUDropFiles.Destroy();
begin
  inherited;
end;


// ----------------------------------------------------------------------------
procedure TSDUDropFiles.BeforeDestruction();
begin
  Active := FALSE;
  inherited;
end;


// ----------------------------------------------------------------------------
function WMDropFilesHook(nCode: Integer; wParam: Longint; var Msg: TMsg): Longint; stdcall;
var
  i: integer;
  retval: Longint;
  currDropObj: TSDUDropFiles;
begin
  if (nCode = HC_ACTION) then
    begin
    if (wParam <> PM_REMOVE) then
    begin
      if (Msg.message = WM_DROPFILES) then
        begin
        for i:=0 to (SDFileDropObjs.count - 1) do
          begin
          currDropObj := TSDUDropFiles(SDFileDropObjs[i]);
          if (msg.hWnd = currDropObj.DropControlHandle) then
            begin
            currDropObj.WMDropFiles(msg);
            break;
            end;
          end;
        end;
      end;
    end;

  retval := CallNextHookEx(
                           SDFileDropHook,
                           nCode,
                           wParam,
                           Longint(@Msg)
                          );

  Result := retval;
end;


// ----------------------------------------------------------------------------
procedure TSDUDropFiles.SetActive(newActive: boolean);
begin
  if (Active <> newActive) then
    begin
    if (DropControl = nil) then
      begin
      if (self.Owner is TWinControl) then
        begin
        DropControl := TWinControl(self.Owner);
        end;
      end;
      
    if (DropControl <> nil) then
      begin
      if newActive then
        begin
        // Update DropControlHandle - the handle could have been changed since
        // the control was assigned
        ResetDropControlHandle();

        SDFileDropObjs.Add(self);

        // If the thread's hook hasn't been setup yet; set it up now
        if (SDFileDropHook = 0) then
          begin
          SDFileDropHook := SetWindowsHookEx(
                                  WH_GETMESSAGE,
                                  @WMDropFilesHook,
                                  0,
                                  GetCurrentThreadID
                                 );
          end;

        DragAcceptFiles(DropControlHandle, TRUE);
        end
      else
        begin
        DragAcceptFiles(DropControlHandle, FALSE);
        SDFileDropObjs.Delete(SDFileDropObjs.IndexOf(self));
        if (SDFileDropObjs.Count <= 0) then
          begin
          UnhookWindowsHookEx(SDFileDropHook);
          SDFileDropHook := 0;
          end;
        end;

      end;

    FActive := newActive;
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUDropFiles.SetControl(newControl: TWinControl);
var
  prevActive: boolean;
begin
  prevActive := Active;

  Active := FALSE;

  FDropControl := newControl;

  ResetDropControlHandle();

  Active := prevActive;

end;


// ----------------------------------------------------------------------------
procedure TSDUDropFiles.ResetDropControlHandle();
begin
  if (FDropControl = nil) then
    begin
    DropControlHandle:= 0;
    end
  else
    begin
    // Reset active to new control.
    DropControlHandle:= FDropControl.Handle;
    end;

end;


// ----------------------------------------------------------------------------
// Returns TRUE if handled, otherwise FALSE
function TSDUDropFiles.WMDropFiles(var msg: TMsg): boolean;
var
  dropPoint: TPoint;
  itemsDropped: TStringList;
begin
  itemsDropped:= TStringList.Create();
  try
    DropFilenamesToList(msg.wParam, itemsDropped);

    DragQueryPoint(msg.wParam, dropPoint);
    
    DragFinish(msg.wParam);

    // Because we're still handling the Windows message, we kick off pass
    // the list of files/dirs dropped over to a thread, and complete
    // handling the WM.
    // The thread simply sync's back with us, passing back the list of
    // files/dirs dropped, and we fire off events as appropriate
    // This is more complex that you'd expect, but if we call the events
    // directly from here, and those events carry out actions such as
    // displaying messageboxes to the user, things start to crash and the
    // user's application will do "odd things"
    KickOffThread(itemsDropped, dropPoint);

  finally
    itemsDropped.Free();
  end;

  Result := TRUE;
end;

procedure TSDUDropFiles.KickOffThread(itemsDropped: TStringList; dropPoint: TPoint);
var
  thread: TSDUDropFilesThread;
begin
  thread:= TSDUDropFilesThread.Create(TRUE);

  thread.ItemsDropped.Assign(itemsDropped);
  thread.DropPoint:= dropPoint;
  thread.Callback:= ThreadCallback;

  thread.FreeOnTerminate := TRUE;

  thread.Resume();
end;

procedure TSDUDropFiles.ThreadCallback(itemsDropped: TStringList; dropPoint: TPoint);
var
  i: integer;
begin
  // Fire event with all items
  DoOnItemsDrop(itemsDropped, dropPoint);

  // Fire event with each individual item in turn
  for i:=0 to (itemsDropped.Count - 1) do
    begin
    if DirectoryExists(itemsDropped[i]) then
      begin
      DoOnDirectoryDrop(itemsDropped[i], dropPoint);
      end
    else if FileExists(itemsDropped[i]) then
      begin
      DoOnFileDrop(itemsDropped[i], dropPoint);
      end;
    end;

end;

// ----------------------------------------------------------------------------
procedure TSDUDropFiles.DoOnItemsDrop(items: TStringList; dropPoint: TPoint);
begin
  if assigned(FOnItemsDrop) then
    begin
    FOnItemsDrop(self, items, dropPoint);
    end;

end;

// ----------------------------------------------------------------------------
procedure TSDUDropFiles.DoOnFileDrop(filename: string; dropPoint: TPoint);
begin
  if assigned(FOnFileDrop) then
    begin
    FOnFileDrop(self, filename, dropPoint);
    end;

end;


// ----------------------------------------------------------------------------
procedure TSDUDropFiles.DoOnDirectoryDrop(dirname: string; dropPoint: TPoint);
begin
  if assigned(FOnDirectoryDrop) then
    begin
    FOnDirectoryDrop(self, dirname, dropPoint);
    end;

end;


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
procedure TSDUDropFilesThread.SyncMethod();
begin
  if Assigned(Callback) then
    begin
    Callback(ItemsDropped, DropPoint);
    end;
end;

procedure TSDUDropFilesThread.AfterConstruction();
begin
  inherited;
  ItemsDropped:= TStringList.Create();
  Callback := nil;
end;

destructor TSDUDropFilesThread.Destroy();
begin
  ItemsDropped.Free();
  inherited;
end;

procedure TSDUDropFilesThread.Execute();
begin
  Synchronize(SyncMethod);
end;


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

initialization
  SDFileDropHook := 0;
  SDFileDropObjs:= TObjectList.Create();
  SDFileDropObjs.OwnsObjects := FALSE;

finalization
  SDFileDropObjs.Free();

END.

