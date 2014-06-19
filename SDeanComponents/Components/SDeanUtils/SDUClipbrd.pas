unit SDUClipbrd;

interface

uses
dialogs,
  Classes, Types, Windows, Messages, ShlObj;

type
  // Based on:
  //   http://delphi.about.com/od/windowsshellapi/a/clipboard_spy_2.htm
  // and Delphi's TTimer
  //
  TSDUClipboardMonitor = class(TComponent)
  private
    FWindowHandle: HWND;
    FNextInChain: THandle;
    FOnChanged: TNotifyEvent;
    FEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    procedure SetOnChanged(Value: TNotifyEvent);
    procedure WndProc(var Msg: TMessage);
  protected
    procedure DoChanged; dynamic;

    procedure WMDrawClipboard(var Msg:TMessage);
    procedure WMChangeCBChain(var Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default TRUE;
    property OnChanged: TNotifyEvent read FOnChanged write SetOnChanged;
  end;

  TFileDescriptorArray = array of FILEDESCRIPTOR;



function SDUClipboardFormatToStr(clipboardFmt: Word): string;

function SDUClearClipboard(): boolean;

// Simple methods...
function SDUSetDWORDOnClipboard(uFormat: UINT; Value: DWORD): boolean;
function SDUGetDWORDFromClipboard(uFormat: UINT; out Value: DWORD): boolean;

// Drag/drop related...
function SDUSetDropFilesOnClipboard(stlFilenames: TStringList): boolean; overload;
function SDUSetDropFilesOnClipboard(stlFilenames: TStringList; dropPoint: TPoint): boolean; overload;
function SDUGetDropFilesFromClipboard(stlFilenames: TStringList): boolean; overload;
function SDUGetDropFilesFromClipboard(stlFilenames: TStringList; out dropPoint: TPoint): boolean; overload;

function SDUSetPreferredDropEffectOnClipboard(dropEffect: DWORD): boolean;
function SDUGetPreferredDropEffectFromClipboard(out dropEffect: DWORD): boolean;

function SDUGetFileDescriptorArrayFromClipboard(out fileDescriptors: TFileDescriptorArray): boolean;
function SDUSetFileDescriptorArrayFromClipboard(fileDescriptors: TFileDescriptorArray): boolean;

function SDUGetFileContentFromClipboard(out data: string): boolean;


var
  // Formats for Transferring File System Objects
  // CF_HDROP: Word;
  CF_FILECONTENTS: Word;
  CF_FILEDESCRIPTOR: Word;
  CF_FILEDESCRIPTORA: Word;
  CF_FILEDESCRIPTORW: Word;
  CF_FILENAME: Word;
  CF_FILENAMEA: Word;
  CF_FILENAMEW: Word;
  CF_FILENAMEMAP: Word;
  CF_FILENAMEMAPA: Word;
  CF_FILENAMEMAPW: Word;
//  CF_MOUNTEDVOLUME: Word;
  CF_SHELLIDLIST: Word;
  CF_SHELLIDLISTOFFSET: Word;
  // Formats for Transferring Virtual Objects: Word;
  CF_NETRESOURCES: Word;
  CF_PRINTERGROUP: Word;
//  CF_INETURL: Word;
//  CF_INETURLA: Word;
//  CF_INETURLW: Word;
  CF_SHELLURL: Word; // deprecated;
  // Formats for Communication Between Source and Target: Word;
  CF_INDRAGLOOP: Word;
//  CF_DRAGCONTEXT: Word;
//  CF_PERSISTEDDATAOBJECT: Word;
//  CF_LOGICALPERFORMEDDROPEFFECT: Word;
  CF_PASTESUCCEEDED: Word;
  CF_PERFORMEDDROPEFFECT: Word;
  CF_PREFERREDDROPEFFECT: Word;
//  CF_TARGETCLSID: Word;
//  CF_UNTRUSTEDDRAGDROP: Word;
//  CF_AUTOPLAY_SHELLIDLISTS: Word;

procedure Register;

implementation

uses
  Clipbrd, SysUtils, ShellAPI, ActiveX, Forms,
  SDUDropFiles;

// Forward declarations...
function _SDUSetDropFilesOnClipboard(stlFilenames: TStringList; nonClient: boolean; dropPoint: TPoint): boolean; forward;

// ----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUClipboardMonitor]);
end;

function SDUClipboardFormatToStr(clipboardFmt: Word): string;
const
  NAME_BUFFER_SIZE = 1024;
var
  retval: string;
begin
  retval := '';

  retval := stringofchar(#0, NAME_BUFFER_SIZE);
  GetClipboardFormatName(
                         clipboardFmt,
                         PChar(retval),
                         NAME_BUFFER_SIZE
                        );

  retval := copy(retval, 1, strlen(PChar(retval)));

  if (retval = '') then
    begin
    case clipboardFmt of
      CF_TEXT:             retval := 'CF_TEXT';
      CF_BITMAP:           retval := 'CF_BITMAP';
      CF_METAFILEPICT:     retval := 'CF_METAFILEPICT';
      CF_SYLK:             retval := 'CF_SYLK';
      CF_DIF:              retval := 'CF_DIF';
      CF_TIFF:             retval := 'CF_TIFF';
      CF_OEMTEXT:          retval := 'CF_OEMTEXT';
      CF_DIB:              retval := 'CF_DIB';
      CF_PALETTE:          retval := 'CF_PALETTE';
      CF_PENDATA:          retval := 'CF_PENDATA';
      CF_RIFF:             retval := 'CF_RIFF';
      CF_WAVE:             retval := 'CF_WAVE';
      CF_UNICODETEXT:      retval := 'CF_UNICODETEXT';
      CF_ENHMETAFILE:      retval := 'CF_ENHMETAFILE';
      CF_HDROP:            retval := 'CF_HDROP';
      CF_LOCALE:           retval := 'CF_LOCALE';
      CF_DIBV5:            retval := 'CF_DIBV5';
      CF_OWNERDISPLAY:     retval := 'CF_OWNERDISPLAY';
      CF_DSPTEXT:          retval := 'CF_DSPTEXT';
      CF_DSPBITMAP:        retval := 'CF_DSPBITMAP';
      CF_DSPMETAFILEPICT:  retval := 'CF_DSPMETAFILEPICT';
      CF_DSPENHMETAFILE:   retval := 'CF_DSPENHMETAFILE';
    end;

    if (
        (clipboardFmt >= CF_PRIVATEFIRST) and
        (clipboardFmt <= CF_PRIVATELAST)
       ) then
      begin
      retval := 'CF_PRIVATE...';
      end;

    if (
        (clipboardFmt >= CF_GDIOBJFIRST) and
        (clipboardFmt <= CF_GDIOBJLAST)
       ) then
      begin
      retval := 'CF_GDIOBJ...';
      end;

    end;

  if (retval = '') then
    begin
    retval := '0x'+inttohex(clipboardFmt, 8);
    end;

  Result := retval;
end;


function SDUClearClipboard(): boolean;
var
  retval: boolean;
begin
  Clipboard.Open();
  try
    Clipboard.Clear();
    retval := TRUE;
  finally
    Clipboard.Close();
  end;

  Result := retval;
end;

function SDUSetDropFilesOnClipboard(stlFilenames: TStringList): boolean;
var
  tmpPoint: TPoint;
begin
  tmpPoint.X := 0;
  tmpPoint.Y := 0;
  Result := _SDUSetDropFilesOnClipboard(stlFilenames, TRUE, tmpPoint);
end;

function SDUSetDropFilesOnClipboard(stlFilenames: TStringList; dropPoint: TPoint): boolean;
begin
  Result := _SDUSetDropFilesOnClipboard(stlFilenames, FALSE, dropPoint);
end;

function _SDUSetDropFilesOnClipboard(stlFilenames: TStringList; nonClient: boolean; dropPoint: TPoint): boolean;
var
  dropFiles: PDropFiles;
  hGlobal: THandle;
  itemsAsString: string;
  allocSize: integer;
  i: integer;
  retval: boolean;
begin
  retval := FALSE;

  itemsAsString := '';
  for i:=0 to (stlFilenames.count - 1) do
    begin
    itemsAsString := itemsAsString + ExcludeTrailingPathDelimiter(stlFilenames[i]) + #0;
    end;
  // Add on terminating NULL
  itemsAsString := itemsAsString + #0;

  allocSize := sizeof(dropFiles^) + Length(itemsAsString);

  hGlobal := GlobalAlloc(
                         (
                          GMEM_SHARE or
                          GMEM_MOVEABLE or
                          GMEM_ZEROINIT
                         ),
                         allocSize
                        );
  if (hGlobal <> 0) then
    begin
    dropFiles := GlobalLock(hGlobal);

    dropFiles^.pFiles := sizeof(dropFiles^);
    dropFiles^.fWide := FALSE;
    dropFiles^.fNC := nonClient;
    dropFiles^.pt := dropPoint;

    // Note: This one *must* be done *after* the record's changed, or they
    // get overwritten
    Move(itemsAsString[1], (PChar(dropFiles) + sizeof(dropFiles^))^, length(itemsAsString));

    GlobalUnlock(hGlobal);

    Clipboard.Open();
    try
      // DON'T USE Clipboard.SetAsHandle(...) as this can clear the existing
      // contents
//      Clipboard.SetAsHandle(CF_HDROP, hGlobal);
      SetClipboardData(CF_HDROP, hGlobal);
      retval := TRUE;
    finally
      Clipboard.Close();
    end;

    if not(retval) then
      begin
      GlobalFree(hGlobal);
      end;

    end;

  Result := retval;
end;

function SDUGetDropFilesFromClipboard(stlFilenames: TStringList): boolean;
var
  tmpPoint: TPoint;
begin
  Result := SDUGetDropFilesFromClipboard(stlFilenames, tmpPoint);
end;

function SDUGetDropFilesFromClipboard(stlFilenames: TStringList; out dropPoint: TPoint): boolean;
var
  hGlobal: THandle;
  retval: boolean;
begin
  retval := FALSE;

  stlFilenames.Clear();

  Clipboard.Open();
  try
    if Clipboard.HasFormat(CF_HDROP) then
      begin
      hGlobal := Clipboard.GetAsHandle(CF_HDROP);

      // DragQueryFile uses hGlobal directly; not the memory it relates to, so
      // don't need GlobalLock(...)/GlobalUnlock(...)
      //    dropFiles := GlobalLock(hGlobal);

      DropFilenamesToList(hGlobal, stlFilenames);

      DragQueryPoint(hGlobal, dropPoint);

      // DragQueryFile uses hGlobal directly; not the memory it relates to, so
      // don't need GlobalLock(...)/GlobalUnlock(...)
      //    dropFiles := GlobalUnlock(hGlobal);
      
      retval := TRUE;
      end;

  finally
    Clipboard.Close();
  end;

  Result := retval;
end;



function SDUSetPreferredDropEffectOnClipboard(dropEffect: DWORD): boolean;
begin
  Result := SDUSetDWORDOnClipboard(CF_PREFERREDDROPEFFECT, dropEffect);
end;

function SDUSetDWORDOnClipboard(uFormat: UINT; Value: DWORD): boolean;
var
  hGlobal: THandle;
  retval: boolean;
  pdw: PDWORD;
begin
  retval := FALSE;

  hGlobal := GlobalAlloc(
                         (
                          GMEM_SHARE or
                          GMEM_MOVEABLE or
                          GMEM_ZEROINIT
                         ),
                         sizeof(Value)
                        );
  if (hGlobal <> 0) then
    begin
    pdw := GlobalLock(hGlobal);
    pdw^ := Value;
    GlobalUnlock(hGlobal);

    Clipboard.Open();
    try
      // DON'T USE Clipboard.SetAsHandle(...) as this can clear the existing
      // contents
//      Clipboard.SetAsHandle(uFormat, hGlobal);
      SetClipboardData(uFormat, hGlobal);
      retval := TRUE;
    finally
      Clipboard.Close();
    end;

    if not(retval) then
      begin
      GlobalFree(hGlobal);
      end;

    end;

  Result := retval;
end;


function SDUGetPreferredDropEffectFromClipboard(out dropEffect: DWORD): boolean;
begin
  Result := SDUGetDWORDFromClipboard(CF_PREFERREDDROPEFFECT, dropEffect);
end;

function SDUGetDWORDFromClipboard(uFormat: UINT; out Value: DWORD): boolean;
var
  hGlobal: THandle;
  retval: boolean;
  pdw: PDWORD;
begin
  retval := FALSE;

  Clipboard.Open();
  try
    if Clipboard.HasFormat(uFormat) then
      begin
      hGlobal := Clipboard.GetAsHandle(uFormat);

      if (hGlobal <> 0) then
        begin
        pdw := GlobalLock(hGlobal);
        if (pdw <> nil) then
          begin
          Value := pdw^;
          GlobalUnlock(hGlobal);
          end;
        end;
      
      retval := TRUE;
      end;

  finally
    Clipboard.Close();
  end;

  Result := retval;
end;


function SDUGetFileDescriptorArrayFromClipboard(out fileDescriptors: TFileDescriptorArray): boolean;
var
  hGlobal: THandle;
  retval: boolean;
  ptrFGD: PFileGroupDescriptor;
  ptrFileDesc: PFileDescriptor;
  i: integer;
begin
  retval := FALSE;

  Clipboard.Open();
  try
    if Clipboard.HasFormat(CF_FILEDESCRIPTOR) then
      begin
      hGlobal := Clipboard.GetAsHandle(CF_FILEDESCRIPTOR);

      if (hGlobal <> 0) then
        begin
        ptrFGD := GlobalLock(hGlobal);
        if (ptrFGD <> nil) then
          begin
          SetLength(fileDescriptors, ptrFGD.cItems);
          ptrFileDesc := @(ptrFGD.fgd);
          for i:=low(fileDescriptors) to high(fileDescriptors) do
            begin
            fileDescriptors[i] := ptrFileDesc^;
            ptrFileDesc := PFileDescriptor(PChar(ptrFileDesc) + sizeof(ptrFileDesc^));
            end;

          GlobalUnlock(hGlobal);
          end;
        end;
      
      retval := TRUE;
      end;

  finally
    Clipboard.Close();
  end;

  Result := retval;
end;

function SDUSetFileDescriptorArrayFromClipboard(fileDescriptors: TFileDescriptorArray): boolean;
var
  hGlobal: THandle;
  retval: boolean;
  allocSize: integer;
  ptrFGD: PFileGroupDescriptor;
  ptrFileDesc: PFileDescriptor;
  i: integer;
begin
  retval := FALSE;

  allocSize := (
                sizeof(ptrFGD^)
                - sizeof(ptrFGD.fgd)
                + (sizeof(ptrFGD.fgd) * length(fileDescriptors))
               );

  hGlobal := GlobalAlloc(
                         (
                          GMEM_SHARE or
                          GMEM_MOVEABLE or
                          GMEM_ZEROINIT
                         ),
                         allocSize
                        );
  if (hGlobal <> 0) then
    begin
    ptrFGD := GlobalLock(hGlobal);

    if (ptrFGD <> nil) then
      begin
      ptrFGD.cItems := length(fileDescriptors);
      
      ptrFileDesc := @(ptrFGD.fgd);
      for i:=low(fileDescriptors) to high(fileDescriptors) do
        begin
        ptrFileDesc^ := fileDescriptors[i];
        ptrFileDesc := PFileDescriptor(PChar(ptrFileDesc) + sizeof(ptrFileDesc^));
        end;

      GlobalUnlock(hGlobal);
      end;

    Clipboard.Open();
    try
      // DON'T USE Clipboard.SetAsHandle(...) as this can clear the existing
      // contents
//      Clipboard.SetAsHandle(CF_FILEDESCRIPTOR, hGlobal);
      SetClipboardData(CF_FILEDESCRIPTOR, hGlobal);
      retval := TRUE;
    finally
      Clipboard.Close();
    end;

    if not(retval) then
      begin
      GlobalFree(hGlobal);
      end;

    end;

  Result := retval;
end;

function SDUGetFileContentFromClipboard(out data: string): boolean;
var
  hGlobal: THandle;
  retval: boolean;
  ptrFGD: PFileGroupDescriptor;
//  ptrFileDesc: PFileDescriptor;
//  i: integer;
begin
  retval := FALSE;

  Clipboard.Open();
  try
    if Clipboard.HasFormat(CF_FILECONTENTS) then
      begin
      hGlobal := Clipboard.GetAsHandle(CF_FILECONTENTS);

      if (hGlobal <> 0) then
        begin
        ptrFGD := GlobalLock(hGlobal);
        if (ptrFGD <> nil) then
          begin
          data:='wer';
{
          SetLength(fileDescriptors, ptrFGD.cItems);
          ptrFileDesc := @(ptrFGD.fgd);
          for i:=low(fileDescriptors) to high(fileDescriptors) do
            begin
            fileDescriptors[i] := ptrFileDesc^;
            ptrFileDesc := PFileDescriptor(PChar(ptrFileDesc) + sizeof(ptrFileDesc^));
            end;

}
          GlobalUnlock(hGlobal);
          end;
        end;
      
      retval := TRUE;
      end;

  finally
    Clipboard.Close();
  end;

  Result := retval;
end;


// ----------------------------------------------------------------------------

constructor TSDUClipboardMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNextInChain := 0;
{$IFDEF MSWINDOWS}
  FWindowHandle := Classes.AllocateHWnd(WndProc);
{$ENDIF}
{$IFDEF LINUX}
  FWindowHandle := WinUtils.AllocateHWnd(WndProc);
{$ENDIF}
  Enabled := TRUE;
end;

destructor TSDUClipboardMonitor.Destroy;
begin
  Enabled := False;
{$IFDEF MSWINDOWS}
  Classes.DeallocateHWnd(FWindowHandle);
{$ENDIF}
{$IFDEF LINUX}
  WinUtils.DeallocateHWnd(FWindowHandle);
{$ENDIF}   
  inherited Destroy;
end;

procedure TSDUClipboardMonitor.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_DrawClipboard) then
    begin
    try
      WMDrawClipboard(Msg);
    except
      Application.HandleException(Self);
    end
    end
  else if (Msg.Msg = WM_ChangeCBChain) then
    begin
    try
      WMChangeCBChain(Msg);
    except
      Application.HandleException(Self);
    end
    end
  else
    begin
    Msg.Result := DefWindowProc(FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
    end;
end;

procedure TSDUClipboardMonitor.WMChangeCBChain(var Msg: TMessage);
var
  Remove, Next: THandle;
begin
  Remove := Msg.WParam;
  Next := Msg.LParam;
 with Msg do
  if FNextInChain = Remove then
   FNextInChain := Next
  else if FNextInChain <> 0 then
   SendMessage(FNextInChain, WM_ChangeCBChain, Remove, Next)
end;

procedure TSDUClipboardMonitor.WMDrawClipboard(var Msg:TMessage);
begin
  if assigned(FOnChanged) then
    begin
    FOnChanged(self);
    end;

  //pass the message on to the next window
  if FNextInChain <> 0 then
    begin
    SendMessage(FNextInChain, WM_DrawClipboard, 0, 0)
    end;
end;

procedure TSDUClipboardMonitor.SetEnabled(Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;

    if FEnabled then
      begin
      FNextInChain := SetClipboardViewer(FWindowHandle);
      end
    else
      begin
      ChangeClipboardChain(FWindowHandle, FNextInChain);
      end;

  end;

end;

procedure TSDUClipboardMonitor.SetOnChanged(Value: TNotifyEvent);
begin
  FOnChanged := Value;
end;

procedure TSDUClipboardMonitor.DoChanged;
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------


initialization

  // Setup global vars...
  CF_SHELLIDLIST                 := RegisterClipboardFormat(CFSTR_SHELLIDLIST);
  CF_SHELLIDLISTOFFSET           := RegisterClipboardFormat(CFSTR_SHELLIDLISTOFFSET);
  CF_NETRESOURCES                := RegisterClipboardFormat(CFSTR_NETRESOURCES);
  CF_FILEDESCRIPTORA             := RegisterClipboardFormat(CFSTR_FILEDESCRIPTORA);
  CF_FILEDESCRIPTORW             := RegisterClipboardFormat(CFSTR_FILEDESCRIPTORW);
  CF_FILEDESCRIPTOR := CF_FILEDESCRIPTORA;
  CF_FILECONTENTS                := RegisterClipboardFormat(CFSTR_FILECONTENTS);
  CF_FILENAMEA                   := RegisterClipboardFormat(CFSTR_FILENAMEA);
  CF_FILENAMEW                   := RegisterClipboardFormat(CFSTR_FILENAMEW);
  CF_FILENAME := CF_FILENAMEA;
  CF_PRINTERGROUP                := RegisterClipboardFormat(CFSTR_PRINTERGROUP);
  CF_FILENAMEMAPA                := RegisterClipboardFormat(CFSTR_FILENAMEMAPA);
  CF_FILENAMEMAPW                := RegisterClipboardFormat(CFSTR_FILENAMEMAPW);
  CF_FILENAMEMAP := CF_FILENAMEMAPA;
  CF_SHELLURL                    := RegisterClipboardFormat(CFSTR_SHELLURL);
//  CF_INETURLA                    := RegisterClipboardFormat(CFSTR_INETURLA);
//  CF_INETURLW                    := RegisterClipboardFormat(CFSTR_INETURLW);
//  CF_INETURL := CF_INETURLA;
  CF_PREFERREDDROPEFFECT         := RegisterClipboardFormat(CFSTR_PREFERREDDROPEFFECT);
  CF_PERFORMEDDROPEFFECT         := RegisterClipboardFormat(CFSTR_PERFORMEDDROPEFFECT);
  CF_PASTESUCCEEDED              := RegisterClipboardFormat(CFSTR_PASTESUCCEEDED);
  CF_INDRAGLOOP                  := RegisterClipboardFormat(CFSTR_INDRAGLOOP);
//  CF_DRAGCONTEXT                 := RegisterClipboardFormat(CFSTR_DRAGCONTEXT);
//  CF_MOUNTEDVOLUME               := RegisterClipboardFormat(CFSTR_MOUNTEDVOLUME);
//  CF_PERSISTEDDATAOBJECT         := RegisterClipboardFormat(CFSTR_PERSISTEDDATAOBJECT);
//  CF_TARGETCLSID                 := RegisterClipboardFormat(CFSTR_TARGETCLSID);
//  CF_LOGICALPERFORMEDDROPEFFECT  := RegisterClipboardFormat(CFSTR_LOGICALPERFORMEDDROPEFFECT);
//  CF_AUTOPLAY_SHELLIDLISTS       := RegisterClipboardFormat(CFSTR_AUTOPLAY_SHELLIDLISTS);
//  CF_UNTRUSTEDDRAGDROP           := RegisterClipboardFormat(CFSTR_UNTRUSTEDDRAGDROP);

END.

