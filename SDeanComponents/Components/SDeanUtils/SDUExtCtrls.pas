unit SDUExtCtrls;

// TSDFileDropPanel: A panel onto which files can be dropped.
// Example usage: Place a TSDFileDropPanel onto a form, place a list control
//                on top of the panel; the list control may not have files (and
//                directories) dragged and dropped onto it.

interface

uses
  ExtCtrls, Classes, Messages;

type
  TNotifyFileDirectoryDropEvent = procedure (Sender: TObject; filename: string) of object;

  TSDFileDropPanel = class(TPanel)
  private
    FOnFileDrop: TNotifyFileDirectoryDropEvent;
    FOnDirectoryDrop: TNotifyFileDirectoryDropEvent;
    procedure DoOnFileDrop(filename: string);
    procedure DoOnDirectoryDrop(dirname: string);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AcceptFiles(var msg: TMessage); message WM_DROPFILES;
  published
    property OnFileDrop: TNotifyFileDirectoryDropEvent read FOnFileDrop write FOnFileDrop;
    property OnDirectoryDrop: TNotifyFileDirectoryDropEvent read FOnDirectoryDrop write FOnDirectoryDrop;
  end;

procedure Register;

implementation

uses
  ShellAPI, SysUtils;

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDFileDropPanel]);
end;

constructor TSDFileDropPanel.Create(AOwner: TComponent);
begin
  inherited;
  if not(csDesigning in ComponentState) then
    begin
    DragAcceptFiles(self.Handle, TRUE);
    end;
end;

procedure TSDFileDropPanel.AcceptFiles(var msg: TMessage);
const
  MAX_PATH_LENGTH = 2048;
var
  i,
  cntDrop: integer;
  tmpFilename: array [0..(MAX_PATH_LENGTH-1)] of char;
begin
  cntDrop := DragQueryFile(
                              msg.WParam,
                              $FFFFFFFF,
                              tmpFilename,
                              length(tmpFilename)
                             );

  // query Windows one at a time for the file name
  for i := 0 to (cntDrop-1) do
    begin
    DragQueryFile(
                  msg.WParam,
                  i,
                  tmpFilename,
                  length(tmpFilename)
                 );

    if DirectoryExists(tmpFilename) then
      begin
      DoOnDirectoryDrop(tmpFilename);
      end
    else
      begin
      DoOnFileDrop(tmpFilename);
      end;
    end;

  DragFinish(msg.WParam);
end;

procedure TSDFileDropPanel.DoOnFileDrop(filename: string);
begin
  if assigned(FOnFileDrop) then
  begin
    FOnFileDrop(self, filename);
  end;
  
end;

procedure TSDFileDropPanel.DoOnDirectoryDrop(dirname: string);
begin
  if assigned(FOnDirectoryDrop) then
  begin
    FOnDirectoryDrop(self, dirname);
  end;

end;

END.

