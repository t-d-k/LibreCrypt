unit formMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfrmMain = class(TForm)
    pbCopyFile: TButton;
    reReport: TRichEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pbClose: TButton;
    pbClear: TButton;
    GroupBox1: TGroupBox;
    pbWow64EnableWow64FsRedirection: TButton;
    pbWow64RevertWow64FsRedirection: TButton;
    pbWow64DisableWow64FsRedirection: TButton;
    procedure pbWow64EnableWow64FsRedirectionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbWow64RevertWow64FsRedirectionClick(Sender: TObject);
    procedure pbWow64DisableWow64FsRedirectionClick(Sender: TObject);
    procedure pbCopyFileClick(Sender: TObject);
  private
    FOldValue: Pointer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  SDUGeneral,
  SDUWindows64;

const
  // This CSIDL value isn't included in ShlObj in Delphi 5
  CSIDL_SYSTEM = $0025;

procedure TfrmMain.pbCopyFileClick(Sender: TObject);
var
  srcFilename: string;
  destPath: string;
  destFilename: string;
  copyOK: boolean;
begin
  destPath := SDUGetSpecialFolderPath(CSIDL_SYSTEM);
  srcFilename := ParamStr(0);  // This executable
  destFilename := destPath+'\'+ExtractFilename(srcFilename);

  copyOK := CopyFile(PChar(srcFilename), PChar(destFilename), TRUE);

  reReport.lines.add('File copy operation result: '+SDUBoolToStr(copyOK));
  reReport.lines.add('Please manually check:');
  reReport.lines.add('  '+destPath);
  reReport.lines.add('for a copy of this executable');

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  self.caption:= Application.Title;
  reReport.lines.clear();
end;

procedure TfrmMain.pbWow64EnableWow64FsRedirectionClick(Sender: TObject);
var
  fnOut: boolean;
begin
  fnOut := SDUWow64EnableWow64FsRedirection(FOldValue);
  reReport.lines.add('SDUWow64EnableWow64FsRedirection: '+SDUBoolToStr(fnOut));
end;

procedure TfrmMain.pbWow64RevertWow64FsRedirectionClick(Sender: TObject);
var
  fnOut: boolean;
begin
  fnOut := SDUWow64RevertWow64FsRedirection(FOldValue);
  reReport.lines.add('SDUWow64RevertWow64FsRedirection: '+SDUBoolToStr(fnOut));
end;

procedure TfrmMain.pbWow64DisableWow64FsRedirectionClick(Sender: TObject);
var
  fnOut: boolean;
begin
  fnOut := SDUWow64DisableWow64FsRedirection(FOldValue);
  reReport.lines.add('SDUWow64DisableWow64FsRedirection: '+SDUBoolToStr(fnOut));
end;

END.

