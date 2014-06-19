unit Main;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

{$IFDEF LINUX}
This software is only intended for use under MS Windows
{$ENDIF}
{$WARN UNIT_PLATFORM OFF}  // Useless warning about platform - we're already
                           // protecting against that!

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Shredder, ComCtrls, FileCtrl;

type
  TfrmMain = class(TForm)
    ShredderObj: TShredder;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet4: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    dcbOverwriteDriveFreeSpace: TDriveComboBox;
    pbTestOverwriteDriveFreeSpace: TButton;
    ckSilentOverwriteDriveFreeSpace: TCheckBox;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    edFileSlackFilename: TEdit;
    pbBrowseFileSlackFilename: TButton;
    pbOverwriteFileSlack: TButton;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    edDestroyFileDirName: TEdit;
    pbBrowseDestroyFilename: TButton;
    pbBrowseDestroyDirname: TButton;
    pbDestroyFileDir: TButton;
    ckSilentDestroyFileDir: TCheckBox;
    ckQuickShred: TCheckBox;
    TabSheet5: TTabSheet;
    GroupBox5: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    edRegkey: TEdit;
    pbTestDestroyRegkey: TButton;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    dcbOverwriteAllFileSlack: TDriveComboBox;
    pbOverwriteAllFileSlack: TButton;
    ckSilentOverwriteAllFileSlack: TCheckBox;
    pbClose: TButton;
    procedure pbTestDestroyRegkeyClick(Sender: TObject);
    procedure pbDestroyFileDirClick(Sender: TObject);
    procedure pbOverwriteAllFileSlackClick(Sender: TObject);
    procedure pbOverwriteFileSlackClick(Sender: TObject);
    procedure pbTestOverwriteDriveFreeSpaceClick(Sender: TObject);
    procedure pbBrowseFileSlackFilenameClick(Sender: TObject);
    procedure pbBrowseDestroyFilenameClick(Sender: TObject);
    procedure pbBrowseDestroyDirnameClick(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  SDUGeneral;


procedure TfrmMain.pbTestDestroyRegkeyClick(Sender: TObject);
begin
  ShredderObj.DestroyRegKey(edRegkey.text);
  MessageDlg('The operation completed.', mtInformation, [mbOK], 0);

end;

procedure TfrmMain.pbDestroyFileDirClick(Sender: TObject);
begin
  ShredderObj.DestroyFileOrDir(edDestroyFileDirName.text, ckQuickShred.checked, ckSilentDestroyFileDir.checked);
  MessageDlg('The operation completed.', mtInformation, [mbOK], 0);

end;

procedure TfrmMain.pbOverwriteAllFileSlackClick(Sender: TObject);
var
  status: integer;
begin
  status := ShredderObj.OverwriteAllFileSlacks(dcbOverwriteAllFileSlack.drive, ckSilentOverwriteAllFileSlack.checked);
  if (status = -1) then
    begin
    MessageDlg('An error occured while carrying out the requested operation.', mtError, [mbOK], 0);
    end
  else if (status = -2) then
    begin
    MessageDlg('User cancelled the requested operation.', mtInformation, [mbOK], 0);
    end
  else if (status = 1) then
    begin
    MessageDlg('The operation completed successfully.', mtInformation, [mbOK], 0);
    end;

end;

procedure TfrmMain.pbOverwriteFileSlackClick(Sender: TObject);
begin
  if ShredderObj.OverwriteFileSlack(edFileSlackFilename.Text) then
    begin
    MessageDlg('The operation completed successfully.', mtInformation, [mbOK], 0);
    end
  else
    begin
    MessageDlg('An error occured while carrying out the requested operation.', mtError, [mbOK], 0);
    end;

end;

procedure TfrmMain.pbTestOverwriteDriveFreeSpaceClick(Sender: TObject);
var
  status: integer;
begin
  status := ShredderObj.OverwriteDriveFreeSpace(dcbOverwriteDriveFreeSpace.drive, ckSilentOverwriteDriveFreeSpace.checked);
  if (status = -1) then
    begin
    MessageDlg('An error occured while carrying out the requested operation.', mtError, [mbOK], 0);
    end
  else if (status = -2) then
    begin
    MessageDlg('User cancelled the requested operation.', mtInformation, [mbOK], 0);
    end
  else if (status = 1) then
    begin
    MessageDlg('The operation completed successfully.', mtInformation, [mbOK], 0);
    end;

end;

procedure TfrmMain.pbBrowseFileSlackFilenameClick(Sender: TObject);
begin
  SDUOpenSaveDialogSetup(OpenDialog1, edFileSlackFilename.Text);
  if OpenDialog1.Execute() then
    begin
    edFileSlackFilename.Text := OpenDialog1.Filename;
    end;

end;

procedure TfrmMain.pbBrowseDestroyFilenameClick(Sender: TObject);
begin
  SDUOpenSaveDialogSetup(OpenDialog1, edDestroyFileDirName.Text);
  if OpenDialog1.Execute() then
    begin
    edDestroyFileDirName.Text := OpenDialog1.Filename;
    end;

end;

procedure TfrmMain.pbBrowseDestroyDirnameClick(Sender: TObject);
var
  dirToShred: string;
begin
  if SelectDirectory('Select directory to overwrite', '', dirToShred) then
    begin
    edDestroyFileDirName.Text := dirToShred;
    end;

end;

procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  Close();
  
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  self.Caption := Application.Title;
  PageControl1.ActivePageIndex := 0;

end;


// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN UNIT_PLATFORM ON}
// -----------------------------------------------------------------------------

END.


