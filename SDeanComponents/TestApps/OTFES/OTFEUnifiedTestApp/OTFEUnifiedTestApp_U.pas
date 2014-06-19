unit OTFEUnifiedTestApp_U;
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
  StdCtrls,
  Spin64, FileCtrl, ExtCtrls,
  OTFE_U, OTFEUnified_U, ComCtrls;

type
  TOTFEUnifiedTestApp_F = class(TForm)
    pbClose: TButton;
    pbVersion: TButton;
    pbIsEncryptedVolFile: TButton;
    edTestIfMtdVolFile: TEdit;
    DriveComboBox1: TDriveComboBox;
    OpenDialog1: TOpenDialog;
    pbBrowse: TButton;
    pbDisountVolume: TButton;
    pbMountVolume: TButton;
    pbDismountDrive: TButton;
    pbClear: TButton;
    rgActive: TRadioGroup;
    ckDismountDriveEmergency: TCheckBox;
    pbGetFileMountedForDrive: TButton;
    pbGetDriveMountedForFile: TButton;
    pbNumDrivesMounted: TButton;
    pbRefresh: TButton;
    pbGetDrivesMounted: TButton;
    pbIsDriverInstalled: TButton;
    pbDismountAll: TButton;
    OTFEUnified1: TOTFEUnified;
    RichEdit1: TRichEdit;
    procedure pbCloseClick(Sender: TObject);
    procedure pbVersionClick(Sender: TObject);
    procedure pbIsEncryptedVolFileClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure pbMountVolumeClick(Sender: TObject);
    procedure pbDisountVolumeClick(Sender: TObject);
    procedure pbDismountDriveClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure rgActiveClick(Sender: TObject);
    procedure pbGetFileMountedForDriveClick(Sender: TObject);
    procedure pbGetDriveMountedForFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbNumDrivesMountedClick(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure pbGetDrivesMountedClick(Sender: TObject);
    procedure pbIsDriverInstalledClick(Sender: TObject);
    procedure pbDismountAllClick(Sender: TObject);
  private
procedure ReportWhetherActive();
procedure RefreshDriveComboBox();
  public
    { Public declarations }
  end;

var
  OTFEUnifiedTestApp_F: TOTFEUnifiedTestApp_F;

implementation

{$R *.DFM}

uses ShellAPI;

procedure TOTFEUnifiedTestApp_F.ReportWhetherActive();
begin
  if OTFEUnified1.Active then
    begin
    RichEdit1.lines.add('UnifiedDiskEncryption component ACTIVE');
    rgActive.ItemIndex:=0;
    end
  else
    begin
    RichEdit1.lines.add('UnifiedDiskEncryption component NOT Active');
    rgActive.ItemIndex:=1;
    end;

end;

procedure TOTFEUnifiedTestApp_F.pbCloseClick(Sender: TObject);
begin
  Close;
  
end;

procedure TOTFEUnifiedTestApp_F.pbVersionClick(Sender: TObject);
begin
  RichEdit1.lines.add('Driver version: 0x'+inttohex(OTFEUnified1.Version(), 1));

end;

procedure TOTFEUnifiedTestApp_F.pbIsEncryptedVolFileClick(Sender: TObject);
var
  filename: string;
  output: string;
begin
  filename := edTestIfMtdVolFile.text;

  output := filename;
  if OTFEUnified1.IsEncryptedVolFile(filename) then
    begin
    output := output + ' IS ';
    end
  else
    begin
    output := output + ' is NOT ';
    end;

  output := output + 'a UnifiedDiskEncryption volume file';
  
  RichEdit1.lines.add(output);


end;

procedure TOTFEUnifiedTestApp_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edTestIfMtdVolFile.text;
  if OpenDialog1.execute() then
    begin
    edTestIfMtdVolFile.text := OpenDialog1.FileName;
    end;

end;

procedure TOTFEUnifiedTestApp_F.pbMountVolumeClick(Sender: TObject);
begin
  if OTFEUnified1.Mount(edTestIfMtdVolFile.text)<>#0 then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' mounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' mount failed');
    end;

  RefreshDriveComboBox();

end;

procedure TOTFEUnifiedTestApp_F.pbDisountVolumeClick(Sender: TObject);
begin
  if OTFEUnified1.Dismount(edTestIfMtdVolFile.text, ckDismountDriveEmergency.checked) then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismount failed');
    end;

  RefreshDriveComboBox();

end;


procedure TOTFEUnifiedTestApp_F.pbDismountDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFEUnified1.Dismount(DriveComboBox1.drive, ckDismountDriveEmergency.checked) then
    begin
    output := output + 'dismounted OK';
    end
  else
    begin
    output := output + 'dismount FAILED';
    end;

  RichEdit1.lines.add(output);

  RefreshDriveComboBox();

end;

procedure TOTFEUnifiedTestApp_F.RefreshDriveComboBox();
var
  origCase: TTextCase;
begin
  origCase := DriveComboBox1.TextCase;

  DriveComboBox1.TextCase := tcUpperCase;
  DriveComboBox1.TextCase := tcLowerCase;

  DriveComboBox1.TextCase := origCase;
end;

procedure TOTFEUnifiedTestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.Clear();

end;

procedure TOTFEUnifiedTestApp_F.rgActiveClick(Sender: TObject);
begin
  try
    OTFEUnified1.Active := (rgActive.ItemIndex=0);
  finally
    ReportWhetherActive();
  end;

end;


procedure TOTFEUnifiedTestApp_F.pbGetFileMountedForDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive +
            ': is UnifiedDiskEncryption volume file: "' +
            OTFEUnified1.GetVolFileForDrive(DriveComboBox1.drive) +
            '"';

  RichEdit1.lines.add(output);

end;

procedure TOTFEUnifiedTestApp_F.pbGetDriveMountedForFileClick(Sender: TObject);
var
  output: string;
begin
  output := '"' +
            edTestIfMtdVolFile.text +
            '" is mounted as: '+
            OTFEUnified1.GetDriveForVolFile(edTestIfMtdVolFile.text) +
            ':';

  RichEdit1.lines.add(output);


end;

procedure TOTFEUnifiedTestApp_F.FormCreate(Sender: TObject);
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
    edTestIfMtdVolFile.text := 'c:\junk.pgd';
    end
  else
    begin
    edTestIfMtdVolFile.text := 'e:\VolumeFiles\ff.pgd';
    end;

end;

procedure TOTFEUnifiedTestApp_F.pbNumDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of UnifiedDiskEncryption volumes mounted: ';
  output := output + inttostr(OTFEUnified1.CountDrivesMounted());

  RichEdit1.lines.add(output);

end;

procedure TOTFEUnifiedTestApp_F.pbRefreshClick(Sender: TObject);
begin
  RefreshDriveComboBox();

end;

procedure TOTFEUnifiedTestApp_F.pbGetDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of UnifiedDiskEncryption volumes mounted: ';
  output := output + OTFEUnified1.DrivesMounted();

  RichEdit1.lines.add(output);

end;

procedure TOTFEUnifiedTestApp_F.pbIsDriverInstalledClick(Sender: TObject);
begin
  if OTFEUnified1.IsDriverInstalled() then
    begin
    RichEdit1.lines.add('Driver installed');
    end
  else
    begin
    RichEdit1.lines.add('Driver NOT installed');
    end;

end;

procedure TOTFEUnifiedTestApp_F.pbDismountAllClick(
  Sender: TObject);
var
  dismountFailedFor: string;
begin
  dismountFailedFor := OTFEUnified1.DismountAll(ckDismountDriveEmergency.checked);

  if dismountFailedFor='' then
    begin
    RichEdit1.lines.add('DismountAll OK');
    end
  else
    begin
    RichEdit1.lines.add('DismountAll failed for drives: '+dismountFailedFor);
    end;

end;


// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN UNIT_PLATFORM ON}
// -----------------------------------------------------------------------------

END.

