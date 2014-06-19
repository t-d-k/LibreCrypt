unit OTFEPGPDiskTestApp_U;
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
  Spin64, FileCtrl, ExtCtrls, OTFEPGPDisk_U, OTFE_U, ComCtrls;

type
  TOTFEPGPDiskTestApp_F = class(TForm)
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
    pbHasDriveOpenFiles: TButton;
    rgActive: TRadioGroup;
    ckDismountDriveEmergency: TCheckBox;
    pbGetFileMountedForDrive: TButton;
    pbGetDriveMountedForFile: TButton;
    pbNumDrivesMounted: TButton;
    pbRefresh: TButton;
    pbGetDrivesMounted: TButton;
    pbIsDriverInstalled: TButton;
    OTFEPGPDisk1: TOTFEPGPDisk;
    RichEdit1: TRichEdit;
    procedure pbCloseClick(Sender: TObject);
    procedure pbVersionClick(Sender: TObject);
    procedure pbIsEncryptedVolFileClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure pbMountVolumeClick(Sender: TObject);
    procedure pbDisountVolumeClick(Sender: TObject);
    procedure pbDismountDriveClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure pbHasDriveOpenFilesClick(Sender: TObject);
    procedure rgActiveClick(Sender: TObject);
    procedure pbGetFileMountedForDriveClick(Sender: TObject);
    procedure pbGetDriveMountedForFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbNumDrivesMountedClick(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure pbGetDrivesMountedClick(Sender: TObject);
    procedure pbIsDriverInstalledClick(Sender: TObject);
  private
procedure ReportWhetherActive();
procedure RefreshDriveComboBox();
  public
    { Public declarations }
  end;

var
  OTFEPGPDiskTestApp_F: TOTFEPGPDiskTestApp_F;

implementation

{$R *.DFM}

uses ShellAPI;

procedure TOTFEPGPDiskTestApp_F.ReportWhetherActive();
begin
  if OTFEPGPDisk1.Active then
    begin
    RichEdit1.lines.add('PGPDisk component ACTIVE');
    rgActive.ItemIndex:=0;
    end
  else
    begin
    RichEdit1.lines.add('PGPDisk component NOT Active');
    rgActive.ItemIndex:=1;
    end;

end;

procedure TOTFEPGPDiskTestApp_F.pbCloseClick(Sender: TObject);
begin
  Close;
  
end;

procedure TOTFEPGPDiskTestApp_F.pbVersionClick(Sender: TObject);
begin
  RichEdit1.lines.add('Driver version: 0x'+inttohex(OTFEPGPDisk1.Version(), 1));

end;

procedure TOTFEPGPDiskTestApp_F.pbIsEncryptedVolFileClick(Sender: TObject);
var
  filename: string;
  output: string;
begin
  filename := edTestIfMtdVolFile.text;

  output := filename;
  if OTFEPGPDisk1.IsEncryptedVolFile(filename) then
    begin
    output := output + ' IS ';
    end
  else
    begin
    output := output + ' is NOT ';
    end;

  output := output + 'a PGPDisk volume file';
  
  RichEdit1.lines.add(output);


end;

procedure TOTFEPGPDiskTestApp_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edTestIfMtdVolFile.text;
  if OpenDialog1.execute() then
    begin
    edTestIfMtdVolFile.text := OpenDialog1.FileName;
    end;

end;

procedure TOTFEPGPDiskTestApp_F.pbMountVolumeClick(Sender: TObject);
begin
  if OTFEPGPDisk1.Mount(edTestIfMtdVolFile.text)<>#0 then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' mounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' mount failed');
    end;

  RefreshDriveComboBox();

end;

procedure TOTFEPGPDiskTestApp_F.pbDisountVolumeClick(Sender: TObject);
begin
  if OTFEPGPDisk1.Dismount(edTestIfMtdVolFile.text, ckDismountDriveEmergency.checked) then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismount failed');
    end;

  RefreshDriveComboBox();

end;


procedure TOTFEPGPDiskTestApp_F.pbDismountDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFEPGPDisk1.Dismount(DriveComboBox1.drive, ckDismountDriveEmergency.checked) then
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

procedure TOTFEPGPDiskTestApp_F.RefreshDriveComboBox();
var
  origCase: TTextCase;
begin
  origCase := DriveComboBox1.TextCase;

  DriveComboBox1.TextCase := tcUpperCase;
  DriveComboBox1.TextCase := tcLowerCase;

  DriveComboBox1.TextCase := origCase;
end;

procedure TOTFEPGPDiskTestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.Clear();

end;

procedure TOTFEPGPDiskTestApp_F.pbHasDriveOpenFilesClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFEPGPDisk1.HasDriveOpenFiles(DriveComboBox1.drive) then
    begin
    output := output + 'HAS open file(s)';
    end
  else
    begin
    output := output + 'does NOT have open file(s)';
    end;

  RichEdit1.lines.add(output);

end;

procedure TOTFEPGPDiskTestApp_F.rgActiveClick(Sender: TObject);
begin
  try
    OTFEPGPDisk1.Active := (rgActive.ItemIndex=0);
  finally
    ReportWhetherActive();
  end;

end;


procedure TOTFEPGPDiskTestApp_F.pbGetFileMountedForDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive +
            ': is PGPDisk volume file: "' +
            OTFEPGPDisk1.GetVolFileForDrive(DriveComboBox1.drive) +
            '"';

  RichEdit1.lines.add(output);

end;

procedure TOTFEPGPDiskTestApp_F.pbGetDriveMountedForFileClick(Sender: TObject);
var
  output: string;
begin
  output := '"' +
            edTestIfMtdVolFile.text +
            '" is mounted as: '+
            OTFEPGPDisk1.GetDriveForVolFile(edTestIfMtdVolFile.text) +
            ':';

  RichEdit1.lines.add(output);


end;

procedure TOTFEPGPDiskTestApp_F.FormCreate(Sender: TObject);
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

procedure TOTFEPGPDiskTestApp_F.pbNumDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of PGPDisk volumes mounted: ';
  output := output + inttostr(OTFEPGPDisk1.CountDrivesMounted());

  RichEdit1.lines.add(output);

end;

procedure TOTFEPGPDiskTestApp_F.pbRefreshClick(Sender: TObject);
begin
  RefreshDriveComboBox();

end;

procedure TOTFEPGPDiskTestApp_F.pbGetDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of PGPDisk volumes mounted: ';
  output := output + OTFEPGPDisk1.DrivesMounted();

  RichEdit1.lines.add(output);

end;

procedure TOTFEPGPDiskTestApp_F.pbIsDriverInstalledClick(Sender: TObject);
begin
  if OTFEPGPDisk1.IsDriverInstalled() then
    begin
    RichEdit1.lines.add('Driver installed');
    end
  else
    begin
    RichEdit1.lines.add('Driver NOT installed');
    end;

end;


// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN UNIT_PLATFORM ON}
// -----------------------------------------------------------------------------

END.

