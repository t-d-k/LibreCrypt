unit OTFECrossCryptTestApp_U;
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
  ComCtrls,
  OTFE_U,
  OTFECrossCrypt_U;

type
  TOTFECrossCryptTestApp_F = class(TForm)
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
    RichEdit1: TRichEdit;
    pbDriveInfo: TButton;
    gbVolInfo: TGroupBox;
    lblReadOnly: TLabel;
    lblDriveMountedAs: TLabel;
    Label15: TLabel;
    Label22: TLabel;
    Label2: TLabel;
    lblVolumeFile: TLabel;
    pbVersionStr: TButton;
    lblUserDevice: TLabel;
    Label7: TLabel;
    OTFECrossCrypt1: TOTFECrossCrypt;
    Button1: TButton;
    seCreateVolSize: TSpinEdit64;
    Label1: TLabel;
    lblKernelDevice: TLabel;
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
    procedure pbDriveInfoClick(Sender: TObject);
    procedure pbVersionStrClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
procedure ReportWhetherActive();
procedure RefreshDriveComboBox();
  public
    { Public declarations }
  end;

var
  OTFECrossCryptTestApp_F: TOTFECrossCryptTestApp_F;

implementation

{$R *.DFM}

uses ShellAPI;

procedure TOTFECrossCryptTestApp_F.ReportWhetherActive();
begin
  if OTFECrossCrypt1.Active then
    begin
    RichEdit1.lines.add('CrossCrypt component ACTIVE');
    rgActive.ItemIndex:=0;
    end
  else
    begin
    RichEdit1.lines.add('CrossCrypt component NOT Active');
    rgActive.ItemIndex:=1;
    end;

end;

procedure TOTFECrossCryptTestApp_F.pbCloseClick(Sender: TObject);
begin
  Close;
  
end;

procedure TOTFECrossCryptTestApp_F.pbVersionClick(Sender: TObject);
begin
  RichEdit1.lines.add('Driver version: 0x'+inttohex(OTFECrossCrypt1.Version(), 1));

end;

procedure TOTFECrossCryptTestApp_F.pbIsEncryptedVolFileClick(Sender: TObject);
var
  filename: string;
  output: string;
begin
  filename := edTestIfMtdVolFile.text;

  output := filename;
  if OTFECrossCrypt1.IsEncryptedVolFile(filename) then
    begin
    output := output + ' IS ';
    end
  else
    begin
    output := output + ' is NOT ';
    end;

  output := output + 'a CrossCrypt volume file';
  
  RichEdit1.lines.add(output);


end;

procedure TOTFECrossCryptTestApp_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edTestIfMtdVolFile.text;
  if OpenDialog1.execute() then
    begin
    edTestIfMtdVolFile.text := OpenDialog1.FileName;
    end;

end;

procedure TOTFECrossCryptTestApp_F.pbMountVolumeClick(Sender: TObject);
begin
  if OTFECrossCrypt1.Mount(edTestIfMtdVolFile.text)<>#0 then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' mounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' mount failed');
    end;

  RefreshDriveComboBox();

end;

procedure TOTFECrossCryptTestApp_F.pbDisountVolumeClick(Sender: TObject);
begin
  if OTFECrossCrypt1.Dismount(edTestIfMtdVolFile.text, ckDismountDriveEmergency.checked) then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismount failed');
    end;

  RefreshDriveComboBox();

end;


procedure TOTFECrossCryptTestApp_F.pbDismountDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFECrossCrypt1.Dismount(DriveComboBox1.drive, ckDismountDriveEmergency.checked) then
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

procedure TOTFECrossCryptTestApp_F.RefreshDriveComboBox();
var
  origCase: TTextCase;
begin
  origCase := DriveComboBox1.TextCase;

  DriveComboBox1.TextCase := tcUpperCase;
  DriveComboBox1.TextCase := tcLowerCase;

  DriveComboBox1.TextCase := origCase;
end;

procedure TOTFECrossCryptTestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.Clear();

end;

procedure TOTFECrossCryptTestApp_F.rgActiveClick(Sender: TObject);
begin
  try
    OTFECrossCrypt1.Active := (rgActive.ItemIndex=0);
  finally
    ReportWhetherActive();
  end;

end;


procedure TOTFECrossCryptTestApp_F.pbGetFileMountedForDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive +
            ': is CrossCrypt volume file: "' +
            OTFECrossCrypt1.GetVolFileForDrive(DriveComboBox1.drive) +
            '"';

  RichEdit1.lines.add(output);

end;

procedure TOTFECrossCryptTestApp_F.pbGetDriveMountedForFileClick(Sender: TObject);
var
  output: string;
begin
  output := '"' +
            edTestIfMtdVolFile.text +
            '" is mounted as: '+
            OTFECrossCrypt1.GetDriveForVolFile(edTestIfMtdVolFile.text) +
            ':';

  RichEdit1.lines.add(output);


end;

procedure TOTFECrossCryptTestApp_F.FormCreate(Sender: TObject);
begin
  edTestIfMtdVolFile.text := 'C:\crosscrypt.dat';

end;

procedure TOTFECrossCryptTestApp_F.pbNumDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of CrossCrypt volumes mounted: ';
  output := output + inttostr(OTFECrossCrypt1.CountDrivesMounted());

  RichEdit1.lines.add(output);

end;

procedure TOTFECrossCryptTestApp_F.pbRefreshClick(Sender: TObject);
begin
  RefreshDriveComboBox();

end;

procedure TOTFECrossCryptTestApp_F.pbGetDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'CrossCrypt volumes mounted: ';
  output := output + OTFECrossCrypt1.DrivesMounted();

  RichEdit1.lines.add(output);

end;

procedure TOTFECrossCryptTestApp_F.pbIsDriverInstalledClick(Sender: TObject);
begin
  if OTFECrossCrypt1.IsDriverInstalled() then
    begin
    RichEdit1.lines.add('Driver installed');
    end
  else
    begin
    RichEdit1.lines.add('Driver NOT installed');
    end;

end;

procedure TOTFECrossCryptTestApp_F.pbDismountAllClick(
  Sender: TObject);
var
  dismountFailedFor: string;
begin
  dismountFailedFor := OTFECrossCrypt1.DismountAll(ckDismountDriveEmergency.checked);

  if dismountFailedFor='' then
    begin
    RichEdit1.lines.add('DismountAll OK');
    end
  else
    begin
    RichEdit1.lines.add('DismountAll failed for drives: '+dismountFailedFor);
    end;

end;


procedure TOTFECrossCryptTestApp_F.pbDriveInfoClick(Sender: TObject);
var
  output: string;
  volumeInfo: TOTFECrossCryptVolumeInfo;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFECrossCrypt1.GetVolumeInfo(DriveComboBox1.drive, volumeInfo) then
    begin
    output := output + 'Got drive info OK';

    lblDriveMountedAs.caption := volumeInfo.DriveLetter+':';
    lblVolumeFile.caption := volumeInfo.Filename;
    lblKernelDevice.caption := volumeInfo.KernelModeDeviceName;
    lblUserDevice.caption := volumeInfo.UserModeDeviceName;
    if volumeInfo.ReadOnly then
      begin
      lblReadOnly.caption := 'readonly';
      end
    else
      begin
      lblReadOnly.caption := 'read/write';
      end;

    end
  else
    begin
    output := output + 'DriveInfo FAILED';
    end;

  RichEdit1.lines.add(output);

  RefreshDriveComboBox();

end;



procedure TOTFECrossCryptTestApp_F.pbVersionStrClick(Sender: TObject);
begin
  RichEdit1.lines.add('Driver version string: '+OTFECrossCrypt1.VersionStr());

end;


procedure TOTFECrossCryptTestApp_F.Button1Click(Sender: TObject);
begin
  if OTFECrossCrypt1.CreateVolume(edTestIfMtdVolFile.text, seCreateVolSize.Value)<>#0 then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' CreateVolume OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' CreateVolume failed');
    end;

  RefreshDriveComboBox();

end;

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN UNIT_PLATFORM ON}
// -----------------------------------------------------------------------------

END.

