unit OTFETrueCryptTestApp_U;
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
  ComCtrls, OTFE_U, OTFETrueCrypt_U, OTFETrueCryptStructures_U;

type
  TOTFETrueCryptTestApp_F = class(TForm)
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
    lblCipher: TLabel;
    Label8: TLabel;
    Label1: TLabel;
    lblDiskLength: TLabel;
    Label6: TLabel;
    lblVolumeCreated: TLabel;
    Label9: TLabel;
    lblVolumeLocation: TLabel;
    Label2: TLabel;
    lblVolumeFile: TLabel;
    pbVersionStr: TButton;
    OTFETrueCrypt1: TOTFETrueCrypt;
    PKCS5: TLabel;
    lblpkcs5: TLabel;
    lblpkcs5Iterations: TLabel;
    Label4: TLabel;
    lblPasswordChanged: TLabel;
    Label5: TLabel;
    Label3: TLabel;
    lblCipherMode: TLabel;
    Label10: TLabel;
    lblVolumeType: TLabel;
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
  private
procedure ReportWhetherActive();
procedure RefreshDriveComboBox();
  public
    { Public declarations }
  end;

var
  OTFETrueCryptTestApp_F: TOTFETrueCryptTestApp_F;

implementation

{$R *.DFM}

uses ShellAPI;

procedure TOTFETrueCryptTestApp_F.ReportWhetherActive();
begin
  if OTFETrueCrypt1.Active then
    begin
    RichEdit1.lines.add('TrueCrypt component ACTIVE');
    rgActive.ItemIndex:=0;
    end
  else
    begin
    RichEdit1.lines.add('TrueCrypt component NOT Active');
    rgActive.ItemIndex:=1;
    end;

end;

procedure TOTFETrueCryptTestApp_F.pbCloseClick(Sender: TObject);
begin
  Close;
  
end;

procedure TOTFETrueCryptTestApp_F.pbVersionClick(Sender: TObject);
begin
  RichEdit1.lines.add('Driver version: 0x'+inttohex(OTFETrueCrypt1.Version(), 1));

end;

procedure TOTFETrueCryptTestApp_F.pbIsEncryptedVolFileClick(Sender: TObject);
var
  filename: string;
  output: string;
begin
  filename := edTestIfMtdVolFile.text;

  output := filename;
  if OTFETrueCrypt1.IsEncryptedVolFile(filename) then
    begin
    output := output + ' IS ';
    end
  else
    begin
    output := output + ' is NOT ';
    end;

  output := output + 'a TrueCrypt volume file';
  
  RichEdit1.lines.add(output);


end;

procedure TOTFETrueCryptTestApp_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edTestIfMtdVolFile.text;
  if OpenDialog1.execute() then
    begin
    edTestIfMtdVolFile.text := OpenDialog1.FileName;
    end;

end;

procedure TOTFETrueCryptTestApp_F.pbMountVolumeClick(Sender: TObject);
begin
  if OTFETrueCrypt1.Mount(edTestIfMtdVolFile.text)<>#0 then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' mounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' mount failed');
    end;

  RefreshDriveComboBox();

end;

procedure TOTFETrueCryptTestApp_F.pbDisountVolumeClick(Sender: TObject);
begin
  if OTFETrueCrypt1.Dismount(edTestIfMtdVolFile.text, ckDismountDriveEmergency.checked) then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismount failed');
    end;

  RefreshDriveComboBox();

end;


procedure TOTFETrueCryptTestApp_F.pbDismountDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFETrueCrypt1.Dismount(DriveComboBox1.drive, ckDismountDriveEmergency.checked) then
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

procedure TOTFETrueCryptTestApp_F.RefreshDriveComboBox();
var
  origCase: TTextCase;
begin
  origCase := DriveComboBox1.TextCase;

  DriveComboBox1.TextCase := tcUpperCase;
  DriveComboBox1.TextCase := tcLowerCase;

  DriveComboBox1.TextCase := origCase;
end;

procedure TOTFETrueCryptTestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.Clear();

end;

procedure TOTFETrueCryptTestApp_F.rgActiveClick(Sender: TObject);
begin
  try
    OTFETrueCrypt1.Active := (rgActive.ItemIndex=0);
  finally
    ReportWhetherActive();
  end;

end;


procedure TOTFETrueCryptTestApp_F.pbGetFileMountedForDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive +
            ': is TrueCrypt volume file: "' +
            OTFETrueCrypt1.GetVolFileForDrive(DriveComboBox1.drive) +
            '"';

  RichEdit1.lines.add(output);

end;

procedure TOTFETrueCryptTestApp_F.pbGetDriveMountedForFileClick(Sender: TObject);
var
  output: string;
  driveLetter: char;
begin
  driveLetter:= OTFETrueCrypt1.GetDriveForVolFile(edTestIfMtdVolFile.text);
  if (driveLetter = #0) then
    begin
    output := 'FAILED: Unable to determine drive letter (is this volume file even mounted?)';
    end
  else
    begin
    output := '"' +
              edTestIfMtdVolFile.text +
              '" is mounted as: '+
              driveLetter +
              ':';
    end;

  RichEdit1.lines.add(output);


end;

procedure TOTFETrueCryptTestApp_F.FormCreate(Sender: TObject);
begin
  edTestIfMtdVolFile.text := 'C:\OTFE_VOL_FILE.dat';

end;

procedure TOTFETrueCryptTestApp_F.pbNumDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of TrueCrypt volumes mounted: ';
  output := output + inttostr(OTFETrueCrypt1.CountDrivesMounted());

  RichEdit1.lines.add(output);

end;

procedure TOTFETrueCryptTestApp_F.pbRefreshClick(Sender: TObject);
begin
  RefreshDriveComboBox();

end;

procedure TOTFETrueCryptTestApp_F.pbGetDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'TrueCrypt volumes mounted: ';
  output := output + OTFETrueCrypt1.DrivesMounted();

  RichEdit1.lines.add(output);

end;

procedure TOTFETrueCryptTestApp_F.pbIsDriverInstalledClick(Sender: TObject);
begin
  if OTFETrueCrypt1.IsDriverInstalled() then
    begin
    RichEdit1.lines.add('Driver installed');
    end
  else
    begin
    RichEdit1.lines.add('Driver NOT installed');
    end;

end;

procedure TOTFETrueCryptTestApp_F.pbDismountAllClick(
  Sender: TObject);
var
  dismountFailedFor: string;
begin
  dismountFailedFor := OTFETrueCrypt1.DismountAll(ckDismountDriveEmergency.checked);

  if dismountFailedFor='' then
    begin
    RichEdit1.lines.add('DismountAll OK');
    end
  else
    begin
    RichEdit1.lines.add('DismountAll failed for drives: '+dismountFailedFor);
    end;

end;


procedure TOTFETrueCryptTestApp_F.pbDriveInfoClick(Sender: TObject);
var
  output: string;
  volumeInfo: TOTFETrueCryptVolumeInfo;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFETrueCrypt1.GetDriveInfo(DriveComboBox1.drive, @volumeInfo) then
    begin
    output := output + 'Got drive info OK';

    lblDriveMountedAs.caption := volumeInfo.mountedAs+':';

    lblVolumeFile.caption:= volumeInfo.volumeFilename;

    lblVolumeLocation.caption := volumeInfo.volumeLocationName;


    lblDiskLength.caption := inttostr(volumeInfo.diskLength);

    lblCipher.caption := volumeInfo.cipherNames;

    lblpkcs5.caption := volumeInfo.pkcs5TypeName;
    lblpkcs5Iterations.caption := inttostr(volumeInfo.pkcs5Iterations);

    lblVolumeCreated.caption := datetimetostr(volumeInfo.volumeCreated);
    lblPasswordChanged.caption := datetimetostr(volumeInfo.passwordChanged);

    if volumeInfo.ReadOnly then
      begin
      lblReadOnly.caption := 'readonly';
      end
    else
      begin
      lblReadOnly.caption := 'read/write';
      end;

    lblCipherMode.caption := volumeInfo.cipherModeName;

    if volumeInfo.Hidden then
      begin
      lblVolumeType.caption := 'Hidden';
      end
    else
      begin
      lblVolumeType.caption := 'Normal';
      end;

    end
  else
    begin
    output := output + 'DriveInfo FAILED';
    end;

  RichEdit1.lines.add(output);

  RefreshDriveComboBox();

end;



procedure TOTFETrueCryptTestApp_F.pbVersionStrClick(Sender: TObject);
begin
  RichEdit1.lines.add('Driver version string: '+OTFETrueCrypt1.VersionStr());

end;


// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN UNIT_PLATFORM ON}
// -----------------------------------------------------------------------------

END.

