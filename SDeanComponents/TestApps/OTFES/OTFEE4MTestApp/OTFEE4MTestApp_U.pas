unit OTFEE4MTestApp_U;
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
  StdCtrls, ExtCtrls, FileCtrl, ComCtrls, OTFE_U, OTFEE4M_U;

type
  TOTFEE4MTestApp_F = class(TForm)
    pbClose: TButton;
    pbVersion: TButton;
    edTestIfMtdVolFile: TEdit;
    DriveComboBox1: TDriveComboBox;
    pbBrowse: TButton;
    pbDisountVolume: TButton;
    pbMountVolume: TButton;
    pbDismountDrive: TButton;
    pbClear: TButton;
    pbGetDriveInfo: TButton;
    rgActive: TRadioGroup;
    pbGetFileMountedForDrive: TButton;
    pbGetDriveMountedForFile: TButton;
    pbNumDrivesMounted: TButton;
    pbRefresh: TButton;
    OpenDialog1: TOpenDialog;
    pbGetMountedDrives: TButton;
    pbDismountAll: TButton;
    gbVolInfo: TGroupBox;
    lblReadOnly: TLabel;
    lblDriveMountedAs: TLabel;
    lblFilename: TLabel;
    Label15: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    lblAlgorithm: TLabel;
    lblKeyGen: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    pbIsEncryptedVolFile: TButton;
    Label1: TLabel;
    lblVolumeType: TLabel;
    ckScramDiskSupport: TCheckBox;
    pbIsDriverInstalled: TButton;
    pbGetAvailableRawDevices: TButton;
    OTFEE4M1: TOTFEE4M;
    RichEdit1: TRichEdit;
    procedure pbCloseClick(Sender: TObject);
    procedure rgActiveClick(Sender: TObject);
    procedure pbVersionClick(Sender: TObject);
    procedure pbNumDrivesMountedClick(Sender: TObject);
    procedure pbDismountDriveClick(Sender: TObject);
    procedure pbGetFileMountedForDriveClick(Sender: TObject);
    procedure pbIsPGPDiskVolumeClick(Sender: TObject);
    procedure pbMountVolumeClick(Sender: TObject);
    procedure pbDisountVolumeClick(Sender: TObject);
    procedure pbGetDriveMountedForFileClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure pbGetMountedDrivesClick(Sender: TObject);
    procedure pbDismountAllClick(Sender: TObject);
    procedure pbGetDriveInfoClick(Sender: TObject);
    procedure ckScramDiskSupportClick(Sender: TObject);
    procedure pbIsDriverInstalledClick(Sender: TObject);
    procedure pbGetAvailableRawDevicesClick(Sender: TObject);
  private
    procedure ReportWhetherActive();
    procedure RefreshDriveComboBox();
  public
    { Public declarations }
  end;

var
  OTFEE4MTestApp_F: TOTFEE4MTestApp_F;

implementation

{$R *.DFM}

uses SDUGeneral,
     OTFEE4MStructures_U;

procedure TOTFEE4MTestApp_F.ReportWhetherActive();
begin
  if OTFEE4M1.Active then
    begin
    RichEdit1.lines.add('E4M component ACTIVE');
    rgActive.ItemIndex:=0;
    end
  else
    begin
    RichEdit1.lines.add('E4M component NOT Active');
    rgActive.ItemIndex:=1;
    end;

end;

procedure TOTFEE4MTestApp_F.pbCloseClick(Sender: TObject);
begin
  Close;
  
end;

procedure TOTFEE4MTestApp_F.pbVersionClick(Sender: TObject);
begin
  RichEdit1.lines.add('E4M driver version: 0x'+inttohex(OTFEE4M1.Version(), 1));

end;

procedure TOTFEE4MTestApp_F.pbIsPGPDiskVolumeClick(Sender: TObject);
var
  filename: string;
  output: string;
begin
  filename := edTestIfMtdVolFile.text;

  output := filename;

  if OTFEE4M1.IsEncryptedVolFile(filename) then
    begin
    output := output + ' IS ';
    end
  else
    begin
    output := output + ' is NOT ';
    end;

  output := output + 'a E4M volume file';

  RichEdit1.lines.add(output);

end;

procedure TOTFEE4MTestApp_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edTestIfMtdVolFile.text;
  if OpenDialog1.execute() then
    begin
    edTestIfMtdVolFile.text := OpenDialog1.FileName;
    end;

end;

procedure TOTFEE4MTestApp_F.pbMountVolumeClick(Sender: TObject);
begin
  try
    if OTFEE4M1.Mount(edTestIfMtdVolFile.text)<>#0 then
      begin
      RichEdit1.lines.add(edTestIfMtdVolFile.text+' mounted OK');
      end
    else
      begin
      RichEdit1.lines.add(edTestIfMtdVolFile.text+' mount failed');
      end;
  except
    on E:EE4MVxdBadStatus do
      begin
      showmessage('CAUGHT AN EE4MVxdBadStatus');
      end;
  end;

  RefreshDriveComboBox();

end;

procedure TOTFEE4MTestApp_F.pbDisountVolumeClick(Sender: TObject);
begin
  if OTFEE4M1.Dismount(edTestIfMtdVolFile.text) then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismount failed');
    end;

  RefreshDriveComboBox();

end;


procedure TOTFEE4MTestApp_F.pbDismountDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';

  if OTFEE4M1.Dismount(DriveComboBox1.drive) then
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

procedure TOTFEE4MTestApp_F.RefreshDriveComboBox();
var
  origCase: TTextCase;
begin
  origCase := DriveComboBox1.TextCase;

  DriveComboBox1.TextCase := tcUpperCase;
  DriveComboBox1.TextCase := tcLowerCase;

  DriveComboBox1.TextCase := origCase;
end;

procedure TOTFEE4MTestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.Clear();

end;


procedure TOTFEE4MTestApp_F.rgActiveClick(Sender: TObject);
begin
  try
    OTFEE4M1.Active := (rgActive.ItemIndex=0);
  finally
    ReportWhetherActive();
  end;

end;


procedure TOTFEE4MTestApp_F.pbGetFileMountedForDriveClick(Sender: TObject);
var
  output: string;
begin

  output := DriveComboBox1.drive +
            ': is E4M volume file: "' +
            OTFEE4M1.GetVolFileForDrive(DriveComboBox1.drive) +
            '"';

  RichEdit1.lines.add(output);

end;

procedure TOTFEE4MTestApp_F.pbGetDriveMountedForFileClick(Sender: TObject);
var
  output: string;
  drive: char;
begin
   drive := OTFEE4M1.GetDriveForVolFile(edTestIfMtdVolFile.text);
   if drive=#0 then
     begin
     output := '"' +
              edTestIfMtdVolFile.text +
              '" not mounted';
     end
   else
     begin
     output := '"' +
              edTestIfMtdVolFile.text +
              '" is mounted as: '+
              OTFEE4M1.GetDriveForVolFile(edTestIfMtdVolFile.text) +
              ':';
     end;

  RichEdit1.lines.add(output);

end;

procedure TOTFEE4MTestApp_F.FormCreate(Sender: TObject);
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
    edTestIfMtdVolFile.text := 'c:\e4mcipher.vol';
    end
  else
    begin
    edTestIfMtdVolFile.text := 'E:\VolumeFiles\E4Mtest_1.vol';
    end;


  edTestIfMtdVolFile.text := 'C:\mini_scramble.svl';
  edTestIfMtdVolFile.text := 'C:\Copy of e4mcipher1234567890123456789012345678901234567890123456789012345678901234567890.vol';


end;

procedure TOTFEE4MTestApp_F.pbNumDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of volumes mounted: ';
  output := output + inttostr(OTFEE4M1.CountDrivesMounted());

  RichEdit1.lines.add(output);

end;

procedure TOTFEE4MTestApp_F.pbRefreshClick(Sender: TObject);
begin
  RefreshDriveComboBox();

end;

procedure TOTFEE4MTestApp_F.pbGetMountedDrivesClick(Sender: TObject);
begin
  RichEdit1.lines.Add('Drives currently mounted are: '+OTFEE4M1.DrivesMounted());

end;

procedure TOTFEE4MTestApp_F.pbDismountAllClick(Sender: TObject);
var
  output: string;
begin
  output := '';

  if length(OTFEE4M1.DismountAll())=0 then
    begin
    output := output + 'DismountAll OK';
    end
  else
    begin
    output := output + 'DismountAll FAILED';
    end;

  RichEdit1.lines.add(output);

end;



procedure TOTFEE4MTestApp_F.pbGetDriveInfoClick(Sender: TObject);
var
  info: TOTFEE4MVolumeInfo;
begin
  info := OTFEE4M1.GetDriveInfo(DriveComboBox1.drive);
  lblFilename.caption := info.volumeFilename;
  lblDriveMountedAs.caption := info.mountedAs;
  lblAlgorithm.caption := info.cipherName;
  lblKeyGen.caption := info.hashName;
  lblVolumeType.caption := info.volumeTypeName;

  if info.readonly then
    begin
    lblReadOnly.caption := 'Readonly';
    end
  else
    begin
    lblReadOnly.caption := 'Read/Write';
    end;

end;

procedure TOTFEE4MTestApp_F.ckScramDiskSupportClick(Sender: TObject);
begin
  OTFEE4M1.SupportScramDisk := ckScramDiskSupport.checked;

  // In case it can't do ScramDisk...
  ckScramDiskSupport.checked := OTFEE4M1.SupportScramDisk;

end;

procedure TOTFEE4MTestApp_F.pbIsDriverInstalledClick(Sender: TObject);
begin
  if OTFEE4M1.IsDriverInstalled() then
    begin
    RichEdit1.lines.add('Driver installed');
    end
  else
    begin
    RichEdit1.lines.add('Driver NOT installed');
    end;

end;

procedure TOTFEE4MTestApp_F.pbGetAvailableRawDevicesClick(Sender: TObject);
var
  stlTempDisp: TStringList;
  stlTempDevice: TStringList;
  i: integer;
begin
  RichEdit1.lines.Add('Raw devices available:');

  stlTempDisp:= TStringList.Create();
  try
    stlTempDevice:= TStringList.Create();
    try
      OTFEE4M1.GetAvailableRawDevices(stlTempDisp, stlTempDevice);
      for i:=0 to (stlTempDisp.count-1) do
        begin
        RichEdit1.lines.Add(stlTempDisp[i]+' (Mount as: '+stlTempDevice[i]+')');
        end;
    finally
      stlTempDevice.Free();
    end;
  finally
    stlTempDisp.Free();
  end;

end;

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN UNIT_PLATFORM ON}
// -----------------------------------------------------------------------------

END.


