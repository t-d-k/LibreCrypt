unit OTFEBestCryptTestApp_U;
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
  StdCtrls, ExtCtrls, ComCtrls,
  OTFEBestCrypt_U, OTFEBestCryptStructures_U, OTFE_U, FileCtrl;

type
  TBestCryptTestApp_F = class(TForm)
    pbClose: TButton;
    pbVersion: TButton;
    pbIsEncryptedVolFile: TButton;
    edTestIfMtdVolFile: TEdit;
    DriveComboBox1: TDriveComboBox;
    pbBrowse: TButton;
    pbDisountVolume: TButton;
    pbMountVolume: TButton;
    pbDismountDrive: TButton;
    pbClear: TButton;
    pbGetVolumeInfo2: TButton;
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
    Label3: TLabel;
    lblExtent: TLabel;
    lblVersion: TLabel;
    lblAlgorithm: TLabel;
    lblFileSystemID: TLabel;
    lblKeyGen: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    reDescription: TRichEdit;
    pbGetKeyGenIDs: TButton;
    pbGetAlgIDs: TButton;
    pbGetVolumeInfo: TButton;
    Label1: TLabel;
    lblErrsInMounting: TLabel;
    pbIsDriverInstalled: TButton;
    OTFEBestCrypt1: TOTFEBestCrypt;
    RichEdit1: TRichEdit;
    procedure pbCloseClick(Sender: TObject);
    procedure rgActiveClick(Sender: TObject);
    procedure pbVersionClick(Sender: TObject);
    procedure pbNumDrivesMountedClick(Sender: TObject);
    procedure pbDismountDriveClick(Sender: TObject);
    procedure pbGetVolumeInfo2Click(Sender: TObject);
    procedure pbGetFileMountedForDriveClick(Sender: TObject);
    procedure pbIsEncryptedVolumeClick(Sender: TObject);
    procedure pbMountVolumeClick(Sender: TObject);
    procedure pbDisountVolumeClick(Sender: TObject);
    procedure pbGetDriveMountedForFileClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure pbGetMountedDrivesClick(Sender: TObject);
    procedure pbDismountAllClick(Sender: TObject);
    procedure pbGetKeyGenIDsClick(Sender: TObject);
    procedure pbGetAlgIDsClick(Sender: TObject);
    procedure pbGetVolumeInfoClick(Sender: TObject);
    procedure pbIsDriverInstalledClick(Sender: TObject);
  private
    procedure ReportWhetherActive();
    procedure RefreshDriveComboBox();
    procedure DisplayVolumeInfo(info: TBCDiskInfo);
  public
    { Public declarations }
  end;

var
  BestCryptTestApp_F: TBestCryptTestApp_F;

implementation

{$R *.DFM}

procedure TBestCryptTestApp_F.ReportWhetherActive();
begin
  if OTFEBestCrypt1.Active then
    begin
    RichEdit1.lines.add('BestCrypt component ACTIVE');
    rgActive.ItemIndex:=0;
    end
  else
    begin
    RichEdit1.lines.add('BestCrypt component NOT Active');
    rgActive.ItemIndex:=1;
    end;

end;

procedure TBestCryptTestApp_F.pbCloseClick(Sender: TObject);
begin
  Close;
  
end;

procedure TBestCryptTestApp_F.pbVersionClick(Sender: TObject);
begin
  RichEdit1.lines.add('BestCrypt driver version: 0x'+inttohex(OTFEBestCrypt1.Version(), 1));

end;

procedure TBestCryptTestApp_F.pbIsEncryptedVolumeClick(Sender: TObject);
var
  filename: string;
  output: string;
begin
  filename := edTestIfMtdVolFile.text;

  output := filename;

  if OTFEBestCrypt1.IsEncryptedVolFile(filename) then
    begin
    output := output + ' IS ';
    end
  else
    begin
    output := output + ' is NOT ';
    end;

  output := output + 'a BestCrypt volume file';

  RichEdit1.lines.add(output);

end;

procedure TBestCryptTestApp_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edTestIfMtdVolFile.text;
  if OpenDialog1.execute() then
    begin
    edTestIfMtdVolFile.text := OpenDialog1.FileName;
    end;

end;

procedure TBestCryptTestApp_F.pbMountVolumeClick(Sender: TObject);
begin
  try
    if OTFEBestCrypt1.Mount(edTestIfMtdVolFile.text)<>#0 then
      begin
      RichEdit1.lines.add(edTestIfMtdVolFile.text+' mounted OK');
      end
    else
      begin
      RichEdit1.lines.add(edTestIfMtdVolFile.text+' mount failed');
      end;
  except
    on E: EBestCryptVxdBadStatus do
      begin
      showmessage('CAUGHT AN EBestCryptVxdBadStatus');
      end;
  end;

  RefreshDriveComboBox();

end;

procedure TBestCryptTestApp_F.pbDisountVolumeClick(Sender: TObject);
begin
  if OTFEBestCrypt1.Dismount(edTestIfMtdVolFile.text) then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismount failed');
    end;

  RefreshDriveComboBox();

end;


procedure TBestCryptTestApp_F.pbDismountDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';

  if OTFEBestCrypt1.Dismount(DriveComboBox1.drive) then
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

procedure TBestCryptTestApp_F.RefreshDriveComboBox();
var
  origCase: TTextCase;
begin
  origCase := DriveComboBox1.TextCase;

  DriveComboBox1.TextCase := tcUpperCase;
  DriveComboBox1.TextCase := tcLowerCase;

  DriveComboBox1.TextCase := origCase;
end;

procedure TBestCryptTestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.Clear();

end;

procedure TBestCryptTestApp_F.pbGetVolumeInfo2Click(Sender: TObject);
var
  info: TBCDiskInfo;
begin
  info := TBCDiskInfo.Create();
  try
    if OTFEBestCrypt1.GetVolumeInfo(DriveComboBox1.drive, info) then
      begin
      DisplayVolumeInfo(info);
      end
    else
      begin
      RichEdit1.lines.add('Cannot get disk info');
      end;

  finally
    info.Free();
  end

end;


procedure TBestCryptTestApp_F.rgActiveClick(Sender: TObject);
begin
  try
    OTFEBestCrypt1.Active := (rgActive.ItemIndex=0);
  finally
    ReportWhetherActive();
  end;

end;


procedure TBestCryptTestApp_F.pbGetFileMountedForDriveClick(Sender: TObject);
var
  output: string;
begin

  output := DriveComboBox1.drive +
            ': is BestCrypt volume file: "' +
            OTFEBestCrypt1.GetVolFileForDrive(DriveComboBox1.drive) +
            '"';

  RichEdit1.lines.add(output);

end;

procedure TBestCryptTestApp_F.pbGetDriveMountedForFileClick(Sender: TObject);
var
  output: string;
begin
   output := '"' +
            edTestIfMtdVolFile.text +
            '" is mounted as: '+
            OTFEBestCrypt1.GetDriveForVolFile(edTestIfMtdVolFile.text) +
            ':';

  RichEdit1.lines.add(output);

end;

procedure TBestCryptTestApp_F.FormCreate(Sender: TObject);
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
    edTestIfMtdVolFile.text := 'c:\bc_test2.jbc';
    end
  else
    begin
    edTestIfMtdVolFile.text := 'E:\BestCrypt VolumeFile.jbc';
    end;

end;

procedure TBestCryptTestApp_F.pbNumDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of volumes mounted: ';
  output := output + inttostr(OTFEBestCrypt1.CountDrivesMounted());

  RichEdit1.lines.add(output);

end;

procedure TBestCryptTestApp_F.pbRefreshClick(Sender: TObject);
begin
  RefreshDriveComboBox();

end;

procedure TBestCryptTestApp_F.pbGetMountedDrivesClick(Sender: TObject);
begin
  RichEdit1.lines.Add('Drives currently mounted are: '+OTFEBestCrypt1.DrivesMounted());

end;

procedure TBestCryptTestApp_F.pbDismountAllClick(Sender: TObject);
var
  output: string;
begin
  output := '';

  if length(OTFEBestCrypt1.DismountAll())=0 then
    begin
    output := output + 'DismountAll OK';
    end
  else
    begin
    output := output + 'DismountAll FAILED';
    end;

  RichEdit1.lines.add(output);

end;

procedure TBestCryptTestApp_F.pbGetKeyGenIDsClick(Sender: TObject);
var
  ids: TStringList;
  i: integer;
begin
  ids := TStringList.Create();
  try
    OTFEBestCrypt1.GetAllKeyGenIDs(ids);
    for i:=0 to (ids.count-1) do
      begin
      RichEdit1.lines.add('Key generator ID '+ids[i]+':');
      RichEdit1.lines.add('    '+OTFEBestCrypt1.GetKeyGenName(strtoint(ids[i])));
      RichEdit1.lines.add('    Version: 0x'+inttohex(OTFEBestCrypt1.GetKeyGenVersion(strtoint(ids[i])), 4));
      RichEdit1.lines.add('    Stored in DLL: '+OTFEBestCrypt1.GetKeyGenDLLName(strtoint(ids[i])));
      end;
  finally
    ids.Free();
  end;

end;

procedure TBestCryptTestApp_F.pbGetAlgIDsClick(Sender: TObject);
var
  ids: TStringList;
  i: integer;
begin

  ids := TStringList.Create();
  try
    OTFEBestCrypt1.GetAllAlgorithmIDs(ids);
    for i:=0 to (ids.count-1) do
      begin
      RichEdit1.lines.add('Algorithm ID '+ids[i]+':');
      RichEdit1.lines.add('    '+OTFEBestCrypt1.GetAlgorithmName(strtoint(ids[i])));
      RichEdit1.lines.add('    Version: 0x'+inttohex(OTFEBestCrypt1.GetAlgorithmVersion(strtoint(ids[i])), 4));
      RichEdit1.lines.add('    Key length: '+inttostr(OTFEBestCrypt1.GetAlgorithmKeyLength(strtoint(ids[i]))));
      RichEdit1.lines.add('    Driver name: '+OTFEBestCrypt1.GetAlgorithmDriverName(strtoint(ids[i])));
      end;
  finally
    ids.Free();
  end;

end;


procedure TBestCryptTestApp_F.pbGetVolumeInfoClick(Sender: TObject);
var
  info: TBCDiskInfo;
begin
  info := TBCDiskInfo.Create();
  try
    if OTFEBestCrypt1.GetVolumeInfo(edTestIfMtdVolFile.text, info) then
      begin
      DisplayVolumeInfo(info);
      end
    else
      begin
      RichEdit1.lines.add('Cannot get disk info');
      end;

  finally
    info.Free();
  end

end;

procedure TBestCryptTestApp_F.DisplayVolumeInfo(info: TBCDiskInfo);
var
  algorithmProb: boolean;
begin
  lblDriveMountedAs.caption := '<not mounted>';
  lblReadOnly.caption := '<not mounted>';
  lblFilename.caption := '<not mounted>';

  if info.volumeFilename<>'' then
    begin
    lblFilename.caption := info.volumeFilename;
    end;

  if info.mountedAs<>#0 then
    begin
    lblDriveMountedAs.caption := info.mountedAs + ':';
    if info.readOnly then
      begin
      lblReadOnly.caption := 'Readonly';
      end
    else
      begin
      lblReadOnly.caption := 'Read/Write';
      end;

    end;

  lblExtent.caption := '0x'+inttohex(info.extent, 2);
  lblVersion.caption := '0x'+inttohex(info.version, 2);
  lblFileSystemID.caption := '0x'+inttohex(info.fileSystemId, 4);

  algorithmProb := FALSE;
  if info.algorithmName='' then
    begin
    algorithmProb := TRUE;
    lblAlgorithm.caption := '???';
    end
  else
    begin
    lblAlgorithm.caption := info.algorithmName;
    end;
  if info.algorithmKeyLen=-1 then
    begin
    algorithmProb := TRUE;
    lblAlgorithm.caption := lblAlgorithm.caption + ' (??? bit)';
    end
  else
    begin
    lblAlgorithm.caption := lblAlgorithm.caption+' ('+inttostr(info.algorithmKeyLen)+' bit)';
    end;
  if algorithmProb then
    begin
    lblAlgorithm.caption := lblAlgorithm.caption+' (ID=0x'+inttohex(info.algorithmId, 1)+')';
    end;
  if info.keyGenName='' then
    begin
    lblKeyGen.caption := '??? (0x'+inttostr(info.keyGenId)+')';
    end
  else
    begin
    lblKeyGen.caption := info.keyGenName;
    end;

  reDescription.text := info.description;
  lblErrsInMounting.caption := inttostr(info.errsInMounting);

end;

procedure TBestCryptTestApp_F.pbIsDriverInstalledClick(Sender: TObject);
begin
  if OTFEBestCrypt1.IsDriverInstalled() then
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

