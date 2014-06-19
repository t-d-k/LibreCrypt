unit OTFEScramDiskTestApp_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl, ComCtrls,
  OTFE_U, OTFEScramDisk_U, WindowsMissing_U, SdStructures_U, 
  OTFEDriveCrypt_U, Spin64;

type
  TOTFEScramDiskTestApp_F = class(TForm)
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
    lblFilePartitionNameStr: TLabel;
    lblDescriptionStr: TLabel;
    lblSlotNo: TLabel;
    lblHostDrive: TLabel;
    lblCipher: TLabel;
    lblMountType: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    reDescription: TRichEdit;
    Label1: TLabel;
    lblLastAccess: TLabel;
    Label6: TLabel;
    lblPreferredDrive: TLabel;
    Label9: TLabel;
    lblNoAccesses: TLabel;
    pbIsDriverInstalled: TButton;
    RichEdit1: TRichEdit;
    lblWavBitsUsedStr: TLabel;
    lblWavBitsUsed: TLabel;
    lblROSoftmountStr: TLabel;
    lblROSoftmount: TLabel;
    lblROMediaStr: TLabel;
    lblROMedia: TLabel;
    lblPartRemovableStr: TLabel;
    lblPartRemovable: TLabel;
    Label10: TLabel;
    lblViaSKF: TLabel;
    lblBFSStr: TLabel;
    lblBFS: TLabel;
    pbMountPartitionsPrompted: TButton;
    pbValidPartitions: TButton;
    GroupBox1: TGroupBox;
    pbDEBUG_GetDriveInfo: TButton;
    Label2: TLabel;
    ckSetEnableVolumeDeletion: TCheckBox;
    OTFEDriveCrypt1: TOTFEDriveCrypt;
    seDriveNo: TSpinEdit64;
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
    procedure pbIsDriverInstalledClick(Sender: TObject);
    procedure pbMountPartitionsPromptedClick(Sender: TObject);
    procedure pbDEBUG_GetDriveInfoClick(Sender: TObject);
    procedure pbValidPartitionsClick(Sender: TObject);
    procedure ckSetEnableVolumeDeletionClick(Sender: TObject);
  private
procedure ReportWhetherActive();
procedure RefreshDriveComboBox();
  public
    { Public declarations }
  end;

var
  OTFEScramDiskTestApp_F: TOTFEScramDiskTestApp_F;

implementation

{$R *.DFM}

procedure TOTFEScramDiskTestApp_F.ReportWhetherActive();
begin
  if OTFEDriveCrypt1.Active then
    begin
    RichEdit1.lines.add('ScramDisk component ACTIVE');
    rgActive.ItemIndex:=0;
    end
  else
    begin
    RichEdit1.lines.add('ScramDisk component NOT Active');
    rgActive.ItemIndex:=1;
    end;

end;

procedure TOTFEScramDiskTestApp_F.pbCloseClick(Sender: TObject);
begin
  Close;
  
end;

procedure TOTFEScramDiskTestApp_F.pbVersionClick(Sender: TObject);
begin
  RichEdit1.lines.add('ScramDisk driver version: 0x'+inttohex(OTFEDriveCrypt1.Version(), 1));

end;

procedure TOTFEScramDiskTestApp_F.pbIsEncryptedVolumeClick(Sender: TObject);
var
  filename: string;
  output: string;
begin
  filename := edTestIfMtdVolFile.text;

  output := filename;

  if OTFEDriveCrypt1.IsEncryptedVolFile(filename) then
    begin
    output := output + ' IS ';
    end
  else
    begin
    output := output + ' is NOT ';
    end;

  output := output + 'a ScramDisk volume file';

  RichEdit1.lines.add(output);

end;

procedure TOTFEScramDiskTestApp_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edTestIfMtdVolFile.text;
  if OpenDialog1.execute() then
    begin
    edTestIfMtdVolFile.text := OpenDialog1.FileName;
    end;

end;

procedure TOTFEScramDiskTestApp_F.pbMountVolumeClick(Sender: TObject);
begin
  try
    if OTFEDriveCrypt1.Mount(edTestIfMtdVolFile.text)<>#0 then
      begin
      RichEdit1.lines.add(edTestIfMtdVolFile.text+' mounted OK');
      end
    else
      begin
      RichEdit1.lines.add(edTestIfMtdVolFile.text+' mount failed');
      end;
  except
{
    on E: EkrScramDiskVxdBadStatus do
      begin
      showmessage('CAUGHT AN EkrScramDiskVxdBadStatus');
      end;
}
  end;

  RefreshDriveComboBox();

end;

procedure TOTFEScramDiskTestApp_F.pbDisountVolumeClick(Sender: TObject);
begin
  if OTFEDriveCrypt1.Dismount(edTestIfMtdVolFile.text) then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismount failed');
    end;

  RefreshDriveComboBox();

end;


procedure TOTFEScramDiskTestApp_F.pbDismountDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';

  if OTFEDriveCrypt1.Dismount(DriveComboBox1.drive) then
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

procedure TOTFEScramDiskTestApp_F.RefreshDriveComboBox();
var
  origCase: TTextCase;
begin
  origCase := DriveComboBox1.TextCase;

  DriveComboBox1.TextCase := tcUpperCase;
  DriveComboBox1.TextCase := tcLowerCase;

  DriveComboBox1.TextCase := origCase;
end;

procedure TOTFEScramDiskTestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.Clear();

end;

procedure TOTFEScramDiskTestApp_F.pbGetVolumeInfo2Click(Sender: TObject);
var
  info: TSlotInfo;
  OSversion: DWORD;
  RunningOnNT: boolean;
begin
  OTFEDriveCrypt1.GetVolumeInfo(DriveComboBox1.drive, info);

  OSversion := GetVersion();
  RunningOnNT := (OSversion<$80000000);

  lblSlotNo.caption := inttostr(info.SlotNo);
  if info.HostDriveLetter<>#0 then
    begin
    lblHostDrive.caption := info.HostDriveLetter;
    end
  else
    begin
    lblHostDrive.caption := 'n/a';
    end;
  lblDriveMountedAs.caption := info.DriveMountedAs;

  if info.MountType=mtPartition then
    begin
    lblFilePartitionNameStr.caption := 'Partition:';
    if (RunningOnNT) then
      begin
      lblFilename.caption := info.FileName;
      end
    else
      begin
      lblFilename.caption := '(on HDD) '+info.FileName;
      end;
    end
  else
    begin
    lblFilePartitionNameStr.caption := 'Filename:';
    lblFilename.caption := info.FileName;
    end;

  reDescription.text := info.Comments;
  lblPreferredDrive.caption := info.PreferredDrive;
  lblNoAccesses.caption := inttostr(info.NoAccesses);
  lblLastAccess.caption := datetimetostr(info.LastAccess);

  lblWavBitsUsedStr.enabled := FALSE;
  lblWavBitsUsed.enabled := FALSE;
  lblWavBitsUsed.caption := '';
  lblPartRemovableStr.enabled := FALSE;
  lblPartRemovable.enabled := FALSE;
  lblPartRemovable.caption := '';

  if (RunningOnNT) then
    begin
    lblBFSStr.enabled := FALSE;
    lblBFS.caption := '';
    end;

  if info.readonly then
    begin
    lblReadOnly.caption := 'Readonly';
    end
  else
    begin
    lblReadOnly.caption := 'Read/Write';
    end;

  if info.softReadOnly then
    begin
    lblROSoftmount.caption := 'TRUE';
    end
  else
    begin
    lblROSoftmount.caption := 'FALSE';
    end;

  if info.mediaReadOnly then
    begin
    lblROMedia.caption := 'TRUE';
    end
  else
    begin
    lblROMedia.caption := 'FALSE';
    end;

  case info.MountType of
    mtNotMounted:
      begin
      lblMountType.caption := 'Not mounted';
      end;

    mtPartition:
      begin
      lblMountType.caption := 'Partition';
      if info.partitionRemovable then
        begin
        lblPartRemovable.caption := 'TRUE';
        end
      else
        begin
        lblPartRemovable.caption := 'FALSE';
        end;
      lblPartRemovableStr.enabled := TRUE;
      lblPartRemovable.enabled := TRUE;
      end;

    mtSVLFile:
      begin
      lblMountType.caption := 'SVL file';
      end;

    mtWAVFile:
      begin
      lblMountType.caption := 'WAV file';
      lblWavBitsUsed.caption := inttostr(info.wavFileBits);
      lblWavBitsUsedStr.enabled := TRUE;
      lblWavBitsUsed.enabled := TRUE;
      end;

  else
    lblMountType.caption := '<Unknown>';
  end;

  if info.viaSKFFile then
    begin
    lblViaSKF.caption := 'TRUE';
    end
  else
    begin
    lblViaSKF.caption := 'FALSE';
    end;

  if not(RunningOnNT) then
    begin
    if info.bfs then
      begin
      lblBFS.caption := 'TRUE';
      end
    else
      begin
      lblBFS.caption := 'FALSE';
      end;
    end;

  lblCipher.caption := info.CipherType;
end;


procedure TOTFEScramDiskTestApp_F.rgActiveClick(Sender: TObject);
begin
  try
    OTFEDriveCrypt1.Active := (rgActive.ItemIndex=0);
  finally
    ReportWhetherActive();
  end;

end;


procedure TOTFEScramDiskTestApp_F.pbGetFileMountedForDriveClick(Sender: TObject);
var
  output: string;
begin

  output := DriveComboBox1.drive +
            ': is ScramDisk volume file: "' +
            OTFEDriveCrypt1.GetVolFileForDrive(DriveComboBox1.drive) +
            '"';

  RichEdit1.lines.add(output);

end;

procedure TOTFEScramDiskTestApp_F.pbGetDriveMountedForFileClick(Sender: TObject);
var
  output: string;
  drive: char;
  driveStr: string;
begin
  drive := OTFEDriveCrypt1.GetDriveForVolFile(edTestIfMtdVolFile.text);

  driveStr := drive;
  if drive=#0 then
    begin
    driveStr := '<not mounted>';
    end
  else
    begin
    driveStr := driveStr + ':';
    end;

  output := '"' +
            edTestIfMtdVolFile.text +
            '" is mounted as: '+
            driveStr;

  RichEdit1.lines.add(output);

end;

procedure TOTFEScramDiskTestApp_F.FormCreate(Sender: TObject);
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
    edTestIfMtdVolFile.text := 'F:\scramdisk - delete after use\testVol1.svl';
    end
  else
    begin
    edTestIfMtdVolFile.text := 'e:\scramble.svl';
    end;

end;

procedure TOTFEScramDiskTestApp_F.pbNumDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of volumes mounted: ';
  output := output + inttostr(OTFEDriveCrypt1.CountDrivesMounted());

  RichEdit1.lines.add(output);

end;

procedure TOTFEScramDiskTestApp_F.pbRefreshClick(Sender: TObject);
begin
  RefreshDriveComboBox();

end;

procedure TOTFEScramDiskTestApp_F.pbGetMountedDrivesClick(Sender: TObject);
begin
  RichEdit1.lines.Add('Drives currently mounted are: '+OTFEDriveCrypt1.DrivesMounted());

end;

procedure TOTFEScramDiskTestApp_F.pbDismountAllClick(Sender: TObject);
var
  output: string;
begin
  output := '';

  if length(OTFEDriveCrypt1.DismountAll())=0 then
    begin
    output := output + 'DismountAll OK';
    end
  else
    begin
    output := output + 'DismountAll FAILED';
    end;

  RichEdit1.lines.add(output);

end;

procedure TOTFEScramDiskTestApp_F.pbIsDriverInstalledClick(Sender: TObject);
begin
  if OTFEDriveCrypt1.IsDriverInstalled() then
    begin
    RichEdit1.lines.add('Driver installed');
    end
  else
    begin
    RichEdit1.lines.add('Driver NOT installed');
    end;

end;

procedure TOTFEScramDiskTestApp_F.pbMountPartitionsPromptedClick(
  Sender: TObject);
var
  drives: string;
begin
  drives:= OTFEDriveCrypt1.MountPartitionsPrompted();

  RichEdit1.lines.add('Partitions mounted: '+drives);


end;

procedure TOTFEScramDiskTestApp_F.pbDEBUG_GetDriveInfoClick(Sender: TObject);
var
  li: TDRIVE_LAYOUT_INFORMATION;
  i: integer;
  partIdentified: string;
begin
  OTFEDriveCrypt1.DEBUG_GetDriveInfo(seDriveNo.value, li);

  RichEdit1.lines.add('ScramDisk component DEBUG_GetDriveInfo for drive: '+inttostr(seDriveNo.value));
  if li.partitioncount>0 then
    begin
    for i:=0 to (li.partitioncount) do
      begin
        RichEdit1.lines.add('Partition '+
                            inttostr(i)+'/'+inttostr(li.partitioncount)+
                            ': PartitionNumber='+inttostr(li.PartitionEntry[i].PartitionNumber)+
                            ' type='+inttostr(li.PartitionEntry[i].partitiontype)+
                            ' ($'+inttohex(li.PartitionEntry[i].partitiontype, 1)+')'+
                            ' recognised='+  inttostr(li.PartitionEntry[i].RecognizedPartition)
                           );

      case li.PartitionEntry[i].partitiontype of
        0:   partIdentified := 'PARTITION_ENTRY_UNUSED';
        1:   partIdentified := 'PARTITION_FAT_12';
        2:   partIdentified := 'PARTITION_XENIX_1';
        3:   partIdentified := 'PARTITION_XENIX_2';
        4:   partIdentified := 'PARTITION_FAT_16';
        5:   partIdentified := 'PARTITION_EXTENDED';
        6:   partIdentified := 'PARTITION_HUGE';
        7:   partIdentified := 'PARTITION_IFS';
        65:  partIdentified := 'PARTITION_PREP';
        99:  partIdentified := 'PARTITION_UNIX';
        192: partIdentified := 'VALID_NTFT';
        14:  partIdentified := 'PARTITION_XINT13';
        15:  partIdentified := 'PARTITION_XINT13_EXTENDED';

        $74: partIdentified := '*** SCRAMDISK ***';
      else
        partIdentified := '<unknown>';
      end;

      richedit1.lines.add('     '+partIdentified);

    end;
  end;

end;

procedure TOTFEScramDiskTestApp_F.pbValidPartitionsClick(
  Sender: TObject);
var
  partitionNames: TStringList;
begin
  richedit1.lines.add('Valid ScramDisk partitions are:');
  partitionNames:= TStringList.Create();;
  try
    if OTFEDriveCrypt1.ValidScramDiskPartitions(partitionNames) then
      begin
      richedit1.lines.AddStrings(partitionNames);
      end
    else
      begin
      richedit1.lines.add('<none>');
      end;
  finally
    partitionNames.Free();
  end;


end;

procedure TOTFEScramDiskTestApp_F.ckSetEnableVolumeDeletionClick(Sender: TObject);
begin
  if OTFEDriveCrypt1.SetEnableVolumeDeletion(ckSetEnableVolumeDeletion.checked) then
    begin
    richedit1.lines.add('SetEnableVolumeDeletion succeeeded');
    end
  else
    begin
    richedit1.lines.add('SetEnableVolumeDeletion failed');
    end;

end;

END.


