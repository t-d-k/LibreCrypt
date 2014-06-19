unit SDUDiskPartitionsPanel;

interface

uses
  SysUtils,
  SDUBlocksPanel,
  SDUGeneral,
  Classes,
  SDUObjectManager;

type
  // Exceptions...
  ESDUPartitionsException = Exception;
  ESDUUnableToGetDriveLayout = ESDUPartitionsException;

const
  NO_DISK      = -1;
  NO_PARTITION = -1;

resourcestring
  UNABLE_TO_GET_DISK_LAYOUT = 'Unable to get disk layout for disk: %1';

type
  TSDUDiskPartitionsPanel = class(TSDUBlocksPanel)
  private
    FDiskNumber: integer;
    FDriveLayoutInfo: TSDUDriveLayoutInformation;
    FDriveLayoutInfoValid: boolean;
    FShowPartitionsWithPNZero: boolean;

    // The index into this TStringList is the block number
    // The objects of the TStringList are the indexes into FDriveLayoutInfo's
    // "Partition" member
    FMapBlockToPartition: TStringList;

    objMgr: TSDUObjManager;
    // Strings are the device names for the drives, Objects are the ordinal
    // value of the drive letter
    driveDevices: TStringList;

    procedure SetDiskNumber(DiskNo: integer);
    function GetPartitionInfo(idx: integer): TSDUPartitionInfo;

    function GetDriveLetter(idx: integer): char;
    function GetDriveLetterForPartition(DiskNo: integer; PartitionNo: integer): char;

    procedure RefreshPartitions();
  protected
    function  GetDriveLayout(physicalDiskNo: integer; var driveLayout: TSDUDriveLayoutInformation): boolean; virtual;
    function  IgnorePartition(partInfo: TSDUPartitionInfo): boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    property PartitionInfo[idx: integer]: TSDUPartitionInfo read GetPartitionInfo;
    property DriveLetter[idx: integer]: char read GetDriveLetter;

    procedure Clear(); override;

  published
    // Show partitions with partition number zero
    // USE WITH CAUTION! Referencing the partitions with partition number zero
    // mean the entire drive! (Partition number zero) 
    property ShowPartitionsWithPNZero: boolean read FShowPartitionsWithPNZero write FShowPartitionsWithPNZero default FALSE;

    // Set to NO_DISK for no disk display
    property DiskNumber: integer read FDiskNumber write SetDiskNumber default NO_DISK;

    property DriveLayoutInformation: TSDUDriveLayoutInformation read FDriveLayoutInfo;
    property DriveLayoutInformationValid: boolean read FDriveLayoutInfoValid;
  end;

procedure Register;

implementation

uses
  Math,
  Windows;

const
  ULL_ONEMEG: ULONGLONG = 1024*1024;

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUDiskPartitionsPanel]);
end;


constructor TSDUDiskPartitionsPanel.Create(AOwner: TComponent);
begin
  inherited;
  FMapBlockToPartition := TStringList.Create();
  FDiskNumber := NO_DISK;
end;

destructor TSDUDiskPartitionsPanel.Destroy();
begin
  FMapBlockToPartition.Free();

  if (objMgr <> nil) then
    begin
    objMgr.Free();
    end;

  if (driveDevices <> nil) then
    begin
    driveDevices.Free();
    end;

  inherited;
end;

procedure TSDUDiskPartitionsPanel.Clear();
begin
  inherited;
  FMapBlockToPartition.Clear();
end;

procedure TSDUDiskPartitionsPanel.RefreshPartitions();
var
  i: integer;
  blk: TBlock;
  idx: integer;
  cnt: ULONGLONG;
  oneMeg: ULONGLONG;
  // Temp variables to prevent Delphi converting to (signed!) int64 or
  // (signed!) float during math operations
  // For the same reason, calculations are carried out step-by-step 
  tmpA: ULONGLONG;
  tmpB: ULONGLONG;
  currPartition: TSDUPartitionInfo;
  minPcnt: double;
  totalPcnt: double;
  driveLetter: char;
  drive: string;
  prettyPartitionType: string;
begin
  Self.Clear();
  FDriveLayoutInfoValid := FALSE;

  if (DiskNumber >= 0) then
    begin
    FDriveLayoutInfo.PartitionCount := 0;
    if not(GetDriveLayout(DiskNumber, FDriveLayoutInfo)) then
      begin
      // Call invalidate to ensure the display is refreshed
      Invalidate();
      raise ESDUUnableToGetDriveLayout.Create(SDUParamSubstitute(UNABLE_TO_GET_DISK_LAYOUT, [DiskNumber]));
      end
    else
      begin
      FDriveLayoutInfoValid := TRUE;
      oneMeg := 1024*1024;
      cnt := 0;
      for i:=0 to (FDriveLayoutInfo.PartitionCount - 1)do
        begin
        if IgnorePartition(FDriveLayoutInfo.PartitionEntry[i]) then
          begin
          // Extended partition, or similar
          continue;
          end;

        FMapBlockToPartition.AddObject(inttohex(FDriveLayoutInfo.PartitionEntry[i].StartingOffset, 16), TObject(i));
        tmpA := (FDriveLayoutInfo.PartitionEntry[i].PartitionLength div oneMeg);
        cnt := cnt + tmpA;
        end;

      // Partitions within an extended partition reset their partition offset to
      // zero, so the above is actually storing the wrong offsets; otherwise we
      // *should* be doing this...
      //   FMapBlockToPartition.Sort;

      // The percentages are the percentage of the whole disk, with a min
      // percentage of:
      minPcnt := 0;
      if (FMapBlockToPartition.count > 0) then
        begin
        minPcnt := 100 / (FMapBlockToPartition.count * 2);
        end;
      for i:=0 to (FMapBlockToPartition.count - 1) do
        begin
        idx := integer(FMapBlockToPartition.Objects[i]);
        currPartition := FDriveLayoutInfo.PartitionEntry[idx];

        tmpA := (currPartition.PartitionLength div oneMeg);
        if (tmpA = 0) then
          begin
          tmpB := 0;
          end
        else
          begin
          tmpB := (cnt div tmpA);
          end;
        if (tmpB = 0) then
          begin
          blk.Percentage := 0;
          end
        else
          begin
          blk.Percentage := ((1 / tmpB) * 100);
          end;

        blk.Percentage := max(blk.Percentage, minPcnt);


        blk.Data := TObject(idx);
        drive := ' '; // Needs to be set to *something* or it won't be
                      // displayed as a blank line if empty
        driveLetter := GetDriveLetterForPartition(DiskNumber, currPartition.PartitionNumber);
        if (driveLetter <> #0) then
          begin
          drive := driveLetter + ':';
          end;

        prettyPartitionType := SDUPartitionType(currPartition.PartitionType, FALSE);
        
        blk.Caption := SDUFormatAsBytesUnits(currPartition.PartitionLength)+SDUCRLF+
                       drive;
        blk.SubCaption := prettyPartitionType;
                          //'Idx: '+inttostr(idx)+SDUCRLF+
                          //'PN: '+inttostr(currPartition.PartitionNumber)+SDUCRLF+
                          //'PT: 0x'+inttohex(currPartition.PartitionType, 2);
        Add(blk);
        end;

      // Rejig the percentages so they add up to 100% (could be more at this
      // point, due to the min percentage and much larger disks)
      totalPcnt := 0;
      for i:=0 to (Count - 1) do
        begin
        blk := Item[i];
        totalPcnt := totalPcnt + blk.Percentage;
        Item[i] := blk;
        end;
      for i:=0 to (Count - 1) do
        begin
        blk := Item[i];
        blk.Percentage := (blk.Percentage / totalPcnt) * 100;
        Item[i] := blk;
        end;

      end;
    end;

end;

procedure TSDUDiskPartitionsPanel.SetDiskNumber(DiskNo: integer);
begin
  FDiskNumber:= DiskNo;
  RefreshPartitions();
  Invalidate();
end;

function TSDUDiskPartitionsPanel.GetPartitionInfo(idx: integer): TSDUPartitionInfo;
var
  lstIdx: integer;
begin
  // Sanity check...
  Assert(
         (
          (idx >= low(FBlocks)) or
          (idx <= high(FBlocks))
         ),
         'GetPartitionInfo called with bad index: '+inttostr(idx)
        );

  lstIdx := integer(FMapBlockToPartition.Objects[idx]);
  Result := FDriveLayoutInfo.PartitionEntry[lstIdx];
end;

// Returns #0 if drive letter can't be found
function TSDUDiskPartitionsPanel.GetDriveLetterForPartition(DiskNo: integer; PartitionNo: integer): char;
var
  retval: char;
  partitionDevice: string;
  partitionUnderlying: string;
  driveLetter: char;
  idx: integer;
  tmpDeviceName: string;
begin
  retval := #0;

  if (objMgr = nil) then
    begin
    objMgr := TSDUObjManager.Create();
    end;

  // Get the underlying device name for each drive letter...
  if (driveDevices = nil) then
    begin
    driveDevices := TStringList.Create();
    for driveLetter:='A' to 'Z' do
      begin
      tmpDeviceName := objMgr.UnderlyingDeviceForDrive(driveLetter);
      if (tmpDeviceName <> '') then
        begin
        driveDevices.AddObject(tmpDeviceName, TObject(ord(driveLetter)));
        end;
      end;
    end;

  // Get the partition device name for the disk/partition we're interested in...
  partitionDevice := SDUDeviceNameForPartition(DiskNo, PartitionNo);
  // Get the underlying device name for the disk/partition we're interested in...
  partitionUnderlying := objMgr.UnderlyingDeviceForName(partitionDevice);

  // Locate the drive letter for the underlying device name for the
  // disk/partition we're interested in...
  idx := driveDevices.IndexOf(partitionUnderlying);
  if (idx >= 0) then
    begin
    retval := char(integer(driveDevices.Objects[idx]));
    end;

  Result := retval;
end;

// Returns #0 if no partition selected/no drive letter assigned to partition
function TSDUDiskPartitionsPanel.GetDriveLetter(idx: integer): char;
var
  retval: char;
  partInfo: TSDUPartitionInfo;
begin
  retval := #0;

  if (idx >= 0) then
    begin
    partInfo := PartitionInfo[idx];
    retval := GetDriveLetterForPartition(DiskNumber, partInfo.PartitionNumber);
    end;

  Result := retval;
end;

function TSDUDiskPartitionsPanel.GetDriveLayout(physicalDiskNo: integer; var driveLayout: TSDUDriveLayoutInformation): boolean;
begin
  Result := SDUGetDriveLayout(DiskNumber, FDriveLayoutInfo);
end;

function TSDUDiskPartitionsPanel.IgnorePartition(partInfo: TSDUPartitionInfo): boolean;
var
  retval: boolean;
begin
  retval := (
             // USE CAUTION! We don't want the user selecting a partition, and
             // ending up using the whole HDD (partition number zero)
             (
              not(ShowPartitionsWithPNZero) and
              (partInfo.PartitionNumber = 0)
             ) or
             // Piddly partitions; extended partitions, etc
             (partInfo.PartitionLength < ULL_ONEMEG) or
             // $05/$0F = extended partition definition
             (partInfo.PartitionType = $05) or
             (partInfo.PartitionType = $0F)
            );

  Result := retval;
end;

END.

