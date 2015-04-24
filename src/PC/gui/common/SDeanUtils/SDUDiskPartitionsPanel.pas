unit SDUDiskPartitionsPanel;

interface

 {
 layers used are:
 //delphi and 3rd party libs - layer 0

   // LibreCrypt forms - layer 2
    //main form - layer 3
  }


uses
  //delphi and 3rd party libs - layer 0
  Classes, SysUtils,
  //sdu & LibreCrypt utils - layer 1
  SDUBlocksPanel,
  SDUGeneral,
  SDUObjectManager, Vcl.Controls, Vcl.StdCtrls;

type
  // Exceptions...
  ESDUPartitionsException    = Exception;
  ESDUUnableToGetDriveLayout = ESDUPartitionsException;

const
  NO_DISK      = -1;
  NO_PARTITION = -1;

resourcestring
  UNABLE_TO_GET_DISK_LAYOUT = 'Unable to get disk layout for disk: %1';

type
  TSDUDiskPartitionsPanel = class (TSDUBlocksPanel)
  private
    FDiskNumber:           Integer;
    FDriveLayoutInfo:      TSDUDriveLayoutInformation;
    FDriveLayoutInfoValid: Boolean;
    FShowPartitionsWithPNZero: Boolean;

    // The index into this TStringList is the block number
    // The objects of the TStringList are the indexes into FDriveLayoutInfo's
    // "Partition" member
    FMapBlockToPartition: TStringList;

    objMgr:       TSDUObjManager;
    // Strings are the device names for the drives, Objects are the ordinal
    // value of the drive letter
    driveDevices: TStringList;

    procedure SetDiskNumber(DiskNo: Integer);
    function GetPartitionInfo(idx: Integer): TSDUPartitionInfo;

    function GetDriveLetter(idx: Integer): Char;
    function GetDriveLetterForPartition(DiskNo: Integer; PartitionNo: Integer): DriveLetterChar;

    procedure RefreshPartitions();
  protected
    function GetDriveLayout(physicalDiskNo: Integer;
      var driveLayout: TSDUDriveLayoutInformation): Boolean; virtual;
    function IgnorePartition(partInfo: TSDUPartitionInfo): Boolean; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    property PartitionInfo[idx: Integer]: TSDUPartitionInfo Read GetPartitionInfo;
    property DriveLetter[idx: Integer]: Char Read GetDriveLetter;

    procedure Clear(); override;

  published
    // Show partitions with partition number zero
    // USE WITH CAUTION! Referencing the partitions with partition number zero
    // mean the entire drive! (Partition number zero) 
    property ShowPartitionsWithPNZero: Boolean Read FShowPartitionsWithPNZero
      Write FShowPartitionsWithPNZero default False;

    // Set to NO_DISK for no disk display
    property DiskNumber: Integer Read FDiskNumber Write SetDiskNumber default NO_DISK;

    property DriveLayoutInformation: TSDUDriveLayoutInformation Read FDriveLayoutInfo;
    property DriveLayoutInformationValid: Boolean Read FDriveLayoutInfoValid;
  end;

//procedure Register;

implementation

uses
  Math,
  Windows;

{$R *.dfm}

const
  ULL_ONEMEG: ULONGLONG = 1024 * 1024;

 //procedure Register;
 //begin
 //  RegisterComponents('SDeanUtils', [TSDUDiskPartitionsPanel]);
 //end;


constructor TSDUDiskPartitionsPanel.Create(AOwner: TComponent);
begin
  inherited;
  FMapBlockToPartition := TStringList.Create();
  FDiskNumber          := NO_DISK;
end;

destructor TSDUDiskPartitionsPanel.Destroy();
begin
  FMapBlockToPartition.Free();

  if (objMgr <> nil) then begin
    objMgr.Free();
  end;

  if (driveDevices <> nil) then begin
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
  i:                   Integer;
  blk:                 TBlock;
  idx:                 Integer;
  cnt:                 ULONGLONG;
  oneMeg:              ULONGLONG;
  // Temp variables to prevent Delphi converting to (signed!) int64 or
  // (signed!) float during math operations
  // For the same reason, calculations are carried out step-by-step 
  tmpA:                ULONGLONG;
  tmpB:                ULONGLONG;
  currPartition:       TSDUPartitionInfo;
  minPcnt:             Double;
  totalPcnt:           Double;
  driveLetter:         DriveLetterChar;
  drive:               String;
  prettyPartitionType: String;
begin
  Self.Clear();
  FDriveLayoutInfoValid := False;

  if (DiskNumber >= 0) then begin
    FDriveLayoutInfo.PartitionCount := 0;
    if not (GetDriveLayout(DiskNumber, FDriveLayoutInfo)) then begin
      // Call invalidate to ensure the display is refreshed
      Invalidate();
      raise ESDUUnableToGetDriveLayout.Create(SDUParamSubstitute(UNABLE_TO_GET_DISK_LAYOUT,
        [DiskNumber]));
    end else begin
      FDriveLayoutInfoValid := True;
      oneMeg                := 1024 * 1024;
      cnt                   := 0;
      for i := 0 to (FDriveLayoutInfo.PartitionCount - 1) do begin
        if IgnorePartition(FDriveLayoutInfo.PartitionEntry[i]) then begin
          // Extended partition, or similar
          continue;
        end;

        FMapBlockToPartition.AddObject(inttohex(FDriveLayoutInfo.PartitionEntry[i].StartingOffset,
          16), TObject(i));
        tmpA := (FDriveLayoutInfo.PartitionEntry[i].PartitionLength div oneMeg);
        cnt  := cnt + tmpA;
      end;

      // Partitions within an extended partition reset their partition offset to
      // zero, so the above is actually storing the wrong offsets; otherwise we
      // *should* be doing this...
      //   FMapBlockToPartition.Sort;

      // The percentages are the percentage of the whole disk, with a min
      // percentage of:
      minPcnt := 0;
      if (FMapBlockToPartition.Count > 0) then begin
        minPcnt := 100 / (FMapBlockToPartition.Count * 2);
      end;
      for i := 0 to (FMapBlockToPartition.Count - 1) do begin
        idx           := Integer(FMapBlockToPartition.Objects[i]);
        currPartition := FDriveLayoutInfo.PartitionEntry[idx];

        tmpA := (currPartition.PartitionLength div oneMeg);
        if (tmpA = 0) then begin
          tmpB := 0;
        end else begin
          tmpB := (cnt div tmpA);
        end;
        if (tmpB = 0) then begin
          blk.Percentage := 0;
        end else begin
          blk.Percentage := ((1 / tmpB) * 100);
        end;

        blk.Percentage := max(blk.Percentage, minPcnt);


        blk.Data    := TObject(idx);
        drive       := ' '; // Needs to be set to *something* or it won't be
                            // displayed as a blank line if empty
        driveLetter := GetDriveLetterForPartition(DiskNumber, currPartition.PartitionNumber);
        if (driveLetter <> #0) then begin
          drive := driveLetter + ':';
        end;

        prettyPartitionType := SDUPartitionType(currPartition.PartitionType, False);

        blk.Caption    := SDUFormatAsBytesUnits(currPartition.PartitionLength) +
          SDUCRLF + drive;
        blk.SubCaption := prettyPartitionType;
        //'Idx: '+inttostr(idx)+SDUCRLF+
        //'PN: '+inttostr(currPartition.PartitionNumber)+SDUCRLF+
        //'PT: 0x'+inttohex(currPartition.PartitionType, 2);
        Add(blk);
      end;

      // Rejig the percentages so they add up to 100% (could be more at this
      // point, due to the min percentage and much larger disks)
      totalPcnt := 0;
      for i := 0 to (Count - 1) do begin
        blk       := Item[i];
        totalPcnt := totalPcnt + blk.Percentage;
        Item[i]   := blk;
      end;
      for i := 0 to (Count - 1) do begin
        blk            := Item[i];
        blk.Percentage := (blk.Percentage / totalPcnt) * 100;
        Item[i]        := blk;
      end;

    end;
  end;

end;

procedure TSDUDiskPartitionsPanel.SetDiskNumber(DiskNo: Integer);
begin
  FDiskNumber := DiskNo;
  RefreshPartitions();
  Invalidate();
end;

function TSDUDiskPartitionsPanel.GetPartitionInfo(idx: Integer): TSDUPartitionInfo;
var
  lstIdx: Integer;
begin
  // Sanity check...
  Assert(
    ((idx >= low(FBlocks)) or (idx <= high(FBlocks))),
    'GetPartitionInfo called with bad index: ' + IntToStr(idx)
    );

  lstIdx := Integer(FMapBlockToPartition.Objects[idx]);
  Result := FDriveLayoutInfo.PartitionEntry[lstIdx];
end;

// Returns #0 if drive letter can't be found
function TSDUDiskPartitionsPanel.GetDriveLetterForPartition(DiskNo: Integer;
  PartitionNo: Integer): DriveLetterChar;
var
  partitionDevice:     String;
  partitionUnderlying: String;
  driveLetter:         Char;
  idx:                 Integer;
  tmpDeviceName:       String;
begin
  Result := #0;

  if (objMgr = nil) then begin
    objMgr := TSDUObjManager.Create();
  end;

  // Get the underlying device name for each drive letter...
  if (driveDevices = nil) then begin
    driveDevices := TStringList.Create();
    for driveLetter := 'A' to 'Z' do begin
      tmpDeviceName := objMgr.UnderlyingDeviceForDrive(driveLetter);
      if (tmpDeviceName <> '') then begin
        driveDevices.AddObject(tmpDeviceName, TObject(Ord(driveLetter)));
      end;
    end;
  end;

  // Get the partition device name for the disk/partition we're interested in...
  partitionDevice     := SDUDeviceNameForPartition(DiskNo, PartitionNo);
  // Get the underlying device name for the disk/partition we're interested in...
  partitionUnderlying := objMgr.UnderlyingDeviceForName(partitionDevice);

  // Locate the drive letter for the underlying device name for the
  // disk/partition we're interested in...
  idx := driveDevices.IndexOf(partitionUnderlying);
  if (idx >= 0) then begin
    Result := Char(Integer(driveDevices.Objects[idx]));
  end;

end;

// Returns #0 if no partition selected/no drive letter assigned to partition
function TSDUDiskPartitionsPanel.GetDriveLetter(idx: Integer): Char;
var
  partInfo: TSDUPartitionInfo;
begin
  Result := #0;

  if (idx >= 0) then begin
    partInfo := PartitionInfo[idx];
    Result   := GetDriveLetterForPartition(DiskNumber, partInfo.PartitionNumber);
  end;

end;

function TSDUDiskPartitionsPanel.GetDriveLayout(physicalDiskNo: Integer;
  var driveLayout: TSDUDriveLayoutInformation): Boolean;
begin
  Result := SDUGetDriveLayout(DiskNumber, FDriveLayoutInfo);
end;

function TSDUDiskPartitionsPanel.IgnorePartition(partInfo: TSDUPartitionInfo): Boolean;
begin
  Result := (
    // USE CAUTION! We don't want the user selecting a partition, and
    // ending up using the whole HDD (partition number zero)
    (not (ShowPartitionsWithPNZero) and (partInfo.PartitionNumber = 0)) or
    // Piddly partitions; extended partitions, etc
    (partInfo.PartitionLength < ULL_ONEMEG) or
    // $05/$0F = extended partition definition
    (partInfo.PartitionType = $05) or (partInfo.PartitionType = $0F));

end;

end.
