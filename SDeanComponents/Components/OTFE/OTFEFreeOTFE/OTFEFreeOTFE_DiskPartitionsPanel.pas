unit OTFEFreeOTFE_DiskPartitionsPanel;

interface

uses
  SDUGeneral,
  SDUDiskPartitionsPanel,
  OTFEFreeOTFEBase_U;

type
  TOTFEFreeOTFEDiskPartitionsPanel = class(TSDUDiskPartitionsPanel)
  private
    FFreeOTFEObj: TOTFEFreeOTFEBase;
  protected
    function GetDriveLayout(physicalDiskNo: integer; var driveLayout: TSDUDriveLayoutInformation): boolean; override;
    function IgnorePartition(partInfo: TSDUPartitionInfo): boolean; override;
  public
    SyntheticDriveLayout: boolean;
  published
    property FreeOTFEObj: TOTFEFreeOTFEBase read FFreeOTFEObj write FFreeOTFEObj;

  end;

procedure Register;

implementation

uses
  Classes;

procedure Register;
begin
  RegisterComponents('FreeOTFE', [TOTFEFreeOTFEDiskPartitionsPanel]);
end;

function TOTFEFreeOTFEDiskPartitionsPanel.GetDriveLayout(physicalDiskNo: integer; var driveLayout: TSDUDriveLayoutInformation): boolean;
var
  retval: boolean;
  hddDevices: TStringList;
  title: TStringList;
  i: integer;
  partNo: integer;
  tmpPart: TSDUPartitionInfo;
begin
  SyntheticDriveLayout := FALSE;
  retval := inherited GetDriveLayout(physicalDiskNo, driveLayout);

  if not(retval) then
    begin
    // Fallback...
    if (FreeOTFEObj <> nil) then
      begin
      hddDevices:= TStringList.Create();
      title:= TStringList.Create();
      try
        if FreeOTFEObj.HDDDeviceList(physicalDiskNo, hddDevices, title) then
          begin
          driveLayout.PartitionCount := 0;
          driveLayout.Signature := $00000000;

          for i:=0 to (hddDevices.count - 1) do
            begin
            partNo := integer(hddDevices.Objects[i]);
            if (partNo > 0) then
              begin
              inc(driveLayout.PartitionCount);

              tmpPart.StartingOffset      := 0;
              tmpPart.PartitionLength     := 0;
              tmpPart.HiddenSectors       := 0;
              tmpPart.PartitionNumber     := partNo;
              tmpPart.PartitionType       := 0;
              tmpPart.BootIndicator       := FALSE;
              tmpPart.RecognizedPartition := FALSE;
              tmpPart.RewritePartition    := FALSE;

              driveLayout.PartitionEntry[(driveLayout.PartitionCount - 1)] := tmpPart;
              end;
            end;
            
          SyntheticDriveLayout := TRUE;
          retval := TRUE;
          end;
          
      finally
        hddDevices.Free();
        title.Free();
      end;

      end;
    end;

  Result := retval;
end;

function TOTFEFreeOTFEDiskPartitionsPanel.IgnorePartition(partInfo: TSDUPartitionInfo): boolean;
var
  retval: boolean;
begin
  if SyntheticDriveLayout then
    begin
    // Synthetic drive layout; don't ignore any partitions
    retval := FALSE;
    end
  else
    begin
    retval := inherited IgnorePartition(partInfo);
    end;

  Result := retval;
end;

END.

