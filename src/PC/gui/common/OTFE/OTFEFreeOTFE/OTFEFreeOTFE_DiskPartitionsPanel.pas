unit OTFEFreeOTFE_DiskPartitionsPanel;

interface

uses
  Classes, CommonfmeOptions_Base,
  CommonSettings,
  Controls, Dialogs, Forms,
  FreeOTFESettings, Graphics, Messages, OTFEFreeOTFEBase_U, SDUDiskPartitionsPanel,
  SDUGeneral,
  SysUtils, Variants, Windows;

type
  TOTFEFreeOTFEDiskPartitionsPanel = class (TSDUDiskPartitionsPanel)
  PRIVATE
    FFreeOTFEObj: TOTFEFreeOTFEBase;
  PROTECTED
    function GetDriveLayout(physicalDiskNo: Integer;
      var driveLayout: TSDUDriveLayoutInformation): Boolean; OVERRIDE;
    function IgnorePartition(partInfo: TSDUPartitionInfo): Boolean; OVERRIDE;
  PUBLIC
    SyntheticDriveLayout: Boolean;

  PUBLISHED
    property FreeOTFEObj: TOTFEFreeOTFEBase Read FFreeOTFEObj Write FFreeOTFEObj;

  end;

//procedure Register;

implementation

{$R *.dfm}


 //procedure Register;
 //begin
 //  RegisterComponents('FreeOTFE', [TOTFEFreeOTFEDiskPartitionsPanel]);
 //end;

function TOTFEFreeOTFEDiskPartitionsPanel.GetDriveLayout(physicalDiskNo: Integer;
  var driveLayout: TSDUDriveLayoutInformation): Boolean;
var
  retval:     Boolean;
  hddDevices: TStringList;
  title:      TStringList;
  i:          Integer;
  partNo:     Integer;
  tmpPart:    TSDUPartitionInfo;
begin
  SyntheticDriveLayout := False;
  retval               := inherited GetDriveLayout(physicalDiskNo, driveLayout);

  if not (retval) then begin
    // Fallback...
    if (FreeOTFEObj <> nil) then begin
      hddDevices := TStringList.Create();
      title      := TStringList.Create();
      try
        if FreeOTFEObj.HDDDeviceList(physicalDiskNo, hddDevices, title) then begin
          driveLayout.PartitionCount := 0;
          driveLayout.Signature      := $00000000;

          for i := 0 to (hddDevices.Count - 1) do begin
            partNo := Integer(hddDevices.Objects[i]);
            if (partNo > 0) then begin
              Inc(driveLayout.PartitionCount);

              tmpPart.StartingOffset      := 0;
              tmpPart.PartitionLength     := 0;
              tmpPart.HiddenSectors       := 0;
              tmpPart.PartitionNumber     := partNo;
              tmpPart.PartitionType       := 0;
              tmpPart.BootIndicator       := False;
              tmpPart.RecognizedPartition := False;
              tmpPart.RewritePartition    := False;

              driveLayout.PartitionEntry[(driveLayout.PartitionCount - 1)] := tmpPart;
            end;
          end;

          SyntheticDriveLayout := True;
          retval               := True;
        end;

      finally
        hddDevices.Free();
        title.Free();
      end;

    end;
  end;

  Result := retval;
end;

function TOTFEFreeOTFEDiskPartitionsPanel.IgnorePartition(partInfo: TSDUPartitionInfo): Boolean;
var
  retval: Boolean;
begin
  if SyntheticDriveLayout then begin
    // Synthetic drive layout; don't ignore any partitions
    retval := False;
  end else begin
    retval := inherited IgnorePartition(partInfo);
  end;

  Result := retval;
end;

end.
