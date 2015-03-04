unit OTFEFreeOTFE_DiskPartitionsPanel;

interface

uses
  //delphi and 3rd party libs - layer 0
  SysUtils, Variants, Classes, Windows , Graphics, Messages,   Controls, Dialogs, Forms,
  //sdu  - layer 1
    SDUGeneral,
  //doxbox utils -also layer 1
     OTFEFreeOTFEBase_U, SDUDiskPartitionsPanel,  FreeOTFESettings,    CommonfmeOptions_Base,
  CommonSettings
   // doxbox forms - layer 2
    //main form - layer 3
  ;

type
  TOTFEFreeOTFEDiskPartitionsPanel = class (TSDUDiskPartitionsPanel)
  PRIVATE

  PROTECTED
  fSyntheticDriveLayout: Boolean;
    function GetDriveLayout(physicalDiskNo: Integer;
      var driveLayout: TSDUDriveLayoutInformation): Boolean; OVERRIDE;
    function IgnorePartition(partInfo: TSDUPartitionInfo): Boolean; OVERRIDE;
  PUBLIC


  PUBLISHED

    property SyntheticDriveLayout: Boolean Read fSyntheticDriveLayout;


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
  hddDevices: TStringList;
  title:      TStringList;
  i:          Integer;
  partNo:     Integer;
  tmpPart:    TSDUPartitionInfo;
begin
  fSyntheticDriveLayout := False;
  Result               := inherited GetDriveLayout(physicalDiskNo, driveLayout);

  if not (Result) then begin
    // Fallback...

      hddDevices := TStringList.Create();
      title      := TStringList.Create();
      try
        if GetFreeOTFEBase.HDDDeviceList(physicalDiskNo, hddDevices, title) then begin
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

          fSyntheticDriveLayout := True;
          Result               := True;
        end;

      finally
        hddDevices.Free();
        title.Free();
      end;

    end;



end;

function TOTFEFreeOTFEDiskPartitionsPanel.IgnorePartition(partInfo: TSDUPartitionInfo): Boolean;
begin
  if fSyntheticDriveLayout then begin
    // Synthetic drive layout; don't ignore any partitions
    Result := False;
  end else begin
    Result := inherited IgnorePartition(partInfo);
  end;


end;

end.
