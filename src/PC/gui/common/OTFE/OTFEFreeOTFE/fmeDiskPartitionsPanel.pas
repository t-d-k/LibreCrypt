unit fmeDiskPartitionsPanel;

interface

uses
  //delphi and 3rd party libs - layer 0
  SysUtils, Variants, Classes, Windows, Graphics, Messages, Controls, Dialogs, Forms, Vcl.StdCtrls,
  //sdu  - layer 1
  SDUGeneral,
  //LibreCrypt utils -also layer 1
  OTFEFreeOTFEBase_U,  MainSettings,
  CommonSettings,
  PartitionTools,
  // LibreCrypt forms/frames - layer 2
    fmeSDUBlocks, fmeSDUDiskPartitions
  //main form - layer 3
  ;

type
     { TODO 1 -crefactor : TfmeSDUDiskPartitions has only one child, this one, so merge }
  TfmeDiskPartitionsPanel = class (TfmeSDUDiskPartitions)
  private

  protected
    fSyntheticDriveLayout: Boolean;
    function GetDriveLayout(physicalDiskNo: Integer;
      var driveLayout: TSDUDriveLayoutInformationEx): Boolean; override;
    function IgnorePartition(partInfo: TPartitionInformationEx): Boolean; override;
  public


  published

    property SyntheticDriveLayout: Boolean Read fSyntheticDriveLayout;


  end;

implementation

{$R *.dfm}


function TfmeDiskPartitionsPanel.GetDriveLayout(physicalDiskNo: Integer;
  var driveLayout: TSDUDriveLayoutInformationEx): Boolean;
var
  hddDevices: TStringList;
  title:      TStringList;
  i:          Integer;
  partNo:     Integer;
  tmpPart:    TPartitionInformationEx;
begin
  fSyntheticDriveLayout := False;
  Result                := inherited GetDriveLayout(physicalDiskNo, driveLayout);

  if not (Result) then begin
    // Fallback...

    hddDevices := TStringList.Create();
    title      := TStringList.Create();
    try
      if GetFreeOTFEBase.HDDDeviceList(physicalDiskNo, hddDevices, title) then begin
        driveLayout.PartitionCount := 0;
        driveLayout.DriveLayoutInformation.Mbr.Signature      := $00000000;

        for i := 0 to (hddDevices.Count - 1) do begin
          partNo := Integer(hddDevices.Objects[i]);
          if (partNo > 0) then begin
            Inc(driveLayout.PartitionCount);
            tmpPart.PartitionStyle    := PARTITION_STYLE_MBR;
            tmpPart.StartingOffset      := 0;
            tmpPart.PartitionLength     := 0;
             tmpPart.PartitionNumber     := partNo;
             tmpPart.RewritePartition    := False;
            tmpPart.Mbr.HiddenSectors       := 0;
            tmpPart.mbr. PartitionType       := 0;
            tmpPart.mbr.BootIndicator       := False;
            tmpPart.mbr.RecognizedPartition := False;

            driveLayout.PartitionEntry[(driveLayout.PartitionCount - 1)] := tmpPart;
          end;
        end;

        fSyntheticDriveLayout := True;
        Result                := True;
      end;

    finally
      hddDevices.Free();
      title.Free();
    end;

  end;
end;

function TfmeDiskPartitionsPanel.IgnorePartition(partInfo: TPartitionInformationEx): Boolean;
begin
  if fSyntheticDriveLayout then begin
    // Synthetic drive layout; don't ignore any partitions
    Result := False;
  end else begin
    Result := inherited IgnorePartition(partInfo);
  end;

end;

end.
