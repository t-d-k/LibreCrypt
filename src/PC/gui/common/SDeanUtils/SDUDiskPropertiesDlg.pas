unit SDUDiskPropertiesDlg;

interface

uses
 //delphi & libs
  CheckLst, Classes, Controls, Dialogs, ExtCtrls, Forms,
  Graphics, Messages, StdCtrls, SysUtils, Variants, Windows,
  //sdu & lc utils


   SDUCheckLst,
  SDUForms, SDUGeneral
   // lc forms
    //main form

  ;

type
  TfrmDiskProperties = class (TSDUForm)
    Label1:               TLabel;
    edTracksPerCylinder:  TEdit;
    pbClose:              TButton;
    Label4:               TLabel;
    Label5:               TLabel;
    Label6:               TLabel;
    Label7:               TLabel;
    edSectorsPerTrack:    TEdit;
    edBytesPerSector:     TEdit;
    edMediaType:          TEdit;
    edSizeBytes:          TEdit;
    Label11:              TLabel;
    edCylinders:          TEdit;
    edDiskNumber:         TEdit;
    Label8:               TLabel;
    edDiskSignature:      TEdit;
    Label10:              TLabel;
    edDiskPartitionCount: TEdit;
    Label12:              TLabel;
    Label2:               TLabel;
    edSizeUnits:          TEdit;
    procedure FormShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    fDiskNumber: Integer;
  end;

implementation

{$R *.dfm}
uses
 //delphi & libs
  //sdu & lc utils
  PartitionTools,  //for TSDUDriveLayoutInformationEx
fmeSDUDiskPartitions
   // lc forms
    //main form
;

resourcestring
  RS_UNABLE_TO_OBTAIN_DATA = '<Unable to obtain data>';

procedure TfrmDiskProperties.FormShow(Sender: TObject);
var
  layout:       TSDUDriveLayoutInformationEx;
  diskGeometry: TSDUDiskGeometry;
  size:         ULONGLONG;
begin
  edDiskNumber.Text := IntToStr(fDiskNumber);

  edDiskPartitionCount.Text := RS_UNABLE_TO_OBTAIN_DATA;
  edDiskSignature.Text      := RS_UNABLE_TO_OBTAIN_DATA;
  if SDUGetDriveLayout(fDiskNumber, layout) then begin
    edDiskPartitionCount.Text := IntToStr(layout.PartitionCount);
    { done -otdk -cenhance : gpt desc }
    if  layout.PartitionStyle = PARTITION_STYLE_MBR then
      edDiskSignature.Text      := '0x' + inttohex(layout.DriveLayoutInformation.Mbr.Signature, 8)
    else  if  layout.PartitionStyle = PARTITION_STYLE_GPT then
      edDiskSignature.Text      :=GUIDToString(layout.DriveLayoutInformation.gpt.DiskId);
  end;

  edCylinders.Text         := RS_UNABLE_TO_OBTAIN_DATA;
  edTracksPerCylinder.Text := RS_UNABLE_TO_OBTAIN_DATA;
  edSectorsPerTrack.Text   := RS_UNABLE_TO_OBTAIN_DATA;
  edBytesPerSector.Text    := RS_UNABLE_TO_OBTAIN_DATA;
  edSizeBytes.Text         := RS_UNABLE_TO_OBTAIN_DATA;
  edSizeUnits.Text         := RS_UNABLE_TO_OBTAIN_DATA;
  edMediaType.Text         := RS_UNABLE_TO_OBTAIN_DATA;
  if SDUGetDiskGeometry(fDiskNumber, diskGeometry) then begin
    edCylinders.Text         := IntToStr(diskGeometry.Cylinders.QuadPart);
    edTracksPerCylinder.Text := IntToStr(diskGeometry.TracksPerCylinder);
    edSectorsPerTrack.Text   := IntToStr(diskGeometry.SectorsPerTrack);
    edBytesPerSector.Text    := IntToStr(diskGeometry.BytesPerSector);
    size                     := diskGeometry.Cylinders.QuadPart *
      diskGeometry.TracksPerCylinder * diskGeometry.SectorsPerTrack * diskGeometry.BytesPerSector;
    edSizeBytes.Text         := SDUIntToStrThousands(size);
    edSizeUnits.Text         := SDUFormatAsBytesUnits(size);
    edMediaType.Text         := '0x' + inttohex(diskGeometry.MediaType, 2) +
      ': ' + TSDUMediaTypeTitle[TSDUMediaType(diskGeometry.MediaType)];
  end;

end;

end.
