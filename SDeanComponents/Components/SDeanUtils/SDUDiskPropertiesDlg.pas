unit SDUDiskPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, SDUCheckLst,
  SDUGeneral, ExtCtrls, SDUForms;

type
  TSDUDiskPropertiesDialog = class(TSDUForm)
    Label1: TLabel;
    edTracksPerCylinder: TEdit;
    pbClose: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edSectorsPerTrack: TEdit;
    edBytesPerSector: TEdit;
    edMediaType: TEdit;
    edSizeBytes: TEdit;
    Label11: TLabel;
    edCylinders: TEdit;
    edDiskNumber: TEdit;
    Label8: TLabel;
    edDiskSignature: TEdit;
    Label10: TLabel;
    edDiskPartitionCount: TEdit;
    Label12: TLabel;
    Label2: TLabel;
    edSizeUnits: TEdit;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    DiskNumber: integer;
  end;

implementation

{$R *.dfm}

resourcestring
  RS_UNABLE_TO_OBTAIN_DATA = '<Unable to obtain data>';

procedure TSDUDiskPropertiesDialog.FormShow(Sender: TObject);
var
  layout: TSDUDriveLayoutInformation;
  diskGeometry: TSDUDiskGeometry;
  size: ULONGLONG;
begin
  edDiskNumber.text := inttostr(DiskNumber);

  edDiskPartitionCount.text := RS_UNABLE_TO_OBTAIN_DATA;
  edDiskSignature.text      := RS_UNABLE_TO_OBTAIN_DATA;
  if SDUGetDriveLayout(DiskNumber, layout) then
    begin
    edDiskPartitionCount.text := inttostr(layout.PartitionCount);
    edDiskSignature.text      := '0x'+inttohex(layout.Signature, 8);
    end;

  edCylinders.text          := RS_UNABLE_TO_OBTAIN_DATA;
  edTracksPerCylinder.text  := RS_UNABLE_TO_OBTAIN_DATA;
  edSectorsPerTrack.text    := RS_UNABLE_TO_OBTAIN_DATA;
  edBytesPerSector.text     := RS_UNABLE_TO_OBTAIN_DATA;
  edSizeBytes.text          := RS_UNABLE_TO_OBTAIN_DATA;
  edSizeUnits.text          := RS_UNABLE_TO_OBTAIN_DATA;
  edMediaType.text          := RS_UNABLE_TO_OBTAIN_DATA;
  if SDUGetDiskGeometry(DiskNumber, diskGeometry) then
    begin
    edCylinders.text          := inttostr(diskGeometry.Cylinders.QuadPart);
    edTracksPerCylinder.text  := inttostr(diskGeometry.TracksPerCylinder);
    edSectorsPerTrack.text    := inttostr(diskGeometry.SectorsPerTrack);
    edBytesPerSector.text     := inttostr(diskGeometry.BytesPerSector);
    size := diskGeometry.Cylinders.QuadPart *
            diskGeometry.TracksPerCylinder *
            diskGeometry.SectorsPerTrack *
            diskGeometry.BytesPerSector;
    edSizeBytes.text          := SDUIntToStrThousands(size);
    edSizeUnits.text          := SDUFormatAsBytesUnits(size);
    edMediaType.text          := '0x'+inttohex(diskGeometry.MediaType, 2) +
                                 ': ' + TSDUMediaTypeTitle[TSDUMediaType(diskGeometry.MediaType)];
    end;

end;

END.


