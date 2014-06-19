unit SDUPartitionPropertiesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, SDUCheckLst,
  SDUGeneral, ExtCtrls, SDUForms;

type
  TSDUPartitionPropertiesDialog = class(TSDUForm)
    Label1: TLabel;
    edStartingOffset: TEdit;
    pbClose: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    edPartitionLengthBytes: TEdit;
    edHiddenSectors: TEdit;
    edPartitionNumber: TEdit;
    edPartitionType: TEdit;
    cbPartitionFlags: TSDUCheckListBox;
    Label11: TLabel;
    edMountedAs: TEdit;
    Label8: TLabel;
    edPartitionLengthUnits: TEdit;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    PartitionInfo: TSDUPartitionInfo;
    MountedAsDrive: char;
  end;

implementation

{$R *.dfm}

const
  DRIVE_LETTER_UNKNOWN = '?';
  DRIVE_LETTER_NONE    = #0;

resourcestring
  RS_BOOTABLE   = 'Bootable';
  RS_REWRITE    = 'Rewrite';
  RS_RECOGNISED = 'Recognised';

  RS_UNKNOWN         = '<Unknown>';
  RS_NO_DRIVE_LETTER = '<No drive letter>';

procedure TSDUPartitionPropertiesDialog.FormShow(Sender: TObject);
var
  idx: integer;
  volID: string;
begin
  edPartitionLengthBytes.text := SDUIntToStrThousands(PartitionInfo.PartitionLength);
  edPartitionLengthUnits.text := SDUFormatAsBytesUnits(PartitionInfo.PartitionLength);
  edStartingOffset.text       := SDUIntToStrThousands(PartitionInfo.StartingOffset);
  edHiddenSectors.text        := inttostr(PartitionInfo.HiddenSectors);
  edPartitionNumber.text      := inttostr(PartitionInfo.PartitionNumber);
  edPartitionType.text        := '0x'+inttohex(ord(PartitionInfo.PartitionType), 2)+
                                 ': '+SDUPartitionType(PartitionInfo.PartitionType, TRUE);

  edMountedAs.text := RS_UNKNOWN;
  if (MountedAsDrive = DRIVE_LETTER_UNKNOWN) then
    begin
    // Do nothing - already set to "Unknown"
    end
  else if (MountedAsDrive = DRIVE_LETTER_NONE) then
    begin
    edMountedAs.text := RS_NO_DRIVE_LETTER;
    end
  else if (MountedAsDrive <> DRIVE_LETTER_NONE) then
    begin
    volID := trim(SDUVolumeID(MountedAsDrive));
    if (volID <> '') then
      begin
      volID := ' ['+volID+']';
      end;
    
    edMountedAs.text := upcase(MountedAsDrive)+':'+volID;
    end;

  idx := cbPartitionFlags.Items.Add(RS_BOOTABLE);
  cbPartitionFlags.Checked[idx] := PartitionInfo.BootIndicator;
  idx := cbPartitionFlags.Items.Add(RS_REWRITE);
  cbPartitionFlags.Checked[idx] := PartitionInfo.RewritePartition;
  idx := cbPartitionFlags.Items.Add(RS_RECOGNISED);
  cbPartitionFlags.Checked[idx] := PartitionInfo.RecognizedPartition;

end;

END.


