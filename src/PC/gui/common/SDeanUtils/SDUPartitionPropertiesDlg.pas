unit SDUPartitionPropertiesDlg;

interface

uses
  CheckLst, Classes, Controls, Dialogs, ExtCtrls, Forms,
  Graphics, Messages, SDUCheckLst,
  SDUForms, SDUGeneral, StdCtrls, SysUtils, Variants, Windows;

type
  TSDUPartitionPropertiesDialog = class (TSDUForm)
    Label1:                 TLabel;
    edStartingOffset:       TEdit;
    pbClose:                TButton;
    Label4:                 TLabel;
    Label5:                 TLabel;
    Label6:                 TLabel;
    Label7:                 TLabel;
    Label9:                 TLabel;
    edPartitionLengthBytes: TEdit;
    edHiddenSectors:        TEdit;
    edPartitionNumber:      TEdit;
    edPartitionType:        TEdit;
    cbPartitionFlags:       TSDUCheckListBox;
    Label11:                TLabel;
    edMountedAs:            TEdit;
    Label8:                 TLabel;
    edPartitionLengthUnits: TEdit;
    procedure FormShow(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    PartitionInfo:  TSDUPartitionInfo;
    MountedAsDrive: DriveLetterChar;
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
  idx:   Integer;
  volID: String;
begin
  edPartitionLengthBytes.Text := SDUIntToStrThousands(PartitionInfo.PartitionLength);
  edPartitionLengthUnits.Text := SDUFormatAsBytesUnits(PartitionInfo.PartitionLength);
  edStartingOffset.Text       := SDUIntToStrThousands(PartitionInfo.StartingOffset);
  edHiddenSectors.Text        := IntToStr(PartitionInfo.HiddenSectors);
  edPartitionNumber.Text      := IntToStr(PartitionInfo.PartitionNumber);
  edPartitionType.Text        := '0x' + inttohex(Ord(PartitionInfo.PartitionType), 2) +
    ': ' + SDUPartitionType(PartitionInfo.PartitionType, True);

  edMountedAs.Text := RS_UNKNOWN;
  if (MountedAsDrive = DRIVE_LETTER_UNKNOWN) then begin
    // Do nothing - already set to "Unknown"
  end else
  if (MountedAsDrive = DRIVE_LETTER_NONE) then begin
    edMountedAs.Text := RS_NO_DRIVE_LETTER;
  end else
  if (MountedAsDrive <> DRIVE_LETTER_NONE) then begin
    volID := trim(SDUVolumeID(MountedAsDrive));
    if (volID <> '') then begin
      volID := ' [' + volID + ']';
    end;

    edMountedAs.Text := upcase(MountedAsDrive) + ':' + volID;
  end;

  idx                           := cbPartitionFlags.Items.Add(RS_BOOTABLE);
  cbPartitionFlags.Checked[idx] := PartitionInfo.BootIndicator;
  idx                           := cbPartitionFlags.Items.Add(RS_REWRITE);
  cbPartitionFlags.Checked[idx] := PartitionInfo.RewritePartition;
  idx                           := cbPartitionFlags.Items.Add(RS_RECOGNISED);
  cbPartitionFlags.Checked[idx] := PartitionInfo.RecognizedPartition;

end;

end.
