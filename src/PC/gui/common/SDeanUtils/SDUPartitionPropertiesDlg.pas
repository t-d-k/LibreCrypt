unit SDUPartitionPropertiesDlg;

interface

uses
  //delphi & libs
  CheckLst, Classes, Controls, Dialogs, ExtCtrls, Forms,
  Graphics, Messages,
  StdCtrls, SysUtils, Variants, Windows,
  //sdu & lc utils
  SDUCheckLst, SDUForms, SDUGeneral, lcTypes, PartitionTools
  // lc forms
  //main form
  ;

type
  TSDUPartitionPropertiesDialog = class (TSDUForm)
    Label1:           TLabel;
    edStartingOffset: TEdit;
    pbClose:          TButton;
    Label4:           TLabel;
    Label5:           TLabel;
    Label6:           TLabel;
    Label7:           TLabel;
    Label9:           TLabel;
    edPartitionLengthBytes: TEdit;
    edHiddenSectors:  TEdit;
    edPartitionNumber: TEdit;
    edPartitionType:  TEdit;
    Label11:          TLabel;
    edMountedAs:      TEdit;
    Label8:           TLabel;
    edPartitionLengthUnits: TEdit;
    cbPartitionFlags: TSDUCheckListBox;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    fPartitionInfo:  TPartitionInformationEx;
    fMountedAsDrive: DriveLetterChar;
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
  edPartitionLengthBytes.Text := SDUIntToStrThousands(fPartitionInfo.PartitionLength);
  edPartitionLengthUnits.Text := SDUFormatAsBytesUnits(fPartitionInfo.PartitionLength);
  edStartingOffset.Text       := SDUIntToStrThousands(fPartitionInfo.StartingOffset);
  { TODO -otdk -cenhance : gpt desc }
  edPartitionNumber.Text      := IntToStr(fPartitionInfo.PartitionNumber);
  if fPartitionInfo.PartitionStyle = PARTITION_STYLE_MBR then begin

    edHiddenSectors.Text := IntToStr(fPartitionInfo.Mbr.HiddenSectors);

    edPartitionType.Text := '0x' + inttohex(Ord(fPartitionInfo.mbr.PartitionType), 2) +
      ': ' + SDUMbrPartitionType(fPartitionInfo.mbr.PartitionType, True);
  end else begin
    if fPartitionInfo.PartitionStyle = PARTITION_STYLE_GPT then begin
      edPartitionType.Text :=
        ': ' + SDUGptPartitionType(fPartitionInfo.gpt.PartitionType, True) + '"' +
        fPartitionInfo.Gpt.Name + '"' + 'GUID: ' + GUIDToString(fPartitionInfo.Gpt.PartitionId);
    end else begin
      if fPartitionInfo.PartitionStyle = PARTITION_STYLE_GPT then begin
        edPartitionType.Text := 'RAW';
      end else begin
        edPartitionType.Text := 'UNKNOWN: ' + IntToHex(fPartitionInfo.PartitionStyle, 8);
      end;
    end;

  end;



  edMountedAs.Text := RS_UNKNOWN;
  if (fMountedAsDrive = DRIVE_LETTER_UNKNOWN) then begin
    // Do nothing - already set to "Unknown"
  end else
  if (fMountedAsDrive = DRIVE_LETTER_NONE) then begin
    edMountedAs.Text := RS_NO_DRIVE_LETTER;
  end else
  if (fMountedAsDrive <> DRIVE_LETTER_NONE) then begin
    volID := trim(SDUVolumeID(fMountedAsDrive));
    if (volID <> '') then begin
      volID := ' [' + volID + ']';
    end;

    edMountedAs.Text := upcase(fMountedAsDrive) + ':' + volID;
  end;

  idx := cbPartitionFlags.Items.Add(RS_BOOTABLE);
  cbPartitionFlags.Checked[idx] := fPartitionInfo.Mbr.BootIndicator;
  idx := cbPartitionFlags.Items.Add(RS_REWRITE);
  cbPartitionFlags.Checked[idx] := fPartitionInfo.RewritePartition;
  idx := cbPartitionFlags.Items.Add(RS_RECOGNISED);
  cbPartitionFlags.Checked[idx] := fPartitionInfo.Mbr.RecognizedPartition;

end;

end.
