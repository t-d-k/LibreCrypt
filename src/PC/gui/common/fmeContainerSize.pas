unit fmeContainerSize;

{

a frame to set size of a new container (partition or file)
(c) tdk
licence: gpl
}

interface

uses
  // delphi
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SDUFrames, SDUSpin64Units,
  Vcl.StdCtrls
  // sdu, lc utils common
  //LibreCrypt specific
  ;

type
  TTfmeContainerSize = class (TFrame)
    ckSizeEntirePartitionDisk: TCheckBox;
    lblPartitionDiskSize: TLabel;
    Label6:       TLabel;
    se64UnitSize: TSDUSpin64Unit_Storage;
    lblInstructSizeCommon: TLabel;
    lblInstructSizeHidden: TLabel;
    lblInstructSizeNotHidden: TLabel;
    procedure se64UnitSizeChange(Sender: TObject);
    procedure ckSizeEntirePartitionDiskClick(Sender: TObject);
  private
    FOnChange:  TNotifyEvent;
    fis_hidden: Boolean;
  published
    property OnChange: TNotifyEvent Read FOnChange Write FOnChange;
    property IsHidden: Boolean read fis_hidden Write fis_hidden;

  public
    procedure EnableDisableControls(IsPartition, IsHiddenInPartition, SyntheticDriveLayout: Boolean);
    function GetIsSizeEntirePartitionDisk: Boolean;
        procedure SetIsSizeEntirePartitionDisk(val: Boolean);
    function GetSize(): ULONGLONG;
    procedure SetPartitionSize(new_size: ULONGLONG);
    procedure Initialise();

  end;

implementation

{$R *.dfm}

uses
  // delphi
  // sdu, lc utils common
  SDUGeneral, SDUi18n
  //LibreCrypt specific

  ;

{ TTfmeContainerSize }

procedure TTfmeContainerSize.ckSizeEntirePartitionDiskClick(Sender: TObject);
begin
  SDUEnableControl(se64UnitSize, not (ckSizeEntirePartitionDisk.Checked));
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TTfmeContainerSize.EnableDisableControls(
  IsPartition, IsHiddenInPartition, SyntheticDriveLayout: Boolean);
begin
  // If the user is creating a volume on a partition/disk, give the user the
  // option of using the whole partition/disk's space
  // ...But if the user's creating a hidden partition - make sure they enter
  // the size

  // Weird, without this FALSE/TRUE, the ".Visible := " following it has no
  // effect; even though the ".Visible := " following *that* does?!!
  ckSizeEntirePartitionDisk.Visible := False;
  ckSizeEntirePartitionDisk.Visible := True;


  ckSizeEntirePartitionDisk.Visible :=
    (IsPartition and not (IsHiddenInPartition) and not (SyntheticDriveLayout));
  lblPartitionDiskSize.Visible      := ckSizeEntirePartitionDisk.Visible;
  // If the control's not visible, don't let it be selected; the user enters
  // the size
  if not (ckSizeEntirePartitionDisk.Visible) then
    ckSizeEntirePartitionDisk.Checked := False;

  SDUEnableControl(se64UnitSize, not (ckSizeEntirePartitionDisk.Checked));

end;

function TTfmeContainerSize.GetIsSizeEntirePartitionDisk: Boolean;
begin
  Result := ckSizeEntirePartitionDisk.Checked;
end;

procedure TTfmeContainerSize.SetIsSizeEntirePartitionDisk(val: Boolean);
begin
    ckSizeEntirePartitionDisk.Checked  := val;
end;

function TTfmeContainerSize.GetSize: ULONGLONG;
begin
  // Calculate the number of bytes...
  // Note: The in64(...) cast is REQUIRED, otherwise Delphi will calculate the
  //       value in 32 bits, and assign it to the 64 bit VolumeSize
  Result := ULONGLONG(se64UnitSize.Value);
end;

procedure TTfmeContainerSize.Initialise;
begin
  SDUTranslateComp(lblInstructSizeCommon);
  SDUTranslateComp(lblInstructSizeHidden);
  SDUTranslateComp(lblInstructSizeNotHidden);
  lblInstructSizeHidden.Visible    := fis_hidden;
  lblInstructSizeNotHidden.Visible := not fis_hidden;

end;

procedure TTfmeContainerSize.se64UnitSizeChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;



procedure TTfmeContainerSize.SetPartitionSize(new_size: ULONGLONG);
begin
  lblPartitionDiskSize.Caption :=
    Format(_('(Approx: %s)'), [SDUFormatAsBytesUnits(new_size)]);
end;



end.
