unit OTFEBestCryptGetDriveLetter_U;
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask;

type
  TGetDriveLetter_F = class(TForm)
    Label1: TLabel;
    cbDriveLetter: TComboBox;
    Label2: TLabel;
    lblVolFilename: TLabel;
    pbOK: TButton;
    pbCancel: TButton;
    cbAlwaysThisDrive: TCheckBox;
    cbAutomount: TCheckBox;
    Label3: TLabel;
    MaskEdit1: TMaskEdit;
    Label4: TLabel;
    procedure FormShow(Sender: TObject);
    procedure cbAlwaysThisDriveClick(Sender: TObject);
  private
    FFilename: string;
    FDefaultDrive: char;
    FAutoMountOptionAlwaysEnabled: boolean;
    FForcedDriveLetter: char;
    FProhibitedDriveLetters: string;
    procedure RefreshDriveLetters();
  public
    constructor Create(AOwner : TComponent); override;
  published
    property Filename: string read FFilename write FFilename;
    property DefaultDrive: char read FDefaultDrive write FDefaultDrive;
    property AutoMountOptionAlwaysEnabled: boolean read FAutoMountOptionAlwaysEnabled write FAutoMountOptionAlwaysEnabled;
    // Set ForceDriveLetter to #0 to permit user to select any unused drive
    // letter. Set to a drive letter to only permit that drive
    property ForceDriveLetter: char read FForcedDriveLetter write FForcedDriveLetter;
    // Set ProhibitedDriveLetters to be a string containing all drive letters
    // that the user may *not* mount a drive as
    property ProhibitedDriveLetters: string read FProhibitedDriveLetters write FProhibitedDriveLetters;

  end;

implementation

{$R *.DFM}

uses INIFiles,
     OTFEBestCryptStructures_U,
     SDUGeneral;


constructor TGetDriveLetter_F.Create(AOwner : TComponent);
begin
  inherited;
  ForceDriveLetter := #0;
  
end;

procedure TGetDriveLetter_F.FormShow(Sender: TObject);
begin
  self.left := (screen.width-self.width) div 2;
  self.top := (screen.height-self.height) div 2;

  RefreshDriveLetters();

  if DefaultDrive=#0 then
    begin
    cbDriveLetter.itemindex := 0;
    end
  else
    begin
    if cbDriveLetter.items.indexof(FDefaultDrive)=-1 then
      begin
      cbDriveLetter.itemindex := 0;
      end
    else
      begin
      cbDriveLetter.itemindex := cbDriveLetter.items.indexof(FDefaultDrive);
      end;
    end;
    
  lblVolFilename.caption := FFilename;

  cbAutomount.enabled := AutoMountOptionAlwaysEnabled;

end;

procedure TGetDriveLetter_F.RefreshDriveLetters();
var
  DriveNum: Integer;
  DriveChar: char;
  DriveBits: set of 0..25;
begin
  cbDriveLetter.items.Clear;

  if (ForceDriveLetter<>#0) then
    begin
    cbDriveLetter.Items.Add(ForceDriveLetter+':');
    SDUEnableControl(cbDriveLetter, FALSE);
    end
  else
    begin
    SDUEnableControl(cbDriveLetter, TRUE);
    Integer(DriveBits) := GetLogicalDrives;
    for DriveNum := 2 to 25 do // from C: to Z:
      begin
      if not(DriveNum in DriveBits) then
        begin
        DriveChar := Char(DriveNum + Ord('A'));
        if (Pos(DriveChar, FProhibitedDriveLetters)<=0) then
          begin
          cbDriveLetter.Items.Add(DriveChar+':');
          end;
        end;

      end;
      
    end;

end;

procedure TGetDriveLetter_F.cbAlwaysThisDriveClick(Sender: TObject);
begin
  if not(AutoMountOptionAlwaysEnabled) then
    begin
    cbAutomount.enabled := cbAlwaysThisDrive.checked;
    if not(cbAutomount.enabled) then
      begin
      cbAutomount.checked := FALSE;
      end;
    end;

end;

END.

