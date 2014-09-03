unit OTFEScramDiskDismountVolumes_U;
// Description: Dismount ScramDisk Volumes
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  OTFEScramDisk_U;

type
  TOTFEScramDiskDismountVolumes_F = class(TForm)
    lbDrivesMounted: TListBox;
    pbDismount: TButton;
    pbCancel: TButton;
    Label1: TLabel;
    ckDismountBrutal: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbDismountClick(Sender: TObject);
  private
    { Private declarations }
  public
    ScramDiskComponent: TOTFEScramDisk;
    SelectedDrives: TStringList;
    DismountBrutal: boolean;
    procedure Free();
  end;


implementation

{$R *.DFM}

uses SdStructures_U, SDUGeneral;

procedure TOTFEScramDiskDismountVolumes_F.FormCreate(Sender: TObject);
begin
  SelectedDrives:= TStringList.Create();

end;

procedure TOTFEScramDiskDismountVolumes_F.Free();
begin
  inherited;
  SelectedDrives.Free();
end;


procedure TOTFEScramDiskDismountVolumes_F.FormShow(Sender: TObject);
var
  drivesMounted: Ansistring;
  i : integer;
  slotTitle : string;
  volInfoOK: boolean;
  driveRoot: string;
  volumeLabel: array [0..MAX_PATH] of Widechar;
  maxFilenameLen: DWORD;
  sysFlags: DWORD;
begin
  lbDrivesMounted.clear;
  ScramDiskComponent.UpdateSlotInfo();

  drivesMounted := ScramDiskComponent.DrivesMounted();

  for i:=1 to (length(drivesMounted)) do
    begin
    if (drivesMounted[i]>='C') then
      begin
      driveRoot := drivesMounted[i] + ':\';
      volInfoOK:= GetVolumeInformation(
                      PChar(driveRoot),    // address of root directory of the file system
                      @volumeLabel,        // address of name of the volume
                      sizeof(volumeLabel), // length of lpVolumeNameBuffer
                      nil,                 // address of volume serial number
                      maxFilenameLen,      // address of system's maximum filename length
                      sysFlags,            // address of file system flags
                      nil,                 // address of name of file system
                      0                    // length of lpFileSystemNameBuffer
                      );

      if not(volInfoOK) then
        begin
        volumeLabel := '<unknown>';
        end;

      end
    else
      begin
      volumeLabel := '<floppy>';
      end;

    slotTitle := drivesMounted[i] + ': ' + volumeLabel;
    lbDrivesMounted.items.add(slotTitle);

    end;

  lbDrivesMounted.itemindex := 0;

end;


procedure TOTFEScramDiskDismountVolumes_F.pbDismountClick(Sender: TObject);
var
  i: integer;
begin
  SelectedDrives.Clear();
  for i:=0 to (lbDrivesMounted.items.count-1) do
    begin
    if lbDrivesMounted.selected[i] then
      begin
      SelectedDrives.add((lbDrivesMounted.items[i])[1]);
      end;
    end;

  DismountBrutal := ckDismountBrutal.checked;

  ModalResult := mrOK;
end;

END.

