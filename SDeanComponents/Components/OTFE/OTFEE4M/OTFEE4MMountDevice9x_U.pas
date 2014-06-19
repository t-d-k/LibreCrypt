unit OTFEE4MMountDevice9x_U;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  OTFEE4M_U,
  OTFEE4MMountDevice_U;

type
  TOTFEE4MMountDevice9x_F = class(TOTFEE4MMountDevice_F)
    pbOK: TButton;
    pbCancel: TButton;
    GroupBox1: TGroupBox;
    cbDrive: TComboBox;
    cbPartition: TComboBox;
    rbFloppyA: TRadioButton;
    rbFloppyB: TRadioButton;
    rbPartition: TRadioButton;
    lblDrive: TLabel;
    lblPartition: TLabel;
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbDriveChange(Sender: TObject);
    procedure rbClick(Sender: TObject);
  private
    partitionDeviceDispNames: TStringList;
    procedure FillInDrives();
  public
  end;

implementation

{$R *.DFM}

uses
  SDUGeneral,
  OTFEE4MStructures_U;

procedure TOTFEE4MMountDevice9x_F.pbOKClick(Sender: TObject);
begin
  if rbFloppyA.Checked then
    begin
    PartitionDevice := '\Device\Floppy0';
    end
  else if rbFloppyB.Checked then
    begin
    PartitionDevice := '\Device\Floppy1';
    end
  else
    begin
    PartitionDevice := Format(E4M_HDD_PARTITION_DEVICE_NAME_FORMAT, [strtoint(cbDrive.text), strtoint(cbPartition.text)]);
    end;

  ModalResult := mrOK;

end;

procedure TOTFEE4MMountDevice9x_F.FormShow(Sender: TObject);
begin
  partitionDeviceDispNames:= TStringList.Create();
  partitionDeviceNames:= TStringList.Create();

  E4MComponent.GetAvailableRawDevices(partitionDeviceDispNames, partitionDeviceNames);

  rbFloppyA.enabled := (partitionDeviceNames.IndexOf('\Device\Floppy0')>-1);
  rbFloppyB.enabled := (partitionDeviceNames.IndexOf('\Device\Floppy1')>-1);

  FillInDrives();
  cbDriveChange(nil);

end;

procedure TOTFEE4MMountDevice9x_F.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 partitionDeviceDispNames.Free();
 partitionDeviceNames.Free();

end;

procedure TOTFEE4MMountDevice9x_F.cbDriveChange(Sender: TObject);
var
  i: integer;
  currTestDrive: string;
begin
  cbPartition.items.clear();
  for i:=1 to E4M_HDD_MAX_PARTITIONS do
    begin
    currTestDrive := Format(E4M_HDD_PARTITION_DEVICE_NAME_FORMAT, [strtoint(cbDrive.text), i]);
    if partitionDeviceNames.IndexOf(currTestDrive)>-1 then
      begin
      cbPartition.items.add(inttostr(i));
      end;
    end;

  cbPartition.itemindex := 0;

end;

procedure TOTFEE4MMountDevice9x_F.FillInDrives();
var
  i: integer;
  j: integer;
  currTestDrive: string;
begin
  cbDrive.items.clear();
  for i:=0 to MAX_HDD_MAX_PHYSICAL_DRIVES do
    begin
    for j:=1 to E4M_HDD_MAX_PARTITIONS do
      begin
      currTestDrive := Format(E4M_HDD_PARTITION_DEVICE_NAME_FORMAT, [i, j]);
      if partitionDeviceNames.IndexOf(currTestDrive)>-1 then
        begin
        if cbDrive.items.indexof(inttostr(i))<0 then
          begin
          cbDrive.items.add(inttostr(i));
          end;
        end;
      end;
    end;

  cbDrive.itemindex := 0;

end;

procedure TOTFEE4MMountDevice9x_F.rbClick(Sender: TObject);
begin
  SDUEnableControl(lblDrive,     rbPartition.checked);
  SDUEnableControl(lblPartition, rbPartition.checked);
  SDUEnableControl(cbDrive,      rbPartition.checked);
  SDUEnableControl(cbPartition,  rbPartition.checked);

end;

END.

