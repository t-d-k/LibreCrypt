unit OTFETrueCryptMountDevice9x_U;
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
  OTFETrueCrypt_U,
  OTFETrueCryptMountDevice_U;

type
  TOTFETrueCryptMountDevice9x_F = class(TOTFETrueCryptMountDevice_F)
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
  OTFETrueCryptStructures_U;

procedure TOTFETrueCryptMountDevice9x_F.pbOKClick(Sender: TObject);
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
    PartitionDevice := Format(TRUECRYPT_HDD_PARTITION_DEVICE_NAME_FORMAT, [strtoint(cbDrive.text), strtoint(cbPartition.text)]);
    end;

  ModalResult := mrOK;

end;

procedure TOTFETrueCryptMountDevice9x_F.FormShow(Sender: TObject);
begin
  partitionDeviceDispNames:= TStringList.Create();
  partitionDeviceNames:= TStringList.Create();

  TrueCryptComponent.GetAvailableRawDevices(partitionDeviceDispNames, partitionDeviceNames);

  rbFloppyA.enabled := (partitionDeviceNames.IndexOf('\Device\Floppy0')>-1);
  rbFloppyB.enabled := (partitionDeviceNames.IndexOf('\Device\Floppy1')>-1);

  FillInDrives();
  cbDriveChange(nil);

end;

procedure TOTFETrueCryptMountDevice9x_F.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 partitionDeviceDispNames.Free();
 partitionDeviceNames.Free();

end;

procedure TOTFETrueCryptMountDevice9x_F.cbDriveChange(Sender: TObject);
var
  i: integer;
  currTestDrive: string;
begin
  cbPartition.items.clear();
  for i:=1 to TRUECRYPT_HDD_MAX_PARTITIONS do
    begin
    currTestDrive := Format(TRUECRYPT_HDD_PARTITION_DEVICE_NAME_FORMAT, [strtoint(cbDrive.text), i]);
    if partitionDeviceNames.IndexOf(currTestDrive)>-1 then
      begin
      cbPartition.items.add(inttostr(i));
      end;
    end;

  cbPartition.itemindex := 0;

end;

procedure TOTFETrueCryptMountDevice9x_F.FillInDrives();
var
  i: integer;
  j: integer;
  currTestDrive: string;
begin
  cbDrive.items.clear();
  for i:=0 to TrueCrypt_HDD_MAX_PHYSICAL_DRIVES do
    begin
    for j:=1 to TrueCrypt_HDD_MAX_PARTITIONS do
      begin
      currTestDrive := Format(TrueCrypt_HDD_PARTITION_DEVICE_NAME_FORMAT, [i, j]);
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

procedure TOTFETrueCryptMountDevice9x_F.rbClick(Sender: TObject);
begin
  SDUEnableControl(lblDrive,     rbPartition.checked);
  SDUEnableControl(lblPartition, rbPartition.checked);
  SDUEnableControl(cbDrive,      rbPartition.checked);
  SDUEnableControl(cbPartition,  rbPartition.checked);

end;

END.

