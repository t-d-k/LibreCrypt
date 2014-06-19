unit OTFETrueCryptMountDeviceNT_U;
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
  TOTFETrueCryptMountDeviceNT_F = class(TOTFETrueCryptMountDevice_F)
    pbOK: TButton;
    pbCancel: TButton;
    lbDevices: TListBox;
    Label1: TLabel;
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
  public
  end;

implementation

{$R *.DFM}

uses SDUGeneral;

procedure TOTFETrueCryptMountDeviceNT_F.pbOKClick(Sender: TObject);
begin
  PartitionDevice := partitionDeviceNames[lbDevices.itemindex];
  ModalResult := mrOK;

end;

procedure TOTFETrueCryptMountDeviceNT_F.FormShow(Sender: TObject);
begin
 partitionDeviceNames:= TStringList.Create();

 TrueCryptComponent.GetAvailableRawDevices(TStringList(lbDevices.items), partitionDeviceNames);
                                      
end;

procedure TOTFETrueCryptMountDeviceNT_F.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 partitionDeviceNames.Free();

end;

END.

