unit OTFEE4MMountDeviceNT_U;
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
  TOTFEE4MMountDeviceNT_F = class(TOTFEE4MMountDevice_F)
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

procedure TOTFEE4MMountDeviceNT_F.pbOKClick(Sender: TObject);
begin
  PartitionDevice := partitionDeviceNames[lbDevices.itemindex];
  ModalResult := mrOK;

end;

procedure TOTFEE4MMountDeviceNT_F.FormShow(Sender: TObject);
begin
 partitionDeviceNames:= TStringList.Create();

 E4MComponent.GetAvailableRawDevices(TStringList(lbDevices.items), partitionDeviceNames);
                                      
end;

procedure TOTFEE4MMountDeviceNT_F.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
 partitionDeviceNames.Free();


end;

END.

