unit frmUserSelectPartitionSimple;
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
  StdCtrls,
  OTFEFreeOTFE_U, SDUForms;

type
  TfrmUserSelectPartitionSimple = class(TSDUForm)
    Label1: TLabel;
    pbOK: TButton;
    pbCancel: TButton;
    lbPartition: TListBox;
    ckListRemovable: TCheckBox;
    procedure lbPartitionClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure lbPartitionDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ckListRemovableClick(Sender: TObject);
  private
    fPartition: string;

    deviceList: TStringList;
    deviceTitle: TStringList;

    procedure PopulatePartitions();
    procedure PopulatePartitionsAndRemovable();

    procedure EnableDisableControls();

  public
    OTFEFreeOTFE: TOTFEFreeOTFE;

  published
    property Partition: string read fPartition;

  end;


implementation

{$R *.DFM}


procedure TfrmUserSelectPartitionSimple.lbPartitionClick(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmUserSelectPartitionSimple.FormShow(Sender: TObject);
begin
  lbPartition.MultiSelect := FALSE;
  lbPartition.ItemIndex := -1;

  PopulatePartitions();
  
  EnableDisableControls();

end;

procedure TfrmUserSelectPartitionSimple.PopulatePartitions();
begin
  lbPartition.Items.Clear();
  deviceList.Clear();
  deviceTitle.Clear();
  if (OTFEFreeOTFE.HDDDeviceList(deviceList, deviceTitle)) then
    begin
    lbPartition.Items.Assign(deviceTitle);
    end;

end;

procedure TfrmUserSelectPartitionSimple.PopulatePartitionsAndRemovable();
var
  dl, dt: TStringList;
begin
  lbPartition.Items.Clear();

  deviceList.Clear();
  deviceTitle.Clear();

  dl := TStringList.Create();
  try
    dt := TStringList.Create();
    try

      dl.Clear();
      dt.Clear();
      if (OTFEFreeOTFE.HDDDeviceList(dl, dt)) then
        begin
        deviceList.AddStrings(dl);
        deviceTitle.AddStrings(dt);
        lbPartition.Items.AddStrings(dt);
        end;

      dl.Clear();
      dt.Clear();
      if (OTFEFreeOTFE.CDROMDeviceList(dl, dt)) then
        begin
        deviceList.AddStrings(dl);
        deviceTitle.AddStrings(dt);
        lbPartition.Items.AddStrings(dt);
        end;

    finally
      dt.Free();
    end;
  finally
    dl.Free();
  end;

end;

procedure TfrmUserSelectPartitionSimple.pbOKClick(Sender: TObject);
begin
  fPartition := deviceList[lbPartition.ItemIndex];
  ModalResult := mrOK;

end;


procedure TfrmUserSelectPartitionSimple.lbPartitionDblClick(Sender: TObject);
begin
  EnableDisableControls();

  // Emulate clicking "OK" button, *if* it's enabled
  if (pbOK.Enabled) then
    begin
    pbOKClick(Sender);
    end;

end;

procedure TfrmUserSelectPartitionSimple.EnableDisableControls();
begin
  pbOK.Enabled := (lbPartition.ItemIndex >= 0);
  
end;


procedure TfrmUserSelectPartitionSimple.FormCreate(Sender: TObject);
begin
  deviceList:= TStringList.Create();
  deviceTitle:= TStringList.Create();

end;

procedure TfrmUserSelectPartitionSimple.FormDestroy(Sender: TObject);
begin
  deviceList.Free();
  deviceTitle.Free();

end;

procedure TfrmUserSelectPartitionSimple.ckListRemovableClick(Sender: TObject);
begin
  if (ckListRemovable.checked) then
    begin
    PopulatePartitionsAndRemovable();
    end
  else
    begin
    PopulatePartitions();
    end;

  EnableDisableControls();

end;

END.


