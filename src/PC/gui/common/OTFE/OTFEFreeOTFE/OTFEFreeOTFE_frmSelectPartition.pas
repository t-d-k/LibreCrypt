unit OTFEFreeOTFE_frmSelectPartition;
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
  OTFEFreeOTFEBase_U, OTFEFreeOTFE_fmeSelectPartition, ExtCtrls, SDUForms;

type
  TfrmSelectPartition = class(TSDUForm)
    Label1: TLabel;
    pnlButtonCenter: TPanel;
    pbOK: TButton;
    pbCancel: TButton;
    fmeSelectPartition: TfmeSelectPartition;
    procedure FormShow(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure fmeSelectPartitionSDUDiskPartitionsPanel1DblClick(
      Sender: TObject);
    procedure fmeSelectPartitionpnlNoPartitionDisplayDblClick(
      Sender: TObject);
  private
    procedure EnableDisableControls();

    function GetPartition(): string;

    procedure fmeSelectPartitionChanged(Sender: TObject);
  public
//    OTFEFreeOTFE: TOTFEFreeOTFEBase;

  published
    property Partition: string read GetPartition;

  end;


implementation

{$R *.DFM}

uses
  SDUGeneral,
  SDUDialogs;
	

procedure TfrmSelectPartition.FormCreate(Sender: TObject);
begin
  SDUClearPanel(pnlButtonCenter);

end;

procedure TfrmSelectPartition.FormResize(Sender: TObject);
begin
  SDUCenterControl(pnlButtonCenter, ccHorizontal);

end;

procedure TfrmSelectPartition.FormShow(Sender: TObject);
begin
//  fmeSelectPartition.FreeOTFEObj := OTFEFreeOTFE;
  fmeSelectPartition.AllowCDROM := TRUE;
  fmeSelectPartition.OnChange := fmeSelectPartitionChanged;
  fmeSelectPartition.Initialize();

  EnableDisableControls();

end;

procedure TfrmSelectPartition.pbOKClick(Sender: TObject);
begin
  ModalResult := mrOK;

end;


procedure TfrmSelectPartition.EnableDisableControls();
begin
  pbOK.Enabled := (Partition <> '');
  
end;


function TfrmSelectPartition.GetPartition(): string;
begin
  Result := fmeSelectPartition.SelectedDevice;
end;

procedure TfrmSelectPartition.fmeSelectPartitionChanged(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmSelectPartition.fmeSelectPartitionSDUDiskPartitionsPanel1DblClick(
  Sender: TObject);
begin
  pbOKClick(Sender);
end;

procedure TfrmSelectPartition.fmeSelectPartitionpnlNoPartitionDisplayDblClick(
  Sender: TObject);
begin
  pbOKClick(Sender);
end;

END.


