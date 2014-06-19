unit OTFEUnified_frmSelectOTFESystem;
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
  OTFEUnified_U;

type
  TfrmSelectOTFESystem = class(TForm)
    Label1: TLabel;
    cbOTFESystem: TComboBox;
    pbOK: TButton;
    pbCancel: TButton;
    Label2: TLabel;
    Label3: TLabel;
    procedure pbOKClick(Sender: TObject);
    procedure cbOTFESystemChange(Sender: TObject);
  private
    Selected: TOTFESystem;
  public
    procedure Add(system: TOTFESystem);
    function  GetSelected(): TOTFESystem;
  end;


implementation

{$R *.DFM}

procedure TfrmSelectOTFESystem.Add(system: TOTFESystem);
begin
  cbOTFESystem.Items.AddObject(OTFESDispNames[system], TObject(ord(system)));
  cbOTFESystem.ItemIndex := 0;

end;


function TfrmSelectOTFESystem.GetSelected(): TOTFESystem;
begin
  Result := Selected;

end;


procedure TfrmSelectOTFESystem.pbOKClick(Sender: TObject);
begin
  Selected := TOTFESystem(cbOTFESystem.Items.Objects[cbOTFESystem.ItemIndex]);
  ModalResult := mrOK;
  
end;

procedure TfrmSelectOTFESystem.cbOTFESystemChange(Sender: TObject);
begin
  pbOK.Enabled := (cbOTFESystem.ItemIndex>=0);
  
end;

END.

