unit OTFEFreeOTFE_fmePKCS11_MgrBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs,
  pkcs11_session, ActnList, Menus;

type
  TfmePKCS11_MgrBase = class(TFrame)
    ActionList1: TActionList;
    actRefresh: TAction;
    PopupMenu1: TPopupMenu;
    Refresh1: TMenuItem;
    procedure actRefreshExecute(Sender: TObject);
  private
    { Private declarations }
  protected
    FPKCS11Session: TPKCS11Session;

    procedure Refresh(); virtual;
  public
    property PKCS11Session: TPKCS11Session read FPKCS11Session write FPKCS11Session;

    procedure Initialize(); virtual; abstract;
    procedure EnableDisableControls(); virtual; abstract;
  end;

implementation

{$R *.dfm}

procedure TfmePKCS11_MgrBase.actRefreshExecute(Sender: TObject);
begin
  Refresh();
end;

procedure TfmePKCS11_MgrBase.Refresh();
begin
  EnableDisableControls();
end;

END.

