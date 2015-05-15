unit fmePKCS11_MgrBase;

interface

uses
  ActnList, Classes, Controls, Dialogs,
  Forms,
  Graphics, Menus, Messages, pkcs11_session, SysUtils, Variants, Windows;

type
  TfmePKCS11Mgr = class (TFrame)
    ActionList1: TActionList;
    actRefresh:  TAction;
    PopupMenu1:  TPopupMenu;
    Refresh1:    TMenuItem;
    procedure actRefreshExecute(Sender: TObject);
  PRIVATE
    { Private declarations }
  PROTECTED
    FPKCS11Session: TPKCS11Session;

    procedure Refresh(); VIRTUAL;
  PUBLIC
    property PKCS11Session: TPKCS11Session Read FPKCS11Session Write FPKCS11Session;

    procedure Initialize(); VIRTUAL; ABSTRACT;
    procedure EnableDisableControls(); VIRTUAL; ABSTRACT;
  end;

implementation

{$R *.dfm}

procedure TfmePKCS11Mgr.actRefreshExecute(Sender: TObject);
begin
  Refresh();
end;

procedure TfmePKCS11Mgr.Refresh();
begin
  EnableDisableControls();
end;

end.
