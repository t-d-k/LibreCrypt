unit FreeOTFEfmeOptions_Hotkeys;

interface

uses
  Classes, ComCtrls, CommonfmeOptions_Base,
  Controls, Dialogs, Forms,
  FreeOTFEfmeOptions_Base, FreeOTFESettings,
  Graphics, Messages, SDUStdCtrls, StdCtrls, SysUtils, Variants, Windows;

type
  TfmeOptions_Hotkeys = class (TfmeFreeOTFEOptions_Base)
    gbHotkeys:             TGroupBox;
    Label1:                TLabel;
    Label2:                TLabel;
    hkDismount:            THotKey;
    ckHotkeyDismount:      TSDUCheckBox;
    ckHotkeyDismountEmerg: TSDUCheckBox;
    hkDismountEmerg:       THotKey;
    procedure ckCheckBoxClick(Sender: TObject);
  PROTECTED
    procedure _ReadSettings(config: TFreeOTFESettings); OVERRIDE;
    procedure _WriteSettings(config: TFreeOTFESettings); OVERRIDE;
  PUBLIC
    procedure Initialize(); OVERRIDE;
    procedure EnableDisableControls(); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
  Math,
  SDUGeneral;

procedure TfmeOptions_Hotkeys.ckCheckBoxClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeOptions_Hotkeys.Initialize();
var
  maxCkBoxWidth: Integer;
begin
  // ckHotkeyDismount and ckHotkeyDismountEmerg have AutoSize := TRUE
  // Use the autosized controls to determine how big the groupbox needs to be
  maxCkBoxWidth   := max(ckHotkeyDismount.Width, ckHotkeyDismountEmerg.Width);
  gbHotkeys.Width := max(((ckHotkeyDismount.left * 2) + maxCkBoxWidth),
    max((hkDismount.left +
    hkDismount.Width + ckHotkeyDismount.left), (hkDismountEmerg.left +
    hkDismountEmerg.Width + ckHotkeyDismount.left))
    );

  SDUCenterControl(gbHotkeys, ccHorizontal);
  SDUCenterControl(gbHotkeys, ccVertical, 25);
end;

procedure TfmeOptions_Hotkeys.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(hkDismount, ckHotkeyDismount.Checked);
  SDUEnableControl(hkDismountEmerg, ckHotkeyDismountEmerg.Checked);
end;

procedure TfmeOptions_Hotkeys._ReadSettings(config: TFreeOTFESettings);
begin
  // Hotkeys...
  ckHotkeyDismount.Checked      := config.OptHKeyEnableDismount;
  hkDismount.HotKey             := config.OptHKeyKeyDismount;
  ckHotkeyDismountEmerg.Checked := config.OptHKeyEnableDismountEmerg;
  hkDismountEmerg.HotKey        := config.OptHKeyKeyDismountEmerg;

end;

procedure TfmeOptions_Hotkeys._WriteSettings(config: TFreeOTFESettings);
begin
  // Hotkeys...
  config.OptHKeyEnableDismount      := ckHotkeyDismount.Checked;
  config.OptHKeyKeyDismount         := hkDismount.HotKey;
  config.OptHKeyEnableDismountEmerg := ckHotkeyDismountEmerg.Checked;
  config.OptHKeyKeyDismountEmerg    := hkDismountEmerg.HotKey;

end;

end.
