unit fmeHotKeysOptions;
{todo:this frame is only used in one place so should be inserted there instead}
interface

uses
  Classes, ComCtrls, fmeBaseOptions,
  Controls, Dialogs, Forms,
  fmeLcOptions, MainSettings,
  Graphics, Messages, SDUStdCtrls, StdCtrls, SysUtils, Variants, Windows;

type
  TfmeHotKeysOptions = class (TfmeLcOptions)
    gbHotkeys:             TGroupBox;
    Label1:                TLabel;
    Label2:                TLabel;
    hkDismount:            THotKey;
    ckHotkeyDismount:      TSDUCheckBox;
    ckHotkeyDismountEmerg: TSDUCheckBox;
    hkDismountEmerg:       THotKey;
    procedure ckCheckBoxClick(Sender: TObject);
  PROTECTED
    procedure _ReadSettings(config: TMainSettings); OVERRIDE;
    procedure _WriteSettings(config: TMainSettings); OVERRIDE;
  PUBLIC
    procedure Initialize(); OVERRIDE;
    procedure EnableDisableControls(); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
  Math,
  SDUGeneral;

procedure TfmeHotKeysOptions.ckCheckBoxClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeHotKeysOptions.Initialize();
var
  maxCkBoxWidth: Integer;
begin
  // ckHotkeyDismount and ckHotkeyDismountEmerg have AutoSize := TRUE
  // Use the autosized controls to determine how big the groupbox needs to be
  maxCkBoxWidth   := max(ckHotkeyDismount.Width, ckHotkeyDismountEmerg.Width);
  gbHotkeys.Width := max(((ckHotkeyDismount.left * 2) + maxCkBoxWidth), max(
    (hkDismount.left + hkDismount.Width + ckHotkeyDismount.left),
    (hkDismountEmerg.left + hkDismountEmerg.Width + ckHotkeyDismount.left)));

  SDUCenterControl(gbHotkeys, ccHorizontal);
  SDUCenterControl(gbHotkeys, ccVertical, 25);
end;

procedure TfmeHotKeysOptions.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(hkDismount, ckHotkeyDismount.Checked);
  SDUEnableControl(hkDismountEmerg, ckHotkeyDismountEmerg.Checked);
end;

procedure TfmeHotKeysOptions._ReadSettings(config: TMainSettings);
begin
  // Hotkeys...
  ckHotkeyDismount.Checked      := config.OptHKeyEnableDismount;
  hkDismount.HotKey             := config.OptHKeyKeyDismount;
  ckHotkeyDismountEmerg.Checked := config.OptHKeyEnableDismountEmerg;
  hkDismountEmerg.HotKey        := config.OptHKeyKeyDismountEmerg;

end;

procedure TfmeHotKeysOptions._WriteSettings(config: TMainSettings);
begin
  // Hotkeys...
  config.OptHKeyEnableDismount      := ckHotkeyDismount.Checked;
  config.OptHKeyKeyDismount         := hkDismount.HotKey;
  config.OptHKeyEnableDismountEmerg := ckHotkeyDismountEmerg.Checked;
  config.OptHKeyKeyDismountEmerg    := hkDismountEmerg.HotKey;

end;

end.
