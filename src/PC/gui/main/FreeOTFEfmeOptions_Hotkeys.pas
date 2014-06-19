unit FreeOTFEfmeOptions_Hotkeys;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, FreeOTFESettings,
  SDUStdCtrls, CommonfmeOptions_Base,
  FreeOTFEfmeOptions_Base;

type
  TfmeOptions_Hotkeys = class(TfmeFreeOTFEOptions_Base)
    gbHotkeys: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    hkDismount: THotKey;
    ckHotkeyDismount: TSDUCheckBox;
    ckHotkeyDismountEmerg: TSDUCheckBox;
    hkDismountEmerg: THotKey;
    procedure ckCheckBoxClick(Sender: TObject);
  protected
    procedure _ReadSettings(config: TFreeOTFESettings); override;
    procedure _WriteSettings(config: TFreeOTFESettings); override;
  public
    procedure Initialize(); override;
    procedure EnableDisableControls(); override;
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
  maxCkBoxWidth: integer;
begin
  // ckHotkeyDismount and ckHotkeyDismountEmerg have AutoSize := TRUE
  // Use the autosized controls to determine how big the groupbox needs to be
  maxCkBoxWidth := max(ckHotkeyDismount.width, ckHotkeyDismountEmerg.width);
  gbHotkeys.width := max(
                         ((ckHotkeyDismount.left * 2) + maxCkBoxWidth),
                         max(
                             (hkDismount.left + hkDismount.width + ckHotkeyDismount.left),
                             (hkDismountEmerg.left + hkDismountEmerg.width + ckHotkeyDismount.left)
                            )
                        );

  SDUCenterControl(gbHotkeys, ccHorizontal);
  SDUCenterControl(gbHotkeys, ccVertical, 25);
end;

procedure TfmeOptions_Hotkeys.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(hkDismount,      ckHotkeyDismount.checked);
  SDUEnableControl(hkDismountEmerg, ckHotkeyDismountEmerg.checked);
end;

procedure TfmeOptions_Hotkeys._ReadSettings(config: TFreeOTFESettings);
begin
  // Hotkeys...
  ckHotkeyDismount.checked      := config.OptHKeyEnableDismount;
  hkDismount.HotKey             := config.OptHKeyKeyDismount;
  ckHotkeyDismountEmerg.checked := config.OptHKeyEnableDismountEmerg;
  hkDismountEmerg.HotKey        := config.OptHKeyKeyDismountEmerg;

end;

procedure TfmeOptions_Hotkeys._WriteSettings(config: TFreeOTFESettings);
begin
  // Hotkeys...
  config.OptHKeyEnableDismount      := ckHotkeyDismount.checked;
  config.OptHKeyKeyDismount         := hkDismount.HotKey;
  config.OptHKeyEnableDismountEmerg := ckHotkeyDismountEmerg.checked;
  config.OptHKeyKeyDismountEmerg    := hkDismountEmerg.HotKey;

end;

END.

