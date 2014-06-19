unit FreeOTFEfmeOptions_SystemTray;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FreeOTFESettings, SDUStdCtrls,
  CommonfmeOptions_Base, FreeOTFEfmeOptions_Base;

type
  TfmeOptions_SystemTray = class(TfmeFreeOTFEOptions_Base)
    gbSystemTrayIcon: TGroupBox;
    ckUseSystemTrayIcon: TSDUCheckBox;
    ckMinToIcon: TSDUCheckBox;
    ckCloseToIcon: TSDUCheckBox;
    gbClickActions: TGroupBox;
    rbSingleClick: TRadioButton;
    rbDoubleClick: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    cbClickAction: TComboBox;
    procedure ckUseSystemTrayIconClick(Sender: TObject);
  private
    procedure PopulateAndSetClickAction(cbox: TComboBox; selectedAction: TSystemTrayClickAction);
    function  GetClickAction(cbox: TComboBox): TSystemTrayClickAction;
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

procedure TfmeOptions_SystemTray.ckUseSystemTrayIconClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeOptions_SystemTray.Initialize();
var
  maxToIconCkBoxWidth: integer;
  maxgbWidth: integer;
begin
  // ckHotkeyDismount and ckHotkeyDismountEmerg have AutoSize := TRUE
  // Use the autosized controls to determine how big the groupbox needs to be
  maxToIconCkBoxWidth := max(ckMinToIcon.width, ckCloseToIcon.width);
  maxgbWidth := 0;
  maxgbWidth := max(
                    maxgbWidth,
                    (ckUseSystemTrayIcon.left * 2) + ckUseSystemTrayIcon.width
                   );
  maxgbWidth := max(
                    maxgbWidth,
                   (ckMinToIcon.left + maxToIconCkBoxWidth + ckUseSystemTrayIcon.left)
                   );
  maxgbWidth := max(
                    maxgbWidth,
                    (gbClickActions.left + gbClickActions.width + ckUseSystemTrayIcon.left)
                   );
  gbSystemTrayIcon.width := maxgbWidth;

  SDUCenterControl(gbSystemTrayIcon, ccHorizontal);
  SDUCenterControl(gbSystemTrayIcon, ccVertical, 25);

end;

procedure TfmeOptions_SystemTray.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(ckMinToIcon,   ckUseSystemTrayIcon.checked);
  SDUEnableControl(ckCloseToIcon, ckUseSystemTrayIcon.checked);
  SDUEnableControl(gbClickActions, ckUseSystemTrayIcon.checked);
end;

procedure TfmeOptions_SystemTray._ReadSettings(config: TFreeOTFESettings);
begin
  // System tray icon related...
  ckUseSystemTrayIcon.checked := config.OptSystemTrayIconDisplay;
  ckMinToIcon.checked         := config.OptSystemTrayIconMinTo;
  ckCloseToIcon.checked       := config.OptSystemTrayIconCloseTo;

  if (config.OptSystemTrayIconActionSingleClick <> stcaDoNothing) then
    begin
    rbSingleClick.checked := TRUE;
    PopulateAndSetClickAction(cbClickAction, config.OptSystemTrayIconActionSingleClick);
    end
  else
    begin
    rbDoubleClick.checked := TRUE;
    PopulateAndSetClickAction(cbClickAction, config.OptSystemTrayIconActionDoubleClick);
    end;

end;

procedure TfmeOptions_SystemTray._WriteSettings(config: TFreeOTFESettings);
begin
  // System tray icon related...
  config.OptSystemTrayIconDisplay := ckUseSystemTrayIcon.checked;
  config.OptSystemTrayIconMinTo   := ckMinToIcon.checked;
  config.OptSystemTrayIconCloseTo := ckCloseToIcon.checked;

  if rbSingleClick.checked then
    begin
    config.OptSystemTrayIconActionSingleClick := GetClickAction(cbClickAction);
    config.OptSystemTrayIconActionDoubleClick := stcaDoNothing;
    end
  else
    begin
    config.OptSystemTrayIconActionSingleClick := stcaDoNothing;
    config.OptSystemTrayIconActionDoubleClick := GetClickAction(cbClickAction);
    end;

end;

procedure TfmeOptions_SystemTray.PopulateAndSetClickAction(cbox: TComboBox; selectedAction: TSystemTrayClickAction);
var
  stca: TSystemTrayClickAction;
  idx: integer;
  useIdx: integer;
begin
  // Populate and set default store op dropdown
  cbox.Items.Clear();
  idx := -1;
  useIdx := -1;
  for stca:=low(stca) to high(stca) do
    begin
    inc(idx);
    cbox.Items.Add(SystemTrayClickActionTitle(stca));
    if (selectedAction = stca) then
      begin
      useIdx := idx;
      end;
    end;
  cbox.ItemIndex := useIdx;

end;

function TfmeOptions_SystemTray.GetClickAction(cbox: TComboBox): TSystemTrayClickAction;
var
  stca: TSystemTrayClickAction;
  retval: TSystemTrayClickAction;
begin
  retval := stcaDoNothing;

  // Decode click action...
  for stca:=low(stca) to high(stca) do
    begin
    if (SystemTrayClickActionTitle(stca) = cbox.Items[cbox.ItemIndex]) then
      begin
      retval := stca;
      break;
      end;
    end;

  Result := retval;
end;

END.

