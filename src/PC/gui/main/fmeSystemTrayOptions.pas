unit fmeSystemTrayOptions;
{ TODO : why is this a separate frame? only used once }

interface

uses
  Classes, fmeBaseOptions, Controls, Dialogs, Forms,
  fmeLcOptions, MainSettings, Graphics, Messages, SDUStdCtrls,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeSystemTrayOptions = class (TfmeLcOptions)
    gbSystemTrayIcon:    TGroupBox;
    ckUseSystemTrayIcon: TSDUCheckBox;
    ckMinToIcon:         TSDUCheckBox;
    ckCloseToIcon:       TSDUCheckBox;
    gbClickActions:      TGroupBox;
    rbSingleClick:       TRadioButton;
    rbDoubleClick:       TRadioButton;
    Label1:              TLabel;
    Label2:              TLabel;
    cbClickAction:       TComboBox;
    procedure ckUseSystemTrayIconClick(Sender: TObject);
  PRIVATE
    procedure PopulateAndSetClickAction(cbox: TComboBox; selectedAction: TSystemTrayClickAction);
    function GetClickAction(cbox: TComboBox): TSystemTrayClickAction;
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

procedure TfmeSystemTrayOptions.ckUseSystemTrayIconClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeSystemTrayOptions.Initialize();
var
  maxToIconCkBoxWidth: Integer;
  maxgbWidth:          Integer;
begin
  // ckHotkeyDismount and ckHotkeyDismountEmerg have AutoSize := TRUE
  // Use the autosized controls to determine how big the groupbox needs to be
  maxToIconCkBoxWidth    := max(ckMinToIcon.Width, ckCloseToIcon.Width);
  maxgbWidth             := 0;
  maxgbWidth             := max(maxgbWidth, (ckUseSystemTrayIcon.left * 2) +
    ckUseSystemTrayIcon.Width);
  maxgbWidth             := max(maxgbWidth, (ckMinToIcon.left + maxToIconCkBoxWidth +
    ckUseSystemTrayIcon.left));
  maxgbWidth             := max(maxgbWidth, (gbClickActions.left +
    gbClickActions.Width + ckUseSystemTrayIcon.left));
  gbSystemTrayIcon.Width := maxgbWidth;

  SDUCenterControl(gbSystemTrayIcon, ccHorizontal);
  SDUCenterControl(gbSystemTrayIcon, ccVertical, 25);

end;

procedure TfmeSystemTrayOptions.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(ckMinToIcon, ckUseSystemTrayIcon.Checked);
  SDUEnableControl(ckCloseToIcon, ckUseSystemTrayIcon.Checked);
  SDUEnableControl(gbClickActions, ckUseSystemTrayIcon.Checked);
end;

procedure TfmeSystemTrayOptions._ReadSettings(config: TMainSettings);
begin
  // System tray icon related...
  ckUseSystemTrayIcon.Checked := config.OptSystemTrayIconDisplay;
  ckMinToIcon.Checked         := config.OptSystemTrayIconMinTo;
  ckCloseToIcon.Checked       := config.OptSystemTrayIconCloseTo;

  if (config.OptSystemTrayIconActionSingleClick <> stcaDoNothing) then begin
    rbSingleClick.Checked := True;
    PopulateAndSetClickAction(cbClickAction, config.OptSystemTrayIconActionSingleClick);
  end else begin
    rbDoubleClick.Checked := True;
    PopulateAndSetClickAction(cbClickAction, config.OptSystemTrayIconActionDoubleClick);
  end;

end;

procedure TfmeSystemTrayOptions._WriteSettings(config: TMainSettings);
begin
  // System tray icon related...
  config.OptSystemTrayIconDisplay := ckUseSystemTrayIcon.Checked;
  config.OptSystemTrayIconMinTo   := ckMinToIcon.Checked;
  config.OptSystemTrayIconCloseTo := ckCloseToIcon.Checked;

  if rbSingleClick.Checked then begin
    config.OptSystemTrayIconActionSingleClick := GetClickAction(cbClickAction);
    config.OptSystemTrayIconActionDoubleClick := stcaDoNothing;
  end else begin
    config.OptSystemTrayIconActionSingleClick := stcaDoNothing;
    config.OptSystemTrayIconActionDoubleClick := GetClickAction(cbClickAction);
  end;

end;

procedure TfmeSystemTrayOptions.PopulateAndSetClickAction(cbox: TComboBox;
  selectedAction: TSystemTrayClickAction);
var
  stca:   TSystemTrayClickAction;
  idx:    Integer;
  useIdx: Integer;
begin
  // Populate and set default store op dropdown
  cbox.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for stca := low(stca) to high(stca) do begin
    Inc(idx);
    cbox.Items.Add(SystemTrayClickActionTitle(stca));
    if (selectedAction = stca) then begin
      useIdx := idx;
    end;
  end;
  cbox.ItemIndex := useIdx;

end;

function TfmeSystemTrayOptions.GetClickAction(cbox: TComboBox): TSystemTrayClickAction;
var
  stca:   TSystemTrayClickAction;
begin
  Result := stcaDoNothing;

  // Decode click action...
  for stca := low(stca) to high(stca) do begin
    if (SystemTrayClickActionTitle(stca) = cbox.Items[cbox.ItemIndex]) then begin
      Result := stca;
      break;
    end;
  end;


end;

end.
