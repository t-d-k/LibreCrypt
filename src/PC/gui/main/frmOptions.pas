unit frmOptions;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //

{ TODO 1 -otdk -crefactor : move most controls on general tab to common options parent as shared with explorer }

interface

uses
  //delphi & libs
  ExtCtrls, Forms, Classes, ComCtrls, StdCtrls, SysUtils, Windows, Graphics,
  Messages, Controls, Dialogs,

  //sdu & LibreCrypt utils
  CommonSettings, OTFEFreeOTFE_U, SDUStdCtrls, MainSettings, SDUFilenameEdit_U,

  // LibreCrypt forms
  frmCommonOptions,
//  fmeAdvancedOptions, fmeSystemTrayOptions,
//   fmeLcOptions,
//   fmeBaseOptions,
    fmeVolumeSelect, SDUForms, SDUFrames,
  Spin64, SDUDialogs;

type


  TfrmOptions = class (TfrmCommonOptions)
    tsHotkeys:        TTabSheet;
    tcSystemTray:     TTabSheet;
    tsMain: TTabSheet;
    ckLaunchAtStartup: TSDUCheckBox;
    ckLaunchMinimisedAtStartup: TSDUCheckBox;
    gbHotkeys:        TGroupBox;
    Label1:           TLabel;
    Label2:           TLabel;
    hkDismount:       THotKey;
    ckHotkeyDismount: TSDUCheckBox;
    ckHotkeyDismountEmerg: TSDUCheckBox;
    hkDismountEmerg:  THotKey;
    gbAdvanced: TGroupBox;
    lblDragDrop: TLabel;
    lblOnNormalDismountFail: TLabel;
    lblDefaultMountAs: TLabel;
    lblOnExitWhenMounted: TLabel;
    lblOnExitWhenPortableMode: TLabel;
    ckAllowMultipleInstances: TSDUCheckBox;
    cbDragDrop: TComboBox;
    ckAutoStartPortable: TSDUCheckBox;
    ckWarnBeforeForcedDismount: TSDUCheckBox;
    cbOnNormalDismountFail: TComboBox;
    cbDefaultMountAs: TComboBox;
    cbOnExitWhenMounted: TComboBox;
    cbOnExitWhenPortableMode: TComboBox;
    gbSystemTrayIcon: TGroupBox;
    ckUseSystemTrayIcon: TSDUCheckBox;
    ckMinToIcon: TSDUCheckBox;
    ckCloseToIcon: TSDUCheckBox;
    gbClickActions: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    cbSingleClickAction: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    cbDbleClickAction: TComboBox;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ckLaunchAtStartupClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);


  private

    procedure _InitializeHotKeys;
    procedure _WriteSettingsHotKeys(config: TMainSettings);
    procedure _ReadSettingsHotKeys(config: TMainSettings);
    procedure _EnableDisableControlsHotKeys;
    procedure _InitializeAdvancedTab;
    procedure _ReadSettingsAdvanced(config: TMainSettings);
    procedure _WriteSettingsAdvanced(config: TMainSettings);
    procedure _EnableDisableControlsGeneral;
    procedure _InitializeGeneral;
    procedure _ReadSettingsGeneral(config: TMainSettings);
    procedure _InitializeSystemTrayOptions;
    procedure _EnableDisableControlsSystemTray;
    procedure _ReadSettingsSystemTray(config: TMainSettings);
    procedure _WriteSettingsSystemTray(config: TMainSettings);
    procedure _PopulateAndSetClickAction(cbox: TComboBox;
      selectedAction: TSystemTrayClickAction);
    function _GetClickAction(cbox: TComboBox): TSystemTrayClickAction;
  protected
    FOrigLaunchAtStartup:          Boolean;
    FOrigLaunchMinimisedAtStartup: Boolean;

    procedure _EnableDisableControls(); override;
    procedure _InitAndReadSettings(config: TCommonSettings); override;
    procedure _WriteAllSettings(config: TCommonSettings); override;
    function _DoOKClicked(): Boolean; override;
  public


  end;

//const
//  CONTROL_MARGIN_LBL_TO_CONTROL = 5;

implementation

{$R *.DFM}

uses
  // delphi

  Math,
  ShlObj,  // Required for CSIDL_PROGRAMS
  StrUtils,
  // sdu /librecrypt
  OTFEFreeOTFEBase_U,
  SDUGeneral, lcTypes, CommonConsts,lcDialogs, lcConsts,
  SDUi18n,
  lcCommandLine // for CMDLINE_MINIMIZE
  //forms
  ;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


procedure TfrmOptions._WriteSettingsHotKeys(config: TMainSettings);
begin
  // Hotkeys...
  { TODO : what;s the point of separate enable options - just set keys to blank }
  config.DismountHotKeyEnabled      := ckHotkeyDismount.Checked;
  config.DismountHotKey         := hkDismount.HotKey;
  config.EmergencyDismountHotKeyEnabled := ckHotkeyDismountEmerg.Checked;
  config.EmergencyDismountHotKey    := hkDismountEmerg.HotKey;
end;


function TfrmOptions._GetClickAction(cbox: TComboBox): TSystemTrayClickAction;
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

procedure TfrmOptions._WriteSettingsSystemTray(config: TMainSettings);
begin
  // System tray icon related...
  config.ShowSysTrayIcon := ckUseSystemTrayIcon.Checked;
  config.MinimiseToSysTray   := ckMinToIcon.Checked;
  config.CloseToSysTray := ckCloseToIcon.Checked;
  config.SysTraySingleClickAction := _GetClickAction(cbSingleClickAction);
  config.SysTrayDoubleClickAction := _GetClickAction(cbDbleClickAction);
end;

procedure TfrmOptions._WriteSettingsAdvanced(config: TMainSettings);
var
  owem: eOnExitWhenMounted;
  owrp: eOnExitWhenPortableMode;
  ondf: eOnNormalDismountFail;
  ma:   TMountDiskType;
  ft:   TVolumeType;
begin
  // Advanced...
  config.AllowMultipleInstances   := ckAllowMultipleInstances.Checked;
  config.AutoStartPortableMode        := ckAutoStartPortable.Checked;

  config.WarnBeforeForceDismount := ckWarnBeforeForcedDismount.Checked;

  // Decode action on exiting when volumes mounted
  config.ExitWhenMountedAction := oewmPromptUser;
  for owem := low(owem) to high(owem) do begin
    if (OnExitWhenMountedTitle(owem) = cbOnExitWhenMounted.Items[cbOnExitWhenMounted.ItemIndex])
    then begin
      config.ExitWhenMountedAction := owem;
      break;
    end;
  end;

  // Decode action on exiting when in portable mode
  config.ExitWhenPortableModeAction := oewpPromptUser;
  for owrp := low(owrp) to high(owrp) do begin
    if (OnExitWhenPortableModeTitle(owrp) =
      cbOnExitWhenPortableMode.Items[cbOnExitWhenPortableMode.ItemIndex]) then begin
      config.ExitWhenPortableModeAction := owrp;
      break;
    end;
  end;

  // Decode action when normal dismount fails
  config.NormalDismountFailAction := ondfPromptUser;
  for ondf := low(ondf) to high(ondf) do begin
    if (OnNormalDismountFailTitle(ondf) =
      cbOnNormalDismountFail.Items[cbOnNormalDismountFail.ItemIndex]) then begin
      config.NormalDismountFailAction := ondf;
      break;
    end;
  end;

  // Decode default mount type
  config.DefaultMountDiskType := fomaRemovableDisk;
  for ma := low(ma) to high(ma) do begin
    if (FreeOTFEMountAsTitle(ma) = cbDefaultMountAs.Items[cbDefaultMountAs.ItemIndex]) then begin
      config.DefaultMountDiskType := ma;
      break;
    end;
  end;

  // Decode drag drop filetype
  config.DefaultVolType := vtUnknown;
  for ft := low(ft) to high(ft) do begin
    if (DragDropFileTypeTitle(ft) = cbDragDrop.Items[cbDragDrop.ItemIndex]) then begin
      config.DefaultVolType := ft;
      break;
    end;
  end;



end;

procedure TfrmOptions._WriteAllSettings(config: TCommonSettings);
begin
  inherited;
  _WriteSettingsHotKeys(config as TMainSettings);

  _WriteSettingsAdvanced(config as TMainSettings);
  _WriteSettingsSystemTray(config as TMainSettings);
end;


procedure TfrmOptions.ckLaunchAtStartupClick(Sender: TObject);
begin
  inherited;
  _EnableDisableControls();
end;

procedure TfrmOptions.ControlChanged(Sender: TObject);
begin
  inherited;
  _EnableDisableControls();
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  inherited;
  // Set active page to the first one
  pcOptions.ActivePage := tsGeneral;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  inherited;

  FOrigLaunchAtStartup      := SDUDoesShortcutExist(SDU_CSIDL_STARTUP, Application.Title);
  ckLaunchAtStartup.Checked := FOrigLaunchAtStartup;

  FOrigLaunchMinimisedAtStartup := False;
  if FOrigLaunchAtStartup then
    FOrigLaunchMinimisedAtStartup :=
      AnsiContainsStr(SDUGetShortCutArguments(SDU_CSIDL_STARTUP, Application.Title), CMDLINE_MINIMIZE);

  ckLaunchMinimisedAtStartup.Checked := FOrigLaunchMinimisedAtStartup;

  // Push the "Advanced" tab to the far end; even after the "PKCS#11" tab
  tsAdvanced.PageIndex := (pcOptions.PageCount - 1);
end;


procedure TfrmOptions._EnableDisableControlsHotKeys();
begin
  inherited;

  SDUEnableControl(hkDismount, ckHotkeyDismount.Checked);
  SDUEnableControl(hkDismountEmerg, ckHotkeyDismountEmerg.Checked);
end;

procedure TfrmOptions._EnableDisableControlsSystemTray();
begin
  inherited;

  SDUEnableControl(ckMinToIcon, ckUseSystemTrayIcon.Checked);
  SDUEnableControl(ckCloseToIcon, ckUseSystemTrayIcon.Checked);
  SDUEnableControl(gbClickActions, ckUseSystemTrayIcon.Checked);
end;



procedure TfrmOptions._EnableDisableControlsGeneral();
begin
  inherited;
  // Language at index 0 is "(Default)"
  SDUEnableControl(pbLangDetails, (cbLanguage.ItemIndex > 0));

  SDUEnableControl(ckDisplayToolbarLarge, ckDisplayToolbar.Checked);
  SDUEnableControl(ckDisplayToolbarCaptions,
    (ckDisplayToolbar.Checked and ckDisplayToolbarLarge.Checked));

  // Only allow captions if the user has selected large icons
  if not (ckDisplayToolbarLarge.Checked) then begin
    ckDisplayToolbarCaptions.Checked := False;
  end;
end;

procedure TfrmOptions._EnableDisableControls();
begin
  inherited;
  _EnableDisableControlsHotKeys;
  _EnableDisableControlsGeneral;
  _EnableDisableControlsSystemTray;

  SDUEnableControl(ckLaunchMinimisedAtStartup, ckLaunchAtStartup.Checked);
  if not (ckLaunchMinimisedAtStartup.Enabled) then
    ckLaunchMinimisedAtStartup.Checked := False;
end;



procedure TfrmOptions._PopulateAndSetClickAction(cbox: TComboBox;
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


procedure TfrmOptions._InitializeAdvancedTab();
const
  // Vertical spacing between checkboxes, and min horizontal spacing between
  // checkbox label and the vertical separator line
  // Set to 6 for now as that looks reasonable. 10 is excessive, 5 is a bit
  // cramped
  CHKBOX_CONTROL_MARGIN = 6;

  // Min horizontal spacing between label and control to the right of it
  LABEL_CONTROL_MARGIN = 10;

  // This adjusts the width of a checkbox, and resets it caption so it
  // autosizes. If it autosizes such that it's too wide, it'll drop the width
  // and repeat
//  procedure NudgeCheckbox(chkBox: TCheckBox);
//  var
//    tmpCaption:     String;
//    maxWidth:       Integer;
//    useWidth:       Integer;
//    lastTriedWidth: Integer;
//  begin
//    tmpCaption := chkBox.Caption;
//
//    maxWidth := (pnlVertSplit.left - CHKBOX_CONTROL_MARGIN) - chkBox.Left;
//    useWidth := maxWidth;
//
//    chkBox.Caption := 'X';
//    chkBox.Width   := useWidth;
//    lastTriedWidth := useWidth;
//    chkBox.Caption := tmpCaption;
//    while ((chkBox.Width > maxWidth) and (lastTriedWidth > 0)) do begin
//      // 5 used here; just needs to be something sensible to reduce the
//      // width by; 1 would do pretty much just as well
//      useWidth := useWidth - 5;
//
//      chkBox.Caption := 'X';
//      chkBox.Width   := useWidth;
//      lastTriedWidth := useWidth;
//      chkBox.Caption := tmpCaption;
//    end;
//
//  end;

//  procedure NudgeFocusControl(lbl: TLabel);
//  begin
//    if (lbl.FocusControl <> nil) then begin
//      lbl.FocusControl.Top := lbl.Top + lbl.Height + CONTROL_MARGIN_LBL_TO_CONTROL;
//    end;
//
//  end;

//  procedure NudgeLabel(lbl: TLabel);
//  var
//    maxWidth: Integer;
//  begin
//    if (pnlVertSplit.left > lbl.left) then begin
//      maxWidth := (pnlVertSplit.left - LABEL_CONTROL_MARGIN) - lbl.left;
//    end else begin
//      maxWidth := (lbl.Parent.Width - LABEL_CONTROL_MARGIN) - lbl.left;
//    end;
//
//    lbl.Width := maxWidth;
//  end;

var
//  stlChkBoxOrder: TStringList;
//  YPos:           Integer;
//  i:              Integer;
//  currChkBox:     TCheckBox;
  groupboxMargin: Integer;
begin
  inherited;

  SDUCenterControl(gbAdvanced, ccHorizontal);
  SDUCenterControl(gbAdvanced, ccVertical, 25);

  // Re-jig label size to take cater for differences in translation lengths
  // Size so the max. right is flush with the max right of pbLangDetails
  //  lblDragDrop.width        := (pbLangDetails.left + pbLangDetails.width) - lblDragDrop.left;
  //  lblMRUMaxItemCount.width := (pbLangDetails.left + pbLangDetails.width) - lblMRUMaxItemCount.left;
  groupboxMargin           := ckAutoStartPortable.left;
  lblDragDrop.Width        := (gbAdvanced.Width - groupboxMargin) - lblDragDrop.left;
  lblMRUMaxItemCount.Width := (gbAdvanced.Width - groupboxMargin) - lblMRUMaxItemCount.left;

//  pnlVertSplit.Caption    := '';
//  pnlVertSplit.bevelouter := bvLowered;
//  pnlVertSplit.Width      := 3;

  // Here we re-jig the checkboxes so that they are nicely spaced vertically.
  // This is needed as some language translation require the checkboxes to have
  // more than one line of text
  //
  // !! IMPORTANT !!
  // When adding a checkbox:
  //   1) Add it to stlChkBoxOrder below (doesn't matter in which order these
  //      are added)
  //   2) Make sure the checkbox is a TSDUCheckBox, not just a normal Delphi
  //      TCheckBox
  //   3) Make sure it's autosize property is TRUE
  //
//  stlChkBoxOrder := TStringList.Create();
//  try
//    // stlChkBoxOrder is used to order the checkboxes in their vertical order;
//    // this allows checkboxes to be added into the list below in *any* order,
//    // and it'll still work
//    stlChkBoxOrder.Sorted := True;
//
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckAllowMultipleInstances.Top]),
//      ckAllowMultipleInstances);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckAutoStartPortable.Top]), ckAutoStartPortable);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckAdvancedMountDlg.Top]), ckAdvancedMountDlg);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckRevertVolTimestamps.Top]),
//      ckRevertVolTimestamps);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckWarnBeforeForcedDismount.Top]),
//      ckWarnBeforeForcedDismount);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckAllowNewlinesInPasswords.Top]),
//      ckAllowNewlinesInPasswords);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckAllowTabsInPasswords.Top]),
//      ckAllowTabsInPasswords);
//
//    currChkBox := TCheckBox(stlChkBoxOrder.Objects[0]);
//    YPos       := currChkBox.Top;
//    YPos       := YPos + currChkBox.Height;
//    for i := 1 to (stlChkBoxOrder.Count - 1) do begin
//      currChkBox := TCheckBox(stlChkBoxOrder.Objects[i]);
//
//      currChkBox.Top := YPos + CHKBOX_CONTROL_MARGIN;
//
//      // Sort out the checkbox's height
//      NudgeCheckbox(currChkBox);
//
//      YPos := currChkBox.Top;
//      YPos := YPos + currChkBox.Height;
//    end;
//
//  finally
//    stlChkBoxOrder.Free();
//  end;

  // Nudge labels so they're as wide as can be allowed
//  NudgeLabel(lblOnExitWhenMounted);
//  NudgeLabel(lblOnExitWhenPortableMode);
//  NudgeLabel(lblOnNormalDismountFail);
//  NudgeLabel(lblDragDrop);
//  NudgeLabel(lblMRUMaxItemCount);
//  NudgeLabel(lblMRUMaxItemCountInst);
//  NudgeLabel(lblDefaultMountAs);

  // Here we move controls associated with labels, such that they appear
  // underneath the label
  //  NudgeFocusControl(lblLanguage);
//  NudgeFocusControl(lblDragDrop);
  //  NudgeFocusControl(lblDefaultDriveLetter);
  //  NudgeFocusControl(lblMRUMaxItemCount);

end;

procedure TfrmOptions._InitializeHotKeys();
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


procedure TfrmOptions._InitializeSystemTrayOptions();
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


procedure TfrmOptions._InitializeGeneral();
const
  // Vertical spacing between checkboxes, and min horizontal spacing between
  // checkbox label and the vertical separator line
  // Set to 6 for now as that looks reasonable. 10 is excessive, 5 is a bit
  // cramped
  CHKBOX_CONTROL_MARGIN = 6;

  // This adjusts the width of a checkbox, and resets it caption so it
  // autosizes. If it autosizes such that it's too wide, it'll drop the width
  // and repeat
//  procedure NudgeCheckbox(chkBox: TCheckBox);
//  var
//    tmpCaption:     String;
//    maxWidth:       Integer;
//    useWidth:       Integer;
//    lastTriedWidth: Integer;
//  begin
//    tmpCaption := chkBox.Caption;

//    maxWidth := (pnlVertSplit.left - CHKBOX_CONTROL_MARGIN) - chkBox.Left;
//    useWidth := maxWidth;
//
//    chkBox.Caption := 'X';
//    chkBox.Width   := useWidth;
//    lastTriedWidth := useWidth;
//    chkBox.Caption := tmpCaption;
//    while ((chkBox.Width > maxWidth) and (lastTriedWidth > 0)) do begin
//      // 5 used here; just needs to be something sensible to reduce the
//      // width by; 1 would do pretty much just as well
//      useWidth := useWidth - 5;
//
//      chkBox.Caption := 'X';
//      chkBox.Width   := useWidth;
//      lastTriedWidth := useWidth;
//      chkBox.Caption := tmpCaption;
//    end;

//  end;

//  procedure NudgeFocusControl(lbl: TLabel);
//  begin
//    if (lbl.FocusControl <> nil) then begin
//      lbl.FocusControl.Top := lbl.Top + lbl.Height + CONTROL_MARGIN_LBL_TO_CONTROL;
//    end;
//
//  end;

var
  driveLetter:    Char;
//  stlChkBoxOrder: TStringList;
//  YPos:           Integer;
//  i:              Integer;//
//  currChkBox:     TCheckBox;
begin
  inherited;

//  SDUCenterControl(gbGeneral, ccHorizontal);
//  SDUCenterControl(gbGeneral, ccVertical, 25);

//  pnlVertSplit.Caption    := '';
//  pnlVertSplit.bevelouter := bvLowered;
//  pnlVertSplit.Width      := 3;



  cbDrive.Items.Clear();
  cbDrive.Items.Add(USE_DEFAULT);
  //  for driveLetter:='C' to 'Z' do
  for driveLetter := 'A' to 'Z' do begin
    cbDrive.Items.Add(driveLetter + ':');
  end;


  // Here we re-jig the checkboxes so that they are nicely spaced vertically.
  // This is needed as some language translation require the checkboxes to have
  // more than one line of text
  //
  // !! IMPORTANT !!
  // When adding a checkbox:
  //   1) Add it to stlChkBoxOrder below (doesn't matter in which order these
  //      are added)
  //   2) Make sure the checkbox is a TSDUCheckBox, not just a normal Delphi
  //      TCheckBox
  //   3) Make sure it's autosize property is TRUE
  //
//  stlChkBoxOrder := TStringList.Create();
//  try
//    // stlChkBoxOrder is used to order the checkboxes in their vertical order;
//    // this allows checkboxes to be added into the list below in *any* order,
//    // and it'll still work
//    stlChkBoxOrder.Sorted := True;
//
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckExploreAfterMount.Top]), ckExploreAfterMount);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbar.Top]), ckDisplayToolbar);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarLarge.Top]),
//      ckDisplayToolbarLarge);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarCaptions.Top]),
//      ckDisplayToolbarCaptions);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayStatusbar.Top]), ckDisplayStatusbar);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckShowPasswords.Top]), ckShowPasswords);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckPromptMountSuccessful.Top]),
//      ckPromptMountSuccessful);

//    currChkBox := TCheckBox(stlChkBoxOrder.Objects[0]);
//    YPos       := currChkBox.Top;
//    YPos       := YPos + currChkBox.Height;
//    for i := 1 to (stlChkBoxOrder.Count - 1) do begin
//      currChkBox := TCheckBox(stlChkBoxOrder.Objects[i]);
//
//      if currChkBox.Visible then begin
//        currChkBox.Top := YPos + CHKBOX_CONTROL_MARGIN;
//
//        // Sort out the checkbox's height
//        NudgeCheckbox(currChkBox);
//
//        YPos := currChkBox.Top;
//        YPos := YPos + currChkBox.Height;
//      end;
//    end;
//
//  finally
//    stlChkBoxOrder.Free();
//  end;

  // Here we move controls associated with labels, such that they appear
  // underneath the label
  //  NudgeFocusControl(lblLanguage);
  //  NudgeFocusControl(lblDefaultDriveLetter);
  //  NudgeFocusControl(lblMRUMaxItemCount);

end;

procedure TfrmOptions._ReadSettingsHotKeys(config: TMainSettings);
begin
  // Hotkeys...
  ckHotkeyDismount.Checked      := config.DismountHotKeyEnabled;
  hkDismount.HotKey             := config.DismountHotKey;
  ckHotkeyDismountEmerg.Checked := config.EmergencyDismountHotKeyEnabled;
  hkDismountEmerg.HotKey        := config.EmergencyDismountHotKey;
end;



procedure TfrmOptions._ReadSettingsSystemTray(config: TMainSettings);
begin
  // System tray icon related...
  ckUseSystemTrayIcon.Checked := config.ShowSysTrayIcon;
  ckMinToIcon.Checked         := config.MinimiseToSysTray;
  ckCloseToIcon.Checked       := config.CloseToSysTray;
  _PopulateAndSetClickAction(cbSingleClickAction, config.SysTraySingleClickAction);
  _PopulateAndSetClickAction(cbDbleClickAction, config.SysTrayDoubleClickAction);
end;


procedure TfrmOptions._ReadSettingsGeneral(config: TMainSettings);



begin
  // General...

  ckDisplayToolbar.Checked         := config.ShowToolbar;
  ckDisplayToolbarLarge.Checked    := config.ShowLargeToolbar;
  ckDisplayToolbarCaptions.Checked := config.ShowToolbarCaptions;
  ckDisplayStatusbar.Checked       := config.ShowStatusbar;



  // Default drive letter
  if (config.DefaultDriveChar = #0) then begin
    cbDrive.ItemIndex := 0;
  end else begin
    cbDrive.ItemIndex := cbDrive.Items.IndexOf(config.DefaultDriveChar + ':');
  end;



end;


procedure TfrmOptions._ReadSettingsAdvanced(config: TMainSettings);
var
  owem:   eOnExitWhenMounted;
  owrp:   eOnExitWhenPortableMode;
  ondf:   eOnNormalDismountFail;
  ma:     TMountDiskType;
  ft:     TVolumeType;
  idx:    Integer;
  useIdx: Integer;
begin
  // Advanced...
  ckAllowMultipleInstances.Checked   := config.AllowMultipleInstances;
  ckAutoStartPortable.Checked        := config.AutoStartPortableMode;

  ckWarnBeforeForcedDismount.Checked := config.WarnBeforeForceDismount;

  // Populate and set action on exiting when volumes mounted
  cbOnExitWhenMounted.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for owem := low(owem) to high(owem) do begin
    Inc(idx);
    cbOnExitWhenMounted.Items.Add(OnExitWhenMountedTitle(owem));
    if (config.ExitWhenMountedAction = owem) then begin
      useIdx := idx;
    end;
  end;
  cbOnExitWhenMounted.ItemIndex := useIdx;

  // Populate and set action on exiting when in portable mode
  cbOnExitWhenPortableMode.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for owrp := low(owrp) to high(owrp) do begin
    Inc(idx);
    cbOnExitWhenPortableMode.Items.Add(OnExitWhenPortableModeTitle(owrp));
    if (config.ExitWhenPortableModeAction = owrp) then begin
      useIdx := idx;
    end;
  end;
  cbOnExitWhenPortableMode.ItemIndex := useIdx;

  // Populate and set action when normal dismount fails
  cbOnNormalDismountFail.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for ondf := low(ondf) to high(ondf) do begin
    Inc(idx);
    cbOnNormalDismountFail.Items.Add(OnNormalDismountFailTitle(ondf));
    if (config.NormalDismountFailAction = ondf) then begin
      useIdx := idx;
    end;
  end;
  cbOnNormalDismountFail.ItemIndex := useIdx;

  // Populate and set default mount type
  cbDefaultMountAs.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for ma := low(ma) to high(ma) do begin
    if (ma = fomaUnknown) then begin
      continue;
    end;

    Inc(idx);
    cbDefaultMountAs.Items.Add(FreeOTFEMountAsTitle(ma));
    if (config.DefaultMountDiskType = ma) then begin
      useIdx := idx;
    end;
  end;
  cbDefaultMountAs.ItemIndex := useIdx;

  // Populate and set drag drop filetype dropdown
  cbDragDrop.Items.Clear();
  idx    := -1;

  for ft := low(ft) to high(ft) do begin
    //only add types that can't be deected - ie not luks
  if ft in sDragDropFileType then begin
    Inc(idx);
    cbDragDrop.Items.Add(DragDropFileTypeTitle(ft));
    if (config.DefaultVolType = ft) then begin
      cbDragDrop.ItemIndex := idx;
    end;
  end;
  end;



end;


procedure TfrmOptions._InitAndReadSettings(config: TCommonSettings);
 //var
 //  ckboxIndent:  Integer;
 //  maxCBoxWidth: Integer;
begin
  inherited;

  ckLaunchAtStartup.Caption := Format(_('Start %s at system startup'), [Application.Title]);

  _InitializeGeneral;
   _ReadSettingsGeneral(config as TMainSettings);
  _InitializeHotKeys();
   _ReadSettingsHotKeys(config as TMainSettings);


  _InitializeAdvancedTab;
  _ReadSettingsAdvanced(config as TMainSettings);

  _InitializeSystemTrayOptions();
  _ReadSettingsSystemTray(config as TMainSettings);
  //  ckboxIndent  := ckLaunchMinimisedAtStartup.left - ckLaunchAtStartup.left;
  //  maxCBoxWidth := max(ckAssociateFiles.Width, ckLaunchAtStartup.Width);
  //
  //  ckAssociateFiles.Left           := ((self.Width - maxCBoxWidth) div 2);
  //  ckLaunchAtStartup.Left          := ckAssociateFiles.Left;
  //  ckLaunchMinimisedAtStartup.left := ckLaunchAtStartup.left + ckboxIndent;

  _EnableDisableControls();
end;

function TfrmOptions._DoOKClicked(): Boolean;
var
  minimisedParam: String;
begin
  Result := inherited _DoOKClicked();

  if Result then begin
    if ((ckLaunchAtStartup.Checked <> FOrigLaunchAtStartup) or
      (ckLaunchMinimisedAtStartup.Checked <> FOrigLaunchMinimisedAtStartup)) then begin
      // Purge any existing shortcut...
      SDUDeleteShortcut(
        SDU_CSIDL_STARTUP,
        Application.Title
        );

      // ...and recreate if necessary
      if ckLaunchAtStartup.Checked then begin
        minimisedParam := Ifthen(ckLaunchMinimisedAtStartup.Checked, '/' + CMDLINE_MINIMIZE, '');

        SDUCreateShortcut(
          SDU_CSIDL_STARTUP,
          Application.Title,
          ParamStr(0),
          minimisedParam,
          '',
          // ShortcutKey: TShortCut;  - not yet implemented
          wsNormal,
          // for some reason wsMinimized  doesnt work here - makes app hang. so use 'minimise' flag instead
          ''
          );
      end;

      FOrigAssociateFiles := ckAssociateFiles.Checked;
    end;

  end;

end;


end.
