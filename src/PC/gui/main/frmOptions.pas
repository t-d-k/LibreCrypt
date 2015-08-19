unit frmOptions;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  //delphi & libs
  ExtCtrls, Forms, Classes, ComCtrls, StdCtrls, SysUtils, Windows, Graphics,
  Messages, Controls, Dialogs,

  //sdu & LibreCrypt utils
  CommonSettings, OTFEFreeOTFE_U, SDUStdCtrls, MainSettings, SDUFilenameEdit_U,

  // LibreCrypt forms
  fmeAutorunOptions,
  frmCommonOptions,
//  fmeAdvancedOptions, fmeSystemTrayOptions,
//   fmeLcOptions,
//   fmeBaseOptions,
    fmeVolumeSelect, SDUForms, SDUFrames,
  Spin64;

type


  TLanguageTranslation = record
    Name:    String;
    Code:    String;
    Contact: String;
  end;
  PLanguageTranslation = ^TLanguageTranslation;
  TfrmOptions = class (TfrmCommonOptions)
    tsGeneral:        TTabSheet;
    tsHotkeys:        TTabSheet;
    tcSystemTray:     TTabSheet;
    tsAutorun:        TTabSheet;
    tsAdvanced:       TTabSheet;
    fmeOptions_Autorun1: TfmeAutorunOptions;
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
    lblMRUMaxItemCountInst: TLabel;
    lblMRUMaxItemCount: TLabel;
    lblOnNormalDismountFail: TLabel;
    lblDefaultMountAs: TLabel;
    lblOnExitWhenMounted: TLabel;
    lblOnExitWhenPortableMode: TLabel;
    ckAllowMultipleInstances: TSDUCheckBox;
    cbDragDrop: TComboBox;
    ckAutoStartPortable: TSDUCheckBox;
    ckAdvancedMountDlg: TSDUCheckBox;
    ckRevertVolTimestamps: TSDUCheckBox;
    pnlVertSplit: TPanel;
    seMRUMaxItemCount: TSpinEdit64;
    ckWarnBeforeForcedDismount: TSDUCheckBox;
    cbOnNormalDismountFail: TComboBox;
    cbDefaultMountAs: TComboBox;
    cbOnExitWhenMounted: TComboBox;
    cbOnExitWhenPortableMode: TComboBox;
    ckAllowNewlinesInPasswords: TSDUCheckBox;
    ckAllowTabsInPasswords: TSDUCheckBox;
    gbGeneral: TGroupBox;
    lblDefaultDriveLetter: TLabel;
    lblLanguage: TLabel;
    lblChkUpdatesFreq: TLabel;
    ckDisplayToolbar: TSDUCheckBox;
    ckDisplayStatusbar: TSDUCheckBox;
    ckExploreAfterMount: TSDUCheckBox;
    Panel1: TPanel;
    cbDrive: TComboBox;
    ckShowPasswords: TSDUCheckBox;
    cbLanguage: TComboBox;
    pbLangDetails: TButton;
    ckPromptMountSuccessful: TSDUCheckBox;
    ckDisplayToolbarLarge: TSDUCheckBox;
    ckDisplayToolbarCaptions: TSDUCheckBox;
    cbChkUpdatesFreq: TComboBox;
    gbSystemTrayIcon: TGroupBox;
    ckUseSystemTrayIcon: TSDUCheckBox;
    ckMinToIcon: TSDUCheckBox;
    ckCloseToIcon: TSDUCheckBox;
    gbClickActions: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    rbSingleClick: TRadioButton;
    rbDoubleClick: TRadioButton;
    cbClickAction: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ckLaunchAtStartupClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure pbLangDetailsClick(Sender: TObject);
  private
      FLanguages: array of TLanguageTranslation;

    procedure _InitializeHotKeys;
    procedure _WriteSettingsHotKeys(config: TMainSettings);
    procedure _ReadSettingsHotKeys(config: TMainSettings);
    procedure _EnableDisableControlsHotKeys;
    procedure _InitializeAdvancedTab;
    procedure _ReadSettingsAdvanced(config: TMainSettings);
    procedure _WriteSettingsAdvanced(config: TMainSettings);
    function _LanguageControlLanguage(idx: Integer): TLanguageTranslation;
    function _SelectedLanguage: TLanguageTranslation;
    procedure _PopulateLanguages;
    procedure _SetLanguageSelection(langCode: String);
    procedure _EnableDisableControlsGeneral;
    procedure _InitializeGeneral;
    procedure _ReadSettingsGeneral(config: TMainSettings);
    procedure _WriteSettingsGeneral(config: TMainSettings);
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

    procedure EnableDisableControls(); override;

    procedure AllTabs_InitAndReadSettings(config: TCommonSettings); override;
    procedure AllTabs_WriteSettings(config: TCommonSettings); override;

    function DoOKClicked(): Boolean; override;

  public

    procedure ChangeLanguage(langCode: String); override;
  end;

const
  CONTROL_MARGIN_LBL_TO_CONTROL = 5;

implementation

{$R *.DFM}

uses
  // delphi

  Math,
  ShlObj,  // Required for CSIDL_PROGRAMS
  StrUtils,
  // sdu /librecrypt
  OTFEFreeOTFEBase_U,
  SDUDialogs, SDUGeneral, lcTypes, CommonConsts,lcDialogs, lcConsts,
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
  config.OptHKeyEnableDismount      := ckHotkeyDismount.Checked;
  config.OptHKeyKeyDismount         := hkDismount.HotKey;
  config.OptHKeyEnableDismountEmerg := ckHotkeyDismountEmerg.Checked;
  config.OptHKeyKeyDismountEmerg    := hkDismountEmerg.HotKey;
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
  config.OptSystemTrayIconDisplay := ckUseSystemTrayIcon.Checked;
  config.OptSystemTrayIconMinTo   := ckMinToIcon.Checked;
  config.OptSystemTrayIconCloseTo := ckCloseToIcon.Checked;

  if rbSingleClick.Checked then begin
    config.OptSystemTrayIconActionSingleClick := _GetClickAction(cbClickAction);
    config.OptSystemTrayIconActionDoubleClick := stcaDoNothing;
  end else begin
    config.OptSystemTrayIconActionSingleClick := stcaDoNothing;
    config.OptSystemTrayIconActionDoubleClick := _GetClickAction(cbClickAction);
  end;

end;

procedure TfrmOptions._WriteSettingsAdvanced(config: TMainSettings);
var
  owem: eOnExitWhenMounted;
  owrp: eOnExitWhenPortableMode;
  ondf: eOnNormalDismountFail;
  ma:   TFreeOTFEMountAs;
  ft:   TVolumeType;
begin
  // Advanced...
  config.OptAllowMultipleInstances   := ckAllowMultipleInstances.Checked;
  config.OptAutoStartPortable        := ckAutoStartPortable.Checked;
  config.OptAdvancedMountDlg         := ckAdvancedMountDlg.Checked;
  config.OptRevertVolTimestamps      := ckRevertVolTimestamps.Checked;
  config.OptWarnBeforeForcedDismount := ckWarnBeforeForcedDismount.Checked;
  config.OptAllowNewlinesInPasswords := ckAllowNewlinesInPasswords.Checked;
  config.OptAllowTabsInPasswords     := ckAllowTabsInPasswords.Checked;

  // Decode action on exiting when volumes mounted
  config.OptOnExitWhenMounted := oewmPromptUser;
  for owem := low(owem) to high(owem) do begin
    if (OnExitWhenMountedTitle(owem) = cbOnExitWhenMounted.Items[cbOnExitWhenMounted.ItemIndex])
    then begin
      config.OptOnExitWhenMounted := owem;
      break;
    end;
  end;

  // Decode action on exiting when in portable mode
  config.OptOnExitWhenPortableMode := oewpPromptUser;
  for owrp := low(owrp) to high(owrp) do begin
    if (OnExitWhenPortableModeTitle(owrp) =
      cbOnExitWhenPortableMode.Items[cbOnExitWhenPortableMode.ItemIndex]) then begin
      config.OptOnExitWhenPortableMode := owrp;
      break;
    end;
  end;

  // Decode action when normal dismount fails
  config.OptOnNormalDismountFail := ondfPromptUser;
  for ondf := low(ondf) to high(ondf) do begin
    if (OnNormalDismountFailTitle(ondf) =
      cbOnNormalDismountFail.Items[cbOnNormalDismountFail.ItemIndex]) then begin
      config.OptOnNormalDismountFail := ondf;
      break;
    end;
  end;

  // Decode default mount type
  config.OptDefaultMountAs := fomaRemovableDisk;
  for ma := low(ma) to high(ma) do begin
    if (FreeOTFEMountAsTitle(ma) = cbDefaultMountAs.Items[cbDefaultMountAs.ItemIndex]) then begin
      config.OptDefaultMountAs := ma;
      break;
    end;
  end;

  // Decode drag drop filetype
  config.OptDragDropFileType := vtUnknown;
  for ft := low(ft) to high(ft) do begin
    if (DragDropFileTypeTitle(ft) = cbDragDrop.Items[cbDragDrop.ItemIndex]) then begin
      config.OptDragDropFileType := ft;
      break;
    end;
  end;

  config.OptMRUList.MaxItems := seMRUMaxItemCount.Value;

end;

procedure TfrmOptions.AllTabs_WriteSettings(config: TCommonSettings);
begin
  inherited;
  _WriteSettingsGeneral(config as TMainSettings);
  _WriteSettingsHotKeys(config as TMainSettings);
  fmeOptions_Autorun1.WriteSettings(config);
  _WriteSettingsAdvanced(config as TMainSettings);
  _WriteSettingsSystemTray(config as TMainSettings);
end;

function TfrmOptions._SelectedLanguage(): TLanguageTranslation;
begin
  Result := _LanguageControlLanguage(cbLanguage.ItemIndex);

end;


procedure TfrmOptions._PopulateLanguages();
var
  i:            Integer;
  origLangCode: String;
  langCodes:    TStringList;
  sortedList:   TStringList;
begin
  // Store language information for later use...
  langCodes := TStringList.Create();
  try
    // Get all language codes...
    SDUGetLanguageCodes(langCodes);

    // +1 to include "Default"
    SetLength(FLanguages, langCodes.Count);

    // Spin though the languages, getting their corresponding human-readable
    // names
    origLangCode := SDUGetCurrentLanguageCode();
    try
      for i := 0 to (langCodes.Count - 1) do begin
        SDUSetLanguage(langCodes[i]);

        FLanguages[i].Code    := langCodes[i];
        FLanguages[i].Name    := _(CONST_LANGUAGE_ENGLISH);
        FLanguages[i].Contact := SDUGetTranslatorNameAndEmail();

        // Force set contact details for English version; the dxgettext software sets
        // this to some stupid default
        if (langCodes[i] = ISO639_ALPHA2_ENGLISH) then begin
          FLanguages[i].Contact := ENGLISH_TRANSLATION_CONTACT;
        end;

      end;
    finally
      // Flip back to original language...
      SDUSetLanguage(origLangCode);
    end;

  finally
    langCodes.Free();
  end;

  // Add "default" into the list
  SetLength(FLanguages, (length(FLanguages) + 1));
  FLanguages[length(FLanguages) - 1].Code    := '';
  FLanguages[length(FLanguages) - 1].Name    := _('(Default)');
  FLanguages[length(FLanguages) - 1].Contact := '';

  // Populate list
  sortedList := TStringList.Create();
  try
    for i := 0 to (length(FLanguages) - 1) do begin
      sortedList.AddObject(FLanguages[i].Name, @(FLanguages[i]));
    end;

    sortedList.Sorted := True;
    sortedList.Sorted := False;

    cbLanguage.Items.Assign(sortedList)
  finally
    sortedList.Free();
  end;

end;

procedure TfrmOptions._SetLanguageSelection(langCode: String);
var
  useIdx:   Integer;
  currLang: TLanguageTranslation;
  i:        Integer;
begin
  useIdx := 0;
  for i := 0 to (cbLanguage.items.Count - 1) do begin
    currLang := _LanguageControlLanguage(i);
    if (currLang.Code = langCode) then begin
      useIdx := i;
      break;
    end;
  end;
  cbLanguage.ItemIndex := useIdx;
end;

function TfrmOptions._LanguageControlLanguage(idx: Integer): TLanguageTranslation;
begin
  Result := (PLanguageTranslation(cbLanguage.Items.Objects[idx]))^;

end;

procedure TfrmOptions.pbLangDetailsClick(Sender: TObject);
var
  rcdLanguage: TLanguageTranslation;
begin
  inherited;

  rcdLanguage := _SelectedLanguage();
  SDUMessageDlg(
    Format(_('Language name: %s'), [rcdLanguage.Name]) + SDUCRLF +
    Format(_('Language code: %s'), [rcdLanguage.Code]) + SDUCRLF +
    Format(_('Translator: %s'), [rcdLanguage.Contact])
    );
end;


procedure TfrmOptions.cbLanguageChange(Sender: TObject);
var
  useLang: TLanguageTranslation;
  langIdx: Integer;
begin
  inherited;

  // Preserve selected language; the selected language gets change by the
  // PopulateLanguages() call
  langIdx := cbLanguage.ItemIndex;

  useLang := _SelectedLanguage();
  TfrmCommonOptions(Owner).ChangeLanguage(useLang.Code);
  // Repopulate the languages list; translation would have translated them all
  _PopulateLanguages();

  // Restore selected
  cbLanguage.ItemIndex := langIdx;

  EnableDisableControls();
end;


procedure TfrmOptions.ChangeLanguage(langCode: String);
var
  tmpConfig: TMainSettings;
begin
  tmpConfig := TMainSettings.Create();
  try
    tmpConfig.Assign(GetSettings());
    AllTabs_WriteSettings(tmpConfig);

    SDUSetLanguage(langCode);
    try
      SDURetranslateComponent(self);
    except
      on E: Exception do begin
        SDUTranslateComponent(self);
      end;
    end;

    AllTabs_InitAndReadSettings(tmpConfig);

  finally
    tmpConfig.Free();
  end;

  // Call EnableDisableControls() as this re-jigs the "Save above settings to:"
  // label
  EnableDisableControls();
end;



procedure TfrmOptions.ckLaunchAtStartupClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfrmOptions.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
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

procedure TfrmOptions.EnableDisableControls();
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
  procedure NudgeCheckbox(chkBox: TCheckBox);
  var
    tmpCaption:     String;
    maxWidth:       Integer;
    useWidth:       Integer;
    lastTriedWidth: Integer;
  begin
    tmpCaption := chkBox.Caption;

    maxWidth := (pnlVertSplit.left - CHKBOX_CONTROL_MARGIN) - chkBox.Left;
    useWidth := maxWidth;

    chkBox.Caption := 'X';
    chkBox.Width   := useWidth;
    lastTriedWidth := useWidth;
    chkBox.Caption := tmpCaption;
    while ((chkBox.Width > maxWidth) and (lastTriedWidth > 0)) do begin
      // 5 used here; just needs to be something sensible to reduce the
      // width by; 1 would do pretty much just as well
      useWidth := useWidth - 5;

      chkBox.Caption := 'X';
      chkBox.Width   := useWidth;
      lastTriedWidth := useWidth;
      chkBox.Caption := tmpCaption;
    end;

  end;

  procedure NudgeFocusControl(lbl: TLabel);
  begin
    if (lbl.FocusControl <> nil) then begin
      lbl.FocusControl.Top := lbl.Top + lbl.Height + CONTROL_MARGIN_LBL_TO_CONTROL;
    end;

  end;

  procedure NudgeLabel(lbl: TLabel);
  var
    maxWidth: Integer;
  begin
    if (pnlVertSplit.left > lbl.left) then begin
      maxWidth := (pnlVertSplit.left - LABEL_CONTROL_MARGIN) - lbl.left;
    end else begin
      maxWidth := (lbl.Parent.Width - LABEL_CONTROL_MARGIN) - lbl.left;
    end;

    lbl.Width := maxWidth;
  end;

var
  stlChkBoxOrder: TStringList;
  YPos:           Integer;
  i:              Integer;
  currChkBox:     TCheckBox;
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

  pnlVertSplit.Caption    := '';
  pnlVertSplit.bevelouter := bvLowered;
  pnlVertSplit.Width      := 3;

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
  stlChkBoxOrder := TStringList.Create();
  try
    // stlChkBoxOrder is used to order the checkboxes in their vertical order;
    // this allows checkboxes to be added into the list below in *any* order,
    // and it'll still work
    stlChkBoxOrder.Sorted := True;

    stlChkBoxOrder.AddObject(Format('%.5d', [ckAllowMultipleInstances.Top]),
      ckAllowMultipleInstances);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckAutoStartPortable.Top]), ckAutoStartPortable);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckAdvancedMountDlg.Top]), ckAdvancedMountDlg);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckRevertVolTimestamps.Top]),
      ckRevertVolTimestamps);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckWarnBeforeForcedDismount.Top]),
      ckWarnBeforeForcedDismount);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckAllowNewlinesInPasswords.Top]),
      ckAllowNewlinesInPasswords);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckAllowTabsInPasswords.Top]),
      ckAllowTabsInPasswords);

    currChkBox := TCheckBox(stlChkBoxOrder.Objects[0]);
    YPos       := currChkBox.Top;
    YPos       := YPos + currChkBox.Height;
    for i := 1 to (stlChkBoxOrder.Count - 1) do begin
      currChkBox := TCheckBox(stlChkBoxOrder.Objects[i]);

      currChkBox.Top := YPos + CHKBOX_CONTROL_MARGIN;

      // Sort out the checkbox's height
      NudgeCheckbox(currChkBox);

      YPos := currChkBox.Top;
      YPos := YPos + currChkBox.Height;
    end;

  finally
    stlChkBoxOrder.Free();
  end;

  // Nudge labels so they're as wide as can be allowed
  NudgeLabel(lblOnExitWhenMounted);
  NudgeLabel(lblOnExitWhenPortableMode);
  NudgeLabel(lblOnNormalDismountFail);
  NudgeLabel(lblDragDrop);
  NudgeLabel(lblMRUMaxItemCount);
  NudgeLabel(lblMRUMaxItemCountInst);
  NudgeLabel(lblDefaultMountAs);

  // Here we move controls associated with labels, such that they appear
  // underneath the label
  //  NudgeFocusControl(lblLanguage);
  NudgeFocusControl(lblDragDrop);
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
  procedure NudgeCheckbox(chkBox: TCheckBox);
  var
    tmpCaption:     String;
    maxWidth:       Integer;
    useWidth:       Integer;
    lastTriedWidth: Integer;
  begin
    tmpCaption := chkBox.Caption;

    maxWidth := (pnlVertSplit.left - CHKBOX_CONTROL_MARGIN) - chkBox.Left;
    useWidth := maxWidth;

    chkBox.Caption := 'X';
    chkBox.Width   := useWidth;
    lastTriedWidth := useWidth;
    chkBox.Caption := tmpCaption;
    while ((chkBox.Width > maxWidth) and (lastTriedWidth > 0)) do begin
      // 5 used here; just needs to be something sensible to reduce the
      // width by; 1 would do pretty much just as well
      useWidth := useWidth - 5;

      chkBox.Caption := 'X';
      chkBox.Width   := useWidth;
      lastTriedWidth := useWidth;
      chkBox.Caption := tmpCaption;
    end;

  end;

  procedure NudgeFocusControl(lbl: TLabel);
  begin
    if (lbl.FocusControl <> nil) then begin
      lbl.FocusControl.Top := lbl.Top + lbl.Height + CONTROL_MARGIN_LBL_TO_CONTROL;
    end;

  end;

var
  driveLetter:    Char;
  stlChkBoxOrder: TStringList;
  YPos:           Integer;
  i:              Integer;
  currChkBox:     TCheckBox;
begin
  inherited;

  SDUCenterControl(gbGeneral, ccHorizontal);
  SDUCenterControl(gbGeneral, ccVertical, 25);

  pnlVertSplit.Caption    := '';
  pnlVertSplit.bevelouter := bvLowered;
  pnlVertSplit.Width      := 3;

  _PopulateLanguages();

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
  stlChkBoxOrder := TStringList.Create();
  try
    // stlChkBoxOrder is used to order the checkboxes in their vertical order;
    // this allows checkboxes to be added into the list below in *any* order,
    // and it'll still work
    stlChkBoxOrder.Sorted := True;

    stlChkBoxOrder.AddObject(Format('%.5d', [ckExploreAfterMount.Top]), ckExploreAfterMount);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbar.Top]), ckDisplayToolbar);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarLarge.Top]),
      ckDisplayToolbarLarge);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarCaptions.Top]),
      ckDisplayToolbarCaptions);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayStatusbar.Top]), ckDisplayStatusbar);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckShowPasswords.Top]), ckShowPasswords);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckPromptMountSuccessful.Top]),
      ckPromptMountSuccessful);

    currChkBox := TCheckBox(stlChkBoxOrder.Objects[0]);
    YPos       := currChkBox.Top;
    YPos       := YPos + currChkBox.Height;
    for i := 1 to (stlChkBoxOrder.Count - 1) do begin
      currChkBox := TCheckBox(stlChkBoxOrder.Objects[i]);

      if currChkBox.Visible then begin
        currChkBox.Top := YPos + CHKBOX_CONTROL_MARGIN;

        // Sort out the checkbox's height
        NudgeCheckbox(currChkBox);

        YPos := currChkBox.Top;
        YPos := YPos + currChkBox.Height;
      end;
    end;

  finally
    stlChkBoxOrder.Free();
  end;

  // Here we move controls associated with labels, such that they appear
  // underneath the label
  //  NudgeFocusControl(lblLanguage);
  //  NudgeFocusControl(lblDefaultDriveLetter);
  //  NudgeFocusControl(lblMRUMaxItemCount);

end;

procedure TfrmOptions._ReadSettingsHotKeys(config: TMainSettings);
begin
  // Hotkeys...
  ckHotkeyDismount.Checked      := config.OptHKeyEnableDismount;
  hkDismount.HotKey             := config.OptHKeyKeyDismount;
  ckHotkeyDismountEmerg.Checked := config.OptHKeyEnableDismountEmerg;
  hkDismountEmerg.HotKey        := config.OptHKeyKeyDismountEmerg;
end;



procedure TfrmOptions._ReadSettingsSystemTray(config: TMainSettings);
begin
  // System tray icon related...
  ckUseSystemTrayIcon.Checked := config.OptSystemTrayIconDisplay;
  ckMinToIcon.Checked         := config.OptSystemTrayIconMinTo;
  ckCloseToIcon.Checked       := config.OptSystemTrayIconCloseTo;

  if (config.OptSystemTrayIconActionSingleClick <> stcaDoNothing) then begin
    rbSingleClick.Checked := True;
    _PopulateAndSetClickAction(cbClickAction, config.OptSystemTrayIconActionSingleClick);
  end else begin
    rbDoubleClick.Checked := True;
    _PopulateAndSetClickAction(cbClickAction, config.OptSystemTrayIconActionDoubleClick);
  end;

end;


procedure TfrmOptions._ReadSettingsGeneral(config: TMainSettings);
var
  uf:     TUpdateFrequency;
  idx:    Integer;
  useIdx: Integer;
begin
  // General...
  ckExploreAfterMount.Checked      := config.OptExploreAfterMount;
  ckDisplayToolbar.Checked         := config.OptDisplayToolbar;
  ckDisplayToolbarLarge.Checked    := config.OptDisplayToolbarLarge;
  ckDisplayToolbarCaptions.Checked := config.OptDisplayToolbarCaptions;
  ckDisplayStatusbar.Checked       := config.OptDisplayStatusbar;
  ckShowPasswords.Checked          := config.OptShowPasswords;

  ckPromptMountSuccessful.Checked := config.OptPromptMountSuccessful;

  // In case language code not found; reset to "(Default)" entry; at index 0
  _SetLanguageSelection(config.OptLanguageCode);

  // Default drive letter
  if (config.OptDefaultDriveLetter = #0) then begin
    cbDrive.ItemIndex := 0;
  end else begin
    cbDrive.ItemIndex := cbDrive.Items.IndexOf(config.OptDefaultDriveLetter + ':');
  end;

  // Populate and set update frequency dropdown
  cbChkUpdatesFreq.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for uf := low(uf) to high(uf) do begin

    Inc(idx);
    cbChkUpdatesFreq.Items.Add(UpdateFrequencyTitle(uf));
    if (config.OptUpdateChkFrequency = uf) then begin
      useIdx := idx;
    end;
  end;
  cbChkUpdatesFreq.ItemIndex := useIdx;

end;


procedure TfrmOptions._WriteSettingsGeneral(config: TMainSettings);
var
  uf:      TUpdateFrequency;
  useLang: TLanguageTranslation;
begin
  // General...
  config.OptExploreAfterMount      := ckExploreAfterMount.Checked;
  config.OptDisplayToolbar         := ckDisplayToolbar.Checked;
  config.OptDisplayToolbarLarge    := ckDisplayToolbarLarge.Checked;
  config.OptDisplayToolbarCaptions := ckDisplayToolbarCaptions.Checked;
  config.OptDisplayStatusbar       := ckDisplayStatusbar.Checked;
  config.OptShowPasswords          := ckShowPasswords.Checked;

  config.OptPromptMountSuccessful := ckPromptMountSuccessful.Checked;

  useLang                := _SelectedLanguage();
  config.OptLanguageCode := useLang.Code;

  // Default drive letter
  if (cbDrive.ItemIndex = 0) then begin
    config.OptDefaultDriveLetter := #0;
  end else begin
    config.OptDefaultDriveLetter := DriveLetterChar(cbDrive.Items[cbDrive.ItemIndex][1]);
  end;

  // Decode update frequency
  config.OptUpdateChkFrequency := ufNever;
  for uf := low(uf) to high(uf) do begin
    if (UpdateFrequencyTitle(uf) = cbChkUpdatesFreq.Items[cbChkUpdatesFreq.ItemIndex]) then begin
      config.OptUpdateChkFrequency := uf;
      break;
    end;
  end;

end;


procedure TfrmOptions._ReadSettingsAdvanced(config: TMainSettings);
var
  owem:   eOnExitWhenMounted;
  owrp:   eOnExitWhenPortableMode;
  ondf:   eOnNormalDismountFail;
  ma:     TFreeOTFEMountAs;
  ft:     TVolumeType;
  idx:    Integer;
  useIdx: Integer;
begin
  // Advanced...
  ckAllowMultipleInstances.Checked   := config.OptAllowMultipleInstances;
  ckAutoStartPortable.Checked        := config.OptAutoStartPortable;
  ckAdvancedMountDlg.Checked         := config.OptAdvancedMountDlg;
  ckRevertVolTimestamps.Checked      := config.OptRevertVolTimestamps;
  ckWarnBeforeForcedDismount.Checked := config.OptWarnBeforeForcedDismount;
  ckAllowNewlinesInPasswords.Checked := config.OptAllowNewlinesInPasswords;
  ckAllowTabsInPasswords.Checked     := config.OptAllowTabsInPasswords;

  // Populate and set action on exiting when volumes mounted
  cbOnExitWhenMounted.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for owem := low(owem) to high(owem) do begin
    Inc(idx);
    cbOnExitWhenMounted.Items.Add(OnExitWhenMountedTitle(owem));
    if (config.OptOnExitWhenMounted = owem) then begin
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
    if (config.OptOnExitWhenPortableMode = owrp) then begin
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
    if (config.OptOnNormalDismountFail = ondf) then begin
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
    if (config.OptDefaultMountAs = ma) then begin
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
    if (config.OptDragDropFileType = ft) then begin
      cbDragDrop.ItemIndex := idx;
    end;
  end;
  end;

  seMRUMaxItemCount.Value := config.OptMRUList.MaxItems;

end;


procedure TfrmOptions.AllTabs_InitAndReadSettings(config: TCommonSettings);
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
   fmeOptions_Autorun1.Initialize;
  fmeOptions_Autorun1.ReadSettings(config);

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

  EnableDisableControls();
end;

function TfrmOptions.DoOKClicked(): Boolean;
var
  minimisedParam: String;
begin
  Result := inherited DoOKClicked();

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
