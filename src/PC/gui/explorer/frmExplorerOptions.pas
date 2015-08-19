unit frmExplorerOptions;

interface

uses
  //delphi & libs
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Graphics, Messages,
  Forms, StdCtrls, SysUtils, Variants, Windows,
  //sdu & LibreCrypt utils
  SDUFilenameEdit_U, OTFEFreeOTFEDLL_U, SDUStdCtrls, CommonSettings,
  Shredder, ExplorerSettings,
  // LibreCrypt forms
  fmeAutorunOptions,
//   fmeBaseOptions,
  frmCommonOptions,
  SDUFrames, fmeVolumeSelect, Spin64;

type
  TLanguageTranslation = record
    Name:    String;
    Code:    String;
    Contact: String;
  end;
  PLanguageTranslation = ^TLanguageTranslation;

  TfrmExplorerOptions = class (TfrmCommonOptions)
    tsGeneral:     TTabSheet;
    tsAdvanced:    TTabSheet;
    tsWebDAV:      TTabSheet;
    tsAutorun:     TTabSheet;
    fmeOptions_Autorun1: TfmeAutorunOptions;
    gbAdvanced:    TGroupBox;
    lblMRUMaxItemCountInst: TLabel;
    lblMRUMaxItemCount: TLabel;
    lblOverwritePasses: TLabel;
    lblOverwriteMethod: TLabel;
    lblMoveDeletionMethod: TLabel;
    ckAdvancedMountDlg: TSDUCheckBox;
    ckRevertVolTimestamps: TSDUCheckBox;
    pnlVertSplit:  TPanel;
    seMRUMaxItemCount: TSpinEdit64;
    seOverwritePasses: TSpinEdit64;
    cbOverwriteMethod: TComboBox;
    cbMoveDeletionMethod: TComboBox;
    ckPreserveTimestampsOnStoreExtract: TSDUCheckBox;
    ckAllowTabsInPasswords: TSDUCheckBox;
    ckAllowNewlinesInPasswords: TSDUCheckBox;
    gbWebDAV:      TGroupBox;
    lblDefaultDriveLetter: TLabel;
    ckWebDAV:      TSDUCheckBox;
    cbDrive:       TComboBox;
    gbWebDAVAdvanced: TGroupBox;
    Label6:        TLabel;
    fedWebDAVLogDebug: TSDUFilenameEdit;
    fedWebDAVLogAccess: TSDUFilenameEdit;
    edWebDAVShareName: TEdit;
    ckOverwriteCacheOnDismount: TSDUCheckBox;
    ckWebDAVLogAccess: TSDUCheckBox;
    ckWebDAVLogDebug: TSDUCheckBox;
    ckExploreAfterMount: TSDUCheckBox;
    ckPromptMountSuccessful: TSDUCheckBox;
    gbGeneral:     TGroupBox;
    tlab:          TLabel;
    lblChkUpdatesFreq: TLabel;
    Label1:        TLabel;
    ckDisplayToolbar: TSDUCheckBox;
    Panel1:        TPanel;
    ckShowPasswords: TSDUCheckBox;
    cbLanguage:    TComboBox;
    pbLangDetails: TButton;
    ckDisplayToolbarLarge: TSDUCheckBox;
    ckDisplayToolbarCaptions: TSDUCheckBox;
    cbChkUpdatesFreq: TComboBox;
    ckShowHiddenItems: TSDUCheckBox;
    ckHideKnownFileExtns: TSDUCheckBox;
    ckStoreLayout: TSDUCheckBox;
    cbDefaultStoreOp: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbLangDetailsClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure ckStoreLayoutClick(Sender: TObject);
    procedure ckWebDAVClick(Sender: TObject);
  private
    FLanguages: array of TLanguageTranslation;
    FFlagClearLayoutOnSave: Boolean;
    FWarnUserChangesRequireRemount: Boolean;

    procedure _SetLanguageSelection(langCode: String);

    function GetSelectedLanguage(): TLanguageTranslation;
    function GetLanguage(idx: Integer): TLanguageTranslation;
     procedure _PopulateLanguages;
     function GetOverwriteMethod: TShredMethod;
    procedure SetOverwriteMethod(useMethod: TShredMethod);

    procedure _NudgeCheckbox(chkBox: TCheckBox);
    procedure _PopulateOverwriteMethods;
    //procs to set up/read/write tabs
    procedure _EnableDisableControlsAdvancedExplorer;
    procedure _ReadSettingsAdvancedExplorer(config: TExplorerSettings);
    procedure _InitializeAdvancedExplorer;
    procedure _WriteSettingsAdvanced(config: TExplorerSettings);

    procedure _EnableDisableControlsGeneral;
    procedure _InitializeGeneral;
    procedure _ReadSettingsGeneral(config: TExplorerSettings);
    procedure _WriteSettingsGeneral(config: TExplorerSettings);

    procedure _EnableDisableControlsWebDAV;
    procedure _InitializeWebDAV;
    procedure _ReadSettingsWebDAV(config: TExplorerSettings);
    procedure _WriteSettingsWebDAV(config: TExplorerSettings);

  protected
    procedure AllTabs_WriteSettings(config: TCommonSettings); override;
    procedure AllTabs_InitAndReadSettings(config: TCommonSettings); override;
    procedure EnableDisableControls(); override;
  public
    procedure ChangeLanguage(langCode: String); override;
  end;


implementation

{$R *.dfm}

uses
           //delphi & libs
  ShlObj,  // Required for CSIDL_PROGRAMS
           //sdu & LibreCrypt utils
  OTFEFreeOTFEBase_U,
  SDUDialogs, SDUGeneral,
  SDUi18n, lcDialogs, lcConsts, commonconsts, lcTypes
  // LibreCrypt forms
  ;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
//  CONTROL_MARGIN_LBL_TO_CONTROL = 5;

  // Vertical spacing between checkboxes, and min horizontal spacing between
  // checkbox label and the vertical separator line
  // Set to 6 for now as that looks reasonable. 10 is excessive, 5 is a bit
  // cramped
  CHKBOX_CONTROL_MARGIN = 6;

procedure TfrmExplorerOptions.AllTabs_InitAndReadSettings(
  config: TCommonSettings);
begin
  inherited;
  fmeOptions_Autorun1.Initialize;
  fmeOptions_Autorun1.ReadSettings(config);

  _InitializeAdvancedExplorer;
  _ReadSettingsAdvancedExplorer(config as TExplorerSettings);

  _InitializeWebDAV;
  _ReadSettingsWebDAV(config as TExplorerSettings);

  _InitializeGeneral;
  _ReadSettingsGeneral(config as TExplorerSettings);
end;



procedure TfrmExplorerOptions._WriteSettingsGeneral(config: TExplorerSettings);

  procedure GetGreyedCheckbox(chkBox: TCheckbox; var stateOne: Boolean; var stateTwo: Boolean);
  begin
    if (chkBox.State <> cbGrayed) then begin
      stateOne := chkBox.Checked;
      stateTwo := chkBox.Checked;
    end;
  end;

var
  useLang: TLanguageTranslation;
  dso:     TDefaultStoreOp;
  uf:      TUpdateFrequency;
begin
  // General...
  GetGreyedCheckbox(
    ckDisplayToolbar,
    config.OptShowToolbarVolume,
    config.OptShowToolbarExplorer
    );
  GetGreyedCheckbox(
    ckDisplayToolbarLarge,
    config.OptToolbarVolumeLarge,
    config.OptToolbarExplorerLarge
    );
  GetGreyedCheckbox(
    ckDisplayToolbarCaptions,
    config.OptToolbarVolumeCaptions,
    config.OptToolbarExplorerCaptions
    );

  config.OptShowPasswords      := ckShowPasswords.Checked;
  config.OptShowHiddenItems    := ckShowHiddenItems.Checked;
  config.OptHideKnownFileExtns := ckHideKnownFileExtns.Checked;
  config.OptStoreLayout        := ckStoreLayout.Checked;

  config.FlagClearLayoutOnSave := FFlagClearLayoutOnSave;

  useLang                := GetSelectedLanguage();
  config.OptLanguageCode := useLang.Code;

  // Decode default store op
  config.OptDefaultStoreOp := dsoPrompt;
  for dso := low(dso) to high(dso) do begin
    if (DefaultStoreOpTitle(dso) = cbDefaultStoreOp.Items[cbDefaultStoreOp.ItemIndex]) then begin
      config.OptDefaultStoreOp := dso;
      break;
    end;
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


procedure TfrmExplorerOptions._WriteSettingsAdvanced(config: TExplorerSettings);
var
  mdm: TMoveDeletionMethod;
begin
  // Advanced...
  config.OptAdvancedMountDlg                 := ckAdvancedMountDlg.Checked;
  config.OptRevertVolTimestamps              := ckRevertVolTimestamps.Checked;
  config.OptPreserveTimestampsOnStoreExtract := ckPreserveTimestampsOnStoreExtract.Checked;
  config.OptAllowNewlinesInPasswords         := ckAllowNewlinesInPasswords.Checked;
  config.OptAllowTabsInPasswords             := ckAllowTabsInPasswords.Checked;

  config.OptMRUList.MaxItems := seMRUMaxItemCount.Value;

  // Decode move deletion method
  config.OptMoveDeletionMethod := mdmPrompt;
  for mdm := low(mdm) to high(mdm) do begin
    if (MoveDeletionMethodTitle(mdm) = cbMoveDeletionMethod.Items[cbMoveDeletionMethod.ItemIndex])
    then begin
      config.OptMoveDeletionMethod := mdm;
      break;
    end;
  end;

  config.OptOverwriteMethod := GetOverwriteMethod();
  config.OptOverwritePasses := seOverwritePasses.Value;

end;

procedure TfrmExplorerOptions._ReadSettingsAdvancedExplorer(config: TExplorerSettings);
var
  mdm:    TMoveDeletionMethod;
  idx:    Integer;
  useIdx: Integer;
begin
  // Advanced...
  ckAdvancedMountDlg.Checked                 := config.OptAdvancedMountDlg;
  ckRevertVolTimestamps.Checked              := config.OptRevertVolTimestamps;
  ckPreserveTimestampsOnStoreExtract.Checked := config.OptPreserveTimestampsOnStoreExtract;
  ckAllowNewlinesInPasswords.Checked         := config.OptAllowNewlinesInPasswords;
  ckAllowTabsInPasswords.Checked             := config.OptAllowTabsInPasswords;

  seMRUMaxItemCount.Value := config.OptMRUList.MaxItems;

  // Populate and set move deletion method
  cbMoveDeletionMethod.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for mdm := low(mdm) to high(mdm) do begin
    Inc(idx);
    cbMoveDeletionMethod.Items.Add(MoveDeletionMethodTitle(mdm));
    if (config.OptMoveDeletionMethod = mdm) then begin
      useIdx := idx;
    end;
  end;
  cbMoveDeletionMethod.ItemIndex := useIdx;

  SetOverwriteMethod(config.OptOverwriteMethod);
  seOverwritePasses.Value := config.OptOverwritePasses;

end;



procedure TfrmExplorerOptions._PopulateOverwriteMethods();
var
  sm: TShredMethod;
begin
  cbOverwriteMethod.Items.Clear();
  for sm := low(TShredMethodTitle) to high(TShredMethodTitle) do begin
    cbOverwriteMethod.Items.Add(ShredMethodTitle(sm));
  end;
end;

procedure TfrmExplorerOptions._SetLanguageSelection(langCode: String);
var
  useIdx:   Integer;
  currLang: TLanguageTranslation;
  i:        Integer;
begin
  useIdx := 0;
  for i := 0 to (cbLanguage.items.Count - 1) do begin
    currLang := GetLanguage(i);
    if (currLang.Code = langCode) then begin
      useIdx := i;
      break;
    end;
  end;
  cbLanguage.ItemIndex := useIdx;
end;


procedure TfrmExplorerOptions.SetOverwriteMethod(useMethod: TShredMethod);
var
  i: Integer;
begin
  for i := 0 to (cbOverwriteMethod.items.Count - 1) do begin
    if (cbOverwriteMethod.items[i] = ShredMethodTitle(useMethod)) then begin
      cbOverwriteMethod.ItemIndex := i;
      break;
    end;
  end;

end;

function TfrmExplorerOptions.GetOverwriteMethod(): TShredMethod;
var
  sm: TShredMethod;
begin
  Result := smPseudorandom;

  for sm := low(sm) to high(sm) do begin
    if (cbOverwriteMethod.items[cbOverwriteMethod.ItemIndex] = ShredMethodTitle(sm)) then begin
      Result := sm;
      break;
    end;
  end;

end;

procedure TfrmExplorerOptions.AllTabs_WriteSettings(config: TCommonSettings);
begin
  inherited;
  fmeOptions_Autorun1.WriteSettings(config);
  _WriteSettingsAdvanced(config as TExplorerSettings);
  _WriteSettingsWebDAV(config as TExplorerSettings);
  _WriteSettingsGeneral(config as TExplorerSettings);
end;

procedure TfrmExplorerOptions.cbLanguageChange(Sender: TObject);
var
  useLang: TLanguageTranslation;
  langIdx: Integer;
begin
  inherited;

  // Preserve selected language; the selected language gets change by the
  // PopulateLanguages() call
  langIdx := cbLanguage.ItemIndex;

  useLang := GetSelectedLanguage();
  TfrmCommonOptions(Owner).ChangeLanguage(useLang.Code);
  // Repopulate the languages list; translation would have translated them all
  _PopulateLanguages();

  // Restore selected
  cbLanguage.ItemIndex := langIdx;

  EnableDisableControls();
end;

procedure TfrmExplorerOptions.ChangeLanguage(langCode: String);
var
  tmpConfig: TExplorerSettings;
begin
  tmpConfig := TExplorerSettings.Create();
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


procedure TfrmExplorerOptions.ckStoreLayoutClick(Sender: TObject);
begin
  inherited;

  if not (ckStoreLayout.Checked) then begin
    FFlagClearLayoutOnSave := SDUConfirmYN(Format(
      _('You have turned off the option to automatically save the window layout on exiting.' +
      SDUCRLF + SDUCRLF +
      'Would you like to reset %s to its default window layout the next time it is started?'),
      [Application.Title]));
  end;

end;

procedure TfrmExplorerOptions.ckWebDAVClick(Sender: TObject);
begin
  inherited;

  if SDUOSVistaOrLater() then begin
    SDUMessageDlg(
      RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7 + SDUCRLF + Format(
      _('Opened containers will only be mapped to drive letters when %s is run under Windows 2000/Windows XP'),
      [Application.Title]),
      mtInformation
      );
  end else
  if FWarnUserChangesRequireRemount then begin
    SDUMessageDlg(
      _('The changes you make here will only take effect when you next open a container'),
      mtInformation
      );

    FWarnUserChangesRequireRemount := False;
  end;

  EnableDisableControls();
end;
                        { TODO 1 -otdk -crefactor : whats with all the moving controls - try just using designer }

// This adjusts the width of a checkbox, and resets it caption so it
  // autosizes. If it autosizes such that it's too wide, it'll drop the width
  // and repeat
  procedure TfrmExplorerOptions._NudgeCheckbox(chkBox: TCheckBox);
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

procedure TfrmExplorerOptions._InitializeAdvancedExplorer();
const
  // Min horizontal spacing between label and control to the right of it
  LABEL_CONTROL_MARGIN = 10;

//  procedure NudgeFocusControl(lbl: TLabel);
//  begin
//    if (lbl.FocusControl <> nil) then begin
//      lbl.FocusControl.Top := lbl.Top + lbl.Height + CONTROL_MARGIN_LBL_TO_CONTROL;
//    end;
//
//  end;

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
  //  lblMRUMaxItemCount.width := (pbLangDetails.left + pbLangDetails.width) - lblMRUMaxItemCount.left;
  groupboxMargin              := ckAdvancedMountDlg.left;
  lblMRUMaxItemCount.Width    := (gbAdvanced.Width - groupboxMargin) - lblMRUMaxItemCount.left;
  lblMoveDeletionMethod.Width := (gbAdvanced.Width - groupboxMargin) - lblMoveDeletionMethod.left;
  lblOverwriteMethod.Width    := (gbAdvanced.Width - groupboxMargin) - lblOverwriteMethod.left;
  lblOverwritePasses.Width    := (gbAdvanced.Width - groupboxMargin) - lblOverwritePasses.left;

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

    stlChkBoxOrder.AddObject(Format('%.5d', [ckAdvancedMountDlg.Top]), ckAdvancedMountDlg);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckRevertVolTimestamps.Top]),
      ckRevertVolTimestamps);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckPreserveTimestampsOnStoreExtract.Top]),
      ckPreserveTimestampsOnStoreExtract);
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
      _NudgeCheckbox(currChkBox);

      YPos := currChkBox.Top;
      YPos := YPos + currChkBox.Height;
    end;

  finally
    stlChkBoxOrder.Free();
  end;

  // Nudge labels so they're as wide as can be allowed
  NudgeLabel(lblMoveDeletionMethod);
  NudgeLabel(lblOverwriteMethod);
  NudgeLabel(lblOverwritePasses);
  NudgeLabel(lblMRUMaxItemCount);
  NudgeLabel(lblMRUMaxItemCountInst);

  // Here we move controls associated with labels, such that they appear
  // underneath the label
  //  NudgeFocusControl(lblMRUMaxItemCount);
  //  NudgeFocusControl(lblMoveDeletionMethod);
  //  NudgeFocusControl(lblOverwriteMethod);
  //  NudgeFocusControl(lblOverwritePasses);

  _PopulateOverwriteMethods();

end;

procedure TfrmExplorerOptions._EnableDisableControlsAdvancedExplorer();
var
  userEnterPasses: Boolean;
begin
  inherited;

  userEnterPasses := (TShredMethodPasses[GetOverwriteMethod()] <= 0);

  SDUEnableControl(seOverwritePasses, userEnterPasses);
  if not (userEnterPasses) then begin
    seOverwritePasses.Value := TShredMethodPasses[GetOverwriteMethod()];
  end;

end;


procedure TfrmExplorerOptions.EnableDisableControls;
begin
  inherited;
  _EnableDisableControlsGeneral();
  _EnableDisableControlsAdvancedExplorer();
  _EnableDisableControlsWebDAV();
end;

procedure TfrmExplorerOptions.FormCreate(Sender: TObject);
begin
  inherited;

  // Set active page to the first one
  pcOptions.ActivePage := tsGeneral;

end;

procedure TfrmExplorerOptions.FormShow(Sender: TObject);
begin
  inherited;
  // Push the "Advanced" tab to the far end; even after the "PKCS#11" tab
  tsAdvanced.PageIndex := (pcOptions.PageCount - 1);
end;

function TfrmExplorerOptions.GetLanguage(
  idx: Integer): TLanguageTranslation;
begin
  Result := (PLanguageTranslation(cbLanguage.Items.Objects[idx]))^;
end;

procedure TfrmExplorerOptions.pbLangDetailsClick(Sender: TObject);
var
  rcdLanguage: TLanguageTranslation;
begin
  inherited;

  rcdLanguage := GetSelectedLanguage();
  SDUMessageDlg(
    Format(_('Language name: %s'), [rcdLanguage.Name]) + SDUCRLF +
    Format(_('Language code: %s'), [rcdLanguage.Code]) + SDUCRLF +
    Format(_('Translator: %s'), [rcdLanguage.Contact])
    );
end;

procedure TfrmExplorerOptions._EnableDisableControlsWebDAV();
begin
  inherited;

  SDUEnableControl(ckExploreAfterMount, ckWebDAV.Checked);
  SDUEnableControl(ckPromptMountSuccessful, ckWebDAV.Checked);
  SDUEnableControl(cbDrive, ckWebDAV.Checked);

  SDUEnableControl(gbWebDAVAdvanced, ckWebDAV.Checked);

  SDUEnableControl(fedWebDAVLogAccess,
    (ckWebDAV.Checked and ckWebDAVLogAccess.Checked));
  SDUEnableControl(fedWebDAVLogDebug, (ckWebDAV.Checked and ckWebDAVLogDebug.Checked));

  if not (ckWebDAVLogAccess.Checked) then begin
    fedWebDAVLogAccess.Filename := '';
  end;
  if not (ckWebDAVLogDebug.Checked) then begin
    fedWebDAVLogDebug.Filename := '';
  end;

end;


procedure TfrmExplorerOptions._EnableDisableControlsGeneral();
begin
  inherited;
  // Language at index 0 is "(Default)"
  SDUEnableControl(pbLangDetails, (cbLanguage.ItemIndex > 0));

  SDUEnableControl(
    ckDisplayToolbarLarge,
    (ckDisplayToolbar.Checked or (ckDisplayToolbar.State = cbGrayed))
    );
  SDUEnableControl(ckDisplayToolbarCaptions,
    (ckDisplayToolbar.Checked and ckDisplayToolbarLarge.Checked));

  // Only allow captions if the user has selected large icons
  if (ckDisplayToolbarLarge.state = cbUnchecked) then begin
    ckDisplayToolbarCaptions.Checked := False;
  end;
end;

procedure TfrmExplorerOptions._InitializeWebDAV();
var
  driveLetter: Char;
begin
  inherited;

  FWarnUserChangesRequireRemount := True;

  SDUCenterControl(gbWebDAV, ccHorizontal);
  SDUCenterControl(gbWebDAV, ccVertical, 25);

  cbDrive.Items.Clear();
  cbDrive.Items.Add(USE_DEFAULT);
  //  for driveLetter:='C' to 'Z' do
  for driveLetter := 'A' to 'Z' do
    cbDrive.Items.Add(driveLetter + ':');
end;

procedure TfrmExplorerOptions._InitializeGeneral();

//  procedure NudgeFocusControl(lbl: TLabel);
//  begin
//    if (lbl.FocusControl <> nil) then begin
//      lbl.FocusControl.Top := lbl.Top + lbl.Height + CONTROL_MARGIN_LBL_TO_CONTROL;
//    end;
//
//  end;

var
  stlChkBoxOrder: TStringList;
  YPos:           Integer;
  i:              Integer;
  currChkBox:     TCheckBox;
begin
  inherited;

  FFlagClearLayoutOnSave := False;

  SDUCenterControl(gbGeneral, ccHorizontal);
  SDUCenterControl(gbGeneral, ccVertical, 25);

  pnlVertSplit.Caption    := '';
  pnlVertSplit.bevelouter := bvLowered;
  pnlVertSplit.Width      := 3;

  _PopulateLanguages();


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

    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbar.Top]), ckDisplayToolbar);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckShowPasswords.Top]), ckShowPasswords);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarLarge.Top]),
      ckDisplayToolbarLarge);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarCaptions.Top]),
      ckDisplayToolbarCaptions);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckShowHiddenItems.Top]), ckShowHiddenItems);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckHideKnownFileExtns.Top]), ckHideKnownFileExtns);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckStoreLayout.Top]), ckStoreLayout);

    currChkBox := TCheckBox(stlChkBoxOrder.Objects[0]);
    YPos       := currChkBox.Top;
    YPos       := YPos + currChkBox.Height;
    for i := 1 to (stlChkBoxOrder.Count - 1) do begin
      currChkBox := TCheckBox(stlChkBoxOrder.Objects[i]);

      if currChkBox.Visible then begin
        currChkBox.Top := YPos + CHKBOX_CONTROL_MARGIN;

        // Sort out the checkbox's height
        _NudgeCheckbox(currChkBox);

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
  //  NudgeFocusControl(lblMRUMaxItemCount);

end;

procedure TfrmExplorerOptions._ReadSettingsWebDAV(config: TExplorerSettings);
var
  prevWarnUserChangesRequireRemount: Boolean;
begin
  // Temporarily set FWarnUserChangesRequireRemount to FALSE - otherwise we'll
  // get the warning message as the frame is loaded!
  prevWarnUserChangesRequireRemount := FWarnUserChangesRequireRemount;
  FWarnUserChangesRequireRemount    := False;
  ckWebDAV.Checked                  := config.OptWebDAVEnableServer;
  FWarnUserChangesRequireRemount    := prevWarnUserChangesRequireRemount;

  ckExploreAfterMount.Checked     := config.OptExploreAfterMount;
  ckPromptMountSuccessful.Checked := config.OptPromptMountSuccessful;

  // Default drive letter
  if (config.OptDefaultDriveLetter = #0) then
    cbDrive.ItemIndex := 0
  else
    cbDrive.ItemIndex := cbDrive.Items.IndexOf(config.OptDefaultDriveLetter + ':');


  ckOverwriteCacheOnDismount.Checked := config.OptOverwriteWebDAVCacheOnDismount;
  edWebDAVShareName.Text             := config.OptWebDavShareName;
  fedWebDAVLogAccess.Filename        := config.OptWebDavLogAccess;
  fedWebDAVLogDebug.Filename         := config.OptWebDavLogDebug;

  ckWebDAVLogAccess.Checked := (trim(fedWebDAVLogAccess.Filename) <> '');
  ckWebDAVLogDebug.Checked  := (trim(fedWebDAVLogDebug.Filename) <> '');

end;


procedure TfrmExplorerOptions._ReadSettingsGeneral(config: TExplorerSettings);

  procedure SetupGreyedCheckbox(chkBox: TCheckbox; stateOne: Boolean; stateTwo: Boolean);
  begin
    if (stateOne = stateTwo) then begin
      chkBox.Checked := stateOne;
    end else begin
      chkBox.State := cbGrayed;
    end;
    chkBox.Tag := Ord(chkBox.State);
  end;

var
  uf:     TUpdateFrequency;
  dso:    TDefaultStoreOp;
  idx:    Integer;
  useIdx: Integer;
begin
  // General...
  SetupGreyedCheckbox(
    ckDisplayToolbar,
    config.OptShowToolbarVolume,
    config.OptShowToolbarExplorer
    );
  SetupGreyedCheckbox(
    ckDisplayToolbarLarge,
    config.OptToolbarVolumeLarge,
    config.OptToolbarExplorerLarge
    );
  SetupGreyedCheckbox(
    ckDisplayToolbarCaptions,
    config.OptToolbarVolumeCaptions,
    config.OptToolbarExplorerCaptions
    );

  ckShowPasswords.Checked      := config.OptShowPasswords;
  ckShowHiddenItems.Checked    := config.OptShowHiddenItems;
  ckHideKnownFileExtns.Checked := config.OptHideKnownFileExtns;
  ckStoreLayout.Checked        := config.OptStoreLayout;

  // In case language code not found; reset to "(Default)" entry; at index 0
  _SetLanguageSelection(config.OptLanguageCode);

  // Populate and set default store op dropdown
  cbDefaultStoreOp.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for dso := low(dso) to high(dso) do begin
    Inc(idx);
    cbDefaultStoreOp.Items.Add(DefaultStoreOpTitle(dso));
    if (config.OptDefaultStoreOp = dso) then begin
      useIdx := idx;
    end;
  end;
  cbDefaultStoreOp.ItemIndex := useIdx;

  // Populate and set update frequency dropdown
  cbChkUpdatesFreq.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for uf := low(uf) to high(uf) do begin
    // Daily and weekly disabled for now; not sure what the load on the
    // server would be like

    Inc(idx);
    cbChkUpdatesFreq.Items.Add(UpdateFrequencyTitle(uf));
    if (config.OptUpdateChkFrequency = uf) then begin
      useIdx := idx;
    end;
  end;
  cbChkUpdatesFreq.ItemIndex := useIdx;

end;

procedure TfrmExplorerOptions._WriteSettingsWebDAV(config: TExplorerSettings);
begin
  config.OptWebDAVEnableServer := ckWebDAV.Checked;

  config.OptExploreAfterMount     := ckExploreAfterMount.Checked;
  config.OptPromptMountSuccessful := ckPromptMountSuccessful.Checked;

  // Default drive letter
  if (cbDrive.ItemIndex = 0) then begin
    config.OptDefaultDriveLetter := #0;
  end else begin
    config.OptDefaultDriveLetter := DriveLetterChar(cbDrive.Items[cbDrive.ItemIndex][1]);
  end;

  config.OptOverwriteWebDAVCacheOnDismount := ckOverwriteCacheOnDismount.Checked;
  config.OptWebDavShareName                := edWebDAVShareName.Text;
  config.OptWebDavLogDebug                 := fedWebDAVLogDebug.Filename;
  config.OptWebDavLogAccess                := fedWebDAVLogAccess.Filename;

end;

procedure TfrmExplorerOptions._PopulateLanguages();
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

function TfrmExplorerOptions.GetSelectedLanguage: TLanguageTranslation;
begin
  Result := GetLanguage(cbLanguage.ItemIndex);
end;

end.
