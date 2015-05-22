unit fmeGeneralOptions;
{TODO: this is only used in TfrmOptions, so should be part of that, not a seperate frame}
interface

uses
  Classes, fmeBaseOptions,
  Controls, Dialogs, ExtCtrls, Forms,
  fmeLcOptions, MainSettings, Graphics, Messages, SDUStdCtrls, Spin64,
  StdCtrls, SysUtils, Variants, Windows;

type
  TLanguageTranslation = record
    Name:    String;
    Code:    String;
    Contact: String;
  end;
  PLanguageTranslation = ^TLanguageTranslation;

  TfmeGeneralOptions = class (TfmeLcOptions)
    gbGeneral:                TGroupBox;
    ckDisplayToolbar:         TSDUCheckBox;
    ckDisplayStatusbar:       TSDUCheckBox;
    ckExploreAfterMount:      TSDUCheckBox;
    pnlVertSplit:             TPanel;
    cbDrive:                  TComboBox;
    lblDefaultDriveLetter:    TLabel;
    ckShowPasswords:          TSDUCheckBox;
    cbLanguage:               TComboBox;
    lblLanguage:              TLabel;
    pbLangDetails:            TButton;
    ckPromptMountSuccessful:  TSDUCheckBox;
    ckDisplayToolbarLarge:    TSDUCheckBox;
    ckDisplayToolbarCaptions: TSDUCheckBox;
    cbChkUpdatesFreq:         TComboBox;
    lblChkUpdatesFreq:        TLabel;
    procedure pbLangDetailsClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
  PRIVATE
    FLanguages: array of TLanguageTranslation;

    procedure PopulateLanguages();
    procedure SetLanguageSelection(langCode: String);

    function SelectedLanguage(): TLanguageTranslation;
    function LanguageControlLanguage(idx: Integer): TLanguageTranslation;
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
  CommonConsts,
  frmCommonOptions, CommonSettings,
  OTFE_U,
  lcDialogs,
  SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
  CONTROL_MARGIN_LBL_TO_CONTROL = 5;

procedure TfmeGeneralOptions.cbLanguageChange(Sender: TObject);
var
  useLang: TLanguageTranslation;
  langIdx: Integer;
begin
  inherited;

  // Preserve selected language; the selected language gets change by the
  // PopulateLanguages() call
  langIdx := cbLanguage.ItemIndex;

  useLang := SelectedLanguage();
  TfrmCommonOptions(Owner).ChangeLanguage(useLang.Code);
  // Repopulate the languages list; translation would have translated them all
  PopulateLanguages();

  // Restore selected
  cbLanguage.ItemIndex := langIdx;

  EnableDisableControls();
end;

procedure TfmeGeneralOptions.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeGeneralOptions.EnableDisableControls();
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

procedure TfmeGeneralOptions.Initialize();
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

  PopulateLanguages();

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

procedure TfmeGeneralOptions.pbLangDetailsClick(Sender: TObject);
var
  rcdLanguage: TLanguageTranslation;
begin
  inherited;

  rcdLanguage := SelectedLanguage();
  SDUMessageDlg(
    SDUParamSubstitute(_('Language name: %1'), [rcdLanguage.Name]) + SDUCRLF +
    SDUParamSubstitute(_('Language code: %1'), [rcdLanguage.Code]) + SDUCRLF +
    SDUParamSubstitute(_('Translator: %1'), [rcdLanguage.Contact])
    );

end;

procedure TfmeGeneralOptions._ReadSettings(config: TMainSettings);
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
  SetLanguageSelection(config.OptLanguageCode);

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


procedure TfmeGeneralOptions._WriteSettings(config: TMainSettings);
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

  useLang                := SelectedLanguage();
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

procedure TfmeGeneralOptions.PopulateLanguages();
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

procedure TfmeGeneralOptions.SetLanguageSelection(langCode: String);
var
  useIdx:   Integer;
  currLang: TLanguageTranslation;
  i:        Integer;
begin
  useIdx := 0;
  for i := 0 to (cbLanguage.items.Count - 1) do begin
    currLang := LanguageControlLanguage(i);
    if (currLang.Code = langCode) then begin
      useIdx := i;
      break;
    end;
  end;
  cbLanguage.ItemIndex := useIdx;
end;

function TfmeGeneralOptions.SelectedLanguage(): TLanguageTranslation;
begin
  Result := LanguageControlLanguage(cbLanguage.ItemIndex);

end;

function TfmeGeneralOptions.LanguageControlLanguage(idx: Integer): TLanguageTranslation;
begin
  Result := (PLanguageTranslation(cbLanguage.Items.Objects[idx]))^;

end;

end.
