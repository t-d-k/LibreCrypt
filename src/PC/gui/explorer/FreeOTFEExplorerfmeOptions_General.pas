unit FreeOTFEExplorerfmeOptions_General;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin64,
  FreeOTFEExplorerSettings, SDUStdCtrls, CommonfmeOptions_Base,
  FreeOTFEExplorerfmeOptions_Base;

type
  TLanguageTranslation = record
    Name: string;
    Code: string;
    Contact: string;
  end;
  PLanguageTranslation = ^TLanguageTranslation;

  TfmeOptions_FreeOTFEExplorerGeneral = class(TfmeFreeOTFEExplorerOptions_Base)
    gbGeneral: TGroupBox;
    ckDisplayToolbar: TSDUCheckBox;
    pnlVertSplit: TPanel;
    ckShowPasswords: TSDUCheckBox;
    cbLanguage: TComboBox;
    tlab: TLabel;
    pbLangDetails: TButton;
    ckDisplayToolbarLarge: TSDUCheckBox;
    ckDisplayToolbarCaptions: TSDUCheckBox;
    lblChkUpdatesFreq: TLabel;
    cbChkUpdatesFreq: TComboBox;
    ckShowHiddenItems: TSDUCheckBox;
    ckHideKnownFileExtns: TSDUCheckBox;
    ckStoreLayout: TSDUCheckBox;
    Label1: TLabel;
    cbDefaultStoreOp: TComboBox;
    procedure pbLangDetailsClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure ckStoreLayoutClick(Sender: TObject);
  private
    FLanguages: array of TLanguageTranslation;
    FFlagClearLayoutOnSave: boolean;

    procedure PopulateLanguages();
    procedure SetLanguageSelection(langCode: string);

    function  SelectedLanguage(): TLanguageTranslation;
    function  LanguageControlLanguage(idx: integer): TLanguageTranslation;
  protected
    procedure _ReadSettings(config: TFreeOTFEExplorerSettings); override;
    procedure _WriteSettings(config: TFreeOTFEExplorerSettings); override;
  public
    procedure Initialize(); override;
    procedure EnableDisableControls(); override;
  end;

implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral,
  SDUDialogs,
  CommonConsts,
  CommonSettings,
  CommonfrmOptions;

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

procedure TfmeOptions_FreeOTFEExplorerGeneral.cbLanguageChange(Sender: TObject);
var
  useLang: TLanguageTranslation;
  langIdx: integer;
begin
  inherited;

  // Preserve selected language; the selected language gets change by the
  // PopulateLanguages() call
  langIdx := cbLanguage.ItemIndex;

  useLang:= SelectedLanguage();
  TfrmOptions(Owner).ChangeLanguage(useLang.Code);
  // Repopulate the languages list; translation would have translated them all
  PopulateLanguages();

  // Restore selected
  cbLanguage.ItemIndex := langIdx;

  EnableDisableControls();
end;

procedure TfmeOptions_FreeOTFEExplorerGeneral.ckStoreLayoutClick(
  Sender: TObject);
begin
  inherited;

  if not(ckStoreLayout.checked) then
    begin
    FFlagClearLayoutOnSave := SDUConfirmYN(SDUParamSubstitute(
                                           _('You have turned off the option to automatically save the window layout on exiting.'+SDUCRLF+
                                             SDUCRLF+
                                             'Would you like to reset %1 to its default window layout the next time it is started?'),
                                           [Application.Title]
                                           )
                                          );
    end;

end;

procedure TfmeOptions_FreeOTFEExplorerGeneral.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeOptions_FreeOTFEExplorerGeneral.EnableDisableControls();
begin
  inherited;
  // Language at index 0 is "(Default)"
  SDUEnableControl(pbLangDetails, (cbLanguage.ItemIndex > 0));

  SDUEnableControl(
                   ckDisplayToolbarLarge,
                   (
                    ckDisplayToolbar.checked or
                    (ckDisplayToolbar.State = cbGrayed)
                   )
                  );
  SDUEnableControl(ckDisplayToolbarCaptions, (
                                              ckDisplayToolbar.checked and
                                              ckDisplayToolbarLarge.checked
                                             ));

  // Only allow captions if the user has selected large icons
  if (ckDisplayToolbarLarge.state = cbUnchecked) then
    begin
    ckDisplayToolbarCaptions.Checked := FALSE;
    end;
end;

procedure TfmeOptions_FreeOTFEExplorerGeneral.Initialize();
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
    tmpCaption: string;
    maxWidth: integer;
    useWidth: integer;
    lastTriedWidth: integer;
  begin
    tmpCaption := chkBox.Caption;

    maxWidth := (pnlVertSplit.left - CHKBOX_CONTROL_MARGIN) - chkBox.Left;
    useWidth := maxWidth;

    chkBox.Caption := 'X';
    chkBox.Width := useWidth;
    lastTriedWidth := useWidth;
    chkBox.Caption := tmpCaption;
    while (
           (chkBox.Width > maxWidth) and
           (lastTriedWidth > 0)
          ) do
      begin
      // 5 used here; just needs to be something sensible to reduce the
      // width by; 1 would do pretty much just as well
      useWidth := useWidth - 5;

      chkBox.Caption := 'X';
      chkBox.Width := useWidth;
      lastTriedWidth := useWidth;
      chkBox.Caption := tmpCaption;
      end;

  end;

  procedure NudgeFocusControl(lbl: TLabel);
  begin
    if (lbl.FocusControl <> nil) then
      begin
      lbl.FocusControl.Top := lbl.Top + lbl.Height + CONTROL_MARGIN_LBL_TO_CONTROL;
      end;

  end;

var
  stlChkBoxOrder: TStringList;
  YPos: integer;
  i: integer;
  currChkBox: TCheckBox;
begin
  inherited;

  FFlagClearLayoutOnSave := FALSE;
  
  SDUCenterControl(gbGeneral, ccHorizontal);
  SDUCenterControl(gbGeneral, ccVertical, 25);

  pnlVertSplit.caption := '';
  pnlVertSplit.bevelouter := bvLowered;
  pnlVertSplit.width := 3;

  PopulateLanguages();


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
  stlChkBoxOrder:= TStringList.Create();
  try
    // stlChkBoxOrder is used to order the checkboxes in their vertical order;
    // this allows checkboxes to be added into the list below in *any* order,
    // and it'll still work
    stlChkBoxOrder.Sorted := TRUE;

    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbar.Top]),         ckDisplayToolbar);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckShowPasswords.Top]),          ckShowPasswords);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarLarge.Top]),    ckDisplayToolbarLarge);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarCaptions.Top]), ckDisplayToolbarCaptions);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckShowHiddenItems.Top]),        ckShowHiddenItems);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckHideKnownFileExtns.Top]),     ckHideKnownFileExtns);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckStoreLayout.Top]),            ckStoreLayout);

    currChkBox := TCheckBox(stlChkBoxOrder.Objects[0]);
    YPos := currChkBox.Top;
    YPos := YPos + currChkBox.Height;
    for i:=1 to (stlChkBoxOrder.count - 1) do
      begin
      currChkBox := TCheckBox(stlChkBoxOrder.Objects[i]);

      if currChkBox.visible then
        begin
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
//  NudgeFocusControl(lblMRUMaxItemCount);

end;

procedure TfmeOptions_FreeOTFEExplorerGeneral.pbLangDetailsClick(Sender: TObject);
var
  rcdLanguage: TLanguageTranslation;
begin
  inherited;

  rcdLanguage := SelectedLanguage();
  SDUMessageDlg(
                SDUParamSubstitute(_('Language name: %1'), [rcdLanguage.Name])+SDUCRLF+
                SDUParamSubstitute(_('Language code: %1'), [rcdLanguage.Code])+SDUCRLF+
                SDUParamSubstitute(_('Translator: %1'), [rcdLanguage.Contact])
               );

end;

procedure TfmeOptions_FreeOTFEExplorerGeneral._ReadSettings(config: TFreeOTFEExplorerSettings);

  procedure SetupGreyedCheckbox(chkBox: TCheckbox; stateOne: boolean; stateTwo: boolean);
  begin
    if (stateOne = stateTwo) then
      begin
      chkBox.checked := stateOne;
      end
    else
      begin
      chkBox.State := cbGrayed;
      end;
    chkBox.Tag := ord(chkBox.State);
  end;

var
  uf: TUpdateFrequency;
  dso: TDefaultStoreOp;
  idx: integer;
  useIdx: integer;
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

  ckShowPasswords.checked := config.OptShowPasswords;
  ckShowHiddenItems.checked := config.OptShowHiddenItems;
  ckHideKnownFileExtns.checked := config.OptHideKnownFileExtns;
  ckStoreLayout.checked := config.OptStoreLayout;

  // In case language code not found; reset to "(Default)" entry; at index 0
  SetLanguageSelection(config.OptLanguageCode);

  // Populate and set default store op dropdown
  cbDefaultStoreOp.Items.Clear();
  idx := -1;
  useIdx := -1;
  for dso:=low(dso) to high(dso) do
    begin
    inc(idx);
    cbDefaultStoreOp.Items.Add(DefaultStoreOpTitle(dso));
    if (config.OptDefaultStoreOp = dso) then
      begin
      useIdx := idx;
      end;
    end;
  cbDefaultStoreOp.ItemIndex := useIdx;

  // Populate and set update frequency dropdown
  cbChkUpdatesFreq.Items.Clear();
  idx := -1;
  useIdx := -1;
  for uf:=low(uf) to high(uf) do
    begin
    // Daily and weekly disabled for now; not sure what the load on the 
    // server would be like
    if (
        (uf = ufDaily) or
        (uf = ufWeekly)
       ) then
      begin
      continue;
      end;

    inc(idx);
    cbChkUpdatesFreq.Items.Add(UpdateFrequencyTitle(uf));
    if (config.OptUpdateChkFrequency = uf) then
      begin
      useIdx := idx;
      end;
    end;
  cbChkUpdatesFreq.ItemIndex := useIdx;

end;


procedure TfmeOptions_FreeOTFEExplorerGeneral._WriteSettings(config: TFreeOTFEExplorerSettings);

  procedure GetGreyedCheckbox(chkBox: TCheckbox; var stateOne: boolean; var stateTwo: boolean);
  begin
    if (chkBox.State <> cbGrayed) then
      begin
      stateOne := chkBox.Checked;
      stateTwo := chkBox.Checked;
      end;
  end;

var
  useLang: TLanguageTranslation;
  dso: TDefaultStoreOp;
  uf: TUpdateFrequency;
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

  config.OptShowPasswords := ckShowPasswords.checked;
  config.OptShowHiddenItems := ckShowHiddenItems.checked;
  config.OptHideKnownFileExtns := ckHideKnownFileExtns.checked;
  config.OptStoreLayout := ckStoreLayout.checked;
  
  config.FlagClearLayoutOnSave := FFlagClearLayoutOnSave;

  useLang:= SelectedLanguage();
  config.OptLanguageCode := useLang.Code;

  // Decode default store op
  config.OptDefaultStoreOp := dsoPrompt;
  for dso:=low(dso) to high(dso) do
    begin
    if (DefaultStoreOpTitle(dso) = cbDefaultStoreOp.Items[cbDefaultStoreOp.ItemIndex]) then
      begin
      config.OptDefaultStoreOp := dso;
      break;
      end;
    end;
  
  // Decode update frequency
  config.OptUpdateChkFrequency := ufNever;
  for uf:=low(uf) to high(uf) do
    begin
    if (UpdateFrequencyTitle(uf) = cbChkUpdatesFreq.Items[cbChkUpdatesFreq.ItemIndex]) then
      begin
      config.OptUpdateChkFrequency := uf;
      break;
      end;
    end;
  
end;

procedure TfmeOptions_FreeOTFEExplorerGeneral.PopulateLanguages();
var
  i: integer;
  origLangCode: string;
  langCodes: TStringlist;
  sortedList: TStringList;
begin
  // Store language information for later use...
  langCodes:= TStringlist.Create();
  try
    // Get all language codes...
    SDUGetLanguageCodes(langCodes);

    // +1 to include "Default"
    SetLength(FLanguages, langCodes.count);

    // Spin though the languages, getting their corresponding human-readable
    // names
    origLangCode:= SDUGetCurrentLanguageCode();
    try
      for i:=0 to (langCodes.count - 1) do
        begin
        SDUSetLanguage(langCodes[i]);

        FLanguages[i].Code := langCodes[i];
        FLanguages[i].Name := _(CONST_LANGUAGE_ENGLISH);
        FLanguages[i].Contact := SDUGetTranslatorNameAndEmail();

        // Force set contact details for English version; the dxgettext software sets
        // this to some stupid default
        if (langCodes[i] = ISO639_ALPHA2_ENGLISH) then
          begin
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
  FLanguages[length(FLanguages)-1].Code := '';
  FLanguages[length(FLanguages)-1].Name := _('(Default)');
  FLanguages[length(FLanguages)-1].Contact := '';

  // Populate list
  sortedList:= TStringList.Create();
  try
    for i:=0 to (length(FLanguages) - 1) do
      begin
      sortedList.AddObject(FLanguages[i].Name, @(FLanguages[i]));
      end;

    sortedList.Sorted := TRUE;
    sortedList.Sorted := FALSE;

    cbLanguage.Items.Assign(sortedList)
  finally
    sortedList.Free();
  end;

end;

procedure TfmeOptions_FreeOTFEExplorerGeneral.SetLanguageSelection(langCode: string);
var
  useIdx: integer;
  currLang: TLanguageTranslation;
  i: integer;
begin
  useIdx := 0;
  for i:=0 to (cbLanguage.items.count - 1) do
    begin
    currLang := LanguageControlLanguage(i);
    if (currLang.Code = langCode) then
      begin
      useIdx := i;
      break
      end;
    end;
  cbLanguage.ItemIndex := useIdx;
end;

function TfmeOptions_FreeOTFEExplorerGeneral.SelectedLanguage(): TLanguageTranslation;
var
  retval: TLanguageTranslation;
begin
  retval := LanguageControlLanguage(cbLanguage.ItemIndex);
  Result := retval;
end;

function TfmeOptions_FreeOTFEExplorerGeneral.LanguageControlLanguage(idx: integer): TLanguageTranslation;
var
  retval: TLanguageTranslation;
begin
  retval := (PLanguageTranslation(cbLanguage.Items.Objects[idx]))^;
  Result := retval;
end;

END.

