unit FreeOTFEExplorerfmeOptions_Advanced;

interface

uses
  Classes, CommonfmeOptions_Base,
  Controls, Dialogs, ExtCtrls, Forms,
  FreeOTFEExplorerfmeOptions_Base,
  FreeOTFEExplorerSettings, Graphics, Messages, SDUStdCtrls, Shredder, Spin64,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeOptions_FreeOTFEExplorerAdvanced = class (TfmeFreeOTFEExplorerOptions_Base)
    gbAdvanced:                         TGroupBox;
    ckAdvancedMountDlg:                 TSDUCheckBox;
    ckRevertVolTimestamps:              TSDUCheckBox;
    pnlVertSplit:                       TPanel;
    seMRUMaxItemCount:                  TSpinEdit64;
    lblMRUMaxItemCountInst:             TLabel;
    lblMRUMaxItemCount:                 TLabel;
    seOverwritePasses:                  TSpinEdit64;
    lblOverwritePasses:                 TLabel;
    lblOverwriteMethod:                 TLabel;
    cbOverwriteMethod:                  TComboBox;
    lblMoveDeletionMethod:              TLabel;
    cbMoveDeletionMethod:               TComboBox;
    ckPreserveTimestampsOnStoreExtract: TSDUCheckBox;
    ckAllowTabsInPasswords:             TSDUCheckBox;
    ckAllowNewlinesInPasswords:         TSDUCheckBox;
    procedure ControlChanged(Sender: TObject);
  PRIVATE

  PROTECTED
    procedure _ReadSettings(config: TFreeOTFEExplorerSettings); OVERRIDE;
    procedure _WriteSettings(config: TFreeOTFEExplorerSettings); OVERRIDE;

    procedure PopulateOverwriteMethods();
    procedure SetOverwriteMethod(useMethod: TShredMethod);
    function GetOverwriteMethod(): TShredMethod;
  PUBLIC
    procedure Initialize(); OVERRIDE;
    procedure EnableDisableControls(); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
  CommonfrmOptions,
  CommonSettings,
  OTFEFreeOTFEBase_U, SDUDialogs,
  SDUGeneral,
  SDUi18n;

const
  CONTROL_MARGIN_LBL_TO_CONTROL = 5;

resourcestring
  USE_DEFAULT = 'Use default';

procedure TfmeOptions_FreeOTFEExplorerAdvanced.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeOptions_FreeOTFEExplorerAdvanced.EnableDisableControls();
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

procedure TfmeOptions_FreeOTFEExplorerAdvanced.Initialize();
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
    while ((chkBox.Width > maxWidth) and (lastTriedWidth > 0))
      do begin
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
      NudgeCheckbox(currChkBox);

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

  PopulateOverwriteMethods();

end;

procedure TfmeOptions_FreeOTFEExplorerAdvanced.PopulateOverwriteMethods();
var
  sm: TShredMethod;
begin
  cbOverwriteMethod.Items.Clear();
  for sm := low(TShredMethodTitle) to high(TShredMethodTitle) do begin
    cbOverwriteMethod.Items.Add(ShredMethodTitle(sm));
  end;
end;

procedure TfmeOptions_FreeOTFEExplorerAdvanced.SetOverwriteMethod(useMethod: TShredMethod);
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

function TfmeOptions_FreeOTFEExplorerAdvanced.GetOverwriteMethod(): TShredMethod;
var
  sm:     TShredMethod;
  retval: TShredMethod;
begin
  retval := smPseudorandom;

  for sm := low(sm) to high(sm) do begin
    if (cbOverwriteMethod.items[cbOverwriteMethod.ItemIndex] = ShredMethodTitle(sm)) then begin
      retval := sm;
      break;
    end;
  end;

  Result := retval;
end;

procedure TfmeOptions_FreeOTFEExplorerAdvanced._ReadSettings(config: TFreeOTFEExplorerSettings);
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


procedure TfmeOptions_FreeOTFEExplorerAdvanced._WriteSettings(config: TFreeOTFEExplorerSettings);
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

end.
