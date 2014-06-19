unit FreeOTFEExplorerfmeOptions_Advanced;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin64,
  FreeOTFEExplorerSettings, SDUStdCtrls, CommonfmeOptions_Base,
  FreeOTFEExplorerfmeOptions_Base,
  Shredder;

type
  TfmeOptions_FreeOTFEExplorerAdvanced = class(TfmeFreeOTFEExplorerOptions_Base)
    gbAdvanced: TGroupBox;
    ckAdvancedMountDlg: TSDUCheckBox;
    ckRevertVolTimestamps: TSDUCheckBox;
    pnlVertSplit: TPanel;
    seMRUMaxItemCount: TSpinEdit64;
    lblMRUMaxItemCountInst: TLabel;
    lblMRUMaxItemCount: TLabel;
    seOverwritePasses: TSpinEdit64;
    lblOverwritePasses: TLabel;
    lblOverwriteMethod: TLabel;
    cbOverwriteMethod: TComboBox;
    lblMoveDeletionMethod: TLabel;
    cbMoveDeletionMethod: TComboBox;
    ckPreserveTimestampsOnStoreExtract: TSDUCheckBox;
    ckAllowTabsInPasswords: TSDUCheckBox;
    ckAllowNewlinesInPasswords: TSDUCheckBox;
    procedure ControlChanged(Sender: TObject);
  private

  protected
    procedure _ReadSettings(config: TFreeOTFEExplorerSettings); override;
    procedure _WriteSettings(config: TFreeOTFEExplorerSettings); override;

    procedure PopulateOverwriteMethods();
    procedure SetOverwriteMethod(useMethod: TShredMethod);
    function  GetOverwriteMethod(): TShredMethod;
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
  CommonSettings,
  CommonfrmOptions,
  OTFEFreeOTFEBase_U;

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
  userEnterPasses: boolean;
begin
  inherited;

  userEnterPasses := (TShredMethodPasses[GetOverwriteMethod()] <= 0);

  SDUEnableControl(seOverwritePasses, userEnterPasses);
  if not(userEnterPasses) then
    begin
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

  procedure NudgeLabel(lbl: TLabel);
  var
    maxWidth: integer;
  begin
    if (pnlVertSplit.left > lbl.left) then
      begin
      maxWidth := (pnlVertSplit.left - LABEL_CONTROL_MARGIN) - lbl.left;
      end
    else
      begin
      maxWidth := (lbl.Parent.Width - LABEL_CONTROL_MARGIN) - lbl.left;
      end;

    lbl.Width := maxWidth;
  end;

var
  stlChkBoxOrder: TStringList;
  YPos: integer;
  i: integer;
  currChkBox: TCheckBox;
  groupboxMargin: integer;
begin
  inherited;

  SDUCenterControl(gbAdvanced, ccHorizontal);
  SDUCenterControl(gbAdvanced, ccVertical, 25);

  // Re-jig label size to take cater for differences in translation lengths
  // Size so the max. right is flush with the max right of pbLangDetails
//  lblMRUMaxItemCount.width := (pbLangDetails.left + pbLangDetails.width) - lblMRUMaxItemCount.left;
  groupboxMargin := ckAdvancedMountDlg.left;
  lblMRUMaxItemCount.width := (gbAdvanced.width - groupboxMargin) - lblMRUMaxItemCount.left;
  lblMoveDeletionMethod.width := (gbAdvanced.width - groupboxMargin) - lblMoveDeletionMethod.left;
  lblOverwriteMethod.width := (gbAdvanced.width - groupboxMargin) - lblOverwriteMethod.left;
  lblOverwritePasses.width := (gbAdvanced.width - groupboxMargin) - lblOverwritePasses.left;

  pnlVertSplit.caption := '';
  pnlVertSplit.bevelouter := bvLowered;
  pnlVertSplit.width := 3;

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

    stlChkBoxOrder.AddObject(Format('%.5d', [ckAdvancedMountDlg.Top]),       ckAdvancedMountDlg);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckRevertVolTimestamps.Top]),    ckRevertVolTimestamps);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckPreserveTimestampsOnStoreExtract.Top]),    ckPreserveTimestampsOnStoreExtract);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckAllowNewlinesInPasswords.Top]),    ckAllowNewlinesInPasswords);
    stlChkBoxOrder.AddObject(Format('%.5d', [ckAllowTabsInPasswords.Top]),   ckAllowTabsInPasswords);

    currChkBox := TCheckBox(stlChkBoxOrder.Objects[0]);
    YPos := currChkBox.Top;
    YPos := YPos + currChkBox.Height;
    for i:=1 to (stlChkBoxOrder.count - 1) do
      begin
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
  for sm := low(TShredMethodTitle) to high(TShredMethodTitle) do
    begin
    cbOverwriteMethod.Items.Add(ShredMethodTitle(sm));
    end;
end;

procedure TfmeOptions_FreeOTFEExplorerAdvanced.SetOverwriteMethod(useMethod: TShredMethod);
var
  i: integer;
begin
  for i := 0 to (cbOverwriteMethod.items.Count - 1) do
    begin
    if (cbOverwriteMethod.items[i] = ShredMethodTitle(useMethod)) then
      begin
      cbOverwriteMethod.itemindex := i;
      break;
      end;
    end;

end;

function TfmeOptions_FreeOTFEExplorerAdvanced.GetOverwriteMethod(): TShredMethod;
var
  sm: TShredMethod;
  retval: TShredMethod;
begin
  retval := smPseudorandom;

  for sm := low(sm) to high(sm) do
    begin
    if (cbOverwriteMethod.items[cbOverwriteMethod.itemindex] = ShredMethodTitle(sm)) then
      begin
      retval := sm;
      break;
      end;
    end;

  Result := retval;
end;

procedure TfmeOptions_FreeOTFEExplorerAdvanced._ReadSettings(config: TFreeOTFEExplorerSettings);
var
  mdm: TMoveDeletionMethod;
  idx: integer;
  useIdx: integer;
begin
  // Advanced...
  ckAdvancedMountDlg.checked := config.OptAdvancedMountDlg;
  ckRevertVolTimestamps.checked := config.OptRevertVolTimestamps;
  ckPreserveTimestampsOnStoreExtract.checked := config.OptPreserveTimestampsOnStoreExtract;
  ckAllowNewlinesInPasswords.checked := config.OptAllowNewlinesInPasswords;
  ckAllowTabsInPasswords.checked := config.OptAllowTabsInPasswords;

  seMRUMaxItemCount.Value := config.OptMRUList.MaxItems;

  // Populate and set move deletion method
  cbMoveDeletionMethod.Items.Clear();
  idx := -1;
  useIdx := -1;
  for mdm:=low(mdm) to high(mdm) do
    begin
    inc(idx);
    cbMoveDeletionMethod.Items.Add(MoveDeletionMethodTitle(mdm));
    if (config.OptMoveDeletionMethod = mdm) then
      begin
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
  config.OptAdvancedMountDlg := ckAdvancedMountDlg.checked;
  config.OptRevertVolTimestamps := ckRevertVolTimestamps.checked;
  config.OptPreserveTimestampsOnStoreExtract := ckPreserveTimestampsOnStoreExtract.checked;
  config.OptAllowNewlinesInPasswords := ckAllowNewlinesInPasswords.checked;
  config.OptAllowTabsInPasswords := ckAllowTabsInPasswords.checked;

  config.OptMRUList.MaxItems := seMRUMaxItemCount.Value;

  // Decode move deletion method
  config.OptMoveDeletionMethod := mdmPrompt;
  for mdm:=low(mdm) to high(mdm) do
    begin
    if (MoveDeletionMethodTitle(mdm) = cbMoveDeletionMethod.Items[cbMoveDeletionMethod.ItemIndex]) then
      begin
      config.OptMoveDeletionMethod := mdm;
      break;
      end;
    end;

  config.OptOverwriteMethod := GetOverwriteMethod();
  config.OptOverwritePasses := seOverwritePasses.Value;

end;

END.

