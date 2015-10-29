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
  //   fmeBaseOptions,
  frmCommonOptions,
  SDUFrames, fmeVolumeSelect, Spin64, SDUDialogs;

type


  TfrmExplorerOptions = class (TfrmCommonOptions)
    tsExplorer: TTabSheet;
    tsWebDAV:      TTabSheet;
    gbAdvanced:    TGroupBox;
    lblOverwritePasses: TLabel;
    lblOverwriteMethod: TLabel;
    lblMoveDeletionMethod: TLabel;
    seOverwritePasses: TSpinEdit64;
    cbOverwriteMethod: TComboBox;
    cbMoveDeletionMethod: TComboBox;
    ckPreserveTimestampsOnStoreExtract: TSDUCheckBox;
    gbWebDAV:      TGroupBox;
    ckWebDAV:      TSDUCheckBox;
    gbWebDAVAdvanced: TGroupBox;
    Label6:        TLabel;
    fedWebDAVLogDebug: TSDUFilenameEdit;
    fedWebDAVLogAccess: TSDUFilenameEdit;
    edWebDAVShareName: TEdit;
    ckOverwriteCacheOnDismount: TSDUCheckBox;
    ckWebDAVLogAccess: TSDUCheckBox;
    ckWebDAVLogDebug: TSDUCheckBox;
    Label1: TLabel;
    cbDefaultStoreOp: TComboBox;
    ckShowHiddenItems: TSDUCheckBox;
    ckHideKnownFileExtns: TSDUCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure ckWebDAVClick(Sender: TObject);
  private


    FWarnUserChangesRequireRemount: Boolean;



    function GetOverwriteMethod: TShredMethod;
    procedure SetOverwriteMethod(useMethod: TShredMethod);

//    procedure _NudgeCheckbox(chkBox: TCheckBox);
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
    procedure _WriteAllSettings(config: TCommonSettings); override;
    procedure _InitAndReadSettings(config: TCommonSettings); override;
    procedure _EnableDisableControls(); override;
  public

  end;


implementation

{$R *.dfm}

uses
           //delphi & libs
  ShlObj,  // Required for CSIDL_PROGRAMS
           //sdu & LibreCrypt utils
  OTFEFreeOTFEBase_U,
  SDUGeneral,
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

procedure TfrmExplorerOptions._InitAndReadSettings(
  config: TCommonSettings);
begin
  inherited;

  _InitializeAdvancedExplorer;
  _ReadSettingsAdvancedExplorer(config as TExplorerSettings);

  _InitializeWebDAV;
  _ReadSettingsWebDAV(config as TExplorerSettings);

  _InitializeGeneral;
  _ReadSettingsGeneral(config as TExplorerSettings);
end;



procedure TfrmExplorerOptions._WriteSettingsGeneral(config: TExplorerSettings);

      procedure GetGreyedCheckbox(chkBox: TCheckbox; out stateOne: Boolean; out stateTwo: Boolean);
  begin
    if (chkBox.State <> cbGrayed) then begin
      stateOne := chkBox.Checked;
      stateTwo := chkBox.Checked;
    end;
  end;

var

  dso:     TDefaultStoreOp;
begin


   if ckDisplayToolbar.State <> cbGrayed then begin
     config.showToolbar :=  ckDisplayToolbar.Checked;
     config.ShowExplorerToolBar  :=  ckDisplayToolbar.Checked;
   end;

   if ckDisplayToolbarLarge.State <> cbGrayed then begin
     config.ShowLargeToolbar :=  ckDisplayToolbarLarge.Checked;
     config.showLargerExplorerToolbar  :=  ckDisplayToolbarLarge.Checked;
   end;



   if ckDisplayToolbarCaptions.State <> cbGrayed then begin
     config.ShowToolbarCaptions :=  ckDisplayToolbarCaptions.Checked;
     config.showExplorerToolbarCaptions  :=  ckDisplayToolbarCaptions.Checked;
   end;

  config.ShowHiddenItems    := ckShowHiddenItems.Checked;
  config.hideKnownFileExtns := ckHideKnownFileExtns.Checked;

  // Decode default store op
  config.DefaultStoreOp := dsoPrompt;
  for dso := low(dso) to high(dso) do begin
    if (DefaultStoreOpTitle(dso) = cbDefaultStoreOp.Items[cbDefaultStoreOp.ItemIndex]) then begin
      config.DefaultStoreOp := dso;
      break;
    end;
  end;



end;


procedure TfrmExplorerOptions._WriteSettingsAdvanced(config: TExplorerSettings);
var
  mdm: TMoveDeletionMethod;
begin
  // Advanced...

  config.keepTimestampsOnStoreExtract := ckPreserveTimestampsOnStoreExtract.Checked;




  // Decode move deletion method
  config.MoveDeletionMethod := mdmPrompt;
  for mdm := low(mdm) to high(mdm) do begin
    if (MoveDeletionMethodTitle(mdm) = cbMoveDeletionMethod.Items[cbMoveDeletionMethod.ItemIndex])
    then begin
      config.MoveDeletionMethod := mdm;
      break;
    end;
  end;

  config.OverwriteMethod := GetOverwriteMethod();
  config.overwritePasses := seOverwritePasses.Value;

end;

procedure TfrmExplorerOptions._ReadSettingsAdvancedExplorer(config: TExplorerSettings);
var
  mdm:    TMoveDeletionMethod;
  idx:    Integer;
  useIdx: Integer;
begin
  // Advanced...


  ckPreserveTimestampsOnStoreExtract.Checked := config.keepTimestampsOnStoreExtract;

  // Populate and set move deletion method
  cbMoveDeletionMethod.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for mdm := low(mdm) to high(mdm) do begin
    Inc(idx);
    cbMoveDeletionMethod.Items.Add(MoveDeletionMethodTitle(mdm));
    if (config.MoveDeletionMethod = mdm) then begin
      useIdx := idx;
    end;
  end;
  cbMoveDeletionMethod.ItemIndex := useIdx;

  SetOverwriteMethod(config.OverwriteMethod);
  seOverwritePasses.Value := config.overwritePasses;

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

procedure TfrmExplorerOptions._WriteAllSettings(config: TCommonSettings);
begin
  inherited;
  _WriteSettingsAdvanced(config as TExplorerSettings);
  _WriteSettingsWebDAV(config as TExplorerSettings);
  _WriteSettingsGeneral(config as TExplorerSettings);
end;








procedure TfrmExplorerOptions.ckWebDAVClick(Sender: TObject);
begin
  inherited;

  if SDUOSVistaOrLater() then begin
    SDUMessageDlg(
      RS_DRIVEMAPPING_NOT_SUPPORTED_UNDER_VISTA_AND_7 + SDUCRLF + Format(
      _('Opened containers will only be mapped to drive letters when %s is run under Windows 2000/Windows XP'), [Application.Title]),
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

  _EnableDisableControls();
end;

{ TODO 1 -otdk -crefactor : whats with all the moving controls - try just using designer and set to max translation size}

 // This adjusts the width of a checkbox, and resets it caption so it
 // autosizes. If it autosizes such that it's too wide, it'll drop the width
 // and repeat
//procedure TfrmExplorerOptions._NudgeCheckbox(chkBox: TCheckBox);
//var
//  tmpCaption:     String;
//  maxWidth:       Integer;
//  useWidth:       Integer;
//  lastTriedWidth: Integer;
//begin
//  tmpCaption := chkBox.Caption;
//
//  maxWidth := (pnlVertSplit.left - CHKBOX_CONTROL_MARGIN) - chkBox.Left;
//  useWidth := maxWidth;
//
//  chkBox.Caption := 'X';
//  chkBox.Width   := useWidth;
//  lastTriedWidth := useWidth;
//  chkBox.Caption := tmpCaption;
//  while ((chkBox.Width > maxWidth) and (lastTriedWidth > 0)) do begin
//    // 5 used here; just needs to be something sensible to reduce the
//    // width by; 1 would do pretty much just as well
//    useWidth := useWidth - 5;
//
//    chkBox.Caption := 'X';
//    chkBox.Width   := useWidth;
//    lastTriedWidth := useWidth;
//    chkBox.Caption := tmpCaption;
//  end;
//
//end;

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
  //  lblMRUMaxItemCount.width := (pbLangDetails.left + pbLangDetails.width) - lblMRUMaxItemCount.left;
  groupboxMargin              := ckAdvancedMountDlg.left;
  lblMRUMaxItemCount.Width    := (gbAdvanced.Width - groupboxMargin) - lblMRUMaxItemCount.left;
  lblMoveDeletionMethod.Width := (gbAdvanced.Width - groupboxMargin) - lblMoveDeletionMethod.left;
  lblOverwriteMethod.Width    := (gbAdvanced.Width - groupboxMargin) - lblOverwriteMethod.left;
  lblOverwritePasses.Width    := (gbAdvanced.Width - groupboxMargin) - lblOverwritePasses.left;

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
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckAdvancedMountDlg.Top]), ckAdvancedMountDlg);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckRevertVolTimestamps.Top]),
//      ckRevertVolTimestamps);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckPreserveTimestampsOnStoreExtract.Top]),
//      ckPreserveTimestampsOnStoreExtract);
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
//      _NudgeCheckbox(currChkBox);
//
//      YPos := currChkBox.Top;
//      YPos := YPos + currChkBox.Height;
//    end;
//
//  finally
//    stlChkBoxOrder.Free();
//  end;

  // Nudge labels so they're as wide as can be allowed
//  NudgeLabel(lblMoveDeletionMethod);
//  NudgeLabel(lblOverwriteMethod);
//  NudgeLabel(lblOverwritePasses);
//  NudgeLabel(lblMRUMaxItemCount);
//  NudgeLabel(lblMRUMaxItemCountInst);

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


procedure TfrmExplorerOptions._EnableDisableControls;
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

//var
//  stlChkBoxOrder: TStringList;
//  YPos:           Integer;
//  i:              Integer;
//  currChkBox:     TCheckBox;
begin
  inherited;


//
//  SDUCenterControl(gbGeneral, ccHorizontal);
//  SDUCenterControl(gbGeneral, ccVertical, 25);

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
 // stlChkBoxOrder := TStringList.Create();
//  try
//    // stlChkBoxOrder is used to order the checkboxes in their vertical order;
//    // this allows checkboxes to be added into the list below in *any* order,
//    // and it'll still work
//    stlChkBoxOrder.Sorted := True;
//
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbar.Top]), ckDisplayToolbar);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckShowPasswords.Top]), ckShowPasswords);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarLarge.Top]),
//      ckDisplayToolbarLarge);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckDisplayToolbarCaptions.Top]),
//      ckDisplayToolbarCaptions);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckShowHiddenItems.Top]), ckShowHiddenItems);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckHideKnownFileExtns.Top]), ckHideKnownFileExtns);
//    stlChkBoxOrder.AddObject(Format('%.5d', [ckStoreLayout.Top]), ckStoreLayout);

  //  currChkBox := TCheckBox(stlChkBoxOrder.Objects[0]);
//    YPos       := currChkBox.Top;
//    YPos       := YPos + currChkBox.Height;
//    for i := 1 to (stlChkBoxOrder.Count - 1) do begin
//      currChkBox := TCheckBox(stlChkBoxOrder.Objects[i]);
//
//      if currChkBox.Visible then begin
//        currChkBox.Top := YPos + CHKBOX_CONTROL_MARGIN;
//
//        // Sort out the checkbox's height
//        _NudgeCheckbox(currChkBox);
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
  ckWebDAV.Checked                  := config.EnableWebDAVServer;
  FWarnUserChangesRequireRemount    := prevWarnUserChangesRequireRemount;



  // Default drive letter
  if (config.DefaultDriveChar = #0) then
    cbDrive.ItemIndex := 0
  else
    cbDrive.ItemIndex := cbDrive.Items.IndexOf(config.DefaultDriveChar + ':');


  ckOverwriteCacheOnDismount.Checked := config.OverwriteWebDAVCacheOnDismount;
  edWebDAVShareName.Text             := config.WebDavShareName;
  fedWebDAVLogAccess.Filename        := config.WebDavLogAccessFile;
  fedWebDAVLogDebug.Filename         := config.WebDavLogDebugFile;

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
  dso:    TDefaultStoreOp;
  idx:    Integer;
  useIdx: Integer;
  showTemp   : Boolean;
begin
  // General...

  SetupGreyedCheckbox(
    ckDisplayToolbar,
    config.ShowToolbar,
    config.ShowExplorerToolBar
    );

  SetupGreyedCheckbox(
    ckDisplayToolbarLarge,
    config.ShowLargeToolbar,
    config.ShowLargerExplorerToolbar
    );

  SetupGreyedCheckbox(
    ckDisplayToolbarCaptions,
    config.ShowToolbarCaptions ,
    config.ShowExplorerToolbarCaptions
    );


  ckShowHiddenItems.Checked    := config.ShowHiddenItems;
  ckHideKnownFileExtns.Checked := config.hideKnownFileExtns;

  // Populate and set default store op dropdown
  cbDefaultStoreOp.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for dso := low(dso) to high(dso) do begin
    Inc(idx);
    cbDefaultStoreOp.Items.Add(DefaultStoreOpTitle(dso));
    if (config.DefaultStoreOp = dso) then begin
      useIdx := idx;
    end;
  end;
  cbDefaultStoreOp.ItemIndex := useIdx;
end;

procedure TfrmExplorerOptions._WriteSettingsWebDAV(config: TExplorerSettings);
begin
  config.EnableWebDAVServer := ckWebDAV.Checked;
  config.OverwriteWebDAVCacheOnDismount := ckOverwriteCacheOnDismount.Checked;
  config.WebDavShareName                := edWebDAVShareName.Text;
  config.WebDavLogDebugFile                 := fedWebDAVLogDebug.Filename;
  config.WebDavLogAccessFile                := fedWebDAVLogAccess.Filename;

end;




end.
