unit OTFEFreeOTFE_frmWizard;

interface

uses
  Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls,
  Forms,
  Graphics, Messages, OTFEFreeOTFEBase_U, SDUForms, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmWizard = class (TSDUForm)
    pnlRight:   TPanel;
    pcWizard:   TPageControl;
    pnlButtons: TPanel;
    lblStage:   TLabel;
    lblCompleteIndicator: TLabel;
    pbNext:     TButton;
    pbBack:     TButton;
    pbFinish:   TButton;
    pbCancel:   TButton;
    pbStage:    TProgressBar;
    bvlLine:    TBevel;
    procedure pbBackClick(Sender: TObject);
    procedure pbNextClick(Sender: TObject);
    procedure pbFinishClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fOnWizardStepChanged: TNotifyEvent;

  protected
    procedure SetupInstructions(); virtual;

    procedure EnableDisableControls(); virtual;
    procedure UpdateUIAfterChangeOnCurrentTab(); virtual;
    function IsTabComplete(checkTab: TTabSheet): Boolean; virtual; abstract;

    procedure UpdateStageDisplay();
    function NextTabInDir(gotoNext: Boolean): TTabSheet;
    function IsTabSkipped(tabSheet: TTabSheet): Boolean; virtual;
    //is tab required to be completed
    function IsTabRequired(tabSheet: TTabSheet): Boolean; virtual;
  public
    //    fFreeOTFEObj: TOTFEFreeOTFEBase;

  published
    property OnWizardStepChanged: TNotifyEvent Read fOnWizardStepChanged
      Write fOnWizardStepChanged;
  end;


implementation

{$R *.dfm}

uses
  //sdu
  SDUGeneral, SDUi18n,
  //delphi
  Math;

procedure TfrmWizard.FormShow(Sender: TObject);
var
  i: Integer;
begin
  // Note: SetupInstructions(...) must be called before inherited - otherwise
  //       the translation system will cause the instructions controls to
  //       display whatever they were set to at design time
  SetupInstructions();

  inherited;

  // Set this to TRUE or comment out the next line for debug
  lblCompleteIndicator.Visible :=
{$IFDEF DEBUG}
//True
false
{$ELSE}
    False
{$ENDIF}
  ;

  // We change the style to one of the "flat" styles so that no border is shown
  pcWizard.Style := tsFlatButtons;

  SDUClearPanel(pnlRight);
  SDUClearPanel(pnlButtons);

  bvlLine.Height := 2;


  // Select the first tab
  // This is done to ensure that the "master key length" tab isn't selected
  // during initialization; if it is then it'll be checked to see if it's
  // complete during initialization - and since it requires the user to
  // have selected a cypher, it will fail to get the cypher's keylen
  pcWizard.ActivePageIndex := 0;

  //set up tags. tag=0 means needs to be completed. if not req'd set to 1 initially, else assume incomplete
  for i := 0 to (pcWizard.PageCount - 1) do
    pcWizard.Pages[i].tag := IfThen(IsTabRequired(pcWizard.Pages[i]), 0, 1);

  // hide all tabs - wizard style
  for i := 0 to (pcWizard.PageCount - 1) do
    pcWizard.Pages[i].TabVisible := False;
  // Select the first tab
  // Yes, this is required; get an access violation if this isn't done
  pcWizard.ActivePageIndex := 0;

end;

procedure TfrmWizard.pbBackClick(Sender: TObject);
var
  newTab: TTabSheet;
begin
  inherited;

  newTab := NextTabInDir(False);
  if (newTab <> nil) then begin
    pcWizard.ActivePage := newTab;
    UpdateUIAfterChangeOnCurrentTab();

    if Assigned(FOnWizardStepChanged) then begin
      FOnWizardStepChanged(self);
    end;
  end;

end;

procedure TfrmWizard.pbFinishClick(Sender: TObject);
begin
  // Nothing in base class
end;

procedure TfrmWizard.pbNextClick(Sender: TObject);
var
  newTab: TTabSheet;
begin
  inherited;

  newTab := NextTabInDir(True);
  if (newTab <> nil) then begin
    pcWizard.ActivePage := newTab;
    UpdateUIAfterChangeOnCurrentTab();

    if Assigned(FOnWizardStepChanged) then begin
      FOnWizardStepChanged(self);
    end;
  end;

end;


procedure TfrmWizard.UpdateStageDisplay();
var
  totalReqStages, totalStages: Integer;
  currStage: Integer;
  i: Integer;
begin
  // This functions also sets the stage x/y progress display
  totalStages    := pcWizard.PageCount;
  totalReqStages := pcWizard.PageCount;
  currStage      := pcWizard.ActivePageIndex + 1;


  for i := 0 to (pcWizard.PageCount - 1) do begin
    if IsTabSkipped(pcWizard.Pages[i]) then begin
      Dec(totalStages);
      Dec(totalReqStages);
    end else begin
      // skipped tabs have undefined return values from IsTabRequired
      if (not IsTabRequired(pcWizard.Pages[i])) then
        Dec(totalReqStages);
    end;


    //todo:seems a bit complicated - isn't there an activepageindex property?
    //    if IsTabSkipped(pcWizard.Pages[i]) and  SDUIsTabSheetAfter(pcWizard, pcWizard.Pages[i], pcWizard.ActivePage) then
    //        Dec(currStage);
    if IsTabSkipped(pcWizard.Pages[i]) and (pcWizard.ActivePageIndex > i) then
      Dec(currStage);
  end;

  pbStage.Min := 1;
  // if have done all required stages, update wit non required ones
  if currStage <= totalReqStages then begin
    pbStage.Max      := totalReqStages;
    pbStage.Position := currStage;

    lblStage.Caption := SDUParamSubstitute(_('Stage %1 of %2'), [currStage, totalReqStages]);

  end else begin
    pbStage.Max      := totalStages;
    pbStage.Position := currStage;

    lblStage.Caption := SDUParamSubstitute(_('Stage %1 of %2 (Optional)'),
      [currStage, totalStages]);
  end;

end;

procedure TfrmWizard.UpdateUIAfterChangeOnCurrentTab;
var
  allOK: Boolean;
begin
  //is tab complete?
  allOK                   := IsTabComplete(pcWizard.ActivePage);
  pcWizard.ActivePage.Tag := 0;
  if allOK then
    pcWizard.ActivePage.Tag := 1;

  EnableDisableControls();

  // This is a good time to update the stage X of Y display - any changes to
  // the tab's settings may have reduced/increased the number of stages
  UpdateStageDisplay();
end;

 // Get the next tabsheet in the specified direction
 // gotoNext - Set to TRUE to go to the next tab, set to FALSE to go to the
 //            previous tab
function TfrmWizard.NextTabInDir(gotoNext: Boolean): TTabSheet;
var
  tmpSheet:     TTabSheet;
  sheetSkipped: Boolean;
begin
  tmpSheet := pcWizard.FindNextPage(pcWizard.ActivePage, gotoNext, False);

  // Set sheetSkipped to TRUE to bootstrap the while statement
  sheetSkipped := True;
  while (sheetSkipped) do begin
    if ((gotoNext and (tmpSheet = pcWizard.Pages[0])) or ((not gotoNext) and
      (tmpSheet = pcWizard.Pages[pcWizard.PageCount - 1]))) then begin
      // Wrapped; no more pages in the specified direction, so break & return
      // nil
      tmpSheet := nil;
      break;
    end;

    sheetSkipped := IsTabSkipped(tmpSheet);

    if (sheetSkipped) then begin
      tmpSheet.Tag := 1; // Skipped sheets are always complete
      tmpSheet     := pcWizard.FindNextPage(tmpSheet, gotoNext, False);
    end;

  end;  // while (sheetSkipped) do

  Result := tmpSheet;
end;

function TfrmWizard.IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  Result := False;
end;

//is tab required to be completed (not called for skipped tabs)
function TfrmWizard.IsTabRequired(tabSheet: TTabSheet): Boolean;
begin
  Result := True;
end;

procedure TfrmWizard.EnableDisableControls();
var
  allDone: Boolean;
  i:       Integer;
begin
  // Enable/disable the standard wizard Back, Next, Finish and Cancel buttons

  // If there's a tab to "Next>" *to*, and we've completed the current tab...
  pbNext.Enabled := (NextTabInDir(True) <> nil) and (pcWizard.ActivePage.Tag = 1);

  pbBack.Enabled := (pcWizard.ActivePageIndex > 0);

  allDone := True;
  lblCompleteIndicator.Caption := '';
  for i := 0 to (pcWizard.PageCount - 1) do begin
    allDone := allDone and (pcWizard.Pages[i].Tag = 1);
    if not (pcWizard.Pages[i].Tag = 1) then begin
      lblCompleteIndicator.Caption := lblCompleteIndicator.Caption + ' -- ' + IntToStr(i);
    end;
  end;
  pbFinish.Enabled := allDone;

  // Change default button as sensible
  pbBack.Default   := False;
  pbNext.Default   := False;
  pbFinish.Default := False;
  if pbFinish.Enabled then
    pbFinish.Default := True
  else
  if pbNext.Enabled then
    pbNext.Default := True
  else
  if pbBack.Enabled then
    pbBack.Default := True;

end;

procedure TfrmWizard.SetupInstructions();
begin
  // Nothing in base class
end;

end.
