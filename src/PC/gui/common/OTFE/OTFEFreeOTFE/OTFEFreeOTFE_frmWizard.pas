unit OTFEFreeOTFE_frmWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,
  SDUForms, StdCtrls, ExtCtrls,
  OTFEFreeOTFEBase_U;

type
  TfrmWizard = class(TSDUForm)
    pnlRight: TPanel;
    pcWizard: TPageControl;
    pnlButtons: TPanel;
    lblStage: TLabel;
    lblCompleteIndicator: TLabel;
    pbNext: TButton;
    pbBack: TButton;
    pbFinish: TButton;
    pbCancel: TButton;
    pbStage: TProgressBar;
    bvlLine: TBevel;
    procedure pbBackClick(Sender: TObject);
    procedure pbNextClick(Sender: TObject);
    procedure pbFinishClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fOnWizardStepChanged: TNotifyEvent;
  protected
    procedure SetupInstructions(); virtual;

    procedure EnableDisableControls(); virtual;
    procedure CheckCurrentTabComplete(); virtual;

    procedure UpdateStageDisplay();
    function  NextTabInDir(gotoNext: boolean): TTabSheet;
    function  SkipTab(tabSheet: TTabSheet): boolean; virtual;
  public
    fFreeOTFEObj: TOTFEFreeOTFEBase;

  published
    property OnWizardStepChanged: TNotifyEvent read fOnWizardStepChanged write fOnWizardStepChanged;
  end;


implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral;

procedure TfrmWizard.FormShow(Sender: TObject);
begin
  // Note: SetupInstructions(...) must be called before inherited - otherwise
  //       the translation system will cause the instructions controls to
  //       display whatever they were set to at design time
  SetupInstructions();

  inherited;
  
  // Set this to TRUE or comment out the next line for debug
  lblCompleteIndicator.Visible := FALSE;

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

end;

procedure TfrmWizard.pbBackClick(Sender: TObject);
var
  newTab: TTabSheet;
begin
  inherited;

  newTab := NextTabInDir(FALSE);
  if (newTab <> nil) then
    begin
    pcWizard.ActivePage := newTab;
    CheckCurrentTabComplete();

    if Assigned(FOnWizardStepChanged) then
      begin
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

  newTab := NextTabInDir(TRUE);
  if (newTab <> nil) then
    begin
    pcWizard.ActivePage := newTab;
    CheckCurrentTabComplete();

    if Assigned(FOnWizardStepChanged) then
      begin
      FOnWizardStepChanged(self);
      end;
    end;

end;


procedure TfrmWizard.UpdateStageDisplay();
var
  totalStages: integer;
  currStage: integer;
  i: integer;
begin
  // This functions also sets the stage x/y progress display
  totalStages := pcWizard.PageCount;
  currStage := pcWizard.ActivePageIndex + 1;

  for i:=0 to (pcWizard.PageCount-1) do
    begin
    if SkipTab(pcWizard.Pages[i]) then
      begin
      dec(totalStages);
      if SDUIsTabSheetAfter(pcWizard, pcWizard.Pages[i], pcWizard.ActivePage) then
        begin
        dec(currStage);
        end;
      end;

    end;

  pbStage.Min := 1;
  pbStage.Max := totalStages;
  pbStage.Position := currStage;

  lblStage.Caption := SDUParamSubstitute(_('Stage %1 of %2'), [currStage, totalStages]);

end;

// Get the next tabsheet in the specified direction
// gotoNext - Set to TRUE to go to the next tab, set to FALSE to go to the
//            previous tab
function TfrmWizard.NextTabInDir(gotoNext: boolean): TTabSheet;
var
  tmpSheet: TTabSheet;
  sheetSkipped: boolean;
begin
  tmpSheet := pcWizard.FindNextPage(pcWizard.ActivePage, gotoNext, FALSE);

  // Set sheetSkipped to TRUE to bootstrap the while statement
  sheetSkipped:= TRUE;
  while (sheetSkipped) do
    begin
    if (
        ( (gotoNext)    and (tmpSheet = pcWizard.Pages[0]) ) OR
        ( not(gotoNext) and (tmpSheet = pcWizard.Pages[pcWizard.PageCount-1]) )
       ) then
      begin
      // Wrapped; no more pages in the specified direction, so break & return
      // nil
      tmpSheet := nil;
      break;
      end;

    sheetSkipped:= SkipTab(tmpSheet);

    if (sheetSkipped) then
      begin
      tmpSheet.Tag := 1; // Skipped sheets are always complete
      tmpSheet := pcWizard.FindNextPage(tmpSheet, gotoNext, FALSE);
      end;

    end;  // while (sheetSkipped) do

  Result := tmpSheet;
end;

function TfrmWizard.SkipTab(tabSheet: TTabSheet): boolean;
begin
  Result := FALSE;
end;

procedure TfrmWizard.EnableDisableControls();
var
  allDone: boolean;
  i: integer;
begin
  // Enable/disable the standard wizard Back, Next, Finish and Cancel buttons
  
  // If there's a tab to "Next>" *to*, and we've completed the current tab...
  pbNext.Enabled := (NextTabInDir(TRUE) <> nil) and (pcWizard.ActivePage.Tag = 1);
  
  pbBack.Enabled := (pcWizard.ActivePageIndex > 0);
                                        
  allDone := TRUE;
  lblCompleteIndicator.caption := '';
  for i:=0 to (pcWizard.PageCount-1) do
    begin
    allDone := allDone AND (pcWizard.Pages[i].Tag = 1);
    if not(pcWizard.Pages[i].Tag = 1) then
      begin
      lblCompleteIndicator.caption := lblCompleteIndicator.caption +' -- '+inttostr(i);
      end;
    end;
  pbFinish.Enabled := allDone;

  // Change default button as sensible
  pbBack.Default   := FALSE;
  pbNext.Default   := FALSE;
  pbFinish.Default := FALSE;
  if pbFinish.Enabled then                                 
    begin
    pbFinish.Default := TRUE;
    end
  else if pbNext.Enabled then
    begin
    pbNext.Default := TRUE;
    end
  else if pbBack.Enabled then
    begin
    pbBack.Default := TRUE;
    end;

end;

procedure TfrmWizard.CheckCurrentTabComplete();
begin
  // Nothing in base class
end;

procedure TfrmWizard.SetupInstructions(); 
begin
  // Nothing in base class
end;

END.

