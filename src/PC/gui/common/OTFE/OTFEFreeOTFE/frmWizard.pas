unit frmWizard;

interface

uses
     //delphi & libs
       Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls,
  Forms,
  Graphics, Messages,  StdCtrls, SysUtils, Variants, Windows,
  //sdu & LibreCrypt utils
   OTFEFreeOTFEBase_U,
   // LibreCrypt forms
   SDUForms
  ;

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
    procedure FormCreate(Sender: TObject);
  private
    fOnWizardStepChanged: TNotifyEvent;

  protected
      fcreating   : Boolean;  // if form is being created
    procedure _SetupInstructions(); virtual;

    procedure _EnableDisableControls(); virtual;
    procedure _UpdateUIAfterChangeOnCurrentTab(); virtual;
    function _IsTabComplete(checkTab: TTabSheet): Boolean; virtual; abstract;

    procedure _UpdateStepLabel();
    function _NextTabInDir(gotoNext: Boolean): TTabSheet;
    function _IsTabSkipped(tabSheet: TTabSheet): Boolean; virtual;
    //is tab required to be completed
    function _IsTabRequired(tabSheet: TTabSheet): Boolean; virtual;
  public

  published
    property OnWizardStepChanged: TNotifyEvent Read fOnWizardStepChanged
      Write fOnWizardStepChanged;
  end;


implementation

{$R *.dfm}

uses
   //delphi
  Math,
  //sdu, lc utils
  SDUGeneral, SDUi18n
 ;

procedure TfrmWizard.FormCreate(Sender: TObject);
begin
  inherited;
fcreating :=  true;
end;

procedure TfrmWizard.FormShow(Sender: TObject);
var
  i: Integer;
begin
  // Note: SetupInstructions(...) must be called before inherited - otherwise
  //       the translation system will cause the instructions controls to
  //       display whatever they were set to at design time
  _SetupInstructions();

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
    pcWizard.Pages[i].tag := IfThen(_IsTabRequired(pcWizard.Pages[i]), 0, 1);

  // hide all tabs - wizard style
  for i := 0 to (pcWizard.PageCount - 1) do
    pcWizard.Pages[i].TabVisible := False;
  // Select the first tab
  // Yes, this is required; get an access violation if this isn't done
  pcWizard.ActivePageIndex := 0;
fcreating :=  false;
end;

procedure TfrmWizard.pbBackClick(Sender: TObject);
var
  newTab: TTabSheet;
begin
  inherited;

  newTab := _NextTabInDir(False);
  if (newTab <> nil) then begin
    pcWizard.ActivePage := newTab;
    _UpdateUIAfterChangeOnCurrentTab();

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

  newTab := _NextTabInDir(True);
  if (newTab <> nil) then begin
    pcWizard.ActivePage := newTab;
    _UpdateUIAfterChangeOnCurrentTab();

    if Assigned(FOnWizardStepChanged) then begin
      FOnWizardStepChanged(self);
    end;
  end;

end;


procedure TfrmWizard._UpdateStepLabel();
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
    if _IsTabSkipped(pcWizard.Pages[i]) then begin
      Dec(totalStages);
      Dec(totalReqStages);
    end else begin
      // skipped tabs have undefined return values from IsTabRequired
      if (not _IsTabRequired(pcWizard.Pages[i])) then
        Dec(totalReqStages);
    end;


    //done:seems a bit complicated - isn't there an activepageindex property?
    //    if IsTabSkipped(pcWizard.Pages[i]) and  SDUIsTabSheetAfter(pcWizard, pcWizard.Pages[i], pcWizard.ActivePage) then
    //        Dec(currStage);
    if _IsTabSkipped(pcWizard.Pages[i]) and (pcWizard.ActivePageIndex > i) then
      Dec(currStage);
  end;

  pbStage.Min := 1;
  // if have done all required stages, update with non required ones
  if currStage <= totalReqStages then begin
    pbStage.Max      := totalReqStages;
    lblStage.Caption := Format(_('Step %u of %u'), [currStage, totalReqStages]);
  end else begin
    pbStage.Max      := totalStages;
    lblStage.Caption := Format(_('Step %u of %u (Optional)'), [currStage, totalStages]);
  end;
   pbStage.Position := currStage;

end;

procedure TfrmWizard._UpdateUIAfterChangeOnCurrentTab;
var
  allOK: Boolean;
begin
if fcreating then exit;
  //is tab complete?
  allOK                   := _IsTabComplete(pcWizard.ActivePage);
  pcWizard.ActivePage.Tag := 0;
  if allOK then
    pcWizard.ActivePage.Tag := 1;

  _EnableDisableControls();

  // This is a good time to update the stage X of Y display - any changes to
  // the tab's settings may have reduced/increased the number of stages
  _UpdateStepLabel();
end;

 // Get the next tabsheet in the specified direction
 // gotoNext - Set to TRUE to go to the next tab, set to FALSE to go to the
 //            previous tab
function TfrmWizard._NextTabInDir(gotoNext: Boolean): TTabSheet;
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

    sheetSkipped := _IsTabSkipped(tmpSheet);

    if (sheetSkipped) then begin
      tmpSheet.Tag := 1; // Skipped sheets are always complete
      tmpSheet     := pcWizard.FindNextPage(tmpSheet, gotoNext, False);
    end;

  end;  // while (sheetSkipped) do

  Result := tmpSheet;
end;

function TfrmWizard._IsTabSkipped(tabSheet: TTabSheet): Boolean;
begin
  Result := False;
end;

//is tab required to be completed (not called for skipped tabs)
function TfrmWizard._IsTabRequired(tabSheet: TTabSheet): Boolean;
begin
  Result := True;
end;

procedure TfrmWizard._EnableDisableControls();
var
  allDone: Boolean;
  i:       Integer;
begin
  // Enable/disable the standard wizard Back, Next, Finish and Cancel buttons

  // If there's a tab to "Next>" *to*, and we've completed the current tab...
  pbNext.Enabled := (_NextTabInDir(True) <> nil) and (pcWizard.ActivePage.Tag = 1);

  pbBack.Enabled := (pcWizard.ActivePageIndex > 0);

  allDone := True;
  lblCompleteIndicator.Caption := '';
  for i := 0 to (pcWizard.PageCount - 1) do begin
    allDone := allDone and (pcWizard.Pages[i].Tag = 1);
    if not (pcWizard.Pages[i].Tag = 1) then
      lblCompleteIndicator.Caption := lblCompleteIndicator.Caption + ' -- ' + IntToStr(i);
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

procedure TfrmWizard._SetupInstructions();
begin
  // Nothing in base class
end;

end.
