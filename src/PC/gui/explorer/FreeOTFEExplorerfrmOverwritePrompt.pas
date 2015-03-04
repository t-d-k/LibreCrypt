unit FreeOTFEExplorerfrmOverwritePrompt;

interface

uses
  Classes, Controls, Dialogs, Forms,
  FreeOTFEExplorerSettings, Graphics, Messages, SDUForms,
  StdCtrls,
  SysUtils, Variants, Windows,       // Required for TMoveDeletionMethod
  ExtCtrls, FreeOTFEExplorerfrmMain; // Required for TFExplOperation

type
  TfrmOverwritePrompt = class (TSDUForm)
    Label1:               TLabel;
    pbOK:                 TButton;
    pbCancel:             TButton;
    gbMoveDeletionMethod: TRadioGroup;
    procedure rbLeaveAloneClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    function GetSelected(): TMoveDeletionMethod;
  PUBLIC
    procedure EnableDisableControls();
  PUBLISHED
    property MoveDeletionMethod: TMoveDeletionMethod Read GetSelected;
  end;

implementation

{$R *.dfm}

uses
  SDUDialogs, SDUGeneral,
  SDUi18n;

procedure TfrmOverwritePrompt.EnableDisableControls();
begin
  SDUEnableControl(pbOK, (MoveDeletionMethod <> mdmPrompt));
end;

procedure TfrmOverwritePrompt.FormShow(Sender: TObject);
const
  DEFAULT_METHOD = mdmDelete;
var
  mdm:    TMoveDeletionMethod;
  idx:    Integer;
  useIdx: Integer;
begin
  // Populate and set move deletion method
  gbMoveDeletionMethod.Items.Clear();
  idx    := -1;
  useIdx := -1;
  for mdm := low(mdm) to high(mdm) do begin
    // Skip the obvious one!
    if (mdm = mdmPrompt) then begin
      continue;
    end;

    Inc(idx);
    gbMoveDeletionMethod.Items.Add(MoveDeletionMethodTitle(mdm));
    if (DEFAULT_METHOD = mdm) then begin
      useIdx := idx;
    end;
  end;
  gbMoveDeletionMethod.ItemIndex := useIdx;

  EnableDisableControls();
end;

function TfrmOverwritePrompt.GetSelected(): TMoveDeletionMethod;
var
  mdm:    TMoveDeletionMethod;
begin
  // Decode move deletion method
  Result := mdmPrompt;
  for mdm := low(mdm) to high(mdm) do begin
    if (MoveDeletionMethodTitle(mdm) = gbMoveDeletionMethod.Items[gbMoveDeletionMethod.ItemIndex])
    then begin
      Result := mdm;
      break;
    end;
  end;


end;

procedure TfrmOverwritePrompt.rbLeaveAloneClick(Sender: TObject);
begin
  EnableDisableControls();
end;

end.
