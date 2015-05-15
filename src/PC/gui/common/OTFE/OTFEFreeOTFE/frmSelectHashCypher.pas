unit frmSelectHashCypher;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  Classes, Controls, Dialogs,
  Forms, Graphics, Grids,
  Menus, Messages, OTFEFreeOTFEBase_U,
  SDUForms, StdCtrls, SysUtils, VolumeFileAPI, Windows;

{ TODO -otdk -cui : doesnt look good - replace custom draw with simple grid }
type
  TfrmSelectHashCypher = class (TSDUForm)
    Label1:         TLabel;
    sgCombinations: TStringGrid;
    pbOK:           TButton;
    pbCancel:       TButton;
    Label2:         TLabel;
    miPopup:        TPopupMenu;
    miHashDetails:  TMenuItem;
    miCypherDetails: TMenuItem;
    Label3:         TLabel;
    procedure FormCreate(Sender: TObject);
    procedure miHashDetailsClick(Sender: TObject);
    procedure miCypherDetailsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgCombinationsClick(Sender: TObject);
    procedure sgCombinationsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure sgCombinationsDblClick(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
  private
    HashDriverKernelModeNames: TStringList;
    HashGUIDs:   TStringList;
    CypherDriverKernelModeNames: TStringList;
    CypherGUIDs: TStringList;

    procedure EnableDisableControls();
  public
    FreeOTFEObj: TOTFEFreeOTFEBase;

    procedure AddCombination(
      HashDriverKernelModeName: String;
      HashGUID: TGUID;
      CypherDriverKernelModeName: String;
      CypherGUID: TGUID
      );

    function SelectedCypherDriverKernelModeName(): String;
    function SelectedCypherGUID(): TGUID;
    function SelectedHashDriverKernelModeName(): String;
    function SelectedHashGUID(): TGUID;
  end;


implementation

{$R *.DFM}


uses
  ComObj,  // Required for GUIDToString
  OTFEFreeOTFE_U,
  SDUi18n,
  SDUGeneral;

procedure TfrmSelectHashCypher.FormCreate(Sender: TObject);
begin
  sgCombinations.ColWidths[0] := sgCombinations.Width;

  HashDriverKernelModeNames   := TStringList.Create();
  HashGUIDs                   := TStringList.Create();
  CypherDriverKernelModeNames := TStringList.Create();
  CypherGUIDs                 := TStringList.Create();

end;


 // Add a particular hash/cypher combination to the list of combinations the
 // user may select from
procedure TfrmSelectHashCypher.AddCombination(
  HashDriverKernelModeName: String;
  HashGUID: TGUID;
  CypherDriverKernelModeName: String;
  CypherGUID: TGUID
  );
var
  tmpHashDetails:         TFreeOTFEHash;
  tmpCypherDetails:       TFreeOTFECypher_v3;
  tmpHashDriverDetails:   TFreeOTFEHashDriver;
  tmpCypherDriverDetails: TFreeOTFECypherDriver;
  prettyHashDriverName:   String;
  prettyCypherDriverName: String;
  prettyHashName:         String;
  prettyCypherName:       String;
  hashText:               String;
  cypherText:             String;
  summaryLine:            String;
  cellContents:           String;
begin
  // Store the details...
  HashDriverKernelModeNames.Add(HashDriverKernelModeName);
  HashGUIDs.Add(GUIDToString(HashGUID));
  CypherDriverKernelModeNames.Add(CypherDriverKernelModeName);
  CypherGUIDs.Add(GUIDToString(CypherGUID));


  // >2 because we always have one fixed row, and one data row
  // Also checks if 1st cell on that data row is populated
  if ((sgCombinations.RowCount > 1) or (sgCombinations.Cells[0, 0] <> '')) then begin
    sgCombinations.RowCount := sgCombinations.RowCount + 1;
  end;


  // Hash details...
  prettyHashName := '';
  hashText       := '';
  if (HashDriverKernelModeName <> '') then begin
    prettyHashDriverName := '???';
    if FreeOTFEObj.GetHashDriverHashes(HashDriverKernelModeName, tmpHashDriverDetails) then
      prettyHashDriverName := tmpHashDriverDetails.Title + ' (' +
        FreeOTFEObj.VersionIDToStr(tmpHashDriverDetails.VersionID) + ')';

    prettyHashName := '???';
    if FreeOTFEObj.GetSpecificHashDetails(HashDriverKernelModeName, HashGUID, tmpHashDetails) then
      prettyHashName := FreeOTFEObj.GetHashDisplayTitle(tmpHashDetails);


    hashText := SDUParamSubstitute(_('Hash implementation: %1'), [prettyHashDriverName]) +
      SDUCRLF + '  ' + SDUParamSubstitute(_('Kernel mode driver: %1'),
      [HashDriverKernelModeName]) + SDUCRLF + '  ' + SDUParamSubstitute(
      _('Algorithm GUID: %1'), [GUIDToString(HashGUID)]);
  end;


  // Cypher details...
  prettyCypherName := '';
  cypherText       := '';
  if (CypherDriverKernelModeName <> '') then begin
    prettyCypherDriverName := '???';
    if FreeOTFEObj.GetCypherDriverCyphers(CypherDriverKernelModeName,
      tmpCypherDriverDetails) then
      prettyCypherDriverName := tmpCypherDriverDetails.Title + ' (' +
        FreeOTFEObj.VersionIDToStr(tmpCypherDriverDetails.VersionID) + ')';

    prettyCypherName := '???';
    if FreeOTFEObj.GetSpecificCypherDetails(CypherDriverKernelModeName,
      CypherGUID, tmpCypherDetails) then
      prettyCypherName := FreeOTFEObj.GetCypherDisplayTitle(tmpCypherDetails);

    cypherText := SDUParamSubstitute(_('Cypher implementation: %1'),
      [prettyCypherDriverName]) + SDUCRLF + '  ' + SDUParamSubstitute(
      _('Kernel mode driver: %1'), [CypherDriverKernelModeName]) + SDUCRLF +
      '  ' + SDUParamSubstitute(_('Algorithm GUID: %1'), [GUIDToString(CypherGUID)]);
  end;


  // Work out cell layout...
  if ((HashDriverKernelModeName <> '') and (CypherDriverKernelModeName <> '')) then
    cellContents := prettyHashName + ' / ' + prettyCypherName + SDUCRLF +
      hashText + SDUCRLF + cypherText
  else
  if (HashDriverKernelModeName <> '') then
    cellContents :=
      prettyHashName + SDUCRLF + hashText
  else
  if (CypherDriverKernelModeName <> '') then
    cellContents := prettyCypherName + SDUCRLF + cypherText
  else
    summaryLine  := _('Error! Please report seeing this!');

  // -1 because we index from zero
  sgCombinations.Cells[0, (sgCombinations.RowCount - 1)] := cellContents;

  EnableDisableControls();
end;



procedure TfrmSelectHashCypher.miHashDetailsClick(Sender: TObject);
begin
  FreeOTFEObj.ShowHashDetailsDlg(
    SelectedHashDriverKernelModeName(),
    SelectedHashGUID()
    );

end;

procedure TfrmSelectHashCypher.miCypherDetailsClick(Sender: TObject);
begin
  FreeOTFEObj.ShowCypherDetailsDlg(
    SelectedCypherDriverKernelModeName(),
    SelectedCypherGUID()
    );

end;

procedure TfrmSelectHashCypher.EnableDisableControls();
var
  rowSelected: Boolean;
begin
  rowSelected := (sgCombinations.Row >= 0);

  miHashDetails.Enabled   := rowSelected;
  miCypherDetails.Enabled := rowSelected;
  pbOK.Enabled            := rowSelected;

end;



function TfrmSelectHashCypher.SelectedHashDriverKernelModeName(): String;
begin
  Result := HashDriverKernelModeNames[sgCombinations.Row];

end;


function TfrmSelectHashCypher.SelectedHashGUID(): TGUID;
begin
  Result := StringToGUID(HashGUIDs[sgCombinations.Row]);

end;


function TfrmSelectHashCypher.SelectedCypherDriverKernelModeName(): String;
begin
  Result := CypherDriverKernelModeNames[sgCombinations.Row];

end;


function TfrmSelectHashCypher.SelectedCypherGUID(): TGUID;
begin
  Result := StringToGUID(CypherGUIDs[sgCombinations.Row]);

end;



procedure TfrmSelectHashCypher.FormShow(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmSelectHashCypher.sgCombinationsClick(Sender: TObject);
begin
  EnableDisableControls();

end;


 // This taken from news posting by "Peter Below (TeamB)" <100113.1...@compuXXserve.com>
 // Code tidied up a little to reflect different style
procedure TfrmSelectHashCypher.sgCombinationsDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  S:        String;
  drawrect: trect;
begin
  S := (Sender as TStringgrid).Cells[ACol, ARow];

  if (Length(S) > 0) then begin
    drawrect := Rect;
    DrawText(
      TStringGrid(Sender).canvas.handle,
      PChar(S),
      Length(S),
      drawrect,
      (dt_calcrect or dt_wordbreak or dt_left)
      );

    if ((drawrect.bottom - drawrect.top) > TStringGrid(Sender).RowHeights[ARow]) then begin
      TStringGrid(Sender).RowHeights[ARow] := (drawrect.bottom - drawrect.top);
    end else begin
      drawrect.Right := Rect.right;
      TStringGrid(Sender).canvas.fillrect(drawrect);
      DrawText(
        TStringGrid(Sender).canvas.handle,
        PChar(S),
        Length(S),
        drawrect,
        (dt_wordbreak or dt_left)
        );
    end;
  end;

end;


procedure TfrmSelectHashCypher.FormDestroy(Sender: TObject);
begin
  HashDriverKernelModeNames.Free();
  HashGUIDs.Free();
  CypherDriverKernelModeNames.Free();
  CypherGUIDs.Free();

end;

procedure TfrmSelectHashCypher.sgCombinationsDblClick(Sender: TObject);
begin
  if (pbOK.Enabled) then begin
    pbOKClick(Sender);
  end;

end;

procedure TfrmSelectHashCypher.pbOKClick(Sender: TObject);
begin
  ModalResult := mrOk;

end;

end.
