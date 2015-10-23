unit frmCypherReport;

interface

uses
    //delphi & libs (0)
    ActnList, Buttons, Classes, ComCtrls, Controls, Dialogs, ExtCtrls,
  Forms,   Graphics, Grids, Menus, Messages, StdCtrls,  SysUtils, Variants, Windows,
  //sdu & LibreCrypt utils (1)
     lcDialogs,SDUStringGrid,SDUDialogs ,
   // LibreCrypt forms and frames (2)
   frmGridReport
   ;

type
  // IMPORTANT: If this is updated, GetColumnTitle() MUST ALSO BE UPDATED
  TGridColumn_Cypher = (
    gccDriverTitle,
    gccDriverVersion,
    gccDriverGUID,
    gccDriverDeviceName,
    gccDriverUserModeName,
    gccDriverKernelModeName,

    gccCypherTitle,
    gccCypherKeysizeUnderlying,
    gccCypherMode,
    gccCypherKeysizeRequired,
    gccCypherBlocksize,
    gccCypherVersion,
    gccCypherGUID
    );

  TfrmCypherReport = class (TfrmGridReport)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    // Get the grid column index for a specific column
    function ColumnToColIdx(column: TGridColumn_Cypher): Integer;
    function ColIdxToColumn(colIdx: Integer): TGridColumn_Cypher;

    procedure AddSubItem(col: TGridColumn_Cypher; item: TListItem; Value: String);

    function GetColumnTitle(column: TGridColumn_Cypher): WideString;

  PUBLIC
    function CountDrivers(): Integer; OVERRIDE;
    function CountImplementations(): Integer; OVERRIDE;

    procedure SetupGrid(); OVERRIDE;
    procedure PopulateGrid(); OVERRIDE;
    function IsColumnAdvanced(colIdx: Integer): Boolean; OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
     //delphi & libs (0)

  //sdu & LibreCrypt utils (1)
     lcConsts,
  OTFEFreeOTFE_U, OTFEFreeOTFEBase_U,
  SDUGeneral,
  SDUi18n
   // LibreCrypt forms and frames (2)
;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


resourcestring
  // IMPORTANT: If this is updated, GetColumnTitle() MUST ALSO BE UPDATED
  COL_TITLE_CYPHER_TITLE        = 'Cypher title';
  COL_TITLE_CYPHER_KEYSIZE      = 'Cypher keysize';
  COL_TITLE_CYPHER_MODE         = 'Cypher mode';
  COL_TITLE_CYPHER_VERSION      = 'Cypher version';
  COL_TITLE_CYPHER_KEYSIZE_REQD = 'Cypher keysize required';
  COL_TITLE_CYPHER_BLOCKSIZE    = 'Cypher blocksize';
  COL_TITLE_CYPHER_GUID         = 'Cypher GUID';

const
  ADVANCED_COLS_CYPHER: set of TGridColumn_Cypher =
    [gccDriverGUID, gccDriverDeviceName, gccDriverUserModeName,
    gccDriverKernelModeName, gccCypherBlocksize, gccCypherKeysizeRequired,
    gccCypherGUID];



function TfrmCypherReport.ColumnToColIdx(column: TGridColumn_Cypher): Integer;
begin
  Result := Ord(column);
end;

function TfrmCypherReport.ColIdxToColumn(colIdx: Integer): TGridColumn_Cypher;
var
  i:      TGridColumn_Cypher;
begin
  Result := low(TGridColumn_Cypher);

  for i := low(TGridColumn_Cypher) to high(TGridColumn_Cypher) do begin
    if (ColumnToColIdx(i) = colIdx) then begin
      Result := i;
      break;
    end;
  end;


end;

procedure TfrmCypherReport.AddSubItem(col: TGridColumn_Cypher; item: TListItem;
  Value: String);
var
  dispValue: String;
begin
  if (not (IsColumnAdvanced(ColumnToColIdx(col))) or ckShowAdvanced.Checked) then begin
    dispValue := Value;

    if (col = gccCypherKeysizeUnderlying) then begin
      if (StrToInt(Value) = 0) then begin
        dispValue := _('<null>');
      end else
      if (StrToInt(Value) = -1) then begin
        dispValue := _('<any>');
      end;
    end;

    if (col = gccCypherKeysizeRequired) then begin
      if (StrToInt(Value) = 0) then begin
        dispValue := _('<null>');
      end else
      if (StrToInt(Value) = -1) then begin
        dispValue := _('<any>');
      end;
    end;

    if (col = gccCypherBlockSize) then begin
      if (StrToInt(Value) = -1) then begin
        dispValue := _('<any>');
      end;
    end;

    item.SubItems.Add(dispValue);
  end;

end;

procedure TfrmCypherReport.SetupGrid();
var
  i: TGridColumn_Cypher;
begin
  inherited;

  AddCol('#');
  for i := low(TGridColumn_Cypher) to high(TGridColumn_Cypher) do begin
    if (not (IsColumnAdvanced(ColumnToColIdx(i))) or ckShowAdvanced.Checked) then begin
      AddCol(GetColumnTitle(i));
    end;
  end;

end;

procedure TfrmCypherReport.PopulateGrid();
var
  allData:    TFreeOTFECypherDriverArray;
  i:          Integer;
  j:          Integer;
  currDriver: TFreeOTFECypherDriver;
  currImpl:   TFreeOTFECypher_v3;
  currRow:    Integer;
  item:       TListItem;
begin
  inherited;

  if GetFreeOTFEBase().GetCypherDrivers(allData) then begin
    currRow := 0;
    for i := low(allData) to high(allData) do begin
      currDriver := allData[i];
      for j := low(currDriver.Cyphers) to high(currDriver.Cyphers) do begin
        Inc(currRow);
        currImpl := currDriver.Cyphers[j];

        item := lvReport.Items.Insert(lvReport.Items.Count);

        item.data := @currImpl;

        item.Caption := IntToStr(currRow);

        AddSubItem(gccDriverTitle, item, String(currDriver.Title));
        AddSubItem(gccDriverVersion, item,
          GetFreeOTFEBase().VersionIDToStr(currDriver.VersionID));
        AddSubItem(gccDriverGUID, item, GUIDToString(currDriver.DriverGUID));
        AddSubItem(gccDriverDeviceName, item, currDriver.DeviceName);
        AddSubItem(gccDriverUserModeName, item, currDriver.DeviceUserModeName);
        AddSubItem(gccDriverKernelModeName, item, currDriver.LibFNOrDevKnlMdeName);

        AddSubItem(gccCypherTitle, item, String(currImpl.Title));
        AddSubItem(gccCypherKeysizeUnderlying, item, IntToStr(currImpl.KeySizeUnderlying));
        AddSubItem(gccCypherMode, item, FreeOTFECypherModeTitle(currImpl.Mode));
        AddSubItem(gccCypherKeysizeRequired, item, IntToStr(currImpl.KeySizeRequired));
        AddSubItem(gccCypherBlocksize, item, IntToStr(currImpl.BlockSize));
        AddSubItem(gccCypherVersion, item,
          GetFreeOTFEBase().VersionIDToStr(currImpl.VersionID));
        AddSubItem(gccCypherGUID, item, GUIDToString(currImpl.CypherGUID));
      end;
    end;
  end;

  ResizeColumns();
end;

function TfrmCypherReport.IsColumnAdvanced(colIdx: Integer): Boolean;
begin
  Result := (ColIdxToColumn(colIdx) in ADVANCED_COLS_CYPHER);
end;

function TfrmCypherReport.CountDrivers(): Integer;
var
  allData: TFreeOTFECypherDriverArray;
begin
  inherited;

  Result := 0;
  if GetFreeOTFEBase().GetCypherDrivers(allData) then begin
    Result := high(allData) - low(allData) + 1;
  end;


end;

function TfrmCypherReport.CountImplementations(): Integer;
var
  allData:    TFreeOTFECypherDriverArray;
  i:          Integer;
  currDriver: TFreeOTFECypherDriver;
begin
  inherited;

  Result := 0;
  if GetFreeOTFEBase().GetCypherDrivers(allData) then begin
    for i := low(allData) to high(allData) do begin
      currDriver := allData[i];
      Result     := Result + currDriver.CypherCount;
    end;
  end;


end;

procedure TfrmCypherReport.FormCreate(Sender: TObject);
begin
  inherited;

  self.Caption     := _('Available Cyphers');
  lblTitle.Caption := _('The following cyphers are available for use:');

end;

procedure TfrmCypherReport.FormShow(Sender: TObject);
var
  msg: String;
begin
  inherited;

  if (CountImplementations() <= 0) then begin
    msg := _('No cypher algorithms could be found.');
    if (GetFreeOTFEBase() is TOTFEFreeOTFE) then begin
      msg := msg + SDUCRLF + SDUCRLF +
        _('Please start portable mode, or click "Drivers..." to install one or more cypher drivers');
    end;

    SDUMessageDlg(msg, mtError);
  end;

end;

function TfrmCypherReport.GetColumnTitle(column: TGridColumn_Cypher): WideString;
begin
  Result := RS_UNKNOWN;
  case column of
    gccDriverTitle: Result          := COL_TITLE_DRIVER_TITLE;
    gccDriverVersion: Result        := COL_TITLE_DRIVER_VERSION;
    gccDriverGUID: Result           := COL_TITLE_DRIVER_GUID;
    gccDriverDeviceName: Result     := COL_TITLE_DRIVER_DEVICE_NAME;
    gccDriverUserModeName: Result   := COL_TITLE_DRIVER_USER_MODE_NAME;
    gccDriverKernelModeName: Result := COL_TITLE_DRIVER_KERNEL_MODE_NAME;

    gccCypherTitle: Result             := COL_TITLE_CYPHER_TITLE;
    gccCypherKeysizeUnderlying: Result := COL_TITLE_CYPHER_KEYSIZE;
    gccCypherMode: Result              := COL_TITLE_CYPHER_MODE;
    gccCypherVersion: Result           := COL_TITLE_CYPHER_VERSION;
    gccCypherKeysizeRequired: Result   := COL_TITLE_CYPHER_KEYSIZE_REQD;
    gccCypherBlocksize: Result         := COL_TITLE_CYPHER_BLOCKSIZE;
    gccCypherGUID: Result              := COL_TITLE_CYPHER_GUID;
  end;
end;

end.
