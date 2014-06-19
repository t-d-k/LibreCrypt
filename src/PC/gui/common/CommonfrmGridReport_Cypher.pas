unit CommonfrmGridReport_Cypher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CommonfrmGridReport, Menus, ActnList, StdCtrls, Grids, ExtCtrls,
  SDUStringGrid, Buttons, ComCtrls, SDUDialogs;

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

  TfrmGridReport_Cypher = class(TfrmGridReport)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    // Get the grid column index for a specific column
    function  ColumnToColIdx(column: TGridColumn_Cypher): integer;
    function  ColIdxToColumn(colIdx: integer): TGridColumn_Cypher;

    procedure AddSubItem(col: TGridColumn_Cypher; item: TListItem; Value: string);

    function  GetColumnTitle(column: TGridColumn_Cypher): widestring;

  public
    function CountDrivers(): integer; override;
    function CountImplementations(): integer; override;

    procedure SetupGrid(); override;
    procedure PopulateGrid(); override;
    function  IsColumnAdvanced(colIdx: integer): boolean; override;
  end;

implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral,
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_U;

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
  ADVANCED_COLS_CYPHER: set of TGridColumn_Cypher = [
                                                    gccDriverGUID,
                                                    gccDriverDeviceName,
                                                    gccDriverUserModeName,
                                                    gccDriverKernelModeName,
                                                    gccCypherBlocksize,
                                                    gccCypherKeysizeRequired,
                                                    gccCypherGUID
                                                   ];



function TfrmGridReport_Cypher.ColumnToColIdx(column: TGridColumn_Cypher): integer;
begin
  Result:= ord(column);
end;

function TfrmGridReport_Cypher.ColIdxToColumn(colIdx: integer): TGridColumn_Cypher;
var
  i: TGridColumn_Cypher;
  retval: TGridColumn_Cypher;
begin
  retval := low(TGridColumn_Cypher);

  for i:=low(TGridColumn_Cypher) to high(TGridColumn_Cypher) do
    begin
    if (ColumnToColIdx(i) = colIdx) then
      begin
      retval := i;
      break;
      end;
    end;

  Result := retval;
end;

procedure TfrmGridReport_Cypher.AddSubItem(col: TGridColumn_Cypher; item: TListItem; Value: string);
var
  dispValue: string;
begin
  if (
      not(IsColumnAdvanced(ColumnToColIdx(col))) or
      ckShowAdvanced.checked
     ) then
    begin
    dispValue := Value;

    if (col = gccCypherKeysizeUnderlying) then
      begin
      if (strtoint(Value) = 0) then
        begin
        dispValue := _('<null>');
        end
      else if (strtoint(Value) = -1) then
        begin
        dispValue := _('<any>');
        end;
      end;

    if (col = gccCypherKeysizeRequired) then
      begin
      if (strtoint(Value) = 0) then
        begin
        dispValue := _('<null>');
        end
      else if (strtoint(Value) = -1) then
        begin
        dispValue := _('<any>');
        end;
      end;

    if (col = gccCypherBlockSize) then
      begin
      if (strtoint(Value) = -1) then
        begin
        dispValue := _('<any>');
        end;
      end;

    item.SubItems.Add(dispValue);
    end;

end;

procedure TfrmGridReport_Cypher.SetupGrid();
var
  i: TGridColumn_Cypher;
begin
  inherited;

  AddCol('#');
  for i:=low(TGridColumn_Cypher) to high(TGridColumn_Cypher) do
    begin
    if (
        not(IsColumnAdvanced(ColumnToColIdx(i))) or
        ckShowAdvanced.checked
       ) then
      begin
      AddCol(GetColumnTitle(i));
      end;
    end;

end;

procedure TfrmGridReport_Cypher.PopulateGrid();
var
  allData: TFreeOTFECypherDriverArray;
  i: integer;
  j: integer;
  currDriver: TFreeOTFECypherDriver;
  currImpl: TFreeOTFECypher_v3;
  currRow: integer;
  item: TListItem;
begin
  inherited;

  if OTFEFreeOTFE.GetCypherDrivers(allData) then
    begin
    currRow := 0;
    for i:=low(allData) to high(allData) do
      begin
      currDriver := allData[i];
      for j:=low(currDriver.Cyphers) to high(currDriver.Cyphers) do
        begin
        inc(currRow);
        currImpl := currDriver.Cyphers[j];

        item := lvReport.Items.Insert(lvReport.Items.count);

        item.data := @currImpl;

        item.Caption := inttostr(currRow);

        AddSubItem(gccDriverTitle,             item, currDriver.Title);
        AddSubItem(gccDriverVersion,           item, OTFEFreeOTFE.VersionIDToStr(currDriver.VersionID));
        AddSubItem(gccDriverGUID,              item, GUIDToString(currDriver.DriverGUID));
        AddSubItem(gccDriverDeviceName,        item, currDriver.DeviceName);
        AddSubItem(gccDriverUserModeName,      item, currDriver.DeviceUserModeName);
        AddSubItem(gccDriverKernelModeName,    item, currDriver.LibFNOrDevKnlMdeName);

        AddSubItem(gccCypherTitle,             item, currImpl.Title);
        AddSubItem(gccCypherKeysizeUnderlying, item, inttostr(currImpl.KeySizeUnderlying));
        AddSubItem(gccCypherMode,              item, FreeOTFECypherModeTitle(currImpl.Mode));
        AddSubItem(gccCypherKeysizeRequired,   item, inttostr(currImpl.KeySizeRequired));
        AddSubItem(gccCypherBlocksize,         item, inttostr(currImpl.BlockSize));
        AddSubItem(gccCypherVersion,           item, OTFEFreeOTFE.VersionIDToStr(currImpl.VersionID));
        AddSubItem(gccCypherGUID,              item, GUIDToString(currImpl.CypherGUID));
        end;
      end;
    end;

  ResizeColumns();
end;

function TfrmGridReport_Cypher.IsColumnAdvanced(colIdx: integer): boolean;
begin
  Result := (ColIdxToColumn(colIdx) in ADVANCED_COLS_CYPHER);
end;

function TfrmGridReport_Cypher.CountDrivers(): integer;
var
  retval: integer;
  allData: TFreeOTFECypherDriverArray;
begin
  inherited;

  retval := 0;
  if OTFEFreeOTFE.GetCypherDrivers(allData) then
    begin
    retval := high(allData) - low(allData) + 1;
    end;

  Result := retval;
end;

function TfrmGridReport_Cypher.CountImplementations(): integer;
var
  retval: integer;
  allData: TFreeOTFECypherDriverArray;
  i: integer;
  currDriver: TFreeOTFECypherDriver;
begin
  inherited;

  retval := 0;
  if OTFEFreeOTFE.GetCypherDrivers(allData) then
    begin
    for i:=low(allData) to high(allData) do
      begin
      currDriver := allData[i];
      retval := retval + currDriver.CypherCount;
      end;
    end;

  Result := retval;
end;

procedure TfrmGridReport_Cypher.FormCreate(Sender: TObject);
begin
  inherited;

  self.caption := _('Available Cyphers');
  lblTitle.Caption := _('The following cyphers are available for use:');

end;

procedure TfrmGridReport_Cypher.FormShow(Sender: TObject);
var
  msg: string;
begin
  inherited;

  if (CountImplementations() <= 0) then
    begin
    msg := _('No cypher algorithms could be found.');
    if (OTFEFreeOTFE is TOTFEFreeOTFE) then
      begin
      msg := msg + SDUCRLF +
             SDUCRLF+
             _('Please start portable mode, or click "Drivers..." to install one or more cypher drivers');
      end;

    SDUMessageDlg(msg, mtError);
    end;

end;

function TfrmGridReport_Cypher.GetColumnTitle(column: TGridColumn_Cypher): widestring;
var
  retval: widestring;
begin
  retval := RS_UNKNOWN;

  case column of
    gccDriverTitle             : retval := COL_TITLE_DRIVER_TITLE;
    gccDriverVersion           : retval := COL_TITLE_DRIVER_VERSION;
    gccDriverGUID              : retval := COL_TITLE_DRIVER_GUID;
    gccDriverDeviceName        : retval := COL_TITLE_DRIVER_DEVICE_NAME;
    gccDriverUserModeName      : retval := COL_TITLE_DRIVER_USER_MODE_NAME;
    gccDriverKernelModeName    : retval := COL_TITLE_DRIVER_KERNEL_MODE_NAME;

    gccCypherTitle             : retval := COL_TITLE_CYPHER_TITLE;
    gccCypherKeysizeUnderlying : retval := COL_TITLE_CYPHER_KEYSIZE;
    gccCypherMode              : retval := COL_TITLE_CYPHER_MODE;
    gccCypherVersion           : retval := COL_TITLE_CYPHER_VERSION;
    gccCypherKeysizeRequired   : retval := COL_TITLE_CYPHER_KEYSIZE_REQD;
    gccCypherBlocksize         : retval := COL_TITLE_CYPHER_BLOCKSIZE;
    gccCypherGUID              : retval := COL_TITLE_CYPHER_GUID;
  end;

  Result := retval;
end;

END.

