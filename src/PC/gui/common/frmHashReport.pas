unit frmHashReport;

interface

uses
  ActnList, Buttons, Classes, ComCtrls, frmGridReport, Controls, Dialogs, ExtCtrls,
  Forms,
  Graphics, Grids, Menus, Messages, lcDialogs, SDUStringGrid, StdCtrls,
  SysUtils, Variants, Windows, SDUDialogs;

type
  // IMPORTANT: If this is updated, GetColumnTitle() MUST ALSO BE UPDATED
  TGridColumn_Hash = (
    gchDriverTitle,
    gchDriverVersion,
    gchDriverGUID,
    gchDriverDeviceName,
    gchDriverUserModeName,
    gchDriverKernelModeName,

    gchHashTitle,
    gchHashVersion,
    gchHashLength,
    gchHashBlocksize,
    gchHashGUID
    );

  TfrmHashReport = class (TfrmGridReport)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    // Get the grid column index for a specific column
    function ColumnToColIdx(column: TGridColumn_Hash): Integer;
    function ColIdxToColumn(colIdx: Integer): TGridColumn_Hash;

    procedure AddSubItem(col: TGridColumn_Hash; item: TListItem; Value: String);

    function GetColumnTitle(column: TGridColumn_Hash): WideString;

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
  OTFEFreeOTFE_U, OTFEFreeOTFEBase_U,
  SDUGeneral,
  SDUi18n;

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
  COL_TITLE_HASH_TITLE     = 'Hash title';
  COL_TITLE_HASH_VERSION   = 'Hash version';
  COL_TITLE_HASH_LENGTH    = 'Hash length';
  COL_TITLE_HASH_BLOCKSIZE = 'Hash blocksize';
  COL_TITLE_HASH_GUID      = 'Hash GUID';

const
  ADVANCED_COLS_HASH: set of TGridColumn_Hash =
    [gchDriverGUID, gchDriverDeviceName, gchDriverUserModeName,
    gchDriverKernelModeName, gchHashBlocksize, gchHashGUID];



function TfrmHashReport.ColumnToColIdx(column: TGridColumn_Hash): Integer;
begin
  Result := Ord(column);
end;

function TfrmHashReport.ColIdxToColumn(colIdx: Integer): TGridColumn_Hash;
var
  i:      TGridColumn_Hash;
begin
  Result := low(TGridColumn_Hash);

  for i := low(TGridColumn_Hash) to high(TGridColumn_Hash) do begin
    if (ColumnToColIdx(i) = colIdx) then begin
      Result := i;
      break;
    end;
  end;


end;

procedure TfrmHashReport.AddSubItem(col: TGridColumn_Hash; item: TListItem; Value: String);
var
  dispValue: String;
begin
  if (not (IsColumnAdvanced(ColumnToColIdx(col))) or ckShowAdvanced.Checked) then begin
    dispValue := Value;

    if (col = gchHashLength) then begin
      if (StrToInt(Value) = -1) then begin
        dispValue := _('<variable>');
      end;
    end;

    if (col = gchHashBlockSize) then begin
      if (StrToInt(Value) = -1) then begin
        dispValue := _('<n/a>');
      end;
    end;

    item.SubItems.Add(dispValue);
  end;

end;

procedure TfrmHashReport.SetupGrid();
var
  i: TGridColumn_Hash;
begin
  inherited;

  AddCol('#');
  for i := low(TGridColumn_Hash) to high(TGridColumn_Hash) do begin
    if (not (IsColumnAdvanced(ColumnToColIdx(i))) or ckShowAdvanced.Checked) then begin
      AddCol(GetColumnTitle(i));
    end;
  end;

end;

procedure TfrmHashReport.PopulateGrid();
var
  allData:    TFreeOTFEHashDriverArray;
  i:          Integer;
  j:          Integer;
  currDriver: TFreeOTFEHashDriver;
  currImpl:   TFreeOTFEHash;
  currRow:    Integer;
  item:       TListItem;
begin
  inherited;

  if GetFreeOTFEBase().GetHashDrivers(allData) then begin
    currRow := 0;
    for i := low(allData) to high(allData) do begin
      currDriver := allData[i];
      for j := low(currDriver.Hashes) to high(currDriver.Hashes) do begin
        Inc(currRow);
        currImpl := currDriver.Hashes[j];

        item := lvReport.Items.Insert(lvReport.Items.Count);

        item.data := @currImpl;

        item.Caption := IntToStr(currRow);

        AddSubItem(gchDriverTitle, item, String(currDriver.Title));
        AddSubItem(gchDriverVersion, item,
          GetFreeOTFEBase().VersionIDToStr(currDriver.VersionID));
        AddSubItem(gchDriverGUID, item, GUIDToString(currDriver.DriverGUID));
        AddSubItem(gchDriverDeviceName, item, currDriver.DeviceName);
        AddSubItem(gchDriverUserModeName, item, currDriver.DeviceUserModeName);
        AddSubItem(gchDriverKernelModeName, item, currDriver.LibFNOrDevKnlMdeName);

        AddSubItem(gchHashTitle, item, String(currImpl.Title));
        AddSubItem(gchHashVersion, item,
          GetFreeOTFEBase().VersionIDToStr(currImpl.VersionID));
        AddSubItem(gchHashLength, item, IntToStr(currImpl.Length));
        AddSubItem(gchHashBlocksize, item, IntToStr(currImpl.BlockSize));
        AddSubItem(gchHashGUID, item, GUIDToString(currImpl.HashGUID));
      end;
    end;
  end;

  ResizeColumns();
end;

function TfrmHashReport.IsColumnAdvanced(colIdx: Integer): Boolean;
begin
  Result := (ColIdxToColumn(colIdx) in ADVANCED_COLS_HASH);
end;

function TfrmHashReport.CountDrivers(): Integer;
var
  allData: TFreeOTFEHashDriverArray;
begin
  inherited;

  Result := 0;
  if GetFreeOTFEBase().GetHashDrivers(allData) then begin
    Result := high(allData) - low(allData) + 1;
  end;


end;

function TfrmHashReport.CountImplementations(): Integer;
var
  allData:    TFreeOTFEHashDriverArray;
  i:          Integer;
  currDriver: TFreeOTFEHashDriver;
begin
  inherited;

  Result := 0;
  if GetFreeOTFEBase().GetHashDrivers(allData) then begin
    for i := low(allData) to high(allData) do begin
      currDriver := allData[i];
      Result     := Result + currDriver.HashCount;
    end;
  end;


end;

procedure TfrmHashReport.FormCreate(Sender: TObject);
begin
  inherited;

  self.Caption     := _('Available Hashes');
  lblTitle.Caption := _('The following hashes are available for use:');

end;

procedure TfrmHashReport.FormShow(Sender: TObject);
var
  msg: String;
begin
  inherited;

  if (CountImplementations() <= 0) then begin
    msg := _('No hash algorithms could be found.');
    if (GetFreeOTFEBase() is TOTFEFreeOTFE) then begin
      msg := msg + SDUCRLF + SDUCRLF +
        _('Please start portable mode, or click "Drivers..." to install one or more hash drivers');
    end;

    SDUMessageDlg(msg, mtError);
  end;

end;

function TfrmHashReport.GetColumnTitle(column: TGridColumn_Hash): WideString;
begin
  Result := RS_UNKNOWN;

  case column of
    gchDriverTitle: Result          := COL_TITLE_DRIVER_TITLE;
    gchDriverVersion: Result        := COL_TITLE_DRIVER_VERSION;
    gchDriverGUID: Result           := COL_TITLE_DRIVER_GUID;
    gchDriverDeviceName: Result     := COL_TITLE_DRIVER_DEVICE_NAME;
    gchDriverUserModeName: Result   := COL_TITLE_DRIVER_USER_MODE_NAME;
    gchDriverKernelModeName: Result := COL_TITLE_DRIVER_KERNEL_MODE_NAME;

    gchHashTitle: Result     := COL_TITLE_HASH_TITLE;
    gchHashVersion: Result   := COL_TITLE_HASH_VERSION;
    gchHashLength: Result    := COL_TITLE_HASH_LENGTH;
    gchHashBlocksize: Result := COL_TITLE_HASH_BLOCKSIZE;
    gchHashGUID: Result      := COL_TITLE_HASH_GUID;
  end;
end;

end.
