unit CommonfrmGridReport_Hash;

interface

uses
  ActnList, Buttons, Classes, ComCtrls, CommonfrmGridReport, Controls, Dialogs, ExtCtrls,
  Forms,
  Graphics, Grids, Menus, Messages, SDUDialogs, SDUStringGrid, StdCtrls,
  SysUtils, Variants, Windows;

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

  TfrmGridReport_Hash = class (TfrmGridReport)
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
    [gchDriverGUID,
    gchDriverDeviceName,
    gchDriverUserModeName,
    gchDriverKernelModeName,
    gchHashBlocksize,
    gchHashGUID
    ];



function TfrmGridReport_Hash.ColumnToColIdx(column: TGridColumn_Hash): Integer;
begin
  Result := Ord(column);
end;

function TfrmGridReport_Hash.ColIdxToColumn(colIdx: Integer): TGridColumn_Hash;
var
  i:      TGridColumn_Hash;
  retval: TGridColumn_Hash;
begin
  retval := low(TGridColumn_Hash);

  for i := low(TGridColumn_Hash) to high(TGridColumn_Hash) do begin
    if (ColumnToColIdx(i) = colIdx) then begin
      retval := i;
      break;
    end;
  end;

  Result := retval;
end;

procedure TfrmGridReport_Hash.AddSubItem(col: TGridColumn_Hash; item: TListItem; Value: String);
var
  dispValue: String;
begin
  if (not (IsColumnAdvanced(ColumnToColIdx(col))) or ckShowAdvanced.Checked) then
  begin
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

procedure TfrmGridReport_Hash.SetupGrid();
var
  i: TGridColumn_Hash;
begin
  inherited;

  AddCol('#');
  for i := low(TGridColumn_Hash) to high(TGridColumn_Hash) do begin
    if (not (IsColumnAdvanced(ColumnToColIdx(i))) or ckShowAdvanced.Checked)
    then begin
      AddCol(GetColumnTitle(i));
    end;
  end;

end;

procedure TfrmGridReport_Hash.PopulateGrid();
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

  if OTFEFreeOTFE.GetHashDrivers(allData) then begin
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
          OTFEFreeOTFE.VersionIDToStr(currDriver.VersionID));
        AddSubItem(gchDriverGUID, item, GUIDToString(currDriver.DriverGUID));
        AddSubItem(gchDriverDeviceName, item, currDriver.DeviceName);
        AddSubItem(gchDriverUserModeName, item, currDriver.DeviceUserModeName);
        AddSubItem(gchDriverKernelModeName, item, currDriver.LibFNOrDevKnlMdeName);

        AddSubItem(gchHashTitle, item, String(currImpl.Title));
        AddSubItem(gchHashVersion, item,
          OTFEFreeOTFE.VersionIDToStr(currImpl.VersionID));
        AddSubItem(gchHashLength, item, IntToStr(currImpl.Length));
        AddSubItem(gchHashBlocksize, item, IntToStr(currImpl.BlockSize));
        AddSubItem(gchHashGUID, item, GUIDToString(currImpl.HashGUID));
      end;
    end;
  end;

  ResizeColumns();
end;

function TfrmGridReport_Hash.IsColumnAdvanced(colIdx: Integer): Boolean;
begin
  Result := (ColIdxToColumn(colIdx) in ADVANCED_COLS_HASH);
end;

function TfrmGridReport_Hash.CountDrivers(): Integer;
var
  retval:  Integer;
  allData: TFreeOTFEHashDriverArray;
begin
  inherited;

  retval := 0;
  if OTFEFreeOTFE.GetHashDrivers(allData) then begin
    retval := high(allData) - low(allData) + 1;
  end;

  Result := retval;
end;

function TfrmGridReport_Hash.CountImplementations(): Integer;
var
  retval:     Integer;
  allData:    TFreeOTFEHashDriverArray;
  i:          Integer;
  currDriver: TFreeOTFEHashDriver;
begin
  inherited;

  retval := 0;
  if OTFEFreeOTFE.GetHashDrivers(allData) then begin
    for i := low(allData) to high(allData) do begin
      currDriver := allData[i];
      retval     := retval + currDriver.HashCount;
    end;
  end;

  Result := retval;
end;

procedure TfrmGridReport_Hash.FormCreate(Sender: TObject);
begin
  inherited;

  self.Caption     := _('Available Hashes');
  lblTitle.Caption := _('The following hashes are available for use:');

end;

procedure TfrmGridReport_Hash.FormShow(Sender: TObject);
var
  msg: String;
begin
  inherited;

  if (CountImplementations() <= 0) then begin
    msg := _('No hash algorithms could be found.');
    if (OTFEFreeOTFE is TOTFEFreeOTFE) then begin
      msg := msg + SDUCRLF + SDUCRLF +
        _('Please start portable mode, or click "Drivers..." to install one or more hash drivers');
    end;

    SDUMessageDlg(msg, mtError);
  end;

end;

function TfrmGridReport_Hash.GetColumnTitle(column: TGridColumn_Hash): WideString;
var
  retval: WideString;
begin
  retval := RS_UNKNOWN;

  case column of
    gchDriverTitle: retval          := COL_TITLE_DRIVER_TITLE;
    gchDriverVersion: retval        := COL_TITLE_DRIVER_VERSION;
    gchDriverGUID: retval           := COL_TITLE_DRIVER_GUID;
    gchDriverDeviceName: retval     := COL_TITLE_DRIVER_DEVICE_NAME;
    gchDriverUserModeName: retval   := COL_TITLE_DRIVER_USER_MODE_NAME;
    gchDriverKernelModeName: retval := COL_TITLE_DRIVER_KERNEL_MODE_NAME;

    gchHashTitle: retval     := COL_TITLE_HASH_TITLE;
    gchHashVersion: retval   := COL_TITLE_HASH_VERSION;
    gchHashLength: retval    := COL_TITLE_HASH_LENGTH;
    gchHashBlocksize: retval := COL_TITLE_HASH_BLOCKSIZE;
    gchHashGUID: retval      := COL_TITLE_HASH_GUID;
  end;

  Result := retval;
end;

end.
