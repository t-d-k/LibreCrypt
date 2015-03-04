unit CommonfrmGridReport;

interface

uses
  ActnList, Buttons, Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, Forms,
  Graphics, Grids,
  Menus, Messages, OTFEFreeOTFEBase_U, SDUDialogs, SDUForms, SDUStringGrid,
  StdCtrls, SysUtils, Variants, Windows;

type
  TTextFormat = (clfText, clfTSV, clfCSV);

  TfrmGridReport = class (TSDUForm)
    pbSave:                  TButton;
    pbClose:                 TButton;
    ActionList1:             TActionList;
    mnuPopup:                TPopupMenu;
    actSelectAll:            TAction;
    actCopy:                 TAction;
    Selectall1:              TMenuItem;
    Copy1:                   TMenuItem;
    lblTitle:                TLabel;
    SaveDialog:              TSDUSaveDialog;
    pnlBetweenButtons:       TPanel;
    pnlBetweenButtonsCenter: TPanel;
    lblTotalDrivers:         TLabel;
    lblTotalAlgorithms:      TLabel;
    ckShowAdvanced:          TCheckBox;
    lvReport:                TListView;
    pbDrivers:               TButton;
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure ckShowAdvancedClick(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure pbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvReportColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvReportCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure pbDriversClick(Sender: TObject);
  PRIVATE
    FColumnToSort: Integer;

    function FormatRow(format: TTextFormat; rowIdx: Integer): String;
    function FormatRow_TSV(rowIdx: Integer): String;
    function FormatRow_CSV(rowIdx: Integer): String;
    function FormatRow_xSV(rowIdx: Integer; seperatorChar: Char): String;
    function FormatRow_xSV_Header(seperatorChar: Char): String;
    function FormatRow_xSV_Item(rowIdx: Integer; seperatorChar: Char): String;
    function FormatRow_Text(rowIdx: Integer): String;
    function CellText(x, y: Integer): String;
  PROTECTED
    function CountDrivers(): Integer; VIRTUAL; ABSTRACT;
    function CountImplementations(): Integer; VIRTUAL; ABSTRACT;

    procedure SetupGrid(); VIRTUAL;
    procedure AddCol(colCaption: String);
    procedure ResizeColumns();
    procedure PopulateGrid(); VIRTUAL;
    function IsColumnAdvanced(colIdx: Integer): Boolean; VIRTUAL;
  PUBLIC
//    OTFEFreeOTFE: TOTFEFreeOTFEBase;
  end;

resourcestring
  COL_TITLE_DRIVER_TITLE            = 'Driver title';
  COL_TITLE_DRIVER_VERSION          = 'Driver version';
  COL_TITLE_DRIVER_GUID             = 'Driver GUID';
  COL_TITLE_DRIVER_DEVICE_NAME      = 'Driver device name';
  COL_TITLE_DRIVER_USER_MODE_NAME   = 'Driver user mode name';
  COL_TITLE_DRIVER_KERNEL_MODE_NAME = 'Driver kernel mode name';

const
  HEADER_ROW = -1;

implementation

{$R *.dfm}

uses
  Clipbrd, // Required for clipboard functions
  Math,
  SDUGeneral,
  SDUi18n,
  {$IFDEF FREEOTFE_MAIN}
  // When run under main FreeOTFE GUI, user can access driver control dialog
  // via main FreeOTFE app
  FreeOTFEfrmMain,
{$ENDIF}
  CommonSettings,
  SDUGraphics;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


resourcestring
  // Save dialog related
  FILE_FILTER_FLT_REPORTS  =
    'Text report (*.txt)|*.txt|Comma separated values (*.csv)|*.csv|Tab separated values (*.tsv)|*.tsv';
  FILE_FILTER_DFLT_REPORTS = 'txt';

const
  // Important! This ordering maps the TTextFormat to the filter index
  // specified in FILE_FILTER_FLT_REPORTS
  FILE_FILTER_TO_FORMAT: array [TTextFormat] of Integer = (1, 3, 2);

procedure TfrmGridReport.SetupGrid();
begin
  lvReport.items.Clear();
  lvReport.RowSelect := True;
  FColumnToSort      := 0;
  lvReport.Columns.Clear();

end;

procedure TfrmGridReport.actCopyExecute(Sender: TObject);
var
  outString: String;
  i:         Integer;
begin
  outString := '';

  outString := outString + FormatRow(clfTSV, HEADER_ROW);
  for i := 0 to (lvReport.items.Count - 1) do begin
    if lvReport.items[i].selected then begin
      outString := outString + FormatRow(clfTSV, i);
    end;
  end;

  Clipboard.AsText := outString;
end;

procedure TfrmGridReport.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
var
  i: Integer;
begin
  // Can't select all unless there's something to select...
  actSelectAll.Enabled := (lvReport.items.Count > 0);

  // Can't copy unless there's something selected...
  actCopy.Enabled := False;
  for i := 0 to (lvReport.items.Count - 1) do begin
    if lvReport.items[i].selected then begin
      actCopy.Enabled := True;
      break;
    end;
  end;
end;

procedure TfrmGridReport.actSelectAllExecute(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to (lvReport.items.Count - 1) do begin
    lvReport.items[i].Selected := True;
  end;

end;

procedure TfrmGridReport.ckShowAdvancedClick(Sender: TObject);
begin
  PopulateGrid();
end;


procedure TfrmGridReport.FormResize(Sender: TObject);
begin
  SDUCenterControl(pnlBetweenButtonsCenter, ccHorizontal);
end;

procedure TfrmGridReport.FormShow(Sender: TObject);
var
  totalAlgs: Integer;
begin
  totalAlgs := CountImplementations();

  lblTotalDrivers.Caption    := SDUParamSubstitute(_('Total drivers: %1'), [CountDrivers]);
  lblTotalAlgorithms.Caption := SDUParamSubstitute(_('Total algorithms: %1'), [totalAlgs]);

  ckShowAdvanced.Checked := False;

  PopulateGrid();

end;

procedure TfrmGridReport.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TfrmGridReport.pbDriversClick(Sender: TObject);
begin
{$IFDEF FREEOTFE_MAIN}
  // When run under main FreeOTFE GUI, user can access driver control dialog
  // via main FreeOTFE app
  if (Owner is TfrmFreeOTFEMain) then
    begin
    TfrmFreeOTFEMain(Owner).DisplayDriverControlDlg();
    end;
{$ENDIF}

end;

procedure TfrmGridReport.pbSaveClick(Sender: TObject);
var
  testFormatCypher: TTextFormat;
  useFormat:        TTextFormat;
  sContent:         String;
  stlContent:       TStringList;
  i:                Integer;
begin
  SaveDialog.Filter     := FILE_FILTER_FLT_REPORTS;
  SaveDialog.DefaultExt := FILE_FILTER_DFLT_REPORTS;
  FreeOTFEGUISetupOpenSaveDialog(SaveDialog);

  if SaveDialog.Execute() then begin
    // Identify file format to save as...
    useFormat := clfText;
    for testFormatCypher := low(FILE_FILTER_TO_FORMAT) to high(FILE_FILTER_TO_FORMAT) do begin
      if (SaveDialog.FilterIndex = FILE_FILTER_TO_FORMAT[testFormatCypher]) then begin
        useFormat := testFormatCypher;
        break;
      end;
    end;

    // Generate file contents...
    if (CountImplementations() <= 0) then begin
      sContent := _('No implementations found');
    end else begin
      if (useFormat <> clfText) then begin
        sContent := FormatRow(useFormat, HEADER_ROW);
      end;

      for i := 0 to (lvReport.items.Count - 1) do begin
        sContent := sContent + FormatRow(useFormat, i);
      end;
    end;

    stlContent := TStringList.Create();
    try
      // Add header if text report
      if (useFormat = clfText) then begin
        GetFreeOTFEBase().AddStdDumpHeader(stlContent, self.Caption);
        stlContent.Add(_('Summary'));
        stlContent.Add(StringOfChar('-', length(_('Summary'))));
        stlContent.Add(SDUParamSubstitute(_('Total drivers   : %1'), [CountDrivers()]));
        stlContent.Add(SDUParamSubstitute(_('Total algorithms: %1'), [CountImplementations()]));
        stlContent.Add('');
        stlContent.Add('');
        stlContent.Add(_('Details'));
        stlContent.Add(StringOfChar('-', length(_('Details'))));
        stlContent.Add('');
      end;

      // ...and dump to file
      stlContent.Text := stlContent.Text + sContent;
      stlContent.SaveToFile(SaveDialog.FileName);
    finally
      stlContent.Free();
    end;
  end;
end;

function TfrmGridReport.FormatRow_TSV(rowIdx: Integer): String;
begin
  Result := FormatRow_xSV(rowIdx, #9);
end;

function TfrmGridReport.FormatRow_CSV(rowIdx: Integer): String;
begin
  Result := FormatRow_xSV(rowIdx, ',');
end;

function TfrmGridReport.FormatRow_xSV(rowIdx: Integer; seperatorChar: Char): String;
var
  row: String;
begin
  if (rowIdx = HEADER_ROW) then begin
    row := FormatRow_xSV_Header(seperatorChar);
  end else begin
    row := FormatRow_xSV_Item(rowIdx, seperatorChar);
  end;

  Result := row + SDUCRLF;
end;

function TfrmGridReport.FormatRow_xSV_Header(seperatorChar: Char): String;
var
  i:      Integer;
begin
  Result := '';

  for i := 0 to (lvReport.columns.Count - 1) do begin
    if (Result <> '') then begin
      Result := Result + seperatorChar;
    end;

    Result := Result + lvReport.columns[i].Caption;
  end;


end;

function TfrmGridReport.FormatRow_xSV_Item(rowIdx: Integer; seperatorChar: Char): String;
var
  i:        Integer;
  item:     TListItem;
  itemText: String;
begin
  Result := '';

  for i := 0 to (lvReport.columns.Count - 1) do begin
    if (Result <> '') then begin
      Result := Result + seperatorChar;
    end;

    item := lvReport.Items[rowIdx];

    if (i = 0) then begin
      itemText := item.Caption;
    end else begin
      itemText := item.SubItems.Strings[i - 1];
    end;

    Result := Result + itemText;
  end;


end;

procedure TfrmGridReport.FormCreate(Sender: TObject);
begin
  lvReport.ViewStyle   := vsReport;
  lvReport.Multiselect := True;
  lvReport.ReadOnly    := True;

  pnlBetweenButtons.BevelInner       := bvNone;
  pnlBetweenButtons.BevelOuter       := bvNone;
  pnlBetweenButtons.Caption          := '';
  pnlBetweenButtonsCenter.BevelInner := bvNone;
  pnlBetweenButtonsCenter.BevelOuter := bvNone;
  pnlBetweenButtonsCenter.Caption    := '';

  SDUSetUACShieldIcon(pbDrivers);

{$IFNDEF FREEOTFE_MAIN}
  // When run under main FreeOTFE GUI, user can access driver control dialog
  // via main FreeOTFE app
  pbDrivers.Visible := False;
{$ENDIF}

end;

function TfrmGridReport.FormatRow_Text(rowIdx: Integer): String;
var
  i:         Integer;
  maxTitleX: Integer;
begin
  Result := '';

  // Identify longest column title
  maxTitleX := 0;
  for i := 0 to (lvReport.columns.Count - 1) do begin
    maxTitleX := max(maxTitleX, length(CellText(i, HEADER_ROW)));
  end;

  for i := 0 to (lvReport.columns.Count - 1) do begin
    Result := Result + CellText(i, HEADER_ROW) + StringOfChar(' ',
      (maxTitleX - length(CellText(i, HEADER_ROW)))) + ': ' + CellText(i, rowIdx) + SDUCRLF;
  end;

  Result := Result + SDUCRLF;


end;

function TfrmGridReport.FormatRow(format: TTextFormat; rowIdx: Integer): String;
begin
  Result := '';

  case format of
    clfTSV:
    begin
      Result := FormatRow_TSV(rowIdx);
    end;

    clfCSV:
    begin
      Result := FormatRow_CSV(rowIdx);
    end;

    clfText:
    begin
      Result := FormatRow_Text(rowIdx);
    end;

  else
  begin
    SDUMessageDlg(
      _('Unknown output format?!') + SDUCRLF + SDUCRLF +
      _('Please report seeing this error!'),
      mtError
      );
  end;

  end;


end;

function TfrmGridReport.IsColumnAdvanced(colIdx: Integer): Boolean;
begin
  Result := False;
end;

procedure TfrmGridReport.lvReportColumnClick(Sender: TObject; Column: TListColumn);
begin
  FColumnToSort := Column.Index;
  lvReport.AlphaSort();  // trigger compare
  if (FColumnToSort = 0) then begin
    if (lvReport.Tag = 1) then begin
      lvReport.Tag := 0;
    end else begin
      lvReport.Tag := 1;
    end;

  end else begin
    if (lvReport.Columns.Items[FColumnToSort - 1].Tag = 1) then begin
      lvReport.Columns.Items[FColumnToSort - 1].Tag := 0;
    end else begin
      lvReport.Columns.Items[FColumnToSort - 1].Tag := 1;
    end;

  end;

end;

procedure TfrmGridReport.lvReportCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);

  function CompareNumbers(ext1, ext2: Extended): Integer;
  begin
    Result := 0;

    if (ext1 > ext2) then begin
      Result := 1;
    end else
    if (ext1 < ext2) then begin
      Result := -1;
    end;


  end;

var
  str1, str2: String;
  colTag:     Integer;
  ext1, ext2: Extended;
  areNumbers: Boolean;
begin
  if (FColumnToSort = 0) then begin
    ColTag := lvReport.Tag;
    Str1   := Item1.Caption;
    Str2   := Item2.Caption;
  end else begin
    ColTag := lvReport.Columns.Items[FColumnToSort - 1].Tag;
    Str1   := Item1.SubItems.Strings[FColumnToSort - 1];
    Str2   := Item2.SubItems.Strings[FColumnToSort - 1];
  end;

  try
    ext1       := StrToFloat(Str1);
    ext2       := StrToFloat(Str2);
    areNumbers := True;

    if (ColTag = 1) then begin
      Compare := CompareNumbers(ext1, ext2);
    end else begin
      Compare := CompareNumbers(ext2, ext1);
    end;
  except
    areNumbers := False;
  end;

  if not (areNumbers) then begin
    if (ColTag = 1) then begin
      Compare := CompareText(Str1, Str2);
    end else begin
      Compare := CompareText(Str2, Str1);
    end;
  end;

end;

procedure TfrmGridReport.PopulateGrid();
begin
  SetupGrid();
end;

procedure TfrmGridReport.AddCol(colCaption: String);
var
  newCol: TListColumn;
begin
  newCol         := lvReport.columns.Add();
  newCol.Caption := colCaption;
  // Note: DON'T set AutoSize to TRUE; otherwise if the user resizes the
  //       dialog, any user set column widths will be reverted
  // newCol.AutoSize := TRUE;
end;

procedure TfrmGridReport.ResizeColumns();
const
  // Resize the columns such that they're as wide as the widest item/subitem
  // text
  RESIZE_EXCL_HEADER = -1;
  // Resize the columns such that they're as wide as the column header text/the
  // widest item/subitem
  RESIZE_INCL_HEADER = -2;
var
  i:            Integer;
  prevAutoSize: Boolean;
begin
  for i := 0 to (lvReport.columns.Count - 1) do begin
    prevAutoSize                := lvReport.column[i].AutoSize;
    lvReport.column[i].AutoSize := True;
    lvReport.column[i].Width    := RESIZE_INCL_HEADER;
    // Revert AutoSize...
    lvReport.column[i].AutoSize := prevAutoSize;
  end;
end;

function TfrmGridReport.CellText(x, y: Integer): String;
var
  itemText: String;
  item:     TListItem;
begin
  if (y = HEADER_ROW) then begin
    itemText := lvReport.columns[x].Caption;
  end else begin
    item := lvReport.items[y];

    if (x = 0) then begin
      itemText := item.Caption;
    end else begin
      itemText := item.SubItems.Strings[x - 1];
    end;

  end;

  Result := itemText;
end;


end.
