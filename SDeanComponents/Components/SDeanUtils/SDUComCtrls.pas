unit SDUComCtrls;

// TSDListView: An extended TListView
// Inherits from TListView, but includes functionality when in vsReport mode
// such that events are available when columns are resized, and supports
// ordering columns

// TSDProgressBarIndeterminate: An indeterminate progress bar
// ! IMPORTANT !
// Requires XP manifest in order to work!


interface

uses
  ComCtrls, Classes, Messages, windows, CommCtrl, Controls;

type


  PNMHEADER = ^TNMHEADER;
  {$EXTERNALSYM tagNMHEADER}
  tagNMHEADER = packed record
    hdr: NMHDR;
    iItem: integer;
    iButton: integer;
    pitem: PHdItem;//PHDITEM;
  end;
  TNMHEADER = tagNMHEADER;
  {$EXTERNALSYM NMHEADER}
  NMHEADER = tagNMHEADER;

  TColumnResizeEvent = procedure (Sender: TListview; ColumnIdx: integer; NewWidth: integer) of object;
  TColumnSortEvent = procedure (Sender: TListview; ColumnIdx: integer) of object;
  TColumnSortDirection = (csdAscending, csdDescending);

  TSDListView = class(TListView)
  private
    FOnBeforeColumnSort: TColumnSortEvent;
    FOnAfterColumnSort: TColumnSortEvent;

    FOnBeforeColumnResize: TColumnResizeEvent;
    FOnColumnResize: TColumnResizeEvent;
    FOnAfterColumnResize: TColumnResizeEvent;

    procedure DoOnBeforeColumnSort(ColumnIdx: integer);
    procedure DoOnAfterColumnSort(ColumnIdx: integer);

    procedure DoOnBeforeColumnResize(ColumnIdx: integer; newWidth: integer);
    procedure DoOnColumnResize(ColumnIdx: integer; newWidth: integer);
    procedure DoOnAfterColumnResize(ColumnIdx: integer; newWidth: integer);

    procedure DoColumnSort(Column: TListColumn);

    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY; 

    function  GetRealColWidth(col: TListColumn): integer;
    procedure SetRealColWidth(col: TListColumn; newWidth: integer);

  protected
    FColumnToSort: integer;

    procedure ColClick(Column: TListColumn); override;

    // specify -1 (HEADER_ROW) as y to get column headers
    function  GetCellText(x, y: integer): string;
    procedure SetCellText(x, y: integer; newText: string);

    function  GetSelectedIdx(): integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    function  AddCol(colCaption: string): TListColumn;
    function  InsertRow(position: integer; rowFields: TStringList; data: Pointer = nil): TListItem; overload;
    function  InsertRow(position: integer; rowFields: array of const; data: Pointer = nil): TListItem; overload;
    function  AppendRow(rowFields: TStringList; data: Pointer = nil): TListItem; overload;
    function  AppendRow(rowFields: array of const; data: Pointer = nil): TListItem; overload;

    procedure ResizeColumns();
    procedure _ColumnSortCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer); virtual;

    procedure SwapItems(a: integer; b: integer);

    procedure InvertSelection();
    procedure SelectedMoveUp();
    procedure SelectedMoveDown();
    function  GetFocused(): integer;
    function  GetColumnByID(colID: integer): TListColumn;
    function  GetColumnDisplayIndex(col: TListColumn): integer;
    property  CellText[x: integer; y: integer]: string read GetCellText write SetCellText;
    property  RealColWidth[col: TListColumn]: integer read GetRealColWidth write SetRealColWidth;

    // Note: This takes/return a TListColumn.Index value - NOT a column index
    //       returned by GetColumnDisplayIndex(...)
    function  GetSelectedColumn(): integer;
    procedure SetSelectedColumn(ColIdx: integer);

  published
    property OnBeforeColumnSort: TColumnSortEvent read FOnBeforeColumnSort write FOnBeforeColumnSort;
    property OnAfterColumnSort: TColumnSortEvent read FOnAfterColumnSort write FOnAfterColumnSort;

    property OnBeforeColumnResize: TColumnResizeEvent read FOnBeforeColumnResize write FOnBeforeColumnResize;
    property OnColumnResize: TColumnResizeEvent read FOnColumnResize write FOnColumnResize;
    property OnAfterColumnResize: TColumnResizeEvent read FOnAfterColumnResize write FOnAfterColumnResize;

    // Note: This returns the *lowest* index number of all sected items.
    // Returns -1 if none selected
    property SelectedIdx: integer read GetSelectedIdx;

    property SelectedColumn: integer read GetSelectedColumn write SetSelectedColumn nodefault;

  end;

  TSDProgressBarIndeterminate = class(TProgressBar)
  private
    FMarquee: boolean;
    FMarqueeUpdateDelay: integer;
  public
    procedure CreateParams(var Params: TCreateParams); override;

    function GetMarquee(): boolean;
    procedure SetMarquee(newValue: boolean);
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Marquee: boolean read GetMarquee write SetMarquee;
    property MarqueeUpdate: integer read FMarqueeUpdateDelay write FMarqueeUpdateDelay;
  end;

procedure Register;

implementation

uses
  SysUtils,
  SDUGeneral;

const
  HEADER_ROW = -1;

  PBS_MARQUEE = $08;
  PBM_SETMARQUEE = (WM_USER + 10);

  // Windows XP and later only
  LVM_SETSELECTEDCOLUMN = LVM_FIRST + 140;
  LVM_GETSELECTEDCOLUMN = LVM_FIRST + 174;

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDListView]);
  RegisterComponents('SDeanUtils', [TSDProgressBarIndeterminate]);
end;

constructor TSDListView.Create(AOwner: TComponent);
begin
  inherited;

  ViewStyle := vsReport;
  
  FColumnToSort := 0;
end;

destructor TSDListView.Destroy();
begin
  inherited;
end;

procedure TSDListView.ColClick(Column: TListColumn);
begin
  DoOnBeforeColumnSort(Column.Index);
  DoColumnSort(Column);
  inherited;
  DoOnAfterColumnSort(Column.Index);
end;

function DefaultListViewColumnSort(Item1, Item2: TListItem;
  lParam: Integer): Integer; stdcall;
begin
  if Assigned(TListView(Item1.ListView).OnCompare) then
  begin
    TListView(Item1.ListView).OnCompare(Item1.ListView, Item1, Item2, lParam, Result);
  end
  else
  begin
    TSDListView(Item1.ListView)._ColumnSortCompare(Item1.ListView, Item1, Item2, lParam, Result);
  end;
end;

// Note: This returns a TListColumn.Index value - NOT a column index returned
//       by GetColumnDisplayIndex(...)
function TSDListView.GetSelectedColumn(): integer;
begin
  Result := SendMessage(
              self.Handle,
              LVM_GETSELECTEDCOLUMN,
              0,
              0
             );
end;

// Note: This takes a TListColumn.Index value - NOT a column index returned
//       by GetColumnDisplayIndex(...)
procedure TSDListView.SetSelectedColumn(ColIdx: integer);
begin
  SendMessage(
              self.Handle,
              LVM_SETSELECTEDCOLUMN,
              ColIdx,
              0
             );
end;

procedure TSDListView.DoColumnSort(Column: TListColumn);
begin
  FColumnToSort := Column.ID;

  // Make sorted column have shaded background
  SetSelectedColumn(Column.Index);

  // Don't call AlphaSort(...) - that uses the default sort; instead do what
  // AlphaSort(...) does, but call *our* version
  if HandleAllocated then
    begin
    ListView_SortItems(Handle, @DefaultListViewColumnSort, 0);
    end;

{
  if (FColumnToSort = 0) then
    begin
    if (Tag = 1) then
      begin
      Tag := 0;
      end
    else
      begin
      Tag := 1;
      end;

    end
  else
    begin
    if (Column.Tag = csdDescending) then
      begin
      Column.Tag := csdAscending;
      end
    else
      begin
      Column.Tag := csdDescending;
      end;

    end;
}

  if (Column.Tag = ord(csdDescending)) then
    begin
    Column.Tag := ord(csdAscending);
    end
  else
    begin
    Column.Tag := ord(csdDescending);
    end;

end;

procedure TSDListView._ColumnSortCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);

  function _CompareText(str1, str2: string): integer;
{
  var
    retval: integer;
}
  begin
{
    retval := 0;

    str1 := uppercase(str1);
    str2 := uppercase(str2);

    if (str1 > str2) then
      begin
      retval := 1;
      end
    else if (str1 < str2) then
      begin
      retval := -1;
      end;

    Result := retval;
}
    // This uses the same method to sort as TTreeView does when sorting it's
    // nodes
    Result := lstrcmp(PChar(str1), PChar(str2));
  end;

  function CompareNumbers(ext1, ext2: extended): integer;
  var
    retval: integer;
  begin
    retval := 0;

    if (ext1 > ext2) then
      begin
      retval := 1;
      end
    else if (ext1 < ext2) then
      begin
      retval := -1;
      end;

    Result := retval;
  end;

var
  str1, str2: string;
  ext1, ext2: Extended;
  sortDirection: TColumnSortDirection;
  areNumbers: boolean;
  sortCol: TListColumn;
  sortColOrderIdx: integer;
begin
  sortCol := self.GetColumnByID(FColumnToSort);
  // We use GetColumnDisplayIndex(...) and NOT sortCol.Index because the user 
  // could have reordered the columns, in which case the column order is 
  // changed, but the SubItems (and the item.Caption) don't
  sortColOrderIdx := GetColumnDisplayIndex(sortCol);

  if (sortColOrderIdx = 0) then
    begin
    Str1 := Item1.Caption;
    Str2 := Item2.Caption;
    end
  else
    begin
    Str1 := Item1.SubItems.Strings[sortColOrderIdx-1];
    Str2 := Item2.SubItems.Strings[sortColOrderIdx-1];
    end;

  sortDirection := TColumnSortDirection(sortCol.Tag);

  try
    ext1 := StrToFloat(Str1);
    ext2 := StrToFloat(Str2);
    areNumbers := TRUE;

    if (sortDirection = csdAscending) then
      begin
      Compare := CompareNumbers(ext1, ext2);
      end
    else
      begin
      Compare := CompareNumbers(ext2, ext1);
      end;
  except
    areNumbers := FALSE;
  end;

  if not(areNumbers) then
    begin
    if (sortDirection = csdAscending) then
      begin
      // Don't use Delphi's CompareText(...) - that places "_ABC" after "ABC"
      //  - we want it before
      Compare := _CompareText(Str1, Str2);
      end
    else
      begin
      // Don't use Delphi's CompareText(...) - that places "_ABC" after "ABC"
      //  - we want it before
      Compare := _CompareText(Str2, Str1);
      end;
    end;

end;

function TSDListView.AddCol(colCaption: string): TListColumn;
var
  newCol: TListColumn;
begin
  newCol := Columns.Add();
  newCol.Caption := colCaption;
  // Note: DON'T set AutoSize to TRUE; otherwise if the user resizes the
  //       dialog, any user set column widths will be reverted
  // newCol.AutoSize := TRUE;
  Result := newCol;
end;

function TSDListView.InsertRow(position: integer; rowFields: array of const; data: Pointer = nil): TListItem;
var
  i: integer;
  stlArgs: TStringList;
begin
  stlArgs:= TStringList.Create();
  try
    for i:=low(rowFields) to high(rowFields) do
      begin
      case rowFields[i].vtype of
        vtInteger    : stlArgs.Add(inttostr(rowFields[i].vinteger));
        vtBoolean    : stlArgs.Add(SDUBoolToStr(rowFields[i].vboolean));
        vtChar       : stlArgs.Add(rowFields[i].vchar);
        vtExtended   : stlArgs.Add(FloatToStr(rowFields[i].VExtended^));
        vtString     : stlArgs.Add(rowFields[i].VString^);
        vtPointer    : stlArgs.Add('0x'+inttohex(integer(rowFields[i].VPointer), 8));
        vtPChar      : stlArgs.Add(rowFields[i].VPChar);
        vtObject     : stlArgs.Add(rowFields[i].VObject.Classname);
        vtClass      : stlArgs.Add(rowFields[i].VClass.Classname);
        vtWideChar   : stlArgs.Add(rowFields[i].VWideChar);
        vtPWideChar  : stlArgs.Add(rowFields[i].VPWideChar);
        vtAnsiString : stlArgs.Add(PChar(rowFields[i].VAnsiString));
//        vtCurrency   : stlArgs.Add(rowFields[i].VCurrency);
        vtVariant    : stlArgs.Add(rowFields[i].VVariant^);
//        vtInterface  : stlArgs.Add(rowFields[i].VInterface);
        vtWideString : stlArgs.Add(PWideChar(rowFields[i].VWideString));
        vtInt64      : stlArgs.Add(inttostr(int64(rowFields[i].VInt64)));

      else
        stlArgs.Add('0x'+inttohex(rowFields[i].vtype, 2));
      end;

      end;

    Result := InsertRow(position, stlArgs, data);
    
  finally
    stlArgs.Free();
  end;

end;

function TSDListView.InsertRow(position: integer; rowFields: TStringList; data: Pointer = nil): TListItem;
var
  item: TListItem;
  stlPaddedFields: TStringList;
  i: integer;
begin
  stlPaddedFields:= TStringList.Create();
  try
    stlPaddedFields.Assign(rowFields);

    // Pad with blank strings so we have enough for each column
    for i:=stlPaddedFields.count to Columns.count do
      begin
      stlPaddedFields.Add('');
      end;

    item := Items.Insert(position);
    item.data := data;
    item.Caption := stlPaddedFields[0];

    // Start from 1 here; we've already done the initial caption
    for i:=1 to (stlPaddedFields.count - 1) do
      begin
      item.SubItems.Add(stlPaddedFields[i]);
      end;

  finally
    stlPaddedFields.Free();
  end;

  Result := item;
end;

function TSDListView.AppendRow(rowFields: TStringList; data: Pointer = nil): TListItem;
begin
  Result := InsertRow(Items.count, rowFields, data);
end;

function TSDListView.AppendRow(rowFields: array of const; data: Pointer = nil): TListItem;
begin
  Result := InsertRow(Items.count, rowFields, data);
end;

procedure TSDListView.ResizeColumns();
const
  // Resize the columns such that they're as wide as the widest item/subitem
  // text
  RESIZE_EXCL_HEADER = -1;
  // Resize the columns such that they're as wide as the column header text/the
  // widest item/subitem
  RESIZE_INCL_HEADER   = -2;
var
  i: integer;
  prevAutoSize: boolean;
begin
  for i:=0 to (Columns.count -1) do
    begin
    prevAutoSize := Column[i].AutoSize;
    Column[i].AutoSize := TRUE;
    Column[i].width := RESIZE_INCL_HEADER;
    // Revert AutoSize...
    Column[i].AutoSize := prevAutoSize;
    end;
end;

function TSDListView.GetCellText(x, y: integer): string;
var
  itemText: string;
  item: TListItem;
begin
  if (y = HEADER_ROW) then
    begin
    itemText := Columns[x].caption
    end
  else
    begin
    item := Items[y];

    if (x = 0) then
      begin
      itemText := item.caption;
      end
    else
      begin
      itemText := item.SubItems.Strings[x-1];
      end;

    end;

  Result := itemText;
end;

procedure TSDListView.SetCellText(x, y: integer; newText: string);
var
  item: TListItem;
begin
  if (y = HEADER_ROW) then
    begin
    Columns[x].caption := newText;
    end
  else
    begin
    item := Items[y];

    if (x = 0) then
      begin
      item.caption := newText;
      end
    else
      begin
      item.SubItems.Strings[x-1] := newText;
      end;

    end;

end;

procedure TSDListView.SwapItems(a: integer; b: integer);
var
  tempLI: TListItem;
begin
  Items.BeginUpdate;
  try
    tempLI := TListItem.Create(Items);
    try
      tempLI.Assign(Items.Item[a]);
      Items.Item[a].Assign(Items.Item[b]);
      Items.Item[b].Assign(tempLI);
    finally
      tempLI.Free;
    end;
  finally
    Items.EndUpdate
  end;
end;


function TSDListView.GetFocused(): integer;
var
  i: integer;
  retval: integer;
begin
  retval := -1;

  for i:=0 to (items.count-1) do
    begin
    if Items[i].focused then
      begin
      retval := i;
      break;
      end;
    end;

  Result := retval;
end;

function TSDListView.GetColumnByID(colID: integer): TListColumn;
var
  retval: TListColumn;
  tmpItem: TCollectionItem;
begin
  retval := nil;
  
  tmpItem := self.Columns.FindItemID(colID);
  if (tmpItem <> nil) then
    begin
    retval := self.Columns.Items[tmpItem.Index];
    end;

// Alternativly (using casting)
//  retval := TListColumn(self.Columns.FindItemID(colID));

  Result := retval;
end;

// This retrieves the ordered index of the specified column
// e.g. If FullDrag is TRUE, and the user moves the 2nd column between the 4th
//      and 5th columns, this will return 2, while the (previously 2nd) column
//      will have its .Index property set to 4
function TSDListView.GetColumnDisplayIndex(col: TListColumn): integer;
var
  colOrder: array of integer;
  colCollection: TListColumns;
begin
  colCollection := TListColumns(col.Collection);
  SetLength(colOrder, colCollection.Count);
  ListView_GetColumnOrderArray(
                               colCollection.Owner.Handle,
                               colCollection.Count,
                               PInteger(colOrder)
                              );
  Result := colOrder[col.Index];
end;

function TSDListView.GetRealColWidth(col: TListColumn): integer;
var
  widthColIdx: integer;
begin
  // This is UGLY.
  // We can't just use this:
  //   colLayout.Width := listCol.Width;
  // If columns have been moved around, listCol.Width returns the *wrong*
  // *columns* *width*!
  // See also Delphi fault # 30134 at:
  //   http://qc.embarcadero.com/wc/qcmain.aspx?d=30134
  // (URL correct as of 1st July 2009)
  // Apparently, this has been fixed in Delphi 2009; in which case it
  // should be possible to use listCol.Width directly with that version
  // of Delphi (and later)
  widthColIdx := GetColumnDisplayIndex(col);
  Result := self.Columns[widthColIdx].Width;
end;

procedure TSDListView.SetRealColWidth(col: TListColumn; newWidth: integer);
begin
  // Set directly - unlike how GetRealColWidth(...) works
  col.Width := newwidth;
end;

procedure TSDListView.InvertSelection();
var
  i: integer;
begin
  for i:=0 to (Items.Count - 1) do
  begin
    Items[i].Selected := not(Items[i].Selected);
  end;

end;

procedure TSDListView.SelectedMoveUp();
var
  i: integer;
  focusedIdx: integer;
  selStateOther: boolean;
begin
  focusedIdx:= GetFocused();

  // Start from 1 - can't move the first item up
  for i:=1 to (items.count-1) do
    begin
    if items[i].selected then
      begin
      selStateOther:= items[i-1].selected;

      SwapItems(i, (i-1));

      items[i].selected := selStateOther;
      items[i-1].selected := TRUE;
      end;
    end;


  if (
      ((focusedIdx-1) >= 0) and
      ((focusedIdx-1) <= (Items.count - 1))
     ) then
  begin
    items[focusedIdx-1].focused := TRUE;
  end;

end;

procedure TSDListView.SelectedMoveDown();
var
  i: integer;
  focusedIdx: integer;
  selStateOther: boolean;
begin
  focusedIdx:= GetFocused();

  // -2 because we can't move the last item down
  for i:=(items.count-2) downto 0 do
    begin
    if items[i].selected then
      begin
      selStateOther:= items[i+1].selected;

      SwapItems(i, (i+1));

      items[i].selected := selStateOther;
      items[i+1].selected := TRUE;
      end;
    end;

  if (
      ((focusedIdx+1) >= 0) and
      ((focusedIdx+1) <= (Items.count - 1))
     ) then
  begin
    items[focusedIdx+1].focused := TRUE;
  end;

end;

procedure TSDListView.WMNotify(var Message: TWMNotify);
var
  header: PNMHEADER;
  newWidth: integer;
begin

  case Message.NMHdr.code of
//    HDN_BEGINTRACK:
    HDN_BEGINTRACKA,
    HDN_BEGINTRACKW,
//    HDN_TRACK:
    HDN_TRACKA,
    HDN_TRACKW,
//    HDN_ENDTRACK,
    HDN_ENDTRACKA,
    HDN_ENDTRACKW:
      begin
        newWidth := -1;
        header := PNMHEADER(Message.NMHdr);
        if (header.pitem <> nil) then
          begin
          if ((header.pItem.mask and HDI_WIDTH) <> 0) then
            begin
            newWidth := header.pItem.cxy;
            end;
          end;


      case Message.NMHdr.code of
    //    HDN_BEGINTRACK:
        HDN_BEGINTRACKA,
        HDN_BEGINTRACKW:
          begin
          DoOnBeforeColumnResize(header.iItem, newWidth);
          end;

    //    HDN_TRACK:
        HDN_TRACKA,
        HDN_TRACKW:
          begin
          DoOnColumnResize(header.iItem, newWidth);
          end;

    //    HDN_ENDTRACK,
        HDN_ENDTRACKA,
        HDN_ENDTRACKW:
          begin
          DoOnAfterColumnResize(header.iItem, newWidth);
          end;

        end;
      end;

    end;

  inherited;

end;

procedure TSDListView.DoOnBeforeColumnSort(ColumnIdx: integer);
begin
  if assigned(FOnBeforeColumnSort) then
    begin
    FOnBeforeColumnSort(self, ColumnIdx);
    end;
end;

procedure TSDListView.DoOnAfterColumnSort(ColumnIdx: integer);
begin
  if assigned(FOnAfterColumnSort) then
    begin
    FOnAfterColumnSort(self, ColumnIdx);
    end;
end;

procedure TSDListView.DoOnBeforeColumnResize(ColumnIdx: integer; newWidth: integer);
begin
  if assigned(FOnBeforeColumnResize) then
    begin
    FOnBeforeColumnResize(self, ColumnIdx, newWidth);
    end;
end;

procedure TSDListView.DoOnColumnResize(ColumnIdx: integer; newWidth: integer);
begin
  if assigned(FOnColumnResize) then
    begin
    FOnColumnResize(self, ColumnIdx, newWidth);
    end;
end;

procedure TSDListView.DoOnAfterColumnResize(ColumnIdx: integer; newWidth: integer);
begin
  if assigned(FOnAfterColumnResize) then
    begin
    FOnAfterColumnResize(self, ColumnIdx, newWidth);
    end;
end;

function TSDListView.GetSelectedIdx(): integer;
var
  retval: integer;
  i: integer;
begin
  retval := -1;

  for i:=0 to (self.Items.count - 1) do
    begin
    if self.Items[i].Selected then
      begin
      retval := i;
      break;
      end;
    end;

  Result := retval;
end;

procedure TSDProgressBarIndeterminate.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or PBS_MARQUEE;
end;

constructor TSDProgressBarIndeterminate.Create(AOwner: TComponent);
begin
  inherited;
  FMarqueeUpdateDelay := 200;
  FMarquee := FALSE;
end;

destructor TSDProgressBarIndeterminate.Destroy();
begin
  inherited;
end;

function TSDProgressBarIndeterminate.GetMarquee(): boolean;
begin
  Result := FMarquee;
end;

procedure TSDProgressBarIndeterminate.SetMarquee(newValue: boolean);
var
  lParam: integer;
begin
  lParam := 0;
  if newValue then
    begin
    lParam := 1;
    end;

  FMarquee := newValue;
  SendMessage(self.Handle, PBM_SETMARQUEE, lParam, FMarqueeUpdateDelay);
end;

END.

