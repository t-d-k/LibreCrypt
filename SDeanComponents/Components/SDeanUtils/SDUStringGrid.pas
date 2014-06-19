unit SDUStringGrid;
// Description: StringGrid with extra functionality
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Grids;

type
  TSDUStringGrid = class(TStringGrid)
  protected
    function  GetColVisible(ACol: integer): boolean;
    procedure SetColVisible(ACol: integer; visible: boolean);
  public
    procedure SortGrid(colIdx: integer);
    procedure AutoSizeGridColumn(ACol: integer);
    property  ColVisible[ACol: integer]: boolean read GetColVisible write SetColVisible;
  end;

procedure Register;

implementation

uses
  Classes,
  Math;

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUStringGrid]);
end;

// Sort grid by column colIdx
// Note: Ignores fixed column
procedure TSDUStringGrid.SortGrid(colIdx: integer);
var
  stlColContent: TStringList;
  i: integer;
  j: integer;
  newPositions: array of integer;
begin
  stlColContent:= TStringList.Create();
  try
    // Make a temp copy of the column's contents...
    stlColContent.Assign(self.Cols[colIdx]);

    // Mark each item's current position in the grid
    for i:=0 to (stlColContent.count - 1) do
      begin
      stlColContent.Objects[i] := TObject(i);
      end;

    // Delete fixed rows...
    for i:=1 to self.FixedRows do
      begin
      stlColContent.Delete(0);
      end;

    SetLength(newPositions, stlColContent.count);

    // Sort...
    stlColContent.Sorted := TRUE;

    for i:=0 to (stlColContent.count - 1) do
      begin
      newPositions[i] := integer(stlColContent.Objects[i]);
      end;


    for i:=low(newPositions) to high(newPositions) do
      begin
      self.moverow(newPositions[i], i+1);
      for j:=i+1 to high(newPositions) do
        begin
        if (newPositions[j] < newPositions[i]) then
          begin
          newPositions[j] := newPositions[j] + 1;
          end;
        end;
      end;

  finally
    stlColContent.Free();
  end;

end;

function TSDUStringGrid.GetColVisible(ACol: integer): boolean;
begin
  Result := (ColWidths[ACol] = 0);
end;

procedure TSDUStringGrid.SetColVisible(ACol: integer; visible: boolean);
begin
  if visible then
    begin
    // This is wrong - should revert the width back to whatever it was
    // previously
    AutoSizeGridColumn(ACol)
    end
  else
    begin
    // Crude, but...
    // Note: If this is changed, GetColVisible(...) must also be changed
    ColWidths[ACol] := 0;
    end;
end;

// Automatically resize a TStringGrid column to the width of the widest string
// it holds
procedure TSDUStringGrid.AutoSizeGridColumn(ACol: integer);
var
  i: integer;
  maxX: integer;
begin
  maxX := 0;
  for i:=0 to (RowCount - 1) do
    begin
    maxX := max(Canvas.TextWidth(cells[ACol, i]), maxX);
    end;

  ColWidths[ACol] := maxX + GridLineWidth + 3;

end;


END.

