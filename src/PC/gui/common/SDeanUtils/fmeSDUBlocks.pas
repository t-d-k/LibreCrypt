unit fmeSDUBlocks;


 // NOTE: When the panel has *no* blocks, the control's Caption will be shown
 //       instead

 {shows a chart of diff colour blocks eg. for partitions display }
 { TODO -otdk -cenhance : pos replace with chart component }
interface

uses
  Classes,
//  fmeBaseOptions,
  CommonSettings, Controls, Dialogs, ExtCtrls,
  Forms,
  MainSettings,
  Graphics, Messages, SysUtils, Types
  , Variants, Windows, Vcl.StdCtrls;

type
  // Note: Both Caption and SubCaption can have CRLFs (e.g. SDUCRLF) in them
  //       to insert newlines 
  TBlock = record
    Caption:    String;
    SubCaption: String;
    Percentage: Double;
    BkColor:    TColor;

    Data: TObject;
  end;


  TfmeSDUBlocks = class (TFrame)
    Label1: TLabel;
  PROTECTED
    FLastXY:     TPoint;
    FBlocks:     array of TBlock;
    FBlockRects: array of TRect;
    FSelected:   Integer;
    FOnChanged:  TNotifyEvent;
    FCanvas :TCanvas;

    procedure Click; OVERRIDE;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); OVERRIDE;

    procedure WMGetDlgCode(var Message: TWMGetDlgCode); MESSAGE WM_GETDLGCODE;
    procedure KeyDown(var Key: Word; Shift: TShiftState); OVERRIDE;
    procedure KeyPress(var Key: Char); OVERRIDE;
    procedure DoEnter; OVERRIDE;

    procedure DrawBlock(idx: Integer);
    procedure RemoveAllBlocks();

    procedure SetSelected(idx: Integer);
      procedure PaintWindow(DC: HDC); OVERRIDE;
  PUBLIC
    constructor Create(AOwner: TComponent); OVERRIDE;
    destructor Destroy(); OVERRIDE;

    procedure Clear(); VIRTUAL;
    procedure Add(blk: TBlock); VIRTUAL;
    function GetItem(idx: Integer): TBlock; VIRTUAL;
    procedure SetItem(idx: Integer; blk: TBlock); VIRTUAL;
    function GetCount(): Integer; VIRTUAL;



  PUBLIC
    property Item[idx: Integer]: TBlock Read GetItem Write SetItem;
    procedure SetCaption(cap: String);
  PUBLISHED
    property Count: Integer Read GetCount;
    property Selected: Integer Read FSelected Write SetSelected;
    property OnChanged: TNotifyEvent Read FOnChanged Write FOnChanged;
  end;

// procedure Register;

implementation

{$R *.dfm}

uses
  GraphUtil, Math;

{
this frame has  a control on it that isnt used, because that forces PaintWindow to be called, see:
http://stackoverflow.com/questions/10681027/what-is-the-most-safe-and-correct-way-to-paint-on-a-tframe-surface
cant set DoubleBuffered := true; bc may be overridden
}
constructor TfmeSDUBlocks.Create(AOwner: TComponent);
begin
  inherited;

  FLastXY := Point(-1, -1);

  // Change TPanel default; we accept keypresses
  TabStop := True;

  RemoveAllBlocks();
  FCanvas := TCanvas.Create();
end;

destructor TfmeSDUBlocks.Destroy();
begin
 FCanvas.Free();
  inherited;
end;

procedure TfmeSDUBlocks.RemoveAllBlocks();
begin
  SetLength(FBlocks, 0);
  SetLength(FBlockRects, 0);
end;


procedure TfmeSDUBlocks.Clear();
begin
  RemoveAllBlocks();
  // Flag as none selected
  FSelected := -1;
end;

procedure TfmeSDUBlocks.Add(blk: TBlock);
begin
  SetLength(FBlocks, (length(FBlocks) + 1));
  FBlocks[length(FBlocks) - 1] := blk;

  SetLength(FBlockRects, (length(FBlockRects) + 1));
  FBlockRects[length(FBlockRects) - 1] := Rect(0, 0, 0, 0);

  Invalidate();
end;

function TfmeSDUBlocks.GetItem(idx: Integer): TBlock;
begin
  Result := FBlocks[idx];
end;

procedure TfmeSDUBlocks.SetCaption(cap: String);
begin
  Caption := cap;
end;

procedure TfmeSDUBlocks.SetItem(idx: Integer; blk: TBlock);
begin
  FBlocks[idx] := blk;
  Invalidate();
end;

procedure TfmeSDUBlocks.PaintWindow(DC: HDC);
var
  i:            Integer;
  x:            Integer;
  useWidth:     Integer;
  currBlkWidth: Integer;
  BevelPixels:  Integer;
  useRect:      TRect;
  currRight:    Integer;
begin
  inherited;

  if (length(FBlocks) > 0) then begin
    BevelPixels := 0;
    if (BevelInner <> bvNone) then
      Inc(BevelPixels, BevelWidth);
    if (BevelOuter <> bvNone) then
      Inc(BevelPixels, BevelWidth);

    useRect := Rect(0, 0, ClientWidth, ClientHeight);
    InflateRect(useRect, -BevelPixels, -BevelPixels);

    // Purge display
    FCanvas.Handle := DC;
    fcanvas.Brush.Color := Color;
    fcanvas.FillRect(useRect);

    x        := useRect.Left;
    useWidth := useRect.Right - useRect.Left;
    for i := low(FBlocks) to high(FBlocks) do begin
      currBlkWidth := trunc((useWidth / 100) * FBlocks[i].Percentage);

      // Ensure we don't go over the bevel/edge of the control...
      currRight := min(x + currBlkWidth, useRect.Right);

      FBlockRects[i] := Rect(x, useRect.Top, currRight, useRect.Bottom);
      DrawBlock(i);

      // +1 to give a slight 
      x := x + currBlkWidth + 1;
    end;
  end;

end;

procedure TfmeSDUBlocks.DrawBlock(idx: Integer);
var
  insideBounds:  TRect;
  textHeight:    Integer;
  blk:           TBlock;
  tmpBounds:     TRect;
  normalFGColor: TColor;
  normalBKColor: TColor;
  stlTextLines:  TStringList;
  i:             Integer;
  textTop:       Integer;
begin
  inherited;

  blk := FBLocks[idx];

  // Clear any lines on the display
  // We surround the Canvas blanking with "parent<>nil" to avoid getting
  // "Control '' has no parent window" errors when the component is dropped
  // onto a form
  if (parent <> nil) then begin
    normalFGColor := Font.Color;
    normalBKColor := Color;
    if (blk.BkColor <> clNone) then begin
      normalBKColor := blk.BkColor;
    end;

    fCanvas.Brush.Color := normalBKColor;
    fCanvas.Brush.Style := bsSolid;

    insideBounds := FBlockRects[idx];
    fCanvas.FillRect(insideBounds);
    InflateRect(insideBounds, -4, -4);

    fCanvas.Pen.Color   := normalFGColor;
    fCanvas.Brush.Color := normalBKColor;
    fCanvas.Brush.Style := bsClear;

    if (Selected = idx) then begin
      fCanvas.Pen.Color   := GetHighLightColor(normalFGColor);
      fCanvas.Brush.Color := GetHighLightColor(normalBKColor);
      // Refill the reduced rect to give highlighted background
      fCanvas.FillRect(insideBounds);
    end;

    fCanvas.Brush.Style := bsClear;

    fCanvas.Font.Style := Font.Style + [fsBold];
    textHeight                      := fCanvas.TextHeight('X');

    // We use a TStringList to split the caption up into separate lines if it's
    // for SDUCRLF's in it, then TextRect(...) the strings out as TextRect(...)
    // doesn't honour CRLFs, or "\n" in strings 
    stlTextLines := TStringList.Create();
    try
      stlTextLines.Text := blk.Caption;
      textTop           := insideBounds.top;
      for i := 0 to (stlTextLines.Count - 1) do begin
        fCanvas.TextRect(
          insideBounds,
          insideBounds.left,
          textTop,
          stlTextLines[i]
          );
        textTop := textTop + textHeight;
      end;

      fCanvas.Font.Style := Font.Style - [fsBold];
      // Changed style could have altered the height of the font
      textHeight                      := fCanvas.TextHeight('X');

      stlTextLines.Text := blk.SubCaption;
      // textTop already set appropriately at this point
      for i := 0 to (stlTextLines.Count - 1) do begin
        fCanvas.TextRect(
          insideBounds,
          insideBounds.left,
          textTop,
          stlTextLines[i]
          );
        textTop := textTop + textHeight;
      end;

    finally
      stlTextLines.Free();
    end;

    // Cell frame...
    fCanvas.Brush.Style := bsSolid;
    fCanvas.Brush.Color := clBlack;
    fCanvas.FrameRect(FBlockRects[idx]);

    // Selected dottec focus rect..
    if (Selected = idx) then begin
      tmpBounds := FBlockRects[idx];
      InflateRect(tmpBounds, -1, -1);
      fCanvas.Brush.Color := color;
      fCanvas.DrawFocusRect(tmpBounds);
    end;

  end;

end;

procedure TfmeSDUBlocks.Click();
var
  i:       Integer;
  currBlk: TRect;
begin
  SetFocus();

  FSelected := -1;
  for i := low(FBlockRects) to high(FBlockRects) do begin
    currBlk := FBlockRects[i];

    if ((currBlk.left <= FLastXY.X) and (currBlk.right >= FLastXY.X) and
      (currBlk.top <= FLastXY.Y) and (currBlk.bottom >= FLastXY.Y)) then begin
      Selected := i;
      break;
    end;
  end;

  inherited;
end;

procedure TfmeSDUBlocks.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  FLastXY := Point(X, Y);
  inherited;
end;

procedure TfmeSDUBlocks.SetSelected(idx: Integer);
begin
  FSelected := idx;
  Invalidate();

  if Assigned(FOnChanged) then begin
    FOnChanged(self);
  end;
end;

function TfmeSDUBlocks.GetCount(): Integer;
begin
  Result := length(FBlocks);
end;

procedure TfmeSDUBlocks.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if ((Key = VK_LEFT) or (Key = VK_UP)) then begin
    if (Selected < low(FBlockRects)) then begin
      // None selected; select first
      Selected := low(FBlockRects);
    end else
    if (Selected = low(FBlockRects)) then begin
      // Do nothing; 1st one already selected
    end else begin
      Selected := Selected - 1;
    end;

  end else
  if ((Key = VK_RIGHT) or (Key = VK_DOWN)) then begin
    if ((Selected < low(FBlockRects)) or (Selected > high(FBlockRects)))
    then begin
      // None selected; select last
      Selected := high(FBlockRects);
    end else
    if (Selected = high(FBlockRects)) then begin
      // Do nothing; 1st one already selected
    end else begin
      Selected := Selected + 1;
    end;

  end;

end;

procedure TfmeSDUBlocks.KeyPress(var Key: Char);
begin
  //ko
end;

procedure TfmeSDUBlocks.DoEnter;
begin
  if (Selected < low(FBlockRects)) then begin
    Selected := low(FBlockRects);
  end;

  inherited;
end;

procedure TfmeSDUBlocks.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS;
end;

end.
