unit SDUStdCtrls;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls;
//  StdCtrls, Messages, Controls, Graphics, Classes;

type
  TSDULabel = class(TLabel);

  TSDUURLLabel = class(TSDULabel)
  private
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    FHoverColor: TColor;
    FNormalColor: TColor;
    FURL: string;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Click(); override;

  published
    property NormalColor: TColor read FNormalColor write FNormalColor;
    property HoverColor: TColor read FHoverColor write FHoverColor;
    property URL: string read FURL write FURL;

  end;

  TSDUFilenameLabel = class(TSDULabel)
  protected
    function GetLabelText(): string; override;
  end;

  TSDUTruncatingLabel = class(TSDULabel)
  protected
    function GetLabelText(): string; override;
  end;

  TSDUCheckBox = class(TCheckBox)
  private
    FCheckWidth: integer;
    FSDUAutoSize: boolean;

    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

    procedure GetCheckWidth();
  protected
    procedure AdjustBounds(); dynamic;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property AutoSize: boolean read FSDUAutoSize write FSDUAutoSize;

  end;

procedure Register;

implementation

uses
{$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  ShellAPI; //Windows;

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUURLLabel]);
  RegisterComponents('SDeanUtils', [TSDUFilenameLabel]);
  RegisterComponents('SDeanUtils', [TSDUTruncatingLabel]);
  RegisterComponents('SDeanUtils', [TSDUCheckBox]);
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
constructor TSDUURLLabel.Create(AOwner: TComponent);
begin
  inherited;
  NormalColor:= clBlue;
  HoverColor:= clRed;
  URL := '';

  Font.Color := NormalColor;
  Font.Style := Font.Style + [fsUnderline];

  Cursor := crHandPoint;

  Invalidate;

end;

destructor TSDUURLLabel.Destroy();
begin

  inherited;
end;

procedure TSDUURLLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;

  Font.Color := HoverColor;
  Repaint;

end;

procedure TSDUURLLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  Font.Color := NormalColor;
  Invalidate;

end;

procedure TSDUURLLabel.Click();
begin
  inherited;

  if (URL <> '') then
    begin
    ShellExecute(
                 Parent.Handle,
                 PChar('open'),
                 PChar(URL),
                 PChar(''),
                 PChar(''),
                 SW_SHOW
                );
    end;
    
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function TSDUFilenameLabel.GetLabelText(): string;
begin
  Result := MinimizeName(Caption, Canvas, Width);
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
function TSDUTruncatingLabel.GetLabelText(): string;
var
  base: string;
  retval: string;
begin
  retval := Caption;

  if (retval = '') then
    begin
    // Do nothing - just return the empty string
    end
  else
    begin
    if (Canvas.TextExtent(retval).cx > Width) then
      begin
      base := retval;
      retval := base + '...';
      while (
             (Canvas.TextExtent(retval).cx > Width) and
             (retval <> '')
            ) do
        begin
        if (base = '') then
          begin
          Delete(retval, length(retval), 1);
          end
        else
          begin
          Delete(base, length(base), 1);
          retval := base + '...';
          end;
        end;
      end;
    end;

  Result := retval;
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
procedure TSDUCheckBox.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
//  X: Integer;
//  AAlignment: TAlignment;
  oldFont: HFONT;
  area: TRect;
begin
//  if not (csReading in ComponentState) then //and AutoSize then

  if not(AutoSize) then
    begin
    inherited;
    end
  else
    begin
    area := ClientRect;
    DC := GetDC(self.Handle);
    oldFont := SelectObject(dc, Font.Handle);

    DrawText(
             dc,
             PChar(Caption),
             Length(Caption),
             area,
             (
              DT_NOCLIP or
              DT_EXPANDTABS or
              DT_CALCRECT or
              WordWraps[WordWrap]
             )
            );

    SelectObject(dc, oldFont);
    ReleaseDC(self.Handle, dc);

{
    X := Left;
    AAlignment := Alignment;
    if UseRightToLeftAlignment then ChangeBiDiModeAlignment(AAlignment);
    if AAlignment = taRightJustify then Inc(X, Width - area.Right);
}
//    SetBounds(X, Top, area.Right+20, area.Bottom);

    // +10 - from empirical testing should be enough
    self.Width := area.Right + FCheckWidth + 10;
    self.Height := area.Bottom;
    end;

end;

procedure TSDUCheckBox.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
  AdjustBounds;
end;

procedure TSDUCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TSDUCheckBox.GetCheckWidth();
var
  tmpBmp: TBitmap;
begin
  // This method to get the checkbox size taken from CheckLst.pas
  tmpBmp := TBitmap.Create();
  try
    tmpBmp.Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));
    FCheckWidth := tmpBmp.Width div 4;
  finally
    tmpBmp.Free();
  end;
end;

constructor TSDUCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  GetCheckWidth();
end;

// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

END.


