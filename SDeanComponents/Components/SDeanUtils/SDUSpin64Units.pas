unit SDUSpin64Units;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SDUFrames, StdCtrls, Spin64,
  SDUGeneral;

type
  TSDUSpin64Unit = class(TSDUFrame)
    se64Value: TSpinEdit64;
    cbUnits: TComboBox;
    procedure se64ValueChange(Sender: TObject);
    procedure cbUnitsChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FMultiplier: integer;
    FReadOnly: boolean;
  protected
    procedure DoShow(); override;
    procedure DoChange();

    function  GetPrettyValue(): string;

    function  GetValue(): int64;
    procedure SetValue(val: int64);
    function  GetUnits(): TStrings;
    procedure SetUnits(units: TStrings);
    function  GetMaxLength(): integer;
    procedure SetMaxLength(val: integer);
    function  GetMaxValue(): int64;
    procedure SetMaxValue(val: int64);
    function  GetMinValue(): int64;
    procedure SetMinValue(val: int64);
    function  GetSelectedUnits: string;
    procedure SetSelectedUnits(selUnits: string);
    function  GetMultiplier(): integer;
    procedure SetMultiplier(mult: integer);

    procedure SetReadOnly(ro: boolean);

  public
    constructor Create(AOwner: TComponent); override;

  published
    property Units: TStrings read GetUnits write SetUnits;
    property Multiplier: integer read GetMultiplier write SetMultiplier default 1000;

    property SelectedUnits: string read GetSelectedUnits write SetSelectedUnits;
    property MaxLength: integer read GetMaxLength write SetMaxLength;
    property MaxValue: int64 read GetMaxValue write SetMaxValue;
    property MinValue: int64 read GetMinValue write SetMinValue;

    property Value: int64 read GetValue write SetValue;
    property PrettyValue: string read GetPrettyValue;

    property ReadOnly: boolean read FReadOnly write SetReadOnly;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  end;

  TSDUSpin64Unit_Storage = class(TSDUSpin64Unit)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Multiplier: integer read GetMultiplier write SetMultiplier default 1024;
  end;

procedure Register;

implementation

{$R *.dfm}

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUSpin64Unit]);
  RegisterComponents('SDeanUtils', [TSDUSpin64Unit_Storage]);
end;

constructor TSDUSpin64Unit.Create(AOwner: TComponent);
begin
  inherited;
  Multiplier := 1000;
  ReadOnly := FALSE;
end;

function TSDUSpin64Unit.GetUnits(): TStrings;
begin
  Result := cbUnits.Items;
end;

procedure TSDUSpin64Unit.SetUnits(units: TStrings);
begin
  cbUnits.Items.Assign(units);
  
  // Select first unit
  if (units.count = 0) then
    begin
    cbUnits.ItemIndex := -1;
    end
  else
    begin
    cbUnits.ItemIndex := 0;
    end;
    
end;

procedure TSDUSpin64Unit.cbUnitsChange(Sender: TObject);
begin
  inherited;
  DoChange();
end;

procedure TSDUSpin64Unit.DoShow();
begin
  se64Value.left := 0;
  se64Value.top := 0;
  cbUnits.top := 0;

  self.height := se64Value.height;
  inherited;
end;

procedure TSDUSpin64Unit.se64ValueChange(Sender: TObject);
begin
  inherited;
  DoChange();
end;

procedure TSDUSpin64Unit.DoChange();
begin
  if Assigned(FOnChange) then
    begin
    FOnChange(self);
    end;
end;

function TSDUSpin64Unit.GetValue(): int64;
var
  i: integer;
  retval: int64;
  i64Multiplier: int64;
begin
  retval := se64Value.Value;

  // Use int64 to store multiplier, so Delphi doesn't cast int64s as integer
  i64Multiplier:= FMultiplier;

  // Start from 1 - we don't divide by 1000 on the single units
  for i:=1 to cbUnits.ItemIndex do
    begin
    retval := retval * i64Multiplier;
    end;

  Result := retval;
end;

procedure TSDUSpin64Unit.SetValue(val: int64);
var
  i: integer;
  i64Zero: int64;
  i64Multiplier: int64;
  unitsIdx: integer;
  useValue: int64;
begin
  // Use int64 to store multiplier, so Delphi doesn't cast int64s as integer
  i64Multiplier:= FMultiplier;
  // Use int64 to store 0, so Delphi doesn't cast int64s as integer
  i64Zero := 0;

  unitsIdx := 0;
  useValue := val;

  if (val <> i64Zero) then
    begin
    // Start from 1 - we don't divide by 1000 on the last one
    for i:=1 to (cbUnits.Items.Count - 1) do
      begin
      if ((useValue mod i64Multiplier) <> i64Zero) then
        begin
        break
        end;

      unitsIdx := i;
      useValue := (useValue div i64Multiplier);
      end;
    end;

  se64Value.Value := useValue;
  cbUnits.ItemIndex := unitsIdx;
end;

function TSDUSpin64Unit.GetMaxLength(): integer;
begin
  Result := se64Value.MaxLength;
end;

procedure TSDUSpin64Unit.SetMaxLength(val: integer);
begin
  se64Value.MaxLength := val
end;

function TSDUSpin64Unit.GetMinValue(): int64;
begin
  Result := se64Value.MinValue;
end;

procedure TSDUSpin64Unit.SetMinValue(val: int64);
begin
  se64Value.MinValue := val
end;

function TSDUSpin64Unit.GetMaxValue(): int64;
begin
  Result := se64Value.MaxValue;
end;

procedure TSDUSpin64Unit.SetMaxValue(val: int64);
begin
  se64Value.MaxValue := val
end;

function TSDUSpin64Unit.GetSelectedUnits: string;
begin
  Result := cbUnits.Items[cbUnits.ItemIndex];
end;

procedure TSDUSpin64Unit.SetSelectedUnits(selUnits: string);
begin
  cbUnits.ItemIndex := cbUnits.items.IndexOf(selUnits);
end;

function TSDUSpin64Unit.GetMultiplier(): integer;
begin
  Result := FMultiplier;
end;

procedure TSDUSpin64Unit.SetMultiplier(mult: integer);
begin
  FMultiplier := mult;
end;

function TSDUSpin64Unit.GetPrettyValue(): string;
begin
  Result := inttostr(se64Value.Value) + ' ' + SelectedUnits;
end;

procedure TSDUSpin64Unit.SetReadOnly(ro: boolean);
begin
  inherited;

  FReadOnly:= ro;

  SDUReadonlyControl(se64Value, FReadOnly);
  SDUReadonlyControl(cbUnits, FReadOnly);

end;

constructor TSDUSpin64Unit_Storage.Create(AOwner: TComponent);
var
  stlTmp: TStringList;
  i: TUnits_Storage;
begin
  inherited;

  Multiplier := UNITS_BYTES_MULTIPLIER;

  stlTmp:= TStringList.Create();
  try
    for i:=low(i) to high(i) do
      begin
      stlTmp.Add(SDUUnitsStorageToText(i));
      end;

    Units := stlTmp;
  finally
    stlTmp.Free();
  end;

end;

// ============================================================================
// ============================================================================
// ============================================================================

END.


