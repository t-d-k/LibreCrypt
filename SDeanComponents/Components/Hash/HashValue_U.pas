unit HashValue_U;
// Description: Hash Value
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  sysutils;

type
{$TYPEINFO ON} // Needed to allow "published"

  // Exceptions...
  EHashValue = class(Exception);
  EHashValueInvalidValue = EHashValue;

  // This use of this is *strongly* discouraged...
  THashArray = array [0..63] of byte;

  THashValue = class
  protected
    FValue: array of byte;

    function GetLength(): integer; virtual;
    function GetValueAsBinary(): string; virtual;
    function GetValueAsASCIIHex(): string; virtual;

    procedure SetValueAsBinary(newValue: string); virtual;
    procedure SetValueAsASCIIHex(newValue: string); virtual;

  public
    constructor Create(); virtual;
    destructor Destroy(); override;

    procedure Clear(); virtual;

  published
    property Length: integer read GetLength;

    property ValueAsBinary: string read GetValueAsBinary write SetValueAsBinary;
    property ValueAsASCIIHex: string read GetValueAsASCIIHex write SetValueAsASCIIHex;

    function GetValueAsArray(): THashArray; virtual;
    procedure SetValueAsArray(newValue: THashArray; bits: integer); virtual;
  end;


implementation

uses
  SDUGeneral,
  Math;

constructor THashValue.Create();
begin
  inherited Create();
  SetLength(FValue, 0);
end;

destructor THashValue.Destroy();
begin
  Clear();
  inherited;
end;

procedure THashValue.Clear();
var
  i: integer;
begin
  for i:=low(FValue) to high(FValue) do
  begin
    FValue[i] := random(256);
  end;

end;

function THashValue.GetLength(): integer;
begin
  Result := system.Length(FValue);
end;

function THashValue.GetValueAsArray(): THashArray;
var
  i: integer;
  maxElem: integer;
  retVal: THashArray;
begin
  maxElem := min(
                 system.Length(FValue),
                 system.Length(retVal)
                );

  for i:=low(retVal) to high(retVal) do
    begin
    retVal[i] := 0;
    end;

  for i:=0 to (maxElem-1) do
    begin
    retVal[low(retVal) + i] := FValue[low(FValue) + i];
    end;

  Result := retVal;
end;

function THashValue.GetValueAsBinary(): string;
var
  i: integer;
  retVal: string;
begin
  retVal := '';
  for i:=low(FValue) to high(FValue) do
    begin
    retVal := retVal + char(FValue[i]);
    end;

  Result := retVal;
end;

function THashValue.GetValueAsASCIIHex(): string;
var
  i: integer;
  retVal: string;
begin
  retVal := '';
  for i:=low(FValue) to high(FValue) do
    begin
    retVal := retVal + inttohex(FValue[i], 2);
    end;

  Result := retVal;
end;

procedure THashValue.SetValueAsArray(newValue: THashArray; bits: integer);
var
  i: integer;
  maxElem: integer;
begin
  Clear();

  SetLength(FValue, (bits div 8));

  // Clear, in case number of bits is greater than the array passed in
  for i:=low(FValue) to high(FValue) do
    begin
    FValue[i] := 0;
    end;

  maxElem := min(
                 system.Length(FValue),
                 system.Length(newValue)
                );

  for i:=0 to (maxElem-1) do
    begin
    FValue[low(FValue) + i] := newValue[low(newValue) + i];
    end;

end;

procedure THashValue.SetValueAsBinary(newValue: string);
var
  i: integer;
begin
  Clear();

  SetLength(FValue, system.length(newValue));
  for i:=1 to system.length(newValue) do
    begin
    FValue[low(FValue) + i - 1] := byte(newValue[i]);
    end;

end;

procedure THashValue.SetValueAsASCIIHex(newValue: string);
var
  i: integer;
  byteAsASCIIHex: string;
  byteValue: integer;
begin
  Clear();

  // Sanity check; must have an even number of hex characters
  if ((system.length(newValue) mod 2) = 1) then
    begin
    raise EHashValueInvalidValue.Create('Invalid number of hex digits in hash value');
    end;

  SetLength(FValue, system.Length(newValue));
  byteAsASCIIHex := '';
  for i:=1 to system.length(newValue) do
    begin
    byteAsASCIIHex := byteAsASCIIHex + newValue[i];
    if (system.length(byteAsASCIIHex) = 2) then
      begin
      if not(SDUTryHexToInt(byteAsASCIIHex, byteValue)) then
        begin
        raise EHashValueInvalidValue.Create('Invalid hex digits in hash value');
        end;

      FValue[low(FValue) + i - 1] := byteValue;
      byteAsASCIIHex := '';
      end;

    end;

end;

END.

