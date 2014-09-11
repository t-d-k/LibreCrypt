unit SDUClasses;

interface

uses
  Classes, Windows;

type
  TSDUMemoryStream = class(TMemoryStream)
  private
    procedure GotoOffset(offset: int64);
  public
    function  ReadByte(offset: int64 = -1): byte;
    procedure WriteByte(x: byte; offset: int64 = -1; cnt: integer = -1);

    // Read/write little endian WORD
    function  ReadWORD_LE(offset: int64 = -1): WORD;
    procedure WriteWORD_LE(x: WORD; offset: int64 = -1);
    // Read/write big endian WORD
    function  ReadWORD_BE(offset: int64 = -1): WORD;
    procedure WriteWORD_BE(x: WORD; offset: int64 = -1);

    // Read/write little endian DWORD
    function  ReadDWORD_LE(offset: int64 = -1): DWORD;
    procedure WriteDWORD_LE(x: DWORD; offset: int64 = -1);
    // Read/write big endian DWORD
    function  ReadDWORD_BE(offset: int64 = -1): DWORD;
    procedure WriteDWORD_BE(x: DWORD; offset: int64 = -1);

    function  ReadString(size: integer; offset: integer = -1): ansistring;
    procedure WriteString(data: ansistring; maxChars: integer = -1; offset: integer = -1);
    function  ReadWideString(size: integer; offset: integer = -1): WideString;
    procedure WriteWideString(data: WideString; offset: integer = -1);

  end;

  TSDUFileStream = class(TFileStream)
  private
    procedure GotoOffset(offset: int64);
  public
    function  ReadByte(offset: int64 = -1): byte;
    procedure WriteByte(x: byte; offset: int64 = -1; cnt: integer = -1);

    // Read/write little endian WORD
    function  ReadWORD_LE(offset: int64 = -1): WORD;
    procedure WriteWORD_LE(x: WORD; offset: int64 = -1);
    // Read/write big endian WORD
    function  ReadWORD_BE(offset: int64 = -1): WORD;
    procedure WriteWORD_BE(x: WORD; offset: int64 = -1);

    // Read/write little endian DWORD
    function  ReadDWORD_LE(offset: int64 = -1): DWORD;
    procedure WriteDWORD_LE(x: DWORD; offset: int64 = -1);
    // Read/write big endian DWORD
    function  ReadDWORD_BE(offset: int64 = -1): DWORD;
    procedure WriteDWORD_BE(x: DWORD; offset: int64 = -1);

    function  ReadString(size: integer; offset: integer = -1): string;
    procedure WriteString(data: string; maxChars: integer = -1; offset: integer = -1);
    function  ReadWideString(size: integer; offset: integer = -1): WideString;
    procedure WriteWideString(data: WideString; offset: integer = -1);

  end;

implementation

uses
  Math,
  SDUGeneral;

// ==========================================================================
// ==========================================================================
procedure TSDUMemoryStream.GotoOffset(offset: int64);
begin
  if (offset >= 0) then
    begin
    self.Position := offset;
    end;
end;

function TSDUMemoryStream.ReadByte(offset: int64 = -1): byte;
var
  retval: byte;
begin
  GotoOffset(offset);
  self.Read(retval, sizeof(retval));
  Result := retval;
end;

procedure TSDUMemoryStream.WriteByte(x: byte; offset: int64 = -1; cnt: integer = -1);
var
  i: integer;
begin
  GotoOffset(offset);

  if (cnt < 0) then
    begin
    cnt := 1;
    end;

  for i:=1 to cnt do
    begin
    self.Write(x, sizeof(x));
    end;
end;

// Read/write little endian DWORD
function TSDUMemoryStream.ReadDWORD_LE(offset: int64 = -1): DWORD;
var
  retval: DWORD;
begin
  GotoOffset(offset);
  self.Read(retval, sizeof(retval));
  Result := retval;
end;

procedure TSDUMemoryStream.WriteDWORD_LE(x: DWORD; offset: int64 = -1);
begin
  GotoOffset(offset);
  self.Write(x, sizeof(x));
end;

// Read/write big endian DWORD
function TSDUMemoryStream.ReadDWORD_BE(offset: int64 = -1): DWORD;
var
  tmpDWORD: DWORD;
begin
  GotoOffset(offset);
  tmpDWORD := ReadDWORD_LE();
  Result := SDUConvertEndian(tmpDWORD);
end;

procedure TSDUMemoryStream.WriteDWORD_BE(x: DWORD; offset: int64 = -1);
var
  tmpDWORD: DWORD;
begin
  GotoOffset(offset);
  tmpDWORD := SDUConvertEndian(x);
  WriteDWORD_LE(tmpDWORD);
end;

// Read/write little endian WORD
function TSDUMemoryStream.ReadWORD_LE(offset: int64 = -1): WORD;
var
  retval: WORD;
begin
  GotoOffset(offset);
  self.Read(retval, sizeof(retval));
  Result := retval;
end;

procedure TSDUMemoryStream.WriteWORD_LE(x: WORD; offset: int64 = -1);
begin
  GotoOffset(offset);
  self.Write(x, sizeof(x));
end;

// Read/write big endian WORD
function TSDUMemoryStream.ReadWORD_BE(offset: int64 = -1): WORD;
var
  tmpWORD: WORD;
begin
  GotoOffset(offset);
  tmpWORD := ReadWORD_LE();
  Result := SDUConvertEndian(tmpWORD);
end;

procedure TSDUMemoryStream.WriteWORD_BE(x: WORD; offset: int64 = -1);
var
  tmpWORD: WORD;
begin
  GotoOffset(offset);
  tmpWORD := SDUConvertEndian(x);
  WriteWORD_LE(tmpWORD);
end;

function TSDUMemoryStream.ReadString(size: integer; offset: integer = -1): ansistring;
var
  retval: ansistring;
  i: integer;
begin
  retval := '';

  GotoOffset(offset);
  for i:=0 to (size - 1) do
    begin
    retval := retval + ansichar(self.ReadByte());
    end;

  Result := retval;
end;

procedure TSDUMemoryStream.WriteString(data: ansistring; maxChars: integer = -1; offset: integer = -1);
begin
  GotoOffset(offset);
  if (maxChars < 0) then
    begin
    maxChars := length(data);
    end;

  maxChars := min(maxChars, length(data));

  Write(PansiChar(data)^, maxChars);
end;

// Note: Size here is in *bytes*
function TSDUMemoryStream.ReadWideString(size: integer; offset: integer = -1): WideString;
var
  retval: WideString;
  i: integer;
begin
  retval := '';

  GotoOffset(offset);
  for i:=0 to ((size - 2) div 2) do
    begin
    retval := retval + WideChar(self.ReadWORD_LE());
    end;

  Result := retval;
end;

procedure TSDUMemoryStream.WriteWideString(data: WideString; offset: integer = -1);
begin
  GotoOffset(offset);
  Write(PWideChar(data)^, (length(data) * sizeof(WideChar)));
end;


// ==========================================================================
// ==========================================================================
procedure TSDUFileStream.GotoOffset(offset: int64);
begin
  if (offset >= 0) then
    begin
    self.Position := offset;
    end;
end;

function TSDUFileStream.ReadByte(offset: int64 = -1): byte;
var
  retval: byte;
begin
  GotoOffset(offset);
  self.Read(retval, sizeof(retval));
  Result := retval;
end;

procedure TSDUFileStream.WriteByte(x: byte; offset: int64 = -1; cnt: integer = -1);
var
  i: integer;
begin
  GotoOffset(offset);

  if (cnt < 0) then
    begin
    cnt := 1;
    end;

  for i:=1 to cnt do
    begin
    self.Write(x, sizeof(x));
    end;
end;

// Read/write little endian DWORD
function TSDUFileStream.ReadDWORD_LE(offset: int64 = -1): DWORD;
var
  retval: DWORD;
begin
  GotoOffset(offset);
  self.Read(retval, sizeof(retval));
  Result := retval;
end;

procedure TSDUFileStream.WriteDWORD_LE(x: DWORD; offset: int64 = -1);
begin
  GotoOffset(offset);
  self.Write(x, sizeof(x));
end;

// Read/write big endian DWORD
function TSDUFileStream.ReadDWORD_BE(offset: int64 = -1): DWORD;
var
  tmpDWORD: DWORD;
begin
  GotoOffset(offset);
  tmpDWORD := ReadDWORD_LE();
  Result := SDUConvertEndian(tmpDWORD);
end;

procedure TSDUFileStream.WriteDWORD_BE(x: DWORD; offset: int64 = -1);
var
  tmpDWORD: DWORD;
begin
  GotoOffset(offset);
  tmpDWORD := SDUConvertEndian(x);
  WriteDWORD_LE(tmpDWORD);
end;

// Read/write little endian WORD
function TSDUFileStream.ReadWORD_LE(offset: int64 = -1): WORD;
var
  retval: WORD;
begin
  GotoOffset(offset);
  self.Read(retval, sizeof(retval));
  Result := retval;
end;

procedure TSDUFileStream.WriteWORD_LE(x: WORD; offset: int64 = -1);
begin
  GotoOffset(offset);
  self.Write(x, sizeof(x));
end;

// Read/write big endian WORD
function TSDUFileStream.ReadWORD_BE(offset: int64 = -1): WORD;
var
  tmpWORD: WORD;
begin
  GotoOffset(offset);
  tmpWORD := ReadWORD_LE();
  Result := SDUConvertEndian(tmpWORD);
end;

procedure TSDUFileStream.WriteWORD_BE(x: WORD; offset: int64 = -1);
var
  tmpWORD: WORD;
begin
  GotoOffset(offset);
  tmpWORD := SDUConvertEndian(x);
  WriteWORD_LE(tmpWORD);
end;

function TSDUFileStream.ReadString(size: integer; offset: integer = -1): string;
var
  retval: string;
  i: integer;
begin
  retval := '';

  GotoOffset(offset);
  for i:=0 to (size - 1) do
    begin
    retval := retval + chr(self.ReadByte());
    end;

  Result := retval;
end;

procedure TSDUFileStream.WriteString(data: string; maxChars: integer = -1; offset: integer = -1);
begin
  GotoOffset(offset);
  if (maxChars < 0) then
    begin
    maxChars := length(data);
    end;

  maxChars := min(maxChars, length(data));

  Write(PChar(data)^, maxChars);
end;

// Note: Size here is in *bytes*
function TSDUFileStream.ReadWideString(size: integer; offset: integer = -1): WideString;
var
  retval: WideString;
  i: integer;
begin
  retval := '';

  GotoOffset(offset);
  for i:=0 to ((size - 2) div 2) do
    begin
    retval := retval + WideChar(self.ReadWORD_LE());
    end;

  Result := retval;
end;

procedure TSDUFileStream.WriteWideString(data: WideString; offset: integer = -1);
begin
  GotoOffset(offset);
  Write(PWideChar(data)^, (length(data) * sizeof(WideChar)));
end;

// ==========================================================================

// ==========================================================================
// ==========================================================================

END.

