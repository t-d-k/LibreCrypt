unit SDUSysUtils;

interface

uses
  SysUtils, Windows;

// Identical to FileAge(filename, datetime) in Delphi 2007
// This function included to allow same API call to be used in Delphi 7
function SDUFileAge(filename: string; var dateTime: TDateTime): boolean;

// Identical to FileAge(filename, datetime) in Delphi 2007
// This function included to allow same API call to be used in Delphi 7
function SDUFileTimestamps(
  const Filename: string;
  var CreationTime: TFileTime;
  var LastAccessTime: TFileTime;
  var LastWriteTime: TFileTime
): boolean;

// Copy of WideStrUtils.WStrLen for compatibility with Delphi 7
function SDUWStrLen(const Str: PWideChar): Cardinal;
// These are purely SDU functions
function SDUWStrLCopy(Dest: PWChar; const Source: PWChar; MaxLen: Cardinal): PWChar;
function SDUWStrPCopy(Dest: PWChar; const Source: WideString): PWChar;
function SDUWStrPLCopy(Dest: PWChar; const Source: WideString;
  MaxLen: Cardinal): PWChar;


implementation

{$IF CompilerVersion >= 18.5}
uses
  WideStrUtils;
{$ENDIF}

function SDUFileAge(filename: string; var dateTime: TDateTime): boolean;
{$IF CompilerVersion >= 18.5}
begin
  Result := FileAge(filename, dateTime);
end;
{$ELSE}
var
  age: integer;
  retval: boolean;
begin
  retval := FALSE;

  age := FileAge(filename);
  if (age <> -1) then
    begin
    dateTime := FileDateToDateTime(age);
    retval := TRUE;
    end;

  Result := retval;
end;
{$ENDIF}


function SDUFileTimestamps(
  const Filename: string;
  var CreationTime: TFileTime;
  var LastAccessTime: TFileTime;
  var LastWriteTime: TFileTime
): boolean;
var
  searchRec: TSearchRec;
  retval: boolean;
begin
  retval := FALSE;
{$WARN SYMBOL_PLATFORM OFF}
  if (SysUtils.FindFirst(filename, faAnyFile, searchRec) = 0) then
    begin
    CreationTime   := searchRec.FindData.ftCreationTime;
    LastAccessTime := searchRec.FindData.ftLastAccessTime;
    LastWriteTime  := searchRec.FindData.ftLastWriteTime;
    SysUtils.FindClose(searchRec);
    retval:= TRUE;
    end;
{$WARN SYMBOL_PLATFORM ON}

  Result := retval;
end;

// ----------------------------------------------------------------------------
{$IF CompilerVersion >= 18.5}
function SDUWStrLen(const Str: PWideChar): Cardinal;
begin
  Result := WStrLen(Str);
end;
{$ELSE}
function SDUWStrLen(const Str: PWideChar): Cardinal;
var
  P : PWideChar;
begin
  P := Str;
  while (P^ <> #0) do Inc(P);
  Result := (P - Str);
end;
{$ENDIF}

// ----------------------------------------------------------------------------
function SDUWStrLCopy(Dest: PWChar; const Source: PWChar; MaxLen: Cardinal): PWChar;
var
  i: integer;
begin
  for i:=1 to MaxLen do
    begin
    Dest[i] := Source[i];
    end;
  Dest[MaxLen+1] := WChar($0000);

  Result := Dest;
end;

function SDUWStrPCopy(Dest: PWChar; const Source: WideString): PWChar;
begin
  Result := SDUWStrLCopy(Dest, PWChar(Source), Length(Source));
end;

function SDUWStrPLCopy(Dest: PWChar; const Source: WideString;
  MaxLen: Cardinal): PWChar;
begin
  Result := SDUWStrLCopy(Dest, PWChar(Source), MaxLen);
end;

// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

END.

