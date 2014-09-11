unit SDUi18n_mo;

// .mo file format described at:
//   http://www.gnu.org/software/hello/manual/gettext/MO-Files.html

interface

uses
  Windows, Classes,
  SDUClasses;

type
  TMOHeader = record
    Magic: DWORD;
    FileFormatRevision: DWORD;
    NumberOfStrings: DWORD;
    OffsetOriginal: DWORD;
    OffsetTranslated: DWORD;
    HashTableSize: DWORD;
    HashTableOffset: DWORD;
  end;

  TTransObj = class
  protected
    FHeader: TMOHeader;
    FFile: TSDUFileStream;

    FOriginal: TStringList;
    FTranslated: TStringList;
  public
    constructor Create();
    destructor  Destroy();

    function Load(filename: WideString): boolean;
    procedure Unload();

    function GetString_Original(idx: DWORD): WideString;
    function GetString_Translated(idx: DWORD): WideString;

    function GetText(original: WideString): WideString; overload;
    function GetText(original: WideString; plural: integer): WideString; overload;

  end;

implementation

uses
  SysUtils,
  SDUGeneral;

const
  MO_MAGIC_1 = $950412de;
  MO_MAGIC_2 = $de120495;

constructor TTransObj.Create();
begin
  inherited;
  FFile := nil;

  FOriginal:= nil;
  FTranslated:= nil;

end;

destructor  TTransObj.Destroy();
begin
  Unload();
  inherited;
end;

function TTransObj.Load(filename: WideString): boolean;
var
  retval: boolean;
  theText: WideString;
  lenStr: DWORD;
  i: DWORD;
  offset: ULONGLONG;
begin
  retval := FALSE;

  Unload();

  FFile := TSDUFileStream.Create(filename, (fmOpenRead or fmShareDenyNone));

  if (FFile <> nil) then
    begin
    FHeader.Magic              := FFile.ReadDWORD_LE();
    
    if (
        (FHeader.Magic = MO_MAGIC_1) or
        (FHeader.Magic = MO_MAGIC_2)
       ) then
      begin
      FHeader.FileFormatRevision := FFile.ReadDWORD_LE();
      FHeader.NumberOfStrings    := FFile.ReadDWORD_LE();
      FHeader.OffsetOriginal     := FFile.ReadDWORD_LE();
      FHeader.OffsetTranslated   := FFile.ReadDWORD_LE();
      FHeader.HashTableSize      := FFile.ReadDWORD_LE();
      FHeader.HashTableOffset    := FFile.ReadDWORD_LE();

      FOriginal:= TStringList.Create();
      for i:=1 to FHeader.NumberOfStrings do
        begin
        FFile.Position := FHeader.OffsetOriginal + ((i-1)*8);
        lenStr := FFile.ReadDWORD_LE();
        offset := FFile.ReadDWORD_LE();
        FFile.Position := offset;
        theText := FFile.ReadString(lenStr);
        FOriginal.Add(theText);
        end;

      FTranslated:= TStringList.Create();
      for i:=1 to FHeader.NumberOfStrings do
        begin
        FFile.Position := FHeader.OffsetTranslated + ((i-1)*8);
        lenStr := FFile.ReadDWORD_LE();
        offset := FFile.ReadDWORD_LE();
        FFile.Position := offset;
        theText := FFile.ReadString(lenStr);
        FTranslated.Add(theText);
        end;

      retval := TRUE;
      end;
    end;

  if not(retval) then
    begin
    Unload();
    end;

  Result := retval;
end;

procedure TTransObj.Unload();
begin
  if (FFile <> nil) then
    begin
    FFile.Free();
    end;

  if (FOriginal <> nil) then
    begin
    FOriginal.Free();
    end;
    
  if (FTranslated <> nil) then
    begin
    FTranslated.Free();
    end;

end;

function TTransObj.GetString_Original(idx: DWORD): WideString;
begin
  Result := FOriginal[idx];
end;

function TTransObj.GetString_Translated(idx: DWORD): WideString;
begin
  Result := FTranslated[idx];
end;

function TTransObj.GetText(original: WideString): WideString;
var
  i: DWORD;
  retval: WideString;
begin
  retval := '';
  for i:=0 to (FHeader.NumberOfStrings-1) do
    begin
    if (GetString_Original(i) = original) then
      begin
      retval := GetString_Translated(i);
      break
      end;
    end;

  if (retval = '') then
    begin
    retval := original;
    end;

  Result := retval;
end;

function TTransObj.GetText(original: WideString; plural: integer): WideString; overload;
begin
lplp
end;

END.

