unit HashAlg_U;
// Description: Hash Algorithm Wrapper Base Class
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Classes,
  HashValue_U;

type
  THashAlg = class(TComponent)
  private
    { Private declarations }
  protected
    // These must be set in the constructor by descendant classes
    fTitle: string;
    fHashLength: integer;
    fBlockLength: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy(); override;

    // ------------------------------------------------------------------
    // Main methods
    //
    // Most users will only need to use the following methods:

    // Simple function to hash a string
    function  HashString(const theString: string; digest: THashValue): boolean; overload; virtual;

    // Simple function to hash a file's contents
    function  HashFile(const filename: string; digest: THashValue): boolean; overload; virtual;

    // Simple function to hash a chunk of memory
    function  HashMemory(const memBlock: Pointer; const len: cardinal; digest: THashValue): boolean; overload; virtual;

    // Simple function to hash a stream
    // Note: This will operate from the *current* *position* in the stream, right
    //       up to the end
    function  HashStream(const stream: TStream; digest: THashValue): boolean; virtual;

    // Generate a prettyprinted version of the supplied hash value, using the
    // conventional ASCII-hex byte order for displaying values by the hash
    // algorithm.
    // Note: ValueAsASCIIHex(...) can be called on hash value objects instead,
    //       though this will only give a non-prettyprinted version
    function  PrettyPrintHashValue(const theHashValue: THashValue): string; virtual;


    // ------------------------------------------------------------------
    // Lower level functions to incrementally generate a hash
    //
    // Most users probably won't need to use these
    // Note: Only Init(...), Update(<with byte array>), and Final(...) should
    //       be implemented in descendant classes. The others will call this in
    //       this class.
    // Usage: Call:
    //   Call Init(...)
    //   Call Update(...)
    //   Call Update(...)
    //     ...                (call Update(...) repeatedly until all  your data
    //     ...                is processed)
    //   Call Update(...)
    //   Call Final(...)
    procedure Init(); virtual; abstract;
    procedure Update(const input: string); overload; virtual;
    procedure Update(const input: array of byte; const inputLen: cardinal); overload; virtual; abstract;
    // Note: This will operate from the *current* *position* in the stream
    procedure Update(input: TStream; const inputLen: int64); overload; virtual;
    // Note: This will operate from the *current* *position* in the stream, right
    //       up to the end
    procedure Update(input: TStream); overload; virtual;
    procedure Final(digest: THashValue); virtual; abstract;


    // ------------------------------------------------------------------
    // Depreciated methods
    //
    // The following methods are depreciated, and should not be used in new
    // developments.
    // Use replacement HashString(...)
    function  HashString(theString: string): THashArray; overload; virtual; deprecated;
    // Use replacement HashMemory(...)
    function  HashMemory(memBlock: Pointer; len: cardinal): THashArray; overload; virtual; deprecated;
    // Use replacement HashFile(...)
    function  HashFile(filename: string; var digest: THashArray): boolean; overload; virtual; deprecated;
    // Use PrettyPrintHashValue(...)
    function  HashToDisplay(theHash: THashArray): string; virtual; deprecated;
    // Use THashValue.ValueAsBinary property
    function  HashToDataString(theHash: THashArray): string; virtual; deprecated;
    // Use THashValue.Clear(...)
    procedure ClearHash(var theHash: THashArray); virtual; deprecated;
    // Use HashLength property
    function  DigestSize(): integer; virtual; deprecated;
  published
    // Title of hash algorithm
    property Title: string read fTitle;
    // Length of output hashes (in bits)
    property HashLength: integer read fHashLength;
    // Block length (in bits)
    property BlockLength: integer read fBlockLength;

  end;


implementation

uses
  Math,
  sysutils,
  SDUGeneral;

const
  // This dictates the buffer size when processing streams (and files, etc)
  // Relativly arbitary (as long as you've got enough memory); you may well be
  // able to improve performace by increasing the size of this
  // !! WARNING !!
  // Increasing this can give stack overflows:
  //  *) When it gets to the stream handling part
  //  *) On the MD2 "transform" operation, with strings of about 30 chars or
  //     more
  // the stack frame isn't big enough
  // !! WARNING !!
  STREAM_BUFFER_SIZE = 1024;


constructor THashAlg.Create(AOwner: TComponent);
begin
  inherited;
  fTitle := '<<UNDEFINED>>';
  fHashLength := 0;
  fBlockLength := 0;

end;

destructor THashAlg.Destroy();
begin
  inherited;

end;

function THashAlg.PrettyPrintHashValue(const theHashValue: THashValue): string;
begin
  // Default to the boring ASCII hex dump...
  Result := theHashValue.ValueAsASCIIHex;
end;


function THashAlg.HashString(const theString: string; digest: THashValue): boolean;
var
  stream: TStringStream;
begin
  stream := TStringStream.Create(theString);
  try
    Result := HashStream(stream, digest);
  finally
    stream.Free();
  end;

end;

function THashAlg.HashFile(const filename: string; digest: THashValue): boolean;
var
  stream: TFileStream;
begin
  Result := FALSE;

  try
    stream := TFileStream.Create(filename, fmOpenRead OR fmShareDenyWrite);
    try
      Result := HashStream(stream, digest);
    finally
      stream.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;

function THashAlg.HashMemory(const memBlock: Pointer; const len: cardinal; digest: THashValue): boolean;
var
  stream: TMemoryStream;
begin
  Result := FALSE;

  try
    stream := TMemoryStream.Create();
    try
      stream.SetSize(len);
      stream.Write(memBlock, len);

      Result := HashStream(stream, digest);
    finally
      stream.Free();
    end;
  except
    // Nothing - Result already = FALSE
  end;

end;

function THashAlg.HashStream(const stream: TStream; digest: THashValue): boolean;
begin
  Result := FALSE;

  try
    Init();
    Update(stream);
    Final(digest);

    Result := TRUE;

  except
    // Nothing - Result already = FALSE
  end;

end;


procedure THashAlg.Update(const input: string);
var
  buffer: array of byte;
  i: integer;
begin
  SetLength(buffer, length(input));
  for i:=1 to length(input) do
    begin
    buffer[i-1] := byte(input[i]);
    end;

  Update(buffer, length(input))

end;

procedure THashAlg.Update(input: TStream; const inputLen: int64);
var
  buffer: array [0..(STREAM_BUFFER_SIZE-1)] of byte;
  len: integer;
  totalRead: int64;
begin
  len := input.Read(buffer, min(sizeof(buffer), inputLen));
  totalRead := len;
  while (len>0) do
    begin
    Update(buffer, len);
    // Cast to int64 to prevent Delphi casting to integer
    len := input.Read(buffer, min(sizeof(buffer), (inputLen-totalRead)));
    inc(totalRead, int64(len));
    end;

end;

procedure THashAlg.Update(input: TStream);
begin
  Update(input, (input.Size - input.Position));

end;

// -- begin depreciated methods --

// WARNING: Depreciated method
//          This method is depreciated and should not be used in new
//          developments
//          This method may be removed at a later date
// Replaced by: HashString(...), taking a THashValue
function THashAlg.HashString(theString: string): THashArray;
var
  hashValue: THashValue;
begin
  hashValue:= THashValue.Create();
  try
    HashString(theString, hashValue);
    Result := hashValue.GetValueAsArray();
  finally
    hashValue.Free();
  end;

end;

// WARNING: Depreciated method
//          This method is depreciated and should not be used in new
//          developments
//          This method may be removed at a later date
// Replaced by: HashMemory(...), taking a THashValue
function  THashAlg.HashMemory(memBlock: Pointer; len: cardinal): THashArray;
var
  hashValue: THashValue;
begin
  hashValue:= THashValue.Create();
  try
    HashMemory(memBlock, len, hashValue);
    Result := hashValue.GetValueAsArray();
  finally
    hashValue.Free();
  end;

end;

// WARNING: Depreciated method
//          This method is depreciated and should not be used in new
//          developments
//          This method may be removed at a later date
// Replaced by: HashFile(...), taking a THashValue
function THashAlg.HashFile(filename: string; var digest: THashArray): boolean;
var
  hashValue: THashValue;
begin
  Result := FALSE;

  hashValue:= THashValue.Create();
  try
    if HashFile(filename, hashValue) then
      begin
      digest := hashValue.GetValueAsArray;
      Result := TRUE;
      end;
  finally
    hashValue.Free();
  end;

end;

// WARNING: Depreciated method
//          This method is depreciated and should not be used in new
//          developments
//          This method may be removed at a later date
// Replaced by: PrettyPrintHashValue(...)
function THashAlg.HashToDisplay(theHash: THashArray): string;
var
  hashValue: THashValue;
begin
  hashValue:= THashValue.Create();
  try
    hashValue.SetValueAsArray(theHash, HashLength);
    Result := PrettyPrintHashValue(HashValue);
  finally
    hashValue.Free();
  end;

end;

// WARNING: Depreciated method
//          This method is depreciated and should not be used in new
//          developments
//          This method may be removed at a later date
// Replaced by: The appropriate method on THashValue
function THashAlg.HashToDataString(theHash: THashArray): string;
var
  hashValue: THashValue;
begin
  hashValue:= THashValue.Create();
  try
    hashValue.SetValueAsArray(theHash, HashLength);
    Result := hashValue.ValueAsBinary;
  finally
    hashValue.Free();
  end;

end;

// WARNING: Depreciated method
//          This method is depreciated and should not be used in new
//          developments
//          This method may be removed at a later date
// Replaced by: Clear(...) on THashValue
procedure THashAlg.ClearHash(var theHash: THashArray);
var
  i: integer;
begin
  for i:=low(theHash) to high(theHash) do
    begin
    theHash[i] := 0;
    end;

end;

// WARNING: Depreciated method
//          This method is depreciated and should not be used in new
//          developments
//          This method may be removed at a later date
// Replaced by: HashLength property
function THashAlg.DigestSize(): integer;
begin
  Result := HashLength;
end;

// -- end depreciated methods --

END.


