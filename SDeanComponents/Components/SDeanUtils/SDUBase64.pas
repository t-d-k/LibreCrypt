unit SDUBase64;

// This source taken from the WWW, full credit to the original author.

interface

function Base64Decode(var Buf: Ansistring): longint; overload;

function Base64Encode(inBuf: Ansistring; var outBuf: Ansistring): longint; overload;

function Base64Decode(const Buf: PAnsiChar): longint; overload;

function Base64Encode(const InBuf: PAnsiChar; const InLen: longint;
                      const OutBuf: PAnsiChar): longint; overload;

implementation

uses SysUtils;

// Decode the supplied string, returning the number of decoded bytes
function Base64Decode(var Buf: Ansistring): longint;
var
  tmpArray: array of byte;
  i: integer;
  decodedLen: integer;
begin

  SetLength(tmpArray, length(buf)+1);
  for i:=1 to length(buf) do
    begin
    tmpArray[i-1] := ord(buf[i]);
    end;
  tmpArray[length(buf)] := 0;

  decodedLen := Base64Decode(@tmpArray[0]);

  buf := '';
  for i:=0 to (decodedLen-1) do
    begin
    buf := buf + Ansichar(tmpArray[i]);
    end;

  Result := decodedLen;

end;


// Encode the supplied string, returning number of bytes
function Base64Encode(inBuf: Ansistring; var outBuf: Ansistring): longint;
var
  tmpInArray: array of byte;
  tmpOutArray: array of byte;
  i: integer;
  decodedLen: integer;
begin

  SetLength(tmpInArray, length(inBuf)+1);
  for i:=1 to length(inBuf) do
    begin
    tmpInArray[i-1] := ord(inBuf[i]);
    end;
  tmpInArray[length(inBuf)] := 0;

  SetLength(tmpOutArray, 2*length(inBuf));

  decodedLen := Base64Encode(@tmpInArray[0], length(inBuf), @tmpOutArray[0]);

  outBuf := '';
  for i:=0 to (decodedLen-1) do
    begin
    outBuf := outBuf + AnsiChar(tmpOutArray[i]);
    end;

  Result := decodedLen;

end;


function Base64Decode(const Buf: PAnsiChar): longint;
{ Do a Base-64 decode of Buf, returning the number of decoded bytes. }
var
   InP, OutP: PAnsiChar;
   Group3: longint; { Must be a 3+ byte entity }
   Idx: integer;
begin
InP:= Buf; OutP:= Buf; Group3:= 0; Idx:= 0;

while (InP^ <> #0) do
   begin
     case InP^ of
     'A'..'Z': Group3:= (Group3 shl 6) + ord(InP^) - ord('A');
     'a'..'z': Group3:= (Group3 shl 6) + ord(InP^) - ord('a') + 26;
     '0'..'9': Group3:= (Group3 shl 6) + ord(InP^) - ord('0') + 52;
     '+'     : Group3:= (Group3 shl 6) + 62;
     '/'     : Group3:= (Group3 shl 6) + 63;
     '='     : Group3:= (Group3 shl 6);
     end;

   if (InP^ in ['A'..'Z', 'a'..'z', '0'..'9', '+', '/']) then
      begin
      Idx:= (Idx + 1) mod 4;
      if (Idx = 0) then
         begin
         OutP^    := AnsiChar((Group3 shr 16) and $ff);
         (OutP+1)^:= AnsiChar((Group3 shr 8)  and $ff);
         (OutP+2)^:= AnsiChar(Group3 and $ff);
         inc(OutP, 3);
         end;
      end;

   inc(InP);
  end;

  { Do the last one or two bytes }
  case Idx of
   0, 1: { Not possible };
   2   : begin
            { Two encoded-data bytes yield one decoded byte }
            OutP^:= AnsiChar((Group3 shr 16) and $ff);
            inc(OutP);
         end;
   3   : begin
            { Three encoded-data bytes yield two decoded bytes }
            OutP^    := AnsiChar((Group3 shr 16) and $ff);
            (OutP+1)^:= AnsiChar((Group3 shr 8) and $ff);
            inc(OutP, 2);
         end;
  end;

OutP^:= #0;
Result:= (OutP - Buf);
end;


function Base64Encode(const InBuf: PAnsiChar; const InLen: longint;
                      const OutBuf: PAnsiChar): longint;
{ Do a base64 encoding of InLen bytes from InBuf,
  save to OutBuf and return number of bytes in OutBuf }

var
   Alphabet : array[0..63] of Ansichar;
   InP, OutP: PAnsiChar;
   Remain: longint;
   i: byte;

begin
InP:= InBuf; OutP:= OutBuf; Remain:= InLen;

{ Setup the encoding alphabet }
for i:= 0 to 25 do
    begin
    Alphabet[i]:= Ansichar(i + ord('A'));
    Alphabet[i+26]:= Ansichar(i + ord('a'));
    end;
for i:= 0 to 9 do Alphabet[i+52]:= Ansichar(i + ord('0'));
Alphabet[62]:= '+'; Alphabet[63]:= '/';

while (remain >= 3) do
   begin
   { Don't try to think about how this works ... drawing it is easier }
   OutP^    := Alphabet[(ord(InP^) shr 2)];
   (OutP+1)^:= Alphabet[((ord(InP^) and $03) shl 4) or (ord((InP+1)^) shr 4)];
   (OutP+2)^:= Alphabet[((ord((InP+1)^) and $0f) shl 2) or (ord((InP+2)^) shr 6)];
   (OutP+3)^:= Alphabet[(ord((InP+2)^) and $3f)];
   dec(Remain, 3); inc(InP, 3); inc(OutP, 4);
   end;

{ There are 0, 1 or 2 bytes left }
if (Remain = 1) then
   begin
   OutP^    := Alphabet[(ord(InP^) shr 2)];
   (OutP+1)^:= Alphabet[((ord(InP^) and $03) shl 4)];
   (OutP+2)^:= '=';
   (OutP+3)^:= '=';
   inc(OutP, 4);
   end
else if (Remain = 2) then
   begin
   OutP^    := Alphabet[(ord(InP^) shr 2)];
   (OutP+1)^:= Alphabet[((ord(InP^) and $03) shl 4) or (ord((InP+1)^) shr 4)];
   (OutP+2)^:= Alphabet[((ord((InP+1)^) and $0f) shl 2)];
   (OutP+3)^:= '=';
   inc(OutP, 4);
   end;

OutP^:= #0;
Result:= (OutP - OutBuf);
end;


END.

