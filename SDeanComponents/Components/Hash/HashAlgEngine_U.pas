unit HashAlgEngine_U;
// Description: Hashing Engine Base Class
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, SDUGeneral,
  HashValue_U;

type
  THashAlgEngine = class
  protected
    procedure HE_memcpy(var output: array of byte; const input: array of byte; const startA, startB, len: cardinal); overload;
    procedure HE_memcpy(var output: array of DWORD; const input: array of DWORD; const startA, startB, len: cardinal); overload;
    procedure HE_memset(var output: array of byte; const value: cardinal; const start, len: cardinal); overload;
    procedure HE_memset(var output: array of DWORD; const value: DWORD; const start, len: cardinal); overload;
    procedure HE_memset(var output: array of ULONGLONG; const value: ULONGLONG; const start, len: cardinal); overload;
  end;

implementation

// Note: Replace "for loop" with standard memcpy if possible.
procedure THashAlgEngine.HE_memcpy(var output: array of byte; const input: array of byte; const startA, startB, len: cardinal);
var
  i: cardinal;
begin
  for i:=1 to len do
    begin
    output[startA+i-1] := input[startB+i-1];
    end;

end;

// Note: Replace "for loop" with standard memcpy if possible.
procedure THashAlgEngine.HE_memcpy(var output: array of DWORD; const input: array of DWORD; const startA, startB, len: cardinal);
var
  i: cardinal;
begin
  for i:=1 to len do
    begin
    output[startA+i-1] := input[startB+i-1];
    end;

end;

// Note: Replace "for loop" with standard memset if possible.
procedure THashAlgEngine.HE_memset(var output: array of byte; const value: cardinal; const start, len: cardinal);
var
  i: cardinal;
begin
  for i:=1 to len do
    begin
    output[start+i-1] := value;
    end;

end;

// Note: Replace "for loop" with standard memset if possible.
procedure THashAlgEngine.HE_memset(var output: array of DWORD; const value: DWORD; const start, len: cardinal);
var
  i: cardinal;
begin
  for i:=1 to len do
    begin
    output[start+i-1] := value;
    end;

end;

// Note: Replace "for loop" with standard memset if possible.
procedure THashAlgEngine.HE_memset(var output: array of ULONGLONG; const value: ULONGLONG; const start, len: cardinal);
var
  i: cardinal;
begin
  for i:=1 to len do
    begin
    output[start+i-1] := value;
    end;

end;

END.


