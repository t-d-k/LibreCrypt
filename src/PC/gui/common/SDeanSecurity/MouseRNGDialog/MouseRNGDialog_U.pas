unit MouseRNGDialog_U;
// Description: MouseRNG Dialog
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type

  // This exception if an attempt is made to set "RequiredBits" to a value
  // which *isn't* a multiple of 8
  ERequiredNot8Multiple = Exception;
  EInternalError = Exception;

  TMouseRNGDialog = class(TObject)
  private
    { Private declarations }
  protected
    FRequiredBits: integer;
    FData: array of byte;

    procedure SetRequiredBits(bits: integer);
  public

    function  Execute(): boolean;
//  published
    constructor Create();
    destructor  Destroy(); override;

    // This value must be set to a multiple of 8
    // An exception will be raised if an attempt is made to set this to a
    // value which *isn't* a multiple of 8
    property RequiredBits: integer read FRequiredBits write SetRequiredBits default 512;

    // Copy up to "count" bits into the buffer, return the number of bits copied
    // Note: "buffer" is indexed from 0
    // count - Set to the number of *bits* to copy into "data"
    // data - Zero indexed array into which the random data should be copied
    // Returns: The number of bits copied into "data"
    function RandomData(countBits: integer; var data: array of byte): integer;

    // Securely cleardown any random data collected
    procedure Wipe();
  end;

//procedure Register;

implementation

uses
  MouseRNGCaptureDlg_U,
  Math;  // Required for "min(...)"


//procedure Register;
//begin
//  RegisterComponents('SDeanSecurity', [TMouseRNGDialog]);
//end;


constructor TMouseRNGDialog.Create();
begin
  inherited;

end;


destructor TMouseRNGDialog.Destroy();
begin
  Wipe();
  
  inherited;
end;


function TMouseRNGDialog.Execute(): boolean;
var
  captureDlg: TMouseRNGCaptureDlg;
  bitsGot: integer;
begin
  Result := FALSE;

  // Cleardown any previous data...
  Wipe();

  captureDlg:= TMouseRNGCaptureDlg.create(nil);
  try
    captureDlg.RequiredBits  := FRequiredBits;

    if captureDlg.Showmodal()=mrOK then
      begin
      SetLength(FData, (FRequiredBits div 8));

      bitsGot := captureDlg.RandomData(FRequiredBits, FData);
      if (bitsGot <> FRequiredBits) then
        begin
        // Cleardown and raise error...
        Wipe();
        raise EInternalError.Create('Could not retieve all random data');
        end
      else
        begin
        Result := TRUE;
        end;

      end;

  finally
    captureDlg.Wipe();
    captureDlg.Free();
  end;
  
end;

procedure TMouseRNGDialog.Wipe();
var
  i: integer;
begin
  randomize;
  for i:=low(FData) to high(FData) do
    begin
    FData[i] := random(255);
    FData[i] := 0;
    end;

  SetLength(FData, 0);
  
end;


function TMouseRNGDialog.RandomData(countBits: integer; var data: array of byte): integer;
var
  copyCountBytes: integer;
  i: integer;
begin
  copyCountBytes := min((countBits div 8), Length(FData));

  for i:=0 to (copyCountBytes-1) do
    begin
    data[i] := FData[i];
    end;

  Result := (copyCountBytes * 8);
end;


procedure TMouseRNGDialog.SetRequiredBits(bits: integer);
begin
  if ((bits mod 8) <> 0) then
    begin
    raise ERequiredNot8Multiple.Create('Required bits must be a multiple of 8');
    end;

  FRequiredBits := bits

end;


END.

