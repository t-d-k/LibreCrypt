unit MouseRNGCaptureDlg_U;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
     //delphi & libs
         Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  //sdu & LibreCrypt utils
       MouseRNG, StdCtrls,
   // LibreCrypt forms


  SDUForms;

const
  BUFFER_SIZE = 1024*1024*8;  // 1MB of data


type
  TMouseRNGCaptureDlg = class(TSDUForm)
    pbOK: TButton;
    pbCancel: TButton;
    MouseRNG: TMouseRNG;
    lblStored: TLabel;
    lblRequired: TLabel;
    lblInstructions: TLabel;
    procedure FormShow(Sender: TObject);
    procedure pbCancelClick(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure MouseRNGByteGenerated(Sender: TObject; random: Byte);
    procedure FormResize(Sender: TObject);
  private
  protected
    FRequiredBits: integer;

    FBitsCollected: integer;
    FData: array [1..BUFFER_SIZE] of byte;

    procedure UpdateDisplay();

    procedure SetRequiredBits(bits: integer);

  public
    property RequiredBits: integer read FRequiredBits write SetRequiredBits;
    
    property BitsCollected: integer read FBitsCollected;

    // Copy up to "count" ***bits*** into the buffer, return the number of bits
    // copied
    // Note: "buffer" is indexed from 0
    // countBits - Set to the number of *bits* to copy into "data"
    // data - Zero indexed array into which the random data should be copied
    // Returns: The number of bits copied into "data"
    function RandomData(countBits: integer; var data: array of byte): integer;

    // Securely cleardown any random data collected
    procedure Wipe();

  end;

implementation

{$R *.DFM}

uses
  SDUGeneral,
  Math;  // Required for "min(...)"

resourcestring
  BITS_REQUIRED = 'Bits required: %d';
  BITS_GENERATED = 'Bits generated: %d';

procedure TMouseRNGCaptureDlg.SetRequiredBits(bits: integer);
begin
  FRequiredBits := bits;
  
end;


procedure TMouseRNGCaptureDlg.FormShow(Sender: TObject);
begin
  UpdateDisplay();
  MouseRNG.Enabled := TRUE;

end;


procedure TMouseRNGCaptureDlg.UpdateDisplay();
var
  enoughGenerated: boolean;
begin
  if FRequiredBits>0 then
    begin
    lblRequired.visible := TRUE;

    lblRequired.caption := Format(BITS_REQUIRED, [FRequiredBits]);

    enoughGenerated := (FBitsCollected>=FRequiredBits);

    SDUEnableControl(pbOK, enoughGenerated);
    if (enoughGenerated) then
      begin
      MouseRNG.Enabled := FALSE;
      MouseRNG.Color := clBtnFace;
      SetFocusedControl(pbOK);
      end;

    end
  else
    begin
    lblRequired.visible := FALSE;

    SDUEnableControl(pbOK, TRUE);
    end;

  lblStored.caption := Format(BITS_GENERATED, [FBitsCollected]);

end;

procedure TMouseRNGCaptureDlg.Wipe();
var
  i: integer;
begin
  MouseRNG.Enabled := FALSE;

  for i:=low(FData) to high(FData) do
    begin
    FData[i] := random(255);
    FData[i] := 0;
    end;

  FRequiredBits:= 0;
  FBitsCollected:= 0;

end;


procedure TMouseRNGCaptureDlg.pbCancelClick(Sender: TObject);
begin
  MouseRNG.Enabled := FALSE;
  Wipe();
  ModalResult := mrCancel;

end;

procedure TMouseRNGCaptureDlg.pbOKClick(Sender: TObject);
begin
  MouseRNG.Enabled := FALSE;
  ModalResult := mrOK;

end;


function TMouseRNGCaptureDlg.RandomData(countBits: integer; var data: array of byte): integer;
var
  copyCountBytes: integer;
  i: integer;
begin
  copyCountBytes := min((countBits div 8), (FBitsCollected div 8));

  for i:=0 to (copyCountBytes-1) do     begin
    data[i] := FData[(FBitsCollected div 8) - i];
  end;

  Result := (copyCountBytes * 8);
end;


procedure TMouseRNGCaptureDlg.MouseRNGByteGenerated(Sender: TObject;
  random: Byte);
begin
  inc(FBitsCollected, 8);
  FData[(FBitsCollected div 8)] := random;
  UpdateDisplay();

end;

procedure TMouseRNGCaptureDlg.FormResize(Sender: TObject);
var
  twoButtonsFullWidth: integer;
  twoButtonsGap: integer;
begin
  // Horizontally center the labels...
  lblInstructions.left := (self.width-lblInstructions.width) div 2;
  lblStored.left := (self.width-lblStored.width) div 2;
  lblRequired.left := (self.width-lblRequired.width) div 2;

  // Nicely horizontally center the buttons...
  twoButtonsGap := pbCancel.left - (pbOK.left + pbOK.width);
  twoButtonsFullWidth := pbOK.width + twoButtonsGap + pbCancel.width;

  pbOK.left := (self.width-twoButtonsFullWidth) div 2;
  pbCancel.left := pbOK.left + pbOK.width + twoButtonsGap;


end;

END.


