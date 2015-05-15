unit RNGTest_U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, MouseRNG,
  OTFEFreeOTFE_WizardCommon;

type
  TRNGTest = class(TForm)
    pbClose: TButton;
    reReport: TRichEdit;
    pbGenerateRandomData: TButton;
    pbClear: TButton;
    edGPGFilename: TEdit;
    lblGPGFilename: TLabel;
    MouseRNG: TMouseRNG;
    lblMouseRNGBits: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    gbRNG: TGroupBox;
    ckRNGMouseMovement: TCheckBox;
    ckRNGCryptoAPI: TCheckBox;
    ckRNGcryptlib: TCheckBox;
    ckRNGGPG: TCheckBox;
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbGenerateRandomDataClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure rgRNGClick(Sender: TObject);
    procedure MouseRNGByteGenerated(Sender: TObject; random: Byte);
  private
    CanUseCryptlib: boolean;

    fRandomData: string;
    function GetRNGSet(): TRNGSet;
  public
    { Public declarations }
  end;

var
  RNGTest: TRNGTest;

implementation

{$R *.DFM}

uses
  DriverAPI,  // Required for CRITICAL_DATA_LENGTH
  SDUGeneral;

procedure TRNGTest.pbCloseClick(Sender: TObject);
begin
  Close();

end;

procedure TRNGTest.FormShow(Sender: TObject);
begin
  ckRNGcryptlib.Enabled := CanUseCryptlib;

  reReport.Lines.Clear();

  Panel1.Caption := '';
  Panel2.Caption := '';
  Panel3.Caption := '';
end;


function TRNGTest.GetRNGSet(): TRNGSet;
var
  retval: TRNGSet;
begin
  retval := [];

  if ckRNGCryptoAPI.checked then
    begin
    retval := retval + [rngCryptoAPI];
    end;

  if ckRNGMouseMovement.checked then
    begin
    retval := retval + [rngMouseMovement];
    end;

  if ckRNGcryptlib.checked then
    begin
    retval := retval + [rngcryptlib];
    end;

  if ckRNGGPG.checked then
    begin
    retval := retval + [rngGPG];
    end;

  Result := retval;
end;

procedure TRNGTest.pbGenerateRandomDataClick(Sender: TObject);
var
  allOK: boolean;
  tmpStringList: TStringList;
begin
  allOK := GenerateRandomData(
                              GetRNGSet(),
                              (CRITICAL_DATA_LENGTH div 8),
                              nil,  // PKCS#11 related
                              0,  // PKCS#11 related
                              edGPGFilename.Text,
                              fRandomData
                             );
  if (allOK) then
    begin
    reReport.Lines.Add('SUCCESS: Random data generated');

    tmpStringList := TStringList.Create();
    try
      SDUPrettyPrintHex(
                        fRandomData,
                        0,
                        length(fRandomData),
                        tmpStringList
                       );

      reReport.Lines.AddStrings(tmpStringList);
    finally
      tmpStringList.Free();
    end;

    end
  else
    begin
    reReport.Lines.Add('FAILED: Random data NOT generated');
    end;

end;

procedure TRNGTest.pbClearClick(Sender: TObject);
begin
  reReport.Lines.Clear();

end;

procedure TRNGTest.FormCreate(Sender: TObject);
begin
  fRandomData:= '';
  
  // Start cryptlib, if possible, as early as we can to allow it as much time
  // as possible to poll entropy
  CanUseCryptlib := cryptlibLoad();

end;

procedure TRNGTest.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Shutdown cryptlib, if used
  if CanUseCryptlib then
    begin
    cryptlibUnload();
    CanUseCryptlib := FALSE;
    end;

end;


procedure TRNGTest.rgRNGClick(Sender: TObject);
begin
  fRandomData := '';

  // Just for testing...
  SDUEnableControl(MouseRNG, ckRNGMouseMovement.checked);
  SDUEnableControl(lblMouseRNGBits, ckRNGMouseMovement.checked);
  
  SDUEnableControl(lblGPGFilename, ckRNGGPG.checked);
  SDUEnableControl(edGPGFilename, ckRNGGPG.checked);

end;

procedure TRNGTest.MouseRNGByteGenerated(Sender: TObject; random: Byte);
var
  randomBitsGenerated: integer;
  allOK: boolean;
begin
  // Note: This is correct; if it's *less than* CRITICAL_DATA_LEN, then store it
  if ((length(fRandomData) * 8) < CRITICAL_DATA_LENGTH) then
    begin
    fRandomData := fRandomData + char(random);
    end;


  // This is a good place to update the display of the number of random bits
  // generated...
  randomBitsGenerated:= (length(fRandomData) * 8);
  lblMouseRNGBits.Caption := 'Random bits generated: '+inttostr(randomBitsGenerated)+'/'+inttostr(CRITICAL_DATA_LENGTH);

  allOK := (randomBitsGenerated >= CRITICAL_DATA_LENGTH);
  MouseRNG.Enabled := not(allOK);
  if MouseRNG.Enabled then
    begin
    MouseRNG.Color := clWindow;
    end
  else
    begin
    MouseRNG.Color := clBtnFace;
    end;


end;

END.


