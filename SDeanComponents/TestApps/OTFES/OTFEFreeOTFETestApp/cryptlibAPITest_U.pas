unit cryptlibAPITest_U;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  cryptlib, Spin64;


type
  TcryptlibAPITest = class(TForm)
    pbLOADDLL: TButton;
    pbCreateContext: TButton;
    pbUNLOADDLL: TButton;
    pbCryptInit: TButton;
    pbCryptEnd: TButton;
    pbDestroyContext: TButton;
    pbCryptGenerateKey: TButton;
    pbCryptEncrypt: TButton;
    pbCryptAddRandom: TButton;
    reReport: TRichEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    pbClear: TButton;
    pbGenerateRandom: TButton;
    seGenerateBytes: TSpinEdit64;
    Label1: TLabel;
    RichEdit1: TRichEdit;
    Panel1: TPanel;
    pbCryptQueryCapability: TButton;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure pbLOADDLLClick(Sender: TObject);
    procedure pbUNLOADDLLClick(Sender: TObject);
    procedure pbCreateContextClick(Sender: TObject);
    procedure pbCryptInitClick(Sender: TObject);
    procedure pbCryptEndClick(Sender: TObject);
    procedure pbDestroyContextClick(Sender: TObject);
    procedure pbCryptGenerateKeyClick(Sender: TObject);
    procedure pbCryptAddRandomClick(Sender: TObject);
    procedure pbCryptEncryptClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure pbGenerateRandomClick(Sender: TObject);
    procedure pbCryptQueryCapabilityClick(Sender: TObject);
  private
    cryptContext: TCRYPT_CONTEXT;

    procedure ReportResult(funcResult: TC_RET; func: string);
  public
    { Public declarations }
  end;


implementation

{$R *.DFM}

uses
  SDUGeneral;

procedure TcryptlibAPITest.FormCreate(Sender: TObject);
begin
  reReport.Lines.Clear();

  Panel1.Caption := '';

end;



procedure TcryptlibAPITest.pbLOADDLLClick(Sender: TObject);
begin
  if cryptlib_LoadDLL() then
    begin
    reReport.Lines.Add('SUCCESS: DLL loaded.');
    end
  else
    begin
    reReport.Lines.Add('FAILED: DLL NOT loaded.');
    end;

end;

procedure TcryptlibAPITest.pbUNLOADDLLClick(Sender: TObject);
begin
  if cryptlib_UnloadDLL() then
    begin
    reReport.Lines.Add('SUCCESS: DLL unloaded.');
    end
  else
    begin
    reReport.Lines.Add('FAILED: DLL NOT unloaded.');
    end;

end;

procedure TcryptlibAPITest.pbCreateContextClick(Sender: TObject);
var
  funcResult: integer;
begin
  funcResult := cryptlib_cryptCreateContext(
                       @cryptContext,
                       cryptlib_CRYPT_UNUSED,
                       cryptlib_CRYPT_ALGO_TYPE_ID[catCRYPT_ALGO_AES]
                      );
  ReportResult(funcResult, 'cryptCreateContext');

end;


procedure TcryptlibAPITest.ReportResult(funcResult: TC_RET; func: string);
begin
  if (cryptlib_cryptStatusOK(funcResult)) then
    begin
    reReport.Lines.Add('SUCCESS: '+func);
    end
  else
    begin
    reReport.Lines.Add('**************************');
    reReport.Lines.Add('**************************');
    reReport.Lines.Add('FAILED: '+func);
    reReport.Lines.Add('**************************');
    reReport.Lines.Add('**************************');
    end;

end;


procedure TcryptlibAPITest.pbCryptInitClick(Sender: TObject);
var
  funcResult: integer;
begin
  funcResult := cryptlib_cryptInit();
  ReportResult(funcResult, 'cryptInit');

end;


procedure TcryptlibAPITest.pbCryptEndClick(Sender: TObject);
var
  funcResult: integer;
begin
  funcResult := cryptlib_cryptEnd();
  ReportResult(funcResult, 'cryptEnd');

end;



procedure TcryptlibAPITest.pbDestroyContextClick(Sender: TObject);
var
  funcResult: integer;
begin
  funcResult := cryptlib_cryptDestroyContext(cryptContext);
  ReportResult(funcResult, 'cryptDestroyContext');

end;


procedure TcryptlibAPITest.pbCryptAddRandomClick(Sender: TObject);
var
  funcResult: integer;
begin
  funcResult := cryptlib_cryptAddRandom(
                                      nil,
                                      cryptlib_CRYPT_RANDOM_SLOWPOLL
                                     );
  ReportResult(funcResult, 'cryptAddRandom');

end;

procedure TcryptlibAPITest.pbCryptGenerateKeyClick(Sender: TObject);
var
  funcResult: integer;
begin
  funcResult := cryptlib_cryptGenerateKey(cryptContext);
  ReportResult(funcResult, 'cryptGenerateKey');

end;

procedure TcryptlibAPITest.pbCryptEncryptClick(Sender: TObject);
const
  SIZE_OF_DATA = 1024;
var
  funcResult: integer;
  buffer: PByte;
  tmpStringList: TStringList;
begin

  buffer := AllocMem(SIZE_OF_DATA);
  try
    funcResult := cryptlib_cryptEncrypt(
                            cryptContext,
                            buffer,
                            SIZE_OF_DATA
                           ); 

    ReportResult(funcResult, 'cryptEncrypt');
    if (funcResult = cryptlib_CRYPT_OK) then
      begin
      tmpStringList:= TStringList.Create();
      try
        SDUPrettyPrintHex(buffer, 0, SIZE_OF_DATA, tmpStringList);
        reReport.Lines.AddStrings(tmpStringList);

      finally
        tmpStringList.Free();
      end;

      end;
      
  finally
    FreeMem(buffer);
  end;

end;


procedure TcryptlibAPITest.pbClearClick(Sender: TObject);
begin
  reReport.Lines.Clear();

end;

procedure TcryptlibAPITest.pbGenerateRandomClick(Sender: TObject);
var
  randomData: ansistring;
  tmpStringList: TStringList;
begin

  randomData := cryptlib_RNG(seGenerateBytes.value);
  if (Length(randomData) <> seGenerateBytes.value) then
    begin
    reReport.Lines.Add('FAILED: Random data not generated.');
    end
  else
    begin
    reReport.Lines.Add('SUCCESS: Random data generated:');
    
    tmpStringList:= TStringList.Create();
    try
      SDUPrettyPrintHex(randomData, 0, length(randomData), tmpStringList);
      reReport.Lines.AddStrings(tmpStringList);

    finally
      tmpStringList.Free();
    end;


    end;

end;

procedure TcryptlibAPITest.pbCryptQueryCapabilityClick(Sender: TObject);
var
  funcResult: integer;
  info: Tcryptlib_CRYPT_QUERY_INFO;
  tmpName: string;
begin
  funcResult := cryptlib_cryptQueryCapability(
                                      cryptlib_CRYPT_ALGO_TYPE_ID[catCRYPT_ALGO_AES],
                                      @info
                                     );
  ReportResult(funcResult, 'cryptlib_cryptQueryCapability');
  if (funcResult = cryptlib_CRYPT_OK) then
    begin
    tmpName := Copy(info.algoName, 1, StrLen(info.algoName));
    reReport.Lines.Add('algoName: '+tmpName);
    reReport.Lines.Add('blockSize: '+inttostr(info.blockSize)+' bytes ('+inttostr(info.blockSize*8)+' bits)');
    reReport.Lines.Add('minKeySize: '+inttostr(info.minKeySize)+' bytes ('+inttostr(info.blockSize*8)+' bits)');
    reReport.Lines.Add('keySize: '+inttostr(info.keySize)+' bytes ('+inttostr(info.blockSize*8)+' bits)');
    reReport.Lines.Add('maxKeySize: '+inttostr(info.maxKeySize)+' bytes ('+inttostr(info.blockSize*8)+' bits)');
    end;

end;


END.


