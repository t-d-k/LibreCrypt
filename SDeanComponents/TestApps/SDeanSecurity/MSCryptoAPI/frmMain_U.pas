unit frmMain_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TfrmMain = class(TForm)
    pbGenerate: TButton;
    reReport: TRichEdit;
    pbClose: TButton;
    pbClear: TButton;
    procedure pbGenerateClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  SDUGeneral,
  MSCryptoAPI;

procedure TfrmMain.pbGenerateClick(Sender: TObject);
const
  GENERATE_BYTECOUNT = 128;
var
  hProv: HCRYPTPROV;
  buffer: array [0..GENERATE_BYTECOUNT] of byte;
  strData: string;
  i: integer;
begin

  if CryptAcquireContext(
                         @hProv,
                         '',
                         '',
                         PROV_RSA_FULL,
                         CRYPT_VERIFYCONTEXT
                        ) then
    begin
    // Cleardown...
    // Note that clearing the buffer in this way is *not* required; it is only
    // included to make it clear that the buffer is being set to something by
    // the MS CrypoAPI
    for i:=low(buffer) to high(buffer) do
      begin
      buffer[i] := 0;
      end;


    if CryptGenRandom(hProv, GENERATE_BYTECOUNT, @buffer) then
      begin
      reReport.Lines.Add(inttostr(GENERATE_BYTECOUNT)+' bytes of MS CryptoAPI random data:');

      for i:=low(buffer) to high(buffer) do
        begin
        strData := strData + char(buffer[i]);
        end;

      if not(SDUPrettyPrintHex(strData, 0, GENERATE_BYTECOUNT, TStringList(reReport.Lines), 8)) then
        begin
        reReport.Lines.Add('Failed to *prettyprint* data');
        end;

      reReport.Lines.Add('');
      end
    else
      begin
      showmessage('CryptGenRandom FAILED');
      end;

    if not(CryptReleaseContext(hProv, 0)) then
      begin
      showmessage('CryptReleaseContext FAILED');
      end;

    end
  else
    begin
    showmessage('CryptAcquireContext FAILED');
    end;


end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  self.Caption := Application.Title;
  reReport.Clear();

end;

procedure TfrmMain.pbClearClick(Sender: TObject);
begin
  reReport.Clear();

end;

procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  Close();
  
end;


END.


