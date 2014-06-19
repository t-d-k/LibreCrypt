unit Main;
// Description: MouseRNG Test Application
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MouseRNG, StdCtrls, ComCtrls, SDUDialogs;

const
  TARGET_BIT_COUNT = 20000;

type
  TfrmMain = class(TForm)
    reReport: TRichEdit;
    reBytes: TRichEdit;
    reBitsCSV: TRichEdit;
    Label3: TLabel;
    Label2: TLabel;
    pbWriteTextFile: TButton;
    pbWriteBinaryFile: TButton;
    SaveDialog1: TSDUSaveDialog;
    Label4: TLabel;
    ckEnableRNG: TCheckBox;
    RichEdit1: TRichEdit;
    MouseRNG1: TMouseRNG;
    Label1: TLabel;
    lblBitCount: TLabel;
    procedure ckEnableRNGClick(Sender: TObject);
    procedure pbWriteTextFileClick(Sender: TObject);
    procedure pbWriteBinaryFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MouseRNG1BitGenerated(Sender: TObject; random: Byte);
    procedure MouseRNG1ByteGenerated(Sender: TObject; random: Byte);
    procedure MouseRNG1DEBUGSampleTakenEvent(Sender: TObject; X,
      Y: Integer);
  private
    sampleCount: integer;
    randomBitCount: integer;
    randomByteCount: integer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.ckEnableRNGClick(Sender: TObject);
begin
  MouseRNG1.enabled := ckEnableRNG.checked;

end;

procedure TfrmMain.pbWriteTextFileClick(Sender: TObject);
begin
  if SaveDialog1.Execute() then
    begin
    reBytes.Lines.SaveToFile(SaveDialog1.Filename);
    end;

end;

procedure TfrmMain.pbWriteBinaryFileClick(Sender: TObject);
var
  strAllData: string;
  currByte: byte;
  dataFile: THandle;
  byteWritten: cardinal;
  i: integer;
begin
  if SaveDialog1.Execute() then
    begin
    strAllData := reBytes.Text;

    dataFile := CreateFile(
                           PChar(SaveDialog1.Filename),
                           GENERIC_WRITE,
                           FILE_SHARE_READ,
                           nil,
                           OPEN_ALWAYS,
                           0,
                           0
                          );
    if not(dataFile<>INVALID_HANDLE_VALUE) then
      begin
      showmessage('Unable to open file for writing.');
      end
    else
      begin
      for i:=0 to (reBytes.lines.count-1) do
        begin
        currByte:= strtoint(reBytes.lines[i]);
        if not(WriteFile(dataFile, currByte, 1, byteWritten, nil)) then
          begin
          showmessage('Write failure.');
          break;
          end;
        end;

      CloseHandle(dataFile);
      end;

    end;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  sampleCount := 0;

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  reReport.PlainText := TRUE;
  reBitsCSV.PlainText := TRUE;
  reBytes.PlainText := TRUE;

end;

procedure TfrmMain.MouseRNG1BitGenerated(Sender: TObject; random: Byte);
begin
  inc(randomBitCount);

  if (randomBitCount>1) then
    begin
    reBitsCSV.Text := reBitsCSV.Text + ',';
    end;
  reBitsCSV.Text := reBitsCSV.Text + inttostr(random);
  lblBitCount.caption := inttostr(randomBitCount);

  if (randomBitCount >= TARGET_BIT_COUNT) then
    begin
    ckEnableRNG.checked := FALSE;
    showmessage(inttostr(TARGET_BIT_COUNT)+' random bits generated/');
    end;

end;

procedure TfrmMain.MouseRNG1ByteGenerated(Sender: TObject; random: Byte);
begin
  inc(randomByteCount);
  reBytes.lines.add(inttostr(random));

end;

procedure TfrmMain.MouseRNG1DEBUGSampleTakenEvent(Sender: TObject; X,
  Y: Integer);
begin
  inc(sampleCount);
  reReport.lines.add(inttostr(sampleCount)+'  -  X='+inttostr(X)+'   Y='+inttostr(Y));

end;

END.


