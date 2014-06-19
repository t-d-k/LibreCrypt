unit Main;
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
  StdCtrls, ComCtrls, Spin64, MouseRNGDialog_U;

type
  TfrmMain = class(TForm)
    MouseRNGDialog1: TMouseRNGDialog;
    pbCapture: TButton;
    Label1: TLabel;
    seRandomCount: TSpinEdit64;
    Label2: TLabel;
    pbClear: TButton;
    pbCancel: TButton;
    reReport: TRichEdit;
    Label3: TLabel;
    procedure pbCancelClick(Sender: TObject);
    procedure pbCaptureClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
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
  SDUGeneral;


procedure TfrmMain.pbCancelClick(Sender: TObject);
begin
  Close();

end;

procedure TfrmMain.pbCaptureClick(Sender: TObject);
var
  randomData: array of byte;
  integerBitsGot: integer;
begin
  SDUEnableControl(seRandomCount, FALSE);
  SDUEnableControl(pbCapture, FALSE);

  try
    try
      MouseRNGDialog1.RequiredBits := seRandomCount.Value;

      if MouseRNGDialog1.Execute() then
        begin
        SetLength(randomData, (seRandomCount.Value div 8));
        integerBitsGot := MouseRNGDialog1.RandomData(seRandomCount.Value, randomData);
        reReport.Lines.Add('Random data collected:');
        SDUPrettyPrintHex(@randomData[0], 0, (integerBitsGot div 8), TStringList(reReport.lines));
        reReport.Lines.Add('');
        end
      else
        begin
        reReport.Lines.Add('User cancelled.');
        reReport.Lines.Add('');
        end;

    except
      // Do nothing
    end;

  finally
    SDUEnableControl(seRandomCount, TRUE);
    SDUEnableControl(pbCapture, TRUE);
  end;

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  self.Caption := Application.Title;
  reReport.Lines.Clear();

end;

procedure TfrmMain.pbClearClick(Sender: TObject);
begin
  reReport.Lines.Clear();

end;

END.


