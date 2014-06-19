unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    edStringA: TEdit;
    edStringB: TEdit;
    pbXOR: TButton;
    reReport: TRichEdit;
    Label1: TLabel;
    Label2: TLabel;
    pbClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure pbXORClick(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  SDUGeneral;

procedure TForm1.FormCreate(Sender: TObject);
begin
  self.caption := Application.title;

  reReport.Lines.clear();
  edStringA.Text := '';
  edStringB.Text := '';

end;

procedure TForm1.pbXORClick(Sender: TObject);
var
  output: string;
  prettyOutput: TStringList;
begin
  output := SDUXOR(edStringA.Text, edStringB.Text);

  prettyOutput := TStringList.Create();
  try
    reReport.lines.Add('String A:');
    prettyOutput.clear();
    SDUPrettyPrintHex(edStringA.Text, 0, length(edStringA.Text), prettyOutput);
    reReport.lines.AddStrings(prettyOutput);
    reReport.lines.Add('');

    reReport.lines.Add('String B:');
    prettyOutput.clear();
    SDUPrettyPrintHex(edStringB.Text, 0, length(edStringB.Text), prettyOutput);
    reReport.lines.AddStrings(prettyOutput);
    reReport.lines.Add('');

    reReport.lines.Add('String A XOR String B:');
    prettyOutput.clear();
    SDUPrettyPrintHex(output, 0, length(output), prettyOutput);
    reReport.lines.AddStrings(prettyOutput);
    reReport.lines.Add('');

    reReport.lines.Add('-----------------------------------------');

  finally
    prettyOutput.Free();
  end;

end;

procedure TForm1.pbCloseClick(Sender: TObject);
begin
  Close();
  
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  SDUCenterControl(pbXOR, ccHorizontal);

end;

END.

