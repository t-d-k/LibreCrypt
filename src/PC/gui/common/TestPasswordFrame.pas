unit TestPasswordFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SDUFrames, fmePassword, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    frmePassword1: TfrmePassword;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  procedure DoChange(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DoChange(Sender: TObject);
begin
  Label1.Caption := frmePassword1.GetKeyPhraseAsString;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  frmePassword1.OnChange := DoChange;
end;

end.
