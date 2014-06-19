unit SecondForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    pbAppMinimise: TButton;
    pbClose: TButton;
    pbWSMinimise: TButton;
    pbHide: TButton;
    pbOK: TButton;
    procedure pbAppMinimiseClick(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure pbWSMinimiseClick(Sender: TObject);
    procedure pbHideClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}


procedure TForm2.pbAppMinimiseClick(Sender: TObject);
begin
  Application.Minimize();

end;

procedure TForm2.pbWSMinimiseClick(Sender: TObject);
begin
  WindowState := wsminimized;

end;

procedure TForm2.pbHideClick(Sender: TObject);
begin
  Hide();

end;

procedure TForm2.pbCloseClick(Sender: TObject);
begin
  Close();

end;


END.


