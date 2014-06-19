unit sdAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ShellApi;

type
  TAboutForm = class(TForm)
    Image1 : TImage;
    Button1 : TButton;
    Label1 : TLabel;
    Label2 : TLabel;
    Bevel1 : TBevel;
    Label3 : TLabel;
    Label4 : TLabel;
    Label5 : TLabel;
    Label6 : TLabel;
    procedure Button1Click(Sender : TObject);
    procedure Label5Click(Sender : TObject);
  private
    { Private declarations }
  public
    class procedure Execute;
  end;

var
  AboutForm : TAboutForm;

implementation

{$R *.DFM}

procedure TAboutForm.Button1Click(Sender : TObject);
begin
  ModalResult := mrOK;
end;

class procedure TAboutForm.Execute;
begin
  with TAboutForm.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TAboutForm.Label5Click(Sender : TObject);
begin
  ShellExecute(0, 'open', 'http://www.kwikrite.clara.net/', nil, nil, SW_NORMAL);
end;

end.

