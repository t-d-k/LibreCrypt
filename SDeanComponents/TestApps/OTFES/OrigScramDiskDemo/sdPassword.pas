unit sdPassword;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TPasswordForm = class(TForm)
    PageControl1 : TPageControl;
    TabSheet1 : TTabSheet;
    Edit1 : TEdit;
    Edit2 : TEdit;
    Edit3 : TEdit;
    Edit4 : TEdit;
    CheckBox1 : TCheckBox;
    Button1 : TButton;
    Button2 : TButton;
    procedure CheckBox1Click(Sender : TObject);
    procedure Button1Click(Sender : TObject);
    procedure Button2Click(Sender : TObject);
  private
    { Private declarations }
  public
    class function Execute(var Password1, Password2, Password3, Password4 : string) : Boolean;
  end;

var
  PasswordForm : TPasswordForm;

implementation

{$R *.DFM}

procedure TPasswordForm.CheckBox1Click(Sender : TObject);
var
  lPasswordChar : Char;
begin
  if CheckBox1.Checked then
    lPasswordChar := '*'
  else
    lPasswordChar := #0;
  Edit1.PasswordChar := lPasswordChar;
  Edit2.PasswordChar := lPasswordChar;
  Edit3.PasswordChar := lPasswordChar;
  Edit4.PasswordChar := lPasswordChar;
end;

class function TPasswordForm.Execute(var Password1, Password2, Password3,
  Password4 : string) : Boolean;
begin
  Result := false;
  with TPasswordForm.Create(nil) do
  try
    Edit1.Text := Copy(Password1, 1, 40);
    Edit2.Text := Copy(Password2, 1, 40);
    Edit3.Text := Copy(Password3, 1, 40);
    Edit4.Text := Copy(Password4, 1, 40);
    if ShowModal = mrOK then
      begin
        Password1 := Edit1.Text;
        Password2 := Edit2.Text;
        Password3 := Edit3.Text;
        Password4 := Edit4.Text;
        Result := true;
      end
  finally
    Free;
  end;
end;

procedure TPasswordForm.Button1Click(Sender : TObject);
begin
  ModalResult := mrOK;
end;

procedure TPasswordForm.Button2Click(Sender : TObject);
begin
  ModalResult := mrCancel;
end;

end.

