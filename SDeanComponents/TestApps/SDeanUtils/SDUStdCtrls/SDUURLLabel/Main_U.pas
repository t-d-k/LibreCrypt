unit Main_U;
// Description: TSDUURLLabel Test Application
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, SDUStdCtrls;

type
  TfrmMain = class(TForm)
    GroupBox1: TGroupBox;
    SDUURLLabel: TSDUURLLabel;
    edURL: TEdit;
    edCaption: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    pbClose: TButton;
    procedure edChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

procedure TfrmMain.edChange(Sender: TObject);
begin
  SDUURLLabel.Caption := edCaption.Text;
  SDUURLLabel.URL := edURL.Text;

end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Self.Caption := Application.Title;
  
  edCaption.Text := 'Sarah Dean''s WWW site!';
  edURL.Text := 'http://www.SDean12.org/';

end;

procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  Close();
  
end;

END.


