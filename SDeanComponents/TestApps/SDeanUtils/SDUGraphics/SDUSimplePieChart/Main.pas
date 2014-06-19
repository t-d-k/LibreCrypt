unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin64;

type
  TfrmMain = class(TForm)
    ListBox1: TListBox;
    sePercent: TSpinEdit64;
    Label1: TLabel;
    Label2: TLabel;
    pbClose: TButton;
    procedure sePercentChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure pbCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  SDUGraphics;

procedure TfrmMain.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  SDUSimplePieChart(ListBox1.canvas, rect, sePercent.Value);

end;

procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  self.Caption := Application.Title;

  ListBox1.Items.Add('one');
  ListBox1.Items.Add('two');
  ListBox1.Items.Add('three');
  ListBox1.Style := lbOwnerDrawFixed;
  ListBox1.ItemHeight := ListBox1.width;

end;

procedure TfrmMain.sePercentChange(Sender: TObject);
begin
  ListBox1.Refresh();

end;

END.


