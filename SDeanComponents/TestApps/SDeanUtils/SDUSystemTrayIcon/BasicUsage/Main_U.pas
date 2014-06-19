unit Main_U;
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
  ImgList, Menus, ExtCtrls, Spin64, StdCtrls, SDUSystemTrayIcon;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ckActive: TCheckBox;
    ckAnimate: TCheckBox;
    edTitle: TEdit;
    edInfo: TEdit;
    pbBubbleMsg: TButton;
    edTip: TEdit;
    pbSetTip: TButton;
    seTimeout: TSpinEdit64;
    pnlOne: TPanel;
    pnlTwo: TPanel;
    PopupMenu1: TPopupMenu;
    miOne: TMenuItem;
    miTwo: TMenuItem;
    ImageList1: TImageList;
    SDUSystemTrayIcon: TSDUSystemTrayIcon;
    procedure miOneClick(Sender: TObject);
    procedure miTwoClick(Sender: TObject);
    procedure ckActiveClick(Sender: TObject);
    procedure ckAnimateClick(Sender: TObject);
    procedure pbBubbleMsgClick(Sender: TObject);
    procedure pbSetTipClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SDUSystemTrayIconBalloonHide(Sender: TObject);
    procedure SDUSystemTrayIconBalloonShow(Sender: TObject);
    procedure SDUSystemTrayIconBalloonTimeout(Sender: TObject);
    procedure SDUSystemTrayIconBalloonUserClick(Sender: TObject);
    procedure SDUSystemTrayIconClick(Sender: TObject);
    procedure SDUSystemTrayIconDblClick(Sender: TObject);  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.miOneClick(Sender: TObject);
begin
  showmessage('Menuitem one clicked');
end;

procedure TForm1.miTwoClick(Sender: TObject);
begin
  showmessage('Menuitem two clicked');

end;


procedure TForm1.ckActiveClick(Sender: TObject);
begin
  SDUSystemTrayIcon.Active := ckActive.Checked;
  
end;

procedure TForm1.ckAnimateClick(Sender: TObject);
begin
  SDUSystemTrayIcon.AnimateIcon := ckAnimate.Checked;

end;

procedure TForm1.pbBubbleMsgClick(Sender: TObject);
begin
  SDUSystemTrayIcon.BubbleMessage(edTitle.text, edInfo.text, shbiWarning, (seTimeout.value * 1000));

end;


procedure TForm1.pbSetTipClick(Sender: TObject);
begin
  SDUSystemTrayIcon.Tip := edTip.text;

end;

procedure TForm1.SDUSystemTrayIconBalloonHide(Sender: TObject);
begin
  showmessage('Balloon hidden');

end;

procedure TForm1.SDUSystemTrayIconBalloonTimeout(Sender: TObject);
begin
  showmessage('Balloon timed out');

end;

procedure TForm1.SDUSystemTrayIconBalloonUserClick(Sender: TObject);
begin
  showmessage('Balloon was clicked on by user');

end;

procedure TForm1.SDUSystemTrayIconBalloonShow(Sender: TObject);
begin
//  showmessage('Balloon displayed');

end;

procedure TForm1.SDUSystemTrayIconClick(Sender: TObject);
begin
  showmessage('Tasktary icon clicked');

end;

procedure TForm1.SDUSystemTrayIconDblClick(Sender: TObject);
begin
  showmessage('Tasktary icon dblclicked');

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  
  pnlOne.caption := '';
  pnlTwo.caption := '';

end;

END.


