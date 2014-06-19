unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMan;

type
  TForm1 = class(TForm)
    XPManifest1: TXPManifest;
    Label1: TLabel;
    edTitle: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    edMainInstruction: TEdit;
    edContent: TEdit;
    pbClose: TButton;
    GroupBox1: TGroupBox;
    pbShowMessage: TButton;
    pbMessageDlg: TButton;
    pbMessageBox: TButton;
    GroupBox2: TGroupBox;
    pbSDUMessageBox: TButton;
    pbSDUVistaTaskDialog: TButton;
    pbSDUMessageDlg: TButton;
    procedure pbCloseClick(Sender: TObject);
    procedure pbSDUMessageBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbShowMessageClick(Sender: TObject);
    procedure pbMessageBoxClick(Sender: TObject);
    procedure pbMessageDlgClick(Sender: TObject);
    procedure pbSDUVistaTaskDialogClick(Sender: TObject);
    procedure pbSDUMessageDlgClick(Sender: TObject);
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
  SDUDialogs;


procedure TForm1.FormCreate(Sender: TObject);
begin
  self.Caption := Application.Title;

  edTitle.text           := 'DIALOG TITLE';
  edMainInstruction.text := 'DIALOG MAIN INSTRUCTION';
  edContent.text         := 'DIALOG CONTENT';

end;

procedure TForm1.pbShowMessageClick(Sender: TObject);
begin
  ShowMessage(edContent.text);

end;

procedure TForm1.pbCloseClick(Sender: TObject);
begin
  Close();

end;

procedure TForm1.pbMessageBoxClick(Sender: TObject);
var
  content: string;
  title: string;
begin
  content := edContent.text;
  title := edTitle.text;
  MessageBox(
             0,
             PChar(content),
             PChar(title),
             (MB_YESNOCANCEL or MB_ICONWARNING)
            );

end;

procedure TForm1.pbSDUMessageBoxClick(Sender: TObject);
begin
  SDUMessageBox(
                self.Handle,
                edContent.text,
                edTitle.text,
                (MB_YESNOCANCEL or MB_ICONWARNING)
               );

end;

procedure TForm1.pbMessageDlgClick(Sender: TObject);
begin
  MessageDlg(edContent.text, mtWarning, [mbOK], 0);

end;

procedure TForm1.pbSDUMessageDlgClick(Sender: TObject);
begin
  SDUMessageDlg(edContent.text, mtWarning, [mbOK], 0);

end;

procedure TForm1.pbSDUVistaTaskDialogClick(Sender: TObject);
begin    
  SDUVistaTaskDialog(
                     edTitle.text,
                     edMainInstruction.text,
                     edContent.text+' - WARNING',
                     (
                      SDVISTA_TDCBF_OK_BUTTON or
                      SDVISTA_TDCBF_YES_BUTTON or
                      SDVISTA_TDCBF_RETRY_BUTTON
                     ),
                     SDVISTA_TD_ICON_WARNING
                    );

  SDUVistaTaskDialog(
                     edTitle.text,
                     edMainInstruction.text,
                     edContent.text+' - QUESTION',
                     (
                      SDVISTA_TDCBF_OK_BUTTON or
                      SDVISTA_TDCBF_YES_BUTTON or
                      SDVISTA_TDCBF_RETRY_BUTTON
                     ),
                     SDVISTA_TD_ICON_QUESTION
                    );

  SDUVistaTaskDialog(
                     edTitle.text,
                     edMainInstruction.text,
                     edContent.text+' - ERROR',
                     (
                      SDVISTA_TDCBF_OK_BUTTON or
                      SDVISTA_TDCBF_YES_BUTTON or
                      SDVISTA_TDCBF_RETRY_BUTTON
                     ),
                     SDVISTA_TD_ICON_ERROR
                    );

  SDUVistaTaskDialog(
                     edTitle.text,
                     edMainInstruction.text,
                     edContent.text+' - INFORMATION',
                     (
                      SDVISTA_TDCBF_OK_BUTTON or
                      SDVISTA_TDCBF_YES_BUTTON or
                      SDVISTA_TDCBF_RETRY_BUTTON
                     ),
                     SDVISTA_TD_ICON_INFORMATION
                    );

//  output := SDUVistaTaskDialog(
//                               edTitle.text,
//                               edMainInstruction.text,
//                               edContent.text+' - BLANK_AGAIN',
//                               (
//                                SDVISTA_TDCBF_OK_BUTTON or
//                                SDVISTA_TDCBF_YES_BUTTON or
//                                SDVISTA_TDCBF_RETRY_BUTTON
//                               ),
//                               SDVISTA_TD_ICON_BLANK_AGAIN
//                              );

  SDUVistaTaskDialog(
                     edTitle.text,
                     edMainInstruction.text,
                     edContent.text+' - SHIELD',
                     (
                      SDVISTA_TDCBF_OK_BUTTON or
                      SDVISTA_TDCBF_YES_BUTTON or
                      SDVISTA_TDCBF_RETRY_BUTTON
                     ),
                     SDVISTA_TD_ICON_SHIELD
                    );

end;

END.

