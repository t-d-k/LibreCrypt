unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, SDUDropFiles, SDUForms;

type
  TForm1 = class(TSDUForm)
    SDUDropFiles1: TSDUDropFiles;
    Panel1: TPanel;
    ListView1: TListView;
    RichEdit1: TRichEdit;
    pbClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    SDUDropFiles2: TSDUDropFiles;
    SDUDropFiles3: TSDUDropFiles;
    SDUDropFiles4: TSDUDropFiles;
    Label5: TLabel;
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SDUDropFilesDirectoryDrop(Sender: TObject; dropItem: string;
      DropPoint: TPoint);
    procedure SDUDropFilesFileDrop(Sender: TObject; dropItem: string;
      DropPoint: TPoint);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
begin
  self.Caption := Application.Title;

  ListView1.AddItem('Item one', nil);
  ListView1.AddItem('Item two', nil);
  ListView1.AddItem('Item three', nil);

  RichEdit1.Lines.Clear();

  SDUDropFiles1.Active := TRUE;
  SDUDropFiles2.Active := TRUE;
  SDUDropFiles3.Active := TRUE;
  SDUDropFiles4.Active := TRUE;
end;

procedure TForm1.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TForm1.SDUDropFilesDirectoryDrop(Sender: TObject; dropItem: string;
  DropPoint: TPoint);
begin
  RichEdit1.Lines.Add('Directory dropped.');
  RichEdit1.Lines.Add('Directory: '+dropItem);
  RichEdit1.Lines.Add('Dropped onto: '+TSDUDropFiles(Sender).DropControl.Name);
  RichEdit1.Lines.Add('Dropped at: '+inttostr(DropPoint.X)+', '+inttostr(DropPoint.Y));
  RichEdit1.Lines.Add('');
end;

procedure TForm1.SDUDropFilesFileDrop(Sender: TObject; dropItem: string;
  DropPoint: TPoint);
begin
  RichEdit1.Lines.Add('File dropped.');
  RichEdit1.Lines.Add('Filename: '+dropItem);
  RichEdit1.Lines.Add('Dropped onto: '+TSDUDropFiles(Sender).DropControl.Name);
  RichEdit1.Lines.Add('Dropped at: '+inttostr(DropPoint.X)+', '+inttostr(DropPoint.Y));
  RichEdit1.Lines.Add('');
end;

END.

