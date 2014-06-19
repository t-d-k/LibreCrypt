unit FreeOTFEExplorerfrmNewDirDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SDUForms;

type
  TfrmNewDirDlg = class(TSDUForm)
    edDirName: TEdit;
    Label1: TLabel;
    pbOK: TButton;
    pbCancel: TButton;
    procedure edDirNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function  GetDirName(): WideString;
    procedure SetDirName(newName: WideString);
  protected
    procedure EnableDisableControls();

  public

  published
    property DirName: WideString read GetDirName write SetDirName;
  end;

implementation

{$R *.dfm}

uses
  SDUGeneral;

procedure TfrmNewDirDlg.edDirNameChange(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmNewDirDlg.EnableDisableControls();
begin
  SDUEnableControl(pbOK, (trim(DirName) <> ''));
end;

function TfrmNewDirDlg.GetDirName(): WideString;
begin
  Result := edDirName.text;
end;

procedure TfrmNewDirDlg.SetDirName(newName: WideString);
begin
  edDirName.Text := newName;
  EnableDisableControls();
end;

procedure TfrmNewDirDlg.FormShow(Sender: TObject);
begin
  DirName := '';
  
  EnableDisableControls();

end;

END.

