unit FreeOTFEExplorerfrmNewDirDlg;

interface

uses
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, SDUForms, StdCtrls,
  SysUtils, Variants, Windows;

type
  TfrmNewDirDlg = class (TSDUForm)
    edDirName: TEdit;
    Label1:    TLabel;
    pbOK:      TButton;
    pbCancel:  TButton;
    procedure edDirNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    function GetDirName(): WideString;
    procedure SetDirName(newName: WideString);
  PROTECTED
    procedure EnableDisableControls();

  PUBLIC

  PUBLISHED
    property DirName: WideString Read GetDirName Write SetDirName;
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
  Result := edDirName.Text;
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

end.
