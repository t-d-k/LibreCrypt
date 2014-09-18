unit FreeOTFEExplorerfrmSelectCopyOrMove;

interface

uses
  Classes, Controls, Dialogs, Forms,
  FreeOTFEExplorerfrmMain, Graphics, Messages, SDUForms,
  StdCtrls,
  SysUtils, Variants, Windows; // Required for TFExplOperation

type
  TfrmSelectCopyOrMove = class (TSDUForm)
    Label1:   TLabel;
    pbCopy:   TButton;
    pbCancel: TButton;
    pbMove:   TButton;
    procedure pbCopyClick(Sender: TObject);
    procedure pbMoveClick(Sender: TObject);
  PRIVATE
    { Private declarations }
  PUBLIC
    OpType: TFExplOperation;
  end;

implementation

{$R *.dfm}

procedure TfrmSelectCopyOrMove.pbCopyClick(Sender: TObject);
begin
  OpType      := cmCopy;
  ModalResult := mrOk;
end;

procedure TfrmSelectCopyOrMove.pbMoveClick(Sender: TObject);
begin
  OpType      := cmMove;
  ModalResult := mrOk;
end;

end.
