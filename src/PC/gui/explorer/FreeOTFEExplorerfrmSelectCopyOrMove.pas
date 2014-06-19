unit FreeOTFEExplorerfrmSelectCopyOrMove;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SDUForms,
  FreeOTFEExplorerfrmMain; // Required for TFExplOperation

type
  TfrmSelectCopyOrMove = class(TSDUForm)
    Label1: TLabel;
    pbCopy: TButton;
    pbCancel: TButton;
    pbMove: TButton;
    procedure pbCopyClick(Sender: TObject);
    procedure pbMoveClick(Sender: TObject);
  private
    { Private declarations }
  public
    OpType: TFExplOperation;
  end;

implementation

{$R *.dfm}

procedure TfrmSelectCopyOrMove.pbCopyClick(Sender: TObject);
begin
  OpType := cmCopy;
  ModalResult := mrOK;
end;

procedure TfrmSelectCopyOrMove.pbMoveClick(Sender: TObject);
begin
  OpType := cmMove;
  ModalResult := mrOK;
end;

END.

