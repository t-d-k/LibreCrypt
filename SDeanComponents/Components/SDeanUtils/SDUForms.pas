unit SDUForms;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TSDUForm = class(TForm)
  private
    { Private declarations }
  protected
    procedure DoCreate; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  SDUi18n;

procedure TSDUForm.DoCreate;
begin
  inherited;
  SDUTranslateComponent(self);
end;

END.


