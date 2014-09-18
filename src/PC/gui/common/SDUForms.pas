unit SDUForms;

interface

uses
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, SysUtils, Variants, Windows;

type
  TSDUForm = class (TForm)
  PRIVATE
    { Private declarations }
  PROTECTED
    procedure DoCreate; OVERRIDE;
  PUBLIC
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

end.
