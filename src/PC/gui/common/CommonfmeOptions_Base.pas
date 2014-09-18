unit CommonfmeOptions_Base;

interface

uses
  Classes, CommonSettings, Controls, Dialogs, Forms,
  Graphics, Messages, SysUtils, Variants, Windows;

type
  TfmeOptions_Base = class (TFrame)
  PRIVATE
    { Private declarations }
  PROTECTED
  PUBLIC
    procedure Initialize(); VIRTUAL;
    procedure EnableDisableControls(); VIRTUAL;
    procedure ReadSettings(config: TSettings); VIRTUAL; ABSTRACT;
    function CheckSettings(): Boolean; VIRTUAL;
    procedure WriteSettings(config: TSettings); VIRTUAL; ABSTRACT;
  end;

implementation

{$R *.dfm}

procedure TfmeOptions_Base.Initialize();
begin
  // Base class does nothing
end;

procedure TfmeOptions_Base.EnableDisableControls();
begin
  // Base class does nothing
end;

function TfmeOptions_Base.CheckSettings(): Boolean;
begin
  Result := True;
end;

end.
