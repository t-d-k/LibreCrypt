unit fmeBaseOptions;

interface

uses
  Classes, CommonSettings, Controls, Dialogs, Forms,
  Graphics, Messages, SysUtils, Variants, Windows;

type
  TfmeBaseOptions  = class (TFrame)
  PRIVATE
    { Private declarations }
  PROTECTED
  PUBLIC
    procedure Initialize(); VIRTUAL;
    procedure EnableDisableControls(); VIRTUAL;
    procedure ReadSettings(config: TCommonSettings); VIRTUAL; ABSTRACT;
    function CheckSettings(): Boolean; VIRTUAL;
    procedure WriteSettings(config: TCommonSettings); VIRTUAL; ABSTRACT;
  end;

implementation

{$R *.dfm}

procedure TfmeBaseOptions.Initialize();
begin
  // Base class does nothing
end;

procedure TfmeBaseOptions.EnableDisableControls();
begin
  // Base class does nothing
end;

function TfmeBaseOptions.CheckSettings(): Boolean;
begin
  Result := True;
end;

end.
