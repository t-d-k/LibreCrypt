unit FreeOTFEfmeOptions_Base;

interface

uses
  Classes, CommonfmeOptions_Base,
  CommonSettings,
  Controls, Dialogs, Forms,
  FreeOTFESettings, Graphics, Messages, SysUtils, Variants, Windows;

type
  TfmeFreeOTFEOptions_Base = class (TfmeOptions_Base)
  PROTECTED
    procedure _ReadSettings(config: TFreeOTFESettings); VIRTUAL; ABSTRACT;
    procedure _WriteSettings(config: TFreeOTFESettings); VIRTUAL; ABSTRACT;
  PUBLIC
    procedure ReadSettings(config: TSettings); OVERRIDE;
    procedure WriteSettings(config: TSettings); OVERRIDE;
  end;


implementation

{$R *.dfm}

procedure TfmeFreeOTFEOptions_Base.ReadSettings(config: TSettings);
begin
  _ReadSettings(TFreeOTFESettings(config));
end;

procedure TfmeFreeOTFEOptions_Base.WriteSettings(config: TSettings);
begin
  _WriteSettings(TFreeOTFESettings(config));
end;

end.
