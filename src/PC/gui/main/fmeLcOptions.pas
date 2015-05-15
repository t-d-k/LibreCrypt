unit fmeLcOptions;

interface

uses
  Classes, fmeBaseOptions,
  CommonSettings,
  Controls, Dialogs, Forms,
  MainSettings, Graphics, Messages, SysUtils, Variants, Windows;

type
  TfmeLcOptions = class (TfmeBaseOptions )
  PROTECTED
    procedure _ReadSettings(config: TMainSettings); VIRTUAL; ABSTRACT;
    procedure _WriteSettings(config: TMainSettings); VIRTUAL; ABSTRACT;
  PUBLIC
    procedure ReadSettings(config: TCommonSettings); OVERRIDE;
    procedure WriteSettings(config: TCommonSettings); OVERRIDE;
  end;


implementation

{$R *.dfm}

procedure TfmeLcOptions.ReadSettings(config: TCommonSettings);
begin
  _ReadSettings(TMainSettings(config));
end;

procedure TfmeLcOptions.WriteSettings(config: TCommonSettings);
begin
  _WriteSettings(TMainSettings(config));
end;

end.
