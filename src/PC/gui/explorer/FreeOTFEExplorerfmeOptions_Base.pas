unit FreeOTFEExplorerfmeOptions_Base;

interface

uses
  Classes, fmeBaseOptions,
  CommonSettings,
  Controls, Dialogs, Forms,
  ExplorerSettings, Graphics, Messages, SysUtils, Variants, Windows;

type
  TfmeCommonExplorerOptions = class (TfmeBaseOptions)
  PROTECTED
    procedure _ReadSettings(config: TExplorerSettings); VIRTUAL; ABSTRACT;
    procedure _WriteSettings(config: TExplorerSettings); VIRTUAL; ABSTRACT;
  PUBLIC
    procedure ReadSettings(config: TCommonSettings); OVERRIDE;
    procedure WriteSettings(config: TCommonSettings); OVERRIDE;
  end;


implementation

{$R *.dfm}

procedure TfmeCommonExplorerOptions.ReadSettings(config: TCommonSettings);
begin
  _ReadSettings(TExplorerSettings(config));
end;

procedure TfmeCommonExplorerOptions.WriteSettings(config: TCommonSettings);
begin
  _WriteSettings(TExplorerSettings(config));
end;

end.
