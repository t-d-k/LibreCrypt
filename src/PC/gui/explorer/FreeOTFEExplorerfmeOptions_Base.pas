unit FreeOTFEExplorerfmeOptions_Base;

interface

uses
  Classes, CommonfmeOptions_Base,
  CommonSettings,
  Controls, Dialogs, Forms,
  FreeOTFEExplorerSettings, Graphics, Messages, SysUtils, Variants, Windows;

type
  TfmeFreeOTFEExplorerOptions_Base = class (TfmeOptions_Base)
  PROTECTED
    procedure _ReadSettings(config: TFreeOTFEExplorerSettings); VIRTUAL; ABSTRACT;
    procedure _WriteSettings(config: TFreeOTFEExplorerSettings); VIRTUAL; ABSTRACT;
  PUBLIC
    procedure ReadSettings(config: TSettings); OVERRIDE;
    procedure WriteSettings(config: TSettings); OVERRIDE;
  end;


implementation

{$R *.dfm}

procedure TfmeFreeOTFEExplorerOptions_Base.ReadSettings(config: TSettings);
begin
  _ReadSettings(TFreeOTFEExplorerSettings(config));
end;

procedure TfmeFreeOTFEExplorerOptions_Base.WriteSettings(config: TSettings);
begin
  _WriteSettings(TFreeOTFEExplorerSettings(config));
end;

end.
