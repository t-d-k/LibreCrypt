unit FreeOTFEfmeOptions_Base;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CommonfmeOptions_Base,
  CommonSettings,
  FreeOTFESettings;

type
  TfmeFreeOTFEOptions_Base = class(TfmeOptions_Base)
  protected
    procedure _ReadSettings(config: TFreeOTFESettings); virtual; abstract;
    procedure _WriteSettings(config: TFreeOTFESettings); virtual; abstract;
  public
    procedure ReadSettings(config: TSettings); override;
    procedure WriteSettings(config: TSettings); override;
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

END.

