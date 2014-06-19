unit FreeOTFEExplorerfmeOptions_Base;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CommonfmeOptions_Base,
  CommonSettings,
  FreeOTFEExplorerSettings;

type
  TfmeFreeOTFEExplorerOptions_Base = class(TfmeOptions_Base)
  protected
    procedure _ReadSettings(config: TFreeOTFEExplorerSettings); virtual; abstract;
    procedure _WriteSettings(config: TFreeOTFEExplorerSettings); virtual; abstract;
  public
    procedure ReadSettings(config: TSettings); override;
    procedure WriteSettings(config: TSettings); override;
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

END.

