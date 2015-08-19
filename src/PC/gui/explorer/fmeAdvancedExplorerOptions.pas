unit fmeAdvancedExplorerOptions;
OBSOLETE
interface

uses
  Classes, fmeBaseOptions,
  Controls, Dialogs, ExtCtrls, Forms,
  fmeCommonExplorerOptions,
  ExplorerSettings, Graphics, Messages, SDUStdCtrls, Shredder, Spin64,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeAdvancedExplorerOptions = class (TfmeCommonExplorerOptions)
    procedure ControlChanged(Sender: TObject);
  PRIVATE

  PROTECTED
    procedure _ReadSettings(config: TExplorerSettings); OVERRIDE;
    procedure _WriteSettings(config: TExplorerSettings); OVERRIDE;

    procedure PopulateOverwriteMethods();
    procedure SetOverwriteMethod(useMethod: TShredMethod);
    function GetOverwriteMethod(): TShredMethod;
  PUBLIC
    procedure Initialize(); OVERRIDE;
    procedure EnableDisableControls(); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
  frmCommonOptions,
  CommonSettings,
   CommonConsts,
  OTFEFreeOTFEBase_U, SDUDialogs,
  SDUGeneral,
  SDUi18n;




procedure TfmeAdvancedExplorerOptions.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;




end.
