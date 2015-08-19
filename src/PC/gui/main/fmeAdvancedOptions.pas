unit fmeAdvancedOptions;
OBSOLETE - now in frmOptions
interface

uses


  Classes, fmeBaseOptions,
  Controls, Dialogs, ExtCtrls, Forms,
  fmeLcOptions, MainSettings, Graphics, Messages, SDUStdCtrls, Spin64,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeAdvancedOptions = class (TfmeLcOptions)
    procedure ControlChanged(Sender: TObject);
  PRIVATE

  PROTECTED
    procedure _ReadSettings(config: TMainSettings); OVERRIDE;
    procedure _WriteSettings(config: TMainSettings); OVERRIDE;
  PUBLIC
    procedure Initialize(); OVERRIDE;
    procedure EnableDisableControls(); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
  //sdu, lcutils
lcTypes,
  frmCommonOptions,
  CommonSettings,
  CommonConsts,
  OTFEFreeOTFEBase_U, SDUDialogs,
  SDUGeneral,
  SDUi18n;




procedure TfmeAdvancedOptions.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfmeAdvancedOptions.EnableDisableControls();
begin
  inherited;

  // Do nothing
end;





end.
