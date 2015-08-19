unit fmeGeneralOptions;
OBSOLETE - now in frmOptions
interface

uses
  Classes, fmeBaseOptions,
  Controls, Dialogs, ExtCtrls, Forms,
  fmeLcOptions, MainSettings, Graphics, Messages, SDUStdCtrls, Spin64,
  StdCtrls, SysUtils, Variants, Windows;

type


  TfmeGeneralOptions = class (TfmeLcOptions)
    procedure pbLangDetailsClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
  PRIVATE


    procedure PopulateLanguages();
    procedure SetLanguageSelection(langCode: String);

    function SelectedLanguage(): TLanguageTranslation;
    function LanguageControlLanguage(idx: Integer): TLanguageTranslation;
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
  lcConsts,
  CommonConsts,
  frmCommonOptions, CommonSettings,
  OTFE_U,
  lcDialogs,
  SDUGeneral,
  SDUi18n;



const
  CONTROL_MARGIN_LBL_TO_CONTROL = 5;









end.
