unit fmeExplorerOptions;
 OBSOLETE
interface

uses
  Classes, fmeBaseOptions,
  Controls, Dialogs, ExtCtrls, Forms,
  fmeCommonExplorerOptions, ExplorerSettings, Graphics, Messages,
  SDUStdCtrls, Spin64,
  StdCtrls, SysUtils, Variants, Windows;

type


  TfmeExplorerOptions = class (TfmeCommonExplorerOptions)



  PRIVATE



  PROTECTED
    procedure _ReadSettings(config: TExplorerSettings); OVERRIDE;
    procedure _WriteSettings(config: TExplorerSettings); OVERRIDE;
  PUBLIC
    procedure Initialize(); OVERRIDE;
    procedure EnableDisableControls(); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
  //delphi
// lc utils
lcConsts,
  CommonConsts,
  frmCommonOptions, CommonSettings,
  lcDialogs,
  SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

const
  CONTROL_MARGIN_LBL_TO_CONTROL = 5;


















end.
