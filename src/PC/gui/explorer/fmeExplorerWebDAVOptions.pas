unit fmeExplorerWebDAVOptions;
  OBSOLETE
interface

uses
  Classes, fmeBaseOptions,
  Controls, Dialogs, ExtCtrls, Forms,
  fmeCommonExplorerOptions,
  ExplorerSettings, Graphics, Messages, SDUFilenameEdit_U, SDUFrames,
  SDUStdCtrls, Shredder, Spin64,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeExplorerWebDAVOptions = class (TfmeCommonExplorerOptions)
    procedure ControlChanged(Sender: TObject);
    procedure ckWebDAVClick(Sender: TObject);
    procedure ckWebDAVLogAccessClick(Sender: TObject);
    procedure ckWebDAVLogDebugClick(Sender: TObject);
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
lcTypes,

  frmCommonOptions,
  CommonSettings,
  CommonConsts ,
  lcConsts, OTFE_U,
  OTFEFreeOTFEBase_U,
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
