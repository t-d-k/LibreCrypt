unit fmeHotKeysOptions;
OBSOLETE - now in frmOptions
interface

uses
  Classes, ComCtrls, fmeBaseOptions,
  Controls, Dialogs, Forms,
  fmeLcOptions,
  Graphics, Messages, SDUStdCtrls, StdCtrls, SysUtils, Variants, Windows;

type
  TfmeHotKeysOptions = class (TfmeLcOptions)
    procedure ckCheckBoxClick(Sender: TObject);
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
  Math,
  SDUGeneral;











end.
