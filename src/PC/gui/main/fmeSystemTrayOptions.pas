unit fmeSystemTrayOptions;
OBSOLETE - now in frmOptions

interface

uses
  Classes, fmeBaseOptions, Controls, Dialogs, Forms,
  fmeLcOptions, MainSettings, Graphics, Messages, SDUStdCtrls,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeSystemTrayOptions = class (TfmeLcOptions)
    procedure ckUseSystemTrayIconClick(Sender: TObject);
  PRIVATE
    procedure PopulateAndSetClickAction(cbox: TComboBox; selectedAction: TSystemTrayClickAction);
    function GetClickAction(cbox: TComboBox): TSystemTrayClickAction;
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
