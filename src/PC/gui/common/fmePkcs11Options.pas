unit fmePkcs11Options;
   OBSOLETE

interface

uses
  Classes, fmeBaseOptions, CommonSettings, Controls, Dialogs, Forms,
  Graphics, Messages, fmeVolumeSelect, OTFEFreeOTFEBase_U, SDUFilenameEdit_U,
  SDUFrames, StdCtrls,
  SysUtils, Variants, Windows;

type
  TfmePkcs11Options = class (TfmeBaseOptions)
    gbPKCS11:           TGroupBox;
    lblLibrary:         TLabel;
    ckEnablePKCS11:     TCheckBox;
    pbVerify:           TButton;
    gbPKCS11AutoActions: TGroupBox;
    lblAutoMountVolume: TLabel;
    ckPKCS11AutoDismount: TCheckBox;
    ckPKCS11AutoMount:  TCheckBox;
    feLibFilename:      TSDUFilenameEdit;
    pbAutoDetect:       TButton;
    OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect;
    procedure pbVerifyClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbAutoDetectClick(Sender: TObject);

  private
    function LibraryDLL(): String;
    function VerifyLibrary(): Boolean;

  public

    procedure EnableDisableControls(); override;
    procedure Initialize(); override;
    procedure ReadSettings(config: TCommonSettings); override;
    procedure WriteSettings(config: TCommonSettings); override;
    function CheckSettings(): Boolean; override;
    constructor Create(AOwner: TComponent); override;


  end;

implementation

{$R *.dfm}

uses
  lcDialogs,
  OTFEFreeOTFEDLL_U, pkcs11_library,
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



end.
