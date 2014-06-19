unit CommonfmeOptions_PKCS11;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  OTFEFreeOTFEBase_U, CommonSettings, SDUFrames, SDUFilenameEdit_U,
  OTFEFreeOTFE_VolumeSelect, CommonfmeOptions_Base;

type
  TfmeOptions_PKCS11 = class(TfmeOptions_Base)
    gbPKCS11: TGroupBox;
    lblLibrary: TLabel;
    ckEnablePKCS11: TCheckBox;
    pbVerify: TButton;
    gbPKCS11AutoActions: TGroupBox;
    lblAutoMountVolume: TLabel;
    ckPKCS11AutoDismount: TCheckBox;
    ckPKCS11AutoMount: TCheckBox;
    feLibFilename: TSDUFilenameEdit;
    pbAutoDetect: TButton;
    OTFEFreeOTFEVolumeSelect1: TOTFEFreeOTFEVolumeSelect;
    procedure pbVerifyClick(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbAutoDetectClick(Sender: TObject);
  private
    function LibraryDLL(): string;
    function VerifyLibrary(): boolean;

  public
    OTFEFreeOTFE: TOTFEFreeOTFEBase;

    procedure EnableDisableControls(); override;
    procedure Initialize(); override;
    procedure ReadSettings(config: TSettings); override;
    procedure WriteSettings(config: TSettings); override;
    function  CheckSettings(): boolean; override;
  end;

implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral,
  SDUDialogs,
  pkcs11_library,
  OTFEFreeOTFEDLL_U;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}
  
resourcestring
  FILTER_LIBRARY_FILES = 'Library files (*.dll)|*.dll|All files|*.*';

procedure TfmeOptions_PKCS11.ControlChanged(Sender: TObject);
begin
  inherited;
  EnableDisableControls();

end;

procedure TfmeOptions_PKCS11.pbAutoDetectClick(Sender: TObject);
var
  detectedLib: string;
begin
  inherited;

  detectedLib := PKCS11AutoDetectLibrary(self);
  if (detectedLib = PKCS11_USER_CANCELED) then
    begin
    // User canceled - do nothing
    end
  else if (detectedLib = '') then
    begin
    SDUMessageDlg(
                  _('No PKCS#11 libraries detected.'+SDUCRLF+
                    SDUCRLF+
                    'Please manually enter the PKCS#11 library to be used.'),
                  mtError
                 );
    end
  else
    begin
    feLibFilename.Filename := detectedLib;
    end;

end;


procedure TfmeOptions_PKCS11.pbVerifyClick(Sender: TObject);
begin
  inherited;
  
  if VerifyLibrary() then
    begin
    SDUMessageDlg(_('PKCS#11 library appears to be functional'), mtInformation);
    end;

end;

procedure TfmeOptions_PKCS11.Initialize();
begin
  SDUCenterControl(gbPKCS11, ccHorizontal);
  SDUCenterControl(gbPKCS11, ccVertical, 25);

  feLibFilename.DefaultExt := 'dll';
  feLibFilename.OpenDialog.Options := feLibFilename.OpenDialog.Options + [ofPathMustExist, ofFileMustExist, ofForceShowHidden];
  feLibFilename.OpenDialog.Options := feLibFilename.OpenDialog.Options + [ofDontAddToRecent];
  feLibFilename.Filter := FILTER_LIBRARY_FILES;
  feLibFilename.FilterIndex := 0;

  OTFEFreeOTFEVolumeSelect1.OTFEFreeOTFE := OTFEFreeOTFE;
  OTFEFreeOTFEVolumeSelect1.FileSelectFilter     := FILE_FILTER_FLT_VOLUMES;
  OTFEFreeOTFEVolumeSelect1.FileSelectDefaultExt := FILE_FILTER_DFLT_VOLUMES;

  // Disallow partition selection if running under DLL version
  OTFEFreeOTFEVolumeSelect1.AllowPartitionSelect := not(OTFEFreeOTFE is TOTFEFreeOTFEDLL);

end;

procedure TfmeOptions_PKCS11.ReadSettings(config: TSettings);
begin
  // General...
  ckEnablePKCS11.checked := config.OptPKCS11Enable;
  feLibFilename.Filename := config.OptPKCS11Library;

  ckPKCS11AutoMount.checked := config.OptPKCS11AutoMount;
  OTFEFreeOTFEVolumeSelect1.Filename := config.OptPKCS11AutoMountVolume;
  ckPKCS11AutoDismount.checked := config.OptPKCS11AutoDismount;

end;

procedure TfmeOptions_PKCS11.WriteSettings(config: TSettings);
begin
  // General...
  config.OptPKCS11Enable := ckEnablePKCS11.checked;
  config.OptPKCS11Library := LibraryDLL;
  config.OptPKCS11AutoMount := ckPKCS11AutoMount.checked;
  config.OptPKCS11AutoMountVolume := OTFEFreeOTFEVolumeSelect1.Filename;
  config.OptPKCS11AutoDismount := ckPKCS11AutoDismount.checked;

end;

function TfmeOptions_PKCS11.LibraryDLL(): string;
begin
  Result := trim(feLibFilename.Filename);
end;

function TfmeOptions_PKCS11.VerifyLibrary(): boolean;
var
  allOK: boolean;
begin
  allOK := TRUE;
  if (LibraryDLL = '') then
    begin
    SDUMessageDlg(_('Please specify the PKCS#11 library to be used'), mtError);
    allOK := FALSE;
    end;

  if allOK then
    begin
    allOK:= PKCS11VerifyLibrary(LibraryDLL);

    if not(allOK) then
      begin
      SDUMessageDlg(_('The library specified does not appear to be a valid/working PKCS#11 library'), mtError);
      end;
    end;

  Result := allOK;
end;


function TfmeOptions_PKCS11.CheckSettings(): boolean;
var
  allOK: boolean;
begin
  allOK := inherited CheckSettings();

  if allOK then
    begin
    if ckEnablePKCS11.checked then
      begin
      allOK := VerifyLibrary();
      end;
    end;

  if ckEnablePKCS11.checked then
    begin
    if allOK then
      begin
      if ckPKCS11AutoMount.checked then
        begin
        if (OTFEFreeOTFEVolumeSelect1.Filename = '') then
          begin
          SDUMessageDlg(
                        _('If automount on PKCS#11 token insertion is enabled, the volume to be mounted must be specified'),
                        mtError
                       );
          allOK := FALSE;
          end;
        end;

      end;
    end;

  Result := allOK;
end;

procedure TfmeOptions_PKCS11.EnableDisableControls();
var
  pkcs11Enabled: boolean;
begin
  inherited;

  pkcs11Enabled := ckEnablePKCS11.checked;

  SDUEnableControl(feLibFilename, pkcs11Enabled);
  SDUEnableControl(lblLibrary,    pkcs11Enabled);
  SDUEnableControl(pbAutoDetect,  pkcs11Enabled);
  SDUEnableControl(pbVerify,      (
                                   pkcs11Enabled and
                                   (LibraryDLL <> '')
                                  ));


  SDUEnableControl(gbPKCS11AutoActions, pkcs11Enabled);
  SDUEnableControl(OTFEFreeOTFEVolumeSelect1, (
                                               pkcs11Enabled and  // Yes, this is needed; otherwise it'll enable/disable regardless
                                               ckPKCS11AutoMount.checked
                                              ));
  SDUEnableControl(lblAutoMountVolume, OTFEFreeOTFEVolumeSelect1.enabled);

end;

END.

