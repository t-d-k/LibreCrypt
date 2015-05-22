unit FreeOTFEExplorerfrmOptions;

interface
                                                     
uses
  Classes, ComCtrls, fmeAutorunOptions, fmeBaseOptions, CommonfmeOptions_PKCS11,
  frmCommonOptions, Controls, Dialogs, ExtCtrls,
  Forms,
  fmeAdvancedExplorerOptions, fmeCommonExplorerOptions,
  fmeExplorerOptions, FreeOTFEExplorerfmeOptions_WebDAV,
  Graphics, Messages, OTFEFreeOTFEDLL_U, SDUStdCtrls, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmExplorerOptions = class (TfrmCommonOptions)
    tsGeneral:                            TTabSheet;
    fmeOptions_FreeOTFEExplorerGeneral1:  TfmeExplorerOptions;
    tsAdvanced:                           TTabSheet;
    fmeOptions_FreeOTFEExplorerAdvanced1: TfmeAdvancedExplorerOptions;
    tsWebDAV:                             TTabSheet;
    fmeOptions_FreeOTFEExplorerWebDAV1:   TfmeExplorerWebDAVOptions;
    tsAutorun:                            TTabSheet;
    fmeOptions_Autorun1:                  TfmeAutorunOptions;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PUBLIC
//    function OTFEFreeOTFE(): TOTFEFreeOTFEDLL;

    procedure ChangeLanguage(langCode: String); OVERRIDE;
  end;


implementation

{$R *.dfm}

uses
  ShlObj,  // Required for CSIDL_PROGRAMS
  ExplorerSettings,
  OTFEFreeOTFEBase_U,
  SDUDialogs, SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

procedure TfrmExplorerOptions.ChangeLanguage(langCode: String);
var
  tmpConfig: TExplorerSettings;
begin
  tmpConfig := TExplorerSettings.Create();
  try
    tmpConfig.Assign(gSettings);
    AllTabs_WriteSettings(tmpConfig);

    SDUSetLanguage(langCode);
    try
      SDURetranslateComponent(self);
    except
      on E: Exception do begin
        SDUTranslateComponent(self);
      end;
    end;

    AllTabs_InitAndReadSettings(tmpConfig);

  finally
    tmpConfig.Free();
  end;

  // Call EnableDisableControls() as this re-jigs the "Save above settings to:"
  // label
  EnableDisableControls();
end;


procedure TfrmExplorerOptions.FormCreate(Sender: TObject);
begin
  inherited;

  // Set active page to the first one
  pcOptions.ActivePage := tsGeneral;

end;

procedure TfrmExplorerOptions.FormShow(Sender: TObject);
begin
  inherited;

  // Push the "Advanced" tab to the far end; even after the "PKCS#11" tab
  tsAdvanced.PageIndex := (pcOptions.PageCount - 1);

end;

//function TfrmExplorerOptions.OTFEFreeOTFE(): TOTFEFreeOTFEDLL;
//begin
//  Result := TOTFEFreeOTFEDLL(OTFEFreeOTFEBase);
//end;

end.
