unit FreeOTFEExplorerfrmOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CommonfrmOptions, ComCtrls, StdCtrls, SDUStdCtrls, ExtCtrls,
  OTFEFreeOTFEDLL_U, CommonfmeOptions_Base, CommonfmeOptions_PKCS11,
  FreeOTFEExplorerfmeOptions_General, FreeOTFEExplorerfmeOptions_Base,
  FreeOTFEExplorerfmeOptions_Advanced;

type
  TfrmOptions_FreeOTFEExplorer = class(TfrmOptions)
    tsGeneral: TTabSheet;
    fmeOptions_FreeOTFEExplorerGeneral1: TfmeOptions_FreeOTFEExplorerGeneral;
    tsAdvanced: TTabSheet;
    fmeOptions_FreeOTFEExplorerAdvanced1: TfmeOptions_FreeOTFEExplorerAdvanced;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    function  OTFEFreeOTFE(): TOTFEFreeOTFEDLL;

    procedure ChangeLanguage(langCode: string); override;
  end;


implementation

{$R *.dfm}

uses
  ShlObj,  // Required for CSIDL_PROGRAMS
  OTFEFreeOTFEBase_U,
  FreeOTFEExplorerSettings,
  SDUi18n,
  SDUGeneral,
  SDUDialogs;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}
  
procedure TfrmOptions_FreeOTFEExplorer.ChangeLanguage(langCode: string);
var
  tmpConfig: TFreeOTFEExplorerSettings;
begin
  tmpConfig := TFreeOTFEExplorerSettings.Create();
  try
    tmpConfig.Assign(Settings);
    AllTabs_WriteSettings(tmpConfig);

    SDUSetLanguage(langCode);
    try
      SDURetranslateComponent(self);
    except
      on E:Exception do
        begin
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


procedure TfrmOptions_FreeOTFEExplorer.FormCreate(Sender: TObject);
begin
  inherited;

  // Set active page to the first one
  pcOptions.ActivePage := tsGeneral;

end;

procedure TfrmOptions_FreeOTFEExplorer.FormShow(Sender: TObject);
begin
  inherited;

  // Push the "Advanced" tab to the far end; even after the "PKCS#11" tab
  tsAdvanced.PageIndex := (pcOptions.PageCount - 1);

end;

function TfrmOptions_FreeOTFEExplorer.OTFEFreeOTFE(): TOTFEFreeOTFEDLL;
begin
  Result := TOTFEFreeOTFEDLL(OTFEFreeOTFEBase);
end;

END.

