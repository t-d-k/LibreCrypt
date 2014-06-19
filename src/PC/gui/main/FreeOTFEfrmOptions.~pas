unit FreeOTFEfrmOptions;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  CommonSettings, FreeOTFEfmeOptions_SystemTray, FreeOTFEfmeOptions_Hotkeys,
  FreeOTFEfmeOptions_General,
  FreeOTFEfmeOptions_AutoRun, 
  OTFEFreeOTFE_U, ExtCtrls, SDUForms, SDUStdCtrls, CommonfmeOptions_Base,
  FreeOTFEfmeOptions_Base, CommonfrmOptions, CommonfmeOptions_PKCS11,
  FreeOTFEfmeOptions_Advanced;

type
  TfrmOptions_FreeOTFE = class(TfrmOptions)
    tsGeneral: TTabSheet;
    tsHotkeys: TTabSheet;
    tcSystemTray: TTabSheet;
    tsAutorun: TTabSheet;
    tsAdvanced: TTabSheet;
    fmeOptions_FreeOTFEGeneral1: TfmeOptions_FreeOTFEGeneral;
    fmeOptions_Hotkeys1: TfmeOptions_Hotkeys;
    fmeOptions_SystemTray1: TfmeOptions_SystemTray;
    fmeOptions_FreeOTFEAdvanced1: TfmeOptions_FreeOTFEAdvanced;
    fmeOptions_Autorun1: TfmeOptions_Autorun;
    ckLaunchAtStartup: TSDUCheckBox;
    ckLaunchMinimisedAtStartup: TSDUCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ckLaunchAtStartupClick(Sender: TObject);
  protected
    FOrigLaunchAtStartup: boolean;
    FOrigLaunchMinimisedAtStartup: boolean;

    procedure EnableDisableControls(); override;

    procedure AllTabs_InitAndReadSettings(config: TSettings); override;

    function  DoOKClicked(): boolean; override;

  public
    function OTFEFreeOTFE(): TOTFEFreeOTFE;

    procedure ChangeLanguage(langCode: string); override;
  end;

implementation

{$R *.DFM}

uses
  Math,
  ShlObj,  // Required for CSIDL_PROGRAMS
  OTFEFreeOTFEBase_U,
  FreeOTFESettings,
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
  
procedure TfrmOptions_FreeOTFE.ChangeLanguage(langCode: string);
var
  tmpConfig: TFreeOTFESettings;
begin
  tmpConfig := TFreeOTFESettings.Create();
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


procedure TfrmOptions_FreeOTFE.ckLaunchAtStartupClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfrmOptions_FreeOTFE.FormCreate(Sender: TObject);
begin
  inherited;
  
  // Set active page to the first one
  pcOptions.ActivePage := tsGeneral;

end;

procedure TfrmOptions_FreeOTFE.FormShow(Sender: TObject);
begin
  inherited;

  FOrigLaunchAtStartup := SDUDoesShortcutExist(
                                          SDU_CSIDL_STARTUP,
                                          Application.Title
                                          );
  ckLaunchAtStartup.checked := FOrigLaunchAtStartup;

  FOrigLaunchMinimisedAtStartup := FALSE;
  if FOrigLaunchAtStartup then
    begin
    FOrigLaunchMinimisedAtStartup := (SDUGetShortCutRunWindowState(
                                                               SDU_CSIDL_STARTUP,
                                                               Application.Title
                                                               ) = wsMinimized);

    end;
  ckLaunchMinimisedAtStartup.checked := FOrigLaunchMinimisedAtStartup;

  // Push the "Advanced" tab to the far end; even after the "PKCS#11" tab
  tsAdvanced.PageIndex := (pcOptions.PageCount - 1);

end;

function TfrmOptions_FreeOTFE.OTFEFreeOTFE(): TOTFEFreeOTFE;
begin
  Result := TOTFEFreeOTFE(OTFEFreeOTFEBase);
end;

procedure TfrmOptions_FreeOTFE.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(ckLaunchMinimisedAtStartup, ckLaunchAtStartup.checked);
  if not(ckLaunchMinimisedAtStartup.Enabled) then
    begin
    ckLaunchMinimisedAtStartup.checked := FALSE;
    end;

end;


procedure TfrmOptions_FreeOTFE.AllTabs_InitAndReadSettings(config: TSettings);
var
  ckboxIndent: integer;
  maxCBoxWidth: integer;
begin
  inherited;

  ckLaunchAtStartup.Caption := SDUParamSubstitute(
                                                 _('Start %1 at system startup'),
                                                 [Application.Title]
                                                );

  ckboxIndent := ckLaunchMinimisedAtStartup.left - ckLaunchAtStartup.left;
  maxCBoxWidth := max(ckAssociateFiles.width, ckLaunchAtStartup.width);

  ckAssociateFiles.Left := ((self.Width - maxCBoxWidth) div 2);
  ckLaunchAtStartup.Left := ckAssociateFiles.Left;
  ckLaunchMinimisedAtStartup.left := ckLaunchAtStartup.left + ckboxIndent;

  EnableDisableControls();
end;

function TfrmOptions_FreeOTFE.DoOKClicked(): boolean;
var
  allOK: boolean;
  useRunWindowState: TWindowState;
begin
  allOK := inherited DoOKClicked();

  if allOK then
    begin
    if (
        (ckLaunchAtStartup.checked <> FOrigLaunchAtStartup) or
        (ckLaunchMinimisedAtStartup.checked <> FOrigLaunchMinimisedAtStartup)
       ) then
      begin
      // Purge any existing shortcut...
      SDUDeleteShortcut(
                        SDU_CSIDL_STARTUP,
                        Application.Title
                       );

      // ...and recreate if necessary
      if ckLaunchAtStartup.checked then
        begin
        useRunWindowState := wsNormal;
        if ckLaunchMinimisedAtStartup.checked then
          begin
          useRunWindowState := wsMinimized;
          end;

        SDUCreateShortcut(
                          SDU_CSIDL_STARTUP,
                          Application.Title,

                          ParamStr(0),

                          '',
                          '',
                          // ShortcutKey: TShortCut;  - not yet implemented
                          useRunWindowState,
                          ''
                         );
        end;

      FOrigAssociateFiles := ckAssociateFiles.checked;
      end;

    end;
        
  Result := allOK;
end;


END.


