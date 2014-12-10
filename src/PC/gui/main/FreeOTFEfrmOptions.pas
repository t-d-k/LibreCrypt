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
  Classes, ComCtrls,
  CommonfmeOptions_AutoRun,
  CommonfmeOptions_Base,
  CommonfmeOptions_PKCS11,
  CommonfrmOptions, CommonSettings, Controls, Dialogs,
  ExtCtrls, Forms, FreeOTFEfmeOptions_Advanced, FreeOTFEfmeOptions_Base,
  FreeOTFEfmeOptions_General,
  FreeOTFEfmeOptions_Hotkeys,
  FreeOTFEfmeOptions_SystemTray, Graphics, Messages, OTFEFreeOTFE_U, SDUForms,
  SDUStdCtrls, StdCtrls, SysUtils, Windows;

type
  TfrmOptions_FreeOTFE = class (TfrmOptions)
    tsGeneral:                    TTabSheet;
    tsHotkeys:                    TTabSheet;
    tcSystemTray:                 TTabSheet;
    tsAutorun:                    TTabSheet;
    tsAdvanced:                   TTabSheet;
    fmeOptions_FreeOTFEGeneral1:  TfmeOptions_FreeOTFEGeneral;
    fmeOptions_Hotkeys1:          TfmeOptions_Hotkeys;
    fmeOptions_SystemTray1:       TfmeOptions_SystemTray;
    fmeOptions_FreeOTFEAdvanced1: TfmeOptions_FreeOTFEAdvanced;
    fmeOptions_Autorun1:          TfmeOptions_Autorun;
    ckLaunchAtStartup:            TSDUCheckBox;
    ckLaunchMinimisedAtStartup:   TSDUCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ckLaunchAtStartupClick(Sender: TObject);
  PROTECTED
    FOrigLaunchAtStartup:          Boolean;
    FOrigLaunchMinimisedAtStartup: Boolean;

    procedure EnableDisableControls(); OVERRIDE;

    procedure AllTabs_InitAndReadSettings(config: TSettings); OVERRIDE;

    function DoOKClicked(): Boolean; OVERRIDE;

  PUBLIC
    function OTFEFreeOTFE(): TOTFEFreeOTFE;

    procedure ChangeLanguage(langCode: String); OVERRIDE;
  end;

implementation

{$R *.DFM}

uses
  Math,
  ShlObj,  // Required for CSIDL_PROGRAMS
  FreeOTFESettings,
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

procedure TfrmOptions_FreeOTFE.ChangeLanguage(langCode: String);
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

  FOrigLaunchAtStartup      := SDUDoesShortcutExist(SDU_CSIDL_STARTUP, Application.Title);
  ckLaunchAtStartup.Checked := FOrigLaunchAtStartup;

  FOrigLaunchMinimisedAtStartup := False;
  if FOrigLaunchAtStartup then begin
    FOrigLaunchMinimisedAtStartup :=
      (SDUGetShortCutRunWindowState(SDU_CSIDL_STARTUP, Application.Title) =
      wsMinimized);

  end;
  ckLaunchMinimisedAtStartup.Checked := FOrigLaunchMinimisedAtStartup;

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

  SDUEnableControl(ckLaunchMinimisedAtStartup, ckLaunchAtStartup.Checked);
  if not (ckLaunchMinimisedAtStartup.Enabled) then begin
    ckLaunchMinimisedAtStartup.Checked := False;
  end;

end;


procedure TfrmOptions_FreeOTFE.AllTabs_InitAndReadSettings(config: TSettings);
var
  ckboxIndent:  Integer;
  maxCBoxWidth: Integer;
begin
  inherited;

  ckLaunchAtStartup.Caption := SDUParamSubstitute(_('Start %1 at system startup'),
    [Application.Title]);

  ckboxIndent  := ckLaunchMinimisedAtStartup.left - ckLaunchAtStartup.left;
  maxCBoxWidth := max(ckAssociateFiles.Width, ckLaunchAtStartup.Width);

  ckAssociateFiles.Left           := ((self.Width - maxCBoxWidth) div 2);
  ckLaunchAtStartup.Left          := ckAssociateFiles.Left;
  ckLaunchMinimisedAtStartup.left := ckLaunchAtStartup.left + ckboxIndent;

  EnableDisableControls();
end;

function TfrmOptions_FreeOTFE.DoOKClicked(): Boolean;
var
  allOK:             Boolean;
  useRunWindowState: TWindowState;
begin
  allOK := inherited DoOKClicked();

  if allOK then begin
    if ((ckLaunchAtStartup.Checked <> FOrigLaunchAtStartup) or
      (ckLaunchMinimisedAtStartup.Checked <> FOrigLaunchMinimisedAtStartup)) then begin
      // Purge any existing shortcut...
      SDUDeleteShortcut(
        SDU_CSIDL_STARTUP,
        Application.Title
        );

      // ...and recreate if necessary
      if ckLaunchAtStartup.Checked then begin
        useRunWindowState := wsNormal;
        if ckLaunchMinimisedAtStartup.Checked then begin
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

      FOrigAssociateFiles := ckAssociateFiles.Checked;
    end;

  end;

  Result := allOK;
end;


end.
