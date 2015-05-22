unit frmOptions;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
// delphi
  Classes, ComCtrls, StdCtrls, SysUtils, Windows,Graphics, Messages,Controls, Dialogs,
  fmeAutorunOptions,
  fmeBaseOptions,
  CommonfmeOptions_PKCS11,
  frmCommonOptions, CommonSettings,              
  ExtCtrls, Forms, fmeAdvancedOptions, fmeLcOptions,
  fmeGeneralOptions,
  fmeHotKeysOptions,
  fmeSystemTrayOptions,  OTFEFreeOTFE_U, SDUForms,
  SDUStdCtrls
  // librecrypt
  ;

type
  TfrmOptions = class (TfrmCommonOptions)
    tsGeneral:           TTabSheet;
    tsHotkeys:           TTabSheet;
    tcSystemTray:        TTabSheet;
    tsAutorun:           TTabSheet;
    tsAdvanced:          TTabSheet;
    fmeOptions_FreeOTFEGeneral1: TfmeGeneralOptions;
    fmeOptions_Hotkeys1: TfmeHotKeysOptions;
    fmeOptions_SystemTray1: TfmeSystemTrayOptions;
    fmeOptions_FreeOTFEAdvanced1: TfmeAdvancedOptions;
    fmeOptions_Autorun1: TfmeAutorunOptions;
    ckLaunchAtStartup:   TSDUCheckBox;
    ckLaunchMinimisedAtStartup: TSDUCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ckLaunchAtStartupClick(Sender: TObject);
  protected
    FOrigLaunchAtStartup:          Boolean;
    FOrigLaunchMinimisedAtStartup: Boolean;

    procedure EnableDisableControls(); override;

    procedure AllTabs_InitAndReadSettings(config: TCommonSettings); override;

    function DoOKClicked(): Boolean; override;

  public
    //    function OTFEFreeOTFE(): TOTFEFreeOTFE;

    procedure ChangeLanguage(langCode: String); override;
  end;

implementation

{$R *.DFM}

uses
  // delphi

  Math,
  ShlObj,  // Required for CSIDL_PROGRAMS
  StrUtils,
  // sdu /librecrypt
  MainSettings,
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

procedure TfrmOptions.ChangeLanguage(langCode: String);
var
  tmpConfig: TMainSettings;
begin
  tmpConfig := TMainSettings.Create();
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


procedure TfrmOptions.ckLaunchAtStartupClick(Sender: TObject);
begin
  inherited;
  EnableDisableControls();
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  inherited;
  // Set active page to the first one
  pcOptions.ActivePage := tsGeneral;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  inherited;

  FOrigLaunchAtStartup      := SDUDoesShortcutExist(SDU_CSIDL_STARTUP, Application.Title);
  ckLaunchAtStartup.Checked := FOrigLaunchAtStartup;

  FOrigLaunchMinimisedAtStartup := False;
  if FOrigLaunchAtStartup then
    FOrigLaunchMinimisedAtStartup := AnsiContainsStr(SDUGetShortCutArguments(SDU_CSIDL_STARTUP, Application.Title),CMDLINE_MINIMIZE);

  ckLaunchMinimisedAtStartup.Checked := FOrigLaunchMinimisedAtStartup;

  // Push the "Advanced" tab to the far end; even after the "PKCS#11" tab
  tsAdvanced.PageIndex := (pcOptions.PageCount - 1);
end;

 //function TfrmOptions.OTFEFreeOTFE(): TOTFEFreeOTFE;
 //begin
 //  Result := TOTFEFreeOTFE(OTFEFreeOTFEBase);
 //end;

procedure TfrmOptions.EnableDisableControls();
begin
  inherited;

  SDUEnableControl(ckLaunchMinimisedAtStartup, ckLaunchAtStartup.Checked);
  if not (ckLaunchMinimisedAtStartup.Enabled) then
    ckLaunchMinimisedAtStartup.Checked := False;
end;


procedure TfrmOptions.AllTabs_InitAndReadSettings(config: TCommonSettings);
//var
//  ckboxIndent:  Integer;
//  maxCBoxWidth: Integer;
begin
  inherited;

  ckLaunchAtStartup.Caption := Format(_('Start %s at system startup'),
    [Application.Title]);

//  ckboxIndent  := ckLaunchMinimisedAtStartup.left - ckLaunchAtStartup.left;
//  maxCBoxWidth := max(ckAssociateFiles.Width, ckLaunchAtStartup.Width);
//
//  ckAssociateFiles.Left           := ((self.Width - maxCBoxWidth) div 2);
//  ckLaunchAtStartup.Left          := ckAssociateFiles.Left;
//  ckLaunchMinimisedAtStartup.left := ckLaunchAtStartup.left + ckboxIndent;

  EnableDisableControls();
end;

function TfrmOptions.DoOKClicked(): Boolean;
var
  minimisedParam: string;
begin
  Result := inherited DoOKClicked();

  if Result then begin
    if ((ckLaunchAtStartup.Checked <> FOrigLaunchAtStartup) or
      (ckLaunchMinimisedAtStartup.Checked <> FOrigLaunchMinimisedAtStartup)) then begin
      // Purge any existing shortcut...
      SDUDeleteShortcut(
        SDU_CSIDL_STARTUP,
        Application.Title
        );

      // ...and recreate if necessary
      if ckLaunchAtStartup.Checked then begin
        minimisedParam := Ifthen(ckLaunchMinimisedAtStartup.Checked,'/'+CMDLINE_MINIMIZE,'');

        SDUCreateShortcut(
          SDU_CSIDL_STARTUP,
          Application.Title,
          ParamStr(0) ,
          minimisedParam,
          '',
          // ShortcutKey: TShortCut;  - not yet implemented
          wsNormal,  // for some reason wsMinimized  doesnt work here - makes app hang. so use 'minimise' flag instead
          ''
          );
      end;

      FOrigAssociateFiles := ckAssociateFiles.Checked;
    end;

  end;

end;


end.
