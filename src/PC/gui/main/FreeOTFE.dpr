program FreeOTFE;

uses
  Forms,
  SDUi18n,
  FreeOTFEfrmMain in 'FreeOTFEfrmMain.pas' {frmFreeOTFEMain},
  FreeOTFEConsts in 'FreeOTFEConsts.pas',
  FreeOTFEfrmVolProperties in 'FreeOTFEfrmVolProperties.pas' {frmFreeOTFEVolProperties},
  FreeOTFEfrmSelectOverwriteMethod in 'FreeOTFEfrmSelectOverwriteMethod.pas' {frmFreeOTFESelectOverwriteMethod},
  CommonfrmCDBBackupRestore in '..\common\CommonfrmCDBBackupRestore.pas' {frmCDBBackupRestore},
  CommonSettings in '..\common\CommonSettings.pas',
  CommonfrmOptions in '..\common\CommonfrmOptions.pas' {frmOptions},
  Windows,
  SDUGeneral,
  CommonfrmGridReport in '..\common\CommonfrmGridReport.pas' {frmGridReport},
  CommonfrmGridReport_Hash in '..\common\CommonfrmGridReport_Hash.pas' {frmGridReport_Hash},
  CommonfrmGridReport_Cypher in '..\common\CommonfrmGridReport_Cypher.pas' {frmGridReport_Cypher},
  CommonfmeOptions_Base in '..\common\CommonfmeOptions_Base.pas' {fmeOptions_Base: TFrame},
  FreeOTFEfmeOptions_SystemTray in 'FreeOTFEfmeOptions_SystemTray.pas' {fmeOptions_SystemTray: TFrame},
  FreeOTFEfmeOptions_Hotkeys in 'FreeOTFEfmeOptions_Hotkeys.pas' {fmeOptions_Hotkeys: TFrame},
  CommonfmeOptions_PKCS11 in '..\common\CommonfmeOptions_PKCS11.pas' {fmeOptions_PKCS11: TFrame},
  FreeOTFEfmeOptions_Autorun in 'FreeOTFEfmeOptions_Autorun.pas' {fmeOptions_Autorun: TFrame},
  FreeOTFEfmeOptions_General in 'FreeOTFEfmeOptions_General.pas' {fmeOptions_FreeOTFEGeneral: TFrame},
  CommonfrmInstallOnUSBDrive in '..\common\CommonfrmInstallOnUSBDrive.pas' {frmInstallOnUSBDrive},
  CommonfrmAbout in '..\common\CommonfrmAbout.pas' {frmAbout},
  FreeOTFESettings in 'FreeOTFESettings.pas',
  CommonfrmMain in '..\common\CommonfrmMain.pas' {frmMain},
  FreeOTFEfrmOptions in 'FreeOTFEfrmOptions.pas' {frmOptions_FreeOTFE},
  FreeOTFEfmeOptions_Base in 'FreeOTFEfmeOptions_Base.pas' {fmeFreeOTFEOptions_Base: TFrame},
  CommonfrmVersionCheck in '..\common\CommonfrmVersionCheck.pas' {frmVersionCheck},
  FreeOTFEfmeOptions_Advanced in 'FreeOTFEfmeOptions_Advanced.pas' {fmeOptions_FreeOTFEAdvanced: TFrame},
  CommonConsts in '..\common\CommonConsts.pas',
  CommonfrmCDBDump_LUKS in '..\common\CommonfrmCDBDump_LUKS.pas' {frmCDBDump_LUKS},
  CommonfrmCDBDump_Base in '..\common\CommonfrmCDBDump_Base.pas' {frmCDBDump_Base},
  CommonfrmCDBDump_FreeOTFE in '..\common\CommonfrmCDBDump_FreeOTFE.pas' {frmCDBDump_FreeOTFE};

{$R *.RES}

var
  otherRunningAppWindow: THandle;
  CommandLineOnly: boolean;
  cmdExitCode: integer;
  settingsFilename: string;
{$IFDEF VER185}
  // Delphi 7 doesn't need this, but Delphi 2007 (and 2006 as well? Not
  // checked...) need this to honor any "Run minimised" option set in any
  // launching MS Windows shortcut
  sui: TStartUpInfo;
{$ENDIF}

begin
  GLOBAL_VAR_WM_FREEOTFE_RESTORE := RegisterWindowMessage('FREEOTFE_RESTORE');
  GLOBAL_VAR_WM_FREEOTFE_REFRESH := RegisterWindowMessage('FREEOTFE_REFRESH');

  Application.Initialize;
{$IFDEF VER185}
  // Vista fix for Delphi 2007 and later
  Application.MainFormOnTaskbar := TRUE;
{$ENDIF}
  Application.Title := 'FreeOTFE';

  FreeOTFESettings.Settings:= TFreeOTFESettings.Create();
  try
    CommonSettings.CommonSettingsObj := FreeOTFESettings.Settings;
    if SDUCommandLineParameter(CMDLINE_SETTINGSFILE, settingsFilename) then
      begin
      settingsFilename := SDURelativePathToAbsolute(settingsFilename);
      FreeOTFESettings.Settings.CustomLocation := settingsFilename;
      end;

    FreeOTFESettings.Settings.Load();

    Application.ShowMainForm := FALSE;
    // NOTE: The main form's Visible property is set to FALSE anyway - it *HAS*
    // to be, otherwise it'll be displayed; dispite the following two lines
    Application.CreateForm(TfrmFreeOTFEMain, frmFreeOTFEMain);
  frmFreeOTFEMain.Visible := FALSE;
    Application.ShowMainForm := FALSE;

    CommandLineOnly := frmFreeOTFEMain.HandleCommandLineOpts(cmdExitCode);


    //   if we were called with no command line arguments then
    //     if no running app was found then
    //       we continue to run the main app
    //     else
    //       we raise the previous application to the top
    //   else
    //     quit (ran with command line arguments)

    // if we were called with no command line arguments then
    if (not(CommandLineOnly)) then
      begin
      otherRunningAppWindow := SDUDetectExistingApp();
      // If no running app was found then
      if (
          (otherRunningAppWindow = 0) or
          Settings.OptAllowMultipleInstances
         ) then
        begin
        // We continue to run the main app
        frmFreeOTFEMain.Visible := TRUE;
        Application.ShowMainForm := TRUE;
        frmFreeOTFEMain.InitApp();

{$IFDEF VER185}
        // Delphi 7 doesn't need this, but Delphi 2007 (and 2006 as well? Not
        // checked...) need this to honor any "Run minimised" option set in any
        // launching MS Windows shortcut
        GetStartUpInfo(sui);
        if ((sui.dwFlags and STARTF_USESHOWWINDOW) > 0) then
          begin
          if (
//              (sui.wShowWindow = SW_FORCEMINIMIZE) or
              (sui.wShowWindow = SW_HIDE) or
              (sui.wShowWindow = SW_MINIMIZE) or
              (sui.wShowWindow = SW_SHOWMINIMIZED) or
              (sui.wShowWindow = SW_SHOWMINNOACTIVE)
             ) then
            begin
            Application.Minimize();
            end;

          // *Should* have a corresponding check for SW_MAXIMIZE / SW_SHOWMAXIMIZED
          // here, but not a priority...
          end;
{$ENDIF}
        if SDUCommandLineSwitch(CMDLINE_MINIMIZE) then
          begin
          Application.Minimize();
          end;

        Application.Run;
        end
      else
        begin
        // Poke already running application
{$IFDEF VER185}
        SendMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_RESTORE, 0, 0);
{$ELSE}
        SDUPostMessageExistingApp(GLOBAL_VAR_WM_FREEOTFE_RESTORE, 0, 0);
{$ENDIF}

// Alternativly:
//         SetForegroundWindow(otherRunningAppWindow);
        end;
      end
    else
      begin
      // Do nothing; ran with command line arguments, already processed
      end;

  finally
    // Note: We *don't* free of the Settings object if the main form was shown;
    //       the main form is still closing down at this stage, and this causes
    //       an exception.
    //       Instead, this is free'd off in the FormDestroy(...) method
    if not(Application.ShowMainForm) then
      begin
      FreeOTFESettings.Settings.Free();
      end;
  end;

  if (
      CommandLineOnly and
      (cmdExitCode <> CMDLINE_SUCCESS)
     ) then
    begin
    Halt(cmdExitCode); // Note: System.ExitCode may be tested in finalization sections
    end;

END.

