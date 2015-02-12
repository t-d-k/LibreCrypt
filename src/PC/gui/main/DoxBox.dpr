program DoxBox;
    {$IFDEF EnableMemoryLeakReporting }
error
{$ENDIF}
uses
  FastMM4,
  Forms,
  FreeOTFEfrmMain in 'FreeOTFEfrmMain.pas' {frmFreeOTFEMain},
  FreeOTFEConsts in 'FreeOTFEConsts.pas',
  FreeOTFEfrmVolProperties in 'FreeOTFEfrmVolProperties.pas' {frmFreeOTFEVolProperties},
  FreeOTFEfrmSelectOverwriteMethod in 'FreeOTFEfrmSelectOverwriteMethod.pas' {frmFreeOTFESelectOverwriteMethod},
  CommonfrmCDBBackupRestore in '..\common\CommonfrmCDBBackupRestore.pas' {frmCDBBackupRestore},
  CommonSettings in '..\common\CommonSettings.pas',
  CommonfrmOptions in '..\common\CommonfrmOptions.pas' {frmOptions},
  Windows,
  CommonfrmGridReport in '..\common\CommonfrmGridReport.pas' {frmGridReport},
  CommonfrmGridReport_Hash in '..\common\CommonfrmGridReport_Hash.pas' {frmGridReport_Hash},
  CommonfrmGridReport_Cypher in '..\common\CommonfrmGridReport_Cypher.pas' {frmGridReport_Cypher},
  CommonfmeOptions_Base in '..\common\CommonfmeOptions_Base.pas' {fmeOptions_Base: TFrame},
  FreeOTFEfmeOptions_SystemTray in 'FreeOTFEfmeOptions_SystemTray.pas' {fmeOptions_SystemTray: TFrame},
  FreeOTFEfmeOptions_Hotkeys in 'FreeOTFEfmeOptions_Hotkeys.pas' {fmeOptions_Hotkeys: TFrame},
  CommonfmeOptions_PKCS11 in '..\common\CommonfmeOptions_PKCS11.pas' {fmeOptions_PKCS11: TFrame},
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
  CommonfrmCDBDump_FreeOTFE in '..\common\CommonfrmCDBDump_FreeOTFE.pas' {frmCDBDump_FreeOTFE},
  SDUForms in '..\common\SDUForms.pas' {SDUForm},
  SDUFrames in '..\common\SDUFrames.pas' {SDUFrame: TFrame},
  SDPartitionImage in '..\common\Filesystem\SDPartitionImage.pas',
  SDPartitionImage_File in '..\common\Filesystem\SDPartitionImage_File.pas',
  SDUProgressDlg in '..\common\SDeanUtils\SDUProgressDlg.pas' {SDUProgressDialog},
  SDUDiskPropertiesDlg in '..\common\SDeanUtils\SDUDiskPropertiesDlg.pas' {SDUDiskPropertiesDialog},
  SDUFilenameEdit_U in '..\common\SDeanUtils\SDUFilenameEdit_U.pas' {SDUFilenameEdit: TFrame},
  SDUPartitionPropertiesDlg in '..\common\SDeanUtils\SDUPartitionPropertiesDlg.pas' {SDUPartitionPropertiesDialog},
  SDUMRUList in '..\common\SDeanUtils\SDUMRUList.pas',
  SDURegistry in '..\common\SDeanUtils\SDURegistry.pas',
  SDUi18n in '..\common\SDeanUtils\SDUi18n.pas',
  gnugettext in '..\..\..\..\tools\gnugettext.pas',
  SDUGeneral in '..\common\SDeanUtils\SDUGeneral.pas',
  SDFilesystem in '..\common\Filesystem\SDFilesystem.pas',
  SDFilesystem_FAT in '..\common\Filesystem\SDFilesystem_FAT.pas',
  CommonfmeOptions_Autorun in '..\common\CommonfmeOptions_Autorun.pas' {fmeOptions_Autorun: TFrame},
  FreeOTFEDLLCypherAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLCypherAPI.pas',
  FreeOTFEDLLHashAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLHashAPI.pas',
  FreeOTFEDLLMainAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLMainAPI.pas',
  OTFEFreeOTFE_frmKeyEntryFreeOTFE in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmKeyEntryFreeOTFE.pas' {frmKeyEntryFreeOTFE},
  OTFEFreeOTFE_frmKeyEntryLinux in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmKeyEntryLinux.pas' {frmKeyEntryLinux},
  OTFEFreeOTFE_frmKeyEntryLUKS in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmKeyEntryLUKS.pas' {frmKeyEntryLUKS},
  OTFEFreeOTFE_frmWizard in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizard.pas' {frmWizard},
  OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile.pas' {frmWizardChangePasswordCreateKeyfile},
  SDURandPool in '..\common\OTFE\SDURandPool.pas',
  OTFEFreeOTFEBase_U in '..\common\OTFE\OTFEFreeOTFEBase_U.pas',
  OTFEFreeOTFE_DiskPartitionsPanel in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_DiskPartitionsPanel.pas',
  OTFEFreeOTFE_fmeSelectPartition in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmeSelectPartition.pas',
  OTFEFreeOTFE_LUKSKeyOrKeyfileEntry in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_LUKSKeyOrKeyfileEntry.pas' {OTFEFreeOTFELUKSKeyOrKeyfileEntry: TFrame},
  OTFEFreeOTFE_VolumeSelect in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_VolumeSelect.pas',
  SDUDiskPartitionsPanel in '..\common\SDeanUtils\SDUDiskPartitionsPanel.pas',
  SDUBlocksPanel in '..\common\SDeanUtils\SDUBlocksPanel.pas',
  SDUObjectManager in '..\common\SDeanUtils\SDUObjectManager.pas',
  OTFEFreeOTFE_fmePKCS11_MgrBase in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmePKCS11_MgrBase.pas' {fmePKCS11_MgrBase: TFrame},
  OTFEFreeOTFE_fmePKCS11_MgrKeyfile in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmePKCS11_MgrKeyfile.pas' {fmePKCS11_MgrKeyfile: TFrame},
  OTFEFreeOTFE_fmePKCS11_MgrSecretKey in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmePKCS11_MgrSecretKey.pas' {fmePKCS11_MgrSecretKey: TFrame},
  OTFEFreeOTFE_frmHashInfo in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmHashInfo.pas' {frmHashInfo},
  MouseRNGCaptureDlg_U in '..\common\SDeanSecurity\MouseRNGDialog\MouseRNGCaptureDlg_U.pas' {MouseRNGCaptureDlg},
  MouseRNGDialog_U in '..\common\SDeanSecurity\MouseRNGDialog\MouseRNGDialog_U.pas',
  OTFEFreeOTFE_frmSelectVolumeAndOffset in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmSelectVolumeAndOffset.pas' {frmSelectVolumeFileAndOffset},
  SDUEndianIntegers in '..\common\SDeanUtils\SDUEndianIntegers.pas',
  DbugIntf in 'C:\Program Files (x86)\GExperts for RAD Studio XE2\DbugIntf.pas',
  OTFE_U in '..\common\OTFE\OTFE\OTFE_U.pas',
  SDUWinHTTP in '..\common\SDeanUtils\SDUWinHTTP.pas',
  SDUWinHttp_API in '..\common\SDeanUtils\SDUWinHttp_API.pas',
  MSCryptoAPI in '..\common\SDeanSecurity\MSCryptoAPI\MSCryptoAPI.pas',
  pkcs11_library in '..\common\SDeanSecurity\PKCS#11\pkcs11_library.pas',
  pkcs11_slot in '..\common\SDeanSecurity\PKCS#11\pkcs11_slot.pas',
  pkcs11t in '..\common\SDeanSecurity\PKCS#11\pkcs11t.pas',
  pkcs11_api in '..\common\SDeanSecurity\PKCS#11\pkcs11_api.pas',
  pkcs11_token in '..\common\SDeanSecurity\PKCS#11\pkcs11_token.pas',
  pkcs11f in '..\common\SDeanSecurity\PKCS#11\pkcs11f.pas',
  pkcs11_session in '..\common\SDeanSecurity\PKCS#11\pkcs11_session.pas',
  pkcs11_slot_event_thread in '..\common\SDeanSecurity\PKCS#11\pkcs11_slot_event_thread.pas',
  pkcs11_mechanism in '..\common\SDeanSecurity\PKCS#11\pkcs11_mechanism.pas',
  pkcs11_attribute in '..\common\SDeanSecurity\PKCS#11\pkcs11_attribute.pas',
  pkcs11_object in '..\common\SDeanSecurity\PKCS#11\pkcs11_object.pas',
  PKCS11KnownLibs in '..\common\SDeanSecurity\PKCS#11\PKCS11KnownLibs.pas',
  PKCS11LibrarySelectDlg in '..\common\SDeanSecurity\PKCS#11\PKCS11LibrarySelectDlg.pas' {PKCS11LibrarySelectDialog};

{$R *.RES}

var
  otherRunningAppWindow: THandle;
  CommandLineOnly: boolean;
  cmdExitCode: eCmdLine_Exit;
  settingsFilename: string;
{$IF CompilerVersion >= 18.5}
  // Delphi 7 doesn't need this, but Delphi 2007 (and 2006 as well? Not
  // checked...) need this to honor any "Run minimised" option set in any
  // launching MS Windows shortcut
  sui: TStartUpInfo;
{$IFEND}

begin
  GLOBAL_VAR_WM_FREEOTFE_RESTORE := RegisterWindowMessage('FREEOTFE_RESTORE');
  GLOBAL_VAR_WM_FREEOTFE_REFRESH := RegisterWindowMessage('FREEOTFE_REFRESH');

{$IFNDEF AlwaysClearFreedMemory}
// #error - this needs to be set to clear down any freed memory
 {$ENDIF}
{$IFDEF DEBUG}
  System.ReportMemoryLeaksOnShutdown := true;
{$ELSE}
  // this doesnt appear to work (built-in version only?),  so SuppressMessageBoxes set as well
  System.ReportMemoryLeaksOnShutdown := false;
  FastMM4.SuppressMessageBoxes := true;
{$ENDIF}

{ see http://sourceforge.net/p/fastmm/code/HEAD/tree/FastMM4Options.inc:
  With this option enabled freed memory will immediately be cleared inside the
  FreeMem routine. This incurs a big performance hit, but may be worthwhile for
  additional peace of mind when working with highly sensitive data. This option
  supersedes the ClearMemoryBeforeReturningToOS option.}

  Application.Initialize;

{$IF CompilerVersion >= 15.0}
  // Vista fix for Delphi 2007 and later
  Application.MainFormOnTaskbar := TRUE;
{$IFEND}
  Application.Title := 'DoxBox';

  FreeOTFESettings.gSettings:= TFreeOTFESettings.Create();
  try
    CommonSettings.CommonSettingsObj := FreeOTFESettings.gSettings;
    if SDUCommandLineParameter(CMDLINE_SETTINGSFILE, settingsFilename) then
      begin
      settingsFilename := SDURelativePathToAbsolute(settingsFilename);
      FreeOTFESettings.gSettings.CustomLocation := settingsFilename;
      end;

    FreeOTFESettings.gSettings.Load();

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
          gSettings.OptAllowMultipleInstances
         ) then
        begin
        // We continue to run the main app
        frmFreeOTFEMain.Visible := TRUE;
        Application.ShowMainForm := TRUE;
        frmFreeOTFEMain.InitApp();


{$IF CompilerVersion >= 18.5}
        // Delphi 7 doesn't need this, but Delphi 2007 (and 2006 as well? Not
        // checked...) need this to honour any "Run minimised" option set in any
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
{$IFEND}
        if SDUCommandLineSwitch(CMDLINE_MINIMIZE) then
          begin
          Application.Minimize();
          end;

        Application.Run;
        end
      else
        begin
        // Poke already running application
{$IF CompilerVersion >= 18.5}
        SendMessage(HWND_BROADCAST, GLOBAL_VAR_WM_FREEOTFE_RESTORE, 0, 0);
{$ELSE}
        SDUPostMessageExistingApp(GLOBAL_VAR_WM_FREEOTFE_RESTORE, 0, 0);
{$IFEND}

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
      FreeOTFESettings.gSettings.Free();
      end;
  end;

  if (
      CommandLineOnly and
      (cmdExitCode <> ceSUCCESS)
     ) then
    begin
    Halt(Integer(cmdExitCode)); // Note: System.ExitCode may be tested in finalization sections
    end;

END.

