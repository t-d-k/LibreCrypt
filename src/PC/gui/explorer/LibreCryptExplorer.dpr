program LibreCryptExplorer;

uses
  FastMM4,
  Forms,
  frmExplorerMain in 'frmExplorerMain.pas' {frmExplorerMain},
  FreeOTFEExplorerfrmNewDirDlg in 'FreeOTFEExplorerfrmNewDirDlg.pas' {frmNewDirDlg},
  FreeOTFEExplorerfrmPropertiesDlg_Base in 'FreeOTFEExplorerfrmPropertiesDlg_Base.pas' {frmPropertiesDialog_Base},
  frmAbout in '..\common\frmAbout.pas' {frmAbout},
  CommonSettings in '..\common\CommonSettings.pas',
  frmCommonMain in '..\common\frmCommonMain.pas' {frmMain},
  ExplorerSettings in 'ExplorerSettings.pas',
  CommonfrmCDBBackupRestore in '..\common\CommonfrmCDBBackupRestore.pas' {frmCDBBackupRestore},
  CommonfrmInstallOnUSBDrive in '..\common\CommonfrmInstallOnUSBDrive.pas' {frmInstallOnUSBDrive},
  CommonfrmGridReport_Hash in '..\common\CommonfrmGridReport_Hash.pas' {frmGridReport_Hash},
  CommonfrmGridReport in '..\common\CommonfrmGridReport.pas' {frmGridReport},
  CommonfrmGridReport_Cypher in '..\common\CommonfrmGridReport_Cypher.pas' {frmGridReport_Cypher},
  FreeOTFEExplorerfrmPropertiesDlg_File in 'FreeOTFEExplorerfrmPropertiesDlg_File.pas' {frmPropertiesDialog_File},
  FreeOTFEExplorerfrmPropertiesDlg_Directory in 'FreeOTFEExplorerfrmPropertiesDlg_Directory.pas' {frmPropertiesDialog_Directory},
  FreeOTFEExplorerfrmPropertiesDlg_Multiple in 'FreeOTFEExplorerfrmPropertiesDlg_Multiple.pas' {frmPropertiesDialog_Multiple},
  fmeBaseOptions in '..\common\fmeBaseOptions.pas' {fmeOptions_Base: TFrame},
  frmCommonOptions in '..\common\frmCommonOptions.pas' {frmOptions},
  CommonfmeOptions_PKCS11 in '..\common\CommonfmeOptions_PKCS11.pas' {fmeOptions_PKCS11: TFrame},
  fmeCommonExplorerOptions in 'fmeCommonExplorerOptions.pas' {fmeFreeOTFEExplorerOptions_Base: TFrame},
  fmeExplorerOptions in 'fmeExplorerOptions.pas' {fmeOptions_FreeOTFEExplorerGeneral: TFrame},
  FreeOTFEExplorerfrmOptions in 'FreeOTFEExplorerfrmOptions.pas' {frmOptions_FreeOTFEExplorer},
  frmVersionCheck in '..\common\frmVersionCheck.pas' {frmVersionCheck},
  CommonConsts in '..\common\CommonConsts.pas',
  FreeOTFEExplorerfrmSelectDirectoryDlg in 'FreeOTFEExplorerfrmSelectDirectoryDlg.pas' {frmSelectDirectoryDlg},
  FreeOTFEExplorerfrmPropertiesDlg_Volume in 'FreeOTFEExplorerfrmPropertiesDlg_Volume.pas' {frmPropertiesDialog_Volume},
  fmeAdvancedExplorerOptions in 'fmeAdvancedExplorerOptions.pas' {fmeOptions_FreeOTFEExplorerAdvanced: TFrame},
  frmOverwritePrompt in 'frmOverwritePrompt.pas' {frmOverwritePrompt},
  frmSelectCopyOrMove in 'frmSelectCopyOrMove.pas' {frmSelectCopyOrMove},
  CommonfrmCDBDump_LUKS in '..\common\CommonfrmCDBDump_LUKS.pas' {frmCDBDump_LUKS},
  CommonfrmCDBDump_Base in '..\common\CommonfrmCDBDump_Base.pas' {frmCDBDump_Base},
  CommonfrmCDBDump_FreeOTFE in '..\common\CommonfrmCDBDump_FreeOTFE.pas' {frmCDBDump_FreeOTFE},
  CheckFilesystem in 'CheckFilesystem.pas',
  SDUForms in '..\common\SDUForms.pas' {SDUForm},
  SDUFrames in '..\common\SDUFrames.pas' {SDUFrame: TFrame},
  frmWizard in '..\common\OTFE\OTFEFreeOTFE\frmWizard.pas' {frmWizard},
  frmWizardChangePasswordCreateKeyfile in '..\common\OTFE\OTFEFreeOTFE\frmWizardChangePasswordCreateKeyfile.pas' {frmWizardChangePasswordCreateKeyfile},
  SDUProgressDlg in '..\common\SDeanUtils\SDUProgressDlg.pas' {SDUProgressDialog},
  SDUDiskPropertiesDlg in '..\common\SDeanUtils\SDUDiskPropertiesDlg.pas' {SDUDiskPropertiesDialog},
  SDUFilenameEdit_U in '..\common\SDeanUtils\SDUFilenameEdit_U.pas' {SDUFilenameEdit: TFrame},
  SDUPartitionPropertiesDlg in '..\common\SDeanUtils\SDUPartitionPropertiesDlg.pas' {SDUPartitionPropertiesDialog},
  SDFATBootSectorPropertiesDlg in '..\common\Filesystem\SDFATBootSectorPropertiesDlg.pas' {SDFATBootSectorPropertiesDialog},
  SDFilesystemCtrls_ColDetails in '..\common\Filesystem\SDFilesystemCtrls_ColDetails.pas' {SDFilesystemListView_ColDetails},
  SDUAboutDlg in '..\common\SDeanUtils\SDUAboutDlg.pas' {SDUAboutDialog},
  SDUMRUList in '..\common\SDeanUtils\SDUMRUList.pas',
  SDURegistry in '..\common\SDeanUtils\SDURegistry.pas',
  SDUGeneral in '..\common\SDeanUtils\SDUGeneral.pas',
  SDFilesystem_FAT in '..\common\Filesystem\SDFilesystem_FAT.pas',
  SDUWebDav in '..\common\SDeanUtils\SDUWebDav.pas',
  SDUHTTPServer in '..\common\SDeanUtils\SDUHTTPServer.pas',
  frmKeyEntryFreeOTFE in '..\common\OTFE\OTFEFreeOTFE\frmKeyEntryFreeOTFE.pas' {frmKeyEntryFreeOTFE},
  frmKeyEntryLUKS in '..\common\OTFE\OTFEFreeOTFE\frmKeyEntryLUKS.pas' {frmKeyEntryLUKS},
  frmKeyEntryLinux in '..\common\OTFE\OTFEFreeOTFE\frmKeyEntryLinux.pas' {frmKeyEntryPlainLinux},
  frmNewVolumeSize in '..\common\OTFE\OTFEFreeOTFE\frmNewVolumeSize.pas' {frmNewVolumeSize},
  OTFEFreeOTFE_U in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_U.pas',
  OTFEFreeOTFEBase_U in '..\common\OTFE\OTFEFreeOTFEBase_U.pas',
  DriverControl in '..\common\OTFE\OTFEFreeOTFE\DriverControl.pas',
  MainSettings in '..\main\MainSettings.pas',
  fmeSDUBlocks in '..\common\SDeanUtils\fmeSDUBlocks.pas' {SDUBlocksPanel: TFrame},
  fmeSDUDiskPartitions in '..\common\SDeanUtils\fmeSDUDiskPartitions.pas' {fmeSDUDiskPartitions: TFrame},
  PKCS11Lib in '..\common\OTFE\OTFEFreeOTFE\PKCS11Lib.pas',
  fmeVolumeSelect in '..\common\OTFE\OTFEFreeOTFE\fmeVolumeSelect.pas' {OTFEFreeOTFEVolumeSelect: TFrame},
  SDURandPool in '..\common\OTFE\SDURandPool.pas',
  SDUWinHTTP in '..\common\SDeanUtils\SDUWinHTTP.pas',
  SDUWinHttp_API in '..\common\SDeanUtils\SDUWinHttp_API.pas',
  SDUi18n in '..\common\SDeanUtils\SDUi18n.pas',
  SDUGraphics in '..\common\SDeanUtils\SDUGraphics.pas',
  SDUWindows64 in '..\common\SDeanUtils\SDUWindows64.pas',
  SDUWindows in '..\common\SDeanUtils\SDUWindows.pas',
  SDUClasses in '..\common\SDeanUtils\SDUClasses.pas',
  SDUDirIterator_U in '..\common\SDeanUtils\SDUDirIterator_U.pas',
  FreeOTFEExplorerfmeOptions_WebDAV in 'FreeOTFEExplorerfmeOptions_WebDAV.pas' {fmeOptions_FreeOTFEExplorerWebDAV: TFrame},
  FreeOTFEExplorerWebDAV in 'FreeOTFEExplorerWebDAV.pas',
  frmHashInfo in '..\common\OTFE\OTFEFreeOTFE\frmHashInfo.pas' {frmHashInfo},
  frmCypherInfo in '..\common\OTFE\OTFEFreeOTFE\frmCypherInfo.pas' {frmCypherInfo},
  frmDriverControl in '..\common\OTFE\OTFEFreeOTFE\frmDriverControl.pas' {frmDriverControl},
  fmeSelectPartition in '..\common\OTFE\OTFEFreeOTFE\fmeSelectPartition.pas' {fmeSelectPartition: TFrame},
  frmSelectPartition in '..\common\OTFE\OTFEFreeOTFE\frmSelectPartition.pas' {frmSelectPartition},
  frmUserSelectPartitionSimple in '..\common\OTFE\OTFEFreeOTFE\frmUserSelectPartitionSimple.pas' {frmUserSelectPartitionSimple},
  frmPKCS11Management in '..\common\OTFE\OTFEFreeOTFE\frmPKCS11Management.pas' {frmPKCS11Management},
  frmPKCS11NewSecretKey in '..\common\OTFE\OTFEFreeOTFE\frmPKCS11NewSecretKey.pas' {frmPKCS11NewSecretKey},
  frmPKCS11Session in '..\common\OTFE\OTFEFreeOTFE\frmPKCS11Session.pas' {frmPKCS11Session},
  fmePKCS11_MgrBase in '..\common\OTFE\OTFEFreeOTFE\fmePKCS11_MgrBase.pas' {fmePKCS11_MgrBase: TFrame},
  fmePKCS11_MgrKeyfile in '..\common\OTFE\OTFEFreeOTFE\fmePKCS11_MgrKeyfile.pas' {fmePKCS11_MgrKeyfile: TFrame},
  fmePKCS11_MgrSecretKey in '..\common\OTFE\OTFEFreeOTFE\fmePKCS11_MgrSecretKey.pas' {fmePKCS11_MgrSecretKey: TFrame},
  frmSelectVolumeType in '..\common\OTFE\OTFEFreeOTFE\frmSelectVolumeType.pas' {frmSelectVolumeType},
  frmSelectHashCypher in '..\common\OTFE\OTFEFreeOTFE\frmSelectHashCypher.pas' {frmSelectHashCypher},
  frmSelectVolumeAndOffset in '..\common\OTFE\OTFEFreeOTFE\frmSelectVolumeAndOffset.pas',
  MouseRNGCaptureDlg_U in '..\common\SDeanSecurity\MouseRNGDialog\MouseRNGCaptureDlg_U.pas' {MouseRNGCaptureDlg},
  MouseRNGDialog_U in '..\common\SDeanSecurity\MouseRNGDialog\MouseRNGDialog_U.pas',
  VolumeFileAPI in '..\common\OTFE\OTFEFreeOTFE\VolumeFileAPI.pas',
  OTFE_U in '..\common\OTFE\OTFE\OTFE_U.pas',
  OTFEFreeOTFEDLL_U in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFEDLL_U.pas',
  frmWizardCreateVolume in '..\common\OTFE\OTFEFreeOTFE\frmWizardCreateVolume.pas' {frmWizardCreateVolume},
  pkcs11_api in '..\common\SDeanSecurity\PKCS#11\pkcs11_api.pas',
  pkcs11_attribute in '..\common\SDeanSecurity\PKCS#11\pkcs11_attribute.pas',
  pkcs11_library in '..\common\SDeanSecurity\PKCS#11\pkcs11_library.pas',
  pkcs11_mechanism in '..\common\SDeanSecurity\PKCS#11\pkcs11_mechanism.pas',
  pkcs11_object in '..\common\SDeanSecurity\PKCS#11\pkcs11_object.pas',
  pkcs11_session in '..\common\SDeanSecurity\PKCS#11\pkcs11_session.pas',
  pkcs11_slot in '..\common\SDeanSecurity\PKCS#11\pkcs11_slot.pas',
  pkcs11_slot_event_thread in '..\common\SDeanSecurity\PKCS#11\pkcs11_slot_event_thread.pas',
  pkcs11_token in '..\common\SDeanSecurity\PKCS#11\pkcs11_token.pas',
  pkcs11f in '..\common\SDeanSecurity\PKCS#11\pkcs11f.pas',
  PKCS11KnownLibs in '..\common\SDeanSecurity\PKCS#11\PKCS11KnownLibs.pas',
  PKCS11LibrarySelectDlg in '..\common\SDeanSecurity\PKCS#11\PKCS11LibrarySelectDlg.pas' {PKCS11LibrarySelectDialog},
  pkcs11t in '..\common\SDeanSecurity\PKCS#11\pkcs11t.pas',
  MSCryptoAPI in '..\common\SDeanSecurity\MSCryptoAPI\MSCryptoAPI.pas',
  DriverAPI in '..\common\OTFE\OTFEFreeOTFE\DriverAPI.pas',
  SDUEndianIntegers in '..\common\SDeanUtils\SDUEndianIntegers.pas',
  cryptlib in '..\common\OTFE\OTFEFreeOTFE\cryptlib.pas',
  FreeOTFEDLLMainAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLMainAPI.pas',
  FreeOTFEDLLHashAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLHashAPI.pas',
  FreeOTFEDLLCypherAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLCypherAPI.pas',
  SDFilesystem in '..\common\Filesystem\SDFilesystem.pas',
  SDPartitionImage in '..\common\Filesystem\SDPartitionImage.pas',
  SDPartitionImage_File in '..\common\Filesystem\SDPartitionImage_File.pas',
  fmeLUKSKeyOrKeyfileEntry in '..\common\OTFE\OTFEFreeOTFE\fmeLUKSKeyOrKeyfileEntry.pas' {frmeLUKSKeyOrKeyfileEntry: TFrame},
  lcConsts in '..\common\lcConsts.pas',
  fmeDiskPartitionsPanel in '..\common\OTFE\OTFEFreeOTFE\fmeDiskPartitionsPanel.pas',
  PartitionImageDLL in '..\common\OTFE\OTFEFreeOTFE\PartitionImageDLL.pas',
  fmeAutorunOptions in '..\common\fmeAutorunOptions.pas' {fmeAutorunOptions: TFrame};

{$R *.res}
{$R FreeOTFEExplorerCursors.res}

begin
{$IFNDEF AlwaysClearFreedMemory}
  // #error - this needs to be set to clear down any freed memory
 {$ENDIF}
{$IFDEF DEBUG}
  System.ReportMemoryLeaksOnShutdown := true;
{$ELSE}
  // this doesnt appear to work with fastMM4 (built-in version only?),  so SuppressMessageBoxes set as well
  System.ReportMemoryLeaksOnShutdown := False;
  FastMM4.SuppressMessageBoxes       := True;
{$ENDIF}

  Application.Initialize;
{$IFDEF VER185}
  // Vista fix for Delphi 2007 and later
  Application.MainFormOnTaskbar := TRUE;
{$ENDIF}
  Application.Title := 'LibreCrypt Explorer';
  Application.CreateForm(TfrmExplorerMain, GfrmExplorerMain);
  GfrmExplorerMain.InitApp();
  Application.Run;
end.
