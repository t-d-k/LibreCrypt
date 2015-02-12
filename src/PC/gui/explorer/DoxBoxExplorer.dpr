program DoxBoxExplorer;

uses
  FastMM4,
  Forms,
  FreeOTFEExplorerfrmMain in 'FreeOTFEExplorerfrmMain.pas' {frmFreeOTFEExplorerMain},
  FreeOTFEExplorerfrmNewDirDlg in 'FreeOTFEExplorerfrmNewDirDlg.pas' {frmNewDirDlg},
  FreeOTFEExplorerfrmPropertiesDlg_Base in 'FreeOTFEExplorerfrmPropertiesDlg_Base.pas' {frmPropertiesDialog_Base},
  FreeOTFEExplorerConsts in 'FreeOTFEExplorerConsts.pas',
  CommonfrmAbout in '..\common\CommonfrmAbout.pas' {frmAbout},
  CommonSettings in '..\common\CommonSettings.pas',
  CommonfrmMain in '..\common\CommonfrmMain.pas' {frmMain},
  FreeOTFEExplorerSettings in 'FreeOTFEExplorerSettings.pas',
  CommonfrmCDBBackupRestore in '..\common\CommonfrmCDBBackupRestore.pas' {frmCDBBackupRestore},
  CommonfrmInstallOnUSBDrive in '..\common\CommonfrmInstallOnUSBDrive.pas' {frmInstallOnUSBDrive},
  CommonfrmGridReport_Hash in '..\common\CommonfrmGridReport_Hash.pas' {frmGridReport_Hash},
  CommonfrmGridReport in '..\common\CommonfrmGridReport.pas' {frmGridReport},
  CommonfrmGridReport_Cypher in '..\common\CommonfrmGridReport_Cypher.pas' {frmGridReport_Cypher},
  FreeOTFEExplorerfrmPropertiesDlg_File in 'FreeOTFEExplorerfrmPropertiesDlg_File.pas' {frmPropertiesDialog_File},
  FreeOTFEExplorerfrmPropertiesDlg_Directory in 'FreeOTFEExplorerfrmPropertiesDlg_Directory.pas' {frmPropertiesDialog_Directory},
  FreeOTFEExplorerfrmPropertiesDlg_Multiple in 'FreeOTFEExplorerfrmPropertiesDlg_Multiple.pas' {frmPropertiesDialog_Multiple},
  CommonfmeOptions_Base in '..\common\CommonfmeOptions_Base.pas' {fmeOptions_Base: TFrame},
  CommonfrmOptions in '..\common\CommonfrmOptions.pas' {frmOptions},
  CommonfmeOptions_PKCS11 in '..\common\CommonfmeOptions_PKCS11.pas' {fmeOptions_PKCS11: TFrame},
  FreeOTFEExplorerfmeOptions_Base in 'FreeOTFEExplorerfmeOptions_Base.pas' {fmeFreeOTFEExplorerOptions_Base: TFrame},
  FreeOTFEExplorerfmeOptions_General in 'FreeOTFEExplorerfmeOptions_General.pas' {fmeOptions_FreeOTFEExplorerGeneral: TFrame},
  FreeOTFEExplorerfrmOptions in 'FreeOTFEExplorerfrmOptions.pas' {frmOptions_FreeOTFEExplorer},
  CommonfrmVersionCheck in '..\common\CommonfrmVersionCheck.pas' {frmVersionCheck},
  CommonConsts in '..\common\CommonConsts.pas',
  FreeOTFEExplorerfrmSelectDirectoryDlg in 'FreeOTFEExplorerfrmSelectDirectoryDlg.pas' {frmSelectDirectoryDlg},
  FreeOTFEExplorerfrmPropertiesDlg_Volume in 'FreeOTFEExplorerfrmPropertiesDlg_Volume.pas' {frmPropertiesDialog_Volume},
  FreeOTFEExplorerfmeOptions_Advanced in 'FreeOTFEExplorerfmeOptions_Advanced.pas' {fmeOptions_FreeOTFEExplorerAdvanced: TFrame},
  FreeOTFEExplorerfrmOverwritePrompt in 'FreeOTFEExplorerfrmOverwritePrompt.pas' {frmOverwritePrompt},
  FreeOTFEExplorerfrmSelectCopyOrMove in 'FreeOTFEExplorerfrmSelectCopyOrMove.pas' {frmSelectCopyOrMove},
  CommonfrmCDBDump_LUKS in '..\common\CommonfrmCDBDump_LUKS.pas' {frmCDBDump_LUKS},
  CommonfrmCDBDump_Base in '..\common\CommonfrmCDBDump_Base.pas' {frmCDBDump_Base},
  CommonfrmCDBDump_FreeOTFE in '..\common\CommonfrmCDBDump_FreeOTFE.pas' {frmCDBDump_FreeOTFE},
  FreeOTFEExplorerCheckFilesystem in 'FreeOTFEExplorerCheckFilesystem.pas',
  SDUForms in '..\common\SDUForms.pas' {SDUForm},
  SDUFrames in '..\common\SDUFrames.pas' {SDUFrame: TFrame},
  OTFEFreeOTFE_frmWizard in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizard.pas' {frmWizard},
  OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile.pas' {frmWizardChangePasswordCreateKeyfile},
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
  OTFEFreeOTFE_frmKeyEntryFreeOTFE in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmKeyEntryFreeOTFE.pas' {frmKeyEntryFreeOTFE},
  OTFEFreeOTFE_frmKeyEntryLUKS in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmKeyEntryLUKS.pas' {frmKeyEntryLUKS},
  OTFEFreeOTFE_frmKeyEntryLinux in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmKeyEntryLinux.pas' {frmKeyEntryLinux},
  OTFEFreeOTFE_frmNewVolumeSize in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmNewVolumeSize.pas' {frmNewVolumeSize},
  OTFEFreeOTFE_U in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_U.pas',
  OTFEFreeOTFEBase_U in '..\common\OTFE\OTFEFreeOTFEBase_U.pas',
  OTFEFreeOTFE_LUKSKeyOrKeyfileEntry in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_LUKSKeyOrKeyfileEntry.pas' {OTFEFreeOTFELUKSKeyOrKeyfileEntry: TFrame},
  OTFEFreeOTFE_DriverControl in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_DriverControl.pas',
  OTFEFreeOTFE_DiskPartitionsPanel in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_DiskPartitionsPanel.pas' {OTFEFreeOTFEDiskPartitionsPanel: TFrame},
  FreeOTFESettings in '..\main\FreeOTFESettings.pas',
  SDUBlocksPanel in '..\common\SDeanUtils\SDUBlocksPanel.pas' {SDUBlocksPanel: TFrame},
  SDUDiskPartitionsPanel in '..\common\SDeanUtils\SDUDiskPartitionsPanel.pas' {SDUDiskPartitionsPanel: TFrame},
  OTFEFreeOTFE_PKCS11 in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_PKCS11.pas',
  OTFEFreeOTFE_VolumeSelect in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_VolumeSelect.pas' {OTFEFreeOTFEVolumeSelect: TFrame},
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
  OTFEFreeOTFE_frmHashInfo in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmHashInfo.pas' {frmHashInfo},
  OTFEFreeOTFE_frmCypherInfo in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmCypherInfo.pas' {frmCypherInfo},
  OTFEFreeOTFE_frmDriverControl in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmDriverControl.pas' {frmDriverControl},
  OTFEFreeOTFE_fmeSelectPartition in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmeSelectPartition.pas' {fmeSelectPartition: TFrame},
  OTFEFreeOTFE_frmSelectPartition in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmSelectPartition.pas' {frmSelectPartition},
  OTFEFreeOTFE_frmSelectPartition_Simple in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmSelectPartition_Simple.pas' {frmSelectPartition_Simple},
  OTFEFreeOTFE_frmPKCS11Management in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmPKCS11Management.pas' {frmPKCS11Management},
  OTFEFreeOTFE_frmPKCS11NewSecretKey in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmPKCS11NewSecretKey.pas' {frmPKCS11NewSecretKey},
  OTFEFreeOTFE_frmPKCS11Session in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmPKCS11Session.pas' {frmPKCS11Session},
  OTFEFreeOTFE_fmePKCS11_MgrBase in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmePKCS11_MgrBase.pas' {fmePKCS11_MgrBase: TFrame},
  OTFEFreeOTFE_fmePKCS11_MgrKeyfile in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmePKCS11_MgrKeyfile.pas' {fmePKCS11_MgrKeyfile: TFrame},
  OTFEFreeOTFE_fmePKCS11_MgrSecretKey in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmePKCS11_MgrSecretKey.pas' {fmePKCS11_MgrSecretKey: TFrame},
  OTFEFreeOTFE_frmVolumeType in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmVolumeType.pas' {frmSelectVolumeType},
  OTFEFreeOTFE_frmSelectHashCypher in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmSelectHashCypher.pas' {frmSelectHashCypher},
  OTFEFreeOTFE_frmSelectVolumeAndOffset in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmSelectVolumeAndOffset.pas',
  OTFEFreeOTFEDLL_PartitionImage in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFEDLL_PartitionImage.pas',
  MouseRNGCaptureDlg_U in '..\common\SDeanSecurity\MouseRNGDialog\MouseRNGCaptureDlg_U.pas' {MouseRNGCaptureDlg},
  MouseRNGDialog_U in '..\common\SDeanSecurity\MouseRNGDialog\MouseRNGDialog_U.pas',
  OTFEFreeOTFE_VolumeFileAPI in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_VolumeFileAPI.pas',
  OTFE_U in '..\common\OTFE\OTFE\OTFE_U.pas',
  OTFEFreeOTFEDLL_U in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFEDLL_U.pas',
  OTFEFreeOTFE_frmWizardCreateVolume in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizardCreateVolume.pas' {frmWizardCreateVolume},
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
  OTFEFreeOTFE_DriverAPI in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_DriverAPI.pas',
  SDUEndianIntegers in '..\common\SDeanUtils\SDUEndianIntegers.pas',
  OTFEFreeOTFE_cryptlib in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_cryptlib.pas',
  FreeOTFEDLLMainAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLMainAPI.pas',
  FreeOTFEDLLHashAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLHashAPI.pas',
  FreeOTFEDLLCypherAPI in '..\common\OTFE\OTFEFreeOTFE\FreeOTFEDLLCypherAPI.pas',
  SDFilesystem in '..\common\Filesystem\SDFilesystem.pas',
  SDPartitionImage in '..\common\Filesystem\SDPartitionImage.pas',
  SDPartitionImage_File in '..\common\Filesystem\SDPartitionImage_File.pas';

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
  System.ReportMemoryLeaksOnShutdown := false;
  FastMM4.SuppressMessageBoxes := true;
{$ENDIF}

  Application.Initialize;
{$IFDEF VER185}
  // Vista fix for Delphi 2007 and later
  Application.MainFormOnTaskbar := TRUE;
{$ENDIF}
  Application.Title := 'DoxBox Explorer';
  Application.CreateForm(TfrmFreeOTFEExplorerMain, frmFreeOTFEExplorerMain);
  frmFreeOTFEExplorerMain.InitApp();
  Application.Run;
end.
