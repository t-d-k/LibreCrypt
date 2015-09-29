program LibreCryptExplorer;

{
 layers used are:
 //delphi / 3rd party libs (layer 0)
  //sdu & LibreCrypt utils (ie all not forms - layer 1)
   // LibreCrypt forms (layer 2)
    //main form (layer 3)

     //delphi & libs

  //sdu & LibreCrypt utils

   // LibreCrypt forms

    //main form


  }

uses
  FastMM4,
  Forms,
  frmExplorerMain in 'frmExplorerMain.pas' {frmExplorerMain},
  frmNewDirDlg in 'frmNewDirDlg.pas' {frmNewDirDlg},
  frmProperties in 'frmProperties.pas' {frmPropertiesDialog_Base},
  frmAbout in '..\common\frmAbout.pas' {frmAbout},
  CommonSettings in '..\common\CommonSettings.pas',
  ExplorerSettings in 'ExplorerSettings.pas',
  frmHdrBackupRestore in '..\common\frmHdrBackupRestore.pas' {frmCDBBackupRestore},
  frmInstallOnUSBDrive in '..\common\frmInstallOnUSBDrive.pas' {frmInstallOnUSBDrive},
  frmHashReport in '..\common\frmHashReport.pas' {frmGridReport_Hash},
  frmGridReport in '..\common\frmGridReport.pas' {frmGridReport},
  frmCypherReport in '..\common\frmCypherReport.pas' {frmGridReport_Cypher},
  frmFileProperties in 'frmFileProperties.pas' {frmPropertiesDialog_File},
  frmDirProperties in 'frmDirProperties.pas' {frmPropertiesDialog_Directory},
  frmMultipleProperties in 'frmMultipleProperties.pas' {frmPropertiesDialog_Multiple},
  frmCommonOptions in '..\common\frmCommonOptions.pas' {frmOptions},
  frmExplorerOptions in 'frmExplorerOptions.pas' {frmOptions_FreeOTFEExplorer},
  frmVersionCheck in '..\common\frmVersionCheck.pas' {frmVersionCheck},
  CommonConsts in '..\common\CommonConsts.pas',
  frmSelectDirectory in 'frmSelectDirectory.pas' {frmSelectDirectoryDlg},
  frmVolumeProperties in 'frmVolumeProperties.pas' {frmPropertiesDialog_Volume},
  frmOverwritePrompt in 'frmOverwritePrompt.pas' {frmOverwritePrompt},
  frmSelectCopyOrMove in 'frmSelectCopyOrMove.pas' {frmSelectCopyOrMove},
  frmLUKSHdrDump in '..\common\frmLUKSHdrDump.pas' {frmCDBDump_LUKS},
  frmHdrDump in '..\common\frmHdrDump.pas' {frmCDBDump_Base},
  frmFreeOTFEHdrDump in '..\common\frmFreeOTFEHdrDump.pas' {frmCDBDump_FreeOTFE},
  CheckFilesystem in 'CheckFilesystem.pas',
  SDUForms in '..\common\SDUForms.pas' {SDUForm},
  SDUFrames in '..\common\SDUFrames.pas' {SDUFrame: TFrame},
  frmWizard in '..\common\OTFE\OTFEFreeOTFE\frmWizard.pas' {frmWizard},
  frmWizardChangePasswordCreateKeyfile in '..\common\OTFE\OTFEFreeOTFE\frmWizardChangePasswordCreateKeyfile.pas' {frmWizardChangePasswordCreateKeyfile},
  dlgProgress in '..\common\SDeanUtils\dlgProgress.pas' {SDUProgressDialog},
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
  ExplorerWebDAV in 'ExplorerWebDAV.pas',
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
  PartitionImageDLL in '..\common\OTFE\OTFEFreeOTFE\PartitionImageDLL.pas',
  OTFEConsts_U in '..\common\OTFE\OTFE\OTFEConsts_U.pas',
  Shredder in '..\common\SDeanSecurity\Shredder\Shredder.pas',
  FileList_U in '..\common\SDeanSecurity\Shredder\FileList_U.pas' {frmFileList},
  OTFEFreeOTFE_DriverCypherAPI in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_DriverCypherAPI.pas',
  OTFEFreeOTFE_DriverHashAPI in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_DriverHashAPI.pas',
  OTFEFreeOTFE_LUKSAPI in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_LUKSAPI.pas',
  AFSplitMerge in '..\common\OTFE\AFSplitMerge.pas',
  frmSelectVolumeType in '..\common\OTFE\OTFEFreeOTFE\frmSelectVolumeType.pas' {frmSelectVolumeType},
  frmCreateLUKSVolumeWizard in '..\main\frmCreateLUKSVolumeWizard.pas' {frmCreateLUKSVolumeWizard},
  gnugettext in 'P:\tools\gnugettext.pas';

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
  GfrmExplorerMain.Visible := True;;
  GfrmExplorerMain.InitApp();
  Application.Run;
end.
