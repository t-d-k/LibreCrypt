program DoxBoxExplorer;

uses
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
  OTFEFreeOTFE_fmeSelectPartition in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_fmeSelectPartition.pas' {fmeSelectPartition: TFrame},
  OTFEFreeOTFE_frmWizard in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizard.pas' {frmWizard},
  OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizardChangePasswordCreateKeyfile.pas' {frmWizardChangePasswordCreateKeyfile},
  OTFEFreeOTFE_frmWizardCreateVolume in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizardCreateVolume.pas' {frmWizardCreateVolume},
  OTFEFreeOTFE_frmWizardCreateVolumeAdvanced in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmWizardCreateVolumeAdvanced.pas' {frmWizardCreateVolumeAdvanced},
  OTFEFreeOTFE_WizardCommon in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_WizardCommon.pas',
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
  OTFEFreeOTFE_frmNewVolumeSize in '..\common\OTFE\OTFEFreeOTFE\OTFEFreeOTFE_frmNewVolumeSize.pas' {frmNewVolumeSize};

{$R *.res}
{$R FreeOTFEExplorerCursors.res}

begin
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
