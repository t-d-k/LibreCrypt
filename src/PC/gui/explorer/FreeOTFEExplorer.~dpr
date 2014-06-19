program FreeOTFEExplorer;

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
  FreeOTFEExplorerCheckFilesystem in 'FreeOTFEExplorerCheckFilesystem.pas';

{$R *.res}
{$R FreeOTFEExplorerCursors.res}

begin
  Application.Initialize;
{$IFDEF VER185}
  // Vista fix for Delphi 2007 and later
  Application.MainFormOnTaskbar := TRUE;
{$ENDIF}
  Application.Title := 'FreeOTFE Explorer';
  Application.CreateForm(TfrmFreeOTFEExplorerMain, frmFreeOTFEExplorerMain);
  frmFreeOTFEExplorerMain.InitApp();
  Application.Run;
end.
