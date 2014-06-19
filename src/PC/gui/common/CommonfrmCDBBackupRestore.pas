unit CommonfrmCDBBackupRestore;
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
  Spin64, StdCtrls,
  OTFEFreeOTFEBase_U, Buttons, SDUForms, SDUFrames,
  SDUSpin64Units, OTFEFreeOTFE_VolumeSelect;

type
  TCDBOperationType = (opBackup, opRestore);

  TfrmCDBBackupRestore = class(TSDUForm)
    pbCancel: TButton;
    pbOK: TButton;
    gbDest: TGroupBox;
    lblFileDescDest: TLabel;
    lblOffsetDest: TLabel;
    gbSrc: TGroupBox;
    lblFileDescSrc: TLabel;
    lblOffsetSrc: TLabel;
    se64UnitOffsetSrc: TSDUSpin64Unit_Storage;
    se64UnitOffsetDest: TSDUSpin64Unit_Storage;
    SelectSrcFile: TOTFEFreeOTFEVolumeSelect;
    SelectDestFile: TOTFEFreeOTFEVolumeSelect;
    procedure FormCreate(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlChange(Sender: TObject);
  protected
    FDlgType: TCDBOperationType;
    FOTFEFreeOTFE: TOTFEFreeOTFEBase;

    function  GetSrcFilename(): string;
    function  GetSrcOffset(): int64;
    function  GetDestFilename(): string;
    function  GetDestOffset(): int64;

    procedure SetDlgType(dType: TCDBOperationType);

    function  SanityCheckBackup(): boolean;
    function  SanityCheckRestore(): boolean;

    procedure EnableDisableControls();

  public
    OTFEFreeOTFE: TOTFEFreeOTFEBase;

    property DlgType: TCDBOperationType read FDlgType write SetDlgType;

    property SrcFilename: string read GetSrcFilename;
    property SrcOffset: int64 read GetSrcOffset;
    property DestFilename: string read GetDestFilename;
    property DestOffset: int64 read GetDestOffset;

  end;


implementation

{$R *.DFM}

uses
  SDUi18n,
  SDUGeneral,
  SDUDialogs,
  OTFEFreeOTFE_DriverAPI;  // Required for CRITICAL_DATA_LENGTH

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

resourcestring
  USE_NOT_IN_USE = 'Please ensure that the volume is not mounted, or otherwise in use';

procedure TfrmCDBBackupRestore.FormShow(Sender: TObject);
begin
  SelectSrcFile.Filename := '';
  SelectSrcFile.OTFEFreeOTFE := OTFEFreeOTFE;
  SelectDestFile.Filename  := '';
  SelectDestFile.OTFEFreeOTFE := OTFEFreeOTFE;

  se64UnitOffsetSrc.Value  := 0;
  se64UnitOffsetDest.Value := 0;

  if (DlgType = opBackup) then
    begin
    self.caption := _('Backup Volume Critical Data Block');

    gbSrc.Caption          := _('Volume details');
    lblFileDescSrc.caption := _('&Volume:');

    gbDest.Caption          := _('Backup details');
    lblFileDescDest.caption := _('&Backup filename:');

    // Backup file starts from 0 - don't allow the user to change offset
    SDUEnableControl(se64UnitOffsetDest, FALSE);
    SDUEnableControl(lblOffsetDest, FALSE);
    end
  else
    begin
    self.caption := _('Restore Volume Critical Data Block');

    gbSrc.Caption          := _('Backup details');
    lblFileDescSrc.caption := _('&Backup filename:');

    gbDest.Caption          := _('Volume details');
    lblFileDescDest.caption := _('&Volume:');

    // Backup file starts from 0 - don't allow the user to change offset
    SDUEnableControl(se64UnitOffsetSrc, FALSE);
    SDUEnableControl(lblOffsetSrc, FALSE);
    end;


  EnableDisableControls();

end;


procedure TfrmCDBBackupRestore.FormCreate(Sender: TObject);
begin
  SelectSrcFile.SelectFor := fndOpen;
  SelectDestFile.SelectFor := fndSave;

  // Default to backup...
  DlgType := opBackup;

end;


// Sanity check a BACKUP operation
// Returns TRUE if all values entered make sense, otherwise FALSE
function TfrmCDBBackupRestore.SanityCheckBackup(): boolean;
var
  allOK: boolean;
begin
  allOK:= TRUE;

  // BACKUP SANITY CHECKS

  // If we're backing up, the destination filename MUST NOT EXIST.
  // This is done for safety - if we just prompted the user and asked them
  // to continue, the user may just slam "YES!" without checking - and if
  // they've transposed the filenames, this would overwrite the volume file!
  // Note: You can only backup to a *file*
  if FileExists(DestFilename) then
    begin
    SDUMessageDlg(
               _('The file you are attempting to backup to already exists.')+SDUCRLF+
               SDUCRLF+
               _('Please delete this backup file and try again, or specify a different filename to backup to.'),
               mtError, [mbOK], 0);
    allOK := FALSE;
    end;

  // Note: We're not worried if the source doens't exist - the backup operation
  //       will simply fail in this case

  Result := allOK;
end;


// Sanity check a RESTORE operation
// Returns TRUE if all values entered make sense, otherwise FALSE
function TfrmCDBBackupRestore.SanityCheckRestore(): boolean;
var
  allOK: boolean;
  confirm: DWORD;
  srcSize: ULONGLONG;
begin
  allOK:= FALSE;

  // RESTORE SANITY CHECKS

  // Ensure that both the source (backup) and destination (volume) files
  // exist
  // Note: You can only restore from a *file*
  if not(FileExists(SrcFilename)) then
    begin
    SDUMessageDlg(_('The backup file specified does not exist.'), mtError, [mbOK], 0);
    end
  else
    begin
    // Note: We're not worried if the destination doens't exist - the restore
    //       operation will simply fail in this case

    // Check that the source file is the right size for a backup file
    srcSize := SDUGetFileSize(SrcFilename);
    if (srcSize <> (CRITICAL_DATA_LENGTH div 8)) then
      begin
      SDUMessageDlg(
                    _('The source file you specified does not appear to be the right size for a backup file')+SDUCRLF+
                    SDUCRLF+
                    _('Please check your settings and try again.'),
                    mtError
                   );
      end
    else
      begin
      // Get the user to confirm the restoration

      // We don't need this confirmation when BACKING UP - only when RESTORING
      // Backup is relativly safe as it backs up to a file which doesn't exist
      confirm := SDUMessageDlg(
                             SDUParamSubstitute(_(
                                      'Please confirm: Do you wish to restore the critial data block from backup file:'+SDUCRLF+
                                      SDUCRLF+
                                      '%1'+SDUCRLF+
                                      SDUCRLF+
                                      'Into the volume:'+SDUCRLF+
                                      SDUCRLF+
                                      '%2'+SDUCRLF+
                                      SDUCRLF+
                                      'Starting from offset %3 in the volume?'),
                                      [SrcFilename, DestFilename, DestOffset]
                                     ),
                             mtConfirmation,
                             [mbYes,mbNo],
                             0
                            );

      if (confirm = mrYes) then
        begin
        allOK := TRUE;
        end;

      end;  // ELSE PART - if (not(CheckDestLarger())) then

    end;  // ELSE PART - if not(FileExists(GetSrcFilename()) then


  Result := allOK;
end;


procedure TfrmCDBBackupRestore.pbOKClick(Sender: TObject);
var
  allOK: boolean;
begin
  allOK := FALSE;

  if (DlgType = opBackup) then
    begin
    if SanityCheckBackup() then
      begin
      if OTFEFreeOTFE.BackupVolumeCriticalData(
                                       SrcFilename,
                                       SrcOffset,
                                       DestFilename
                                      ) then
        begin
        SDUMessageDlg(_('Backup operation completed successfully.'), mtInformation);
        allOK:= TRUE;
        end
      else
        begin
        SDUMessageDlg(
                   _('Backup operation failed.')+SDUCRLF+
                   SDUCRLF+
                   USE_NOT_IN_USE,
                   mtError
                  );
        end;

      end;

    end  // if (dlgType = opBackup) then
  else
    begin
    if SanityCheckRestore() then
      begin
      if OTFEFreeOTFE.RestoreVolumeCriticalData(
                                   SrcFilename,
                                   DestFilename,
                                   DestOffset
                                  ) then
        begin
        SDUMessageDlg(_('Restore operation completed successfully.'), mtInformation);
        allOK:= TRUE;
        end
      else
        begin
        SDUMessageDlg(
                   _('Restore operation failed.')+SDUCRLF+
                   SDUCRLF+
                   USE_NOT_IN_USE,
                   mtError
                  );
        end;

      end;

    end;  // ELSE PART - if (dlgType = opBackup) then


  if (allOK) then
    begin
    ModalResult := mrOK;
    end;

end;

function TfrmCDBBackupRestore.GetSrcFilename(): string;
begin
  Result := SelectSrcFile.Filename;
end;

function TfrmCDBBackupRestore.GetSrcOffset(): int64;
begin
  Result := se64UnitOffsetSrc.Value;
end;

function TfrmCDBBackupRestore.GetDestFilename(): string;
begin
  Result := SelectDestFile.Filename;
end;

function TfrmCDBBackupRestore.GetDestOffset(): int64;
begin
  Result := se64UnitOffsetDest.Value;
end;


procedure TfrmCDBBackupRestore.EnableDisableControls();
begin
  // Src and dest must be specified, and different
  // Offsets must be 0 or +ve
  pbOK.Enabled :=
                  (SelectSrcFile.Filename <> '')  AND
                  (SelectDestFile.Filename <> '') AND
                  (SelectSrcFile.Filename <> SelectDestFile.Filename) AND  // Silly!
                  (se64UnitOffsetSrc.Value >= 0)  AND
                  (se64UnitOffsetDest.Value >= 0);

end;

procedure TfrmCDBBackupRestore.SetDlgType(dType: TCDBOperationType);
begin
  FDlgType:= dType;
  
  SelectSrcFile.AllowPartitionSelect  := (FDlgType = opBackup);
  SelectDestFile.AllowPartitionSelect := (FDlgType <> opBackup);

  if (FDlgType = opBackup) then
    begin
    SelectSrcFile.FileSelectFilter     := FILE_FILTER_FLT_VOLUMESANDKEYFILES;
    SelectSrcFile.FileSelectDefaultExt := FILE_FILTER_DFLT_VOLUMESANDKEYFILES;
    SelectDestFile.FileSelectFilter     := FILE_FILTER_FLT_CDBBACKUPS;
    SelectDestFile.FileSelectDefaultExt := FILE_FILTER_DFLT_CDBBACKUPS;
    end
  else
    begin
    SelectSrcFile.FileSelectFilter     := FILE_FILTER_FLT_CDBBACKUPS;
    SelectSrcFile.FileSelectDefaultExt := FILE_FILTER_DFLT_CDBBACKUPS;
    SelectDestFile.FileSelectFilter     := FILE_FILTER_FLT_VOLUMESANDKEYFILES;
    SelectDestFile.FileSelectDefaultExt := FILE_FILTER_DFLT_VOLUMESANDKEYFILES;
    end;

end;


procedure TfrmCDBBackupRestore.ControlChange(Sender: TObject);
begin
  EnableDisableControls();

end;

END.


