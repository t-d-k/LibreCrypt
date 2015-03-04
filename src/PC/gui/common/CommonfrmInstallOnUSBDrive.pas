unit CommonfrmInstallOnUSBDrive;

interface

uses
  Classes, ComCtrls, Controls, Dialogs, Forms,
  Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit, SDUForms, StdCtrls,
  SysUtils, Variants, Windows;

type
  TfrmInstallOnUSBDrive = class (TSDUForm)
    pbOK:                     TButton;
    pbCancel:                 TButton;
    edPath:                   TEdit;
    cbDrive:                  TComboBox;
    Label1:                   TLabel;
    Label2:                   TLabel;
    ckSetupAutoplay:          TCheckBox;
    pbBrowse:                 TButton;
    pbRefreshDrives:          TButton;
    ckHideAutorunInf:         TCheckBox;
    reInstructCopyToUSBDrive: TOTFEFreeOTFE_InstructionRichEdit;
    procedure FormShow(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure edPathChange(Sender: TObject);
    procedure pbRefreshDrivesClick(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
  PRIVATE
    procedure EnableDisableControls();

    function GetInstallDrive(): Char;
    function GetInstallFullPath(): String;
    function GetInstallRelativePath(): String;

    function InstallOnUSBDrive(): Boolean;
    function CreateAutorunInfFile(): Boolean;
  PUBLIC
    procedure PopulateUSBDrives();
  end;


implementation

{$R *.dfm}

uses
{$WARN UNIT_PLATFORM OFF}
  FileCtrl,
{$WARN UNIT_PLATFORM ON}
  SDUDialogs,
  SDUGeneral, SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

procedure TfrmInstallOnUSBDrive.FormShow(Sender: TObject);
begin
  self.Caption := SDUParamSubstitute(_('Copy %1 to USB Drive'), [Application.title]);

  reInstructCopyToUSBDrive.Text :=
    SDUParamSubstitute(_(
    'This function provides an easy means of copying %1 to a USB drive, and configuring it to launch automatically when the USB drive is plugged in.' +
    SDUCRLF + SDUCRLF +
    'Please select the USB drive, and location on it, where you would like %1 to be copied to:'),
    [Application.Title]);

  ckSetupAutoplay.Caption := SDUParamSubstitute(
    _('&Setup autorun.inf to launch %1 when drive inserted'), [Application.Title]);

  // Replace any " " with "_", otherwise autorun.inf won't be able to launch
  // the executable
  edPath.Text             := '\' + StringReplace(Application.Title, ' ', '_', [rfReplaceAll]);
  ckSetupAutoplay.Checked := True;

  PopulateUSBDrives();

  EnableDisableControls();
end;


procedure TfrmInstallOnUSBDrive.pbBrowseClick(Sender: TObject);
var
  newPath:  String;
  rootPath: String;
begin
  rootPath := GetInstallDrive() + ':\';
  if SelectDirectory(SDUParamSubstitute(_('Select location to copy %1 to'),
    [Application.Title]), rootPath, newPath
{$IF CompilerVersion >= 18.5}
    , // Comma from previous line
    [sdNewUI, sdNewFolder]
{$IFEND}
    ) then begin
    // 3 and -2 in order to strip off the "<driveletter>:"
    edPath.Text := Copy(newPath, 3, (length(newPath) - 2));
  end;

end;

procedure TfrmInstallOnUSBDrive.pbOKClick(Sender: TObject);
begin
  if InstallOnUSBDrive() then begin
    SDUMessageDlg(SDUParamSubstitute(_('%1 copy complete.'), [Application.Title]),
      mtInformation);
    ModalResult := mrOk;
  end;

end;

function TfrmInstallOnUSBDrive.InstallOnUSBDrive(): Boolean;
var
  destPath: String;
  srcPath:  String;
  copyOK:   Boolean;
begin
  Result := True;

  destPath := GetInstallFullPath();
  srcPath  := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));

  // Check that if user wants to create an autorun.inf file, they don't have
  // any spaces in teh install path
  if Result then begin
    if ckSetupAutoplay.Checked then begin
      if (Pos(' ', GetInstallRelativePath()) > 0) then begin
        Result := (SDUMessageDlg(SDUParamSubstitute(
          _('The path specified has spaces in it.' + SDUCRLF + SDUCRLF +
          'Because of this, Windows will be able to display the %1 icon for the drive, but not launch %1 automatically when the drive is inserted.' + SDUCRLF + SDUCRLF + 'Do you wish to continue?'), [Application.Title]), mtWarning, [mbYes, mbNo], 0) = mrYes);
      end;
    end;
  end;

  if Result then begin
    // Sanity check - user trying to install into root dir?
    // Note: GetInstallRelativePath() will return '\', at a minimum
    if (length(GetInstallRelativePath()) <= 1) then begin
      Result := (SDUMessageDlg(SDUParamSubstitute(
        _('You have opted to copy %1 to the root directory of your USB drive, and not a subdirectory.'),
        [Application.title]) + SDUCRLF + SDUCRLF + _('Are you sure you wish to do this?'),
        mtWarning, [mbYes, mbNo], 0) = mrYes);
    end;
  end;

  // Copy FreeOTFE software to drive
  if Result then begin
    // Disable the form, so the user mess with it while files are being copied
    SDUEnableControl(self, False);

    // CopyFile(...), but using Windows API to display "flying files" dialog
    // while copying
    // Note: This force-creates the destPath directory
    copyOK := SDUFileCopy(srcPath + '\*', destPath);

    if not (copyOK) then begin
      SDUMessageDlg(
        SDUParamSubstitute(_('Unable to copy %1 to:' + SDUCRLF + SDUCRLF + '%2'),
        [Application.Title, destPath]),
        mtError
        );
      Result := False;
    end;

    // Reenable the form
    SDUEnableControl(self, True);
    // SDUEnableControl(...) resets various display properties on the
    // instructions control; reset them here
    reInstructCopyToUSBDrive.ResetDisplay();
    EnableDisableControls();
  end;

  // Create autorun.inf file, if needed
  if Result then begin
    if ckSetupAutoplay.Checked then begin
      if not (CreateAutorunInfFile()) then begin
        SDUMessageDlg(
          SDUParamSubstitute(_(
          '%1 was successfully copied over, but an autoplay (autorun.inf) file could not be created.'),
          [Application.title]),
          mtWarning
          );
        // We take this as a success - the autorun.inf is pretty minor
        Result := True;
      end;
    end;
  end;

end;

function TfrmInstallOnUSBDrive.CreateAutorunInfFile(): Boolean;
var
  autorunContent:  TStringList;
  partPath:        String;
  srcExeFilename:  String;
  autorunFilename: String;
begin
  Result := False;

  autorunContent := TStringList.Create();
  try
    partPath       := GetInstallRelativePath();
    srcExeFilename := ExtractFileName(ParamStr(0));

    // Strip off any prefixing "\"
    if (length(partPath) > 0) then begin
      if (partPath[1] = '\') then begin
        partPath := Copy(partPath, 2, (length(partPath) - 1));
      end;
    end;

    autorunContent.Add('[autorun]');
    autorunContent.Add('icon=' + partPath + '\' + srcExeFilename);
    autorunContent.Add('open=' + partPath + '\' + srcExeFilename);
    autorunContent.Add('action=' + SDUParamSubstitute(_('Launch %1'), [Application.Title]));
    autorunContent.Add('shell\launch\=' + SDUParamSubstitute(_('Launch %1'), [Application.Title]));
    autorunContent.Add('shell\launch\command=' + partPath + '\' + srcExeFilename);

    autorunFilename := GetInstallDrive() + ':\autorun.inf';

    try
      // Try to delete any existing autorun.inf file
      if FileExists(autorunFilename) then begin
        SysUtils.DeleteFile(autorunFilename);
      end;

      autorunContent.SaveToFile(autorunFilename);

      if ckHideAutorunInf.Checked then begin
        SetFileAttributes(PChar(autorunFilename), FILE_ATTRIBUTE_HIDDEN);
      end;

      Result := True;
    except
      on E: Exception do begin
        // Nothing - just swallow exception
      end;
    end;

  finally
    autorunContent.Free();
  end;

end;

procedure TfrmInstallOnUSBDrive.pbRefreshDrivesClick(Sender: TObject);
begin
  PopulateUSBDrives();
end;

procedure TfrmInstallOnUSBDrive.PopulateUSBDrives();
begin
  SDUPopulateRemovableDrives(cbDrive);

  // Select first drive, if any available
  cbDrive.ItemIndex := -1;
  if (cbDrive.items.Count > 0) then begin
    cbDrive.ItemIndex := 0;
  end;

end;

procedure TfrmInstallOnUSBDrive.edPathChange(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmInstallOnUSBDrive.EnableDisableControls();
begin
  SDUEnableControl(cbDrive, (cbDrive.Items.Count > 1));

  SDUEnableControl(ckHideAutorunInf, ckSetupAutoplay.Checked);

  SDUEnableControl(
    pbOK,
    ((cbDrive.ItemIndex >= 0) and (Pos(':', edPath.Text) = 0)  // No ":" allowed in path
    )
    );

end;

function TfrmInstallOnUSBDrive.GetInstallDrive(): Char;
begin
  Result := #0;

  if (cbDrive.ItemIndex >= 0) then begin
    // Only the 1st char of the drive...
    Result := cbDrive.Items[cbDrive.ItemIndex][1];
  end;


end;

function TfrmInstallOnUSBDrive.GetInstallFullPath(): String;
begin
  Result := GetInstallDrive() + ':' + GetInstallRelativePath();
end;

function TfrmInstallOnUSBDrive.GetInstallRelativePath(): String;
begin
  Result := trim(edPath.Text);
  if (Pos('\', Result) <> 1) then begin
    Result := '\' + Result;
  end;


end;

end.
