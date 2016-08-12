unit frmKeyEntryLUKS;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  //delphi
  Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Messages,
  Spin64,
  StdCtrls, SysUtils, Windows,

  //lc utils
  DriverAPI, OTFEFreeOTFE_PasswordRichEdit, OTFEFreeOTFEBase_U, OTFEFreeOTFE_U,
  PasswordRichEdit, SDUForms,
  SDUFrames,
  SDUSpin64Units, SDUStdCtrls,  // Required for TFreeOTFESectorIVGenMethod and NULL_GUID
  SDUDropFiles, SDUFilenameEdit_U, lcTypes,
  //lc forms
  fmeLUKSKeyOrKeyfileEntry;

type
  TfrmKeyEntryLUKS = class (TSDUForm)
    pbCancel:    TButton;
    pbOK:        TButton;
    GroupBox2:   TGroupBox;
    lblDrive:    TLabel;
    lblMountAs:  TLabel;
    cbDrive:     TComboBox;
    ckMountReadonly: TSDUCheckBox;
    cbMediaType: TComboBox;
    GroupBox4:   TGroupBox;
    Label10:     TLabel;
    Label22:     TLabel;
    GroupBox1:   TGroupBox;
    ckMountForAllUsers: TSDUCheckBox;
    se64UnitSizeLimit: TSDUSpin64Unit_Storage;
    frmeLUKSKeyOrKeyfileEntry: TfrmeLUKSKeyOrKeyfileEntry;
    procedure FormCreate(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure SelectionChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure preUserkeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pbCancelClick(Sender: TObject);
    procedure ckSelected(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
    // These are ordered lists corresponding to the items shown in the combobox
    fHashKernelModeDriverNames: TStringList;
    fHashGUIDs:   TStringList;
    fCypherKernelModeDriverNames: TStringList;
    fCypherGUIDs: TStringList;

    //        fsilent:         Boolean;
    fsilentResult:   TModalResult;
    fsilentPassword: PasswordString;
    // some bug whereby password is reset in silent mode { TODO 1 -otdk -crefactor : investigate }

    procedure PopulateDrives();
    procedure PopulateMountAs();

    procedure DefaultOptions();

    procedure EnableDisableControls();

    procedure DoCancel();

  public

    procedure Initialize();

    // Key...
    function GetKey(var userKey: TSDUBYtes): Boolean;
    procedure SetKey(userKey: PasswordString);
    procedure SetKeyfile(filename: String);
    function GetKeyfileIsASCII(): Boolean;
    procedure SetKeyfileIsASCII(isASCII: Boolean);
    function GetKeyfileNewlineType(var nlType: TSDUNewline): Boolean;
    function SetKeyfileNewlineType(nlType: TSDUNewline): Boolean;

    function GetIVCypherBase(): Boolean;
    procedure SetIVCypherBase(baseIVCypherOnHashLength: Boolean);

    // File options...
    function GetSizeLimit(): Int64;
    procedure SetSizeLimit(fileOptSizeLimit: Int64);

    // Mount options...
    function GetDriveLetter(): DriveLetterChar;
    function SetDriveLetter(mountDriveLetter: DriveLetterChar): Boolean;
    function GetReadonly(): Boolean;
    procedure SetReadonly(mountReadonly: Boolean);
    function GetMountAs(var mountAs: TMountDiskType): Boolean;
    function SetMountAs(mountAs: TMountDiskType): Boolean;
    function GetMountForAllUsers(): Boolean;
    procedure SetMountForAllUsers(allUsers: Boolean);
    procedure SetVolumeFilename(volumeFilename: String);


    //properties
    //        property silent: boolean write fsilent;
  end;


implementation

{$R *.DFM}


uses
  //delphi
  ComObj,                      // Required for StringToGUID
  VolumeFileAPI,               // Required for SCTRIVGEN_USES_SECTOR_ID and SCTRIVGEN_USES_HASH
  INIFiles,
  // sdu, lcutils
  OTFEFreeOTFEDLL_U,
  lcDialogs,
  SDUi18n,
  lcConsts,
  sduGeneral,
  CommonSettings,
  MainSettings, lcCommandLine
  //lc forms
  ;

procedure TfrmKeyEntryLUKS.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if GetCmdLine.isSilent then
    ModalResult := FSilentResult;

end;

procedure TfrmKeyEntryLUKS.FormCreate(Sender: TObject);
begin
  fhashKernelModeDriverNames   := TStringList.Create();
  fhashGUIDs                   := TStringList.Create();
  fcypherKernelModeDriverNames := TStringList.Create();
  fcypherGUIDs                 := TStringList.Create();
  //  fsilent := False;
end;

procedure TfrmKeyEntryLUKS.FormDestroy(Sender: TObject);
begin
  fhashKernelModeDriverNames.Free();
  fhashGUIDs.Free();
  fcypherKernelModeDriverNames.Free();
  fcypherGUIDs.Free();
end;

procedure TfrmKeyEntryLUKS.PopulateDrives();
var
  driveLetters: Ansistring;
  i:            Integer;
begin
  cbDrive.Items.Clear();
  cbDrive.Items.Add(_('Use default'));
  driveLetters := SDUGetUnusedDriveLetters();
  for i := 1 to length(driveLetters) do begin
    // Skip the drive letters traditionally reserved for floppy disk drives
    //    if (
    //        (driveLetters[i] <> 'A') AND
    //        (driveLetters[i] <> 'B')
    //       ) then
    //      begin
    cbDrive.Items.Add(driveLetters[i] + ':');
    //      end;
  end;

end;


procedure TfrmKeyEntryLUKS.PopulateMountAs();
var
  currMountAs: TMountDiskType;
begin
  cbMediaType.Items.Clear();
  for currMountAs := low(TMountDiskType) to high(TMountDiskType) do begin
    if (currMountAs <> fomaUnknown) then begin
      cbMediaType.Items.Add(FreeOTFEMountAsTitle(currMountAs));
    end;

  end;

end;

procedure TfrmKeyEntryLUKS.DefaultOptions();
var
  i:               Integer;
  currDriveLetter: DriveLetterChar;
begin
  frmeLUKSKeyOrKeyfileEntry.DefaultOptions();

  if (GetFreeOTFEBase() is TOTFEFreeOTFE) then begin
    if (cbDrive.Items.Count > 0) then begin
      cbDrive.ItemIndex := 0;

      if (GetSettings().DefaultDriveChar <> #0) then begin
        // Start from 1; skip the default
        for i := 1 to (cbDrive.items.Count - 1) do begin
          currDriveLetter := cbDrive.Items[i][1];
          if (currDriveLetter >= GetSettings().DefaultDriveChar) then begin
            cbDrive.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;
  end;

  if (GetSettings() is TMainSettings) then
    SetMountAs(GetMainSettings().DefaultMountDiskType)
  else
    SetMountAs(fomaRemovableDisk);

  frmeLUKSKeyOrKeyfileEntry.baseIVCypherOnHashLength := True;

  se64UnitSizeLimit.Value := 0;

  // Default to TRUE to allow formatting under Windows Vista
  ckMountForAllUsers.Checked := True;

end;


procedure TfrmKeyEntryLUKS.pbOKClick(Sender: TObject);
var
  tmpKey:        TSDUBYtes;
  msgZeroLenKey: String;
begin
  if GetKey(tmpKey) then begin
    if (Length(tmpKey) = 0) then begin
      if frmeLUKSKeyOrKeyfileEntry.KeyIsUserEntered() then begin
        msgZeroLenKey := _('You have not entered a password.');
      end else begin
        msgZeroLenKey := _('Keyfile contained a zero-length key.');
      end;

      if (SDUMessageDlg(msgZeroLenKey + SDUCRLF + SDUCRLF +
        _('Are you sure you wish to proceed?'), mtConfirmation, [mbYes, mbNo], 0) =
        mrYes) then begin
        ModalResult := mrOk;
      end;
    end else begin
      // No problems with the password as entered; OK to close the dialog
      ModalResult := mrOk;
    end;

  end  // if GetKey(tmpKey) then
  else begin
    if not (frmeLUKSKeyOrKeyfileEntry.KeyIsUserEntered()) then begin
      SDUMessageDlg(_('Unable to read keyfile.'), mtError);
    end;

  end;

end;


procedure TfrmKeyEntryLUKS.EnableDisableControls();
var
  mountAsOK:  Boolean;
  tmpMountAs: TMountDiskType;
begin
  // Ensure we know what to mount as
  mountAsOK               := GetMountAs(tmpMountAs);
  ckMountReadonly.Enabled := False;
  if mountAsOK then begin
    if not (CAN_WRITE_TO_MOUNT_TYPE[tmpMountAs]) then begin
      ckMountReadonly.Checked := True;
    end;
    SDUEnableControl(ckMountReadonly, CAN_WRITE_TO_MOUNT_TYPE[tmpMountAs]);
  end;
  mountAsOK := mountAsOK and (tmpMountAs <> fomaUnknown);

  pbOK.Enabled := SetDriveLetter(GetDriveLetter()) and mountAsOK;

end;

procedure TfrmKeyEntryLUKS.SelectionChange(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmKeyEntryLUKS.FormShow(Sender: TObject);
begin
  // Position cursor to the *end* of any password
  frmeLUKSKeyOrKeyfileEntry.CursorToEndOfPassword();

  // Certain controls only visble if used in conjunction with drive mounting
  lblDrive.Visible           := GetFreeOTFEBase() is TOTFEFreeOTFE;
  cbDrive.Visible            := GetFreeOTFEBase() is TOTFEFreeOTFE;
  lblMountAs.Visible         := GetFreeOTFEBase() is TOTFEFreeOTFE;
  cbMediaType.Visible        := GetFreeOTFEBase() is TOTFEFreeOTFE;
  ckMountForAllUsers.Visible := GetFreeOTFEBase() is TOTFEFreeOTFE;

  // Prevent making remaining control look odd, stuck in the middle
  if not (GetFreeOTFEBase() is TOTFEFreeOTFE) then begin
    ckMountReadonly.top  := lblDrive.top;
    ckMountReadonly.left := lblDrive.left;
  end;

  EnableDisableControls();
  if fsilentPassword <> '' then
    frmeLUKSKeyOrKeyfileEntry.SetKey(fsilentPassword);
  if GetCmdLine.isSilent then begin
    frmeLUKSKeyOrKeyfileEntry.SetKey(fsilentPassword);
    pbOKClick(nil);

    FSilentResult := ModalResult;

    PostMessage(Handle, WM_CLOSE, 0, 0);
  end;

end;



procedure TfrmKeyEntryLUKS.preUserkeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (key = 27) then begin
    DoCancel();
  end;

end;

procedure TfrmKeyEntryLUKS.pbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;

end;


procedure TfrmKeyEntryLUKS.DoCancel();
begin
  ModalResult := mrCancel;

end;


function TfrmKeyEntryLUKS.GetIVCypherBase(): Boolean;
begin
  Result := frmeLUKSKeyOrKeyfileEntry.baseIVCypherOnHashLength;
end;

procedure TfrmKeyEntryLUKS.SetIVCypherBase(baseIVCypherOnHashLength: Boolean);
begin
  frmeLUKSKeyOrKeyfileEntry.baseIVCypherOnHashLength := baseIVCypherOnHashLength;
end;


function TfrmKeyEntryLUKS.GetKey(var userKey: TSDUBYtes): Boolean;
begin
  Result := frmeLUKSKeyOrKeyfileEntry.GetKey(userKey);
end;

procedure TfrmKeyEntryLUKS.SetKey(userKey: PasswordString);
begin
  frmeLUKSKeyOrKeyfileEntry.SetKey(userKey);
  fsilentPassword := userKey;
end;

procedure TfrmKeyEntryLUKS.SetKeyfile(filename: String);
begin
  frmeLUKSKeyOrKeyfileEntry.SetKeyfile(filename);
end;

function TfrmKeyEntryLUKS.GetKeyfileIsASCII(): Boolean;
begin
  Result := frmeLUKSKeyOrKeyfileEntry.GetKeyfileIsASCII();
end;

procedure TfrmKeyEntryLUKS.SetKeyfileIsASCII(isASCII: Boolean);
begin
  frmeLUKSKeyOrKeyfileEntry.SetKeyfileIsASCII(isASCII);
end;

function TfrmKeyEntryLUKS.GetKeyfileNewlineType(var nlType: TSDUNewline): Boolean;
begin
  Result := frmeLUKSKeyOrKeyfileEntry.GetKeyfileNewlineType(nlType);
end;

function TfrmKeyEntryLUKS.SetKeyfileNewlineType(nlType: TSDUNewline): Boolean;
begin
  Result := frmeLUKSKeyOrKeyfileEntry.SetKeyfileNewlineType(nlType);
end;


function TfrmKeyEntryLUKS.GetSizeLimit(): Int64;
begin
  Result := se64UnitSizeLimit.Value;
end;

procedure TfrmKeyEntryLUKS.SetSizeLimit(fileOptSizeLimit: Int64);
begin
  se64UnitSizeLimit.Value := fileOptSizeLimit;
end;

procedure TfrmKeyEntryLUKS.SetVolumeFilename(volumeFilename: String);
begin
  if FileIsReadOnly(volumeFilename) then begin
    ckMountReadonly.Checked := True;
    ckMountReadonly.Enabled := False;
  end;

end;

// Note: This may return #0 as mountDriveLetter to indicate "any"
function TfrmKeyEntryLUKS.GetDriveLetter(): DriveLetterChar;
begin
  Result := #0;
  // Note: The item at index zero is "Use default"; #0 is returned for this
  if (cbDrive.ItemIndex > 0) then begin
    Result := cbDrive.Items[cbDrive.ItemIndex][1];
  end;

end;

// mountDriveLetter - Set to #0 to indicate "Use default"
function TfrmKeyEntryLUKS.SetDriveLetter(mountDriveLetter: DriveLetterChar): Boolean;
var
  idx: Integer;
begin
  Result := True;
  if (mountDriveLetter = #0) then begin
    // The item at idx 0 will *always* be "Use default"
    idx := 0;
  end else begin
    idx := cbDrive.Items.IndexOf(mountDriveLetter + ':');
  end;

  if (idx < 0) then begin
    idx    := 0;
    Result := False;
  end;
  cbDrive.ItemIndex := idx;

end;

function TfrmKeyEntryLUKS.GetReadonly(): Boolean;
begin
  Result := ckMountReadonly.Checked;
end;

procedure TfrmKeyEntryLUKS.SetReadonly(mountReadonly: Boolean);
begin
  ckMountReadonly.Checked := mountReadonly;

end;

function TfrmKeyEntryLUKS.GetMountAs(var mountAs: TMountDiskType): Boolean;
var
  currMountAs: TMountDiskType;
begin
  Result := False;

  for currMountAs := low(TMountDiskType) to high(TMountDiskType) do begin
    if (cbMediaType.Items[cbMediaType.ItemIndex] = FreeOTFEMountAsTitle(currMountAs)) then begin
      mountAs := currMountAs;
      Result  := True;
      break;
    end;
  end;

end;


function TfrmKeyEntryLUKS.SetMountAs(mountAs: TMountDiskType): Boolean;
var
  idx: Integer;
begin
  idx                   := cbMediaType.Items.IndexOf(FreeOTFEMountAsTitle(mountAs));
  cbMediaType.ItemIndex := idx;

  Result := (idx >= 0);

end;


procedure TfrmKeyEntryLUKS.SetMountForAllUsers(allUsers: Boolean);
begin
  ckMountForAllUsers.Checked := allUsers;
end;

function TfrmKeyEntryLUKS.GetMountForAllUsers(): Boolean;
begin
  Result := ckMountForAllUsers.Checked;
end;

procedure TfrmKeyEntryLUKS.ckSelected(Sender: TObject);
begin
  EnableDisableControls();
end;


procedure TfrmKeyEntryLUKS.Initialize();
begin
  frmeLUKSKeyOrKeyfileEntry.Initialize();

  PopulateDrives();
  PopulateMountAs();

  DefaultOptions();
end;

end.
