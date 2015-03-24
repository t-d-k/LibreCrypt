unit OTFEFreeOTFE_frmKeyEntryFreeOTFE;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //


 // Panels layout on this form:
 //
 //   +--------------------------------------------------+
 //   |                                                  |
 //   | +----------------------------------------------+ |
 //   | | pnlBasic (alTop)                             | |
 //   | |                                              | |
 //   | |                                              | |
 //   | +----------------------------------------------+ |
 //   |                                                  |
 //   | +----------------------------------------------+ |
 //   | | pnlLower (alClient)                          | |
 //   | | +------------------------------------------+ | |
 //   | | | pnlAdvanced (alTop)                      | | |
 //   | | |                                          | | |
 //   | | |                                          | | |
 //   | | +------------------------------------------+ | |
 //   | |                                              | |
 //   | | +------------------------------------------+ | |
 //   | | | pnlButtons (alClient)                    | | |
 //   | | |                                          | | |
 //   | | +------------------------------------------+ | |
 //   | |                                              | |
 //   | +----------------------------------------------+ |
 //   |                                                  |
 //   +--------------------------------------------------+


interface

uses
  Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls,
  Forms, Graphics, Messages, OTFEFreeOTFE_PasswordRichEdit, OTFEFreeOTFE_PKCS11,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  PasswordRichEdit, pkcs11_library, pkcs11_session,
  SDUDropFiles, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUGeneral,
  SDUSpin64Units, Spin64,
  StdCtrls, SysUtils, Windows;

type
  TfrmKeyEntryFreeOTFE = class (TSDUForm)
    pnlBasic:             TPanel;
    GroupBox1:            TGroupBox;
    Label1:               TLabel;
    Label6:               TLabel;
    lblDrive:             TLabel;
    preUserKey:           TOTFEFreeOTFE_PasswordRichEdit;
    cbDrive:              TComboBox;
    ckMountReadonly:      TCheckBox;
    pnlLower:             TPanel;
    pnlButtons:           TPanel;
    pbCancel:             TButton;
    pbOK:                 TButton;
    pbAdvanced:           TButton;
    pnlAdvanced:          TPanel;
    gbVolumeOptions:      TGroupBox;
    Label8:               TLabel;
    ckOffsetPointsToCDB:  TCheckBox;
    gbMountAs:            TGroupBox;
    Label9:               TLabel;
    cbMediaType:          TComboBox;
    ckMountForAllUsers:   TCheckBox;
    GroupBox3:            TGroupBox;
    Label2:               TLabel;
    Label5:               TLabel;
    Label7:               TLabel;
    seSaltLength:         TSpinEdit64;
    seKeyIterations:      TSpinEdit64;
    cbPKCS11CDB:          TComboBox;
    rbKeyfileFile:        TRadioButton;
    rbKeyfilePKCS11:      TRadioButton;
    cbPKCS11SecretKey:    TComboBox;
    Label10:              TLabel;
    se64UnitOffset:       TSDUSpin64Unit_Storage;
    feKeyfile:            TSDUFilenameEdit;
    SDUDropFiles_Keyfile: TSDUDropFiles;
    procedure pbOKClick(Sender: TObject);
    procedure preUserkeyKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pbCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure feKeyfileChange(Sender: TObject);
    procedure seSaltLengthChange(Sender: TObject);
    procedure seKeyIterationsChange(Sender: TObject);
    procedure cbMediaTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbAdvancedClick(Sender: TObject);
    procedure rbKeyfileFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbPKCS11CDBChange(Sender: TObject);
    procedure rbKeyfilePKCS11Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SDUDropFiles_KeyfileFileDrop(Sender: TObject; DropItem: String;
      DropPoint: TPoint);
  PROTECTED
    fTokenCDB:       TPKCS11CDBPtrArray;
    fTokenSecretKey: TPKCS11SecretKeyPtrArray;
    fPkcs11session:  TPKCS11Session;

    fsilentResult: TModalResult;

    fsilent:        Boolean;
    fVolumeFiles:   TStringList;
    fmountedDrives: string;

    procedure PopulateDrives();
    procedure PopulateMountAs();
    procedure PopulatePKCS11CDB();
    procedure PopulatePKCS11SecretKey();

    procedure DoCancel();

    procedure EnableDisableControls();
    procedure EnableDisableControls_Keyfile();
    procedure EnableDisableControls_SecretKey();

    function GetDriveLetter(): char;
    function GetMountAs(): TFreeOTFEMountAs;
    function SetMountAs(mountAs: TFreeOTFEMountAs): Boolean;

    function CheckInput(): Boolean;
    function AttemptMount(): Boolean;

    function GetPKCS11Session(): TPKCS11Session;

    function IsVolumeStoredOnReadonlyMedia(): Boolean;
    function IsVolumeMarkedAsReadonly(): Boolean;

    function INSMessageDlg(Content: String; DlgType: TMsgDlgType): Integer;
    procedure SetVolumeFilesText(Value :string);
  PUBLIC

    procedure SetPassword(password: TSDUBytes);
    procedure SetReadOnly(ReadOnly: Boolean);
    procedure SetKeyfile(keyFilename: String);
    procedure SetOffset(offset: ULONGLONG);
    procedure SetSaltLength(saltLength: Integer);
    procedure SetKeyIterations(keyIterations: Integer);
    procedure SetCDBAtOffset(CDBAtOffset: Boolean);

    procedure DisplayAdvanced(displayAdvanced: Boolean);

    property Silent :Boolean read fSilent write fSilent ;
    property VolumeFilesText :string write SetVolumeFilesText;
    property MountedDrives :string  read fMountedDrives;
  end;


implementation

{$R *.DFM}


uses
  ComObj,  // Required for StringToGUID
           // Disable useless warnings about faReadOnly, etc and FileSetAttr(...) being
           // platform-specific
           // This is ineffective?!
{$WARN SYMBOL_PLATFORM OFF}
  FileCtrl,  // Required for TDriveType
{$WARN SYMBOL_PLATFORM ON}
  OTFEConsts_U,
  OTFEFreeOTFE_DriverAPI,
  OTFEFreeOTFE_frmPKCS11Session,
  pkcs11_object, SDUDialogs,
  SDUi18n,
  Shredder;

resourcestring
  RS_NOT_SELECTED   = '<none selected>';
  RS_NONE_AVAILABLE = '<none available>';

  RS_BUTTON_ADVANCED = '&Advanced';


function TfrmKeyEntryFreeOTFE.GetDriveLetter(): char;
begin
  Result := #0;
  // Note: The item at index zero is "Use default"; #0 is returned for this
  if (cbDrive.ItemIndex > 0) then begin
    Result := cbDrive.Items[cbDrive.ItemIndex][1];
  end;

end;


procedure TfrmKeyEntryFreeOTFE.SetReadOnly(ReadOnly: Boolean);
begin
  ckMountReadonly.Checked := ReadOnly;
end;

procedure TfrmKeyEntryFreeOTFE.SetKeyfile(keyFilename: String);
begin
  rbKeyfileFile.Checked := True;
  feKeyfile.Filename    := keyFilename;
end;

procedure TfrmKeyEntryFreeOTFE.SetOffset(offset: ULONGLONG);
begin
  se64UnitOffset.Value := offset;
end;

procedure TfrmKeyEntryFreeOTFE.SetSaltLength(saltLength: Integer);
begin
  seSaltLength.Value := saltLength;
end;

procedure TfrmKeyEntryFreeOTFE.SetKeyIterations(keyIterations: Integer);
begin
  seKeyIterations.Value := keyIterations;
end;

procedure TfrmKeyEntryFreeOTFE.SetCDBAtOffset(CDBAtOffset: Boolean);
begin
  ckOffsetPointsToCDB.Checked := CDBAtOffset;
end;

function TfrmKeyEntryFreeOTFE.GetMountAs(): TFreeOTFEMountAs;
var
  currMountAs: TFreeOTFEMountAs;
begin
  Result := low(TFreeOTFEMountAs);

  for currMountAs := low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do begin
    if (cbMediaType.Items[cbMediaType.ItemIndex] = FreeOTFEMountAsTitle(currMountAs)) then begin
      Result := currMountAs;
      break;
    end;
  end;
end;

function TfrmKeyEntryFreeOTFE.SetMountAs(mountAs: TFreeOTFEMountAs): Boolean;
var
  idx:   Integer;
  allOK: Boolean;
begin
  idx                   := cbMediaType.Items.IndexOf(FreeOTFEMountAsTitle(mountAs));
  cbMediaType.ItemIndex := idx;

  allOK := (idx >= 0);

  Result := allOK;
end;

procedure TfrmKeyEntryFreeOTFE.PopulateDrives();
var
  driveLetters: String;
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


procedure TfrmKeyEntryFreeOTFE.PopulateMountAs();
var
  currMountAs: TFreeOTFEMountAs;
begin
  cbMediaType.Items.Clear();
  for currMountAs := low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do begin
    if (currMountAs <> fomaUnknown) then begin
      cbMediaType.Items.Add(FreeOTFEMountAsTitle(currMountAs));
    end;

  end;

end;

procedure TfrmKeyEntryFreeOTFE.PopulatePKCS11CDB();
var
  errMsg:     String;
  i:          Integer;
  warnBadCDB: Boolean;
  session:    TPKCS11Session;
begin
  // Purge stored CDBs...
  DestroyAndFreeRecord_PKCS11CDB(FTokenCDB);

  session := GetPKCS11Session();

  cbPKCS11CDB.items.Clear();
  if (session <> nil) then begin
    if not (GetAllPKCS11CDB(session, FTokenCDB, errMsg)) then begin
      INSMessageDlg(_('Unable to get list of CDB entries from Token') + SDUCRLF +
        SDUCRLF + errMsg, mtError);
    end;
  end;


  // Sanity check - the CDBs stored are sensible, right?
  warnBadCDB := False;
  // Populate combobox...
  for i := low(FTokenCDB) to high(FTokenCDB) do begin
    // Sanity check - the CDBs stored are sensible, right?
    if (length(FTokenCDB[i].CDB) <> (CRITICAL_DATA_LENGTH div 8)) then begin
      warnBadCDB := True;
    end else begin
      cbPKCS11CDB.items.AddObject(FTokenCDB[i].XLabel, TObject(FTokenCDB[i]));
    end;
  end;

  if warnBadCDB then begin
    INSMessageDlg(
      _('One or more of the keyfiles stored on your token are invalid/corrupt and will be ignored') + SDUCRLF + SDUCRLF +
      _('Please check which keyfiles are stored on this token and correct'),
      mtWarning
      );
  end;


  if (cbPKCS11CDB.items.Count > 0) then begin
    cbPKCS11CDB.items.InsertObject(0, RS_NOT_SELECTED, nil);
  end else begin
    cbPKCS11CDB.items.InsertObject(0, RS_NONE_AVAILABLE, nil);
  end;

  // If there's only one item in the list (apart from the the none
  // available/selected), select it
  if (cbPKCS11CDB.items.Count = 2) then begin
    cbPKCS11CDB.ItemIndex := 1;
  end else begin
    // Select the none available/selected item
    cbPKCS11CDB.ItemIndex := 0;
  end;

end;

procedure TfrmKeyEntryFreeOTFE.PopulatePKCS11SecretKey();
var
  errMsg:  String;
  i:       Integer;
  session: TPKCS11Session;
begin
  // Purge stored CDBs...
  DestroyAndFreeRecord_PKCS11SecretKey(FTokenSecretKey);

  session := GetPKCS11Session();

  cbPKCS11SecretKey.items.Clear();
  if (session <> nil) then begin
    if not (GetAllPKCS11SecretKey(session, FTokenSecretKey, errMsg)) then begin
      INSMessageDlg(_('Unable to get a list of secret keys from Token') + SDUCRLF +
        SDUCRLF + errMsg, mtError);
    end;
  end;


  // Populate combobox...
  for i := low(FTokenSecretKey) to high(FTokenSecretKey) do begin
    cbPKCS11SecretKey.items.AddObject(FTokenSecretKey[i].XLabel, TObject(FTokenSecretKey[i]));
  end;


  if (cbPKCS11SecretKey.items.Count > 0) then begin
    cbPKCS11SecretKey.items.InsertObject(0, RS_NOT_SELECTED, nil);
  end else begin
    cbPKCS11SecretKey.items.InsertObject(0, RS_NONE_AVAILABLE, nil);
  end;

  // Select the none available/selected item
  cbPKCS11SecretKey.ItemIndex := 0;

end;


// Returns TRUE if at least *one* volume was mounted successfully
function TfrmKeyEntryFreeOTFE.AttemptMount(): Boolean;
var
  errMsg:             String;
  cntMountOK:         Integer;
  cntMountFailed:     Integer;
  useKeyfilename:     String;
  usePKCS11CDB:       Ansistring;
  tmpCDBRecord:       PPKCS11CDB;
  usePKCS11SecretKey: PPKCS11SecretKey;
  usedSlotID:         Integer;
begin
  Result := False;

  if CheckInput() then begin
    usedSlotID := PKCS11_NO_SLOT_ID;

    useKeyfilename := '';
    if rbKeyfileFile.Checked then begin
      useKeyfilename := feKeyfile.Filename;
    end;

    usePKCS11CDB := '';
    if rbKeyfilePKCS11.Checked then begin
      // >0 here because first item is "none selected/none available"
      if (cbPKCS11CDB.ItemIndex > 0) then begin
        tmpCDBRecord := PPKCS11CDB(cbPKCS11CDB.Items.Objects[cbPKCS11CDB.ItemIndex]);
        usePKCS11CDB := tmpCDBRecord.CDB;
        usedSlotID   := FPKCS11Session.SlotID;
      end;
    end;

    usePKCS11SecretKey := nil;
    // >0 here because first item is "none selected/none available"
    if (cbPKCS11SecretKey.ItemIndex > 0) then begin
      usePKCS11SecretKey := PPKCS11SecretKey(
        cbPKCS11SecretKey.Items.Objects[cbPKCS11SecretKey.ItemIndex]);
      usedSlotID         := FPKCS11Session.SlotID;
    end;

    fMountedDrives := '';
    { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
    Result         := GetFreeOTFEBase.MountFreeOTFE(fVolumeFiles,
      SDUStringToSDUBytes(preUserkey.Text), useKeyfilename,
      usePKCS11CDB, usedSlotID,
      FPKCS11Session, usePKCS11SecretKey,
      seKeyIterations.Value, GetDriveLetter(),
      ckMountReadonly.Checked, GetMountAs(),
      se64UnitOffset.Value,
      ckOffsetPointsToCDB.Checked, seSaltLength.Value,
      ckMountForAllUsers.Checked,
      fMountedDrives);
    if not (Result) then begin
      GetFreeOTFEBase.CountMountedResults(
        fMountedDrives,
        cntMountOK,
        cntMountFailed
        );

      if (cntMountOK = 0) then begin
        // No volumes were mounted...
        errMsg := _('Unable to open Box.');

        // Specific problems when mounting...
        if (GetFreeOTFEBase.LastErrorCode = OTFE_ERR_WRONG_PASSWORD) then begin
          errMsg :=
            _('Unable to open Box; please ensure that you entered the correct details (keyphrase, etc)');
          if (feKeyfile.Filename <> '') then begin
            errMsg := errMsg + SDUCRLF + SDUCRLF +
              _(
              'Please ensure that you check/uncheck the "Data from offset includes CDB" option, as appropriate for your Box');
          end;
        end else
        if (GetFreeOTFEBase.LastErrorCode = OTFE_ERR_VOLUME_FILE_NOT_FOUND) then begin
          errMsg := _('Unable to find volume/read volume CDB.');
        end else
        if (GetFreeOTFEBase.LastErrorCode = OTFE_ERR_KEYFILE_NOT_FOUND) then begin
          errMsg := _('Unable to find keyfile/read keyfile.');
        end else
        if (GetFreeOTFEBase.LastErrorCode = OTFE_ERR_PKCS11_SECRET_KEY_DECRYPT_FAILURE) then begin
          errMsg := _('Unable to decrypt using PKCS#11 secret key.');
        end else
        if (GetFreeOTFEBase.LastErrorCode = OTFE_ERR_NO_FREE_DRIVE_LETTERS) then begin
          errMsg :=
            _('Unable to assign a new drive letter; please confirm you have drive letters free!');
        end else
        if (not (ckMountReadonly.Checked) and
          IsVolumeStoredOnReadonlyMedia()) then begin
          errMsg :=
            _('Unable to open Box; if a Box to be mounted is stored on readonly media (e.g. CDROM or DVD), please check the "open readonly" option.');
        end else
        if (not (ckMountReadonly.Checked) and
          IsVolumeMarkedAsReadonly()) then begin
          errMsg :=
            _('Unable to open Box; if a Box is readonly, please check the "open readonly" option.');
        end;

        INSMessageDlg(errMSg, mtError);
      end else
      if (cntMountFailed > 0) then begin
        // At least one volume was mounted, but not all of them
        errMsg := SDUPluralMsg(cntMountOK,
          SDUParamSubstitute(
          _('%1 Box was opened successfully, but %2 could not be opened'), [cntMountOK, cntMountFailed]),
          SDUParamSubstitute(
          _('%1 Boxes were opened successfully, but %2 could not be opened'),
          [cntMountOK, cntMountFailed]));

        INSMessageDlg(errMSg, mtWarning);
        Result := True;
      end;

    end;
  end;

end;

function TfrmKeyEntryFreeOTFE.IsVolumeStoredOnReadonlyMedia(): Boolean;
var
  i:                   Integer;
  currVol:             String;
  testDriveColonSlash: String;
begin
  Result := False;

  for i := 0 to (fVolumeFiles.Count - 1) do begin
    currVol := fVolumeFiles[i];

    if not (GetFreeOTFEBase.IsPartition_UserModeName(currVol)) then begin
      if (length(currVol) > 2) then begin
        // Check for ":" as 2nd char in filename; i.e. it's a filename with
        // <drive letter>:<path>\<filename>
        if (currVol[2] = ':') then begin
          testDriveColonSlash := currVol[1] + ':\';
          if (TDriveType(GetDriveType(PChar(testDriveColonSlash))) = dtCDROM) then begin
            // At least one of the volumes is stored on a CDROM (readonly media)
            Result := True;
            break;
          end;

        end;
      end;
    end;

  end;
end;

function TfrmKeyEntryFreeOTFE.IsVolumeMarkedAsReadonly(): Boolean;
var
  i:       Integer;
  currVol: String;
begin
  Result := False;

  for i := 0 to (fVolumeFiles.Count - 1) do begin
    currVol := fVolumeFiles[i];

    if not (GetFreeOTFEBase.IsPartition_UserModeName(currVol)) then begin
      if (length(currVol) > 2) then begin
        // Check for ":" as 2nd char in filename; i.e. it's a filename with
        // <drive letter>:<path>\<filename>
        if (currVol[2] = ':') then begin
          if FileIsReadOnly(currVol) then begin
            // At least one of the volumes is readonly
            Result := True;
            break;
          end;

        end;
      end;
    end;

  end;
end;


function TfrmKeyEntryFreeOTFE.CheckInput(): Boolean;
begin
  Result := True;

  if (seSaltLength.Value mod 8 <> 0) then begin
    INSMessageDlg(
      _('Salt length (in bits) must be a multiple of 8'),
      mtError
      );
    Result := False;
  end;
end;

procedure TfrmKeyEntryFreeOTFE.pbOKClick(Sender: TObject);
begin
  if AttemptMount() then begin
    ModalResult := mrOk;
  end;
end;


procedure TfrmKeyEntryFreeOTFE.preUserkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) then begin
    DoCancel();
  end;

end;

procedure TfrmKeyEntryFreeOTFE.rbKeyfilePKCS11Click(Sender: TObject);
begin
  PopulatePKCS11CDB();

  // If there are no keyfiles; flip back
  if (cbPKCS11CDB.items.Count <= 1) then begin
    // If we have a session, the user's logged into the token. However, no
    // keyfiles are on the token, warn the user
    if (FPKCS11Session <> nil) then begin
      INSMessageDlg(
        _('No keyfiles could be found on the token inserted.'),
        mtInformation
        );
    end;

    rbKeyfileFile.Checked := True;
  end;

  EnableDisableControls_Keyfile();

end;

procedure TfrmKeyEntryFreeOTFE.SDUDropFiles_KeyfileFileDrop(Sender: TObject;
  DropItem: String; DropPoint: TPoint);
begin
  SetKeyfile(DropItem);
end;

function TfrmKeyEntryFreeOTFE.GetPKCS11Session(): TPKCS11Session;
var
  pkcs11Dlg: TfrmPKCS11Session;
begin
  if (FPKCS11Session = nil) then begin
    if PKCS11LibraryReady(GetFreeOTFEBase.PKCS11Library) then begin
      // Setup PKCS11 session, as appropriate
      pkcs11Dlg := TfrmPKCS11Session.Create(nil);
      try
        pkcs11Dlg.PKCS11LibObj := GetFreeOTFEBase.PKCS11Library;
        pkcs11Dlg.AllowSkip    := False;
        if (pkcs11Dlg.ShowModal = mrOk) then begin
          FPKCS11Session := pkcs11Dlg.Session;
        end;
      finally
        pkcs11Dlg.Free();
      end;
    end;

  end;

  Result := FPKCS11Session;
end;

procedure TfrmKeyEntryFreeOTFE.rbKeyfileFileClick(Sender: TObject);
begin
  EnableDisableControls_Keyfile();
end;

procedure TfrmKeyEntryFreeOTFE.pbCancelClick(Sender: TObject);
begin
  DoCancel();

end;


procedure TfrmKeyEntryFreeOTFE.DoCancel();
begin
  ModalResult := mrCancel;

end;


procedure TfrmKeyEntryFreeOTFE.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Posting WM_CLOSE causes Delphi to reset ModalResult to mrCancel.
  // As a result, we reset ModalResult here
  if fsilent then begin
    ModalResult := FSilentResult;
  end;
end;

procedure TfrmKeyEntryFreeOTFE.FormCreate(Sender: TObject);
begin
fVolumeFiles := TStringList.Create;
  fsilent := False;

  pnlLower.BevelOuter    := bvNone;
  pnlLower.BevelInner    := bvNone;
  pnlLower.Caption       := '';
  pnlBasic.BevelOuter    := bvNone;
  pnlBasic.BevelInner    := bvNone;
  pnlBasic.Caption       := '';
  pnlAdvanced.BevelOuter := bvNone;
  pnlAdvanced.BevelInner := bvNone;
  pnlAdvanced.Caption    := '';
  pnlButtons.BevelOuter  := bvNone;
  pnlButtons.BevelInner  := bvNone;
  pnlButtons.Caption     := '';

  rbKeyfileFile.Checked := True;
  feKeyfile.Filename    := '';

  cbPKCS11CDB.Sorted := True;

  SetLength(FTokenCDB, 0);
  cbPKCS11CDB.Items.AddObject(RS_NOT_SELECTED, nil);

  SetLength(FTokenSecretKey, 0);
  cbPKCS11SecretKey.Items.AddObject(RS_NOT_SELECTED, nil);

  preUserKey.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKey.WantReturns := True;
  preUserKey.WordWrap    := True;
  preUserKey.Lines.Clear();

  se64UnitOffset.Value        := 0;
  ckOffsetPointsToCDB.Checked := True;

  seSaltLength.Increment := 8;
  seSaltLength.Value     := DEFAULT_SALT_LENGTH;

  seKeyIterations.MinValue  := 1;
  seKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seKeyIterations.Value     := DEFAULT_KEY_ITERATIONS;

  DisplayAdvanced(False);
end;

procedure TfrmKeyEntryFreeOTFE.FormDestroy(Sender: TObject);
begin
  DestroyAndFreeRecord_PKCS11CDB(FTokenCDB);
  DestroyAndFreeRecord_PKCS11SecretKey(FTokenSecretKey);

  if (FPKCS11Session <> nil) then begin
    FPKCS11Session.Logout();
    FPKCS11Session.CloseSession();
    FPKCS11Session.Free();
  end;
fVolumeFiles.Free;
end;

procedure TfrmKeyEntryFreeOTFE.FormShow(Sender: TObject);
var
  i:               Integer;
  currDriveLetter: DriveLetterChar;
begin
  feKeyfile.Filter             := FILE_FILTER_FLT_KEYFILES;
  feKeyfile.DefaultExt         := FILE_FILTER_DFLT_KEYFILES;
  feKeyfile.OpenDialog.Options := feKeyfile.OpenDialog.Options + [ofDontAddToRecent];
  feKeyfile.SaveDialog.Options := feKeyfile.SaveDialog.Options + [ofDontAddToRecent];

  preUserKey.PasswordChar := GetFreeOTFEBase.PasswordChar;
  preUserKey.WantReturns  := GetFreeOTFEBase.AllowNewlinesInPasswords;
  preUserKey.WantTabs     := GetFreeOTFEBase.AllowTabsInPasswords;

  // Note: PKCS#11 CDB list only populated when selected; it's at that point
  //       that the user is prompted for their token's PIN
  // PopulatePKCS11CDB();

  PopulateDrives();
  if (cbDrive.Items.Count > 0) then begin
    cbDrive.ItemIndex := 0;

    if (GetFreeOTFEBase is TOTFEFreeOTFE) then begin
      if (GetFreeOTFE.DefaultDriveLetter <> #0) then begin
        // Start from 1; skip the default
        for i := 1 to (cbDrive.items.Count - 1) do begin
          currDriveLetter := cbDrive.Items[i][1];
          if (currDriveLetter >= GetFreeOTFE.DefaultDriveLetter) then begin
            cbDrive.ItemIndex := i;
            break;
          end;
        end;
      end;
    end;
  end;

  PopulateMountAs();

  if (GetFreeOTFEBase is TOTFEFreeOTFE) then begin
    SetMountAs(GetFreeOTFE.DefaultMountAs);
  end else begin
    SetMountAs(fomaRemovableDisk);
  end;

  // Certain controls only visble if used in conjunction with drive mounting
  gbMountAs.Visible := GetFreeOTFEBase is TOTFEFreeOTFE;
  lblDrive.Visible  := GetFreeOTFEBase is TOTFEFreeOTFE;
  cbDrive.Visible   := GetFreeOTFEBase is TOTFEFreeOTFE;

  // If the mount options groupbox isn't visible, widen the volume options
  // groupbox so that there's no blank space to its left
  if not (gbMountAs.Visible) then begin
    gbVolumeOptions.Width := gbVolumeOptions.Width + (gbVolumeOptions.left - gbMountAs.left);
    gbVolumeOptions.left  := gbMountAs.left;
  end;

  // Default to TRUE to allow formatting under Windows Vista
  ckMountForAllUsers.Checked := True;

  EnableDisableControls();

  // Position cursor to the *end* of any password
  preUserKey.SelStart := length(preUserKey.Text);

  if fSilent then begin
    if AttemptMount() then begin
      ModalResult := mrOk;
    end else begin
      ModalResult := mrCancel;
    end;

    FSilentResult := ModalResult;

    PostMessage(Handle, WM_CLOSE, 0, 0);
  end;

  SDUDropFiles_Keyfile.Active := True;
end;

procedure TfrmKeyEntryFreeOTFE.EnableDisableControls();
var
  tmpMountAs: TFreeOTFEMountAs;
begin
  // Ensure we know what to mount as
  ckMountReadonly.Enabled := False;
  tmpMountAs              := GetMountAs();
  if not (FreeOTFEMountAsCanWrite[tmpMountAs]) then begin
    ckMountReadonly.Checked := True;
  end;
  SDUEnableControl(ckMountReadonly, FreeOTFEMountAsCanWrite[tmpMountAs]);

  EnableDisableControls_Keyfile();
  EnableDisableControls_SecretKey();

  pbOK.Enabled := (tmpMountAs <> fomaUnknown) and (cbDrive.ItemIndex >= 0) and
    (seKeyIterations.Value > 0) and (seSaltLength.Value >= 0);
end;

procedure TfrmKeyEntryFreeOTFE.EnableDisableControls_SecretKey();
begin
  // PKCS#11 secret key controls...
  SDUEnableControl(cbPKCS11SecretKey, (
    // Must have more than the "none" item
    (cbPKCS11SecretKey.items.Count >
    1)));
end;

procedure TfrmKeyEntryFreeOTFE.EnableDisableControls_Keyfile();
begin
  // We never disable rbKeyfileFile, as keeping it enabled gives the user a
  // visual clue that they can enter a keyfile filename
  // SDUEnableControl(rbKeyfileFile, PKCS11LibraryReady(fFreeOTFEObj.PKCS11Library));

  // Protect as this can be called as part of creation
  if (GetFreeOTFEBase <> nil) then begin
    SDUEnableControl(rbKeyfilePKCS11, PKCS11LibraryReady(GetFreeOTFEBase.PKCS11Library));
    if not (PKCS11LibraryReady(GetFreeOTFEBase.PKCS11Library)) then begin
      rbKeyfileFile.Checked   := True;
      rbKeyfilePKCS11.Checked := False;
    end;
  end;

  // File based keyfile controls...
  SDUEnableControl(feKeyfile, rbKeyfileFile.Checked);

  // PKCS#11 based keyfile controls...
  SDUEnableControl(cbPKCS11CDB, (rbKeyfilePKCS11.Checked and
    // Must have more than the "none" item
    (cbPKCS11CDB.items.Count > 1)));


  // If no keyfile/PKCS#11 CDB is specified, then the CDB must reside within
  // the volume file
  if ((rbKeyfileFile.Checked and (feKeyfile.Filename = '')) or
    (rbKeyfilePKCS11.Checked and (cbPKCS11CDB.ItemIndex =
    0) // None available/none selected
    )) then begin
    ckOffsetPointsToCDB.Enabled := False;
    ckOffsetPointsToCDB.Checked := True;
  end else begin
    // If a keyfile is specified, then the user can specify if the volume file
    // includes a CDB
    ckOffsetPointsToCDB.Enabled := True;
  end;

end;

procedure TfrmKeyEntryFreeOTFE.pbAdvancedClick(Sender: TObject);
begin
  DisplayAdvanced(not (pnlAdvanced.Visible));
end;

procedure TfrmKeyEntryFreeOTFE.feKeyfileChange(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmKeyEntryFreeOTFE.seSaltLengthChange(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmKeyEntryFreeOTFE.seKeyIterationsChange(Sender: TObject);
begin
  EnableDisableControls();
end;


procedure TfrmKeyEntryFreeOTFE.cbMediaTypeChange(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmKeyEntryFreeOTFE.cbPKCS11CDBChange(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmKeyEntryFreeOTFE.DisplayAdvanced(displayAdvanced: Boolean);
var
  displayChanged: Boolean;
begin
  displayChanged      := (pnlAdvanced.Visible <> displayAdvanced);
  pnlAdvanced.Visible := displayAdvanced;

  if displayChanged then begin
    if pnlAdvanced.Visible then begin
      self.Height := self.Height + pnlAdvanced.Height;

      PopulatePKCS11SecretKey();
      EnableDisableControls_SecretKey();
    end else begin
      self.Height := self.Height - pnlAdvanced.Height;
    end;

  end;

  if pnlAdvanced.Visible then begin
    pbAdvanced.Caption := '<< ' + RS_BUTTON_ADVANCED;
  end else begin
    pbAdvanced.Caption := RS_BUTTON_ADVANCED + ' >>';
  end;

end;

procedure TfrmKeyEntryFreeOTFE.SetPassword(password: TSDUBytes);
begin
  preUserKey.Text := SDUBytesToString(password);
end;

 // "INS" - "If Not Silent"
 // Display message, if "Silent" not set to TRUE
function TfrmKeyEntryFreeOTFE.INSMessageDlg(Content: String; DlgType: TMsgDlgType): Integer;
begin
  Result := mrOk;
  if not (fsilent) then begin
    Result := SDUMessageDlg(Content, DlgType);
  end;

end;

procedure TfrmKeyEntryFreeOTFE.SetVolumeFilesText(Value :string);
begin
  fVolumeFiles.Text := Value;
end;

end.
