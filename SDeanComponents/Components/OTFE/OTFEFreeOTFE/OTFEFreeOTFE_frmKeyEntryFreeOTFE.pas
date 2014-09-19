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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, PasswordRichEdit, Spin64,
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_U,
  ExtCtrls,
  OTFEFreeOTFE_PKCS11,
  SDUGeneral,
  pkcs11_session,
  pkcs11_library, OTFEFreeOTFE_PasswordRichEdit, SDUForms, SDUFrames,
  SDUSpin64Units, SDUFilenameEdit_U, SDUDropFiles;

type
  TfrmKeyEntryFreeOTFE = class(TSDUForm)
    pnlBasic: TPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label6: TLabel;
    lblDrive: TLabel;
    preUserKey: TOTFEFreeOTFE_PasswordRichEdit;
    cbDrive: TComboBox;
    ckMountReadonly: TCheckBox;
    pnlLower: TPanel;
    pnlButtons: TPanel;
    pbCancel: TButton;
    pbOK: TButton;
    pbAdvanced: TButton;
    pnlAdvanced: TPanel;
    gbVolumeOptions: TGroupBox;
    Label8: TLabel;
    ckOffsetPointsToCDB: TCheckBox;
    gbMountAs: TGroupBox;
    Label9: TLabel;
    cbMediaType: TComboBox;
    ckMountForAllUsers: TCheckBox;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    seSaltLength: TSpinEdit64;
    seKeyIterations: TSpinEdit64;
    cbPKCS11CDB: TComboBox;
    rbKeyfileFile: TRadioButton;
    rbKeyfilePKCS11: TRadioButton;
    cbPKCS11SecretKey: TComboBox;
    Label10: TLabel;
    se64UnitOffset: TSDUSpin64Unit_Storage;
    feKeyfile: TSDUFilenameEdit;
    SDUDropFiles_Keyfile: TSDUDropFiles;
    procedure pbOKClick(Sender: TObject);
    procedure preUserkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
    procedure SDUDropFiles_KeyfileFileDrop(Sender: TObject; DropItem: string;
      DropPoint: TPoint);
  protected
    fTokenCDB: TPKCS11CDBPtrArray;
    fTokenSecretKey: TPKCS11SecretKeyPtrArray;
    fPkcs11session: TPKCS11Session;

    fsilentResult: TModalResult;

    procedure PopulateDrives();
    procedure PopulateMountAs();
    procedure PopulatePKCS11CDB();
    procedure PopulatePKCS11SecretKey();

    procedure DoCancel();

    procedure EnableDisableControls();
    procedure EnableDisableControls_Keyfile();
    procedure EnableDisableControls_SecretKey();

    function  GetDriveLetter(): Ansichar;
    function  GetMountAs(): TFreeOTFEMountAs;
    function  SetMountAs(mountAs: TFreeOTFEMountAs): boolean;

    function CheckInput(): boolean;
    function AttemptMount(): boolean;

    function GetPKCS11Session(): TPKCS11Session;

    function IsVolumeStoredOnReadonlyMedia(): boolean;
    function IsVolumeMarkedAsReadonly(): boolean;

    function INSMessageDlg(
      Content: string;
      DlgType: TMsgDlgType
    ): integer;

  public
    fFreeOTFEObj: TOTFEFreeOTFEBase;
    fsilent: boolean;
    fVolumeFiles: TStringList;
    fmountedDrives: Ansistring;

    procedure SetPassword(password: ansistring);
    procedure SetReadOnly(readonly: boolean);
    procedure SetKeyfile(keyFilename: string);
    procedure SetOffset(offset: ULONGLONG);
    procedure SetSaltLength(saltLength: integer);
    procedure SetKeyIterations(keyIterations: integer);
    procedure SetCDBAtOffset(CDBAtOffset: boolean);

    procedure DisplayAdvanced(displayAdvanced: boolean);

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
  SDUDialogs,
  SDUi18n,
  OTFEConsts_U,
  OTFEFreeOTFE_DriverAPI,
  OTFEFreeOTFE_frmPKCS11Session,
  Shredder,
  pkcs11_object;

resourcestring
  RS_NOT_SELECTED = '<none selected>';
  RS_NONE_AVAILABLE = '<none available>';

  RS_BUTTON_ADVANCED = '&Advanced';


function TfrmKeyEntryFreeOTFE.GetDriveLetter(): Ansichar;
var
  driveLetter: Ansichar;
begin
  driveLetter := #0;
  // Note: The item at index zero is "Use default"; #0 is returned for this
  if (cbDrive.ItemIndex>0) then
    begin
    driveLetter :=AnsiChar( cbDrive.Items[cbDrive.ItemIndex][1]);
    end;

  Result := driveLetter;
end;


procedure TfrmKeyEntryFreeOTFE.SetReadOnly(readonly: boolean);
begin
  ckMountReadonly.checked := readonly;
end;

procedure TfrmKeyEntryFreeOTFE.SetKeyfile(keyFilename: string);
begin
  rbKeyfileFile.checked := TRUE;
  feKeyfile.Filename := keyFilename
end;

procedure TfrmKeyEntryFreeOTFE.SetOffset(offset: ULONGLONG);
begin
  se64UnitOffset.Value := offset;
end;

procedure TfrmKeyEntryFreeOTFE.SetSaltLength(saltLength: integer);
begin
  seSaltLength.Value := saltLength;
end;

procedure TfrmKeyEntryFreeOTFE.SetKeyIterations(keyIterations: integer);
begin
  seKeyIterations.Value := keyIterations;
end;

procedure TfrmKeyEntryFreeOTFE.SetCDBAtOffset(CDBAtOffset: boolean);
begin
  ckOffsetPointsToCDB.checked := CDBAtOffset;
end;

function TfrmKeyEntryFreeOTFE.GetMountAs(): TFreeOTFEMountAs;
var
  retval: TFreeOTFEMountAs;
  currMountAs: TFreeOTFEMountAs;
begin
  retval := low(TFreeOTFEMountAs);

  for currMountAs:=low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do
    begin
    if (cbMediaType.Items[cbMediaType.ItemIndex] = FreeOTFEMountAsTitle(currMountAs)) then
      begin
      retval := currMountAs;
      break;
      end;
    end;

  Result := retval;
end;

function TfrmKeyEntryFreeOTFE.SetMountAs(mountAs: TFreeOTFEMountAs): boolean;
var
  idx: integer;
  allOK: boolean;
begin
  idx := cbMediaType.Items.IndexOf(FreeOTFEMountAsTitle(mountAs));
  cbMediaType.ItemIndex := idx;

  allOK := (idx >= 0);

  Result := allOK;
end;

procedure TfrmKeyEntryFreeOTFE.PopulateDrives();
var
  driveLetters: string;
  i: integer;
begin
  cbDrive.Items.Clear();
  cbDrive.Items.Add(_('Use default'));
  driveLetters := SDUGetUnusedDriveLetters();
  for i:=1 to length(driveLetters) do
    begin
    // Skip the drive letters traditionally reserved for floppy disk drives
//    if (
//        (driveLetters[i] <> 'A') AND
//        (driveLetters[i] <> 'B')
//       ) then
//      begin
      cbDrive.Items.Add(driveLetters[i]+':');
//      end;
    end;

end;


procedure TfrmKeyEntryFreeOTFE.PopulateMountAs();
var
  currMountAs: TFreeOTFEMountAs;
begin
  cbMediaType.Items.Clear();
  for currMountAs:=low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do
    begin
    if (currMountAs <> fomaUnknown) then
      begin
      cbMediaType.Items.Add(FreeOTFEMountAsTitle(currMountAs));
      end;

    end;

end;

procedure TfrmKeyEntryFreeOTFE.PopulatePKCS11CDB();
var
  errMsg: string;
  i: integer;
  warnBadCDB: boolean;
  session: TPKCS11Session;
begin
  // Purge stored CDBs...
  DestroyAndFreeRecord_PKCS11CDB(FTokenCDB);

  session := GetPKCS11Session();

  cbPKCS11CDB.items.clear();
  if (session <> nil) then
    begin
    if not(GetAllPKCS11CDB(session, FTokenCDB, errMsg)) then
      begin
      INSMessageDlg(_('Unable to get list of CDB entries from Token')+SDUCRLF+SDUCRLF+errMsg, mtError);
      end;
    end;


  // Sanity check - the CDBs stored are sensible, right?
  warnBadCDB := FALSE;
  // Populate combobox...
  for i:=low(FTokenCDB) to high(FTokenCDB) do
    begin
    // Sanity check - the CDBs stored are sensible, right?
    if (length(FTokenCDB[i].CDB) <> (CRITICAL_DATA_LENGTH div 8)) then
      begin
      warnBadCDB:= TRUE;
      end
    else
      begin
      cbPKCS11CDB.items.AddObject(FTokenCDB[i].XLabel, TObject(FTokenCDB[i]));
      end;
    end;

  if warnBadCDB then
    begin
    INSMessageDlg(
                  _('One or more of the keyfiles stored on your token are invalid/corrupt and will be ignored')+SDUCRLF+
                  SDUCRLF+
                  _('Please check which keyfiles are stored on this token and correct'),
                  mtWarning
                 );
    end;


  if (cbPKCS11CDB.items.count > 0) then
    begin
    cbPKCS11CDB.items.InsertObject(0, RS_NOT_SELECTED, nil);
    end
  else
    begin
    cbPKCS11CDB.items.InsertObject(0, RS_NONE_AVAILABLE, nil);
    end;

  // If there's only one item in the list (apart from the the none
  // available/selected), select it
  if (cbPKCS11CDB.items.count = 2) then
    begin
    cbPKCS11CDB.itemindex := 1;
    end
  else
    begin
    // Select the none available/selected item
    cbPKCS11CDB.itemindex := 0;
    end;

end;

procedure TfrmKeyEntryFreeOTFE.PopulatePKCS11SecretKey();
var
  errMsg: string;
  i: integer;
  session: TPKCS11Session;
begin
  // Purge stored CDBs...
  DestroyAndFreeRecord_PKCS11SecretKey(FTokenSecretKey);

  session := GetPKCS11Session();

  cbPKCS11SecretKey.items.clear();
  if (session <> nil) then
    begin
    if not(GetAllPKCS11SecretKey(session, FTokenSecretKey, errMsg)) then
      begin
      INSMessageDlg(_('Unable to get a list of secret keys from Token')+SDUCRLF+SDUCRLF+errMsg, mtError);
      end;
    end;


  // Populate combobox...
  for i:=low(FTokenSecretKey) to high(FTokenSecretKey) do
    begin
    cbPKCS11SecretKey.items.AddObject(FTokenSecretKey[i].XLabel, TObject(FTokenSecretKey[i]));
    end;


  if (cbPKCS11SecretKey.items.count > 0) then
    begin
    cbPKCS11SecretKey.items.InsertObject(0, RS_NOT_SELECTED, nil);
    end
  else
    begin
    cbPKCS11SecretKey.items.InsertObject(0, RS_NONE_AVAILABLE, nil);
    end;

  // Select the none available/selected item
  cbPKCS11SecretKey.itemindex := 0;

end;


// Returns TRUE if at least *one* volume was mounted successfully
function TfrmKeyEntryFreeOTFE.AttemptMount(): boolean;
var
  errMsg: string;
  cntMountOK: integer;
  cntMountFailed: integer;
  useKeyfilename: string;
  usePKCS11CDB: AnsiString;
  tmpCDBRecord: PPKCS11CDB;
  usePKCS11SecretKey: PPKCS11SecretKey;
  usedSlotID: integer;
begin
  result := FALSE;

  if CheckInput() then
    begin
    usedSlotID := PKCS11_NO_SLOT_ID;
    
    useKeyfilename := '';
    if rbKeyfileFile.checked then
      begin
      useKeyfilename := feKeyfile.Filename;
      end;

    usePKCS11CDB := '';
    if rbKeyfilePKCS11.checked then
      begin
      // >0 here because first item is "none selected/none available"
      if (cbPKCS11CDB.itemindex > 0) then
        begin
        tmpCDBRecord := PPKCS11CDB(cbPKCS11CDB.Items.Objects[cbPKCS11CDB.itemindex]);
        usePKCS11CDB := tmpCDBRecord.CDB;
        usedSlotID := FPKCS11Session.SlotID;
        end;
      end;

    usePKCS11SecretKey := nil;
    // >0 here because first item is "none selected/none available"
    if (cbPKCS11SecretKey.ItemIndex > 0) then
      begin
      usePKCS11SecretKey := PPKCS11SecretKey(cbPKCS11SecretKey.Items.Objects[cbPKCS11SecretKey.itemindex]);
      usedSlotID := FPKCS11Session.SlotID;
      end;

    fMountedDrives := '';
    { TODO 1 -otdk -cbug : handle non ascii user keys - at least warn user }
    result := fFreeOTFEObj.MountFreeOTFE(
                          fVolumeFiles,
                          Ansistring(preUserkey.Text),
                          useKeyfilename,
                          usePKCS11CDB,
                          usedSlotID, 
                          FPKCS11Session,
                          usePKCS11SecretKey,
                          seKeyIterations.Value,
                          GetDriveLetter(),
                          ckMountReadonly.checked,
                          GetMountAs(),
                          se64UnitOffset.Value,
                          ckOffsetPointsToCDB.checked,
                          seSaltLength.Value,
                          ckMountForAllUsers.checked,
                          fMountedDrives
                        );
    if not(result) then
      begin
      fFreeOTFEObj.CountMountedResults(
                                      fMountedDrives,
                                      cntMountOK,
                                      cntMountFailed
                                     );

      if (cntMountOK = 0) then
        begin
        // No volumes were mounted...
        errMsg := _('Unable to open Box.');

        // Specific problems when mounting...
        if (fFreeOTFEObj.LastErrorCode = OTFE_ERR_WRONG_PASSWORD) then
          begin
          errMsg := _('Unable to open Box; please ensure that you entered the correct details (keyphrase, etc)');
          if (feKeyfile.Filename <> '') then
            begin
            errMsg := errMsg + SDUCRLF +
                      SDUCRLF+
                      _('Please ensure that you check/uncheck the "Data from offset includes CDB" option, as appropriate for your Box');
            end;
          end
        else if (fFreeOTFEObj.LastErrorCode = OTFE_ERR_VOLUME_FILE_NOT_FOUND) then
          begin
          errMsg := _('Unable to find volume/read volume CDB.');
          end
        else if (fFreeOTFEObj.LastErrorCode = OTFE_ERR_KEYFILE_NOT_FOUND) then
          begin
          errMsg := _('Unable to find keyfile/read keyfile.');
          end
        else if (fFreeOTFEObj.LastErrorCode = OTFE_ERR_PKCS11_SECRET_KEY_DECRYPT_FAILURE) then
          begin
          errMsg := _('Unable to decrypt using PKCS#11 secret key.');
          end
        else if (fFreeOTFEObj.LastErrorCode = OTFE_ERR_NO_FREE_DRIVE_LETTERS) then
          begin
          errMsg := _('Unable to assign a new drive letter; please confirm you have drive letters free!');
          end
        else if (
                 not(ckMountReadonly.checked) and
                 IsVolumeStoredOnReadonlyMedia()
                ) then
          begin
          errMsg := _('Unable to open Box; if a Box to be mounted is stored on readonly media (e.g. CDROM or DVD), please check the "open readonly" option.');
          end
        else if (
                 not(ckMountReadonly.checked) and
                 IsVolumeMarkedAsReadonly()
                ) then
          begin
          errMsg := _('Unable to open Box; if a Box is readonly, please check the "open readonly" option.');
          end;

        INSMessageDlg(errMSg, mtError);
        end
      else if (cntMountFailed > 0) then
        begin
        // At least one volume was mounted, but not all of them
        errMsg := SDUPluralMsg(
                               cntMountOK,
                               SDUParamSubstitute(_('%1 Box was opened successfully, but %2 could not be opened'), [cntMountOK, cntMountFailed]),
                               SDUParamSubstitute(_('%1 Boxes were opened successfully, but %2 could not be opened'), [cntMountOK, cntMountFailed])
                              );

        INSMessageDlg(errMSg, mtWarning);
        result := TRUE;
        end;

      end;
    end;


end;

function TfrmKeyEntryFreeOTFE.IsVolumeStoredOnReadonlyMedia(): boolean;
var
  i: integer;
  retval: boolean;
  currVol: string;
  testDriveColonSlash: string;
begin
  retval:= FALSE;

  for i:=0 to (fVolumeFiles.count - 1) do
    begin
    currVol := fVolumeFiles[i];

    if not(fFreeOTFEObj.IsPartition_UserModeName(currVol)) then
      begin
      if (length(currVol) > 2) then
        begin
        // Check for ":" as 2nd char in filename; i.e. it's a filename with
        // <drive letter>:<path>\<filename>
        if (currVol[2] = ':') then
          begin
          testDriveColonSlash := currVol[1] + ':\';
          if (TDriveType(GetDriveType(PChar(testDriveColonSlash))) = dtCDROM) then
            begin
            // At least one of the volumes is stored on a CDROM (readonly media)
            retval := TRUE;
            break;
            end;

          end;
        end;
      end;

    end;

  Result := retval;
end;

function TfrmKeyEntryFreeOTFE.IsVolumeMarkedAsReadonly(): boolean;
var
  i: integer;
  retval: boolean;
  currVol: string;
begin
  retval:= FALSE;

  for i:=0 to (fVolumeFiles.count - 1) do
    begin
    currVol := fVolumeFiles[i];

    if not(fFreeOTFEObj.IsPartition_UserModeName(currVol)) then
      begin
      if (length(currVol) > 2) then
        begin
        // Check for ":" as 2nd char in filename; i.e. it's a filename with
        // <drive letter>:<path>\<filename>
        if (currVol[2] = ':') then
          begin
          if FileIsReadOnly(currVol) then
            begin
            // At least one of the volumes is readonly
            retval := TRUE;
            break;
            end;

          end;
        end;
      end;

    end;

  Result := retval;
end;


function TfrmKeyEntryFreeOTFE.CheckInput(): boolean;
var
  retval: boolean;
begin
  retval:= TRUE;

  if (seSaltLength.value mod 8 <> 0) then
    begin
    INSMessageDlg(
                  _('Salt length (in bits) must be a multiple of 8'),
                  mtError
                 );
    retval := FALSE;
    end;

  Result := retval;
end;

procedure TfrmKeyEntryFreeOTFE.pbOKClick(Sender: TObject);
begin
  if AttemptMount() then
    begin
    ModalResult := mrOK;
    end;

end;


procedure TfrmKeyEntryFreeOTFE.preUserkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) then
    begin
    DoCancel();
    end;

end;

procedure TfrmKeyEntryFreeOTFE.rbKeyfilePKCS11Click(Sender: TObject);
begin
  PopulatePKCS11CDB();

  // If there are no keyfiles; flip back
  if (cbPKCS11CDB.items.count <= 1) then
    begin
    // If we have a session, the user's logged into the token. However, no
    // keyfiles are on the token, warn the user
    if (FPKCS11Session <> nil) then
      begin
      INSMessageDlg(
                    _('No keyfiles could be found on the token inserted.'),
                    mtInformation
                   );
      end;

    rbKeyfileFile.checked := TRUE;
    end;

  EnableDisableControls_Keyfile();

end;

procedure TfrmKeyEntryFreeOTFE.SDUDropFiles_KeyfileFileDrop(Sender: TObject;
  DropItem: string; DropPoint: TPoint);
begin
  SetKeyfile(DropItem);
end;

function TfrmKeyEntryFreeOTFE.GetPKCS11Session(): TPKCS11Session;
var
  pkcs11Dlg: TfrmPKCS11Session;
begin
  if (FPKCS11Session = nil) then
    begin
    if PKCS11LibraryReady(fFreeOTFEObj.PKCS11Library) then
      begin
      // Setup PKCS11 session, as appropriate
      pkcs11Dlg := TfrmPKCS11Session.Create(nil);
      try
        pkcs11Dlg.PKCS11LibObj := fFreeOTFEObj.PKCS11Library;
        pkcs11Dlg.AllowSkip := FALSE;
        if (pkcs11Dlg.ShowModal = mrOK) then
          begin
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


procedure TfrmKeyEntryFreeOTFE.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Posting WM_CLOSE causes Delphi to reset ModalResult to mrCancel.
  // As a result, we reset ModalResult here
  if fsilent then
    begin
    ModalResult := FSilentResult;
    end;

end;

procedure TfrmKeyEntryFreeOTFE.FormCreate(Sender: TObject);
begin
  fsilent := FALSE;
  
  pnlLower.BevelOuter := bvNone;
  pnlLower.BevelInner := bvNone;
  pnlLower.caption := '';
  pnlBasic.BevelOuter := bvNone;
  pnlBasic.BevelInner := bvNone;
  pnlBasic.caption := '';
  pnlAdvanced.BevelOuter := bvNone;
  pnlAdvanced.BevelInner := bvNone;
  pnlAdvanced.caption := '';
  pnlButtons.BevelOuter := bvNone;
  pnlButtons.BevelInner := bvNone;
  pnlButtons.caption := '';

  rbKeyfileFile.checked   := TRUE;
  feKeyfile.Filename := '';

  cbPKCS11CDB.Sorted := TRUE;

  SetLength(FTokenCDB, 0);
  cbPKCS11CDB.Items.AddObject(RS_NOT_SELECTED, nil);

  SetLength(FTokenSecretKey, 0);
  cbPKCS11SecretKey.Items.AddObject(RS_NOT_SELECTED, nil);

  preUserKey.Plaintext := TRUE;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKey.WantReturns := TRUE;
  preUserKey.WordWrap := TRUE;
  preUserKey.Lines.Clear();

  se64UnitOffset.Value := 0;
  ckOffsetPointsToCDB.Checked := TRUE;

  seSaltLength.Increment := 8;
  seSaltLength.Value := DEFAULT_SALT_LENGTH;

  seKeyIterations.MinValue := 1;
  seKeyIterations.MaxValue := 999999; // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seKeyIterations.Value := DEFAULT_KEY_ITERATIONS;

  DisplayAdvanced(FALSE);
end;

procedure TfrmKeyEntryFreeOTFE.FormDestroy(Sender: TObject);
begin
  DestroyAndFreeRecord_PKCS11CDB(FTokenCDB);
  DestroyAndFreeRecord_PKCS11SecretKey(FTokenSecretKey);

  if (FPKCS11Session <> nil) then
    begin
    FPKCS11Session.Logout();
    FPKCS11Session.CloseSession();
    FPKCS11Session.Free();
    end;

end;

procedure TfrmKeyEntryFreeOTFE.FormShow(Sender: TObject);
var
  i: integer;
  currDriveLetter: ansichar;
begin
  feKeyfile.Filter     := FILE_FILTER_FLT_KEYFILES;
  feKeyfile.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  feKeyfile.OpenDialog.Options := feKeyfile.OpenDialog.Options + [ofDontAddToRecent];
  feKeyfile.SaveDialog.Options := feKeyfile.SaveDialog.Options + [ofDontAddToRecent];

  preUserKey.PasswordChar := fFreeOTFEObj.PasswordChar;
  preUserKey.WantReturns  := fFreeOTFEObj.AllowNewlinesInPasswords;
  preUserKey.WantTabs     := fFreeOTFEObj.AllowTabsInPasswords;

  // Note: PKCS#11 CDB list only populated when selected; it's at that point
  //       that the user is prompted for their token's PIN
  // PopulatePKCS11CDB();
  
  PopulateDrives();
  if (cbDrive.Items.Count>0) then
    begin
    cbDrive.ItemIndex := 0;

    if (fFreeOTFEObj is TOTFEFreeOTFE) then
      begin
      if (TOTFEFreeOTFE(fFreeOTFEObj).DefaultDriveLetter <> #0) then
        begin
        // Start from 1; skip the default
        for i:=1 to (cbDrive.items.count-1) do
          begin
          currDriveLetter := ansichar(cbDrive.Items[i][1]);
          if (currDriveLetter >= TOTFEFreeOTFE(fFreeOTFEObj).DefaultDriveLetter) then
            begin
            cbDrive.ItemIndex := i;
            break;
            end;
          end;
        end;
      end;
    end;

  PopulateMountAs();
  
  if (fFreeOTFEObj is TOTFEFreeOTFE) then
    begin
    SetMountAs(TOTFEFreeOTFE(fFreeOTFEObj).DefaultMountAs);
    end
  else
    begin
    SetMountAs(fomaFixedDisk);
    end;

  // Certain controls only visble if used in conjunction with drive mounting
  gbMountAs.Visible := fFreeOTFEObj is TOTFEFreeOTFE;
  lblDrive.Visible := fFreeOTFEObj is TOTFEFreeOTFE;
  cbDrive.Visible := fFreeOTFEObj is TOTFEFreeOTFE;

  // If the mount options groupbox isn't visible, widen the volume options
  // groupbox so that there's no blank space to its left
  if not(gbMountAs.visible) then
    begin
    gbVolumeOptions.width := gbVolumeOptions.width + (gbVolumeOptions.left - gbMountAs.left);
    gbVolumeOptions.left := gbMountAs.left;
    end;

  // Default to TRUE to allow formatting under Windows Vista
  ckMountForAllUsers.checked := TRUE;

  EnableDisableControls();

  // Position cursor to the *end* of any password
  preUserKey.SelStart := length(preUserKey.Text);

  if fSilent then     begin
    if AttemptMount() then      begin
      ModalResult := mrOK;
      end
    else
      begin
      ModalResult := mrCancel;
      end;

    FSilentResult := ModalResult;

    PostMessage(Handle, WM_CLOSE, 0, 0);
  end;

  SDUDropFiles_Keyfile.Active := TRUE;
end;

procedure TfrmKeyEntryFreeOTFE.EnableDisableControls();
var
  tmpMountAs: TFreeOTFEMountAs;
begin
  // Ensure we know what to mount as
  ckMountReadonly.Enabled := FALSE;
  tmpMountAs := GetMountAs();
  if not(FreeOTFEMountAsCanWrite[tmpMountAs]) then
    begin
    ckMountReadonly.checked := TRUE;
    end;
  SDUEnableControl(ckMountReadonly, FreeOTFEMountAsCanWrite[tmpMountAs]);

  EnableDisableControls_Keyfile();
  EnableDisableControls_SecretKey();

  pbOK.Enabled := (tmpMountAs <> fomaUnknown) AND
                  (cbDrive.ItemIndex >= 0) AND
                  (seKeyIterations.Value > 0) AND
                  (seSaltLength.Value >= 0);
end;

procedure TfrmKeyEntryFreeOTFE.EnableDisableControls_SecretKey();
begin
  // PKCS#11 secret key controls...
  SDUEnableControl(cbPKCS11SecretKey, (
                                       // Must have more than the "none" item
                                       (cbPKCS11SecretKey.items.count > 1)
                                      ));
end;

procedure TfrmKeyEntryFreeOTFE.EnableDisableControls_Keyfile();
begin
  // We never disable rbKeyfileFile, as keeping it enabled gives the user a
  // visual clue that they can enter a keyfile filename
  // SDUEnableControl(rbKeyfileFile, PKCS11LibraryReady(fFreeOTFEObj.PKCS11Library));
  
  // Protect as this can be called as part of creation
  if (fFreeOTFEObj <> nil) then
    begin
    SDUEnableControl(rbKeyfilePKCS11, PKCS11LibraryReady(fFreeOTFEObj.PKCS11Library));
    if not(PKCS11LibraryReady(fFreeOTFEObj.PKCS11Library)) then
      begin
      rbKeyfileFile.checked := TRUE;
      rbKeyfilePKCS11.checked := FALSE;
      end;
    end;

  // File based keyfile controls...
  SDUEnableControl(feKeyfile, rbKeyfileFile.checked);

  // PKCS#11 based keyfile controls...
  SDUEnableControl(cbPKCS11CDB, (
                                 rbKeyfilePKCS11.checked and
                                 // Must have more than the "none" item
                                 (cbPKCS11CDB.items.count > 1)
                                ));


  // If no keyfile/PKCS#11 CDB is specified, then the CDB must reside within
  // the volume file
  if (
      (
       rbKeyfileFile.checked and
       (feKeyfile.Filename = '')
      ) or
      (
       rbKeyfilePKCS11.checked and
       (cbPKCS11CDB.itemindex = 0) // None available/none selected
      )
     ) then
    begin
    ckOffsetPointsToCDB.Enabled := FALSE;
    ckOffsetPointsToCDB.Checked := TRUE;
    end
  else
    begin
    // If a keyfile is specified, then the user can specify if the volume file
    // includes a CDB
    ckOffsetPointsToCDB.Enabled := TRUE;
    end;

end;

procedure TfrmKeyEntryFreeOTFE.pbAdvancedClick(Sender: TObject);
begin
  DisplayAdvanced(not(pnlAdvanced.visible));
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

procedure TfrmKeyEntryFreeOTFE.DisplayAdvanced(displayAdvanced: boolean);
var
  displayChanged: boolean;
begin
  displayChanged := (pnlAdvanced.visible <> displayAdvanced);
  pnlAdvanced.visible := displayAdvanced;

  if displayChanged then
    begin
    if pnlAdvanced.visible then
      begin
      self.height := self.height + pnlAdvanced.height;

      PopulatePKCS11SecretKey();
      EnableDisableControls_SecretKey();
      end
    else
      begin
      self.height := self.height - pnlAdvanced.height;
      end;

    end;

  if pnlAdvanced.visible then
    begin
    pbAdvanced.caption := '<< '+RS_BUTTON_ADVANCED;
    end
  else
    begin
    pbAdvanced.caption := RS_BUTTON_ADVANCED+' >>';
    end;

end;

procedure TfrmKeyEntryFreeOTFE.SetPassword(password: ansistring);
begin
  preUserKey.text := password;
end;

// "INS" - "If Not Silent"
// Display message, if "Silent" not set to TRUE
function TfrmKeyEntryFreeOTFE.INSMessageDlg(
  Content: string;
  DlgType: TMsgDlgType
): integer;
begin
  Result := mrOk;
  if not(fsilent) then     begin
    Result := SDUMessageDlg(Content, DlgType);
    end;

end;


END.

