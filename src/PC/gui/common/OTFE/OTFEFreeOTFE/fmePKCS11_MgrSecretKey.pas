unit fmePKCS11_MgrSecretKey;

interface

uses
  ActnList, Classes, ComCtrls,
  Controls, Dialogs, Forms,
  Graphics, lcDialogs, Menus, Messages, fmePKCS11_MgrBase,
  PKCS11Lib,
  OTFEFreeOTFEBase_U, StdCtrls, SysUtils, Variants, Windows;

type
  TfmePKCS11_MgrSecretKey = class (TfmePKCS11Mgr)
    Label1:       TLabel;
    lvSecretKeys: TListView;
    gbSecretKey:  TGroupBox;
    pbNew:        TButton;
    pbDelete:     TButton;
    gbKeyfile:    TGroupBox;
    pbEncrypt:    TButton;
    pbDecrypt:    TButton;
    actNew:       TAction;
    actDelete:    TAction;
    actEncrypt:   TAction;
    actDecrypt:   TAction;
    New1:         TMenuItem;
    Delete1:      TMenuItem;
    N1:           TMenuItem;
    Secure1:      TMenuItem;
    Desecure1:    TMenuItem;
    N2:           TMenuItem;
    procedure lvSecretKeysSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure actNewExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actEncryptExecute(Sender: TObject);
    procedure actDecryptExecute(Sender: TObject);
  private
    FTokenSecretKeys: TPKCS11SecretKeyPtrArray;

    procedure _PopulateSecretKeys();
    procedure _ResizeColumns();

    function _EncryptDecryptCDB(encryptNotDecrypt: Boolean): Boolean;
  protected
    procedure Refresh(); override;
  public
    procedure Initialize(); override;
    procedure EnableDisableControls(); override;
  end;

implementation

{$R *.dfm}

uses
//lc utils
  frmPKCS11NewSecretKey,
  frmSelectVolumeAndOffset,
  SDUGeneral,
  SDUi18n,
  Shredder,lcConsts;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

procedure TfmePKCS11_MgrSecretKey.Initialize();
var
  newCol: TListColumn;
begin
  inherited;

  lvSecretKeys.ViewStyle   := vsReport;
  lvSecretKeys.MultiSelect := True;
  lvSecretKeys.RowSelect   := True;
  lvSecretKeys.ReadOnly    := True;

  newCol          := lvSecretKeys.Columns.Add();
  newCol.Caption  := _('Key');
  newCol.Autosize := True;
  newCol          := lvSecretKeys.Columns.Add();
  newCol.Caption  := _('Cypher');
  newCol.Autosize := True;
  newCol          := lvSecretKeys.Columns.Add();
  newCol.Caption  := _('Keysize');
  newCol.Autosize := True;

  _PopulateSecretKeys();

  EnableDisableControls();

end;

procedure TfmePKCS11_MgrSecretKey.lvSecretKeysSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  inherited;

  EnableDisableControls();
end;

procedure TfmePKCS11_MgrSecretKey.EnableDisableControls();
begin
  inherited;

  actDelete.Enabled  := lvSecretKeys.SelCount > 0;
  actEncrypt.Enabled := (lvSecretKeys.SelCount = 1);
  actDecrypt.Enabled := (lvSecretKeys.SelCount = 1);
end;

procedure TfmePKCS11_MgrSecretKey.Refresh();
begin
  _PopulateSecretKeys();
  inherited;
end;

procedure TfmePKCS11_MgrSecretKey._PopulateSecretKeys();
var
  errMsg:  String;
  i:       Integer;
  csrPrev: TCursor;
  item:    TListItem;
  strSize: String;
begin
  csrPrev       := screen.Cursor;
  screen.Cursor := crHourglass;
  try
    lvSecretKeys.Clear();
    DestroyAndFreeRecord_PKCS11SecretKey(FTokenSecretKeys);

    if not (GetAllPKCS11SecretKey(PKCS11Session, FTokenSecretKeys, errMsg)) then begin
      SDUMessageDlg(_('Unable to get list of keys from Token') + SDUCRLF +
        SDUCRLF + errMsg, mtError);
    end else begin
      for i := low(FTokenSecretKeys) to high(FTokenSecretKeys) do begin
        item         := lvSecretKeys.Items.Insert(lvSecretKeys.Items.Count);
        item.Caption := FTokenSecretKeys[i].XLabel;
        item.Data    := TObject(FTokenSecretKeys[i]);
        item.subitems.Add(PKCS11_SECRET_KEY_TYPES[FTokenSecretKeys[i].XType].Name);

        strSize := '';
        // >0 because ePass2000 tokens return 0 for DES/3DES keys(!)
        if (FTokenSecretKeys[i].Size > 0) then begin
          strSize := IntToStr(FTokenSecretKeys[i].Size);
        end;
        item.subitems.Add(strSize);
      end;

    end;

  finally
    screen.Cursor := csrPrev;
  end;

  _ResizeColumns();
end;

procedure TfmePKCS11_MgrSecretKey.actDeleteExecute(Sender: TObject);
var
  i:              Integer;
  errMsg:         String;
  currObj:        PPKCS11SecretKey;
  opOK:           Boolean;
  csrPrev:        TCursor;
  stlDeletedOK:   TStringList;
  stlDeletedFail: TStringList;
  msg:            String;
  msgType:        TMsgDlgType;
  msgList:        String;
begin
  inherited;

  if SDUConfirmYN(_('Are you sure you wish to delete the selected keys from the token?')) then
  begin
    stlDeletedOK   := TStringList.Create();
    stlDeletedFail := TStringList.Create();
    try
      for i := 0 to (lvSecretKeys.items.Count - 1) do begin
        if lvSecretKeys.items[i].selected then begin
          currObj := PPKCS11SecretKey(lvSecretKeys.items[i].Data);

          csrPrev       := screen.Cursor;
          screen.Cursor := crHourglass;
          try
            opOK          := DestroyPKCS11SecretKey(FPKCS11Session, currObj, errMsg)
          finally
            screen.Cursor := csrPrev;
          end;

          if opOK then begin
            stlDeletedOK.Add(currObj.XLabel);
          end else begin
            stlDeletedOK.Add(currObj.XLabel + ' (' + errMsg + ')');
          end;
        end;
      end;

      msg     := '';
      msgType := mtInformation;
      if (stlDeletedOK.Count > 0) then begin
        msgList := '';
        for i := 0 to (stlDeletedOK.Count - 1) do begin
          msgList := msgList + '  ' + stlDeletedOK[i] + SDUCRLF;
        end;
        msg := Format(_('The following keys were deleted successfully:' +
          SDUCRLF + SDUCRLF + '%s'), [msgList]);
      end;
      if (stlDeletedFail.Count > 0) then begin
        msgType := mtWarning;
        msgList := '';
        for i := 0 to (stlDeletedFail.Count - 1) do begin
          msgList := msgList + '  ' + stlDeletedFail[i] + SDUCRLF;
        end;
        msg := msg + SDUCRLF;
        msg := msg + Format(_('The following keys could not be deleted:' +
          SDUCRLF + SDUCRLF + '%s'), [msgList]);
      end;

      SDUMessageDlg(msg, msgType);

    finally
      stlDeletedOK.Free();
      stlDeletedFail.Free();
    end;

    Refresh();
  end;

end;

procedure TfmePKCS11_MgrSecretKey.actEncryptExecute(Sender: TObject);
begin
  inherited;
  _EncryptDecryptCDB(True);

end;

procedure TfmePKCS11_MgrSecretKey.actDecryptExecute(Sender: TObject);
begin
  inherited;
  _EncryptDecryptCDB(False);

end;

procedure TfmePKCS11_MgrSecretKey.actNewExecute(Sender: TObject);
var
  dlg: TfrmPKCS11NewSecretKey;
  i:   Integer;
begin
  inherited;

  dlg := TfrmPKCS11NewSecretKey.Create(nil);
  try
    // Give the dialog a list of all existing key names, so the user doens't
    // try to duplicate an existing one - could cause confusion when mounting
    dlg.ExistingKeys.Clear();
    for i := 0 to (lvSecretKeys.items.Count - 1) do begin
      dlg.ExistingKeys.Add(lvSecretKeys.items[i].Caption);
    end;

    dlg.PKCS11Session := PKCS11Session;
    if (dlg.ShowModal() = mrOk) then begin
      Refresh();
    end;

  finally
    dlg.Free();
  end;

end;

function TfmePKCS11_MgrSecretKey._EncryptDecryptCDB(encryptNotDecrypt: Boolean): Boolean;
var
  cdbBefore:      Ansistring;
  cdbAfter:       Ansistring;
  errMsg:         String;
  fileToSecure:   String;
  dlg:            TfrmSelectVolumeFileAndOffset;
  offset:         Int64;
  SecretKey:      PPKCS11SecretKey;
  i:              Integer;
  secureUnsecure: String;
begin
  Result := False;

  SecretKey := nil;

  for i := 0 to (lvSecretKeys.items.Count - 1) do begin
    if lvSecretKeys.items[i].selected then begin
      SecretKey := PPKCS11SecretKey(lvSecretKeys.items[i].Data);
      break;
    end;
  end;

  if (SecretKey = nil) then begin
    SDUMessageDlg(_('Please select a secret key to use'), mtInformation);
    Result := False;
    exit;
  end;

  // Get file/partition to secure with secret key and any offset
  dlg := TfrmSelectVolumeFileAndOffset.Create(nil);
  try
    //    dlg.OTFEFreeOTFE := FreeOTFEObj;
    dlg.SetDlgPurpose(encryptNotDecrypt);

    if (dlg.ShowModal = mrOk) then begin
      Result := True;

      fileToSecure := dlg.Volume;
      offset       := dlg.Offset;

      if Result then begin
        secureUnsecure := _('unsecure');
        if encryptNotDecrypt then begin
          secureUnsecure := _('secure');
        end;

        if (offset <> 0) then begin
          Result := SDUConfirmYN(Format(
            _('Are you sure you wish to use PKCS#11 secret key:' + SDUCRLF +
            SDUCRLF + '%s' + SDUCRLF + SDUCRLF +
            'to %s the header starting from offset %d within container:' +
            SDUCRLF + SDUCRLF + '%s'), [SecretKey.XLabel, secureUnsecure,
            offset, fileToSecure]));
        end else begin
          Result := SDUConfirmYN(Format(
            _('Are you sure you wish to %s the container/keyfile:' + SDUCRLF +
            SDUCRLF + '%s' + SDUCRLF + SDUCRLF +
            'using the PKCS#11 secret key:' + SDUCRLF + SDUCRLF + '%s'),
            [secureUnsecure, fileToSecure, SecretKey.XLabel]));
        end;
      end;

      // Read CDB...
      if Result then begin
        if not (GetFreeOTFEBase().ReadRawVolumeCriticalData(
          fileToSecure, offset, cdbBefore)) then begin
          SDUMessageDlg(_('Unable to read keyfile/container header'), mtError);
          Result := False;
        end;
      end;

      // ...Encrypt/decrypt CDB using token and token key...
      if Result then begin
        if encryptNotDecrypt then begin
          Result := PKCS11EncryptCDBWithSecretKey(PKCS11Session,
            SecretKey, cdbBefore, cdbAfter, errMsg);
        end else begin
          Result := PKCS11DecryptCDBWithSecretKey(PKCS11Session,
            SecretKey, cdbBefore, cdbAfter, errMsg);
        end;

        if not (Result) then begin
          SDUMessageDlg(_('Unable to carry out encryption/decryption using Token') +
            SDUCRLF + SDUCRLF + errMsg, mtError);
        end;
      end;

      // ...Write CDB.
      if Result then begin
        if not (GetFreeOTFEBase().WriteRawVolumeCriticalData(
          fileToSecure, offset, cdbAfter)) then begin
          SDUMessageDlg(_('Unable to write keyfile/container CDB'), mtError);
          Result := False;
        end;
      end;

      Overwrite(cdbBefore);
      Overwrite(cdbAfter);
    end;

  finally
    dlg.Free();
  end;

  if Result then begin
    SDUMessageDlg(Format(_('Container/keyfile now %s'), [secureUnsecure]), mtInformation);
  end;

end;

procedure TfmePKCS11_MgrSecretKey._ResizeColumns();
const
  // Resize the columns such that they're as wide as the widest item/subitem
  // text
  RESIZE_EXCL_HEADER = -1;
  // Resize the columns such that they're as wide as the column header text/the
  // widest item/subitem
  RESIZE_INCL_HEADER = -2;
var
  i:            Integer;
  prevAutoSize: Boolean;
begin
  for i := 0 to (lvSecretKeys.columns.Count - 1) do begin
    prevAutoSize                    := lvSecretKeys.column[i].AutoSize;
    lvSecretKeys.column[i].AutoSize := True;
    lvSecretKeys.column[i].Width    := RESIZE_INCL_HEADER;
    // Revert AutoSize...
    lvSecretKeys.column[i].AutoSize := prevAutoSize;
  end;
end;

end.
