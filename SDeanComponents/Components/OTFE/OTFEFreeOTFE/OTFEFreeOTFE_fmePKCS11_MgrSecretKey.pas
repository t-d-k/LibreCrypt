unit OTFEFreeOTFE_fmePKCS11_MgrSecretKey;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  OTFEFreeOTFE_fmePKCS11_MgrBase,
  OTFEFreeOTFE_PKCS11,
  OTFEFreeOTFEBase_U, Menus, ActnList, SDUDialogs;

type
  TfmePKCS11_MgrSecretKey = class(TfmePKCS11_MgrBase)
    Label1: TLabel;
    lvSecretKeys: TListView;
    gbSecretKey: TGroupBox;
    pbNew: TButton;
    pbDelete: TButton;
    gbKeyfile: TGroupBox;
    pbEncrypt: TButton;
    pbDecrypt: TButton;
    actNew: TAction;
    actDelete: TAction;
    actEncrypt: TAction;
    actDecrypt: TAction;
    New1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    Secure1: TMenuItem;
    Desecure1: TMenuItem;
    N2: TMenuItem;
    procedure lvSecretKeysSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actNewExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actEncryptExecute(Sender: TObject);
    procedure actDecryptExecute(Sender: TObject);
  private
    FTokenSecretKeys: TPKCS11SecretKeyPtrArray;

    procedure PopulateSecretKeys();
    procedure ResizeColumns();

    function EncryptDecryptCDB(encryptNotDecrypt: boolean): boolean;
  protected
    procedure Refresh(); override;
  public
    FreeOTFEObj: TOTFEFreeOTFEBase;
    procedure Initialize(); override;
    procedure EnableDisableControls(); override;
  end;

implementation

{$R *.dfm}

uses
  SDUi18n,
  SDUGeneral,
  OTFEFreeOTFE_frmPKCS11NewSecretKey,
  OTFEFreeOTFE_frmSelectVolumeAndOffset,
  Shredder;

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

  lvSecretKeys.ViewStyle := vsReport;
  lvSecretKeys.MultiSelect := TRUE;
  lvSecretKeys.RowSelect := TRUE;
  lvSecretKeys.Readonly := TRUE;

  newCol := lvSecretKeys.Columns.Add();
  newCol.Caption := _('Key');
  newCol.Autosize := TRUE;
  newCol := lvSecretKeys.Columns.Add();
  newCol.Caption := _('Cypher');
  newCol.Autosize := TRUE;
  newCol := lvSecretKeys.Columns.Add();
  newCol.Caption := _('Keysize');
  newCol.Autosize := TRUE;

  PopulateSecretKeys();

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

  SDUEnableControl(actDelete, (lvSecretKeys.SelCount > 0));

  SDUEnableControl(actEncrypt, (lvSecretKeys.SelCount = 1));
  SDUEnableControl(actDecrypt, (lvSecretKeys.SelCount = 1));
end;

procedure TfmePKCS11_MgrSecretKey.Refresh();
begin
  PopulateSecretKeys();
  inherited;
end;

procedure TfmePKCS11_MgrSecretKey.PopulateSecretKeys();
var
  errMsg: string;
  i: integer;
  csrPrev: TCursor;
  item: TListItem;
  strSize: string;
begin
  csrPrev := screen.Cursor;
  screen.Cursor := crHourglass;
  try
    lvSecretKeys.Clear();
    DestroyAndFreeRecord_PKCS11SecretKey(FTokenSecretKeys);

    if not(GetAllPKCS11SecretKey(PKCS11Session, FTokenSecretKeys, errMsg)) then
      begin
      SDUMessageDlg(_('Unable to get list of keys from Token')+SDUCRLF+SDUCRLF+errMsg, mtError);
      end
    else
      begin
      for i:=low(FTokenSecretKeys) to high(FTokenSecretKeys) do
        begin
        item := lvSecretKeys.Items.Insert(lvSecretKeys.Items.count);
        item.Caption := FTokenSecretKeys[i].XLabel;
        item.Data := TObject(FTokenSecretKeys[i]);
        item.subitems.Add(PKCS11_SECRET_KEY_TYPES[FTokenSecretKeys[i].XType].Name);

        strSize := '';
        // >0 because ePass2000 tokens return 0 for DES/3DES keys(!)
        if (FTokenSecretKeys[i].Size > 0) then
          begin
          strSize := inttostr(FTokenSecretKeys[i].Size);
          end;
        item.subitems.Add(strSize);
        end;

      end;

  finally
    screen.Cursor := csrPrev;
  end;

  ResizeColumns();
end;

procedure TfmePKCS11_MgrSecretKey.actDeleteExecute(Sender: TObject);
var
  i: integer;
  errMsg: string;
  currObj: PPKCS11SecretKey;
  opOK: boolean;
  csrPrev: TCursor;
  stlDeletedOK: TStringList;
  stlDeletedFail: TStringList;
  msg: string;
  msgType: TMsgDlgType;
  msgList: string;
begin
  inherited;

  if SDUConfirmYN(_('Are you sure you wish to delete the selected keys from the token?')) then
    begin
    stlDeletedOK := TStringList.Create();
    stlDeletedFail := TStringList.Create();
    try
      for i:=0 to (lvSecretKeys.items.count - 1) do
        begin
        if lvSecretKeys.items[i].selected then
          begin
          currObj := PPKCS11SecretKey(lvSecretKeys.items[i].Data);

          csrPrev := screen.Cursor;
          screen.Cursor := crHourglass;
          try
            opOK := DestroyPKCS11SecretKey(FPKCS11Session, currObj, errMsg)
          finally
            screen.Cursor:= csrPrev;
          end;

          if opOK then
            begin
            stlDeletedOK.Add(currObj.XLabel);
            end
          else
            begin
            stlDeletedOK.Add(currObj.XLabel+' ('+errMsg+')');
            end;
          end;
        end;

        msg := '';
        msgType := mtInformation;
        if (stlDeletedOK.count > 0) then
          begin
          msgList := '';
          for i:=0 to (stlDeletedOK.count - 1) do
            begin
            msgList := msgList + '  ' + stlDeletedOK[i] + SDUCRLF;
            end;
          msg := SDUParamSubstitute(_('The following keys were deleted successfully:'+SDUCRLF+
                              SDUCRLF+
                              '%1'),
                              [msgList]
                             );
          end;
        if (stlDeletedFail.count > 0) then
          begin
          msgType := mtWarning;
          msgList := '';
          for i:=0 to (stlDeletedFail.count - 1) do
            begin
            msgList := msgList + '  ' + stlDeletedFail[i] + SDUCRLF;
            end;
          msg := msg + SDUCRLF;
          msg := msg + SDUParamSubstitute(_('The following keys could not be deleted:'+SDUCRLF+
                              SDUCRLF+
                              '%1'),
                              [msgList]
                             );
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
  EncryptDecryptCDB(TRUE);

end;

procedure TfmePKCS11_MgrSecretKey.actDecryptExecute(Sender: TObject);
begin
  inherited;
  EncryptDecryptCDB(FALSE);

end;

procedure TfmePKCS11_MgrSecretKey.actNewExecute(Sender: TObject);
var
  dlg: TfrmPKCS11NewSecretKey;
  i: integer;
begin
  inherited;

  dlg := TfrmPKCS11NewSecretKey.Create(nil);
  try
    // Give the dialog a list of all existing key names, so the user doens't
    // try to duplicate an existing one - could cause confusion when mounting
    dlg.ExistingKeys.Clear();
    for i:=0 to (lvSecretKeys.items.count - 1) do
      begin
      dlg.ExistingKeys.Add(lvSecretKeys.items[i].Caption);
      end;

    dlg.PKCS11Session := PKCS11Session;
    if (dlg.ShowModal() = mrOK) then
      begin
      Refresh();
      end;

  finally
    dlg.Free();
  end;

end;

function TfmePKCS11_MgrSecretKey.EncryptDecryptCDB(encryptNotDecrypt: boolean): boolean;
var
  cdbBefore: string;
  cdbAfter: string;
  allOK: boolean;
  errMsg: string;
  fileToSecure: string;
  dlg: TfrmSelectVolumeFileAndOffset;
  offset: int64;
  SecretKey: PPKCS11SecretKey;
  i: integer;
  secureUnsecure: string;
begin
  allOK := FALSE;

  SecretKey := nil;

  for i:=0 to (lvSecretKeys.items.count - 1) do
    begin
    if lvSecretKeys.items[i].selected then
      begin
      SecretKey := PPKCS11SecretKey(lvSecretKeys.items[i].Data);
      break;
      end;
    end;

  if (SecretKey = nil) then
    begin
    SDUMessageDlg(_('Please select a secret key to use'), mtInformation);
    Result := FALSE;
    exit;
    end;

  // Get file/partition to secure with secret key and any offset
  dlg:= TfrmSelectVolumeFileAndOffset.Create(nil);
  try
    dlg.OTFEFreeOTFE := FreeOTFEObj;
    dlg.SetDlgPurpose(encryptNotDecrypt);
    
    if (dlg.ShowModal = mrOK) then
      begin
      allOK := TRUE;

      fileToSecure := dlg.Volume;
      offset:= dlg.Offset;

      if allOK then
        begin
        secureUnsecure := _('unsecure');
        if encryptNotDecrypt then
          begin
          secureUnsecure := _('secure');
          end;

        if (offset <> 0) then
          begin
          allOK := SDUConfirmYN(
                                SDUParamSubstitute(_('Are you sure you wish to use PKCS#11 secret key:'+SDUCRLF+
                                        SDUCRLF+
                                        '%1'+SDUCRLF+
                                        SDUCRLF+
                                        'to %2 the CDB starting from offset %3 within volume:'+SDUCRLF+
                                        SDUCRLF+
                                        '%4'),
                                        [SecretKey.XLabel, secureUnsecure, offset, fileToSecure])
                               );
          end
        else
          begin
          allOK := SDUConfirmYN(
                                SDUParamSubstitute(_('Are you sure you wish to %1 the volume/keyfile:'+SDUCRLF+
                                        SDUCRLF+
                                        '%2'+SDUCRLF+
                                        SDUCRLF+
                                        'using the PKCS#11 secret key:'+SDUCRLF+
                                        SDUCRLF+
                                        '%3'),
                                        [secureUnsecure, fileToSecure, SecretKey.XLabel])
                               );
          end;
        end;

      // Read CDB...
      if allOK then
        begin
        if not(FreeOTFEObj.ReadRawVolumeCriticalData(
                                                 fileToSecure,
                                                 offset,
                                                 cdbBefore
                                                )) then
          begin
          SDUMessageDlg(_('Unable to read keyfile/volume CDB'), mtError);
          allOK := FALSE;
          end;
        end;

      // ...Encrypt/decrypt CDB using token and token key...
      if allOK then
        begin
        if encryptNotDecrypt then
          begin
          allOK := PKCS11EncryptCDBWithSecretKey(
                                                 PKCS11Session,
                                                 SecretKey,
                                                 cdbBefore,
                                                 cdbAfter,
                                                 errMsg
                                                );
          end
        else
          begin
          allOK := PKCS11DecryptCDBWithSecretKey(
                                                 PKCS11Session,
                                                 SecretKey,
                                                 cdbBefore,
                                                 cdbAfter,
                                                 errMsg
                                                );
          end;

        if not(allOK) then
          begin
          SDUMessageDlg(_('Unable to carry out encryption/decryption using Token')+SDUCRLF+SDUCRLF+errMsg, mtError);
          end;
        end;

      // ...Write CDB.
      if allOK then
        begin
        if not(FreeOTFEObj.WriteRawVolumeCriticalData(
                                                 fileToSecure,
                                                 offset,
                                                 cdbAfter
                                                )) then
          begin
          SDUMessageDlg(_('Unable to write keyfile/volume CDB'), mtError);
          allOK := FALSE;
          end;
        end;

      Overwrite(cdbBefore);
      Overwrite(cdbAfter);
      end;

  finally
    dlg.Free();
  end;

  if allOK then
    begin
    SDUMessageDlg(SDUParamSubstitute(_('Volume/keyfile now %1'), [secureUnsecure]), mtInformation);
    end;

  Result := allOK;
end;

procedure TfmePKCS11_MgrSecretKey.ResizeColumns();
const
  // Resize the columns such that they're as wide as the widest item/subitem
  // text
  RESIZE_EXCL_HEADER = -1;
  // Resize the columns such that they're as wide as the column header text/the
  // widest item/subitem
  RESIZE_INCL_HEADER   = -2;
var
  i: integer;
  prevAutoSize: boolean;
begin
  for i:=0 to (lvSecretKeys.columns.count -1) do
    begin
    prevAutoSize := lvSecretKeys.column[i].AutoSize;
    lvSecretKeys.column[i].AutoSize := TRUE;
    lvSecretKeys.column[i].width := RESIZE_INCL_HEADER;
    // Revert AutoSize...
    lvSecretKeys.column[i].AutoSize := prevAutoSize;
    end;
end;

END.

