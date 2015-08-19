unit fmePKCS11_MgrKeyfile;

interface

uses
  ActnList, Classes, Controls, Dialogs, Forms,
  Graphics, Menus, Messages, fmePKCS11_MgrBase,
  PKCS11Lib, pkcs11_session,
  lcDialogs, StdCtrls,
  SysUtils, Variants, Windows, SDUDialogs;

type
  TfmePKCS11_MgrKeyfile = class (TfmePKCS11Mgr)
    Label1:     TLabel;
    pbImport:   TButton;
    pbDelete:   TButton;
    pbExport:   TButton;
    lbCDB:      TListBox;
    OpenDialog: TSDUOpenDialog;
    SaveDialog: TSDUSaveDialog;
    actImport:  TAction;
    actExport:  TAction;
    actDelete:  TAction;
    N1:         TMenuItem;
    Import1:    TMenuItem;
    Export1:    TMenuItem;
    Delete1:    TMenuItem;
    N2:         TMenuItem;
    procedure lbCDBClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure actImportExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
  PRIVATE
    FTokenCDBs: TPKCS11CDBPtrArray;

    procedure PopulateCDB();
  PROTECTED
    procedure Refresh(); OVERRIDE;
  PUBLIC
    constructor Create(AOwner: TComponent); OVERRIDE;
    destructor Destroy(); OVERRIDE;
    procedure Initialize(); OVERRIDE;
    procedure EnableDisableControls(); OVERRIDE;
  end;

implementation

{$R *.dfm}

uses
//lc utils
lcConsts,
  DriverAPI,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  SDUGeneral,
  SDUi18n,
  Shredder;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

procedure TfmePKCS11_MgrKeyfile.actDeleteExecute(Sender: TObject);
var
  i:              Integer;
  errMsg:         String;
  currObj:        PPKCS11CDB;
  opOK:           Boolean;
  csrPrev:        TCursor;
  stlDeletedOK:   TStringList;
  stlDeletedFail: TStringList;
  msg:            String;
  msgType:        TMsgDlgType;
  msgList:        String;
begin
  inherited;

  if SDUConfirmYN(_('Are you sure you wish to delete the selected keyfiles from the token?')) then
  begin
    stlDeletedOK   := TStringList.Create();
    stlDeletedFail := TStringList.Create();
    try
      for i := 0 to (lbCDB.items.Count - 1) do begin
        if lbCDB.Selected[i] then begin
          currObj := PPKCS11CDB(lbCDB.items.Objects[i]);

          csrPrev       := screen.Cursor;
          screen.Cursor := crHourglass;
          try
            opOK          := DestroyPKCS11CDB(FPKCS11Session, currObj, errMsg)
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
        msg := Format(_('The following keyfiles were deleted successfully:' + SDUCRLF +
          SDUCRLF + '%s'),
          [msgList]);
      end;
      if (stlDeletedFail.Count > 0) then begin
        msgType := mtWarning;
        msgList := '';
        for i := 0 to (stlDeletedFail.Count - 1) do begin
          msgList := msgList + '  ' + stlDeletedFail[i] + SDUCRLF;
        end;
        msg := msg + SDUCRLF;
        msg := msg + Format(_('The following keyfiles could not be deleted:' + SDUCRLF +
          SDUCRLF + '%s'),
          [msgList]);
      end;

      SDUMessageDlg(msg, msgType);

    finally
      stlDeletedOK.Free();
      stlDeletedFail.Free();
    end;

    Refresh();
  end;

end;

procedure TfmePKCS11_MgrKeyfile.actExportExecute(Sender: TObject);
var
  CDBRecord: PPKCS11CDB;
begin
  SaveDialog.Filter     := FILE_FILTER_FLT_KEYFILES;
  SaveDialog.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  SaveDialog.Options    := SaveDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(SaveDialog, lbCDB.Items[lbCDB.ItemIndex]);
  if SaveDialog.Execute then begin
    CDBRecord := PPKCS11CDB(lbCDB.items.Objects[lbCDB.ItemIndex]);

    if SDUSetFileContent(SaveDialog.Filename,SDUStringToSDUBytes(CDBRecord.CDB)) then begin
      SDUMessageDlg(_('Keyfile exported successfully'));
    end else begin
      SDUMessageDlg(
        Format(_('Unable to export keyfile to:' + SDUCRLF +
        SDUCRLF + '%s'),
        [SaveDialog.Filename])
        );
    end;
  end;

end;

procedure TfmePKCS11_MgrKeyfile.actImportExecute(Sender: TObject);
var
  importFile:   String;
  newLabel:     String;
  newCDB:       Ansistring;
  errMsg:       String;
  labelValid:   Boolean;
  testLabel:    String;
  cntTestLabel: Integer;
  csrPrev:      TCursor;
  opOK:         Boolean;
begin
  OpenDialog.Filter     := FILE_FILTER_FLT_KEYFILES;
  OpenDialog.DefaultExt := FILE_FILTER_DFLT_KEYFILES;
  OpenDialog.Options    := OpenDialog.Options + [ofDontAddToRecent];

  SDUOpenSaveDialogSetup(OpenDialog, '');
  if OpenDialog.Execute then begin
    importFile := OpenDialog.filename;
    if not (SDUGetFileContent(importFile, newCDB)) then begin
      SDUMessageDlg(_('Unable to read keyfile contents'), mtError);
    end else begin
      if (length(newCDB) <> (CRITICAL_DATA_LENGTH div 8)) then begin
        // Sanity check...
        if not (SDUConfirmYN(_(
          'The file specified doesn''t appear to be a FreeOTFE keyfile.') + SDUCRLF +
          SDUCRLF + _('Do you wish to continue anyway?'))) then begin
          exit;
        end;
      end;

      newLabel := ExtractFilename(importFile);
      newLabel := Copy(newLabel, 1, (length(newLabel) - length(ExtractFileExt(newLabel))));
      newLabel := trim(newLabel);

      // Just some sanity checking - this ISN'T ACTUALLY NEEDED, but we force
      // all CDB labels on the token to be unique.
      // Otherwise, the user could end up mounting a volume with the wrong CDB
      cntTestLabel := 0;
      repeat
        Inc(cntTestLabel);
        testLabel := newLabel;
        if (cntTestLabel > 1) then begin
          testLabel := newLabel + ' (' + IntToStr(cntTestLabel) + ')';
        end;

        labelValid := (lbCDB.items.IndexOf(testLabel) < 0);

        if labelValid then begin
          newLabel := testLabel;
        end;
      until labelValid;

      csrPrev       := screen.Cursor;
      screen.Cursor := crHourglass;
      try
        opOK := CreatePKCS11CDB(FPKCS11Session, newLabel, newCDB, errMsg);
      finally
        screen.Cursor := csrPrev;
      end;

      if opOK then begin
        SDUMessageDlg(
          _('Keyfile imported successfully as:') + SDUCRLF +
          SDUCRLF + '  ' + newLabel,
          mtInformation
          );

        Refresh();
      end else begin
        SDUMessageDlg(
          _('Keyfile import failed') + SDUCRLF + SDUCRLF +
          errMsg,
          mtError
          );
      end;
    end;

  end;

end;

constructor TfmePKCS11_MgrKeyfile.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TfmePKCS11_MgrKeyfile.Destroy();
begin
  // Purge stored CDBs...
  DestroyAndFreeRecord_PKCS11CDB(FTokenCDBs);

  inherited;
end;


procedure TfmePKCS11_MgrKeyfile.lbCDBClick(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfmePKCS11_MgrKeyfile.Initialize();
begin
  lbCDB.MultiSelect := True;
  lbCDB.Sorted      := True;

  PopulateCDB();

  EnableDisableControls();

end;

procedure TfmePKCS11_MgrKeyfile.EnableDisableControls();
begin
  inherited;

  actExport.Enabled := (lbCDB.SelCount = 1);
  actDelete.Enabled := (lbCDB.SelCount > 0);

end;

procedure TfmePKCS11_MgrKeyfile.FrameResize(Sender: TObject);
begin
  inherited;
  SDUCenterControl(pbExport, ccHorizontal);
end;

procedure TfmePKCS11_MgrKeyfile.Refresh();
begin
  PopulateCDB();
  inherited;
end;

procedure TfmePKCS11_MgrKeyfile.PopulateCDB();
var
  errMsg:     String;
  i:          Integer;
  warnBadCDB: Boolean;
  csrPrev:    TCursor;
begin
  csrPrev       := screen.Cursor;
  screen.Cursor := crHourglass;
  try
    lbCDB.Clear();
    DestroyAndFreeRecord_PKCS11CDB(FTokenCDBs);

    warnBadCDB := False;
    if not (GetAllPKCS11CDB(PKCS11Session, FTokenCDBs, errMsg)) then begin
      SDUMessageDlg(_('Unable to get list of CDB entries from Token') + SDUCRLF +
        SDUCRLF + errMsg, mtError);
    end else begin
      for i := low(FTokenCDBs) to high(FTokenCDBs) do begin
        lbCDB.items.AddObject(FTokenCDBs[i].XLabel, TObject(FTokenCDBs[i]));

        // Sanity check - the CDBs stored are sensible, right?
        if (length(FTokenCDBs[i].CDB) <> (CRITICAL_DATA_LENGTH div 8)) then begin
          warnBadCDB := True;
        end;
      end;

    end;

  finally
    screen.Cursor := csrPrev;
  end;

  if warnBadCDB then begin
    SDUMessageDlg(
      _('One or more of the keyfiles stored on your token are invalid/corrupt'),
      mtWarning
      );
  end;

end;


end.
