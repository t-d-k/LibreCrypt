unit CommonfrmCDBDump_LUKS;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  Buttons, Classes, ComCtrls,
  CommonfrmCDBDump_Base, Controls, Dialogs,
  Forms, Graphics, Messages, OTFEFreeOTFE_LUKSKeyOrKeyfileEntry, OTFEFreeOTFE_VolumeSelect,
  OTFEFreeOTFEBase_U, PasswordRichEdit, lcDialogs, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUSpin64Units, Spin64,
  StdCtrls, SysUtils, Windows;

type
  TfrmCDBDump_LUKS = class (TfrmCDBDump_Base)
    lblOptional:                        TLabel;
    ckBaseIVCypherOnHashLength:         TCheckBox;
    OTFEFreeOTFELUKSKeyOrKeyfileEntry1: TOTFEFreeOTFELUKSKeyOrKeyfileEntry;
    procedure FormCreate(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    function GetLUKSBaseIVCypherOnHashLength(): Boolean;

  PROTECTED
    procedure EnableDisableControls(); OVERRIDE;

    function DumpLUKSDataToFile(): Boolean; OVERRIDE;

  PUBLIC
    property LUKSBaseIVCypherOnHashLength: Boolean Read GetLUKSBaseIVCypherOnHashLength;
  end;


implementation

{$R *.DFM}

uses
  SDUGeneral, SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


procedure TfrmCDBDump_LUKS.FormCreate(Sender: TObject);
begin
  inherited;

  self.Caption := _('Dump LUKS Details');

end;

procedure TfrmCDBDump_LUKS.FormShow(Sender: TObject);
begin
  inherited;

//  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.FreeOTFEObj := OTFEFreeOTFE;
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.Initialize();
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.DefaultOptions();

  ckBaseIVCypherOnHashLength.Checked := True;

end;

procedure TfrmCDBDump_LUKS.EnableDisableControls();
begin
  pbOK.Enabled := ((VolumeFilename <> '') and (feDumpFilename.Filename <> '') and
    (feDumpFilename.Filename <> VolumeFilename) // Don't overwrite the volume with the dump!!!
    );
end;

procedure TfrmCDBDump_LUKS.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();
end;

function TfrmCDBDump_LUKS.DumpLUKSDataToFile(): Boolean;
var
  userKey:            TSDUBytes;
  keyfile:            String;
  keyfileIsASCII:     Boolean;
  keyfileNewlineType: TSDUNewline;
begin
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKey(userKey);
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyfile(keyfile);
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyfileIsASCII(keyfileIsASCII);
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyfileNewlineType(keyfileNewlineType);

  Result := GetFreeOTFEBase().DumpLUKSDataToFile(VolumeFilename, userKey,
    keyfile, keyfileIsASCII, keyfileNewlineType, LUKSBaseIVCypherOnHashLength,
    DumpFilename);
end;

procedure TfrmCDBDump_LUKS.pbOKClick(Sender: TObject);
var
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
  startTime: TDateTime;
  stopTime: TDateTime;
  diffTime: TDateTime;
  Hour, Min, Sec, MSec: Word;
{$ENDIF}
  dumpOK:             Boolean;
  notepadCommandLine: String;
begin
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
  startTime := Now();
{$ENDIF}

  dumpOK := DumpLUKSDataToFile();

  if dumpOK then begin
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
    stopTime := Now();
    diffTime := (stopTime - startTime);
    DecodeTime(diffTime, Hour, Min, Sec, MSec);
    showmessage('Time taken to dump CDB: '+inttostr(Hour)+' hours, '+inttostr(Min)+' mins, '+inttostr(Sec)+'.'+inttostr(MSec)+' secs');
{$ENDIF}

    if (SDUMessageDlg(_(
      'A human readable copy of your critical data block has been written to:') +
      SDUCRLF + SDUCRLF + DumpFilename + SDUCRLF + SDUCRLF +
      _('Do you wish to open this file in Windows Notepad?'), mtInformation,
      [mbYes, mbNo], 0) = mrYes) then begin
      notepadCommandLine := 'notepad ' + DumpFilename;

      if not (SDUWinExecNoWait32(notepadCommandLine, SW_RESTORE)) then begin
        SDUMessageDlg(_('Error running Notepad'), mtError, [], 0);
      end;

    end;

    ModalResult := mrOk;
  end else begin
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
    stopTime := Now();
    diffTime := (stopTime - startTime);
    DecodeTime(diffTime, Hour, Min, Sec, MSec);
    showmessage('Time taken to FAIL to dump CDB: '+inttostr(Hour)+' hours, '+inttostr(Min)+' mins, '+inttostr(Sec)+'.'+inttostr(MSec)+' secs');
{$ENDIF}

    SDUMessageDlg(
      _('Unable to dump out critical data block.') + SDUCRLF + SDUCRLF +
      _('Please ensure that your password and details are entered correctly, and that this file is not currently in use (e.g. mounted)'),
      mtError, [mbOK], 0);
  end;

end;

function TfrmCDBDump_LUKS.GetLUKSBaseIVCypherOnHashLength(): Boolean;
begin
  Result := ckBaseIVCypherOnHashLength.Checked;
end;

end.
