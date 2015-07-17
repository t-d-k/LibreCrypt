unit frmLUKSHdrDump;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
 // delphi
  Buttons, Classes, ComCtrls,
  Controls, Dialogs,
  Windows,       Messages,Graphics,  Forms, StdCtrls, SysUtils,
    //sdu, lcutils
  PasswordRichEdit, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUSpin64Units, Spin64,
   lcDialogs,
   // librecrypt
   OTFEFreeOTFEBase_U,fmeLUKSKeyOrKeyfileEntry, fmeVolumeSelect,
   frmHdrDump;

type
  TfrmLUKSHdrDump = class (TfrmHdrDump)
    frmeLUKSKeyOrKeyfileEntry1: TfrmeLUKSKeyOrKeyfileEntry;
    procedure FormCreate(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetLUKSBaseIVCypherOnHashLength(): Boolean;

  protected
    procedure EnableDisableControls(); override;
    function DumpLUKSDataToFile(): Boolean; override;
  public

  end;


implementation

{$R *.DFM}

uses
  //sdu, lcutils
  SDUGeneral, SDUi18n
  // librecrypt
  ,LUKSTools ;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


procedure TfrmLUKSHdrDump.FormCreate(Sender: TObject);
begin
  inherited;
  self.Caption := _('Dump LUKS Details');
end;

procedure TfrmLUKSHdrDump.FormShow(Sender: TObject);
begin
  inherited;

  frmeLUKSKeyOrKeyfileEntry1.Initialize();
  frmeLUKSKeyOrKeyfileEntry1.DefaultOptions();
  frmeLUKSKeyOrKeyfileEntry1.baseIVCypherOnHashLength := True;
end;

procedure TfrmLUKSHdrDump.EnableDisableControls();
begin
  pbOK.Enabled := ((GetVolumeFilename <> '') and (feDumpFilename.Filename <> '') and
    (feDumpFilename.Filename <> GetVolumeFilename) // Don't overwrite the volume with the dump!!!
    );
end;

procedure TfrmLUKSHdrDump.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();
end;

function TfrmLUKSHdrDump.DumpLUKSDataToFile(): Boolean;
var
  userKey:            TSDUBytes;
  keyfile:            String;
  keyfileIsASCII:     Boolean;
  keyfileNewlineType: TSDUNewline;
begin
  frmeLUKSKeyOrKeyfileEntry1.GetKey(userKey);
  keyfile        := frmeLUKSKeyOrKeyfileEntry1.GetKeyfile();
  keyfileIsASCII := frmeLUKSKeyOrKeyfileEntry1.GetKeyfileIsASCII();
  frmeLUKSKeyOrKeyfileEntry1.GetKeyfileNewlineType(keyfileNewlineType);

  Result := LUKSTools.DumpLUKSDataToFile(GetVolumeFilename, userKey, keyfile,
    keyfileIsASCII, keyfileNewlineType, GetLUKSBaseIVCypherOnHashLength, GetDumpFilename);
end;

procedure TfrmLUKSHdrDump.pbOKClick(Sender: TObject);
var
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
  startTime: TDateTime;
  stopTime: TDateTime;
  diffTime: TDateTime;
  Hour, Min, Sec, MSec: Word;
{$ENDIF}
  dumpOK: Boolean;
  //  notepadCommandLine: String;
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
      SDUCRLF + SDUCRLF + GetDumpFilename + SDUCRLF + SDUCRLF +
      _('Do you wish to open this file in Windows Notepad?'), mtInformation,
      [mbYes, mbNo], 0) = mrYes) then begin

      if not (SDUWinExecNoWait32('notepad', GetDumpFilename, SW_RESTORE)) then
        SDUMessageDlg(_('Error running Notepad'), mtError, [], 0);

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

function TfrmLUKSHdrDump.GetLUKSBaseIVCypherOnHashLength(): Boolean;
begin

  Result := frmeLUKSKeyOrKeyfileEntry1.baseIVCypherOnHashLength;
end;

end.
