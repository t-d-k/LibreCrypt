unit CommonfrmCDBDump_FreeOTFE;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  Classes, ComCtrls,
  Controls, Dialogs,
  Forms, Graphics, Messages, PasswordRichEdit, Spin64,
  StdCtrls, SysUtils, Windows, //dosbox
  Buttons, CommonfrmCDBDump_Base,
  OTFE_U, OTFEFreeOTFE_VolumeSelect,
  OTFEFreeOTFEBase_U, SDUDialogs, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUGeneral, SDUSpin64Units;

type
  TfrmCDBDump_FreeOTFE = class (TfrmCDBDump_Base)
    lblOffset:         TLabel;
    seSaltLength:      TSpinEdit64;
    lblSaltLengthBits: TLabel;
    lblSaltLength:     TLabel;
    seKeyIterations:   TSpinEdit64;
    lblKeyIterations:  TLabel;
    se64UnitOffset:    TSDUSpin64Unit_Storage;
    preUserKey:        TPasswordRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
  PRIVATE
    function GetUserKey(): TSDUBytes;

    function GetOffset(): Int64;
    function GetSaltLength(): Integer;
    function GetKeyIterations(): Integer;

  PROTECTED
    procedure EnableDisableControls(); OVERRIDE;

    function DumpLUKSDataToFile(): Boolean; OVERRIDE;

  PUBLIC
    property UserKey: TSDUBytes Read GetUserKey;

    property Offset: Int64 Read GetOffset;
    property SaltLength: Integer Read GetSaltLength;
    property KeyIterations: Integer Read GetKeyIterations;
  end;


implementation

{$R *.DFM}

uses
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


function TfrmCDBDump_FreeOTFE.GetUserKey(): TSDUBytes;
begin
  Result := SDUStringToSDUBytes(preUserKey.Text);
end;

function TfrmCDBDump_FreeOTFE.GetOffset(): Int64;
begin
  Result := se64UnitOffset.Value;
end;

function TfrmCDBDump_FreeOTFE.GetSaltLength(): Integer;
begin
  Result := seSaltLength.Value;
end;

function TfrmCDBDump_FreeOTFE.GetKeyIterations(): Integer;
begin
  Result := seKeyIterations.Value;
end;

procedure TfrmCDBDump_FreeOTFE.FormCreate(Sender: TObject);
begin
  inherited;

  self.Caption := _('Dump Critical Data Block');

  preUserKey.Plaintext   := True;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKey.WantReturns := True;
  preUserKey.WordWrap    := True;
  preUserKey.Lines.Clear();
  preUserKey.PasswordChar := '*';

  // se64UnitOffset set in OnShow event (otherwise units not shown correctly)

  seSaltLength.Increment := 8;
  seSaltLength.Value     := DEFAULT_SALT_LENGTH;

  seKeyIterations.MinValue  := 1;
  seKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seKeyIterations.Value     := DEFAULT_KEY_ITERATIONS;

end;

procedure TfrmCDBDump_FreeOTFE.EnableDisableControls();
begin
  pbOK.Enabled := ((VolumeFilename <> '') and (feDumpFilename.Filename <> '') and
    (feDumpFilename.Filename <> VolumeFilename) and
    // Don't overwrite the volume with the dump!!!
    (KeyIterations > 0));
end;

procedure TfrmCDBDump_FreeOTFE.FormShow(Sender: TObject);
begin
  inherited;

  se64UnitOffset.Value := 0;

  EnableDisableControls();

end;

procedure TfrmCDBDump_FreeOTFE.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();
end;

function TfrmCDBDump_FreeOTFE.DumpLUKSDataToFile(): Boolean;
begin
  Result := GetFreeOTFEBase().DumpCriticalDataToFile(VolumeFilename, Offset,
    GetUserKey, SaltLength,  // In bits
    KeyIterations, DumpFilename);
end;

procedure TfrmCDBDump_FreeOTFE.pbOKClick(Sender: TObject);
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

end.
