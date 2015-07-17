unit frmFreeOTFEHdrDump;
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
  StdCtrls, SysUtils, Windows,
  //librecrypt
  Buttons, frmHdrDump,
  OTFE_U, fmeVolumeSelect,
  OTFEFreeOTFEBase_U, lcDialogs, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUGeneral, SDUSpin64Units, fmePassword;

type
  TfrmFreeOTFEHdrDump = class (TfrmHdrDump)
    lblOffset:         TLabel;
    seSaltLength:      TSpinEdit64;
    lblSaltLengthBits: TLabel;
    lblSaltLength:     TLabel;
    seKeyIterations:   TSpinEdit64;
    lblKeyIterations:  TLabel;
    se64UnitOffset:    TSDUSpin64Unit_Storage;
    frmePassword1: TfrmePassword;
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
//    property UserKey: TSDUBytes Read GetUserKey;
//
//    property Offset: Int64 Read GetOffset;
//    property SaltLength: Integer Read GetSaltLength;
//    property KeyIterations: Integer Read GetKeyIterations;
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


function TfrmFreeOTFEHdrDump.GetUserKey(): TSDUBytes;
begin
  Result := frmePassword1.GetKeyPhrase;
end;

function TfrmFreeOTFEHdrDump.GetOffset(): Int64;
begin
  Result := se64UnitOffset.Value;
end;

function TfrmFreeOTFEHdrDump.GetSaltLength(): Integer;
begin
  Result := seSaltLength.Value;
end;

function TfrmFreeOTFEHdrDump.GetKeyIterations(): Integer;
begin
  Result := seKeyIterations.Value;
end;

procedure TfrmFreeOTFEHdrDump.FormCreate(Sender: TObject);
begin
  inherited;

  self.Caption := _('Dump FreeOTFE Header');


  // se64UnitOffset set in OnShow event (otherwise units not shown correctly)

  seSaltLength.Increment := 8;
  seSaltLength.Value     := DEFAULT_SALT_LENGTH;

  seKeyIterations.MinValue  := 1;
  seKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seKeyIterations.Value     := DEFAULT_KEY_ITERATIONS;

end;

procedure TfrmFreeOTFEHdrDump.EnableDisableControls();
begin
  pbOK.Enabled := ((GetVolumeFilename <> '') and (feDumpFilename.Filename <> '') and
    (feDumpFilename.Filename <> GetVolumeFilename) and
    // Don't overwrite the volume with the dump!!!
    (GetKeyIterations > 0));
end;

procedure TfrmFreeOTFEHdrDump.FormShow(Sender: TObject);
begin
  inherited;

  se64UnitOffset.Value := 0;

  EnableDisableControls();

end;

procedure TfrmFreeOTFEHdrDump.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();
end;

function TfrmFreeOTFEHdrDump.DumpLUKSDataToFile(): Boolean;
begin
  Result := GetFreeOTFEBase().DumpCriticalDataToFile(GetVolumeFilename, GetOffset,
    GetUserKey, GetSaltLength,  // In bits
    GetKeyIterations, GetDumpFilename);
end;

procedure TfrmFreeOTFEHdrDump.pbOKClick(Sender: TObject);
var
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
  startTime: TDateTime;
  stopTime: TDateTime;
  diffTime: TDateTime;
  Hour, Min, Sec, MSec: Word;
{$ENDIF}
  dumpOK:             Boolean;
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

      if not (SDUWinExecNoWait32('notepad',GetDumpFilename, SW_RESTORE)) then begin
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
