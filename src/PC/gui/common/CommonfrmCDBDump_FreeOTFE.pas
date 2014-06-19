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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  PasswordRichEdit, Spin64,
  OTFEFreeOTFEBase_U, Buttons, SDUForms, SDUFrames,
  SDUSpin64Units, SDUFilenameEdit_U, SDUDialogs, OTFEFreeOTFE_VolumeSelect,
  CommonfrmCDBDump_Base;

type
  TfrmCDBDump_FreeOTFE = class(TfrmCDBDump_Base)
    lblOffset: TLabel;
    seSaltLength: TSpinEdit64;
    lblSaltLengthBits: TLabel;
    lblSaltLength: TLabel;
    seKeyIterations: TSpinEdit64;
    lblKeyIterations: TLabel;
    se64UnitOffset: TSDUSpin64Unit_Storage;
    preUserKey: TPasswordRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
  private
    function GetUserKey(): string;

    function GetOffset(): int64;
    function GetSaltLength(): integer;
    function GetKeyIterations(): integer;

  protected
    procedure EnableDisableControls(); override;

    function  DumpLUKSDataToFile(): boolean; override;

  public
    property UserKey: string read GetUserKey;

    property Offset: int64 read GetOffset;
    property SaltLength: integer read GetSaltLength;
    property KeyIterations: integer read GetKeyIterations;
  end;


implementation

{$R *.DFM}

uses
  SDUi18n,
  SDUGeneral;


function TfrmCDBDump_FreeOTFE.GetUserKey(): string;
begin
  Result := preUserKey.Text;
end;

function TfrmCDBDump_FreeOTFE.GetOffset(): int64;
begin
  Result := se64UnitOffset.Value;
end;

function TfrmCDBDump_FreeOTFE.GetSaltLength(): integer;
begin
  Result := seSaltLength.Value;
end;

function TfrmCDBDump_FreeOTFE.GetKeyIterations(): integer;
begin
  Result := seKeyIterations.Value;
end;

procedure TfrmCDBDump_FreeOTFE.FormCreate(Sender: TObject);
begin
  inherited;

  self.caption := _('Dump Critical Data Block');

  preUserKey.Plaintext := TRUE;
  // FreeOTFE volumes CAN have newlines in the user's password
  preUserKey.WantReturns := TRUE;
  preUserKey.WordWrap := TRUE;
  preUserKey.Lines.Clear();
  preUserKey.PasswordChar := '*';

  // se64UnitOffset set in OnShow event (otherwise units not shown correctly)

  seSaltLength.Increment := 8;
  seSaltLength.Value := DEFAULT_SALT_LENGTH;

  seKeyIterations.MinValue := 1;
  seKeyIterations.MaxValue := 999999; // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seKeyIterations.Value := DEFAULT_KEY_ITERATIONS;

end;

procedure TfrmCDBDump_FreeOTFE.EnableDisableControls();
begin
  pbOK.Enabled := (
                   (VolumeFilename <> '') AND
                   (feDumpFilename.Filename <> '') AND
                   (feDumpFilename.Filename <> VolumeFilename) AND // Don't overwrite the volume with the dump!!!
                   (KeyIterations > 0)
                  );
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

function TfrmCDBDump_FreeOTFE.DumpLUKSDataToFile(): boolean;
begin
  Result := OTFEFreeOTFE.DumpCriticalDataToFile(
                           VolumeFilename,
                           Offset,
                           UserKey,
                           SaltLength,  // In bits
                           KeyIterations,
                           DumpFilename
                          );
end;

procedure TfrmCDBDump_FreeOTFE.pbOKClick(Sender: TObject);
var
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
  startTime: TDateTime;
  stopTime: TDateTime;
  diffTime: TDateTime;
  Hour, Min, Sec, MSec: Word;
{$ENDIF}
  dumpOK: boolean;
  notepadCommandLine: string;
begin
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
  startTime := Now();
{$ENDIF}

  dumpOK := DumpLUKSDataToFile();

  if dumpOK then
    begin
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
    stopTime := Now();
    diffTime := (stopTime - startTime);
    DecodeTime(diffTime, Hour, Min, Sec, MSec);
    showmessage('Time taken to dump CDB: '+inttostr(Hour)+' hours, '+inttostr(Min)+' mins, '+inttostr(Sec)+'.'+inttostr(MSec)+' secs');
{$ENDIF}

    if (SDUMessageDlg(
               _('A human readable copy of your critical data block has been written to:')+SDUCRLF+
               SDUCRLF+
               DumpFilename+SDUCRLF+
               SDUCRLF+
               _('Do you wish to open this file in Windows Notepad?'),
               mtInformation, [mbYes,mbNo], 0) = mrYes) then
      begin
      notepadCommandLine := 'notepad '+DumpFilename;

      if (WinExec(PChar(notepadCommandLine), SW_RESTORE))<31 then
        begin
        SDUMessageDlg(_('Error running Notepad'), mtError, [], 0);
        end;

      end;

    ModalResult := mrOK;
    end
  else
    begin
{$IFDEF FREEOTFE_TIME_CDB_DUMP}
    stopTime := Now();
    diffTime := (stopTime - startTime);
    DecodeTime(diffTime, Hour, Min, Sec, MSec);
    showmessage('Time taken to FAIL to dump CDB: '+inttostr(Hour)+' hours, '+inttostr(Min)+' mins, '+inttostr(Sec)+'.'+inttostr(MSec)+' secs');
{$ENDIF}

    SDUMessageDlg(
               _('Unable to dump out critical data block.')+SDUCRLF+
               SDUCRLF+
               _('Please ensure that your password and details are entered correctly, and that this file is not currently in use (e.g. mounted)'),
               mtError, [mbOK], 0);
    end;

end;

END.


