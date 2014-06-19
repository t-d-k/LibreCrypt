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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  PasswordRichEdit, Spin64,
  OTFEFreeOTFEBase_U, Buttons, SDUForms, SDUFrames,
  SDUSpin64Units, SDUFilenameEdit_U, SDUDialogs, OTFEFreeOTFE_VolumeSelect,
  CommonfrmCDBDump_Base, OTFEFreeOTFE_LUKSKeyOrKeyfileEntry;

type
  TfrmCDBDump_LUKS = class(TfrmCDBDump_Base)
    lblOptional: TLabel;
    ckBaseIVCypherOnHashLength: TCheckBox;
    OTFEFreeOTFELUKSKeyOrKeyfileEntry1: TOTFEFreeOTFELUKSKeyOrKeyfileEntry;
    procedure FormCreate(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetLUKSBaseIVCypherOnHashLength(): boolean;

  protected
    procedure EnableDisableControls(); override;

    function  DumpLUKSDataToFile(): boolean; override;

  public
    property LUKSBaseIVCypherOnHashLength: boolean read GetLUKSBaseIVCypherOnHashLength;
  end;


implementation

{$R *.DFM}

uses
  SDUi18n,
  SDUGeneral;

procedure TfrmCDBDump_LUKS.FormCreate(Sender: TObject);
begin
  inherited;

  self.caption := _('Dump LUKS Details');

end;

procedure TfrmCDBDump_LUKS.FormShow(Sender: TObject);
begin
  inherited;

  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.FreeOTFEObj := OTFEFreeOTFE;
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.Initialize();
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.DefaultOptions();

  ckBaseIVCypherOnHashLength.checked := TRUE;

end;

procedure TfrmCDBDump_LUKS.EnableDisableControls();
begin
  pbOK.Enabled := (
                   (VolumeFilename <> '') AND
                   (feDumpFilename.Filename <> '') AND
                   (feDumpFilename.Filename <> VolumeFilename) // Don't overwrite the volume with the dump!!!
                  );
end;

procedure TfrmCDBDump_LUKS.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();
end;

function TfrmCDBDump_LUKS.DumpLUKSDataToFile(): boolean;
var
  userKey: string;
  keyfile: string;
  keyfileIsASCII: boolean;
  keyfileNewlineType: TSDUNewline;
begin
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyRaw(userKey);
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyfile(keyfile);
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyfileIsASCII(keyfileIsASCII);
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyfileNewlineType(keyfileNewlineType);

  Result := OTFEFreeOTFE.DumpLUKSDataToFile(
                                              VolumeFilename,
                                              userKey,
                                              keyfile,
                                              keyfileIsASCII,
                                              keyfileNewlineType,
                                              LUKSBaseIVCypherOnHashLength,
                                              DumpFilename
                                             );
end;

procedure TfrmCDBDump_LUKS.pbOKClick(Sender: TObject);
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

function TfrmCDBDump_LUKS.GetLUKSBaseIVCypherOnHashLength(): boolean;
begin
  Result := ckBaseIVCypherOnHashLength.checked;
end;

END.


