unit CommonfrmCDBDump_Base;
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
  SDUSpin64Units, SDUFilenameEdit_U, SDUDialogs, OTFEFreeOTFE_VolumeSelect;

type
  TfrmCDBDump_Base = class(TSDUForm)
    pbOK: TButton;
    pbCancel: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    feDumpFilename: TSDUFilenameEdit;
    OTFEFreeOTFEVolumeSelect1: TOTFEFreeOTFEVolumeSelect;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure OTFEFreeOTFEVolumeSelect1Change(Sender: TObject);
  private
    function GetVolumeFilename(): string;
    function GetDumpFilename(): string;

  protected
    procedure EnableDisableControls(); virtual; abstract;

    function  DumpLUKSDataToFile(): boolean; virtual; abstract;

  public
    OTFEFreeOTFE: TOTFEFreeOTFEBase;

    property VolumeFilename: string read GetVolumeFilename;
    property DumpFilename: string read GetDumpFilename;
  end;


implementation

{$R *.DFM}

uses
  SDUi18n,
  SDUGeneral,
  CommonSettings;


function TfrmCDBDump_Base.GetVolumeFilename(): string;
begin
  Result := OTFEFreeOTFEVolumeSelect1.Filename;
end;

procedure TfrmCDBDump_Base.OTFEFreeOTFEVolumeSelect1Change(Sender: TObject);
begin
{
  if not(fmeVolumeSelect.IsPartitionSelected) then
    begin
    edDumpFilename.text := VolumeFilename + '.txt';
    end;
}

  ControlChanged(Sender);
end;

function TfrmCDBDump_Base.GetDumpFilename(): string;
begin
  Result := feDumpFilename.Filename;
end;


procedure TfrmCDBDump_Base.FormCreate(Sender: TObject);
begin
  // se64UnitOffset set in OnShow event (otherwise units not shown correctly)

  OTFEFreeOTFEVolumeSelect1.Filename := '';
  OTFEFreeOTFEVolumeSelect1.AllowPartitionSelect := TRUE;
  OTFEFreeOTFEVolumeSelect1.FileSelectFilter := FILE_FILTER_FLT_VOLUMESANDKEYFILES;
  OTFEFreeOTFEVolumeSelect1.FileSelectDefaultExt := FILE_FILTER_DFLT_VOLUMESANDKEYFILES;
  OTFEFreeOTFEVolumeSelect1.SelectFor := fndOpen;

  feDumpFilename.Filename := '';

end;

procedure TfrmCDBDump_Base.FormShow(Sender: TObject);
begin
  OTFEFreeOTFEVolumeSelect1.OTFEFreeOTFE := OTFEFreeOTFE;

  FreeOTFEGUISetupOpenSaveDialog(feDumpFilename);
  feDumpFilename.Filter     := FILE_FILTER_FLT_TEXTFILES;
  feDumpFilename.DefaultExt := FILE_FILTER_DFLT_TEXTFILES;

  EnableDisableControls();

end;

procedure TfrmCDBDump_Base.ControlChanged(Sender: TObject);
begin
  EnableDisableControls();
end;

procedure TfrmCDBDump_Base.pbOKClick(Sender: TObject);
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


