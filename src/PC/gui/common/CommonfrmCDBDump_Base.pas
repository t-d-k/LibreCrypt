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
  Buttons, Classes, ComCtrls,
  Controls, Dialogs,
  Forms, Graphics, Messages, OTFEFreeOTFE_VolumeSelect, OTFEFreeOTFEBase_U,
  PasswordRichEdit, lcDialogs, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUSpin64Units, Spin64,
  StdCtrls, SysUtils, Windows;

type
  TfrmCDBDump_Base = class (TSDUForm)
    pbOK:                      TButton;
    pbCancel:                  TButton;
    GroupBox1:                 TGroupBox;
    Label1:                    TLabel;
    Label2:                    TLabel;
    GroupBox2:                 TGroupBox;
    Label3:                    TLabel;
    feDumpFilename:            TSDUFilenameEdit;
    OTFEFreeOTFEVolumeSelect1: TOTFEFreeOTFEVolumeSelect;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure OTFEFreeOTFEVolumeSelect1Change(Sender: TObject);
  PRIVATE
    function GetVolumeFilename(): String;
    function GetDumpFilename(): String;

  PROTECTED
    procedure EnableDisableControls(); VIRTUAL; ABSTRACT;

    function DumpLUKSDataToFile(): Boolean; VIRTUAL; ABSTRACT;

  PUBLIC
//    OTFEFreeOTFE: TOTFEFreeOTFEBase;

    property VolumeFilename: String Read GetVolumeFilename;
    property DumpFilename: String Read GetDumpFilename;
  end;


implementation

{$R *.DFM}

uses
  CommonSettings, SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


function TfrmCDBDump_Base.GetVolumeFilename(): String;
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

function TfrmCDBDump_Base.GetDumpFilename(): String;
begin
  Result := feDumpFilename.Filename;
end;


procedure TfrmCDBDump_Base.FormCreate(Sender: TObject);
begin
  // se64UnitOffset set in OnShow event (otherwise units not shown correctly)

  OTFEFreeOTFEVolumeSelect1.Filename             := '';
  OTFEFreeOTFEVolumeSelect1.AllowPartitionSelect := True;
  OTFEFreeOTFEVolumeSelect1.FileSelectFilter     := FILE_FILTER_FLT_VOLUMESANDKEYFILES;
  OTFEFreeOTFEVolumeSelect1.FileSelectDefaultExt := FILE_FILTER_DFLT_VOLUMESANDKEYFILES;
  OTFEFreeOTFEVolumeSelect1.SelectFor            := fndOpen;
  OTFEFreeOTFEVolumeSelect1.OnChange             := OTFEFreeOTFEVolumeSelect1Change;

  feDumpFilename.Filename := '';
end;

procedure TfrmCDBDump_Base.FormShow(Sender: TObject);
begin
//  OTFEFreeOTFEVolumeSelect1.OTFEFreeOTFE := OTFEFreeOTFE;

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


      if not (SDUWinExecNoWait32('notepad',DumpFilename, SW_RESTORE)) then begin
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
