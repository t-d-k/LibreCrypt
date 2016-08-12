unit frmHdrDump;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses

  //delphi & libs
  Buttons, Classes, ComCtrls,
  Controls, Dialogs,
  Forms, Graphics, Messages,
  Spin64,
  StdCtrls, SysUtils, Windows,
  //sdu & LibreCrypt utils
  SDUSpin64Units, OTFEFreeOTFEBase_U, PasswordRichEdit, lcDialogs, SDUFilenameEdit_U,
  // LibreCrypt forms
  fmeVolumeSelect,
  SDUForms, SDUFrames;

type
  TfrmHdrDump = class (TSDUForm)
    pbOK:           TButton;
    pbCancel:       TButton;
    GroupBox1:      TGroupBox;
    GroupBox2:      TGroupBox;
    feDumpFilename: TSDUFilenameEdit;
    OTFEFreeOTFEVolumeSelect1: TfmeVolumeSelect;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure OTFEFreeOTFEVolumeSelect1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SetDumpFilename(const Value: String);



  protected
    fshow_time:    Boolean;   // show time taken to dump
    fsilent:       Boolean;
    fsilentResult: TModalResult;

    function GetVolumeFilename(): String;
    procedure SetVolumeFilename(const Value: String);
    function GetDumpFilename(): String;
    procedure _PromptDumpData(const hdrType: String);

    procedure _EnableDisableControls(); virtual;

    function _DumpHdrDataToFile(): Boolean; virtual; abstract;
    procedure SetPassword(const Value: String); virtual; abstract;
  public

    property VolumeFilename: String Read GetVolumeFilename Write SetVolumeFilename;
    property DumpFilename: String Read GetDumpFilename Write SetDumpFilename;

    property password: String Write SetPassword;
  end;


implementation

{$R *.DFM}

uses
  //sdu / lc utils
  sduGeneral, lcConsts,
  CommonSettings,
  SDUi18n,lcCommandLine;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


function TfrmHdrDump.GetVolumeFilename(): String;
begin
  Result := OTFEFreeOTFEVolumeSelect1.Filename;
end;



procedure TfrmHdrDump.SetVolumeFilename(const Value: String);
begin
  OTFEFreeOTFEVolumeSelect1.Filename := Value;
end;

procedure TfrmHdrDump.OTFEFreeOTFEVolumeSelect1Change(Sender: TObject);
begin
{
  if not(fmeVolumeSelect.IsPartitionSelected) then
    begin
    edDumpFilename.text := VolumeFilename + '.txt';
    end;
}

  ControlChanged(Sender);
end;

function TfrmHdrDump.GetDumpFilename(): String;
begin
  Result := feDumpFilename.Filename;
end;

procedure TfrmHdrDump.SetDumpFilename(const Value: String);
begin
  feDumpFilename.Filename := Value;
end;

procedure TfrmHdrDump._EnableDisableControls;
begin
  pbOK.Enabled := ((GetVolumeFilename <> '') and (feDumpFilename.Filename <> '') and
    (feDumpFilename.Filename <> GetVolumeFilename) // Don't overwrite the volume with the dump!!!
    );
end;

procedure TfrmHdrDump.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Posting WM_CLOSE causes Delphi to reset ModalResult to mrCancel.
  // As a result, we reset ModalResult here, note will only close automatically if mr = mrok anyway
  if fsilent then
    ModalResult := FSilentResult;
end;

procedure TfrmHdrDump.FormCreate(Sender: TObject);
begin
  // se64UnitOffset set in OnShow event (otherwise units not shown correctly)
  fsilent := GetCmdLine.isSilent;
  SetVolumeFilename(GetCmdLine.volume);
  SetPassword(GetCmdLine.password);
  SetDumpFilename(GetCmdLine.filename);


  OTFEFreeOTFEVolumeSelect1.Filename             := '';
  OTFEFreeOTFEVolumeSelect1.AllowPartitionSelect := True;
  OTFEFreeOTFEVolumeSelect1.SetFileSelectFilter   (  FILE_FILTER_FLT_VOLUMESANDKEYFILES);
  OTFEFreeOTFEVolumeSelect1.SetFileSelectDefaultExt (FILE_FILTER_DFLT_VOLUMESANDKEYFILES);
  OTFEFreeOTFEVolumeSelect1.SelectFor            := osOpen;
  OTFEFreeOTFEVolumeSelect1.OnChange             := OTFEFreeOTFEVolumeSelect1Change;

  feDumpFilename.Filename := '';

  feDumpFilename.TabOrder := 0;
  feDumpFilename.TabStop := False;
  feDumpFilename.FilenameEditType := fetSave;
  feDumpFilename.FilterIndex := 0;
  feDumpFilename.OnChange := ControlChanged;
   {$IFDEF LC_TIME_HDR_DUMP}
      fshow_time := true;
    {$ELSE}
  fshow_time := False;
   {$ENDIF}

end;

procedure TfrmHdrDump.FormShow(Sender: TObject);
begin

  FreeOTFEGUISetupOpenSaveDialog(feDumpFilename);
  feDumpFilename.Filter     := FILE_FILTER_FLT_TEXTFILES;
  feDumpFilename.DefaultExt := FILE_FILTER_DFLT_TEXTFILES;

  _EnableDisableControls();

  if fSilent then begin
    ModalResult := mrCancel;
    _PromptDumpData('silent');
    FSilentResult := ModalResult;
    // if testing and no errors, then close dlg
    if ModalResult = mrOk then
      PostMessage(Handle, WM_CLOSE, 0, 0);
  end;

end;


procedure TfrmHdrDump.ControlChanged(Sender: TObject);
begin
  _EnableDisableControls();
end;

procedure TfrmHdrDump._PromptDumpData(const hdrType: String);
var
  startTime:            TDateTime;
  stopTime:             TDateTime;
  diffTime:             TDateTime;
  Hour, Min, Sec, MSec: Word;

  dumpOK: Boolean;
begin
 startTime := 0;
  if fshow_time then
    startTime := Now();


  dumpOK := _DumpHdrDataToFile();

  if dumpOK then begin
    if fshow_time then begin
      stopTime := Now();
      diffTime := (stopTime - startTime);
      DecodeTime(diffTime, Hour, Min, Sec, MSec);
      ShowMessage('Time taken to dump ' + hdrType + ' Header: ' + IntToStr(Hour) +
        ' hours, ' + IntToStr(Min) + ' mins, ' + IntToStr(Sec) + '.' + IntToStr(MSec) + ' secs');

    end;

    if not fsilent then

      if (SDUMessageDlg(format(_('A human readable copy of your %s Header has been written to: '+ SDUCRLF + SDUCRLF +'%s'),
      [hdrType,GetDumpFilename])   +
        SDUCRLF + SDUCRLF + _('Do you wish to open this file in Windows Notepad?'),
        mtInformation, [mbYes, mbNo], 0) = mrYes) then

        if not (SDUWinExecNoWait32('notepad', GetDumpFilename, SW_RESTORE)) then
          SDUMessageDlg(_('Error running Notepad'), mtError, [], 0);


    ModalResult := mrOk;
  end else begin
    if fshow_time then begin
      stopTime := Now();
      diffTime := (stopTime - startTime);
      DecodeTime(diffTime, Hour, Min, Sec, MSec);
      ShowMessage('Time taken to FAIL to dump ' + hdrType + ' Header: ' +
        IntToStr(Hour) + ' hours, ' + IntToStr(Min) + ' mins, ' + IntToStr(Sec) +
        '.' + IntToStr(MSec) + ' secs');

    end;
    if not fsilent then
      SDUMessageDlg(
        Format(_('Unable to dump out %s Header.'), [hdrType]) + SDUCRLF + SDUCRLF +
        _('Please ensure that your password and details are entered correctly, and that this file is not currently in use (e.g. mounted)'),
        mtError, [mbOK], 0);
  end;

end;

end.
