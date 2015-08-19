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
  Windows, Messages, Graphics, Forms, StdCtrls, SysUtils,
  //sdu, lcutils
  PasswordRichEdit, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUSpin64Units, Spin64,
  lcDialogs,
  // librecrypt
  OTFEFreeOTFEBase_U, fmeLUKSKeyOrKeyfileEntry, fmeVolumeSelect,
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
    function _DumpHdrDataToFile(): Boolean; override;
            procedure SetPassword(const Value: string);  override;
  public

  end;


implementation

{$R *.DFM}

uses
  //sdu, lcutils
  SDUGeneral, SDUi18n, lcTypes, lcConsts,
  // librecrypt
  LUKSTools;

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
  _EnableDisableControls;
end;


procedure TfrmLUKSHdrDump.ControlChanged(Sender: TObject);
begin
  _EnableDisableControls();
end;

function TfrmLUKSHdrDump._DumpHdrDataToFile(): Boolean;
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
begin
  _PromptDumpData('LUKS');
end;

procedure TfrmLUKSHdrDump.SetPassword(const Value: string);
begin
  inherited;
  frmeLUKSKeyOrKeyfileEntry1.SetKey(Value);
   frmeLUKSKeyOrKeyfileEntry1. rbKeyFromUser.Checked := true;
end;

function TfrmLUKSHdrDump.GetLUKSBaseIVCypherOnHashLength(): Boolean;
begin

  Result := frmeLUKSKeyOrKeyfileEntry1.baseIVCypherOnHashLength;
end;

end.
