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
  //delphi
  Classes, ComCtrls, Buttons,
  Controls, Dialogs,
  Forms, Graphics, Messages, PasswordRichEdit, Spin64,
  StdCtrls, SysUtils, Windows,
  //sdu / lc utils
  lcTypes, OTFEFreeOTFEBase_U, lcDialogs, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUGeneral, SDUSpin64Units, OTFE_U,
  //librecrypt forms

  frmHdrDump,
  fmeVolumeSelect,
  fmePassword;

type
  TfrmFreeOTFEHdrDump = class (TfrmHdrDump)
    lblOffset:         TLabel;
    seSaltLength:      TSpinEdit64;
    lblSaltLengthBits: TLabel;
    lblSaltLength:     TLabel;
    seKeyIterations:   TSpinEdit64;
    lblKeyIterations:  TLabel;
    se64UnitOffset:    TSDUSpin64Unit_Storage;
    frmePassword1:     TfrmePassword;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
  private
    function GetUserKey(): TSDUBytes;

    function GetOffset(): Int64;
    function GetSaltLength(): Integer;
    function GetKeyIterations(): Integer;

  protected
    procedure _EnableDisableControls(); override;

    function _DumpHdrDataToFile(): Boolean; override;
            procedure SetPassword(const Value: string);  override;
  public
    //    property UserKey: TSDUBytes Read GetUserKey;
    //
    //    property Offset: Int64 Read GetOffset;
    //    property SaltLength: Integer Read GetSaltLength;
    //    property KeyIterations: Integer Read GetKeyIterations;
  end;


implementation

{$R *.DFM}

uses

  //sdu / lc utils
  lcConsts, SDUi18n;

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

procedure TfrmFreeOTFEHdrDump._EnableDisableControls();
begin
  inherited;
  pbOK.Enabled := pbOK.Enabled and (GetKeyIterations > 0);
end;

procedure TfrmFreeOTFEHdrDump.FormShow(Sender: TObject);
begin
  inherited;

  se64UnitOffset.Value := 0;

  _EnableDisableControls();

end;

procedure TfrmFreeOTFEHdrDump.ControlChanged(Sender: TObject);
begin
  _EnableDisableControls();
end;

function TfrmFreeOTFEHdrDump._DumpHdrDataToFile(): Boolean;
begin
  Result := GetFreeOTFEBase().DumpCriticalDataToFile(GetVolumeFilename, GetOffset,
    GetUserKey, GetSaltLength,  // In bits
    GetKeyIterations, GetDumpFilename);
end;


procedure TfrmFreeOTFEHdrDump.pbOKClick(Sender: TObject);
begin
  _PromptDumpData('FreeOTFE');
end;

procedure TfrmFreeOTFEHdrDump.SetPassword(const Value: string);
begin
  inherited;
  frmePassword1.SetKeyPhrase(Value);
end;

end.
