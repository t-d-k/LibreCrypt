unit OTFEFreeOTFE_frmWizardCreateVolumeAdvanced;
{$ERROR;obsolete unit - use OTFEFreeOTFE_frmWizardCreateVolume instead}
interface

uses
  Classes, ComCtrls, Controls, Dialogs, Forms,
  Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit, OTFEFreeOTFEBase_U,
  SDUDialogs, SDUForms,
  SDUStdCtrls, Spin64, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmWizardCreateVolumeAdvanced = class (TSDUForm)
    procedure FormCreate(Sender: TObject);
//    procedure pbOKClick(Sender: TObject);
  PRIVATE
//    FCypherBlocksize: Integer;

//    function GetKeyIterations(): Integer;
//    procedure SetKeyIterations(keyIterations: Integer);
//    function GetSaltLength(): Integer;
//    procedure SetSaltLength(saltLength: Integer);
//    function GetCDBFilename(): String;
//    procedure SetCDBFilename(CDBFilename: String);
//    function GetPaddingLength(): Int64;
//    procedure SetPaddingLength(len: Int64);
//    function GetOverwriteWithChaff(): Boolean;
//    procedure SetOverwriteWithChaff(secChaff: Boolean);
  PROTECTED
//    procedure EnableDisableControls();

  PUBLIC
    // Used in validation
    // Selected cypher blocksize in bits
//    property CypherBlocksize: Integer Read FCypherBlocksize Write FCypherBlocksize;

//    property KeyIterations: Integer Read GetKeyIterations Write SetKeyIterations;
//    property SaltLength: Integer Read GetSaltLength Write SetSaltLength;
//    property DriveLetter: Ansichar Read GetDriveLetter Write SetDriveLetter;
//    property CDBFilename: String Read GetCDBFilename Write SetCDBFilename;
//    property PaddingLength: Int64 Read GetPaddingLength Write SetPaddingLength;
//    property OverwriteWithChaff: Boolean Read GetOverwriteWithChaff Write SetOverwriteWithChaff;

  end;


implementation

{$R *.dfm}

uses
  OTFEFreeOTFE_U, SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}





//
//function TfrmWizardCreateVolumeAdvanced.GetKeyIterations(): Integer;
//begin
//  Result := seKeyIterations.Value;
//end;
//
//procedure TfrmWizardCreateVolumeAdvanced.SetKeyIterations(keyIterations: Integer);
//begin
//  seKeyIterations.Value := keyIterations;
//end;

//function TfrmWizardCreateVolumeAdvanced.GetSaltLength(): Integer;
//begin
//  Result := seSaltLength.Value;
//end;
//
//procedure TfrmWizardCreateVolumeAdvanced.SetSaltLength(saltLength: Integer);
//begin
//  seSaltLength.Value := saltLength;
//end;





procedure TfrmWizardCreateVolumeAdvanced.FormCreate(Sender: TObject);

begin
//  CypherBlocksize := 0;






  // Set to first page

end;





end.
