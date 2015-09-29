unit fmeAutorunOptions;
   obsolete file

interface

uses
  Classes, ComCtrls,
//  fmeBaseOptions,
   CommonSettings, Controls, Dialogs, Forms,
  Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit, SDUDialogs, SDUStdCtrls,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfmeAutorunOptions = class (TFrame )
  PUBLIC
  end;

implementation

{$R *.dfm}

uses
  //sdu, lcutils
lcConsts,
  SDUGeneral, SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}







end.
