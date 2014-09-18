unit FreeOTFEExplorerCheckFilesystem;

interface

uses
  SDFilesystem_FAT;

procedure CheckFilesystem(Filesystem: TSDFilesystem_FAT);

implementation

uses
  Dialogs, Forms,
  SDUDialogs,
  SDUGeneral,
  SDUi18n;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


procedure CheckFilesystem(Filesystem: TSDFilesystem_FAT);
var
  allOK: Boolean;
begin
  //SetStatusMsg(_('Checking filesystem...'));
  try
    allOK := Filesystem.CheckFilesystem();
  finally
    //SetStatusMsg('');
  end;

  if allOK then begin
    SDUMessageDlg(_('Filesystem OK'), mtInformation);
  end else begin
    Filesystem.ReadOnly := True;
    SDUMessageDlg(
      SDUParamSubstitute(_(
      'Filesystem errors detected.' + SDUCRLF + SDUCRLF +
      'Please mount as a normal drive, and run chkdsk to correct.' +
      SDUCRLF +
      '%1 will continue in readonly mode'),
      [Application.Title]),
      mtError
      );
  end;

end;

end.
