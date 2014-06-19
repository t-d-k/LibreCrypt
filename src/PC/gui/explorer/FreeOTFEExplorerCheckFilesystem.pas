unit FreeOTFEExplorerCheckFilesystem;

interface

uses
  SDFilesystem_FAT;

procedure CheckFilesystem(Filesystem: TSDFilesystem_FAT);

implementation

uses
  Dialogs, Forms,
  SDUGeneral,
  SDUDialogs,
  SDUi18n;

procedure CheckFilesystem(Filesystem: TSDFilesystem_FAT);
var
  allOK: boolean;
begin
  //SetStatusMsg(_('Checking filesystem...'));
  try
    allOK := Filesystem.CheckFilesystem();
  finally
    //SetStatusMsg('');
  end;

  if allOK then
    begin
    SDUMessageDlg(_('Filesystem OK'), mtInformation);
    end
  else
    begin
    Filesystem.Readonly := TRUE;
    SDUMessageDlg(
                  SDUParamSubstitute(
                                     _('Filesystem errors detected.'+SDUCRLF+
                                       SDUCRLF+
                                       'Please mount as a normal drive, and run chkdsk to correct.'+
                                       SDUCRLF+
                                       '%1 will continue in readonly mode'),
                                     [Application.Title]
                                    ),
                  mtError
                  );
    end;

end;

END.

