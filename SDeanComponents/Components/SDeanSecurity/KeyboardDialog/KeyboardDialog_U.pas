unit KeyboardDialog_U;
// Description: "Secure Keyboard Entry" Dialog
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TKeyboardDialog = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    Password: string;
    function  Execute(): boolean;
    procedure BlankPassword();
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses KeyboardEntryDlg_U;

procedure Register;
begin
  RegisterComponents('SDeanSecurity', [TKeyboardDialog]);
end;

function TKeyboardDialog.Execute(): boolean;
var
  entryDlg: TKeyboardEntryDlg;
begin
  Result := FALSE;

  entryDlg:= TKeyboardEntryDlg.create(nil);
  try
    if entryDlg.Showmodal()=mrOK then
      begin
      Password := entryDlg.Password;
      entryDlg.BlankPassword();
      Result := TRUE;
      end;
  finally
    entryDlg.Free();
  end;
end;

procedure TKeyboardDialog.BlankPassword();
var
  i: integer;
begin
  randomize;
  for i:=1 to length(Password) do
    begin
{$WARNINGS OFF}  // Disable useless warning
    Password[i] := chr(random(255));
    Password[i] := #0;
{$WARNINGS ON}
    end;

  Password := '';
end;


END.

