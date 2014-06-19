unit CrossCrypt_PasswordConfirm;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TCrossCrypt_PasswordConfirm_F = class(TForm)
    lblMultipleKeyMode: TLabel;
    rePasswords: TRichEdit;
    lblPasswordCount: TLabel;
    pbCancel: TButton;
    pbOK: TButton;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure rePasswordsChange(Sender: TObject);
  private
    FMultipleKey: boolean;
    FPasswords: TStringList;

    procedure ClearPasswords();

    procedure UpdatePasswordCount();

  public
    property MultipleKey: boolean read FMultipleKey write FMultipleKey;

    // Clear down the dialog's internals...
    procedure Wipe();

  end;


implementation

{$R *.DFM}

procedure TCrossCrypt_PasswordConfirm_F.FormShow(Sender: TObject);
begin
  rePasswords.Lines.Clear();
  rePasswords.PlainText := TRUE;
  if MultipleKey then
    begin
    lblMultipleKeyMode.caption := '(Multiple key)';
    rePasswords.WordWrap := FALSE;
    end
  else
    begin
    lblMultipleKeyMode.caption := '(Single key)';
    rePasswords.WordWrap := TRUE;
    end;

end;


procedure TCrossCrypt_PasswordConfirm_F.ClearPasswords();
var
  i: integer;
begin
  for i:=0 to (FPasswords.count-1) do
    begin
    FPasswords[i] := StringOfChar('X', length(FPasswords[i]));
    end;
    
  FPasswords.Clear();

end;


procedure TCrossCrypt_PasswordConfirm_F.Wipe();
begin
  // Cleardown the internal store...
  ClearPasswords();
  FMultipleKey := FALSE;

end;


procedure TCrossCrypt_PasswordConfirm_F.UpdatePasswordCount();
begin
  if (MultipleKey) then
    begin
    lblPasswordCount.caption := 'Passwords entered: '+inttostr(rePasswords.lines.count)+'/'+inttostr(MULTIKEY_PASSWORD_REQUIREMENT);
    end
  else
    begin
    lblPasswordCount.caption := '';
    end;

end;


procedure TCrossCrypt_PasswordConfirm_F.rePasswordsChange(Sender: TObject);
begin
  UpdatePasswordCount();

end;

END.

