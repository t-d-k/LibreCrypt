unit OTFECrossCrypt_PasswordConfirm;
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
  TOTFECrossCrypt_PasswordConfirm_F = class(TForm)
    lblMultipleKeyMode: TLabel;
    rePasswords: TRichEdit;
    lblPasswordCount: TLabel;
    pbCancel: TButton;
    pbOK: TButton;
    Label2: TLabel;
    Label1: TLabel;
    pbLoadFromFile: TButton;
    OpenKeyfileDlg: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure rePasswordsChange(Sender: TObject);
    procedure pbCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pbOKClick(Sender: TObject);
    procedure pbLoadFromFileClick(Sender: TObject);
  private
    FMultipleKey: boolean;
    FPasswords: TStringList;

    procedure ClearPasswords();

    procedure UpdatePasswordCount();

    procedure WipeGUI();
    procedure WipeInternal();

  public
    property MultipleKey: boolean read FMultipleKey write FMultipleKey;

    procedure GetSinglePassword(var password: string);
    procedure GetMultiplePasswords(passwords: TStringList);

    // Clear down the dialog's internals...
    procedure Wipe();

  end;


implementation

{$R *.DFM}

uses
  OTFECrossCrypt_DriverAPI;

procedure TOTFECrossCrypt_PasswordConfirm_F.FormShow(Sender: TObject);
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

  lblMultipleKeyMode.left := ((self.width - lblMultipleKeyMode.width) div 2);

  // Mask the user's password...
  SendMessage(rePasswords.Handle, EM_SETPASSWORDCHAR, Ord('*'), 0);

end;


procedure TOTFECrossCrypt_PasswordConfirm_F.ClearPasswords();
var
  i: integer;
begin
  for i:=0 to (FPasswords.count-1) do
    begin
    FPasswords[i] := StringOfChar('X', length(FPasswords[i]));
    end;
    
  FPasswords.Clear();

end;


procedure TOTFECrossCrypt_PasswordConfirm_F.Wipe();
begin
  WipeInternal();

end;


procedure TOTFECrossCrypt_PasswordConfirm_F.WipeGUI();
begin
  // Cleardown the GUI components...
  rePasswords.Text := StringOfChar('X', length(rePasswords.text));

end;


procedure TOTFECrossCrypt_PasswordConfirm_F.WipeInternal();
begin
  // Cleardown the internal store...
  ClearPasswords();
  FMultipleKey := FALSE;

end;


procedure TOTFECrossCrypt_PasswordConfirm_F.UpdatePasswordCount();
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


procedure TOTFECrossCrypt_PasswordConfirm_F.rePasswordsChange(Sender: TObject);
begin
  UpdatePasswordCount();

end;

procedure TOTFECrossCrypt_PasswordConfirm_F.pbCancelClick(Sender: TObject);
begin
  // Cleardown...
  Wipe();

  ModalResult := mrCancel;

end;

procedure TOTFECrossCrypt_PasswordConfirm_F.FormCreate(Sender: TObject);
begin
  FPasswords := TStringList.Create();

end;

procedure TOTFECrossCrypt_PasswordConfirm_F.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  WipeGUI();
  
end;


// Get the single password
procedure TOTFECrossCrypt_PasswordConfirm_F.GetSinglePassword(var password: string);
begin
  password := FPasswords.Text;

  // Trim off the CRLF
  Delete(password, (Length(password)-1), 2);

end;


// Returns '' on failure
procedure TOTFECrossCrypt_PasswordConfirm_F.GetMultiplePasswords(passwords: TStringList);
begin
  passwords.Text := StringOfChar('X', length(passwords.text));
  passwords.Clear();

  passwords.AddStrings(FPasswords);
end;



procedure TOTFECrossCrypt_PasswordConfirm_F.pbOKClick(Sender: TObject);
begin
  FPasswords.Clear();
  FPasswords.AddStrings(rePasswords.Lines);

  ModalResult := mrOK;

end;


procedure TOTFECrossCrypt_PasswordConfirm_F.pbLoadFromFileClick(
  Sender: TObject);
begin
  if OpenKeyfileDlg.Execute() then
    begin
    rePasswords.Lines.LoadFromFile(OpenKeyfileDlg.Filename);
    end;

end;

END.

