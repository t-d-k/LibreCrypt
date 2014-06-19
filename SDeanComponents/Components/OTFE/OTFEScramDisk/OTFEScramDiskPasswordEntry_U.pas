unit OTFEScramDiskPasswordEntry_U;
// Description: Password Entry Dialog
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, KeyboardDialog_U;

type
  TOTFEScramDiskPasswordEntry_F = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pbOK: TButton;
    pbCancel: TButton;
    mePassword1: TMaskEdit;
    mePassword3: TMaskEdit;
    mePassword2: TMaskEdit;
    mePassword4: TMaskEdit;
    pbKeydisk: TButton;
    pbKeyboard1: TButton;
    pbKeyboard3: TButton;
    pbKeyboard4: TButton;
    pbKeyboard2: TButton;
    KeyboardDialog: TKeyboardDialog;
    ckHidePasswords: TCheckBox;
    procedure FormDestroy(Sender: TObject);
    procedure pbKeydiskClick(Sender: TObject);
    procedure pbKeyboard1Click(Sender: TObject);
    procedure pbKeyboard2Click(Sender: TObject);
    procedure pbKeyboard3Click(Sender: TObject);
    procedure pbKeyboard4Click(Sender: TObject);
    procedure ckHidePasswordsClick(Sender: TObject);
  public
    procedure ClearEnteredPasswords();
  end;

implementation

{$R *.DFM}

uses Math, SDUGeneral;

const
  ONE_PASSWORD_LENGTH = 40;

// Presumably this should be enough to overwrite the relevant strings in memory?
procedure TOTFEScramDiskPasswordEntry_F.ClearEnteredPasswords();
var
  junkString : string;
  i : integer;
begin
  // Create a string 1024 chars long... (assumes that user won't try to enter
  // a password more than this length; anything more than 40 is probably
  // overkill anyway)
  junkString := '';
  randomize;
  for i:=0 to 1024 do
    begin
    junkString := junkString + chr(random(255));
    end;

  // ...overwrite any passwords entered...
  mePassword1.text := junkString;
  mePassword2.text := junkString;
  mePassword3.text := junkString;
  mePassword4.text := junkString;

  // ...and then reset to a zero length string, just to be tidy.
  mePassword1.text := '';
  mePassword2.text := '';
  mePassword3.text := '';
  mePassword4.text := '';

end;

procedure TOTFEScramDiskPasswordEntry_F.FormDestroy(Sender: TObject);
begin
  ClearEnteredPasswords();

end;


procedure TOTFEScramDiskPasswordEntry_F.pbKeydiskClick(Sender: TObject);
var
  openDialog: TOpenDialog;
  fileHandle: TextFile;
  aPassword: string;
begin
  openDialog := TOpenDialog.Create(nil);
  try
    if openDialog.Execute() then
      begin
      AssignFile(fileHandle, openDialog.Filename);
      Reset(fileHandle);

      Readln(fileHandle, aPassword);
      mePassword1.text := aPassword;
      Readln(fileHandle, aPassword);
      mePassword2.text := aPassword;
      Readln(fileHandle, aPassword);
      mePassword3.text := aPassword;
      Readln(fileHandle, aPassword);
      mePassword4.text := aPassword;

      Readln(fileHandle, aPassword);
      if aPassword<>'KeepDialog' then
        begin
        ModalResult := mrOK;
        end;

      end; // OpenDialog.Execute()
  finally
    CloseFile(fileHandle);
    openDialog.Free();
  end;

end;

{
// This is a binary version of the above function
procedure TPasswordEntry_F.pbKeydiskClick(Sender: TObject);
var
  openDialog: TOpenDialog;
  fileHandle: TFileStream;
  blankingBytes: array [0..ONE_PASSWORD_LENGTH] of byte;
  i: integer;
  bytesRead: integer;
begin
  openDialog := TOpenDialog.Create(nil);
  try
    if openDialog.Execute() then
      begin
      fileHandle := TFileStream.Create(openDialog.Filename, fmOpenRead);

      bytesRead := fileHandle.Read(blankingBytes, ONE_PASSWORD_LENGTH);
      mePassword1.text := '';
      for i:=1 to min(ONE_PASSWORD_LENGTH, bytesRead) do
        begin
        mePassword1.text := mePassword1.text + char(blankingBytes[i-1]);
        end;

      bytesRead := fileHandle.Read(blankingBytes, ONE_PASSWORD_LENGTH);
      mePassword2.text := '';
      for i:=1 to min(ONE_PASSWORD_LENGTH, bytesRead) do
        begin
        mePassword2.text := mePassword2.text + char(blankingBytes[i-1]);
        end;

      bytesRead := fileHandle.Read(blankingBytes, ONE_PASSWORD_LENGTH);
      mePassword3.text := '';
      for i:=1 to min(ONE_PASSWORD_LENGTH, bytesRead) do
        begin
        mePassword3.text := mePassword3.text + char(blankingBytes[i-1]);
        end;

      bytesRead := fileHandle.Read(blankingBytes, ONE_PASSWORD_LENGTH);
      mePassword4.text := '';
      for i:=1 to min(ONE_PASSWORD_LENGTH, bytesRead) do
        begin
        mePassword4.text := mePassword4.text + char(blankingBytes[i-1]);
        end;

      ModalResult := mrOK;

      end; // OpenDialog.Execute()
  finally
    fileHandle.free;
    openDialog.Free();
  end;

end;
}

procedure TOTFEScramDiskPasswordEntry_F.pbKeyboard1Click(Sender: TObject);
begin
  if KeyboardDialog.execute() then
    begin
    mePassword1.text := KeyboardDialog.password;
    end;

  KeyboardDialog.BlankPassword();

end;

procedure TOTFEScramDiskPasswordEntry_F.pbKeyboard2Click(Sender: TObject);
begin
  if KeyboardDialog.execute() then
    begin
    mePassword2.text := KeyboardDialog.password;
    end;

  KeyboardDialog.BlankPassword();

end;

procedure TOTFEScramDiskPasswordEntry_F.pbKeyboard3Click(Sender: TObject);
begin
  if KeyboardDialog.execute() then
    begin
    mePassword3.text := KeyboardDialog.password;
    end;

  KeyboardDialog.BlankPassword();

end;

procedure TOTFEScramDiskPasswordEntry_F.pbKeyboard4Click(Sender: TObject);
begin
  if KeyboardDialog.execute() then
    begin
    mePassword4.text := KeyboardDialog.password;
    end;

  KeyboardDialog.BlankPassword();

end;

procedure TOTFEScramDiskPasswordEntry_F.ckHidePasswordsClick(Sender: TObject);
var
  passwordChar: char;
begin
  passwordChar := #0;
  if ckHidePasswords.checked then
    begin
    passwordChar := '*';
    end;

  mePassword1.passwordchar := passwordChar;
  mePassword2.passwordchar := passwordChar;
  mePassword3.passwordchar := passwordChar;
  mePassword4.passwordchar := passwordChar;

end;

END.

