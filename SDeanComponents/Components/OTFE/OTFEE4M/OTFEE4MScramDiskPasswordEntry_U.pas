unit OTFEE4MScramDiskPasswordEntry_U;
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
  TOTFEE4MScramDiskPasswordEntry_F = class(TForm)
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
    cbDrives: TComboBox;
    Label5: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure pbKeydiskClick(Sender: TObject);
    procedure pbKeyboard1Click(Sender: TObject);
    procedure pbKeyboard2Click(Sender: TObject);
    procedure pbKeyboard3Click(Sender: TObject);
    procedure pbKeyboard4Click(Sender: TObject);
    procedure ckHidePasswordsClick(Sender: TObject);
  public
    FDefaultDrive: char;
    procedure ClearEnteredPasswords();
    procedure SetDrivesAllowed(drvs: string);
    procedure SetDrive(dfltDrv: char);
    function  GetDrive(): char;
  published
    property Drive: char read GetDrive write SetDrive;
    property DrivesAllowed: string write SetDrivesAllowed;
  end;

implementation

{$R *.DFM}

uses Math, SDUGeneral;

const
  ONE_PASSWORD_LENGTH = 40;

// Presumably this should be enough to overwrite the relevant strings in memory?
procedure TOTFEE4MScramDiskPasswordEntry_F.ClearEnteredPasswords();
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

procedure TOTFEE4MScramDiskPasswordEntry_F.FormDestroy(Sender: TObject);
begin
  ClearEnteredPasswords();

end;


procedure TOTFEE4MScramDiskPasswordEntry_F.pbKeydiskClick(Sender: TObject);
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

procedure TOTFEE4MScramDiskPasswordEntry_F.pbKeyboard1Click(Sender: TObject);
begin
  if KeyboardDialog.execute() then
    begin
    mePassword1.text := KeyboardDialog.password;
    end;

  KeyboardDialog.BlankPassword();

end;

procedure TOTFEE4MScramDiskPasswordEntry_F.pbKeyboard2Click(Sender: TObject);
begin
  if KeyboardDialog.execute() then
    begin
    mePassword2.text := KeyboardDialog.password;
    end;

  KeyboardDialog.BlankPassword();

end;

procedure TOTFEE4MScramDiskPasswordEntry_F.pbKeyboard3Click(Sender: TObject);
begin
  if KeyboardDialog.execute() then
    begin
    mePassword3.text := KeyboardDialog.password;
    end;

  KeyboardDialog.BlankPassword();

end;

procedure TOTFEE4MScramDiskPasswordEntry_F.pbKeyboard4Click(Sender: TObject);
begin
  if KeyboardDialog.execute() then
    begin
    mePassword4.text := KeyboardDialog.password;
    end;

  KeyboardDialog.BlankPassword();

end;

procedure TOTFEE4MScramDiskPasswordEntry_F.ckHidePasswordsClick(Sender: TObject);
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

procedure TOTFEE4MScramDiskPasswordEntry_F.SetDrive(dfltDrv: char);
begin
  FDefaultDrive:= dfltDrv;
  dfltDrv := (uppercase(dfltDrv))[1];

  // This will ensure that we either have the default drive selected, or the
  // first drive
  if cbDrives.items.IndexOf(dfltDrv+':')>-1 then
    begin
    cbDrives.itemindex := cbDrives.items.IndexOf(dfltDrv+':');
    end
  else
    begin
    cbDrives.itemindex := 0;
    end;

end;

function TOTFEE4MScramDiskPasswordEntry_F.GetDrive(): char;
begin
  if cbDrives.items.count<1 then
    begin
    Result := #0
    end
  else
    begin
    Result := cbDrives.text[1];
    end;

end;

procedure TOTFEE4MScramDiskPasswordEntry_F.SetDrivesAllowed(drvs: string);
var
  i: integer;
begin
  // Setup the drives the user is allowed to select
  for i:=1 to length(drvs) do
    begin
    cbDrives.items.Add(drvs[i]+':');
    end;

  cbDrives.sorted := TRUE;

  SetDrive(FDefaultDrive);

end;

END.

