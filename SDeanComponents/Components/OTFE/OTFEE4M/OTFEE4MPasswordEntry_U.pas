unit OTFEE4MPasswordEntry_U;
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask;

type
  TOTFEE4MPasswordEntry_F = class(TForm)
    Label1: TLabel;
    mePassword: TMaskEdit;
    Label2: TLabel;
    cbDrives: TComboBox;
    ckHidePassword: TCheckBox;
    pbOK: TButton;
    pbCancel: TButton;
    procedure ckHidePasswordClick(Sender: TObject);
  private
    FDefaultDrive: char;
    procedure SetDrivesAllowed(drvs: string);
    procedure SetDrive(dfltDrv: char);
    function  GetDrive(): char;
  public
    procedure ClearEnteredPassword();
  published
    property Drive: char read GetDrive write SetDrive;
    property DrivesAllowed: string write SetDrivesAllowed;
  end;

implementation

{$R *.DFM}

uses SDUGeneral;

procedure TOTFEE4MPasswordEntry_F.ckHidePasswordClick(Sender: TObject);
var
  passwordChar: char;
begin
  passwordChar := #0;
  if ckHidePassword.checked then
    begin
    passwordChar := '*';
    end;

  mePassword.passwordchar := passwordChar;

end;

// Presumably this should be enough to overwrite the relevant strings in memory?
procedure TOTFEE4MPasswordEntry_F.ClearEnteredPassword();
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
  mePassword.text := junkString;

  // ...and then reset to a zero length string, just to be tidy.
  mePassword.text := '';

end;

procedure TOTFEE4MPasswordEntry_F.SetDrive(dfltDrv: char);
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

function TOTFEE4MPasswordEntry_F.GetDrive(): char;
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

procedure TOTFEE4MPasswordEntry_F.SetDrivesAllowed(drvs: string);
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

