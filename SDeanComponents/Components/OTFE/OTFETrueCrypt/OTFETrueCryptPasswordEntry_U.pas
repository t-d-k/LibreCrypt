unit OTFETrueCryptPasswordEntry_U;
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
  StdCtrls, Mask;

type
  TOTFETrueCryptPasswordEntry_F = class(TForm)
    Label1: TLabel;
    mePassword: TMaskEdit;
    Label2: TLabel;
    cbDrives: TComboBox;
    ckHidePassword: TCheckBox;
    pbOK: TButton;
    pbCancel: TButton;
    ckCachePasswordInDriver: TCheckBox;
    ckForceMount: TCheckBox;
    ckMountReadonly: TCheckBox;
    ckMountAsRemovable: TCheckBox;
    procedure ckHidePasswordClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FDefaultDrive: char;
    FUserCanForceMount: boolean;
    FUserCanMountRemovable: boolean;
    FUserCanMountReadonly: boolean;
    procedure SetDrivesAllowed(drvs: string);
    procedure SetDrive(dfltDrv: char);
    function  GetDrive(): char;
    procedure SetDriverCachePassword(cache: boolean);
    function  GetDriverCachePassword(): boolean;
    function  GetMountReadonly(): boolean;
    procedure SetMountReadonly(readonly: boolean);
    function  GetMountAsRemovable(): boolean;
    procedure SetMountAsRemovable(removable: boolean);
    function  GetForceMount(): boolean;
    procedure SetForceMount(forceMount: boolean);
  public
    procedure ClearEnteredPassword();
  published
    property Drive: char read GetDrive write SetDrive;
    property DrivesAllowed: string write SetDrivesAllowed;

    property UserCanMountReadonly: boolean read FUserCanMountReadonly write FUserCanMountReadonly;
    property UserCanMountRemovable: boolean read FUserCanMountRemovable write FUserCanMountRemovable;
    property UserCanForceMount: boolean read FUserCanForceMount write FUserCanForceMount;

    property DriverCachePassword: boolean read GetDriverCachePassword write SetDriverCachePassword;
    property MountReadonly: boolean read GetMountReadonly write SetMountReadonly;
    property MountAsRemovable: boolean read GetMountAsRemovable write SetMountAsRemovable;
    property ForceMount: boolean read GetForceMount write SetForceMount;
  end;

implementation

{$R *.DFM}

uses SDUGeneral;

procedure TOTFETrueCryptPasswordEntry_F.ckHidePasswordClick(Sender: TObject);
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
procedure TOTFETrueCryptPasswordEntry_F.ClearEnteredPassword();
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

procedure TOTFETrueCryptPasswordEntry_F.SetDrive(dfltDrv: char);
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

function TOTFETrueCryptPasswordEntry_F.GetDrive(): char;
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

procedure TOTFETrueCryptPasswordEntry_F.SetDrivesAllowed(drvs: string);
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

procedure TOTFETrueCryptPasswordEntry_F.FormCreate(Sender: TObject);
begin
  // Set defaults which will work with *all* versions of TrueCrypt
  ForceMount       := FALSE;
  MountReadonly    := FALSE;
  MountAsRemovable := FALSE;

end;


procedure  TOTFETrueCryptPasswordEntry_F.SetDriverCachePassword(cache: boolean);
begin
  ckCachePasswordInDriver.checked := cache;
end;

function TOTFETrueCryptPasswordEntry_F.GetDriverCachePassword(): boolean;
begin
  Result := ckCachePasswordInDriver.checked;
end;

function TOTFETrueCryptPasswordEntry_F.GetForceMount(): boolean;
begin
  Result := ckForceMount.checked;

end;

procedure TOTFETrueCryptPasswordEntry_F.SetForceMount(forceMount: boolean);
begin
  ckForceMount.checked := forceMount;
end;


function TOTFETrueCryptPasswordEntry_F.GetMountReadonly(): boolean;
begin
  Result := ckMountReadonly.checked;
end;

procedure TOTFETrueCryptPasswordEntry_F.SetMountReadonly(readonly: boolean);
begin
  ckMountReadonly.checked := readonly;
end;

function TOTFETrueCryptPasswordEntry_F.GetMountAsRemovable(): boolean;
begin
  Result := ckMountAsRemovable.checked;
end;

procedure TOTFETrueCryptPasswordEntry_F.SetMountAsRemovable(removable: boolean);
begin
  ckMountAsRemovable.checked := removable;
end;

procedure TOTFETrueCryptPasswordEntry_F.FormShow(Sender: TObject);
begin
  ckMountReadonly.Enabled    := UserCanMountReadonly;
  ckMountAsRemovable.Enabled := UserCanMountRemovable;
  ckForceMount.Enabled       := UserCanForceMount;

end;

END.

