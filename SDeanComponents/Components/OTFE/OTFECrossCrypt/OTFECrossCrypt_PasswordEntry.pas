unit OTFECrossCrypt_PasswordEntry;
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
  StdCtrls, ComCtrls,
  OTFECrossCrypt_DriverAPI;

type
  TOTFECrossCrypt_PasswordEntry_F = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    pbOK: TButton;
    pbCancel: TButton;
    cbDrive: TComboBox;
    Label3: TLabel;
    ckReadonly: TCheckBox;
    ckMountAsCD: TCheckBox;
    cbCipher: TComboBox;
    ckMultipleKey: TCheckBox;
    rePasswords: TRichEdit;
    lblPasswordCount: TLabel;
    pbLoadFromFile: TButton;
    OpenKeyfileDlg: TOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ckMultipleKeyClick(Sender: TObject);
    procedure cbCipherChange(Sender: TObject);
    procedure ckMountAsCDClick(Sender: TObject);
    procedure rePasswordsChange(Sender: TObject);
    procedure pbCancelClick(Sender: TObject);
    procedure pbLoadFromFileClick(Sender: TObject);
  private
    FConfirm: boolean;
    FMinPasswordLength: integer;

    FCipher: TCROSSCRYPT_CIPHER_TYPE;
    FDriveLetter: Ansichar;
    FMountReadOnly: boolean;
    FMountAsCD: boolean;
    FMultipleKey: boolean;
    FPasswords: TStringList;

    FOnlyReadWrite: boolean;

    function  GetUnusedDriveLetters(): string;
    procedure ClearPasswords();

    function  DetermineSelectedCipher(): TCROSSCRYPT_CIPHER_TYPE;

    procedure UpdatePasswordCount();

    procedure WipeGUI();
    procedure WipeInternal();

    procedure EnableDisable();
    procedure UserPasswordMask(mask: boolean);


  public
    // Confirm - if set, the user will be asked to confirm their passwords, or
    //           that they wish to proceed if the cipher they choose is
    //           "cphrNone"
    property Confirm: boolean read FConfirm write FConfirm;

    // MinPasswordLength - if set, the user will additionally be prompted to
    //                     confirm the passwords are correct, if their password
    //                     is shorter than this number of chars.
    //                     Set to 0 to disable this confirmation
    property MinPasswordLength: integer read FMinPasswordLength write FMinPasswordLength;

    property Cipher: TCROSSCRYPT_CIPHER_TYPE read FCipher write FCipher;
    property DriveLetter: Ansichar read FDriveLetter write FDriveLetter;
    property MountReadOnly: boolean read FMountReadOnly write FMountReadOnly;
    property MountAsCD: boolean read FMountAsCD write FMountAsCD;
    property MultipleKey: boolean read FMultipleKey write FMultipleKey;

    // If this is set, the user will not be able to specify "readonly" or "CD"
    property OnlyReadWrite: boolean read FOnlyReadWrite write FOnlyReadWrite;

    procedure GetSinglePassword(var password: Ansistring);
    procedure GetMultiplePasswords(passwords: TStringList);
    procedure AddPassword(password: string);

    // Clear down the dialog's internals...
    procedure Wipe();

  end;


implementation

{$R *.DFM}

uses
  SDUGeneral,
  OTFECrossCrypt_PasswordConfirm;

const
  CRLF = #13+#10;



procedure TOTFECrossCrypt_PasswordEntry_F.FormShow(Sender: TObject);
var
  ct: TCROSSCRYPT_CIPHER_TYPE;
  i: integer;
  drvs: string;
  driveLetterColon: string;
  driveIdx: integer;
begin
  rePasswords.PlainText := TRUE;
  rePasswords.Lines.Clear();

  UserPasswordMask(TRUE);

  rePasswords.Lines.AddStrings(FPasswords);

  for ct:=low(CROSSCRYPT_CIPHER_NAMES) to high(CROSSCRYPT_CIPHER_NAMES) do
    begin
    if (ct<>cphrUnknown) then
      begin
      cbCipher.items.add(CROSSCRYPT_CIPHER_NAMES[ct]);
      end;
    end;

  if (Cipher = cphrUnknown) then
    begin
    Cipher := cphrNone;
    end;
  cbCipher.ItemIndex := ord(Cipher);

  // Setup the drives the user is allowed to select
  drvs := GetUnusedDriveLetters();
  for i:=1 to length(drvs) do
    begin
    if ( (drvs[i] <> 'A') and (drvs[i] <> 'B') ) then
      begin
      cbDrive.items.Add(drvs[i]+':');
      end;
    end;

  cbDrive.sorted := TRUE;

  // Select the appropriate drive letter...
  driveLetterColon := DriveLetter+':';
  driveIdx := cbDrive.Items.IndexOf(driveLetterColon);
  if (driveIdx < 0) then
    begin
    driveIdx := 0;
    end;

  cbDrive.ItemIndex := driveIdx;

  ckReadonly.checked := MountReadOnly;
  ckMountAsCD.checked := MountAsCD;
  ckMultipleKey.checked := MultipleKey;

  EnableDisable();

end;

procedure TOTFECrossCrypt_PasswordEntry_F.pbOKClick(Sender: TObject);
var
  tmpDriveStr: Ansistring;
  allOK: boolean;
  confirmDlg: TOTFECrossCrypt_PasswordConfirm_F;
  confirmMultiplePw: TStringList;
  confirmSinglePw: Ansistring;
  i: integer;
  origSinglePw: Ansistring;
  msgDlgReply: word;
  askUser: boolean;
begin
  allOK := TRUE;

  // Ensure that in multikey mode, we have MULTIKEY_PASSWORD_REQUIREMENT passwords
  if ckMultipleKey.checked and (Cipher<>cphrNone)then
    begin
    if (rePasswords.Lines.count<MULTIKEY_PASSWORD_REQUIREMENT) then
      begin
      MessageDlg('You must enter a total of '+inttostr(MULTIKEY_PASSWORD_REQUIREMENT)+' passwords, one per line.'+CRLF+
                   CRLF+
                   'You have entered '+inttostr(rePasswords.Lines.count)+' passwords.'+CRLF+
                   CRLF+
                   'Please correct, and try again.',
                 mtError, [mbOK], 0);

      allOK := FALSE;
      end;

    end;


  // Transfer the data stored in the GUI into the internal store
  if allOK then
    begin
    FPasswords.Clear();
    FPasswords.AddStrings(rePasswords.Lines);

    Cipher := DetermineSelectedCipher();

    tmpDriveStr := cbDrive.items[cbDrive.ItemIndex];
    DriveLetter := tmpDriveStr[1];

    MountReadOnly := ckReadonly.checked;
    MountAsCD := ckMountAsCD.checked;
    MultipleKey := ckMultipleKey.checked;
    end;


  // If we are to confirm the user's passwords, we confirm if the user selected
  // "none" as the encryption algorithm
  if allOK then
    begin
    if Confirm and (Cipher=cphrNone) then
      begin
      msgDlgReply := MessageDlg(
                                'You have selected "'+cbCipher.Items[cbCipher.ItemIndex]+'" as the encryption algorithm.'+CRLF+
                                CRLF+
                                'This means that although CrossCrypt will operate as normal,'+CRLF+
                                'no actual encryption of your data will take place.'+CRLF+
                                CRLF+
                                'Do you wish to continue?',
                                mtConfirmation,
                                [mbYes, mbNo],
                                0
                               );
      allOK := (msgDlgReply = mrYes);
      end;

    end;


  // If we're supposed to get the user to confirm short password(s)...
  if (allOK) then
    begin
    if (MinPasswordLength>0) then
      begin
      msgDlgReply := mrYes;
      if (MultipleKey) then
        begin
        confirmMultiplePw := TStringList.Create();
        try
          GetMultiplePasswords(confirmMultiplePw);
          askUser:= FALSE;
          for i:=0 to (confirmMultiplePw.count-1) do
            begin
            if (length(confirmMultiplePw[i]) < MinPasswordLength) then
              begin
              askUser:= TRUE;
              break;
              end;
            end;

        finally
          confirmMultiplePw.Text := StringOfChar('X', length(confirmMultiplePw.text));
          confirmMultiplePw.Free();
        end;

        // No point in warning the user if there's no encryption!
        if askUser and (Cipher<>cphrNone) then
          begin
          msgDlgReply := MessageDlg('One or more of the keyphrases you entered is less than '+inttostr(MinPasswordLength)+' characters long.'+CRLF+
                                     CRLF+
                                     'In order to be able to use this box with Linux, all of'+CRLF+
                                     'your keyphrases must be longer than '+inttostr(MinPasswordLength)+' characters long.'+CRLF+
                                     CRLF+
                                     'Do you wish to proceed?',
                                     mtWarning,
                                     [mbYes, mbNo],
                                     0
                                    );
          end;

        end
      else
        begin
        GetSinglePassword(origSinglePw);
        // No point in warning the user if there's no encryption!
        if (Length(origSinglePw) < MinPasswordLength) and (Cipher<>cphrNone) then
          begin
          msgDlgReply := MessageDlg('The keyphrase you entered is less than '+inttostr(MinPasswordLength)+' characters long.'+CRLF+
                                     CRLF+
                                     'In order to be able to use this box with Linux,'+CRLF+
                                     'your keyphrase must be longer than '+inttostr(MinPasswordLength)+' characters long.'+CRLF+
                                     CRLF+
                                     'Do you wish to proceed?',
                                     mtWarning,
                                     [mbYes, mbNo],
                                     0
                                    );
          end;
        origSinglePw := StringOfChar('X', length(origSinglePw));
        end;

      allOK := (msgDlgReply = mrYes);
      end;
      
    end;


  // If we are to confirm the user's passwords, do that now...
  if allOK then
    begin
    // Note: We don't confirm if there's no encryption!
    if Confirm and (Cipher<>cphrNone) then
      begin
      // Switch to assuming password mismatch
      allOK := FALSE;

      confirmDlg := TOTFECrossCrypt_PasswordConfirm_F.Create(nil);
      try
        confirmDlg.MultipleKey := MultipleKey;
        if (confirmDlg.ShowModal = mrOK) then
          begin
          if (MultipleKey) then
            begin
            confirmMultiplePw := TStringList.Create();
            try
              confirmDlg.GetMultiplePasswords(confirmMultiplePw);
              if (confirmMultiplePw.count = rePasswords.lines.count) then
                begin
                // Back to assuming allOK = TRUE;
                allOK := TRUE;
                for i:=0 to (confirmMultiplePw.count-1) do
                  begin
                  if (confirmMultiplePw[i] <> rePasswords.lines[i]) then
                    begin
                    allOK := FALSE;
                    break;
                    end;
                  end;

                end;

            finally
              confirmMultiplePw.Text := StringOfChar('X', length(confirmMultiplePw.text));
              confirmMultiplePw.Free();
            end;
            end
          else
            begin
            confirmDlg.GetSinglePassword(confirmSinglePw);
            GetSinglePassword(origSinglePw);

            if (origSinglePw = confirmSinglePw) then
              begin
              allOK := TRUE;
              end;

            origSinglePw := StringOfChar('X', length(origSinglePw));
            confirmSinglePw := StringOfChar('X', length(confirmSinglePw));

            end;

          if (not(allOK)) then
            begin
            MessageDlg('The passwords you entered do not match.'+#13+#10+''+#13+#10+'Please try again.', mtError, [mbOK], 0);
            end;

          end;  // if (confirmDlg.ShowModal = mrOK) then

      finally
        confirmDlg.Wipe();
        confirmDlg.Free();
      end;

      end;  // if Confirm then

    end;  // if allOK then



  if allOK then
    begin
    // All OK, exit...
    ModalResult := mrOK;
    end;

end;


// Returns a string containing the drive letters of "unallocated" drives
function TOTFECrossCrypt_PasswordEntry_F.GetUnusedDriveLetters(): string;
var
  driveLetters: string;
  DriveNum: Integer;
  DriveBits: set of 0..25;
begin
  driveLetters := '';

  Integer(DriveBits) := GetLogicalDrives();
  for DriveNum := 0 to 25 do
    begin
    if not(DriveNum in DriveBits) then
      begin
      driveLetters := driveLetters + Char(DriveNum + Ord('A'));
      end;
    end;

  Result := driveLetters;

end;


function TOTFECrossCrypt_PasswordEntry_F.DetermineSelectedCipher(): TCROSSCRYPT_CIPHER_TYPE;
begin
  Result := TCROSSCRYPT_CIPHER_TYPE(cbCipher.ItemIndex);
end;


procedure TOTFECrossCrypt_PasswordEntry_F.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  WipeGUI();
  
end;


procedure TOTFECrossCrypt_PasswordEntry_F.Wipe();
begin
  WipeInternal();

end;


procedure TOTFECrossCrypt_PasswordEntry_F.WipeGUI();
begin
  // Cleardown the GUI components...
  rePasswords.Text := StringOfChar('X', length(rePasswords.text));
  cbCipher.ItemIndex := 0;
  cbDrive.ItemIndex := 0;
  ckReadonly.checked := FALSE;
  ckMountAsCD.checked := FALSE;

end;


procedure TOTFECrossCrypt_PasswordEntry_F.WipeInternal();
begin
  // Cleardown the internal store...
  ClearPasswords();
  FCipher := low(TCROSSCRYPT_CIPHER_TYPE);
  FDriveLetter := #0;
  FMountReadonly := FALSE;
  FMountAsCD := FALSE;
  FMultipleKey := FALSE;

end;


procedure TOTFECrossCrypt_PasswordEntry_F.ClearPasswords();
var
  i: integer;
begin
  for i:=0 to (FPasswords.count-1) do
    begin
    FPasswords[i] := StringOfChar('X', length(FPasswords[i]));
    end;
    
  FPasswords.Clear();

end;


// Get the single password
procedure TOTFECrossCrypt_PasswordEntry_F.GetSinglePassword(var password: Ansistring);
begin
  password := FPasswords.Text;  { TODO 1 -otdk -cbug : alert user if use unicode }

  // Trim off the CRLF
  Delete(password, (Length(password)-1), 2);

end;


// Returns '' on failure
procedure TOTFECrossCrypt_PasswordEntry_F.GetMultiplePasswords(passwords: TStringList);
begin
  passwords.Text := StringOfChar('X', length(passwords.text));
  passwords.Clear();

  passwords.AddStrings(FPasswords);
end;


procedure TOTFECrossCrypt_PasswordEntry_F.AddPassword(password: string);
begin
  FPasswords.Add(password);
end;


procedure TOTFECrossCrypt_PasswordEntry_F.FormCreate(Sender: TObject);
begin
  FPasswords:= TStringList.Create();

  // Initialize...
  Confirm := FALSE;
  MinPasswordLength := 0;

  Cipher := cphrAES256;
  DriveLetter:= #0;
  MountReadOnly:= FALSE;
  MountAsCD:= FALSE;
  MultipleKey:= FALSE;

end;


procedure TOTFECrossCrypt_PasswordEntry_F.ckMultipleKeyClick(Sender: TObject);
begin
  // If we're only accepting a single password, wordwrap; if multiple
  // passwords, then they need to be entered one per line.
  rePasswords.WordWrap := not(ckMultipleKey.checked);
  UpdatePasswordCount();

  if ckMultipleKey.checked then
    begin
    MessageDlg(
               'Please enter your '+inttostr(MULTIKEY_PASSWORD_REQUIREMENT)+' passwords, one password per line',
               mtInformation,
               [mbOK],
               0
              );
    end;

  UserPasswordMask(TRUE);

end;


procedure TOTFECrossCrypt_PasswordEntry_F.cbCipherChange(Sender: TObject);
begin
  // Enable, disable controls as appropriate...
  EnableDisable();

end;


procedure TOTFECrossCrypt_PasswordEntry_F.ckMountAsCDClick(Sender: TObject);
begin
  // Enable, disable controls as appropriate...
  EnableDisable();

end;


procedure TOTFECrossCrypt_PasswordEntry_F.EnableDisable();
begin
  // Enable, disable controls as appropriate...
  if (DetermineSelectedCipher() = cphrUnknown) OR
     (DetermineSelectedCipher() = cphrNone) then
    begin
    ckMultipleKey.checked := FALSE;
    SDUEnableControl(rePasswords, FALSE);
    SDUEnableControl(ckMultipleKey, FALSE);
    end
  else
    begin
    SDUEnableControl(rePasswords, TRUE);
    SDUEnableControl(ckMultipleKey, TRUE);
    end;


  if OnlyReadWrite then
    begin
    ckMountAsCD.checked := FALSE;
    ckReadonly.checked := FALSE;
    SDUEnableControl(ckMountAsCD, FALSE);
    SDUEnableControl(ckReadonly, FALSE);
    end
  else
    begin
    SDUEnableControl(ckMountAsCD, TRUE);
    if ckMountAsCD.checked then
      begin
      ckReadonly.checked := TRUE;
      SDUEnableControl(ckReadonly, FALSE);
      end
    else
      begin
      ckReadonly.checked := FALSE;
      SDUEnableControl(ckReadonly, TRUE);
      end;
      
    end;

  UserPasswordMask(TRUE);
  
end;


procedure TOTFECrossCrypt_PasswordEntry_F.rePasswordsChange(Sender: TObject);
begin
  UpdatePasswordCount();

end;


procedure TOTFECrossCrypt_PasswordEntry_F.UpdatePasswordCount();
begin
  if (ckMultipleKey.checked) then
    begin
    lblPasswordCount.caption := 'Passwords entered: '+inttostr(rePasswords.lines.count)+'/'+inttostr(MULTIKEY_PASSWORD_REQUIREMENT);
    end
  else
    begin
    lblPasswordCount.caption := '';
    end;

end;


procedure TOTFECrossCrypt_PasswordEntry_F.pbCancelClick(Sender: TObject);
begin
  // Cleardown...
  Wipe();

  ModalResult := mrCancel;

end;


procedure TOTFECrossCrypt_PasswordEntry_F.pbLoadFromFileClick(
  Sender: TObject);
begin
  if OpenKeyfileDlg.Execute() then
    begin
    rePasswords.Lines.LoadFromFile(OpenKeyfileDlg.Filename);
    end;

end;

procedure TOTFECrossCrypt_PasswordEntry_F.UserPasswordMask(mask: boolean);
var
  maskChar: char;
begin
  // Mask the user's password...
  maskChar := #0;
  if mask then
    begin
    maskChar := '*';
    end;

  SendMessage(rePasswords.Handle, EM_SETPASSWORDCHAR, Ord(maskChar), 0);

end;

END.



