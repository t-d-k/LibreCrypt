unit OTFEFreeOTFE_frmKeyEntryLUKS;
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
  StdCtrls, ComCtrls, ExtCtrls, PasswordRichEdit, Spin64,
  SDUForms,
  OTFEFreeOTFE_U,  // Required for TFreeOTFEMountAs
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_DriverAPI, OTFEFreeOTFE_PasswordRichEdit, SDUFrames,
  SDUSpin64Units, SDUStdCtrls,  // Required for TFreeOTFESectorIVGenMethod and NULL_GUID
  SDUGeneral,
  SDUFilenameEdit_U, SDUDropFiles, OTFEFreeOTFE_LUKSKeyOrKeyfileEntry;

type
  TfrmKeyEntryLUKS = class(TSDUForm)
    pbCancel: TButton;
    pbOK: TButton;
    GroupBox2: TGroupBox;
    lblDrive: TLabel;
    lblMountAs: TLabel;
    cbDrive: TComboBox;
    ckMountReadonly: TSDUCheckBox;
    cbMediaType: TComboBox;
    GroupBox4: TGroupBox;
    Label10: TLabel;
    Label22: TLabel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ckBaseIVCypherOnHashLength: TSDUCheckBox;
    ckMountForAllUsers: TSDUCheckBox;
    se64UnitSizeLimit: TSDUSpin64Unit_Storage;
    OTFEFreeOTFELUKSKeyOrKeyfileEntry1: TOTFEFreeOTFELUKSKeyOrKeyfileEntry;
    procedure FormCreate(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
    procedure SelectionChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure preUserkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure pbCancelClick(Sender: TObject);
    procedure ckSelected(Sender: TObject);
  private
  protected
    // These are ordered lists corresponding to the items shown in the combobox
    hashKernelModeDriverNames: TStringList;
    hashGUIDs: TStringList;
    cypherKernelModeDriverNames: TStringList;
    cypherGUIDs: TStringList;

    procedure PopulateDrives();
    procedure PopulateMountAs();

    procedure DefaultOptions();

    procedure EnableDisableControls();

    procedure DoCancel();

  public
    FreeOTFEObj: TOTFEFreeOTFEBase;

    procedure Initialize();

    // Key...
    function  GetKey(var userKey: Ansistring): boolean;
    function  SetKey(userKey: Ansistring): boolean;
    function  SetKeyfile(filename: string): boolean;
    function  GetKeyfileIsASCII(var isASCII: boolean): boolean;
    function  SetKeyfileIsASCII(isASCII: boolean): boolean;
    function  GetKeyfileNewlineType(var nlType: TSDUNewline): boolean;
    function  SetKeyfileNewlineType(nlType: TSDUNewline): boolean;

    function  GetIVCypherBase(var baseIVCypherOnHashLength: boolean): boolean;
    function  SetIVCypherBase(baseIVCypherOnHashLength: boolean): boolean;

    // File options...
    function  GetSizeLimit(var fileOptSizeLimit: int64): boolean;
    function  SetSizeLimit(fileOptSizeLimit: int64): boolean;

    // Mount options...
    function  GetDriveLetter(var mountDriveLetter: ansichar): boolean;
    function  SetDriveLetter(mountDriveLetter: ansichar): boolean;
    function  GetReadonly(var mountReadonly: boolean): boolean;
    function  SetReadonly(mountReadonly: boolean): boolean;
    function  GetMountAs(var mountAs: TFreeOTFEMountAs): boolean;
    function  SetMountAs(mountAs: TFreeOTFEMountAs): boolean;
    function  GetMountForAllUsers(): boolean;
    procedure SetMountForAllUsers(allUsers: boolean);
  end;


implementation

{$R *.DFM}


uses
  ComObj,  // Required for StringToGUID
  OTFEFreeOTFE_VolumeFileAPI,  // Required for SCTRIVGEN_USES_SECTOR_ID and SCTRIVGEN_USES_HASH
  OTFEFreeOTFEDLL_U,
  SDUi18n,
  SDUDialogs,
  INIFiles;


procedure TfrmKeyEntryLUKS.FormCreate(Sender: TObject);
begin
  hashKernelModeDriverNames:= TStringList.Create();
  hashGUIDs:= TStringList.Create();
  cypherKernelModeDriverNames:= TStringList.Create();
  cypherGUIDs:= TStringList.Create();

end;


procedure TfrmKeyEntryLUKS.PopulateDrives();
var
  driveLetters: ansistring;
  i: integer;
begin
  cbDrive.Items.Clear();
  cbDrive.Items.Add(_('Use default'));
  driveLetters := SDUGetUnusedDriveLetters();
  for i:=1 to length(driveLetters) do
    begin
    // Skip the drive letters traditionally reserved for floppy disk drives
//    if (
//        (driveLetters[i] <> 'A') AND
//        (driveLetters[i] <> 'B')
//       ) then
//      begin
      cbDrive.Items.Add(driveLetters[i]+':');
//      end;
    end;

end;


procedure TfrmKeyEntryLUKS.PopulateMountAs();
var
  currMountAs: TFreeOTFEMountAs;
begin
  cbMediaType.Items.Clear();
  for currMountAs:=low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do
    begin
    if (currMountAs <> fomaUnknown) then
      begin
      cbMediaType.Items.Add(FreeOTFEMountAsTitle(currMountAs));
      end;

    end;

end;

procedure TfrmKeyEntryLUKS.DefaultOptions();
var
  i: integer;
  currDriveLetter: ansichar;
begin
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.DefaultOptions();

  if (FreeOTFEObj is TOTFEFreeOTFE) then
    begin
    if (cbDrive.Items.Count>0) then
      begin
      cbDrive.ItemIndex := 0;

      if (TOTFEFreeOTFE(FreeOTFEObj).DefaultDriveLetter <> #0) then
        begin
        // Start from 1; skip the default
        for i:=1 to (cbDrive.items.count-1) do
          begin
          currDriveLetter := ansichar(cbDrive.Items[i][1]);
          if (currDriveLetter >= TOTFEFreeOTFE(FreeOTFEObj).DefaultDriveLetter) then
            begin
            cbDrive.ItemIndex := i;
            break;
            end;
          end;
        end;
      end;
    end;

  if (FreeOTFEObj is TOTFEFreeOTFE) then
    begin
    SetMountAs(TOTFEFreeOTFE(FreeOTFEObj).DefaultMountAs);
    end
  else
    begin
    SetMountAs(fomaFixedDisk);
    end;

  ckBaseIVCypherOnHashLength.checked := TRUE;

  se64UnitSizeLimit.Value := 0;

  // Default to TRUE to allow formatting under Windows Vista
  ckMountForAllUsers.checked := TRUE;

end;


procedure TfrmKeyEntryLUKS.pbOKClick(Sender: TObject);
var
  tmpKey: Ansistring;
  msgZeroLenKey: string;
begin
  if GetKey(tmpKey) then
    begin
    if (Length(tmpKey)=0) then
      begin
      if OTFEFreeOTFELUKSKeyOrKeyfileEntry1.KeyIsUserEntered() then
        begin
        msgZeroLenKey := _('You have not entered a password.');
        end
      else
        begin
        msgZeroLenKey := _('Keyfile contained a zero-length key.');
        end;

      if (SDUMessageDlg(
                 msgZeroLenKey+SDUCRLF+
                 SDUCRLF+
                 _('Are you sure you wish to proceed?'),
                 mtConfirmation,
                 [mbYes, mbNo],
                 0
                ) = mrYes) then
        begin
        ModalResult := mrOK;
        end;
      end
    else
      begin
      // No problems with the password as entered; OK to close the dialog
      ModalResult := mrOK;
      end;

    end  // if GetKey(tmpKey) then
  else
    begin
    if not(OTFEFreeOTFELUKSKeyOrKeyfileEntry1.KeyIsUserEntered()) then
      begin
      SDUMessageDlg(_('Unable to read keyfile.'), mtError);
      end;

    end;


end;


procedure TfrmKeyEntryLUKS.EnableDisableControls();
var
  junkInt64: int64;
  junkChar: ansichar;
  mountAsOK: boolean;
  tmpMountAs: TFreeOTFEMountAs;
begin
  // Ensure we know what to mount as
  mountAsOK := GetMountAs(tmpMountAs);
  ckMountReadonly.Enabled := FALSE;
  if (mountAsOK) then
    begin
    if not(FreeOTFEMountAsCanWrite[tmpMountAs]) then
      begin
      ckMountReadonly.checked := TRUE;
      end;
    SDUEnableControl(ckMountReadonly, FreeOTFEMountAsCanWrite[tmpMountAs]);
    end;
  mountAsOK := mountAsOK AND (tmpMountAs <> fomaUnknown);

  pbOK.Enabled := (
                   (GetSizeLimit(junkInt64))     AND
                   (GetDriveLetter(junkChar))    AND
                   mountAsOK
                  );

end;

procedure TfrmKeyEntryLUKS.SelectionChange(Sender: TObject);
begin
  EnableDisableControls();

end;

procedure TfrmKeyEntryLUKS.FormShow(Sender: TObject);
begin
  // Position cursor to the *end* of any password
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.CursorToEndOfPassword();

  // Certain controls only visble if used in conjunction with drive mounting
  lblDrive.Visible := FreeOTFEObj is TOTFEFreeOTFE;
  cbDrive.Visible := FreeOTFEObj is TOTFEFreeOTFE;
  lblMountAs.Visible := FreeOTFEObj is TOTFEFreeOTFE;
  cbMediaType.Visible := FreeOTFEObj is TOTFEFreeOTFE;
  ckMountForAllUsers.Visible := FreeOTFEObj is TOTFEFreeOTFE;

  // Prevent making remaining control look odd, stuck in the middle
  if not(FreeOTFEObj is TOTFEFreeOTFE) then
    begin
    ckMountReadonly.top := lblDrive.top;
    ckMountReadonly.left := lblDrive.left;
    end;

  EnableDisableControls();

end;


procedure TfrmKeyEntryLUKS.FormDestroy(Sender: TObject);
begin
  hashKernelModeDriverNames.Free();
  hashGUIDs.Free();
  cypherKernelModeDriverNames.Free();
  cypherGUIDs.Free();

end;

procedure TfrmKeyEntryLUKS.preUserkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) then
    begin
    DoCancel();
    end;

end;

procedure TfrmKeyEntryLUKS.pbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;

end;


procedure TfrmKeyEntryLUKS.DoCancel();
begin
  ModalResult := mrCancel;

end;


function TfrmKeyEntryLUKS.GetIVCypherBase(var baseIVCypherOnHashLength: boolean): boolean;
begin
  baseIVCypherOnHashLength := ckBaseIVCypherOnHashLength.checked;
  Result := TRUE;
end;

function TfrmKeyEntryLUKS.SetIVCypherBase(baseIVCypherOnHashLength: boolean): boolean;
begin
  ckBaseIVCypherOnHashLength.checked := baseIVCypherOnHashLength;
  Result := TRUE;
end;


function TfrmKeyEntryLUKS.GetKey(var userKey: Ansistring): boolean;
begin
  Result := OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKey(userKey);
end;

function TfrmKeyEntryLUKS.SetKey(userKey: Ansistring): boolean;
begin
  Result := OTFEFreeOTFELUKSKeyOrKeyfileEntry1.SetKey(userKey);
end;

function TfrmKeyEntryLUKS.SetKeyfile(filename: string): boolean;
begin
  Result := OTFEFreeOTFELUKSKeyOrKeyfileEntry1.SetKeyfile(filename);
end;

function TfrmKeyEntryLUKS.GetKeyfileIsASCII(var isASCII: boolean): boolean;
begin
  Result := OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyfileIsASCII(isASCII);
end;

function TfrmKeyEntryLUKS.SetKeyfileIsASCII(isASCII: boolean): boolean;
begin
  Result := OTFEFreeOTFELUKSKeyOrKeyfileEntry1.SetKeyfileIsASCII(isASCII);
end;

function TfrmKeyEntryLUKS.GetKeyfileNewlineType(var nlType: TSDUNewline): boolean;
begin
  Result := OTFEFreeOTFELUKSKeyOrKeyfileEntry1.GetKeyfileNewlineType(nlType);
end;

function TfrmKeyEntryLUKS.SetKeyfileNewlineType(nlType: TSDUNewline): boolean;
begin
  Result := OTFEFreeOTFELUKSKeyOrKeyfileEntry1.SetKeyfileNewlineType(nlType);
end;


function TfrmKeyEntryLUKS.GetSizeLimit(var fileOptSizeLimit: int64): boolean;
begin
  fileOptSizeLimit := se64UnitSizeLimit.Value;
  Result := TRUE;
end;

function TfrmKeyEntryLUKS.SetSizeLimit(fileOptSizeLimit: int64): boolean;
begin
  se64UnitSizeLimit.Value := fileOptSizeLimit;
  Result := TRUE;
end;

// Note: This may return #0 as mountDriveLetter to indicate "any"
function TfrmKeyEntryLUKS.GetDriveLetter(var mountDriveLetter: ansichar): boolean;
begin
  mountDriveLetter := #0;
  // Note: The item at index zero is "Use default"; #0 is returned for this
  if (cbDrive.ItemIndex>0) then
    begin
    mountDriveLetter :=ansichar( cbDrive.Items[cbDrive.ItemIndex][1]);
    end;

  Result := TRUE;
end;

// mountDriveLetter - Set to #0 to indicate "Use default"
function TfrmKeyEntryLUKS.SetDriveLetter(mountDriveLetter: ansichar): boolean;
var
  idx: integer;
  retVal: boolean;
begin
  retVal := TRUE;

  if (mountDriveLetter = #0) then
    begin
    // The item at idx 0 will *always* be "Use default"
    idx := 0
    end
  else
    begin
    idx := cbDrive.Items.IndexOf(mountDriveLetter+':');
    end;

  if (idx < 0) then
    begin
    idx := 0;
    retVal := FALSE;
    end;
  cbDrive.ItemIndex := idx;

  Result := retVal;
end;

function TfrmKeyEntryLUKS.GetReadonly(var mountReadonly: boolean): boolean;
begin
  mountReadonly := ckMountReadonly.checked;
  Result := TRUE;
end;

function TfrmKeyEntryLUKS.SetReadonly(mountReadonly: boolean): boolean;
begin
  ckMountReadonly.checked := mountReadonly;
  Result := TRUE;
end;

function TfrmKeyEntryLUKS.GetMountAs(var mountAs: TFreeOTFEMountAs): boolean;
var
  currMountAs: TFreeOTFEMountAs;
  allOK: boolean;
begin
  allOK := FALSE;

  for currMountAs:=low(TFreeOTFEMountAs) to high(TFreeOTFEMountAs) do
    begin
    if (cbMediaType.Items[cbMediaType.ItemIndex] = FreeOTFEMountAsTitle(currMountAs)) then
      begin
      mountAs := currMountAs;
      allOK := TRUE;
      break;
      end;
    end;

  Result := allOK;
end;


function TfrmKeyEntryLUKS.SetMountAs(mountAs: TFreeOTFEMountAs): boolean;
var
  idx: integer;
  allOK: boolean;
begin
  idx := cbMediaType.Items.IndexOf(FreeOTFEMountAsTitle(mountAs));
  cbMediaType.ItemIndex := idx;

  allOK := (idx >= 0);

  Result := allOK;
end;


procedure TfrmKeyEntryLUKS.SetMountForAllUsers(allUsers: boolean);
begin
  ckMountForAllUsers.checked := allUsers;
end;

function  TfrmKeyEntryLUKS.GetMountForAllUsers(): boolean;
begin
  Result := ckMountForAllUsers.checked;
end;


procedure TfrmKeyEntryLUKS.ckSelected(Sender: TObject);
begin
  EnableDisableControls();

end;


procedure TfrmKeyEntryLUKS.Initialize();
begin
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.FreeOTFEObj := FreeOTFEObj;
  OTFEFreeOTFELUKSKeyOrKeyfileEntry1.Initialize();

  PopulateDrives();
  PopulateMountAs();

  DefaultOptions();
end;

END.


