unit pkcs11_library;

interface

uses
  Sysutils,
  Classes,
  pkcs11t,
  pkcs11f,
  pkcs11_api,
  pkcs11_slot,
  pkcs11_slot_event_thread;


const
  PKCS11_USER_CANCELED = '<CANCELED>';

type
{$TYPEINFO ON} // Needed to allow "published"
  TPKCS11Library = class(TPKCS11API)
  private
    FDLLHandle: THandle;
    FFilename: string;

    FInitializeCalled: boolean;

    FSlotEventThread: TPKCS11SlotEventThread;
    FOnSlotEvent: TPKCS11SlotEvent;

    FCachedSlotsArray: TPKCS11SlotArray;
  protected

    procedure LoadDLL(DLLfilename: string);
    procedure FreeDLL();

    procedure GetProcs();
    function  LibraryGetFunctionList(): boolean;

    procedure ImportFunctionList();

    function GetSlotList({onlySlotsWithTokenPresent: boolean; }var slotsArray: TPKCS11SlotArray): boolean;
    function GetCountSlots({onlySlotsWithTokenPresent: boolean}): integer;

    function GetSlotByArrayIdx(arrIdx: integer): TPKCS11Slot;
    function GetSlotBySlotID(slotID: integer): TPKCS11Slot;

    function GetInfo(var info: CK_INFO): boolean;

    function GetCryptokiVersion(): CK_VERSION;
    function GetManufacturerID(): string;
    function GetFlags(): CK_FLAGS;
    function GetlibraryDescription(): string;
    function GetlibraryVersion(): CK_VERSION;

    procedure EventThreadStart();
    procedure EventThreadStop();
    procedure EventThreadCallback(Sender: TObject; SlotID: integer);

  public
    constructor Create(filename: string); overload;
    destructor Destroy(); override;

    property Filename: string read FFilename;

    function Initialize(): boolean;
    function Finalize(): boolean;

    property CryptokiVersion: CK_VERSION read GetCryptokiVersion;
    property ManufacturerID: string read GetManufacturerID;
    property Flags: CK_FLAGS read GetFlags;
    property LibraryDescription: string read GetLibraryDescription;
    property LibraryVersion: CK_VERSION read GetLibraryVersion;

    property CountSlots: integer read GetCountSlots;

    // Retrieve a slot by a *Delphi* index; [0..CountSlots-1]
    property Slot[arrayIdx: integer]: TPKCS11Slot read GetSlotByArrayIdx;
    // Retrieve a slot by it's PKCS#11 slot ID
    property SlotByID[slotIdx: integer]: TPKCS11Slot read GetSlotBySlotID;

  published
    property InitializeCalled: boolean read FInitializeCalled;
    property OnSlotEvent: TPKCS11SlotEvent read FOnSlotEvent write FOnSlotEvent;

  end;

// Autodetect PKCS#11 library. If more than one are found, prompt the user
// which one to use
// Returns: One of:
//            *) The DLL filename,
//            *) PKCS11_USER_CANCELED if the user canceled
//            *) An empty string if none can be found
function PKCS11AutoDetectLibrary(AOwner: TComponent): string;
// Verify a PKCS#11 library
// Note: This only carries out a basic check on the library
// Returns: TRUE/FALSE, depending on whether the DLL supplied supports the
//          PKCS#11 interface
function PKCS11VerifyLibrary(LibraryDLL: string): boolean;

implementation


uses
  Windows, Dialogs, Controls, Forms,
  SDUi18n,
  SDUGeneral,
  PKCS11KnownLibs,
  PKCS11LibrarySelectDlg,
  SDUProgressDlg;


function PKCS11AutoDetectLibrary(AOwner: TComponent): string;
var
  dlg: TPKCS11LibrarySelectDialog;
  i: integer;
  cntFound: integer;
  lastFound: TPKCS11KnownLibrary;
  progressDlg: TSDUProgressDialog;
  prevCursor: TCursor;
  canceled: boolean;
begin
  Result := '';

  canceled := FALSE;
  prevCursor := Screen.Cursor;

  dlg:= TPKCS11LibrarySelectDialog.Create(AOwner);
  try
    progressDlg := TSDUProgressDialog.Create(nil);
    try
      progressDlg.Min := 0;
      // Yes, this is correct; it's driverFilenames.count and *not*
      // (driverFilenames.count - 1)
      progressDlg.Max := length(PKCS11_KNOWN_LIBRARIES);
      progressDlg.Position := 0;
      progressDlg.Title := _('Detecting PKCS#11 libraries...');
      progressDlg.ShowStatusText := TRUE;
      progressDlg.Show();
      Screen.Cursor := crAppStart;
      Application.ProcessMessages();

      cntFound := 0;
      for i:=low(PKCS11_KNOWN_LIBRARIES) to high(PKCS11_KNOWN_LIBRARIES) do
        begin
        progressDlg.StatusText := Format(
                                                     _('Checking for: [%s] %s'),
                                                     [
                                                      PKCS11_KNOWN_LIBRARIES[i].DLLFilename,
                                                      PKCS11KnownLibraryPrettyDesc(PKCS11_KNOWN_LIBRARIES[i])
                                                     ]
                                                    );
        Application.ProcessMessages();

        if progressDlg.Cancel then
          begin
          canceled := TRUE;
          break;
          end;

        if PKCS11VerifyLibrary(PKCS11_KNOWN_LIBRARIES[i].DLLFilename) then
          begin
          lastFound := PKCS11_KNOWN_LIBRARIES[i];
          dlg.Add(lastFound);
          inc(cntFound);
          end;

        progressDlg.IncPosition();
        Application.ProcessMessages();
        end;

    finally
      progressDlg.Free();
      Screen.Cursor := prevCursor;
    end;


    if canceled then
      begin
      Result := PKCS11_USER_CANCELED;
      end
    else
      begin
      if (cntFound = 1) then
        begin
        // Only one found; return it's filename
        Result := lastFound.DLLFilename;
        end
      else if (cntFound > 1) then
        begin
        // Multiple found; prompt user to select which one to usef
        if (dlg.ShowModal() = mrOK) then
          begin
          Result := dlg.SelectedLibrary.DLLFilename;
          end
        else
          begin
          Result := PKCS11_USER_CANCELED;
          end;
        end;
      end;

  finally
    dlg.Free();
  end;


end;


function PKCS11VerifyLibrary(LibraryDLL: string): boolean;
var
  lib: TPKCS11Library;
begin
  result := TRUE;

  if result then
    begin
    LibraryDLL := trim(LibraryDLL);
    result:= (LibraryDLL <> '');
    end;

  if result then
    begin
    result:= FALSE;
    try
      lib:= TPKCS11Library.Create(LibraryDLL);
      try
        lib.Initialize();
        lib.Finalize();
        result := TRUE;
      finally
        lib.Free();
      end;

    except
      on E:Exception do
        begin
        // Nothing - just swallow it
        end;
    end;

    end;

end;


constructor TPKCS11Library.Create(filename: string);
begin
  inherited Create();

  FCachedSlotsArray := nil;
  FSlotEventThread := nil;
  FOnSlotEvent := nil;

  FInitializeCalled := FALSE;
  
  LoadDLL(filename);
end;

destructor TPKCS11Library.Destroy();
var
  i: integer;
begin
  EventThreadStop();

  // Free off any cached slots
  if (FCachedSlotsArray <> nil) then
    begin
    for i:=low(FCachedSlotsArray) to high(FCachedSlotsArray) do
      begin
      FCachedSlotsArray[i].Free();
      end;
    end;

  FreeDLL();

  inherited;
end;

procedure TPKCS11Library.LoadDLL(DLLfilename: string);
begin
  //  need to use LoadLibraryEx so it uses path if user specified one
  FDLLHandle := LoadLibraryEx(PChar(DLLfilename),THandle(nil),LOAD_WITH_ALTERED_SEARCH_PATH);

  if FDLLHandle=0 then                         begin
    raise EPKCS11DLLNotFound.Create(SDUParamSubstitute(E_EPKCS11_DLLNOTFOUND, [DLLfilename]));
    end;

  FFilename:= DLLfilename;

  GetProcs();

  // Sanity check; we've got at least *some* functionality, right?
  if (
      (@LibraryFunctionList.CK_C_Initialize = nil) and
      (@LibraryFunctionList.CK_C_GetInfo = nil) and
      (@LibraryFunctionList.CK_C_GetSlotList = nil) and
      (@LibraryFunctionList.CK_C_GetSlotInfo = nil) and
      (@LibraryFunctionList.CK_C_OpenSession = nil)
     ) then     begin
    raise EPKCS11DLLNoAPI.Create(E_EPKCS11_DLLNOAPI);
 end;

end;


procedure TPKCS11Library.FreeDLL();
begin
  if (FDLLHandle<>0) then
    begin
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
    end;

end;

procedure TPKCS11Library.GetProcs();
begin
  ImportFunctionList();

  // If we imported the function list correctly, attempt to list again via
  // C_GetFunctionList - this should get the version ID populated as well
  if (@LibraryFunctionList.CK_C_GetFunctionList <> nil) then
  begin
    LibraryGetFunctionList();
  end;
end;

procedure TPKCS11Library.ImportFunctionList();
begin
  FFunctionList.version.major := 0;
  FFunctionList.version.minor := 0;

  @FFunctionList.CK_C_Initialize          := GetProcAddress(FDLLHandle, FN_NAME_C_Initialize);
  @FFunctionList.CK_C_Finalize            := GetProcAddress(FDLLHandle, FN_NAME_C_Finalize);
  @FFunctionList.CK_C_GetInfo             := GetProcAddress(FDLLHandle, FN_NAME_C_GetInfo);
  @FFunctionList.CK_C_GetFunctionList     := GetProcAddress(FDLLHandle, FN_NAME_C_GetFunctionList);
  @FFunctionList.CK_C_GetSlotList         := GetProcAddress(FDLLHandle, FN_NAME_C_GetSlotList);
  @FFunctionList.CK_C_GetSlotInfo         := GetProcAddress(FDLLHandle, FN_NAME_C_GetSlotInfo);
  @FFunctionList.CK_C_GetTokenInfo        := GetProcAddress(FDLLHandle, FN_NAME_C_GetTokenInfo);
  @FFunctionList.CK_C_GetMechanismList    := GetProcAddress(FDLLHandle, FN_NAME_C_GetMechanismList);
  @FFunctionList.CK_C_GetMechanismInfo    := GetProcAddress(FDLLHandle, FN_NAME_C_GetMechanismInfo);
  @FFunctionList.CK_C_InitToken           := GetProcAddress(FDLLHandle, FN_NAME_C_InitToken);
  @FFunctionList.CK_C_InitPIN             := GetProcAddress(FDLLHandle, FN_NAME_C_InitPIN);
  @FFunctionList.CK_C_SetPIN              := GetProcAddress(FDLLHandle, FN_NAME_C_SetPIN);
  @FFunctionList.CK_C_OpenSession         := GetProcAddress(FDLLHandle, FN_NAME_C_OpenSession);
  @FFunctionList.CK_C_CloseSession        := GetProcAddress(FDLLHandle, FN_NAME_C_CloseSession);
  @FFunctionList.CK_C_CloseAllSessions    := GetProcAddress(FDLLHandle, FN_NAME_C_CloseAllSessions);
  @FFunctionList.CK_C_GetSessionInfo      := GetProcAddress(FDLLHandle, FN_NAME_C_GetSessionInfo);
  @FFunctionList.CK_C_GetOperationState   := GetProcAddress(FDLLHandle, FN_NAME_C_GetOperationState);
  @FFunctionList.CK_C_SetOperationState   := GetProcAddress(FDLLHandle, FN_NAME_C_SetOperationState);
  @FFunctionList.CK_C_Login               := GetProcAddress(FDLLHandle, FN_NAME_C_Login);
  @FFunctionList.CK_C_Logout              := GetProcAddress(FDLLHandle, FN_NAME_C_Logout);
  @FFunctionList.CK_C_CreateObject        := GetProcAddress(FDLLHandle, FN_NAME_C_CreateObject);
  @FFunctionList.CK_C_CopyObject          := GetProcAddress(FDLLHandle, FN_NAME_C_CopyObject);
  @FFunctionList.CK_C_DestroyObject       := GetProcAddress(FDLLHandle, FN_NAME_C_DestroyObject);
  @FFunctionList.CK_C_GetObjectSize       := GetProcAddress(FDLLHandle, FN_NAME_C_GetObjectSize);
  @FFunctionList.CK_C_GetAttributeValue   := GetProcAddress(FDLLHandle, FN_NAME_C_GetAttributeValue);
  @FFunctionList.CK_C_SetAttributeValue   := GetProcAddress(FDLLHandle, FN_NAME_C_SetAttributeValue);
  @FFunctionList.CK_C_FindObjectsInit     := GetProcAddress(FDLLHandle, FN_NAME_C_FindObjectsInit);
  @FFunctionList.CK_C_FindObjects         := GetProcAddress(FDLLHandle, FN_NAME_C_FindObjects);
  @FFunctionList.CK_C_FindObjectsFinal    := GetProcAddress(FDLLHandle, FN_NAME_C_FindObjectsFinal);
  @FFunctionList.CK_C_EncryptInit         := GetProcAddress(FDLLHandle, FN_NAME_C_EncryptInit);
  @FFunctionList.CK_C_Encrypt             := GetProcAddress(FDLLHandle, FN_NAME_C_Encrypt);
  @FFunctionList.CK_C_EncryptUpdate       := GetProcAddress(FDLLHandle, FN_NAME_C_EncryptUpdate);
  @FFunctionList.CK_C_EncryptFinal        := GetProcAddress(FDLLHandle, FN_NAME_C_EncryptFinal);
  @FFunctionList.CK_C_DecryptInit         := GetProcAddress(FDLLHandle, FN_NAME_C_DecryptInit);
  @FFunctionList.CK_C_Decrypt             := GetProcAddress(FDLLHandle, FN_NAME_C_Decrypt);
  @FFunctionList.CK_C_DecryptUpdate       := GetProcAddress(FDLLHandle, FN_NAME_C_DecryptUpdate);
  @FFunctionList.CK_C_DecryptFinal        := GetProcAddress(FDLLHandle, FN_NAME_C_DecryptFinal);
  @FFunctionList.CK_C_DigestInit          := GetProcAddress(FDLLHandle, FN_NAME_C_DigestInit);
  @FFunctionList.CK_C_Digest              := GetProcAddress(FDLLHandle, FN_NAME_C_Digest);
  @FFunctionList.CK_C_DigestUpdate        := GetProcAddress(FDLLHandle, FN_NAME_C_DigestUpdate);
  @FFunctionList.CK_C_DigestKey           := GetProcAddress(FDLLHandle, FN_NAME_C_DigestKey);
  @FFunctionList.CK_C_DigestFinal         := GetProcAddress(FDLLHandle, FN_NAME_C_DigestFinal);
  @FFunctionList.CK_C_SignInit            := GetProcAddress(FDLLHandle, FN_NAME_C_SignInit);
  @FFunctionList.CK_C_Sign                := GetProcAddress(FDLLHandle, FN_NAME_C_Sign);
  @FFunctionList.CK_C_SignUpdate          := GetProcAddress(FDLLHandle, FN_NAME_C_SignUpdate);
  @FFunctionList.CK_C_SignFinal           := GetProcAddress(FDLLHandle, FN_NAME_C_SignFinal);
  @FFunctionList.CK_C_SignRecoverInit     := GetProcAddress(FDLLHandle, FN_NAME_C_SignRecoverInit);
  @FFunctionList.CK_C_SignRecover         := GetProcAddress(FDLLHandle, FN_NAME_C_SignRecover);
  @FFunctionList.CK_C_VerifyInit          := GetProcAddress(FDLLHandle, FN_NAME_C_VerifyInit);
  @FFunctionList.CK_C_Verify              := GetProcAddress(FDLLHandle, FN_NAME_C_Verify);
  @FFunctionList.CK_C_VerifyUpdate        := GetProcAddress(FDLLHandle, FN_NAME_C_VerifyUpdate);
  @FFunctionList.CK_C_VerifyFinal         := GetProcAddress(FDLLHandle, FN_NAME_C_VerifyFinal);
  @FFunctionList.CK_C_VerifyRecoverInit   := GetProcAddress(FDLLHandle, FN_NAME_C_VerifyRecoverInit);
  @FFunctionList.CK_C_VerifyRecover       := GetProcAddress(FDLLHandle, FN_NAME_C_VerifyRecover);
  @FFunctionList.CK_C_DigestEncryptUpdate := GetProcAddress(FDLLHandle, FN_NAME_C_DigestEncryptUpdate);
  @FFunctionList.CK_C_DecryptDigestUpdate := GetProcAddress(FDLLHandle, FN_NAME_C_DecryptDigestUpdate);
  @FFunctionList.CK_C_SignEncryptUpdate   := GetProcAddress(FDLLHandle, FN_NAME_C_SignEncryptUpdate);
  @FFunctionList.CK_C_DecryptVerifyUpdate := GetProcAddress(FDLLHandle, FN_NAME_C_DecryptVerifyUpdate);
  @FFunctionList.CK_C_GenerateKey         := GetProcAddress(FDLLHandle, FN_NAME_C_GenerateKey);
  @FFunctionList.CK_C_GenerateKeyPair     := GetProcAddress(FDLLHandle, FN_NAME_C_GenerateKeyPair);
  @FFunctionList.CK_C_WrapKey             := GetProcAddress(FDLLHandle, FN_NAME_C_WrapKey);
  @FFunctionList.CK_C_UnwrapKey           := GetProcAddress(FDLLHandle, FN_NAME_C_UnwrapKey);
  @FFunctionList.CK_C_DeriveKey           := GetProcAddress(FDLLHandle, FN_NAME_C_DeriveKey);
  @FFunctionList.CK_C_SeedRandom          := GetProcAddress(FDLLHandle, FN_NAME_C_SeedRandom);
  @FFunctionList.CK_C_GenerateRandom      := GetProcAddress(FDLLHandle, FN_NAME_C_GenerateRandom);
  @FFunctionList.CK_C_GetFunctionStatus   := GetProcAddress(FDLLHandle, FN_NAME_C_GetFunctionStatus);
  @FFunctionList.CK_C_CancelFunction      := GetProcAddress(FDLLHandle, FN_NAME_C_CancelFunction);
  @FFunctionList.CK_C_WaitForSlotEvent    := GetProcAddress(FDLLHandle, FN_NAME_C_WaitForSlotEvent);

end;


function TPKCS11Library.LibraryGetFunctionList(): boolean;
var
  pFuncList: ^CK_FUNCTION_LIST;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetFunctionList, FN_NAME_C_GetFunctionList);

  LastRV := LibraryFunctionList.CK_C_GetFunctionList(@pFuncList);
  if RVSuccess(LastRV) then
  begin
    FFunctionList := pFuncList^;
  end;

  Result := RVSuccess(LastRV);
end;

function TPKCS11Library.GetCountSlots({onlySlotsWithTokenPresent: boolean}): integer;
var
  slotsArray: TPKCS11SlotArray;
begin
  Result := 0;
  if GetSlotList({onlySlotsWithTokenPresent, }slotsArray) then
    begin
    Result := length(slotsArray);
    end;


end;

// This method populates FCachedSlotsArray
function TPKCS11Library.GetSlotList({onlySlotsWithTokenPresent: boolean; }var slotsArray: TPKCS11SlotArray): boolean;
var
  apiTokenPresent: CK_BBOOL;
  slotID: CK_SLOT_ID;
  ulCount: CK_ULONG;
  ptrBuffer: CK_SLOT_ID_PTR;
  ptrBufferOffset: CK_SLOT_ID_PTR;
  bufferSize: integer;
  i: integer;
  slotObj: TPKCS11Slot;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetSlotList, FN_NAME_C_GetSlotList);

  apiTokenPresent := CK_FALSE;
{
  if onlySlotsWithTokenPresent then
  begin
    apiTokenPresent := CK_TRUE;
  end;
}

  if (FCachedSlotsArray <> nil) then
    begin
    slotsArray := FCachedSlotsArray;
    Result := TRUE;
    exit;
    end;

  // Get count..
  ulCount := 0;
  LastRV := LibraryFunctionList.CK_C_GetSlotList(apiTokenPresent, nil, @ulCount);
  if RVSuccess(LastRV) then
    begin
    // Get...
    bufferSize := (sizeof(slotID) * ulCount);
    ptrBuffer := allocmem(bufferSize);
    try
      LastRV := LibraryFunctionList.CK_C_GetSlotList(apiTokenPresent, ptrBuffer, @ulCount);
      if RVSuccess(LastRV) then
        begin
        SetLength(slotsArray, ulCount);
        ptrBufferOffset := ptrBuffer;
        for i:=1 to ulCount do
          begin
          slotObj:= TPKCS11Slot.Create();

          slotID := ptrBufferOffset^;
          slotObj.SlotID := slotID;
          slotObj.LibraryFunctionList := LibraryFunctionList;

          slotsArray[i-1] := slotObj;

          ptrBufferOffset := Pointer(PChar(ptrBufferOffset) + sizeof(ptrBufferOffset^));
          end;

        FCachedSlotsArray := slotsArray;
        end;

    finally
      FreeMem(ptrBuffer);
    end;
    end;

  Result := RVSuccess(LastRV);
end;

function TPKCS11Library.GetSlotByArrayIdx(arrIdx: integer): TPKCS11Slot;
var
  slotsArray: TPKCS11SlotArray;
begin
  Result := nil;
  if GetSlotList({FALSE, }slotsArray) then
    begin
    // Sanity check
    if ((low(slotsArray) + arrIdx) > high(slotsArray)) then
      begin
      raise EPKCS11ListIndexOutOFBounds.Create(E_EPKCS11_LISTINDEXOUTOFBOUNDS);
      end;

    Result := slotsArray[(low(slotsArray) + arrIdx)];
    end;


end;

function TPKCS11Library.GetSlotBySlotID(slotID: integer): TPKCS11Slot;
var
  slotsArray: TPKCS11SlotArray;
  i: integer;
begin
  Result := nil;

  if GetSlotList({FALSE, }slotsArray) then
    begin
    for i:=low(slotsArray) to high(slotsArray) do
      begin
      if (slotsArray[i].SlotID = slotID) then
        begin
        Result := slotsArray[i];
        break
        end;
      end;
    end;


end;

function TPKCS11Library.Initialize(): boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Initialize, FN_NAME_C_Initialize);
  LastRV := LibraryFunctionList.CK_C_Initialize(nil);
  Result := RVSuccess(LastRV);
  if Result then
    begin
    FInitializeCalled := TRUE;
    EventThreadStart();
    end;

end;

function TPKCS11Library.Finalize(): boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_Finalize, FN_NAME_C_Finalize);
  LastRV := LibraryFunctionList.CK_C_Finalize(nil);
  Result := RVSuccess(LastRV);
  if Result then
    begin
    FInitializeCalled := FALSE;
    EventThreadStop();
    end;

end;

function TPKCS11Library.GetInfo(var info: CK_INFO): boolean;
begin
  CheckFnAvailable(@LibraryFunctionList.CK_C_GetInfo, FN_NAME_C_GetInfo);

  ZeroMemory(@info, sizeof(info));
  LastRV := LibraryFunctionList.CK_C_GetInfo(@info);

  Result := RVSuccess(LastRV);
end;

function TPKCS11Library.GetCryptokiVersion(): CK_VERSION;
var
  info: CK_INFO;
begin
  Result := NULL_VERSION;

  if (GetInfo(info)) then
    begin
    Result := info.cryptokiVersion;
    end;


end;

function TPKCS11Library.GetManufacturerID(): string;
var
  info: CK_INFO;
begin
  Result := '';

  if (GetInfo(info)) then
    begin
    Result := UTF8CHARArrayToString(TCK_UTF8CHARArray_32(info.manufacturerID));
    end;


end;

function TPKCS11Library.GetFlags(): CK_FLAGS;
var
  info: CK_INFO;
begin
  Result := 0;

  if (GetInfo(info)) then
    begin
    Result := info.Flags;
    end;


end;

function TPKCS11Library.GetLibraryDescription(): string;
var
  info: CK_INFO;
begin
  Result := '';

  if (GetInfo(info)) then
    begin
    Result := UTF8CHARArrayToString(TCK_UTF8CHARArray_32(info.libraryDescription));
    end;


end;

function TPKCS11Library.GetLibraryVersion(): CK_VERSION;
var
  info: CK_INFO;
begin
  Result := NULL_VERSION;

  if (GetInfo(info)) then
    begin
    Result := info.libraryVersion;
    end;


end;

procedure TPKCS11Library.EventThreadStart();
begin
  if (FSlotEventThread = nil) then
    begin
    // Note: Don't use CheckFnAvailable(...) here
    if (@LibraryFunctionList.CK_C_WaitForSlotEvent <> nil) then
      begin
      FSlotEventThread := TPKCS11SlotEventThread.Create(TRUE);
      FSlotEventThread.LibraryFunctionList := LibraryFunctionList;
      FSlotEventThread.LibraryCallback := EventThreadCallback;
      FSlotEventThread.Resume();
      end;
    end;

end;

procedure TPKCS11Library.EventThreadStop();
begin
  if (FSlotEventThread <> nil) then
    begin
    FSlotEventThread.Terminate();
    FSlotEventThread.Free();
    FSlotEventThread := nil;
    end;

end;

procedure TPKCS11Library.EventThreadCallback(Sender: TObject; SlotID: integer);
begin
  if Assigned(FOnSlotEvent) then
    begin
    FOnSlotEvent(self, SlotID);
    end;
end;

END.



