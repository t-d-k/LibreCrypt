unit OTFEUnified_U;
// Description: Delphi Unified OTFE Component
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OTFE_U,
  OTFEConsts_U,
  OTFEBestCrypt_U,
  OTFEE4M_U,
  OTFETrueCrypt_U,
  OTFEPGPDisk_U,
  OTFEScramDisk_U,
  OTFECrossCrypt_U,
  OTFEFreeOTFE_U;

type
  TOTFESystem = (
                 otfesFreeOTFE,
                 otfesBestCrypt,
                 otfesCrossCrypt,
                 otfesE4M,
                 otfesPGPDisk,
                 otfesScramDisk,
                 otfesTrueCrypt
                );

const
  // !!!!!!!!!!!!!!!
  // !!!!!!!!!!!!!!!
  // !!! WARNING !!!
  // !!!!!!!!!!!!!!!
  // !!!!!!!!!!!!!!!
  // THIS MUST BE IN THE SAME ORDER AS TOTFESystem
  // Display names for each of the different types of OTF crypto system
  // Note: Each display name must be unique
  OTFESDispNames: array [TOTFESystem] of string = (
                                                   'FreeOTFE',
                                                   'BestCrypt',
                                                   'CrossCrypt',
                                                   'E4M',
                                                   'PGPDisk',
                                                   'ScramDisk',
                                                   'TrueCrypt'
                                                  );

const
  // !!!!!!!!!!!!!!!
  // !!!!!!!!!!!!!!!
  // !!! WARNING !!!
  // !!!!!!!!!!!!!!!
  // !!!!!!!!!!!!!!!
  // THIS MUST BE IN THE SAME ORDER AS TOTFESystem
  // "Internal" names for each of the different types of OTF crypto system
  // Note: Each internal name must be unique, and contain no spaces or
  // punctuation.
  // This is intended that the user can use this where a display name would be
  // useless (e.g. if the corrosponding display name contains spaces, etc)
  OTFESInternalNames: array [TOTFESystem] of string = (
                                                       'FREEOTFE',
                                                       'BESTCRYPT',
                                                       'CROSSCRYPT',
                                                       'E4M',
                                                       'PGPDISK',
                                                       'SCRAMDISK',
                                                       'TRUECRYPT'
                                                      );

type
  TOTFEUnified = class(TOTFE)
  private
    { private declarations here}
  protected
    // Set the component active/inactive
    procedure SetActive(status: Boolean); override;
  public
    OTFComponents: array [TOTFESystem] of TOTFE;
    OTFEnabledComponents: array [TOTFESystem] of boolean;

    constructor Create(AOwner : TComponent); override;
    destructor  Destroy(); override;
    function  Title(): string; overload; override;
    function  Mount(volumeFilename: Ansistring; readonly: boolean = FALSE): Ansichar; overload; override;
    function  Mount(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean; overload; override;
    function  MountDevices(): Ansistring; override;
    function  CanMountDevice(): boolean; override;
    function  Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean; overload; override;
    function  Dismount(driveLetter: Ansichar; emergency: boolean = FALSE): boolean; overload; override;
    function  IsDriverInstalled(): boolean; overload; override;
    function  IsDriverInstalled(otfeSystem: TOTFESystem): boolean; overload;
    function  Version(): cardinal; overload; override;
    function  VersionStr(): string; overload; override;
    function  IsEncryptedVolFile(volumeFilename: string): boolean; override;
    function  DrivesMounted(): Ansistring; overload; override;
    function  GetVolFileForDrive(driveLetter: Ansichar): string; override;
    function  GetDriveForVolFile(volumeFilename: string): Ansichar; override;
    function  GetMainExe(): string; override;

    function  TypeOfEncryptedVolFile(volumeFilename: string; var typeOfFile: TOTFESystem): boolean;
    function  TypeOfEncryptedDrive(driveLetter: Ansichar; var typeOfFile: TOTFESystem): boolean;

    // OTF Crypto "internal" name to system type
    function  OTFSystemFromDispName(dispName: string; var OTFSystem: TOTFESystem): boolean;
    // OTF Crypto display name to system type
    function  OTFSystemFromInternalName(internalName: string; var OTFSystem: TOTFESystem): boolean;

    // For a *mounted* volume, sets OTFSystem to the OTF crypto system used for
    // that volume
    function  OTFSystemForDrive(driveLetter: Ansichar; var OTFSystem: TOTFESystem): boolean;
    // For a *mounted* volume, sets OTFSystem to the OTF crypto system used for
    // that volume
    function  OTFSystemForVolFile(volumeFilename: string; var OTFSystem: TOTFESystem): boolean;

  end;

procedure Register;

implementation

uses
  OTFEUnified_frmSelectOTFESystem;


procedure Register;
begin
  RegisterComponents('OTFE', [TOTFEUnified]);
end;


constructor TOTFEUnified.Create(AOwner : TComponent);
var
  OTFSystemLoop: TOTFESystem;
begin
  inherited;

  OTFComponents[otfesBestCrypt ] := TOTFEBestCrypt.Create(nil);
  OTFComponents[otfesE4M       ] := TOTFEE4M.Create(nil);
  OTFComponents[otfesTrueCrypt ] := TOTFETrueCrypt.Create(nil);
  OTFComponents[otfesPGPDisk   ] := TOTFEPGPDisk.Create(nil);
  OTFComponents[otfesScramDisk ] := TOTFEScramDisk.Create(nil);
  OTFComponents[otfesCrossCrypt] := TOTFECrossCrypt.Create(nil);
  OTFComponents[otfesFreeOTFE  ] := TOTFEFreeOTFE.Create(nil);

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    OTFEnabledComponents[OTFSystemLoop] := TRUE;
    end;

end;

destructor TOTFEUnified.Destroy();
var
  OTFSystemLoop: TOTFESystem;
begin
  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    OTFComponents[OTFSystemLoop].Free();
    end;

  inherited;

end;

procedure TOTFEUnified.SetActive(status: Boolean);
var
  OTFSystemLoop: TOTFESystem;
begin
  inherited;

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFEnabledComponents[OTFSystemLoop] then
      begin
      try
        OTFComponents[OTFSystemLoop].Active := status;
        OTFEnabledComponents[OTFSystemLoop] := OTFComponents[OTFSystemLoop].Active;
      except
        // Damn. Oh well, better disable this component
        if OTFComponents[OTFSystemLoop].Active<>status then
          begin
          OTFEnabledComponents[OTFSystemLoop] := FALSE;
          FLastErrCode := OTFComponents[OTFSystemLoop].LastErrorCode;
          end;
      end;
      end;
    end;

  // If at least one of the components was set active, then we accept that this
  // unified component may also be set active
  FActive := FALSE;
  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      FActive := TRUE;
      break;
      end;
    end;

end;

function TOTFEUnified.Mount(volumeFilename: Ansistring; readonly: boolean = FALSE): Ansichar;
var
  stlTemp: TStringList;
  mountedAs: Ansistring;
begin
  stlTemp := TStringList.Create();
  try
    stlTemp.Add(volumeFilename);
    if Mount(stlTemp, mountedAs, readonly) then
      begin
      Result := mountedAs[1];
      end
    else
      begin
      Result := #0;
      end;
  finally
    stlTemp.Free();
  end;

end;


function TOTFEUnified.Mount(volumeFilenames: TStringList; var mountedAs: AnsiString; readonly: boolean = FALSE): boolean;
var
  useOTFSystem: TOTFESystem;
  OTFSystemLoop: TOTFESystem;
  matchingOTFE: array [TOTFESystem] of boolean;
  cntMatched: integer;
  retVal: boolean;
  lastMatched: TOTFESystem;
  dlgSelect: TfrmSelectOTFESystem;
begin
  retVal := FALSE;

  cntMatched := 0;
  lastMatched := otfesFreeOTFE; // Doesn't matter; this value should never be used
  if (volumeFilenames.Count>0) then
    begin
    for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
      begin
      matchingOTFE[OTFSystemLoop] := FALSE;
      if OTFComponents[OTFSystemLoop].Active then
        begin
        matchingOTFE[OTFSystemLoop] := OTFComponents[OTFSystemLoop].IsEncryptedVolFile(volumeFilenames[0]);
        if (matchingOTFE[OTFSystemLoop]) then
          begin
          inc(cntMatched);
          lastMatched := OTFSystemLoop;
          end;

        end;
      end;


    if (cntMatched=1) then
      begin
      // Detected single OTFE system capable of mounting...
      retVal := OTFComponents[lastMatched].Mount(volumeFilenames, mountedAs, readonly);
      FLastErrCode := OTFComponents[lastMatched].LastErrorCode;
      end
    else if (cntMatched>1) then
      begin
      // Detected multiple OTFE systems capable of mounting; prompt user...

      dlgSelect:= TfrmSelectOTFESystem.Create(nil);
      try
        for useOTFSystem:=low(matchingOTFE) to high(matchingOTFE) do
          begin
          if matchingOTFE[useOTFSystem] then
            begin
            dlgSelect.Add(useOTFSystem);
            end;
          end;


        if (dlgSelect.ShowModal = mrOK) then
          begin
          useOTFSystem := dlgSelect.GetSelected();
          retVal := OTFComponents[useOTFSystem].Mount(volumeFilenames, mountedAs, readonly);
          FLastErrCode := OTFComponents[useOTFSystem].LastErrorCode;
          end
        else
          begin
          FLastErrCode := OTFE_ERR_USER_CANCEL;
          end;

      finally
        dlgSelect.Free();
      end;
      
      end;

    end;

  Result := retVal;
end;

function TOTFEUnified.Dismount(volumeFilename: string; emergency: boolean = FALSE): boolean;
var
  OTFSystem: TOTFESystem;
begin
  Result := FALSE;

  if OTFSystemForVolFile(volumeFilename, OTFSystem) then
    begin
    Result := OTFComponents[OTFSystem].Dismount(volumeFilename, emergency);
    FLastErrCode := OTFComponents[OTFSystem].LastErrorCode;
    end;

end;


function TOTFEUnified.Dismount(driveLetter: Ansichar; emergency: boolean = FALSE): boolean;
var
  OTFSystem: TOTFESystem;
begin
  Result := FALSE;

  if OTFSystemForDrive(driveLetter, OTFSystem) then
    begin
    Result := OTFComponents[OTFSystem].Dismount(driveLetter, emergency);
    FLastErrCode := OTFComponents[OTFSystem].LastErrorCode;
    end;

end;


function TOTFEUnified.IsDriverInstalled(): boolean;
var
  retVal: boolean;
  OTFSystemLoop: TOTFESystem;
begin
  retVal := FALSE;

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      retVal := retVal OR OTFComponents[OTFSystemLoop].IsDriverInstalled();
      if retVal then
        begin
        break;
        end;
      end;
    end;

  Result := retVal;

end;



// Determine if a specific OTFE driver is installed
function TOTFEUnified.IsDriverInstalled(otfeSystem: TOTFESystem): boolean;
begin
  Result := OTFComponents[otfeSystem].IsDriverInstalled();

end;


function TOTFEUnified.Title(): string;
begin
  Result := '';
end;


function TOTFEUnified.Version(): cardinal;
begin
  Result := $FFFFFFFF;
end;

function TOTFEUnified.VersionStr(): string;
begin
  Result := '';
end;

function TOTFEUnified.IsEncryptedVolFile(volumeFilename: string): boolean;
var
  junkOTFSystem: TOTFESystem;
begin
  Result := TypeOfEncryptedVolFile(volumeFilename, junkOTFSystem);

end;

function TOTFEUnified.TypeOfEncryptedVolFile(volumeFilename: string; var typeOfFile: TOTFESystem): boolean;
var
  retVal: boolean;
  OTFSystemLoop: TOTFESystem;
begin
  retVal := FALSE;

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      retVal := retVal OR OTFComponents[OTFSystemLoop].IsEncryptedVolFile(volumeFilename);
      if retVal then
        begin
        typeOfFile := OTFSystemLoop;
        break;
        end;
      end;
    end;

  Result := retVal;

end;

function TOTFEUnified.TypeOfEncryptedDrive(driveLetter: Ansichar; var typeOfFile: TOTFESystem): boolean;
var
  OTFSystemLoop: TOTFESystem;
begin
  Result := FALSE;
  driveLetter := AnsiChar((uppercase(driveLetter))[1]);

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      if (Pos(driveLetter, OTFComponents[OTFSystemLoop].DrivesMounted())>0) then
        begin
        typeOfFile := OTFSystemLoop;
        Result := TRUE;
        break;
        end;
      end;
    end;

end;


function TOTFEUnified.DrivesMounted(): Ansistring;
var
  retVal: Ansistring;
  OTFSystemLoop: TOTFESystem;
begin
  retVal := '';

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      retVal := retVal + OTFComponents[OTFSystemLoop].DrivesMounted();
      end;
    end;

  Result := SortString(retVal);

end;

function TOTFEUnified.GetVolFileForDrive(driveLetter: Ansichar): string;
var
  retVal: string;
  OTFSystemLoop: TOTFESystem;
begin
  retVal := '';

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      retVal := OTFComponents[OTFSystemLoop].GetVolFileForDrive(driveLetter);
      if retVal<>'' then
        begin
        break;
        end;
      end;
    end;

  Result := retVal;

end;


function TOTFEUnified.GetDriveForVolFile(volumeFilename: string): Ansichar;
var
  retVal: Ansichar;
  OTFSystemLoop: TOTFESystem;
begin
  retVal := #0;

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      retVal := OTFComponents[OTFSystemLoop].GetDriveForVolFile(volumeFilename);
      if retVal<>#0 then
        begin
        break;
        end;
      end;
    end;

  Result := retVal;

end;

function TOTFEUnified.OTFSystemFromDispName(dispName: string; var OTFSystem: TOTFESystem): boolean;
var
  OTFSystemLoop: TOTFESystem;
begin
  Result := FALSE;

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if dispName=OTFESDispNames[OTFsystem] then
      begin
      OTFSystem := OTFSystemLoop;
      Result := TRUE;
      break;
      end;
    end;

end;

function TOTFEUnified.OTFSystemFromInternalName(internalName: string; var OTFSystem: TOTFESystem): boolean;
var
  OTFSystemLoop: TOTFESystem;
begin
  Result := FALSE;

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if internalName=OTFESInternalNames[OTFsystem] then
      begin
      OTFSystem := OTFSystemLoop;
      Result := TRUE;
      break;
      end;
    end;

end;

function TOTFEUnified.OTFSystemForDrive(driveLetter: Ansichar; var OTFSystem: TOTFESystem): boolean;
var
  OTFSystemLoop: TOTFESystem;
begin
  Result := FALSE;

  driveLetter := Ansichar((uppercase(driveLetter))[1]);
  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      if pos(driveLetter, OTFComponents[OTFSystemLoop].DrivesMounted())>0 then
        begin
        OTFSystem := OTFSystemLoop;
        Result := TRUE;
        break;
        end;
      end;
    end;

end;


function TOTFEUnified.OTFSystemForVolFile(volumeFilename: string; var OTFSystem: TOTFESystem): boolean;
var
  OTFSystemLoop: TOTFESystem;
begin
  Result := FALSE;

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      if GetDriveForVolFile(volumeFilename)<>#0 then
        begin
        OTFSystem := OTFSystemLoop;
        Result := TRUE;
        break;
        end;
      end;
    end;

end;

function TOTFEUnified.GetMainExe(): string;
begin
  FLastErrCode:= OTFE_ERR_UNABLE_TO_LOCATE_FILE;
  Result := '';

end;

// -----------------------------------------------------------------------------
// Prompt the user for a device (if appropriate) and password (and drive
// letter if necessary), then mount the device selected
// Returns the drive letter of the mounted devices on success, #0 on failure
function TOTFEUnified.MountDevices(): Ansistring;
var
  useOTFSystem: TOTFESystem;
  OTFSystemLoop: TOTFESystem;
  matchingOTFE: array [TOTFESystem] of boolean;
  cntMatched: integer;
  retVal: Ansistring;
  lastMatched: TOTFESystem;
  dlgSelect: TfrmSelectOTFESystem;
begin
  retVal := '';

  cntMatched := 0;
  lastMatched := otfesFreeOTFE; // Doesn't matter; this value should never be used
  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    matchingOTFE[OTFSystemLoop] := FALSE;
    if OTFComponents[OTFSystemLoop].Active then
      begin
      matchingOTFE[OTFSystemLoop] := OTFComponents[OTFSystemLoop].CanMountDevice();
      if (matchingOTFE[OTFSystemLoop]) then
        begin
        inc(cntMatched);
        lastMatched := OTFSystemLoop;
        end;

      end;

    end;  // for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do

  if (cntMatched=1) then
    begin
    // Detected single OTFE system capable of mounting...
    retVal := OTFComponents[lastMatched].MountDevices();
    FLastErrCode := OTFComponents[lastMatched].LastErrorCode;
    end
  else if (cntMatched>1) then
    begin
    // Detected multiple OTFE systems capable of mounting; prompt user for
    // which one to use...

    dlgSelect:= TfrmSelectOTFESystem.Create(nil);
    try
      for useOTFSystem:=low(matchingOTFE) to high(matchingOTFE) do
        begin
        if matchingOTFE[useOTFSystem] then
          begin
          dlgSelect.Add(useOTFSystem);
          end;
        end;


      if (dlgSelect.ShowModal = mrOK) then
        begin
        useOTFSystem := dlgSelect.GetSelected();
        retVal := OTFComponents[useOTFSystem].MountDevices();
        FLastErrCode := OTFComponents[useOTFSystem].LastErrorCode;
        end
      else
        begin
        FLastErrCode := OTFE_ERR_USER_CANCEL;
        end;

    finally
      dlgSelect.Free();
    end;
      
    end;

  Result := retVal;
end;

// -----------------------------------------------------------------------------
// Determine if any OTFE components can mount devices.
// Returns TRUE if it can, otherwise FALSE
function TOTFEUnified.CanMountDevice(): boolean;
var
  retVal: boolean;
  OTFSystemLoop: TOTFESystem;
begin
  retVal := FALSE;

  for OTFSystemLoop:=low(OTFComponents) to high(OTFComponents) do
    begin
    if OTFComponents[OTFSystemLoop].Active then
      begin
      retVal := OTFComponents[OTFSystemLoop].CanMountDevice();
      if retVal then
        begin
        break;
        end;
      end;
    end;

  Result := retVal;

end;

// -----------------------------------------------------------------------------

END.


