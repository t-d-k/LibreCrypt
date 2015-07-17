unit DriverControl;
 // Description:
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //


 // !!!!!!!!!!!!!!!!!
 // !!!  WARNING  !!!
 // !!!!!!!!!!!!!!!!!
 // AFTER USING THIS OBJECT, YOU ***MUST*** FLUSH THE CACHES IN ANY FREEOTFE
 // COMPONENTS (i.e. call CachesFlush(...) on them) FOR THEM TO SEE DRIVER
 // CHANGES
 // !!!!!!!!!!!!!!!!!
 // !!!  WARNING  !!!
 // !!!!!!!!!!!!!!!!!


interface

uses
  Windows, SysUtils, Classes, Dialogs,
  WinSVC,  // Required for service related definitions and functions
  SDUGeneral;

const
  DRIVER_BIT_SUCCESS           = 1;
  DRIVER_BIT_REBOOT_REQ        = 2;
  DRIVER_BIT_FILE_REMAINS      = 4;
  DRIVER_BIT_CANT_INSTALL_FILE = 8;
  DRIVER_BIT_ALREADY_INSTALLED = 16;

resourcestring
  TEXT_NEED_ADMIN = 'Administrator privileges are required in order to carry out this operation.';

type

  //see driverocntrol for implementation
  TSDUServiceControl = class (Tobject)
  private

  protected
  FSilent: Boolean;
    function FreeOTFEOpenService(service: String;
      var serviceHandle: SC_HANDLE): Boolean;
          procedure FreeOTFECloseService(serviceHandle: SC_HANDLE);
    function _MsgDlg(Content: String; DlgType: TMsgDlgType): Integer;
  public
  property Silent: Boolean Read FSilent Write FSilent;
      function GetServiceState(service: String; var state: DWORD): Boolean; virtual;
          function StartStopService(service: String; start: Boolean): Boolean; virtual;

  end;

  TDriverControl = class(TSDUServiceControl)
  private
//    FSilent: Boolean;
    //    SCManager: SC_HANDLE;

//    function _MsgDlg(Content: String; DlgType: TMsgDlgType): Integer;
//
//    function FreeOTFEOpenService(
//      service: String;
//      var serviceHandle: SC_HANDLE
//      ): Boolean;
//    procedure FreeOTFECloseService(serviceHandle: SC_HANDLE);

    // Open the Service Control Manager
    function SCManagerOpen(): Boolean;
    // Close the Service Control Manager
    procedure SCManagerClose();

    function InstallDriverFile(driverFilename: String; var destFilename: String): Boolean;
    function UninstallDriverFile(driverFilename: String): Boolean;

    function GetFreeOTFEDriversInState(serviceState: DWORD;
      var driverNames: TStringList): Boolean;
    function DriverConfig(service: String; var autoStart: Boolean;
      var fullDriverFilename: String): Boolean;

    // Set a registry setting for the specified driver...
    function SetDriverReg(service: String; Name: String; Value: String): Boolean; overload;
    function SetDriverReg(service: String; Name: String; Value: Boolean): Boolean; overload;
    // Get a registry setting for the specified driver...
{
Commented out; not used atm
    function  GetDriverRegString(service: string; name: string; var value: string): boolean;
}
    function GetDriverRegBoolean(service: String; Name: String; var Value: Boolean): Boolean;

  public
    constructor Create();
    destructor Destroy(); override;


//    property Silent: Boolean Read FSilent Write FSilent;

    function InstallSetAutoStartAndStartDriver(pathAndFilename: String): Boolean; overload;
    function InstallMultipleDrivers(
      driverFilenames: TStringList;
      portableMode: Boolean;
      showProgress: Boolean;
      startAfterInstall: Boolean
      ): Boolean;
    function InstallDriver(filename: String; portableMode: Boolean;
      var driverName: String): DWORD;
    // Check to see if the named driver is installed
    function IsDriverInstalled(service: String; var installed: Boolean): Boolean;
    // Check to see if the named driver is installed in portable mode
    function IsDriverInstalledPortable(service: String; var portableMode: Boolean): Boolean;
    function UninstallDriver(service: String): DWORD;
    function UninstallAllDrivers(portableModeOnly: Boolean): Boolean;

//    function GetServiceState(service: String; var state: DWORD): Boolean;
    function GetFreeOTFEDrivers(var driverNames: TStringList): Boolean;
//    function StartStopService(service: String; start: Boolean): Boolean;
    function GetServiceAutoStart(service: String; var autoStart: Boolean): Boolean;
    function SetServiceAutoStart(service: String; autoStart: Boolean): Boolean;

    function GetServiceNameForFilename(filename: String): String;

    // Return total count of drivers installed
    function CountDrivers(): Integer; overload;
    // Return total count of drivers in portable/non portable mode
    function CountDrivers(portableMode: Boolean): Integer; overload;
  end;

implementation

uses
  Controls, Forms,
  lcDialogs,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  registry,
  SDUi18n,
  dlgProgress,
  SDUWindows64;

const
  // This CSIDL value isn't included in ShlObj in Delphi 5
  CSIDL_SYSTEM = $0025;

  REGKEY_BASE = '\SYSTEM\CurrentControlSet\Services';

  // Flag registry key to indicate driver is a FreeOTFE driver
  REG_FREEOTFE_DRIVER = 'FreeOTFE';
  // Flag registry key to indicate if driver was installed in portable mode
  REG_PORTABLE_MODE   = 'InstalledPortable';

var
{windows 7 doesnt like opeing and closing it too much - get 'invalid handle error', so reuse
  where pos -except when expliclty closed
}
  { TODO -otdk -crefactor : better make TDriverControl singleton }

  uSCManager: SC_HANDLE = 0;

// ----------------------------------------------------------------------------
constructor TDriverControl.Create();
begin
  FSilent := False;

  if uSCManager = 0 then
    if not (SCManagerOpen()) then begin
      raise EFreeOTFENeedAdminPrivs.Create(
        _('Unable to open service control manager.') + SDUCRLF +
        SDUCRLF + TEXT_NEED_ADMIN);
    end;

end;


// ----------------------------------------------------------------------------
destructor TDriverControl.Destroy();
begin
 {  dont call SCManagerClose();  - see comment for uSCManager.
    this isnt a resource leak bc CreateService doesnt actually create anything - only gets handle}
  inherited;
end;


 



 // driverNames - Set to a TStringList object which will have the driver names
 //               *added* to it
function TDriverControl.GetFreeOTFEDrivers(var driverNames: TStringList): Boolean;
begin
  Result := True;

  Result := Result and GetFreeOTFEDriversInState(SERVICE_ACTIVE, driverNames);
  Result := Result and GetFreeOTFEDriversInState(SERVICE_INACTIVE, driverNames);

end;


 // dwServiceState - Set to SERVICE_ACTIVE or SERVICE_INACTIVE
 // driverNames - Set to a TStringList object which will have the driver names
 //               *added* to it
function TDriverControl.GetFreeOTFEDriversInState(serviceState: DWORD;
  var driverNames: TStringList): Boolean;
const
  // Arbitarily high number of services, but should be higher than the
  // number which will ever be encountered
  LARGE_NUMBER_OF_SERVICES = 100000;
type
  TEnumServices = array[0..(LARGE_NUMBER_OF_SERVICES - 1)] of TEnumServiceStatus;
  PEnumServices = ^TEnumServices;
var
  bytesSupplied:       DWORD;
  bytesNeeded:         DWORD;
  cntServicesReturned: DWORD;
  resumeHandle:        DWORD;
  serviceList:         PEnumServices;
  i:                   Integer;
  currDriverName:      String;
  flagFreeOTFEDriver:  Boolean;
begin
  Result := True;

  if (uSCManager <> 0) then begin
    resumeHandle := 0;
    serviceList  := nil;

    EnumServicesStatus(
      uSCManager,           // handle to service control manager database
      SERVICE_DRIVER,       // type of services to enumerate
      serviceState,         // state of services to enumerate
      serviceList[0],       // pointer to service status buffer
      0,                    // size of service status buffer
      bytesNeeded,          // pointer to variable for bytes needed
      cntServicesReturned,  // pointer to variable for number returned
      resumeHandle          // pointer to variable for next entry
      );

    serviceList := AllocMem(bytesNeeded);
    try
      bytesSupplied := bytesNeeded;
      resumeHandle  := 0;

      if EnumServicesStatus(uSCManager,
                         // handle to service control manager database
        SERVICE_DRIVER,  // type of services to enumerate
        serviceState,    // state of services to enumerate
        serviceList[0],  // pointer to service status buffer
        bytesSupplied,   // size of service status buffer
        bytesNeeded,     // pointer to variable for bytes needed
        cntServicesReturned,  // pointer to variable for number returned
        resumeHandle     // pointer to variable for next entry
        ) then begin
        for i := 0 to (cntServicesReturned - 1) do begin
          currDriverName := serviceList[i].lpServiceName;

          // Check the registry if the driver has the FreeOTFE driver flag
          if GetDriverRegBoolean(currDriverName, REG_FREEOTFE_DRIVER, flagFreeOTFEDriver) then
          begin
            if (flagFreeOTFEDriver) then begin
              driverNames.Add(currDriverName);
            end;

          end;  // if GetDriverRegBoolean(currDriverName, REG_FREEOTFE_DRIVER, flagFreeOTFEDriver) then

        end;  // for i:=0 to (cntServicesReturned-1) do

      end;  // if EnumServicesStatus(

    finally
      FreeMem(serviceList);
    end;

  end;

end;


function TDriverControl.DriverConfig(service: String;
  var autoStart: Boolean; var fullDriverFilename: String): Boolean;
var
  serviceHandle: SC_HANDLE;
  serviceConfig: LPQUERY_SERVICE_CONFIGW;
  bytesNeeded:   DWORD;
  bytesSupplied: DWORD;
begin
  Result := False;

  if FreeOTFEOpenService(service, serviceHandle) then begin
    serviceConfig := nil;
    // Note: We need to go through this weridness and can't use
    //       sizeof(TQueryServiceConfig), as QueryServiceConfig(...) writes the
    //       strings pointed to in the TQueryServiceConfig it's passed *after*
    //       the TQueryServiceConfig!!!
    QueryServiceConfig(
      serviceHandle,  // handle of service
      serviceConfig,  // address of service config. structure
      0,              // size of service configuration buffer
      bytesNeeded     // address of variable for bytes needed
      );
    serviceConfig := AllocMem(bytesNeeded);
    try
      bytesSupplied := bytesNeeded;
      if QueryServiceConfig(serviceHandle,  // handle of service
        serviceConfig,  // address of service config. structure
        bytesSupplied,  // size of service configuration buffer
        bytesNeeded  // address of variable for bytes needed
        ) then begin
        autoStart := (serviceConfig.dwStartType = SERVICE_SYSTEM_START);

        fullDriverFilename := copy(serviceConfig.lpBinaryPathName, 0,
          StrLen(serviceConfig.lpBinaryPathName));
        // Remove any "\??\" prefix from filename
        if (Pos('\??\', fullDriverFilename) <> 0) then begin
          Delete(fullDriverFilename, 1, length('\??\'));
        end;

        Result := True;
      end;

    finally
      FreeMem(serviceConfig);
    end;

    FreeOTFECloseService(serviceHandle);
  end;

end;


 // Sets "autoStart" to TRUE if the specified service starts up on system boot,
 // FALSE if it starts up on demand
 // Returns: TRUE/FALSE on success/failure
function TDriverControl.GetServiceAutoStart(service: String;
  var autoStart: Boolean): Boolean;
var
  junkFilename: String;
begin
  Result := DriverConfig(service, autoStart, junkFilename);

end;


 // autoStart - Set to TRUE to start the service on boot, set to FALSE for manual startup
 // Returns: TRUE/FALSE on success/failure
function TDriverControl.SetServiceAutoStart(service: String;
  autoStart: Boolean): Boolean;
var
  startType:     DWORD;
  serviceHandle: SC_HANDLE;
  lock:          SC_LOCK;
begin
  Result := False;

  // Determine const to use...
  startType := SERVICE_DEMAND_START;
  if (autoStart) then begin
    startType := SERVICE_SYSTEM_START;
  end;

  // Make the change...
  if FreeOTFEOpenService(service, serviceHandle) then begin
    lock := LockServiceDatabase(uSCManager
      // handle of service control manager database
      );
    try
      Result := ChangeServiceConfig(serviceHandle,
                            // handle to service
        SERVICE_NO_CHANGE,  // type of service
        startType,          // when to start service
        SERVICE_NO_CHANGE,  // severity if service fails to start
        nil,                // pointer to service binary file name
        nil,                // pointer to load ordering group name
        nil,                // pointer to variable to get tag identifier
        nil,                // pointer to array of dependency names
        nil,                // pointer to account name of service
        nil,                // pointer to password for service account
        nil                 // pointer to display name
        );
    finally
      if (lock <> nil) then begin
        UnlockServiceDatabase(lock);
      end;
    end;

    FreeOTFECloseService(serviceHandle);
  end;

end;



 // Returns: A bitmask, with the following flag bits set as appropriate:
 //          DRIVER_BIT_SUCCESS - Set on sucessful uninstall (Note: Other flag
 //                               bits may well be set in this case)
 //          DRIVER_BIT_REBOOT_REQ - Reboot required for changes to take effect
 //          DRIVER_BIT_FILE_REMAINS - User should remove driver file from dir
function TDriverControl.UninstallDriver(service: String): DWORD;
var
  serviceHandle:  SC_HANDLE;
  rebootReq:      Boolean;
  serviceState:   DWORD;
  portableMode:   Boolean;
  driverFilename: String;
  junkAutoStart:  Boolean;
begin
  Result := 0;

  rebootReq := False;

  if FreeOTFEOpenService(service, serviceHandle) then begin
    // If the service is running, stop it
    if (GetServiceState(service, serviceState)) then begin
      if (serviceState = SERVICE_RUNNING) then begin
        StartStopService(service, False);
      end;
    end;

    // If the service is still running, warn the user later...
    if (GetServiceState(service, serviceState)) then begin
      if (serviceState = SERVICE_RUNNING) then begin
        rebootReq := True;
      end;
    end;


    // Store whether the driver was installed in portable mode or not, BEFORE
    // we remove it...
    if not (IsDriverInstalledPortable(service, portableMode)) then begin
      // Fallback to assuming it was fully installed...
      portableMode := False;
    end;

    // Store the driver's filename, BEFORE we remove it...
    // Attempt to identify the filename of the installed driver
    if not (DriverConfig(service, junkAutoStart, driverFilename)) then begin
      // Fallback to standard location
      driverFilename := SDUGetSpecialFolderPath(CSIDL_SYSTEM) + '\' + service + '.sys';
    end;

    // Note: No need to inform user if unable to open - FreeOTFEOpenService(...)
    //       does this for us
    if DeleteService(serviceHandle) then begin
      // Ditch the service handle.
      // Note: This *should* cause the service to be removed
      FreeOTFECloseService(serviceHandle);

      Result := Result or DRIVER_BIT_SUCCESS;

      if rebootReq then begin
        Result := Result or DRIVER_BIT_REBOOT_REQ;
      end;

      // Delete the associated driver file
      if not (portableMode) then begin
        if not (UninstallDriverFile(driverFilename)) then begin
          Result := Result or DRIVER_BIT_FILE_REMAINS;
        end;
      end;

    end;

  end;


  // Close and reopen the SC Manager handle to ensure that it notices any
  // changes...
  // xxx - IS THIS *REALLY* NEEDED???
  SCManagerClose();
  SCManagerOpen();

  Result := Result;
end;


// Delete the associated driver file
function TDriverControl.UninstallDriverFile(driverFilename: String): Boolean;
var
  changeFSRedirect:   Boolean;
  fsRedirectOldValue: pointer;
begin
  changeFSRedirect := SDUWow64DisableWow64FsRedirection(fsRedirectOldValue);
  try
    Result := DeleteFile(driverFilename);
  finally
    if changeFSRedirect then begin
      SDUWow64RevertWow64FsRedirection(fsRedirectOldValue);
    end;
  end;

end;


 // Install, set to start automatically on reboot, and start the driver specified
 // Returns TRUE if 100% OK, otherwise FALSE (e.g. installed, but couldn't
 // start)
 // Note: This function WILL DISPLAY MESSAGES to the user, as appropriate
function TDriverControl.InstallSetAutoStartAndStartDriver(
  pathAndFilename: String): Boolean;
var
  status:              DWORD;
  msgSpecific:         String;
  msgAlreadyInstalled: String;
  msgCantCopyFile:     String;
  driverName:          String;
  startedOK:           Boolean;
  serviceState:        DWORD;
  msgReboot:           String;
begin
  Result := False;

  msgSpecific := '';

  status := InstallDriver(pathAndFilename, False, driverName);

  if ((status and DRIVER_BIT_SUCCESS) = DRIVER_BIT_SUCCESS) then begin
    // Installation SUCCESSFULL
    if ((status and DRIVER_BIT_REBOOT_REQ) = DRIVER_BIT_REBOOT_REQ) then begin
      msgSpecific := _('Please reboot your computer to allow changes to take effect.');
    end else begin
      // Set driver to autostart on reboot
      SetServiceAutoStart(driverName, True);

      // Start the driver *now*, if it hasn't already been started.
      startedOK := False;
      // (This check should be redundant - we've only just installed it!)
      if GetServiceState(driverName, serviceState) then begin
        startedOK := (serviceState = SERVICE_RUNNING);
      end;

      if not (startedOK) then begin
        // Start the driver, if needed
        startedOK := StartStopService(driverName, True);
      end;

      if startedOK then begin
        Result      := True;
        msgSpecific := _('This driver has been installed, started, and is now available for use');
      end else begin
        msgSpecific :=
          _('This driver has been installed but not started; you may need to reboot your PC for the installation to take effect');
      end;
    end;


    _MsgDlg(
      Format(_('Driver %s installed'), [driverName]) + SDUCRLF +
      SDUCRLF + msgSpecific,
      mtInformation
      );
  end else begin
    // Installation FAILED
    msgReboot := '';
    if ((status and DRIVER_BIT_REBOOT_REQ) = DRIVER_BIT_REBOOT_REQ) then begin
      msgReboot :=
        SDUCRLF + SDUCRLF +
        // 2 x CRLF - one to end the last line, one for spacing
        _('Please reboot your computer to allow changes to take effect.');
    end;

    msgAlreadyInstalled := '';
    if ((status and DRIVER_BIT_ALREADY_INSTALLED) = DRIVER_BIT_ALREADY_INSTALLED) then begin
      msgAlreadyInstalled :=
        SDUCRLF + SDUCRLF +
        // 2 x CRLF - one to end the last line, one for spacing
        _('This driver appears to already be installed.') + SDUCRLF +
        _('Please uninstall the existing driver before attempting to reinstall.');
    end;

    msgCantCopyFile := '';
    if ((status and DRIVER_BIT_CANT_INSTALL_FILE) = DRIVER_BIT_CANT_INSTALL_FILE) then begin
      msgCantCopyFile :=
        SDUCRLF + SDUCRLF +
        // 2 x CRLF - one to end the last line, one for spacing
        _('Unable to copy driver file to windows driver directory');
    end;

    if not (silent) then begin
      _MsgDlg(
        _('Unable to install driver.') + msgAlreadyInstalled +
        msgCantCopyFile + msgReboot,
        mtError
        );
    end;
  end;

end;


 // Install a new FreeOTFE device driver
 // driverName - If the driver is installed successfully, then driverName will
 //              be set to the name of the driver
 // portableMode - Install in portable mode
 // Returns: A bitmask, with the following flag bits set as appropriate:
 //          DRIVER_BIT_ALREADY_INSTALLED - Set if the driver appears to already
 //                                         be installed
 //          DRIVER_BIT_SUCCESS - Set on sucessful uninstall (Note: Other flag
 //                               bits may well be set in this case)
 //          DRIVER_BIT_REBOOT_REQ - Reboot required for changes to take effect
 //          DRIVER_BIT_CANT_INSTALL_FILE - Set if the driver file couldn't be
 //                                         copied to the windows system32
 //                                         driver dir
function TDriverControl.InstallDriver(
  filename: String;
  portableMode: Boolean;
  var driverName: String): DWORD;
var
  newServiceHandle: SC_HANDLE;
  installedFilename, finalFileName: String;
  allOK:            Boolean;
  alreadyInstalled: Boolean;
begin
  Result := 0;
  allOK  := True;

  driverName := GetServiceNameForFilename(filename);

  if IsDriverInstalled(driverName, alreadyInstalled) then begin
    if (alreadyInstalled) then begin
      Result := Result and DRIVER_BIT_ALREADY_INSTALLED;
      allOK  := False;
    end;
  end;

  // convert mapped drives to unmapped here because admin cant access drives mapped by another user
  finalFileName := SDUGetFinalPath(filename);
  if (allOK) then begin
    // Copy the new driver over to the <windows>\system32\drivers dir...
    // DO NOT DO THIS IF THE DRIVER IS BEING INSTALLED IN PORTABLE MODE!!!
    if not (portableMode) then begin
      allOK         := InstallDriverFile(finalFileName, installedFilename);
      finalFileName := installedFilename;
      if not (allOK) then begin
        Result := Result or DRIVER_BIT_CANT_INSTALL_FILE;
      end;
    end;
  end;


  if (allOK) then begin

    // Create the service...
    newServiceHandle := CreateService(uSCManager,
                          // handle to service control manager database
      PChar(driverName),  // pointer to name of service to start
      PChar(driverName),  // pointer to display name
      SERVICE_ALL_ACCESS,  // type of access to service
      SERVICE_KERNEL_DRIVER,  // type of service
      SERVICE_DEMAND_START,  // when to start service
      SERVICE_ERROR_NORMAL,  // severity if service fails to start
      PChar(finalFileName),  // pointer to name of binary file
      nil,                // pointer to name of load ordering group
      nil,                // pointer to variable to get tag identifier
      nil,                // pointer to array of dependency names
      nil,                // pointer to account name of service
      nil                 // pointer to password for service account
      );

    if (newServiceHandle <> 0) then begin
      // We don't actually *need* the service handle CreateService returns;
      // just close it.
      FreeOTFECloseService(newServiceHandle);

      // Flag in the registry that the driver is a FreeOTFE driver
      SetDriverReg(driverName, REG_FREEOTFE_DRIVER, True);

      // Flag in the registry if the driver was installed in portable mode
      // or not
      SetDriverReg(driverName, REG_PORTABLE_MODE, portableMode);

      // Close and reopen the SC Manager handle to ensure that it notices any
      // changes...
      { TODO -otdk -cinvestigate : IS THIS *REALLY* NEEDED }
      SCManagerClose();
      SCManagerOpen();

      Result := Result or DRIVER_BIT_SUCCESS;
    end else begin
      MessageDlg(_('Service start failed: ') + SysErrorMessage(GetLastError), mtError, [mbOK], 0);
    end;
  end;  // if allOK

end;


 // Install a new FreeOTFE device driver
 // ...Install the driver file in the <windows>\system32\drivers dir
 // destFilename will be set to the full filename of the installed file
function TDriverControl.InstallDriverFile(driverFilename: String;
  var destFilename: String): Boolean;
var
  changeFSRedirect:   Boolean;
  fsRedirectOldValue: Pointer;
begin
  destFilename := SDUGetSpecialFolderPath(CSIDL_SYSTEM) + '\' + ExtractFilename(driverFilename);
  //  destFilename := 'C:\Windows\sysnative\'+ExtractFilename(driverFilename);

  if (driverFilename = destFilename) then begin
    // Skip copying if file is already in position...
    Result := True;
  end else begin
    changeFSRedirect := SDUWow64DisableWow64FsRedirection(fsRedirectOldValue);
    try
      Result := CopyFile(PChar(driverFilename),
        // pointer to name of an existing file
        PChar(destFilename),    // pointer to filename to copy to
        False                   // flag for operation if file exists
        );
    finally
      if changeFSRedirect then begin
        SDUWow64RevertWow64FsRedirection(fsRedirectOldValue);
      end;
    end;
  end;

end;


// Open the Service Control Manager
function TDriverControl.SCManagerOpen(): Boolean;
var
  allOK: Boolean;
begin
  if (uSCManager <> 0) then begin
    SCManagerClose();
  end;
  uSCManager := OpenSCManager(nil,
    // pointer to machine name string
    nil,                   // pointer to database name string
    SC_MANAGER_ALL_ACCESS  // type of access
    );

  allOK := (uSCManager <> 0);

  Result := allOK;
end;


// Close the Service Control Manager
procedure TDriverControl.SCManagerClose();
begin
  if (uSCManager <> 0) then begin
    CloseServiceHandle(uSCManager);
    uSCManager := 0;   //tdk change
  end;

end;






 // !! WARNING !!
 // IF THIS FUNCTION IS CHANGED, THEN ALL OTHER SetDriverReg(...) SHOULD
 // PROBABLY ALSO BE CHANGED TO MATCH
function TDriverControl.SetDriverReg(service: String; Name: String;
  Value: String): Boolean;
var
  regObj: TRegistry;
begin
  Result := False;

  regObj := TRegistry.Create();
  try
    regObj.RootKey   := HKEY_LOCAL_MACHINE;
    regObj.LazyWrite := False;
    regObj.Access    := KEY_ALL_ACCESS;

    if regObj.OpenKey(REGKEY_BASE + '\' + service, False) then begin
      try
        RegObj.WriteString(Name, Value);
        Result := True;
      except
        // Do nothing; Result already defaults to FALSE
      end;

      RegObj.CloseKey();
    end;  // if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then

  finally
    regObj.Free();
  end;

end;


 // !! WARNING !!
 // IF THIS FUNCTION IS CHANGED, THEN ALL OTHER SetDriverReg(...) SHOULD
 // PROBABLY ALSO BE CHANGED TO MATCH
function TDriverControl.SetDriverReg(service: String; Name: String;
  Value: Boolean): Boolean;
var
  regObj: TRegistry;
begin
  Result := False;

  regObj := TRegistry.Create();
  try
    regObj.RootKey   := HKEY_LOCAL_MACHINE;
    regObj.LazyWrite := False;
    regObj.Access    := KEY_ALL_ACCESS;

    if regObj.OpenKey(REGKEY_BASE + '\' + service, False) then begin
      try
        RegObj.WriteBool(Name, Value);
        Result := True;
      except
        // Do nothing; Result already defaults to FALSE
      end;

      RegObj.CloseKey();
    end;  // if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then

  finally
    regObj.Free();
  end;

end;


{
Commented out; not used atm
// !! WARNING !!
// IF THIS FUNCTION IS CHANGED, THEN ALL OTHER GetDriverRegXXX(...) SHOULD
// PROBABLY ALSO BE CHANGED TO MATCH
function TDriverControl.GetDriverRegString(service: string; name: string; var value: string): boolean;
var
  regObj: TRegistry;
begin
  Result := FALSE;

  regObj := TRegistry.Create();
  try
    regObj.RootKey := HKEY_LOCAL_MACHINE;
    regObj.Access := KEY_READ;
    
    if regObj.OpenKeyReadOnly(REGKEY_BASE+'\'+service) then
      begin
      try
        if (RegObj.ValueExists(name)) then
          begin
          value := RegObj.ReadString(name);
          Result := TRUE;
          end;
      except
        // Do nothing; Result already defaults to FALSE
      end;

      RegObj.CloseKey();
      end;  // if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then

  finally
    regObj.Free();
  end;


end;
}


 // !! WARNING !!
 // IF THIS FUNCTION IS CHANGED, THEN ALL OTHER GetDriverRegXXX(...) SHOULD
 // PROBABLY ALSO BE CHANGED TO MATCH
function TDriverControl.GetDriverRegBoolean(service: String;
  Name: String; var Value: Boolean): Boolean;
var
  regObj: TRegistry;
begin
  Result := False;

  regObj := TRegistry.Create();
  try
    regObj.RootKey := HKEY_LOCAL_MACHINE;
    regObj.Access  := KEY_READ;
    if regObj.OpenKeyReadOnly(REGKEY_BASE + '\' + service) then begin
      try
        if (RegObj.ValueExists(Name)) then begin
          Value  := RegObj.ReadBool(Name);
          Result := True;
        end;
      except
        // Do nothing; Result already defaults to FALSE
      end;

      RegObj.CloseKey();
    end;  // if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then

  finally
    regObj.Free();
  end;

end;


function TDriverControl.IsDriverInstalled(service: String;
  var installed: Boolean): Boolean;
var
  driverList: TStringList;
  allOK:      Boolean;
begin
  driverList := TStringList.Create();
  try
    allOK := GetFreeOTFEDrivers(driverList);
    if (allOK) then begin
      installed := (driverList.IndexOf(service) >= 0);
    end;

  finally
    driverList.Free();
  end;

  Result := allOK;

end;


function TDriverControl.IsDriverInstalledPortable(service: String;
  var portableMode: Boolean): Boolean;
begin
  Result := GetDriverRegBoolean(service, REG_PORTABLE_MODE, portableMode);

end;


 // Get the service name for the specified driver filename, if it exists as a
 // service
 // Can be supplied with a full path+filename, or just a filename
 // Returns "" on failure/if no service with the filename exists
function TDriverControl.GetServiceNameForFilename(filename: String): String;
var
  driverName:  String;
  filenameExt: String;
begin
  // Strip off the filename extension, and the file path to generate the
  // drivername
  driverName  := filename;
  filenameExt := ExtractFileExt(driverName);
  // "+1" here because it's indexed from one
  Delete(
    driverName,
    (length(filename) - length(filenameExt) + 1),
    length(filenameExt)
    );
  driverName := ExtractFileName(driverName);

  Result := driverName;
end;






 // Return total count of drivers installed
 // Returns -1 on error, or the count
function TDriverControl.CountDrivers(): Integer;
var
  driverNames: TStringList;
begin
  Result      := -1;
  driverNames := TStringList.Create();
  try
    if GetFreeOTFEDrivers(driverNames) then begin
      Result := driverNames.Count;
    end;
  finally
    driverNames.Free();
  end;

end;

// Return total count of drivers in portable/non portable mode
function TDriverControl.CountDrivers(portableMode: Boolean): Integer;
var
  driverNames:  TStringList;
  testPortable: Boolean;
  i:            Integer;
begin
  Result      := -1;
  driverNames := TStringList.Create();
  try
    if GetFreeOTFEDrivers(driverNames) then begin
      Result := 0;
      for i := 0 to (driverNames.Count - 1) do begin
        if IsDriverInstalledPortable(driverNames[i], testPortable) then begin
          if (testPortable = portableMode) then begin
            Inc(Result);
          end;
        end;
      end;
    end;
  finally
    driverNames.Free();
  end;

end;

function TDriverControl.InstallMultipleDrivers(
  driverFilenames: TStringList;
  portableMode: Boolean;
  showProgress: Boolean;
  startAfterInstall: Boolean): Boolean;
var
  i:                Integer;
  status:           DWORD;
  installedOK:      Boolean;
  startedOK:        Boolean;
  currDriverName:   String;
  progressDlg:      TdlgProgress;
  serviceState:     DWORD;
  prevCursor:       TCursor;
  alreadyInstalled: Boolean;
begin
  Result := True;

  progressDlg := nil;
  prevCursor  := Screen.Cursor;
  if showProgress then begin
    progressDlg := TdlgProgress.Create(nil);
  end;

  try
    if (progressDlg <> nil) then begin
      progressDlg.Min      := 0;
      // Yes, this is correct; it's driverFilenames.count and *not*
      // (driverFilenames.count - 1)
      progressDlg.Max      := driverFilenames.Count;
      progressDlg.Position := 0;
      if portableMode then begin
        progressDlg.Title := _('Starting drivers in portable mode...');
      end else begin
        progressDlg.Title := _('Installing drivers...');
      end;
      progressDlg.ShowStatusText := True;
      progressDlg.Show();
      Screen.Cursor := crAppStart;
      Application.ProcessMessages();
    end;

    for i := 0 to (driverFilenames.Count - 1) do begin
      if (progressDlg <> nil) then begin
        if portableMode then begin
          progressDlg.StatusText := _('Starting: ');
        end else begin
          progressDlg.StatusText := _('Installing: ');
        end;
        progressDlg.StatusText := progressDlg.StatusText +
          ExtractFilename(driverFilenames[i]) +
          '...';

        Application.ProcessMessages();
      end;

      // If a driver which uses the same filename (without the path) is
      // already installed, skip installation - we use the existing one
      currDriverName := GetServiceNameForFilename(driverFilenames[i]);
      if not (IsDriverInstalled(currDriverName,
        alreadyInstalled)) then begin
        // Sanity... Force FALSE if we couldn't determine if it was installed
        // or not
        alreadyInstalled := False;
      end;

      if alreadyInstalled then begin
        installedOK := True;
      end else begin
        status      := InstallDriver(driverFilenames[i],
          portableMode,
          currDriverName);
        installedOK := ((status and DRIVER_BIT_SUCCESS) = DRIVER_BIT_SUCCESS);
      end;


      // Start the driver, if it hasn't already been started.
      startedOK := False;
      if installedOK then begin
        if not (startAfterInstall) then begin
          // Set flag anyway so that Result is set correctly later
          startedOK := True;
        end else begin
          if GetServiceState(currDriverName,
            serviceState) then begin
            startedOK := (serviceState = SERVICE_RUNNING);
          end;

          if not (startedOK) then begin
            // Start the driver, if needed
            startedOK := StartStopService(
              currDriverName,
              True);
          end;
        end;

        if ((startedOK or not
          (startAfterInstall)          // i.e. We didn't try to start after install
          ) and not (alreadyInstalled) // Don't touch if already installed
          ) then begin
          // Set driver to autostart on reboot - but not if portable mode;
          // the driver may be located on a removable device
          SetServiceAutoStart(currDriverName, not portableMode);
        end;

      end;

      Result := (Result and startedOK);

      if (progressDlg <> nil) then begin
        progressDlg.IncPosition();

        Application.ProcessMessages();

        if progressDlg.Cancel then begin
          break;
        end;
      end;

    end;

  finally
    if showProgress then begin
      if (progressDlg <> nil) then begin
        progressDlg.Free();
        Screen.Cursor := prevCursor;
      end;
    end;
  end;

end;


function TDriverControl.UninstallAllDrivers(portableModeOnly: Boolean): Boolean;
var
  DriverControlObj:     TDriverControl;
  allDrivers:           TStringList;
  wasInstalledPortable: Boolean;
  i:                    Integer;
  status:               DWORD;
  allOK:                Boolean;
begin
  Result := True;

  DriverControlObj := TDriverControl.Create();
  try
    allDrivers := TStringList.Create();
    try
      if DriverControlObj.GetFreeOTFEDrivers(allDrivers) then begin
        for i := 0 to (allDrivers.Count - 1) do begin
          // If the driver was installed in portable mode, stop and uninstall it
          // Determine if driver was installed in portable mode
          if DriverControlObj.IsDriverInstalledPortable(allDrivers[i], wasInstalledPortable) then
          begin
            if (not (portableModeOnly) or // i.e. All drivers
              wasInstalledPortable        // i.e. only if in portable mode
              ) then begin
              // Stop the driver
              DriverControlObj.StartStopService(allDrivers[i], False);

              // Uninstall the driver
              status := DriverControlObj.UninstallDriver(allDrivers[i]);
              allOK  := ((status and DRIVER_BIT_SUCCESS) = DRIVER_BIT_SUCCESS);
              Result := (Result and allOK);
            end;

          end;
          // if DriverControlObj.IsDriverInstalledPortable(allDrivers[i], wasInstalledPortable) then

        end;  // for i:=0 to (allDrivers.count-1) do

      end;  // if DriverControlObj.GetFreeOTFEDrivers(allDrivers) then

    finally
      allDrivers.Free();
    end;

  finally
    DriverControlObj.Free();
  end;

end;



{ TSDUServiceControl }

function TSDUServiceControl._MsgDlg(Content: String; DlgType: TMsgDlgType): Integer;
begin
  Result := 0;

  if not (Silent) then begin
    Result := SDUMessageDlg(Content, DlgType, [mbOK], 0);
  end;

end;

 // Obtain a service handle to the named service
 // Returns: TRUE/FALSE on success/failure. If TRUE, then serviceHandle will be
 //          set to a handle for the named service
function TSDUServiceControl.FreeOTFEOpenService(
  service: String;
  var serviceHandle: SC_HANDLE): Boolean;
begin
  Result := False;

  if (uSCManager <> 0) then begin
    serviceHandle := OpenService(uSCManager,
                          // handle to service control manager database
      PChar(service),       // pointer to name of service to start
      SERVICE_ALL_ACCESS  // type of access to service
      );
    if (serviceHandle <> 0) then begin
      Result := True;
    end else begin
      _MsgDlg(Format(_('Unable to open service for driver "%s"'), [service]) + SDUCRLF +
        SDUCRLF + TEXT_NEED_ADMIN,
        mtError
        );
    end;
  end;

{$IFDEF FREEOTFE_DEBUG}
showmessage('FreeOTFEOpenService: '+inttohex(serviceHandle, 8)+' ['+service+']');
{$ENDIF}

end;


// Closes serviceHandle previously returned from FreeOTFEOpenService(...)
procedure TSDUServiceControl.FreeOTFECloseService(serviceHandle: SC_HANDLE);
begin
{$IFDEF FREEOTFE_DEBUG}
showmessage('FreeOTFECloseService: '+inttohex(serviceHandle, 8));
{$ENDIF}
  CloseServiceHandle(serviceHandle);

end;
// ----------------------------------------------------------------------------
 // This returns a service state as per calls to SERVICE_STATUS struct;
 // dwCurrentState member
 // state - This will be set to the state of the service
 // Returns: TRUE/FALSE on success/failure
function TSDUServiceControl.GetServiceState(service: String; var state: DWORD): Boolean;
var
  serviceHandle: SC_HANDLE;
  serviceStatus: SERVICE_STATUS;
begin
  Result := False;

  if (uSCManager <> 0) then begin
    serviceHandle := 0;
    if (FreeOTFEOpenService(service, serviceHandle)) then begin
      if QueryServiceStatus(serviceHandle,  // handle of service
        serviceStatus                       // address of service status structure
        ) then begin
        state  := serviceStatus.dwCurrentState;
        Result := True;
      end;

      FreeOTFECloseService(serviceHandle);
    end;

  end;

end;

// start - Set to TRUE to start the service, set to FALSE to stop it
function TSDUServiceControl.StartStopService(service: String;
  start: Boolean): Boolean;
var
  serviceHandle: SC_HANDLE;
  serviceStatus: SERVICE_STATUS;
  X:             PChar;  // No idea why this is needed though...
begin
  Result := False;
  X      := nil;

  if (uSCManager <> 0) then begin
    serviceHandle := 0;
    if (FreeOTFEOpenService(service, serviceHandle)) then begin
      if start then begin
        Result := StartService(serviceHandle, // handle of service
          0,  // number of arguments
          X   // address of array of argument string pointers
          );
      end else begin
        Result := ControlService(serviceHandle,  // handle to service
          SERVICE_CONTROL_STOP,  // control code
          serviceStatus  // pointer to service status structure
          );
      end;

      FreeOTFECloseService(serviceHandle);
    end;

  end;

end;

end.
