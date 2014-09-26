unit OTFEFreeOTFE_DriverControl;
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
  DRIVER_BIT_SUCCESS           =  1;
  DRIVER_BIT_REBOOT_REQ        =  2;
  DRIVER_BIT_FILE_REMAINS      =  4;
  DRIVER_BIT_CANT_INSTALL_FILE =  8;
  DRIVER_BIT_ALREADY_INSTALLED = 16;

resourcestring
  TEXT_NEED_ADMIN = 'Administrator privileges are required in order to carry out this operation.';

type
  TOTFEFreeOTFEDriverControl = class
  private
    FSilent: boolean;
//    SCManager: SC_HANDLE;

    function  _MsgDlg(Content: string; DlgType: TMsgDlgType): integer;

    function  FreeOTFEOpenService(
                                  service: string;
                                  var serviceHandle: SC_HANDLE
                                 ): boolean;
    procedure FreeOTFECloseService(serviceHandle: SC_HANDLE);

    // Open the Service Control Manager
    function  SCManagerOpen(): boolean;
    // Close the Service Control Manager
    procedure SCManagerClose();

    function  InstallDriverFile(driverFilename: string; var destFilename: string): boolean;
    function  UninstallDriverFile(driverFilename: string): boolean;

    function  GetFreeOTFEDriversInState(serviceState: DWORD; var driverNames: TStringList): boolean;
    function  DriverConfig(service: string; var autoStart: boolean; var fullDriverFilename: string): boolean;

    // Set a registry setting for the specified driver...
    function  SetDriverReg(service: string; name: string; value: string): boolean; overload;
    function  SetDriverReg(service: string; name: string; value: boolean): boolean; overload;
    // Get a registry setting for the specified driver...
{
Commented out; not used atm
    function  GetDriverRegString(service: string; name: string; var value: string): boolean;
}
    function  GetDriverRegBoolean(service: string; name: string; var value: boolean): boolean;

  public
    constructor Create();
    destructor  Destroy(); override;


    property Silent: boolean read FSilent write FSilent;
    
    function  InstallSetAutoStartAndStartDriver(pathAndFilename: string): boolean; overload;
    function  InstallMultipleDrivers(
                                     driverFilenames: TStringList;
                                     portableMode: boolean;
                                     showProgress: boolean;
                                     startAfterInstall: boolean
                                    ): boolean;
    function  InstallDriver(filename: string; portableMode: boolean; var driverName: string): DWORD;
    // Check to see if the named driver is installed
    function  IsDriverInstalled(service: string; var installed: boolean): boolean;
    // Check to see if the named driver is installed in portable mode
    function  IsDriverInstalledPortable(service: string; var portableMode: boolean): boolean;
    function  UninstallDriver(service: string): DWORD;
    function  UninstallAllDrivers(portableModeOnly: boolean): boolean;

    function  GetServiceState(service: string; var state: DWORD): boolean;
    function  GetFreeOTFEDrivers(var driverNames: TStringList): boolean;
    function  StartStopService(service: string; start: boolean): boolean;
    function  GetServiceAutoStart(service: string; var autoStart: boolean): boolean;
    function  SetServiceAutoStart(service: string; autoStart: boolean): boolean;

    function  GetServiceNameForFilename(filename: string): string;

    // Return total count of drivers installed
    function  CountDrivers(): integer; overload;
    // Return total count of drivers in portable/non portable mode
    function  CountDrivers(portableMode: boolean): integer; overload;
  end;

implementation

uses
  registry,
  Controls, Forms,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  SDUi18n,
  SDUDialogs,
  SDUProgressDlg,
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
{ TODO -otdk -crefactor : better make TOTFEFreeOTFEDriverControl singleton }

    uSCManager: SC_HANDLE = 0;

// ----------------------------------------------------------------------------
constructor TOTFEFreeOTFEDriverControl.Create();
begin
  FSilent := FALSE;

if uSCManager = 0 then
  if not(SCManagerOpen()) then
    begin
    raise EFreeOTFENeedAdminPrivs.Create(
               _('Unable to open service control manager.')+SDUCRLF+
               SDUCRLF+
               TEXT_NEED_ADMIN
              );
    end;

end;


// ----------------------------------------------------------------------------
destructor TOTFEFreeOTFEDriverControl.Destroy();
begin
 {  dont call SCManagerClose();  - see comment for uSCManager.
    this isnt a resource leak bc CreateService doesnt actually create anything - only gets handle}
  inherited;
end;


// ----------------------------------------------------------------------------
// This returns a service state as per calls to SERVICE_STATUS struct;
// dwCurrentState member
// state - This will be set to the state of the service
// Returns: TRUE/FALSE on success/failure
function TOTFEFreeOTFEDriverControl.GetServiceState(service: string; var state: DWORD): boolean;
var
  serviceHandle: SC_HANDLE;
  retVal: boolean;
  serviceStatus: SERVICE_STATUS;
begin
  retVal := FALSE;

  if (uSCManager<>0) then
    begin
    serviceHandle := 0;
    if (FreeOTFEOpenService(service, serviceHandle)) then
      begin
      if QueryServiceStatus(
                            serviceHandle,  // handle of service
                            serviceStatus   // address of service status structure
                           ) then
        begin
        state := serviceStatus.dwCurrentState;
        retVal := TRUE;
        end;

      FreeOTFECloseService(serviceHandle);
      end;

    end;

  Result := retVal;
end;




// driverNames - Set to a TStringList object which will have the driver names
//               *added* to it
function TOTFEFreeOTFEDriverControl.GetFreeOTFEDrivers(var driverNames: TStringList): boolean;
var
  retVal: boolean;
begin
  retVal := TRUE;

  retVal := retVal AND GetFreeOTFEDriversInState(SERVICE_ACTIVE, driverNames);
  retVal := retVal AND GetFreeOTFEDriversInState(SERVICE_INACTIVE, driverNames);

  Result := retVal;
end;


// dwServiceState - Set to SERVICE_ACTIVE or SERVICE_INACTIVE
// driverNames - Set to a TStringList object which will have the driver names
//               *added* to it
function TOTFEFreeOTFEDriverControl.GetFreeOTFEDriversInState(serviceState: DWORD; var driverNames: TStringList): boolean;
const
  // Arbitarily high number of services, but should be higher than the
  // number which will ever be encountered
  LARGE_NUMBER_OF_SERVICES = 100000;
type
  TEnumServices = array[0..(LARGE_NUMBER_OF_SERVICES-1)] of TEnumServiceStatus;
  PEnumServices = ^TEnumServices;
var
  retVal: boolean;
  bytesSupplied: DWORD;
  bytesNeeded: DWORD;
  cntServicesReturned: DWORD;
  resumeHandle: DWORD;
  serviceList: PEnumServices;
  i: integer;
  currDriverName: string;
  flagFreeOTFEDriver: boolean;
begin
  retVal := TRUE;

  if (uSCManager<>0) then
    begin
    resumeHandle:= 0;
    serviceList:= nil;

    EnumServicesStatus(
                       uSCManager,  // handle to service control manager database
                       SERVICE_DRIVER,  // type of services to enumerate
                       serviceState,  // state of services to enumerate
                       serviceList[0],  // pointer to service status buffer
                       0,  // size of service status buffer
                       bytesNeeded,  // pointer to variable for bytes needed
                       cntServicesReturned,  // pointer to variable for number returned
                       resumeHandle  // pointer to variable for next entry
                      );

    serviceList := AllocMem(bytesNeeded);
    try
      bytesSupplied := bytesNeeded;
      resumeHandle:= 0;

      if EnumServicesStatus(
                            uSCManager,  // handle to service control manager database
                            SERVICE_DRIVER,  // type of services to enumerate
                            serviceState,  // state of services to enumerate
                            serviceList[0],  // pointer to service status buffer
                            bytesSupplied,  // size of service status buffer
                            bytesNeeded,  // pointer to variable for bytes needed
                            cntServicesReturned,  // pointer to variable for number returned
                            resumeHandle  // pointer to variable for next entry
                           ) then
        begin
        for i:=0 to (cntServicesReturned-1) do
          begin
          currDriverName := serviceList[i].lpServiceName;

          // Check the registry if the driver has the FreeOTFE driver flag
          if GetDriverRegBoolean(currDriverName, REG_FREEOTFE_DRIVER, flagFreeOTFEDriver) then
            begin
            if (flagFreeOTFEDriver) then
              begin
              driverNames.Add(currDriverName);
              end;

            end;  // if GetDriverRegBoolean(currDriverName, REG_FREEOTFE_DRIVER, flagFreeOTFEDriver) then

          end;  // for i:=0 to (cntServicesReturned-1) do

        end;  // if EnumServicesStatus(

    finally
      FreeMem(serviceList);
    end;
    
    end;

  Result := retVal;
end;


// start - Set to TRUE to start the service, set to FALSE to stop it
function TOTFEFreeOTFEDriverControl.StartStopService(service: string; start: boolean): boolean;
var
  serviceHandle: SC_HANDLE;
  serviceStatus: SERVICE_STATUS;
  X: PChar;  // No idea why this is needed though...
  retVal: boolean;
begin
  retVal := FALSE;
  X := nil;

  if (uSCManager<>0) then
    begin
    serviceHandle := 0;
    if (FreeOTFEOpenService(service, serviceHandle)) then
      begin
      if start then
        begin
        retVal := StartService(
                               serviceHandle, // handle of service
                               0,  // number of arguments
                               X   // address of array of argument string pointers
                              );
        end
      else
        begin
        retVal := ControlService(
                              serviceHandle,	// handle to service
                              SERVICE_CONTROL_STOP,	// control code
                              serviceStatus  // pointer to service status structure
                             );
        end;

      FreeOTFECloseService(serviceHandle);
      end;

    end;

  Result := retVal;
end;


function TOTFEFreeOTFEDriverControl.DriverConfig(service: string; var autoStart: boolean; var fullDriverFilename: string): boolean;
var
  retVal: boolean;
  serviceHandle: SC_HANDLE;
  serviceConfig: LPQUERY_SERVICE_CONFIGW;
  bytesNeeded: DWORD;
  bytesSupplied: DWORD;
begin
  retVal := FALSE;

  if FreeOTFEOpenService(service, serviceHandle) then
    begin
    serviceConfig := nil;
    // Note: We need to go through this weridness and can't use
    //       sizeof(TQueryServiceConfig), as QueryServiceConfig(...) writes the
    //       strings pointed to in the TQueryServiceConfig it's passed *after*
    //       the TQueryServiceConfig!!!
    QueryServiceConfig(
                       serviceHandle,  // handle of service
                       serviceConfig,  // address of service config. structure
                       0,  // size of service configuration buffer
                       bytesNeeded  // address of variable for bytes needed
                      );
    serviceConfig := AllocMem(bytesNeeded);
    try
      bytesSupplied := bytesNeeded;
      if QueryServiceConfig(
                            serviceHandle,  // handle of service
                            serviceConfig,  // address of service config. structure
                            bytesSupplied,  // size of service configuration buffer
                            bytesNeeded  // address of variable for bytes needed
                           ) then
        begin
        autoStart := (serviceConfig.dwStartType = SERVICE_SYSTEM_START);

        fullDriverFilename := copy(serviceConfig.lpBinaryPathName, 0, StrLen(serviceConfig.lpBinaryPathName));
        // Remove any "\??\" prefix from filename
        if (Pos('\??\', fullDriverFilename)<>0) then
          begin
          delete(fullDriverFilename, 1, length('\??\'));
          end;

        retVal:= TRUE;
        end;

    finally
      FreeMem(serviceConfig);
    end;

    FreeOTFECloseService(serviceHandle);
    end;

  Result := retVal;
end;


// Sets "autoStart" to TRUE if the specified service starts up on system boot,
// FALSE if it starts up on demand
// Returns: TRUE/FALSE on success/failure
function TOTFEFreeOTFEDriverControl.GetServiceAutoStart(service: string; var autoStart: boolean): boolean;
var
  retVal: boolean;
  junkFilename: string;
begin
  retVal := DriverConfig(service, autoStart, junkFilename);

  Result := retVal;
end;


// autoStart - Set to TRUE to start the service on boot, set to FALSE for manual startup
// Returns: TRUE/FALSE on success/failure
function TOTFEFreeOTFEDriverControl.SetServiceAutoStart(service: string; autoStart: boolean): boolean;
var
  startType: DWORD;
  serviceHandle: SC_HANDLE;
  retVal: boolean;
  lock: SC_LOCK;
begin
  retVal:= FALSE;

  // Determine const to use...
  startType:= SERVICE_DEMAND_START;
  if (autoStart) then
    begin
    startType:= SERVICE_SYSTEM_START;
    end;

  // Make the change...
  if FreeOTFEOpenService(service, serviceHandle) then
    begin
    lock := LockServiceDatabase(
                                uSCManager  // handle of service control manager database
                               );
    try
      retVal := ChangeServiceConfig(
                                    serviceHandle,  // handle to service
                                    SERVICE_NO_CHANGE,  // type of service
                                    startType,  // when to start service
                                    SERVICE_NO_CHANGE,  // severity if service fails to start
                                    nil,  // pointer to service binary file name
                                    nil,  // pointer to load ordering group name
                                    nil,  // pointer to variable to get tag identifier
                                    nil,  // pointer to array of dependency names
                                    nil,  // pointer to account name of service
                                    nil,  // pointer to password for service account
                                    nil  // pointer to display name
                                   );
    finally
      if (lock<>nil) then
        begin
        UnlockServiceDatabase(lock);
        end;
    end;

    FreeOTFECloseService(serviceHandle);
    end;


  Result := retVal;
end;



// Returns: A bitmask, with the following flag bits set as appropriate:
//          DRIVER_BIT_SUCCESS - Set on sucessful uninstall (Note: Other flag
//                               bits may well be set in this case)
//          DRIVER_BIT_REBOOT_REQ - Reboot required for changes to take effect
//          DRIVER_BIT_FILE_REMAINS - User should remove driver file from dir
function TOTFEFreeOTFEDriverControl.UninstallDriver(service: string): DWORD;
var
  serviceHandle: SC_HANDLE;
  retVal: DWORD;
  rebootReq: boolean;
  serviceState: DWORD;
  portableMode: boolean;
  driverFilename: string;
  junkAutoStart: boolean;
begin
  retVal:= 0;

  rebootReq:= FALSE;

  if FreeOTFEOpenService(service, serviceHandle) then
    begin
    // If the service is running, stop it
    if (GetServiceState(service, serviceState)) then
      begin
      if (serviceState = SERVICE_RUNNING) then
        begin
        StartStopService(service, FALSE);
        end;
      end;

    // If the service is still running, warn the user later...
    if (GetServiceState(service, serviceState)) then
      begin
      if (serviceState = SERVICE_RUNNING) then
        begin
        rebootReq := TRUE;
        end;
      end;


    // Store whether the driver was installed in portable mode or not, BEFORE
    // we remove it...
    if not(IsDriverInstalledPortable(service, portableMode)) then
      begin
      // Fallback to assuming it was fully installed...
      portableMode := FALSE;
      end;

    // Store the driver's filename, BEFORE we remove it...
    // Attempt to identify the filename of the installed driver
    if not(DriverConfig(service, junkAutoStart, driverFilename)) then
      begin
      // Fallback to standard location
      driverFilename := SDUGetSpecialFolderPath(CSIDL_SYSTEM)+'\'+service+'.sys';
      end;
      
    // Note: No need to inform user if unable to open - FreeOTFEOpenService(...)
    //       does this for us
    if DeleteService(serviceHandle) then
      begin
      // Ditch the service handle.
      // Note: This *should* cause the service to be removed
      FreeOTFECloseService(serviceHandle);

      retVal := retVal OR DRIVER_BIT_SUCCESS;

      if rebootReq then
        begin
        retVal := retVal OR DRIVER_BIT_REBOOT_REQ;
        end;

      // Delete the associated driver file
      if not(portableMode) then
        begin
        if not(UninstallDriverFile(driverFilename)) then
          begin
          retVal := retVal OR DRIVER_BIT_FILE_REMAINS;
          end;
        end;

      end;

    end;


  // Close and reopen the SC Manager handle to ensure that it notices any
  // changes...
  // xxx - IS THIS *REALLY* NEEDED???
  SCManagerClose();
  SCManagerOpen();

  Result:= retVal;
end;


// Delete the associated driver file
function TOTFEFreeOTFEDriverControl.UninstallDriverFile(driverFilename: string): boolean;
var
  retVal: boolean;
  changeFSRedirect: boolean;
  fsRedirectOldValue: pointer;
begin
  changeFSRedirect := SDUWow64DisableWow64FsRedirection(fsRedirectOldValue);
  try
    retVal := DeleteFile(driverFilename);
  finally
    if changeFSRedirect then
      begin
      SDUWow64RevertWow64FsRedirection(fsRedirectOldValue);
      end;
  end;

  Result := retVal;
end;


// Install, set to start automatically on reboot, and start the driver specified
// Returns TRUE if 100% OK, otherwise FALSE (e.g. installed, but couldn't
// start)
// Note: This function WILL DISPLAY MESSAGES to the user, as appropriate
function TOTFEFreeOTFEDriverControl.InstallSetAutoStartAndStartDriver(pathAndFilename: string): boolean;
var
  status: DWORD;
  msgSpecific: string;
  msgAlreadyInstalled: string;
  msgCantCopyFile: string;
  driverName: string;
  startedOK: boolean;
  serviceState: DWORD;
  msgReboot: string;
  retval: boolean;
begin
  retval:= FALSE;

  msgSpecific := '';

  status := InstallDriver(pathAndFilename, FALSE, driverName);

  if ((status AND DRIVER_BIT_SUCCESS) = DRIVER_BIT_SUCCESS) then
    begin
    // Installation SUCCESSFULL
    if ((status AND DRIVER_BIT_REBOOT_REQ) = DRIVER_BIT_REBOOT_REQ) then
      begin
      msgSpecific := _('Please reboot your computer to allow changes to take effect.');
      end
    else
      begin
      // Set driver to autostart on reboot
      SetServiceAutoStart(driverName, TRUE);

      // Start the driver *now*, if it hasn't already been started.
      startedOK := FALSE;
      // (This check should be redundant - we've only just installed it!)
      if GetServiceState(driverName, serviceState) then
        begin
        startedOK := (serviceState = SERVICE_RUNNING);
        end;

      if not(startedOK) then
        begin
        // Start the driver, if needed
        startedOK := StartStopService(driverName, TRUE);
        end;

      if startedOK then
        begin
        retval := TRUE;
        msgSpecific := _('This driver has been installed, started, and is now available for use');
        end
      else
        begin
        msgSpecific := _('This driver has been installed but not started; you may need to reboot your PC for the installation to take effect');
        end;
      end;


    _MsgDlg(
               SDUParamSubstitute(_('Driver %1 installed'), [driverName])+SDUCRLF+
               SDUCRLF+
               msgSpecific,
               mtInformation
              );
    end
  else
    begin
    // Installation FAILED
    msgReboot := '';
    if ((status AND DRIVER_BIT_REBOOT_REQ) = DRIVER_BIT_REBOOT_REQ) then
      begin
      msgReboot :=
                   SDUCRLF+
                   SDUCRLF+  // 2 x CRLF - one to end the last line, one for spacing
                   _('Please reboot your computer to allow changes to take effect.');
      end;

    msgAlreadyInstalled := '';
    if ((status AND DRIVER_BIT_ALREADY_INSTALLED) = DRIVER_BIT_ALREADY_INSTALLED) then
      begin
      msgAlreadyInstalled :=
                         SDUCRLF+
                         SDUCRLF+  // 2 x CRLF - one to end the last line, one for spacing
                         _('This driver appears to already be installed.')+SDUCRLF+
                         _('Please uninstall the existing driver before attempting to reinstall.');
      end;

    msgCantCopyFile := '';
    if ((status AND DRIVER_BIT_CANT_INSTALL_FILE) = DRIVER_BIT_CANT_INSTALL_FILE) then
      begin
      msgCantCopyFile :=
                         SDUCRLF+
                         SDUCRLF+  // 2 x CRLF - one to end the last line, one for spacing
                         _('Unable to copy driver file to windows driver directory');
      end;

    if not(silent) then
      begin
      _MsgDlg(
               _('Unable to install driver.')+
               msgAlreadyInstalled+
               msgCantCopyFile+
               msgReboot,
               mtError
              );
      end;
    end;

  Result := retval;
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
function TOTFEFreeOTFEDriverControl.InstallDriver(
  filename: string;
  portableMode: boolean;
  var driverName: string
): DWORD;
var
  retVal: DWORD;
  newServiceHandle: SC_HANDLE;
  installedFilename,finalFileName: string;
  allOK: boolean;
  alreadyInstalled: boolean;
begin
  retVal := 0;
  allOK := TRUE;

  driverName := GetServiceNameForFilename(filename);

  if IsDriverInstalled(driverName, alreadyInstalled) then
    begin
    if (alreadyInstalled) then
      begin
      retVal := retVal AND DRIVER_BIT_ALREADY_INSTALLED;
      allOK := FALSE;
      end;
    end;

  // convert mapped drives to unmapped here because admin cant access drives mapped by another user
  finalFileName := SDUGetFinalPath(filename);
  if (allOK) then
    begin
    // Copy the new driver over to the <windows>\system32\drivers dir...
    // DO NOT DO THIS IF THE DRIVER IS BEING INSTALLED IN PORTABLE MODE!!!
    if not(portableMode) then
      begin
      allOK := InstallDriverFile(finalFileName, installedFilename);
      finalFileName := installedFilename;
      if not(allOK) then
        begin
        retVal := retVal OR DRIVER_BIT_CANT_INSTALL_FILE;
        end;
      end;
    end;


  if (allOK) then
    begin


    // Create the service...
    newServiceHandle := CreateService(
                                      uSCManager,  // handle to service control manager database
                                      PChar(driverName),  // pointer to name of service to start
                                      PChar(driverName),  // pointer to display name
                                      SERVICE_ALL_ACCESS,  // type of access to service
                                      SERVICE_KERNEL_DRIVER,  // type of service
                                      SERVICE_DEMAND_START,  // when to start service
                                      SERVICE_ERROR_NORMAL,  // severity if service fails to start
                                      PChar(finalFileName),  // pointer to name of binary file
                                      nil,  // pointer to name of load ordering group
                                      nil,  // pointer to variable to get tag identifier
                                      nil,  // pointer to array of dependency names
                                      nil,  // pointer to account name of service
                                      nil  // pointer to password for service account
                                     );

    if (newServiceHandle<>0) then
      begin
      // We don't actually *need* the service handle CreateService returns;
      // just close it.
      FreeOTFECloseService(newServiceHandle);

      // Flag in the registry that the driver is a FreeOTFE driver
      SetDriverReg(driverName, REG_FREEOTFE_DRIVER, TRUE);

      // Flag in the registry if the driver was installed in portable mode
      // or not
      SetDriverReg(driverName, REG_PORTABLE_MODE, portableMode);

      // Close and reopen the SC Manager handle to ensure that it notices any
      // changes...
      { TODO -otdk -cinvestigate : IS THIS *REALLY* NEEDED }
      SCManagerClose();
      SCManagerOpen();

      retVal := retVal OR DRIVER_BIT_SUCCESS;
      end else begin
        MessageDlg(_('Service start failed: ')+SysErrorMessage(GetLastError), mtError, [mbOK], 0);
      end;
    end;  // if allOK

  Result := retVal;
end;


// Install a new FreeOTFE device driver
// ...Install the driver file in the <windows>\system32\drivers dir
// destFilename will be set to the full filename of the installed file
function TOTFEFreeOTFEDriverControl.InstallDriverFile(driverFilename: string; var destFilename: string): boolean;
var
  retVal: boolean;
  changeFSRedirect: boolean;
  fsRedirectOldValue:  Pointer;
begin
 destFilename := SDUGetSpecialFolderPath(CSIDL_SYSTEM)+'\'+ExtractFilename(driverFilename);
//  destFilename := 'C:\Windows\sysnative\'+ExtractFilename(driverFilename);

  if (driverFilename = destFilename) then
    begin
    // Skip copying if file is already in position...
    retVal := TRUE;
    end
  else
    begin
      changeFSRedirect := SDUWow64DisableWow64FsRedirection(fsRedirectOldValue);
      try
        retVal := CopyFile(
                           PChar(driverFilename),  // pointer to name of an existing file
                           PChar(destFilename),    // pointer to filename to copy to
                           FALSE                   // flag for operation if file exists
                          );
      finally
        if changeFSRedirect then
          begin
          SDUWow64RevertWow64FsRedirection(fsRedirectOldValue);
          end;
      end;
    end;

  Result := retVal;
end;


// Open the Service Control Manager
function TOTFEFreeOTFEDriverControl.SCManagerOpen(): boolean;
var
  allOK: boolean;
begin
  if (uSCManager<>0) then
    begin
    SCManagerClose();
    end;
  uSCManager := OpenSCManager(
                             nil,                   // pointer to machine name string
                             nil,                   // pointer to database name string
                             SC_MANAGER_ALL_ACCESS  // type of access
                            );

  allOK := (uSCManager <> 0);

  Result := allOK;
end;


// Close the Service Control Manager
procedure TOTFEFreeOTFEDriverControl.SCManagerClose();
begin
  if (uSCManager<>0) then
    begin
    CloseServiceHandle(uSCManager);
    uSCManager := 0;   //tdk change
    end;

end;


// Obtain a service handle to the named service
// Returns: TRUE/FALSE on success/failure. If TRUE, then serviceHandle will be
//          set to a handle for the named service
function TOTFEFreeOTFEDriverControl.FreeOTFEOpenService(
  service: string;
  var serviceHandle: SC_HANDLE
): boolean;
var
  retVal: boolean;
begin
  retVal := FALSE;

  if (uSCManager<>0) then
    begin
    serviceHandle := OpenService(
                                 uSCManager,            // handle to service control manager database
                                 PChar(service),       // pointer to name of service to start
                                 SERVICE_ALL_ACCESS  // type of access to service
                                );
    if (serviceHandle <> 0) then
      begin
      retVal := TRUE;
      end
    else
      begin
      _MsgDlg(SDUParamSubstitute(_('Unable to open service for driver "%1"'), [service])+SDUCRLF+
                 SDUCRLF+
                 TEXT_NEED_ADMIN,
                 mtError
                );
      end;
    end;

{$IFDEF FREEOTFE_DEBUG}
showmessage('FreeOTFEOpenService: '+inttohex(serviceHandle, 8)+' ['+service+']');
{$ENDIF}
  Result := retVal;
end;


// Closes serviceHandle previously returned from FreeOTFEOpenService(...)
procedure TOTFEFreeOTFEDriverControl.FreeOTFECloseService(serviceHandle: SC_HANDLE);
begin
{$IFDEF FREEOTFE_DEBUG}
showmessage('FreeOTFECloseService: '+inttohex(serviceHandle, 8));
{$ENDIF}
  CloseServiceHandle(serviceHandle);

end;


// !! WARNING !!
// IF THIS FUNCTION IS CHANGED, THEN ALL OTHER SetDriverReg(...) SHOULD
// PROBABLY ALSO BE CHANGED TO MATCH
function TOTFEFreeOTFEDriverControl.SetDriverReg(service: string; name: string; value: string): boolean;
var
  regObj: TRegistry;
  retVal: boolean;
begin
  retVal := FALSE;

  regObj := TRegistry.Create();
  try
    regObj.RootKey := HKEY_LOCAL_MACHINE;
    regObj.LazyWrite := FALSE;
    regObj.Access := KEY_ALL_ACCESS;
    
    if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then
      begin
      try
        RegObj.WriteString(name, value);
        retVal := TRUE;
      except
        // Do nothing; retVal already defaults to FALSE
      end;

      RegObj.CloseKey();
      end;  // if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then

  finally
    regObj.Free();
  end;

  Result := retVal;
end;


// !! WARNING !!
// IF THIS FUNCTION IS CHANGED, THEN ALL OTHER SetDriverReg(...) SHOULD
// PROBABLY ALSO BE CHANGED TO MATCH
function TOTFEFreeOTFEDriverControl.SetDriverReg(service: string; name: string; value: boolean): boolean;
var
  regObj: TRegistry;
  retVal: boolean;
begin
  retVal := FALSE;

  regObj := TRegistry.Create();
  try
    regObj.RootKey := HKEY_LOCAL_MACHINE;
    regObj.LazyWrite := FALSE;
    regObj.Access := KEY_ALL_ACCESS;
    
    if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then
      begin
      try
        RegObj.WriteBool(name, value);
        retVal := TRUE;
      except
        // Do nothing; retVal already defaults to FALSE
      end;

      RegObj.CloseKey();
      end;  // if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then

  finally
    regObj.Free();
  end;

  Result := retVal;
end;


{
Commented out; not used atm
// !! WARNING !!
// IF THIS FUNCTION IS CHANGED, THEN ALL OTHER GetDriverRegXXX(...) SHOULD
// PROBABLY ALSO BE CHANGED TO MATCH
function TOTFEFreeOTFEDriverControl.GetDriverRegString(service: string; name: string; var value: string): boolean;
var
  regObj: TRegistry;
  retVal: boolean;
begin
  retVal := FALSE;

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
          retVal := TRUE;
          end;
      except
        // Do nothing; retVal already defaults to FALSE
      end;

      RegObj.CloseKey();
      end;  // if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then

  finally
    regObj.Free();
  end;

  Result := retVal;
end;
}


// !! WARNING !!
// IF THIS FUNCTION IS CHANGED, THEN ALL OTHER GetDriverRegXXX(...) SHOULD
// PROBABLY ALSO BE CHANGED TO MATCH
function TOTFEFreeOTFEDriverControl.GetDriverRegBoolean(service: string; name: string; var value: boolean): boolean;
var
  regObj: TRegistry;
  retVal: boolean;
begin
  retVal := FALSE;

  regObj := TRegistry.Create();
  try
    regObj.RootKey := HKEY_LOCAL_MACHINE;
    regObj.Access := KEY_READ;
    if regObj.OpenKeyReadOnly(REGKEY_BASE+'\'+service) then
      begin
      try
        if (RegObj.ValueExists(name)) then
          begin
          value := RegObj.ReadBool(name);
          retVal := TRUE;
          end;
      except
        // Do nothing; retVal already defaults to FALSE
      end;

      RegObj.CloseKey();
      end;  // if regObj.OpenKey(REGKEY_BASE+'\'+service, FALSE) then

  finally
    regObj.Free();
  end;

  Result := retVal;
end;


function TOTFEFreeOTFEDriverControl.IsDriverInstalled(service: string; var installed: boolean): boolean;
var
  driverList: TStringList;
  allOK: boolean;
begin
  driverList := TStringList.Create();
  try
    allOK := GetFreeOTFEDrivers(driverList);
    if (allOK) then
      begin
      installed := (driverList.IndexOf(service) >= 0);
      end;
      
  finally
    driverList.Free();
  end;

  Result := allOK;

end;


function TOTFEFreeOTFEDriverControl.IsDriverInstalledPortable(service: string; var portableMode: boolean): boolean;
begin
  Result := GetDriverRegBoolean(service, REG_PORTABLE_MODE, portableMode);

end;


// Get the service name for the specified driver filename, if it exists as a
// service
// Can be supplied with a full path+filename, or just a filename
// Returns "" on failure/if no service with the filename exists
function TOTFEFreeOTFEDriverControl.GetServiceNameForFilename(filename: string): string;
var
  driverName: string;
  filenameExt: string;
begin
  // Strip off the filename extension, and the file path to generate the
  // drivername
  driverName := filename;
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

function TOTFEFreeOTFEDriverControl._MsgDlg(Content: string; DlgType: TMsgDlgType): integer;
var
  retval: integer;
begin
  retval := 0;

  if not(Silent) then
    begin
    retval := SDUMessageDlg(Content, DlgType, [mbOK], 0);
    end;

  Result := retval;
end;


// Return total count of drivers installed
// Returns -1 on error, or the count
function TOTFEFreeOTFEDriverControl.CountDrivers(): integer;
var
  driverNames: TStringlist;
  retval: integer;
begin
  retval := -1;
  driverNames:= TStringlist.Create();
  try
    if GetFreeOTFEDrivers(driverNames) then
      begin
      retval := driverNames.count;
      end;
  finally
    driverNames.Free();
  end;

  Result := retval;
end;

// Return total count of drivers in portable/non portable mode
function TOTFEFreeOTFEDriverControl.CountDrivers(portableMode: boolean): integer;
var
  driverNames: TStringlist;
  retval: integer;
  testPortable: boolean;
  i: integer;
begin
  retval := -1;
  driverNames:= TStringlist.Create();
  try
    if GetFreeOTFEDrivers(driverNames) then
      begin
      retval := 0;
      for i:=0 to (driverNames.count - 1) do
        begin
        if IsDriverInstalledPortable(driverNames[i], testPortable) then
          begin
          if (testPortable = portableMode) then
            begin
            inc(retval);
            end;
          end;
        end;
      end;
  finally
    driverNames.Free();
  end;

  Result := retval;
end;

function TOTFEFreeOTFEDriverControl.InstallMultipleDrivers(
  driverFilenames: TStringList;
  portableMode: boolean;
  showProgress: boolean;
  startAfterInstall: boolean
): boolean;
var
  retVal: boolean;
  i: integer;
  status: DWORD;
  installedOK: boolean;
  startedOK: boolean;
  currDriverName: string;
  progressDlg: TSDUProgressDialog;
  serviceState: DWORD;
  prevCursor: TCursor;
  alreadyInstalled: boolean;
begin
  retVal := TRUE;

  progressDlg := nil;
  prevCursor := Screen.Cursor;
  if showProgress then
    begin
    progressDlg := TSDUProgressDialog.Create(nil);
    end;

  try
    if (progressDlg <> nil) then
      begin
      progressDlg.Min := 0;
      // Yes, this is correct; it's driverFilenames.count and *not*
      // (driverFilenames.count - 1)
      progressDlg.Max := driverFilenames.count;
      progressDlg.Position := 0;
      if portableMode then
        begin
        progressDlg.Title := _('Starting drivers in portable mode...');
        end
      else
        begin
        progressDlg.Title := _('Installing drivers...');
        end;
      progressDlg.ShowStatusText := TRUE;
      progressDlg.Show();
      Screen.Cursor := crAppStart;
      Application.ProcessMessages();
      end;

    for i:=0 to (driverFilenames.count-1) do
      begin
      if (progressDlg <> nil) then
        begin
        if portableMode then
          begin
          progressDlg.StatusText := _('Starting: ');
          end
        else
          begin
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
      if not(IsDriverInstalled(
                               currDriverName,
                               alreadyInstalled
                              )) then
        begin
        // Sanity... Force FALSE if we couldn't determine if it was installed
        // or not
        alreadyInstalled := FALSE;
        end;

      if alreadyInstalled then
        begin
        installedOK := TRUE;
        end
      else
        begin
        status := InstallDriver(
                                driverFilenames[i],
                                portableMode,
                                currDriverName
                               );
        installedOK := ((status AND DRIVER_BIT_SUCCESS) = DRIVER_BIT_SUCCESS);
        end;


      // Start the driver, if it hasn't already been started.
      startedOK := FALSE;
      if installedOK then
        begin
        if not(startAfterInstall) then
          begin
          // Set flag anyway so that retval is set correctly later
          startedOK := TRUE;
          end
        else
          begin
          if GetServiceState(
                             currDriverName,
                             serviceState
                            ) then
            begin
            startedOK := (serviceState = SERVICE_RUNNING);
            end;

          if not(startedOK) then
            begin
            // Start the driver, if needed
            startedOK := StartStopService(
                                          currDriverName,
                                          TRUE
                                         );
            end;
          end;

        if (
            (
             startedOK or
             not(startAfterInstall) // i.e. We didn't try to start after install
            ) and
            not(alreadyInstalled) // Don't touch if already installed
           ) then
          begin
          // Set driver to autostart on reboot - but not if portable mode;
          // the driver may be located on a removable device
          SetServiceAutoStart(currDriverName, not(portableMode));
          end;

        end;

      retVal := (retVal AND startedOK);

      if (progressDlg <> nil) then
        begin
        progressDlg.IncPosition();

        Application.ProcessMessages();

        if progressDlg.Cancel then
          begin
          break;
          end;
        end;

      end;

  finally
    if showProgress then
      begin
      if (progressDlg <> nil) then
        begin
        progressDlg.Free();
        Screen.Cursor := prevCursor;
        end;
      end;
  end;

  Result := retVal;
end;


function TOTFEFreeOTFEDriverControl.UninstallAllDrivers(portableModeOnly: boolean): boolean;
var
  retVal: boolean;
  DriverControlObj: TOTFEFreeOTFEDriverControl;
  allDrivers: TStringList;
  wasInstalledPortable: boolean;
  i: integer;
  status: DWORD;
  allOK: boolean;
begin
  retVal := TRUE;

  DriverControlObj := TOTFEFreeOTFEDriverControl.Create();
  try
    allDrivers:= TStringList.Create();
    try
      if DriverControlObj.GetFreeOTFEDrivers(allDrivers) then
        begin
        for i:=0 to (allDrivers.count-1) do
          begin
          // If the driver was installed in portable mode, stop and uninstall it
          // Determine if driver was installed in portable mode
          if DriverControlObj.IsDriverInstalledPortable(allDrivers[i], wasInstalledPortable) then
            begin
            if (
                not(portableModeOnly) or // i.e. All drivers
                wasInstalledPortable // i.e. only if in portable mode
               ) then
              begin
              // Stop the driver
              DriverControlObj.StartStopService(allDrivers[i], FALSE);

              // Uninstall the driver
              status := DriverControlObj.UninstallDriver(allDrivers[i]);
              allOK := ((status AND DRIVER_BIT_SUCCESS) = DRIVER_BIT_SUCCESS);
              retVal := (retVal AND allOK);
              end;

            end;  // if DriverControlObj.IsDriverInstalledPortable(allDrivers[i], wasInstalledPortable) then

          end;  // for i:=0 to (allDrivers.count-1) do

        end;  // if DriverControlObj.GetFreeOTFEDrivers(allDrivers) then

    finally
      allDrivers.Free();
    end;

  finally
    DriverControlObj.Free();
  end;

  Result := retVal;
end;


END.


