unit lcCommandLine;

interface

type
  TCommandLine = class (TObject)
  private


    function getIsSilent: Boolean;
    function getIsCount: Boolean;
    function getIsCreate: Boolean;
    function getIsForce: Boolean;
    function getIsNoUacEscalate: Boolean;
    function getIsMinimize: Boolean;
    function getIsNoExit: Boolean;
    function getIsEnableDevMenu: Boolean;
    function getIsDmcrypt: Boolean;
    function getIsDump: Boolean;
    function getIsFreeotfe: Boolean;
    function getIsKeyfileisAscii: Boolean;
    function getIsMount: Boolean;
    function getIsNocdbatoffset: Boolean;
    function getIsReadonly: Boolean;

      function getSetTestmodeArg: String;
    function getDriveArg: String;
    function getKeyfileArg: String;
    function getKeyfilenewlineArg: String;
    function getKeyiterationsArg: String;
    function getLesfileArg: String;
    function getOffsetArg: String;
    function getPasswordArg: String;
    function getSaltlengthArg: String;
    function getVolumeArg: String;
     function getPortableArg: String;
    function getFilenameArg: String;
    function getTypeArg: String;
       function getDriverNameArg: String;
    function getdriverControlArg: String;
    function getSettingsFileArg: String;
    function getDismountArg: String;

  public
    constructor Create();
    // switches
    property isSilent: Boolean Read getIsSilent;
    property isCount: Boolean Read getIsCount;
    property isCreate: Boolean Read getIsCreate;
    property isForce: Boolean Read getIsForce;
    property isNoUacEscalate: Boolean Read getIsNoUacEscalate;
    property isNoExit: Boolean Read getIsNoExit;
    property isMinimize: Boolean Read getIsMinimize;
    property isEnableDevMenu: Boolean Read getIsEnableDevMenu;
    property isDump: Boolean Read getIsDump;
    property isMount: Boolean Read getIsMount;
    property isReadonly: Boolean Read getIsReadonly;
    property isNoCdbAtOffset: Boolean Read getIsNoCdbAtOffset;
    property isKeyfileisAscii: Boolean Read getIsKeyfileisAscii;
    property isFreeotfe: Boolean Read getIsFreeotfe;
    property isDmcrypt: Boolean Read getIsDmcrypt;



    //args
    //all args returned in uppercase
    // for all argumetns if parameter doenst exist or isnt valid, argument == ''
    property portableArg: String Read getPortableArg;
    property filenameArg: String Read getFilenameArg;
    property driverNameArg: String Read getDriverNameArg;
    property driverControlArg: String Read getDriverControlArg;

    property typeArg: String Read getTypeArg;
    // this is argument to driverControl, but as its own arguments
    property setTestmodeArg: String Read getSetTestmodeArg;
    property dismountArg: String Read getDismountArg;
    property settingsFileArg: String Read getSettingsFileArg;

    property volumeArg: String Read getVolumeArg;
    property passwordArg: String Read getPasswordArg;
    property offsetArg: String Read getOffsetArg;
    property saltlengthArg: String Read getSaltlengthArg;
    property keyiterationsArg: String Read getKeyiterationsArg;
    property driveArg: String Read getDriveArg;
    property keyFileArg: String Read getKeyfileArg;
    property keyFilenewlineArg: String Read getKeyfilenewlineArg;
    property lesFileArg: String Read getLesfileArg;


     // returns TRUE if command line options were processed, and "/noexit" wasn't specified as a command line
     // parameter
    // true iff run as command line only - ie should exit after commands processed
    function isCommandLineOnly   : Boolean;
  end;

{returns an instance of the only object. call SetFreeOTFEType first}
function GetCmdLine: TCommandLine;

 //some global as are only used once so easier than creating access fn
 //some are also used in creating cmd line for escalation
const
  //all in caps so compare easier

  //arguments to cmd line parameters


  //args to CMDLINE_PORTABLE
  CMDLINE_TOGGLE = 'TOGGLE';
  CMDLINE_START  = 'START';
  CMDLINE_ON     = 'ON';  // also TestmodeArg
  CMDLINE_STOP   = 'STOP';
  CMDLINE_OFF    = 'OFF';

  //args to  CMDLINE_FILENAME
  CMDLINE_ALL = 'ALL';  //also arg to driverName

  // cmds with params
  CMDLINE_DRIVERCONTROL = 'driverControl';
  CMDLINE_PORTABLE      = 'portable';

  //args to driverControl
  CMDLINE_GUI       = 'GUI';
  CMDLINE_COUNT     = 'COUNT';
  CMDLINE_INSTALL   = 'INSTALL';
  CMDLINE_UNINSTALL = 'UNINSTALL';

  //args to driverControl /type
  CMDLINE_TOTAL           = 'TOTAL';
  CMDLINE_DRIVERSPORTABLE = 'PORTABLE';
  CMDLINE_DRIVERSINSTALLED = 'INSTALLED';

  // Command line parameter handled in the .dpr
  CMDLINE_MINIMIZE = 'minimize';

  // Note: Only used when creating command lines; parsing them is more flexable
  CMDLINE_SWITCH_IND = '/';
  CMDLINE_FILENAME   = 'filename';

  //switches only
  CMDLINE_NOUACESCALATE = 'noUACescalate';

  CMDLINE_SILENT = 'silent';



implementation

uses
  System.SysUtils;

var
  //single instance of object, get by calling GetCommandLine.
  _CommandLineObj: TCommandLine;

{
// Set "value" to the value of the command line parameter "-<parameter> value". Returns TRUE/FALSE on success/failure
function SDUCommandLineParameter(parameter: String; var Value: String): Boolean; overload;
function SDUCommandLineParameter(parameter: String; var Value: Integer): Boolean; overload;
// Returns TRUE if the specified command line switch could be found, otherwise FALSE
function SDUCommandLineSwitch(parameter: String): Boolean;
// Returns the parameter number in the command line of the specified parameter. Returns -1 on failure
function SDUCommandLineSwitchNumber(parameter: String): Integer;}

const
  // Command line parameters. case insensitive. generally only one action per invocation
  // Command line switch indicator
  //common

  // Command line parameters...
  CMDLINE_SETTINGSFILE  = 'settings';
  CMDLINE_DRIVE         = 'drive';
  CMDLINE_SALTLENGTH    = 'saltlength';
  CMDLINE_KEYITERATIONS = 'keyiterations';
  CMDLINE_NOEXIT        = 'noexit';
  CMDLINE_CREATE        = 'create';



  // Command line parameters...
  // CMDLINE_SETTINGSFILE defined in "interface" section; used in HandleCommandLineOpts fn
  CMDLINE_MOUNT           = 'mount';
  CMDLINE_FREEOTFE        = 'LibreCrypt';
  CMDLINE_DMCRYPT         = 'dmcrypt';// mount as plain dm-crypt. no cmd for luks as type detected
  CMDLINE_VOLUME          = 'volume';
  CMDLINE_READONLY        = 'readonly';
  CMDLINE_OFFSET          = 'offset';
  CMDLINE_NOCDBATOFFSET   = 'noCDBatoffset';
  CMDLINE_PASSWORD        = 'password';
  CMDLINE_TYPE            = 'type';
  CMDLINE_KEYFILE         = 'keyfile';
  CMDLINE_KEYFILEISASCII  = 'keyfileisascii';
  CMDLINE_KEYFILENEWLINE  = 'keyfilenewline';
  CMDLINE_LESFILE         = 'lesfile';
  CMDLINE_ENABLE_DEV_MENU = 'dev_menu'; // enable dev menu
  CMDLINE_DUMP            = 'dump';     //dump luks hdr



  //switches only
  CMDLINE_FORCE         = 'force';
  CMDLINE_SET_INSTALLED = 'SetInstalled';
  //sets 'installed' flag in ini file, creating if nec. - usually used with CMDLINE_SETTINGSFILE

  // cmds with params
  CMDLINE_DISMOUNT = 'dismount';


  //  CMDLINE_MOUNTED          = 'mounted'; unused
  CMDLINE_SET_TESTMODE = 'SetTestMode';

  // args to Command line parameters...



  CMDLINE_DRIVERNAME = 'drivername';



//taken from SDUGeneral


function SDUCommandLineParameter(parameter: String; var Value: String): Boolean; overload;
var
  i:         Integer;
  testParam: String;
begin
  Result    := False;
  parameter := uppercase(parameter);
  for i := 1 to (ParamCount - 1) do begin
    testParam := uppercase(ParamStr(i));
    if ((testParam = ('-' + parameter)) or (testParam = ('/' + parameter))) then begin
      Value  := Uppercase(ParamStr(i + 1));
      Result := True;
      break;
    end;
  end;

end;

function SDUCommandLineParameter(parameter: String; var Value: Integer): Boolean; overload;
var
  strValue: String;
begin
  Result := SDUCommandLineParameter(parameter, strValue);
  if Result then
    Result := TryStrToInt(strValue, Value);
end;


function SDUCommandLineSwitch(parameter: String): Boolean;
var
  i: Integer;
begin
  Result    := False;
  parameter := uppercase(parameter);
  for i := 1 to ParamCount do begin
    if (uppercase(ParamStr(i)) = ('-' + parameter)) or (uppercase(ParamStr(i)) = ('/' + parameter))
    then begin
      Result := True;
      break;
    end;
  end;

end;

function SDUCommandLineSwitchNumber(parameter: String): Integer;
var
  i: Integer;
begin
  Result    := -1;
  parameter := uppercase(parameter);
  for i := 1 to ParamCount do begin
    if (uppercase(ParamStr(i)) = ('-' + parameter)) or (uppercase(ParamStr(i)) = ('/' + parameter))
    then begin
      Result := i;
      break;
    end;
  end;

end;



{ factory fn creates an instance
returns an instance of type set in SetFreeOTFEType}
function GetCmdLine: TCommandLine;
begin
  if _CommandLineObj = nil then
    _CommandLineObj := TCommandLine.Create;

  assert(_CommandLineObj <> nil);
  Result := _CommandLineObj;
end;


{ TCommandLine }

function TCommandLine.isCommandLineOnly: Boolean;
var
  ignoreParams: Integer;
    settingsFile: String;
begin

  if GetCmdLine.IsNoExit then begin
     Result := False;
  end else begin

   // Return TRUE if commnd line only - there were parameters specified on the command line
  // and "/noexit" wasn't specified
  // Note: Disregard any CMDLINE_SETTINGSFILE and parameter
  // Note: Also disregard any CMDLINE_MINIMIZE and parameter

    ignoreParams := 0;

    if SDUCommandLineParameter(CMDLINE_SETTINGSFILE, settingsFile) then begin
      Inc(ignoreParams);
      if (settingsFile <> '') then
        Inc(ignoreParams);
    end;

    if GetIsMinimize then
      Inc(ignoreParams);

    Result := (ParamCount > ignoreParams);
  end;
end;

constructor TCommandLine.Create;
begin

end;

 //function TCommandLine.GetIsPortable: Boolean;
 //var
 //  paramValue: String;
 //begin
 //result := SDUCommandLineParameter(CMDLINE_PORTABLE, paramValue);
 //end;


function TCommandLine.getDismountArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_DISMOUNT, Result) then
    Result := '';
end;


function TCommandLine.getDriveArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_DRIVE, Result) then
    Result := '';
end;

function TCommandLine.getdriverControlArg: String;
begin

  if not SDUCommandLineParameter(CMDLINE_DRIVERCONTROL, Result) then
    Result := '';
end;

function TCommandLine.getDriverNameArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_DRIVERNAME, Result) then
    Result := '';
end;

function TCommandLine.GetFilenameArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_FILENAME, Result) then
    Result := '';
end;


function TCommandLine.GetIsCount: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_COUNT);
end;

function TCommandLine.GetIsCreate: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_CREATE);
end;



function TCommandLine.getIsDmcrypt: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_DMCRYPT);
end;

function TCommandLine.getIsDump: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_DUMP);
end;

function TCommandLine.GetIsEnableDevMenu: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_ENABLE_DEV_MENU);
end;

function TCommandLine.GetIsForce: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_FORCE);

end;

function TCommandLine.getIsFreeotfe: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_FREEOTFE);
end;

function TCommandLine.getIsKeyfileisAscii: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_KEYFILEISASCII);
end;

function TCommandLine.GetIsMinimize: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_MINIMIZE);
end;

function TCommandLine.getIsMount: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_MOUNT);
end;

function TCommandLine.getIsNocdbatoffset: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_NOCDBATOFFSET);
end;

function TCommandLine.GetIsNoExit: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_NOEXIT);
end;

function TCommandLine.GetIsNoUacEscalate: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_NOUACESCALATE);
end;

function TCommandLine.getIsReadonly: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_READONLY);
end;

function TCommandLine.GetIsSilent: Boolean;
begin
  Result := SDUCommandLineSwitch(CMDLINE_SILENT);
end;



function TCommandLine.getKeyfileArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_KEYFILE, Result) then
    Result := '';
end;

function TCommandLine.getKeyfilenewlineArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_KEYFILENEWLINE, Result) then
    Result := '';
end;

function TCommandLine.getKeyiterationsArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_KEYITERATIONS, Result) then
    Result := '';
end;

function TCommandLine.getLesfileArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_LESFILE, Result) then
    Result := '';
end;

function TCommandLine.getOffsetArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_OFFSET, Result) then
    Result := '';
end;

function TCommandLine.getPasswordArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_PASSWORD, Result) then
    Result := '';
end;

function TCommandLine.GetPortableArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_PORTABLE, Result) then
    Result := '';
end;



function TCommandLine.getSaltlengthArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_SALTLENGTH, Result) then
    Result := '';
end;

function TCommandLine.getSetTestmodeArg: String;
begin

  if not SDUCommandLineParameter(CMDLINE_SET_TESTMODE, Result) then
    Result := '';
end;

function TCommandLine.getSettingsFileArg: String;
begin

  if not SDUCommandLineParameter(CMDLINE_SETTINGSFILE, Result) then
    Result := '';
end;

function TCommandLine.getTypeArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_TYPE, Result) then
    Result := '';
end;

function TCommandLine.getVolumeArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_VOLUME, Result) then
    Result := '';
end;



initialization
  _CommandLineObj := nil; //create by calling SetFreeOTFEType

finalization
  _CommandLineObj.Free;
end.
