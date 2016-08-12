unit lcCommandLine;
 // Description:
 // (c)  tdk
 // licence: gpl

interface

uses
  SysUtils,//delphi & libs (0)

  //sdu & LibreCrypt utils (1)
  lcTypes;
// LibreCrypt forms and frames (2)

type

  ECmdLine = class (Exception);

  TCommandLine = class (TObject)
  private

    fis_silent: Boolean; //this is cached so can be overridden for testing

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
    function getSetTestmode: String;
    function getDrive: String;
    function getKeyfile: String;
    function getKeyfilenewline: TSDUNewline;
    function getKeyIterations: Integer;
    function getLesfile: String;
    function getOffset: Uint64;
    function getPassword: String;
    function getSaltlength: Integer;
    function getVolume: String;
    function getPortable: String;
    function getFilename: String;
    function getTypeArg: String;
    function getDriverName: String;
    function getdriverControl: String;
    function getSettingsFile: String;
    function getDismount: String;
    function getSize: Uint64;
    function getIn64Arg(arg: String): Uint64;
    function getIntArg(arg: String; def: Integer): Integer;
    function getfileArg(arg: String): String;

    procedure _UpdateIsSilent;

  public
    constructor Create();
    // switches
    property isSilent: Boolean Read fis_silent Write fis_silent;  //cached so tests can overwrite
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
    // for all argumetns if parameter doenst exist argument == ''
    property portable: String Read getPortable;
    property filename: String Read getFilename;
    property driverName: String Read getDriverName;
    property driverControl: String Read getdriverControl;

    property typeArg: String Read getTypeArg;
    // this is argument to driverControl, but as its own arguments
    property setTestmode: String Read getSetTestmode;
    property dismount: String Read getDismount;
    property settingsFile: String Read getSettingsFile;

    property volume: String Read getVolume;
    property password: String Read getPassword;
    property offset: Uint64 Read getOffset;
    property size: Uint64 Read getSize;
    property saltlength: Integer Read getSaltlength;
    property keyiterations: Integer Read getKeyiterations;
    property drive: String Read getDrive;
    property keyFile: String Read getKeyfile;
    property keyFileNewline: TSDUNewline Read getKeyfileNewline;
    property lesFile: String Read getLesfile;


    // returns TRUE if command line options were processed, and "/noexit" wasn't specified as a command line
    // parameter
    // true iff run as command line only - ie should exit after commands processed
    function isCommandLineOnly: Boolean;
  end;

// returns an instance of the only object.
function GetCmdLine: TCommandLine;


 // Convert the string representation of a value into it's numerical
 // representation
 // Spaces are ignored
 // e.g.
 //      "10 GB" or "10GB"       -> 1073741824
 //      "10 bytes" or "10bytes" -> 10
 //      "10"                    -> 10
 // Note: This function can't handle values with a decimal point atm
function SDUParseUnitsAsBytesUnits(prettyValue: String): uint64;


 //some global as are only used once so easier than creating access fn
 //some are also used in creating cmd line for escalation
const
  //all in caps so compare easier

  (***************************************
       arguments to cmd line parameters
  ****************************************)
  //args to CMDLINE_PORTABLE
  CMDLINE_TOGGLE = 'TOGGLE';
  CMDLINE_START  = 'START';
  CMDLINE_ON     = 'ON';  // also TestmodeArg
  CMDLINE_STOP   = 'STOP';
  CMDLINE_OFF    = 'OFF';

  //args to  CMDLINE_FILENAME
  CMDLINE_ALL = 'ALL';  //also arg to driverName



  //args to driverControl
  CMDLINE_GUI       = 'GUI';
  CMDLINE_COUNT     = 'COUNT';
  CMDLINE_INSTALL   = 'INSTALL';
  CMDLINE_UNINSTALL = 'UNINSTALL';

  //args to driverControl /type
  CMDLINE_TOTAL           = 'TOTAL';
  CMDLINE_DRIVERSPORTABLE = 'PORTABLE';
  CMDLINE_DRIVERSINSTALLED = 'INSTALLED';

  (***************************************
       cmd line parameters
  ****************************************)
  // cmds with args
  CMDLINE_DRIVERCONTROL = 'driverControl';
  CMDLINE_PORTABLE      = 'portable';
  // Command line parameter handled in the .dpr
  CMDLINE_MINIMIZE      = 'minimize';

  // Note: Only used when creating command lines; parsing them is more flexable
  CMDLINE_SWITCH_IND = '/';
  CMDLINE_FILENAME   = 'filename';

  //switches only
  CMDLINE_NOUACESCALATE = 'noUACescalate';
  CMDLINE_SILENT        = 'silent';

implementation

uses
  lcConsts, SDUGeneral;

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
  CMDLINE_NOEXIT        = 'noExit';
  CMDLINE_CREATE        = 'create';



  // Command line parameters...
  // CMDLINE_SETTINGSFILE defined in "interface" section; used in HandleCommandLineOpts fn
  CMDLINE_MOUNT           = 'mount';
  CMDLINE_FREEOTFE        = 'LibreCrypt';
  CMDLINE_DMCRYPT         = 'dmcrypt';
  // mount/create as plain dm-crypt. no cmd for luks as type detected
  CMDLINE_VOLUME          = 'Volume';
  CMDLINE_READONLY        = 'Readonly';
  CMDLINE_OFFSET          = 'Offset';
  CMDLINE_SIZE            = 'Size';
  CMDLINE_NOCDBATOFFSET   = 'NoCDBatoffset';
  CMDLINE_PASSWORD        = 'Password';
  CMDLINE_TYPE            = 'type';
  CMDLINE_KEYFILE         = 'Keyfile';
  CMDLINE_KEYFILEISASCII  = 'KeyfileIsAscii';
  CMDLINE_KEYFILENEWLINE  = 'Keyfilenewline';
  CMDLINE_LESFILE         = 'Lesfile';
  CMDLINE_ENABLE_DEV_MENU = 'Dev_menu'; // enable dev menu
  CMDLINE_DUMP            = 'Dump';     //dump luks hdr



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



function SDUCommandLineSwitchNumber(parameter: String): Integer;
var
  i:   Integer;
  par: String;
begin
  Result    := -1;
  parameter := uppercase(parameter);
  for i := 1 to ParamCount do begin
    par := uppercase(ParamStr(i));
    if (par = ('-' + parameter)) or (par = ('/' + parameter)) then begin
      Result := i;
      break;
    end;
  end;
end;

function SDUCommandLineSwitch(parameter: String): Boolean;
begin
  Result := SDUCommandLineSwitchNumber(parameter) <> -1;
end;

//set caseSens to false to covert to uppercase
function SDUCommandLineParameter(parameter: String;
  var Value: String; caseSens: Boolean = True): Boolean; overload;
var
  i: Integer;
  //  testParam: String;
begin
  Result := False;
  i      := SDUCommandLineSwitchNumber(parameter);
  if (i > -1) and (i < ParamCount) then begin
    Value  := ParamStr(i + 1);
    Result := True;
  end;
  if not caseSens then
    Value := UpperCase(Value);


  //  parameter := uppercase(parameter);
  //  for i := 1 to (ParamCount - 1) do begin
  //    testParam := uppercase(ParamStr(i));
  //    if ((testParam = ('-' + parameter)) or (testParam = ('/' + parameter))) then begin
  //      Value  := ParamStr(i + 1);
  //      Result := True;
  //      break;
  //    end;
  //  end;

end;

function SDUCommandLineParameter(parameter: String; var Value: Integer): Boolean; overload;
var
  strValue: String;
begin
  Result := SDUCommandLineParameter(parameter, strValue);
  if Result then
    Result := TryStrToInt(strValue, Value);
end;

{ factory fn creates an instance
returns an instance of object}
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
  _UpdateIsSilent;
end;

 //function TCommandLine.GetIsPortable: Boolean;
 //var
 //  paramValue: String;
 //begin
 //result := SDUCommandLineParameter(CMDLINE_PORTABLE, paramValue);
 //end;


function TCommandLine.getDismount: String;
begin
  if not SDUCommandLineParameter(CMDLINE_DISMOUNT, Result, False) then
    Result := '';

end;


function TCommandLine.getDrive: String;
begin
  if not SDUCommandLineParameter(CMDLINE_DRIVE, Result) then
    Result := '';
end;

function TCommandLine.getdriverControl: String;
begin

  if not SDUCommandLineParameter(CMDLINE_DRIVERCONTROL, Result, False) then
    Result := '';
end;

function TCommandLine.getDriverName: String;
begin
  if not SDUCommandLineParameter(CMDLINE_DRIVERNAME, Result) then
    Result := '';
end;

function TCommandLine.getFilename: String;
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

procedure TCommandLine._UpdateIsSilent;
begin
  fis_silent := SDUCommandLineSwitch(CMDLINE_SILENT);
end;

function TCommandLine.getfileArg(arg: String): String;
begin
  if not SDUCommandLineParameter(arg, Result) then
    Result := '';

  if Result <> '' then begin

    Result := SDURelativePathToAbsolute(Result);
    if not (FileExists(Result)) then begin
      raise ECmdLine.Create('"' + Result + '" is not a valid path');
    end;
  end;
end;

function TCommandLine.getKeyfile: String;
begin
  Result := getfileArg(CMDLINE_KEYFILE);
end;

function TCommandLine.getKeyfilenewline: TSDUNewline;
var
  strTemp:        String;
  currParamOK:    Boolean;
  tmpNewlineType: TSDUNewline;
begin
  Result := LINUX_KEYFILE_DEFAULT_NEWLINE;
  if not SDUCommandLineParameter(CMDLINE_KEYFILENEWLINE, strTemp, False) then
    strTemp := '';

  // Setting for newlines where Linux keyfiles contain ASCII passwords


  if strTemp <> '' then begin
    currParamOK := False;
    for tmpNewlineType := low(tmpNewlineType) to high(tmpNewlineType) do begin
      if (strTemp = uppercase(SDUNEWLINE_TITLE[tmpNewlineType])) then begin
        Result      := tmpNewlineType;
        currParamOK := True;
        break;
      end;
    end;

    if not (currParamOK) then begin
      raise ECmdLine.Create('"' + strTemp + '" is not a valid newline name');
    end;
  end;
end;

function TCommandLine.getIntArg(arg: String; def: Integer): Integer;
var
  strTemp: String;
begin
  if not SDUCommandLineParameter(arg, strTemp) then
    Result := DEFAULT_KEY_ITERATIONS;

  if strTemp <> '' then
    if not TryStrToInt(strTemp, Result) then
      raise ECmdLine.Create('"' + strTemp + '" is not a valid number');

end;

function TCommandLine.getKeyIterations: Integer;
begin
  Result := getIntArg(CMDLINE_KEYITERATIONS, DEFAULT_KEY_ITERATIONS);
end;

function TCommandLine.getSaltlength: Integer;
begin
  Result := getIntArg(CMDLINE_SALTLENGTH, DEFAULT_SALT_LENGTH);
end;

function TCommandLine.getLesfile: String;
begin
  Result := getfileArg(CMDLINE_LESFILE);
end;

function TCommandLine.getIn64Arg(arg: String): Uint64;
var
  strTemp: String;
begin
  if not SDUCommandLineParameter(arg, strTemp) then
    strTemp := '';

  Result := 0;
  if strTemp <> '' then
    Result := SDUParseUnitsAsBytesUnits(strTemp);
end;

function TCommandLine.getOffset: Uint64;
begin

  Result := getIn64Arg(CMDLINE_OFFSET);
end;

function TCommandLine.getSize: Uint64;
begin
  Result := getIn64Arg(CMDLINE_SIZE);
end;

function TCommandLine.getPassword: String;
begin
  if not SDUCommandLineParameter(CMDLINE_PASSWORD, Result) then
    Result := '';
end;

function TCommandLine.getPortable: String;
begin
  if not SDUCommandLineParameter(CMDLINE_PORTABLE, Result, False) then
    Result := '';

end;

function TCommandLine.getSetTestmode: String;
begin
  if not SDUCommandLineParameter(CMDLINE_SET_TESTMODE, Result, False) then
    Result := '';
end;

function TCommandLine.getSettingsFile: String;
begin
  if not SDUCommandLineParameter(CMDLINE_SETTINGSFILE, Result) then
    Result := '';
end;


function TCommandLine.getTypeArg: String;
begin
  if not SDUCommandLineParameter(CMDLINE_TYPE, Result, False) then
    Result := '';
end;

function TCommandLine.getVolume: String;
begin
  if not SDUCommandLineParameter(CMDLINE_VOLUME, Result) then
    Result := '';
  //     result := UpperCase(result);
end;

{ TODO 1 -otdk -crefactor : only ever called from SDUParseUnitsAsBytesUnits - merge functions }
function SDUParseUnits(prettyValue: String;
  out Value: uint64): Boolean;
var
  i:               Integer;
  strNumber:       String;
  strUnits:        String;
  unitsMultiplier: Int64;
  foundMultiplier: Boolean;
  denominations:   TSDUArrayString;
begin
  Result        := True;
  denominations := SDUUnitsStorageToTextArr();
  Value         := 0;

  // Split on space, or detect boundry
  strNumber   := '';
  strUnits    := '';
  prettyValue := trim(prettyValue);
  if (Pos(' ', prettyValue) > 0) then begin
    SDUSplitString(prettyValue, strNumber, strUnits, ' ');
  end else begin
    for i := 1 to length(prettyValue) do begin
      if (((prettyValue[i] < '0') or (prettyValue[i] > '9')) and
        (prettyValue[i] <> '-')  // Allow -ve values
        ) then begin
        strUnits := Copy(prettyValue, i, (length(prettyValue) - i + 1));
        break;
      end;

      strNumber := strNumber + prettyValue[i];
    end;
  end;

  strNumber := trim(strNumber);
  strUnits  := trim(strUnits);

  unitsMultiplier := 1;
  if (strUnits <> '') then begin
    strUnits        := uppercase(strUnits);
    foundMultiplier := False;
    for i := low(denominations) to high(denominations) do begin
      if (strUnits = uppercase(denominations[i])) then begin
        foundMultiplier := True;
        break;
      end;

      unitsMultiplier := unitsMultiplier * UNITS_BYTES_MULTIPLIER;
    end;

    Result := Result and foundMultiplier;
  end;

  if Result then begin
    Result := SDUTryStrToInt(strNumber, Value);
  end;

  if Result then begin
    Value := Value * unitsMultiplier;
  end;

end;

// raises ECmdLine ...
function SDUParseUnitsAsBytesUnits(prettyValue: String): uint64;
var
  res: Boolean;
begin
  res := SDUParseUnits(prettyValue, Result);
  if not res then
    raise ECmdLine.Create('"' + prettyValue + '" is not a valid size');
end;



initialization
  _CommandLineObj := nil; //create by calling SetFreeOTFEType

finalization
  _CommandLineObj.Free;
end.
