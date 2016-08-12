unit SDUSysUtils;

{ file and string manipulation }

interface

uses
  SysUtils, Windows, Forms,
  //sdu
  lcTypes {for DriveLetterString};

 // Identical to FileAge(filename, datetime) in Delphi 2007
 // This function included to allow same API call to be used in Delphi 7
function SDUFileAge(filename: String; var dateTime: TDateTime): Boolean;

 // Identical to FileAge(filename, datetime) in Delphi 2007
 // This function included to allow same API call to be used in Delphi 7
function SDUFileTimestamps(
  const Filename: String;
  var CreationTime: TFileTime;
  var LastAccessTime: TFileTime;
  var LastWriteTime: TFileTime): Boolean;

// Copy of WideStrUtils.WStrLen for compatibility with Delphi 7
function SDUWStrLen(const Str: PWideChar): Cardinal;
// These are purely SDU functions
function SDUWStrLCopy(Dest: PWChar; const Source: PWChar; MaxLen: Cardinal): PWChar;
function SDUWStrPCopy(Dest: PWChar; const Source: WideString): PWChar;
function SDUWStrPLCopy(Dest: PWChar; const Source: WideString;
  MaxLen: Cardinal): PWChar;

// External function in shell32.dll
function SHFormatDrive(Handle: HWND; Drive, ID, Options: Word): Longint;
  stdcall; external 'shell32.dll' Name 'SHFormatDrive';

function UACRun(filenameAndPath, cmdLineParams: String;
  suppressMsgs: Boolean; allowUACEscalation: Boolean; out errMsg: String): Boolean;

function Format_drive(driveToFormat: DriveLetterChar; silent: Boolean = False): Boolean;

resourcestring
  FORMAT_ERR = 'The drive could not be formatted.'#10#13#10#13 +
    'Please lock this container and re-open it with the "Open for all users" option, and without the "read-only" option and try again.';

implementation

uses
  System.UITypes, ShellApi,
{$IF CompilerVersion >= 18.5}
  WideStrUtils,
{$IFEND}
  lcDialogs,

  //sdu,lcutils
  sdugeneral;

const
  // for shformat
  // Format disk related functions ripped from the Unofficial Delphi FAQ (UDF)
  SHFMT_ID_DEFAULT      = $FFFF;
  // Formating options
  SHFMT_OPT_QUICKFORMAT = $0000;
  SHFMT_OPT_FULL        = $0001;
  SHFMT_OPT_SYSONLY     = $0002;
  // Error codes
  SHFMT_ERROR           = $FFFFFFFF;
  SHFMT_CANCEL          = $FFFFFFFE;
  SHFMT_NOFORMAT        = $FFFFFFFD;


resourcestring
  TEXT_NEED_ADMIN = 'You need administrator privileges in order to carry out this operation.';

function SDUFileAge(filename: String; var dateTime: TDateTime): Boolean;
{$IF CompilerVersion >= 18.5}
begin
  Result := FileAge(filename, dateTime);
end;

{$ELSE}
var
  age: integer;
begin
  result := FALSE;

  age := FileAge(filename);
  if (age <> -1) then    begin
    dateTime := FileDateToDateTime(age);
    result := TRUE;
    end;


end;
{$IFEND}


function SDUFileTimestamps(
  const Filename: String;
  var CreationTime: TFileTime;
  var LastAccessTime: TFileTime;
  var LastWriteTime: TFileTime): Boolean;
var
  searchRec: TSearchRec;
begin
  Result := False;
{$WARN SYMBOL_PLATFORM OFF}
  if (SysUtils.FindFirst(filename, faAnyFile, searchRec) = 0) then begin
    CreationTime   := searchRec.FindData.ftCreationTime;
    LastAccessTime := searchRec.FindData.ftLastAccessTime;
    LastWriteTime  := searchRec.FindData.ftLastWriteTime;
    SysUtils.FindClose(searchRec);
    Result := True;
  end;
{$WARN SYMBOL_PLATFORM ON}

end;

// ----------------------------------------------------------------------------
{$IF CompilerVersion >= 18.5}
function SDUWStrLen(const Str: PWideChar): Cardinal;
begin
  Result := WStrLen(Str);
end;

{$ELSE}
function SDUWStrLen(const Str: PWideChar): Cardinal;
var
  P : PWideChar;
begin
  P := Str;
  while (P^ <> #0) do Inc(P);
  Result := (P - Str);
end;
{$IFEND}

// ----------------------------------------------------------------------------
function SDUWStrLCopy(Dest: PWChar; const Source: PWChar; MaxLen: Cardinal): PWChar;
var
  i: Integer;
begin
  for i := 1 to MaxLen do
    Dest[i] := Source[i];

  Dest[MaxLen + 1] := WChar($0000);

  Result := Dest;
end;

function SDUWStrPCopy(Dest: PWChar; const Source: WideString): PWChar;
begin
  Result := SDUWStrLCopy(Dest, PWChar(Source), Length(Source));
end;

function SDUWStrPLCopy(Dest: PWChar; const Source: WideString;
  MaxLen: Cardinal): PWChar;
begin
  Result := SDUWStrLCopy(Dest, PWChar(Source), MaxLen);
end;

function UACRun(filenameAndPath, cmdLineParams: String;
  suppressMsgs: Boolean; allowUACEscalation: Boolean; out errMsg: String): Boolean;
const
  VERB_RUNAS = 'runas';
var
  cwd:        String;
  //  retVal: HINST;
  ShExecInfo: SHELLEXECUTEINFO;
  lpExitCode: DWORD;
begin
  // Only applicable in Windows Vista; simply warn user they can't continue on
  // earlier OSs
  errMsg := '';
  if (not (SDUOSVistaOrLater()) or not allowUACEscalation) then begin
    Result := False;
    if not suppressMsgs then
      errMsg := TEXT_NEED_ADMIN;
  end else begin
    cwd := ExtractFilePath(filenameAndPath);


    ShExecInfo.cbSize       := sizeof(SHELLEXECUTEINFO);
    ShExecInfo.fMask        := SEE_MASK_NOCLOSEPROCESS;
    ShExecInfo.Wnd          := Application.Handle;
    ShExecInfo.lpVerb       := VERB_RUNAS;
    ShExecInfo.lpFile       := PChar(filenameAndPath);
    ShExecInfo.lpParameters := PChar(cmdLineParams);
    ShExecInfo.lpDirectory  := PChar(cwd);
    ShExecInfo.nShow        := SW_SHOW;
    ShExecInfo.hInstApp     := 0;
    ShellExecuteEx(@ShExecInfo);
    WaitForSingleObject(ShExecInfo.hProcess, INFINITE);

    if GetExitCodeProcess(ShExecInfo.hProcess, lpExitCode) then begin

      Result := lpExitCode = 0;
    end else begin
      //   Result := retVal > 32;
      Result := False;
    end;

    //    retVal := ShellExecute(Application.Handle, VERB_RUNAS,
    //      PChar(filenameAndPath), PChar(cmdLineParams), PChar(cwd), SW_SHOW);



    if not Result then
      errMsg := Format('Command %s(%s) failed with error: %d %s',
        [filenameAndPath, cmdLineParams, lpExitCode, SysErrorMessage(GetLastError)]);
  end;
end;


function Format_drive(driveToFormat: DriveLetterChar; silent: Boolean = False): Boolean;
  //resourcestring
  //  FORMAT_CAPTION = 'Format';
var
  //  i:         Integer;
  //  currDrive: DriveLetterChar;
  //  driveNum:  Word;
  //  formatRet: Boolean;
  //cont :boolean;
  cmd, errMsg, newlinePath: String;
begin
  Result      := True;
  //    currDrive   := drivesToFormat[i];
  //    driveNum  := Ord(currDrive) - Ord('A');
  {DONE: use cmd line - see below}
  //http://stackoverflow.com/questions/2648305/format-drive-by-c
  // >format g: /fs:fat /q /v:librecrypt - as admin, pipe in newlines
  // get path to 'tools' dir
  newlinePath := ExtractFilePath(ParamStr(0));
    {$IFDEF DEBUG}
      // debug\win32
      newlinePath := newlinePath+'..\..\';
    {$ENDIF}
  newlinePath := newlinePath + 'tools\newline.txt';  // contains newlines
  newlinePath := SDUGetFinalPath(newlinePath);
  // resolve any mapped drives, as will run as admin
  cmd         := format('/c format %s: /fs:fat /q /v:LibreCrypt < %s > c:\format_output.txt',
    [driveToFormat, newlinePath]);
  //    formatRet   := False;
  if not silent then
    Result := SDUConfirmOK(format('Format drive %s?', [driveToFormat]));
  if Result then begin
    application.ProcessMessages;
    Result := UACRun('cmd', cmd, False, True, errMsg); //causes hang?
    //      Result := UACRun('','cmd '+ cmd, False, True, errMsg); //causes hang?

    //        Result := SHFormatDrive(Application.handle, Ord(Uppercase(driveToFormat)) - Ord('A'), SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);

    application.ProcessMessages;
    // check formatted
    if not DirectoryExists(driveToFormat + ':\') then
      Result := False;

    if (not Result) and (not silent) then begin
      SDUMessageDlg(errMsg, TMsgDlgType.mtError);
    end;
    //      break;
    //    end else begin
    {  dialog only returns when done - use cmd line as above
      // close format dlg
      // see also http://stackoverflow.com/questions/15469657/why-is-findwindow-not-100-reliable
      hndle := FindWindow('IEFrame', NIL);
      if hndle>0 then begin
      PostMessage(hndle, WM_CLOSE, 0, 0);
      end;
      }
  end;
  //  end;

  // This is *BIZARRE*.
  // If you are running under Windows Vista, and you mount a volume for
  // yourself only, and then try to format it, the format will fail - even if
  // you UAC escalate to admin
  // That's one thing - HOWEVER! After it fails, for form's title is set to:
  //   Format <current volume label> <drive letter>:
  // FREAKY!
  // OTOH, it does means we can detect and inform the user that the format
  // just failed...
  //  if (Pos(uppercase(FORMAT_CAPTION), uppercase(frm.Caption)) > 0) or not Result then begin
  //    frm.Caption := Application.Title;
  //    Result      := False;
  //  end;

end;

end.
