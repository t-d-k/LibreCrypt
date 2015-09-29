unit frmVersionCheck;

interface

uses
  //delphi & libs
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, Graphics, Messages,
  StdCtrls, SysUtils, Variants, Windows,
  Forms,
  //sdu & LibreCrypt utils
  SDUStdCtrls, CommonSettings,
  OTFEFreeOTFE_InstructionRichEdit, SDUWinHttp,
  // LibreCrypt forms
  SDUForms;

type
  //version shown and stored
  TVersion = record
    major,
    minor: Integer
  end;
  //version from xml /exe
  TBuildVersion = record
    major,
    minor,
    revision,
    build: Integer
  end;

type
  TfrmVersionCheck = class (TSDUForm)
    Label2:           TLabel;
    Label3:           TLabel;
    lblVersionCurrent: TLabel;
    pbClose:          TButton;
    SDUURLLabel1:     TSDUURLLabel;
    ckSuppressNotifyingThisVersion: TCheckBox;
    lblVersionLatest: TLabel;
    procedure pbCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLatestVersion: TVersion;
  protected
    procedure SetAllowSuppress(allow: Boolean);
    function GetSuppressNotifyingThisVersion(): Boolean;
  public
    property AllowSuppress: Boolean Write SetAllowSuppress;
    property LatestVersion: TVersion Read FLatestVersion Write FLatestVersion;

    property SuppressNotifyingThisVersion: Boolean Read GetSuppressNotifyingThisVersion;

  end;

procedure CheckForUpdates_UserCheck();

// check for updates against website
procedure CheckForUpdates_AutoCheck();

 //function SDUGetPADFileVersionInfo(url: String;
 //var latestVersion: TVersion;
 //  userAgent: WideString = DEFAULT_HTTP_USERAGENT; ShowProgressDlg: Boolean = True): TTimeoutGet;
 //

 //
 //function SDUGetPADFileVersionInfo_XML(XML: String;
 //  var version: TBuildVersion): Boolean;



//function SDUGetPADFileVersionInfoString(url: String): String;


 //function SDUVersionCompare(AVersion: TVersion;
 //  BVersion: TVersion): Integer; overload;
 //function SDUVersionCompare(AVersion: TBuildVersion; BVersion: TBuildVersion): Integer; overload;

 //
 //function SDUVersionCompareWithBetaFlag(AVersion: TVersion;
 //  A_BetaVersion: Integer; BVersion: TVersion): Integer;
 //   overload;
 //function SDUVersionCompareWithBetaFlag(AVersion: TBuildVersion;
 //  A_BetaVersion: Integer;
 //  BVersion: TBuildVersion): Integer; overload;

// Returns the executables version numbers as set in Project|Options|Version Info
function SDUGetVersionInfo(filename: String; var version: TVersion): Boolean;
  overload;

function SDUGetVersionInfo(filename: String;
  var version: TBuildVersion): Boolean; overload;

// As SDUGetVersionInfo, but returns a nicely formatted string
function SDUGetVersionInfoString(filename: String): String;
 //function SDUVersionInfoToString(version: TVersion;
 //  betaVersion: Integer = -1): String; overload;
 //function SDUVersionInfoToString(version: TBuildVersion; betaVersion: Integer = -1): String;
 //  overload;

//function SDUGetPADFileVersionInfoString_XML(XML: String): String;

implementation

{$R *.dfm}

uses
             //delphi & libs
             system.IOUtils,
  ShellAPI,  // Required for ShellExecute
  xmldom,    // Required for IDOMDocument, etc
  XMLdoc,    // Required for TXMLDocument
             //sdu & LibreCrypt utils

  CommonConsts, lcConsts,
{$IFDEF FREEOTFE_MAIN}

  MainSettings,
{$ENDIF}
{$IFDEF FREEOTFE_EXPLORER}

  ExplorerSettings,
{$ENDIF}
  lcDialogs, SDUGeneral,
  SDUi18n
  // LibreCrypt forms
  ;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

resourcestring
  RS_CHECKING          = '<checking...>';
  RS_CONFIRM_AUTOCHECK =
    'Do you want to automatically check for updates in the future?';
  RS_UNABLE_TO_DETERMINE_THIS_VERSION = 'Unable to determine which version this software is.';

// Set filename to '' to get version info on the currently running executable
function SDUGetVersionInfo(filename: String;
  var version: TBuildVersion): Boolean;
var
  vsize:     Integer;
  puLen:     Cardinal;
  dwHandle:  DWORD;
  pBlock:    Pointer;
  pVPointer: Pointer;
  tvs:       PVSFixedFileInfo;
  //  iLastError: Cardinal;
begin
  Result := False;

  if filename = '' then
    filename := Application.ExeName;

  vsize := GetFileVersionInfoSize(PChar(filename), dwHandle);

  if vsize = 0 then
    exit;

  GetMem(pBlock, vsize);
  try
    if GetFileVersionInfo(PChar(filename), dwHandle, vsize, pBlock) then begin

      VerQueryValue(pBlock, '\', pVPointer, puLen);
      if puLen > 0 then begin
        //      iLastError := GetLastError;
        //      showmessage(Format('GetFileVersionInfo failed: (%d) %s',                           [iLastError, SysErrorMessage(iLastError)]));
        tvs              := PVSFixedFileInfo(pVPointer);
        version.major    := tvs^.dwFileVersionMS shr 16;
        version.minor    := tvs^.dwFileVersionMS and $ffff;
        version.revision := tvs^.dwFileVersionLS shr 16;
        version.build    := tvs^.dwFileVersionLS and $ffff;
        Result           := True;
      end;
    end;
  finally
    FreeMem(pBlock);
  end;

end;


function SDUGetPADFileVersionInfo_XML(XML: String;
  out version: TBuildVersion): Boolean;
const
  XML_NODE_PROGRAM_NAME    = '/XML_DIZ_INFO/Program_Info/Program_Name';
  XML_NODE_PROGRAM_VERSION = '/XML_DIZ_INFO/Program_Info/Program_Version';
var
  doc: TXMLDocument;

  iXml:          IDOMDocument;
  iNode:         IDOMNode;
  DOMNodeSelect: IDOMNodeSelect;
  iNodeEx:       IDOMNodeEx;

  //  padAppID: string;

  i:          Integer;
  versionStr: String;
  stlVersion: TStringList;
begin
  doc := TXMLDocument.Create(nil);
  try
    doc.XML.Add(XML);
    doc.Active := True;

    iXml := doc.DOMDocument;
    iXml.QueryInterface(IDOMNodeSelect, DOMNodeSelect);

{
    iNode := DOMNodeSelect.selectNode(XML_NODE_PROGRAM_NAME);
    iNodeEx := GetDOMNodeEx(iNode);
    padAppID := iNodeEx.text;
}

    iNode      := DOMNodeSelect.selectNode(XML_NODE_PROGRAM_VERSION);
    iNodeEx    := GetDOMNodeEx(iNode);
    versionStr := iNodeEx.Text;

    stlVersion := TStringList.Create();
    try
      stlVersion.Delimiter     := '.';
      stlVersion.DelimitedText := versionStr;

      Result := (stlVersion.Count > 0);

      version.major    := 0;
      version.minor    := 0;
      version.revision := 0;
      version.build    := 0;
      for i := 0 to (stlVersion.Count - 1) do begin
        stlVersion[i] := trim(stlVersion[i]);

        case i of
          0: Result := TryStrToInt(stlVersion[i], version.major);
          1: Result := TryStrToInt(stlVersion[i], version.minor);
          2: Result := TryStrToInt(stlVersion[i], version.revision);
          3: Result := TryStrToInt(stlVersion[i], version.build);
        end;

        if not (Result) then begin
          break;
        end;

      end;

    finally
      stlVersion.Free();
    end;

  finally
    doc.Free();
  end;

end;

function SDUVersionInfoToString(version: TVersion;
  betaVersion: Integer = -1): String; overload;
begin
  Result := Format('%d.%d', [version.major, version.minor]);

  if (betaVersion > 0) then
    Result := Result + ' ' + RS_BETA + ' ' + IntToStr(betaVersion);
end;

function SDUVersionInfoToString(version: TBuildVersion; betaVersion: Integer = -1): String;
  overload;
begin
  // versions are normally shown as eg 6.1 so format similarly or can confuse ie not 6.01
  Result := Format('%d.%d.%.2d.%.4d', [version.major, version.minor, version.revision,
    version.build]);

  if (betaVersion > 0) then
    Result := Result + ' ' + RS_BETA + ' ' + IntToStr(betaVersion);
end;


function SDUGetPADFileVersionInfoString_XML(XML: String): String;
var
  version: TBuildVersion;
begin
  Result := '';
  if SDUGetPADFileVersionInfo_XML(XML, version) then
    Result := SDUVersionInfoToString(version, -1);
end;

 // Get version ID string for the specified executable
 // filename - The name of the executable to extract the version ID from.
 //            Leave blank to get version ID from current executable
function SDUGetVersionInfoString(filename: String): String;
var
  version: TBuildVersion;
  //  minorVersion:    Integer;//
  //  revisionVersion: Integer;
  //  buildVersion:    Integer;
begin
  Result := '';
  if SDUGetVersionInfo(filename, version) then begin
    Result := SDUVersionInfoToString(version, -1);
  end;

end;



function SDUGetVersionInfo(filename: String; var version: TVersion): Boolean;
var
  junk: TBuildVersion;
begin
  junk.major    := version.major;
  junk.minor    := version.minor;
  Result        := SDUGetVersionInfo(filename, junk);
  version.major := junk.major;
  version.minor := junk.minor;

end;


function _SDUVersionNumberCompare(A, B: Integer): Integer;
begin
  Result := 0;
  if (A > B) then
    Result := -1;
  if (B > A) then
    Result := 1;
end;

 // Check version IDs
 // Returns:
 //   -1 if A is later
 //   0 if they are the same
 //   1 if B is later
function SDUVersionCompare(AVersion: TVersion;
  BVersion: TVersion): Integer; overload;
begin
  Result := _SDUVersionNumberCompare(AVersion.major, BVersion.major);
  if (Result = 0) then
    Result := _SDUVersionNumberCompare(AVersion.minor, BVersion.minor);
end;

 // Check version IDs
 // If A > B, return -1
 // If A = B, return  0
 // If A < B, return  1
function SDUVersionCompare(AVersion: TBuildVersion; BVersion: TBuildVersion): Integer; overload;
begin
  Result := _SDUVersionNumberCompare(AVersion.major, BVersion.major);

  if (Result = 0) then
    Result := _SDUVersionNumberCompare(AVersion.minor, BVersion.minor);

  if (Result = 0) then
    Result := _SDUVersionNumberCompare(AVersion.revision, BVersion.revision);

  if (Result = 0) then
    Result := _SDUVersionNumberCompare(AVersion.build, BVersion.build);

end;


function SDUVersionCompareWithBetaFlag(AVersion: TVersion;BVersion: TVersion): Integer;
begin
  Result := SDUVersionCompare(AVersion, BVersion);
  if (Result = 0) then begin
    if (APP_BETA_BUILD > 0) then begin
      Result := 1;
    end;
  end;

end;

 //function SDUVersionCompareWithBetaFlag(AVersion: TBuildVersion;
 //  A_BetaVersion: Integer;
 //  BVersion: TBuildVersion): Integer;
 //begin
 //  Result := SDUVersionCompare(AVersion, BVersion);
 //  if (Result = 0) then begin
 //    if (A_BetaVersion > 0) then begin
 //      Result := 1;
 //    end;
 //  end;
 //
 //end;


 //function SDUGetPADFileVersionInfo(url: String; var latestVersion: TVersion;
 //  userAgent: WideString = DEFAULT_HTTP_USERAGENT; ShowProgressDlg: Boolean = True): TTimeoutGet;
 //var
 //  temp: TBuildVersion;
 //begin
 //  temp.major          := latestVersion.major; // is this nec, or is always just getitng?
 //  temp.minor          := latestVersion.minor;
 //  Result              := SDUGetPADFileVersionInfo(url, temp, userAgent, ShowProgressDlg);
 //  latestVersion.major := temp.major;
 //  latestVersion.minor := temp.minor;
 //
 //end;

 // As SDUGetVersionInfo, but gets information from the PAD file at the
 // specified URL, or the XML passed in directly
function SDUGetPADFileVersionInfo(url: String;
  var latestVersion: TVersion;
  userAgent: WideString = DEFAULT_HTTP_USERAGENT; ShowProgressDlg: Boolean = True): TTimeoutGet;
var
  xml:  String;
  temp: TBuildVersion;
begin
{$IFDEF FORCE_LOCAL_PAD}
     xml     := TFile.ReadAllText(url);
      Result := tgOK;
   {$ELSE}
  Result := tgFailure;
  if ShowProgressDlg then begin
    Result := SDUGetURLProgress_WithUserAgent(_('Checking for latest version...'),
      url, xml, userAgent);
  end else begin
    if SDUWinHTTPRequest_WithUserAgent(url, userAgent, xml) then begin
      Result := tgOK;
    end;
  end;

   {$ENDIF}
  if (Result = tgOK) then begin
    // out param - not nec. to initialise
    if not (SDUGetPADFileVersionInfo_XML(xml, temp)) then begin
      Result := tgFailure;
    end;
    latestVersion.major := temp.major;
    latestVersion.minor := temp.minor;
  end;

end;


 // As SDUGetVersionInfoString, but gets information from the PAD file at the
 // specified URL, or the XML passed in directly
function SDUGetPADFileVersionInfoString(url: String): String;
var
  xml: String;
begin
  Result := '';
  if SDUWinHTTPRequest(url, xml) then
    Result := SDUGetPADFileVersionInfoString_XML(xml);
end;



 // Manual check algorithm:
 //
 // if user forced check:
 // hit WWW site for latest version
 //  - if user cancel, exit
 //  - if can't connect, tell user then exit
 //  - if got data:
 //     - report version in use and latest version
procedure CheckForUpdates_UserCheck();
var
  dlg:           TfrmVersionCheck;
  wwwResult:     TTimeoutGet;
  latestVersion: TVersion;
  //  latestMinorVersion: Integer;
begin
  if not (SDUWinHTTPSupported) then begin
    // Just open homepage; user can check manually...
    ShellExecute(
      0,
      PChar('open'),
      PChar(URL_HOMEPAGE),
      PChar(''),
      PChar(''),
      SW_SHOW
      );
    exit;
  end;

  wwwResult := SDUGetPADFileVersionInfo(URL_PADFILE, latestVersion, Application.Title +
    '/' + SDUGetVersionInfoString(''), True);
  if (wwwResult = tgCancel) then begin
    // Do nothing; just fall out of procedure
  end else
  if (wwwResult <> tgOK) then begin
    SDUMessageDlg(
      Format(_('Unable to determine latest release of %s.'), [Application.Title]),
      mtError
      );
  end else begin
    dlg := TfrmVersionCheck.Create(nil);
    try
      dlg.latestVersion := latestVersion;
      dlg.AllowSuppress := False;
      dlg.ShowModal();
    finally
      dlg.Free();
    end;
  end;

end;

 // Automatic check algorithm:
 //
 // if time to check again:
 // hit WWW site for latest version
 //  - if user cancel, prompt if want to check again in future
 //     - if don't, update to never check again
 //  - if can't connect, prompt if want to check again in future
 //     - if don't, update to never check again
 //  - if got data:
 //     - if no newer version available
 //        - update date last checked
 //     - if newer version available, display dialog
 //        - if user doens't want to be informed of this version again
 //           - update date last checked
 //        - if user still want to be informed
 //           - (DO NOT update date last checked)
procedure CheckForUpdates_AutoCheck( );
var
  dlg:           TfrmVersionCheck;
  wwwResult:     TTimeoutGet;
  latestVersion: TVersion;
  currVersion:   TVersion;
  compareResult: Integer;
//  Frequency: TUpdateFrequency;
  checked_now : Boolean;
  DontNotifyVer: TVersion ;
begin

   checked_now   := false;

  if not (SDUWinHTTPSupported) then begin
    // No changes to var parameters - just exit
    exit;
  end;

  //prompt to connect to net
  { DONE 1 -otdk -cenhance : remember result }
  if not (SDUConfirmYN(_(
    'A check for an update is due, which requires a connection to the internet. Continue?'))) then
    wwwResult := tgCancel
  else
    wwwResult := SDUGetPADFileVersionInfo(URL_PADFILE, latestVersion, Application.Title +
      '/' + SDUGetVersionInfoString(''), True);

  if (wwwResult = tgCancel) then begin
    // if can't save settings then no point in asking whether to check again (will automatically)
    if GSettingsSaveLocation = slNone then begin
      SDUMessageDlg(_('Canceled checking for updated version'), mtInformation);
    end else begin
      if not (SDUConfirmYN(_('Canceled checking for updated version') +
        SDUCRLF + SDUCRLF + RS_CONFIRM_AUTOCHECK)) then
          GetSettings().UpdateChkFreq  := ufNever;

    end;

  end else
  if (wwwResult <> tgOK) then begin
    if not (SDUErrorYN(Format(_('Unable to determine latest release of %s.'),
      [Application.Title]) + SDUCRLF + SDUCRLF + RS_CONFIRM_AUTOCHECK)) then begin
          GetSettings().UpdateChkFreq  := ufNever;
    end;
  end else begin
    // If user doesn't want to be informed of this version...
    DontNotifyVer.major := GetSettings().UpdateChkDontNotifyMajorVer;
    DontNotifyVer.minor := GetSettings().UpdateChkDontNotifyMinorVer;
    if (SDUVersionCompare(latestVersion, DontNotifyVer) >= 0) then begin
      // Do nothing; just update last checked
      checked_now   := true;
    end else begin
      if not SDUGetVersionInfo('', currVersion) then begin
        SDUMessageDlg(RS_UNABLE_TO_DETERMINE_THIS_VERSION, mtError);
      end;

      compareResult := SDUVersionCompareWithBetaFlag(currVersion, latestVersion);

      if (compareResult <= 0) then begin
        // This software is the latest version, or a later version
        checked_now   := true;
      end else begin
        // This software is an old version
        dlg := TfrmVersionCheck.Create(nil);
        try
          dlg.LatestVersion := latestVersion;
          dlg.AllowSuppress := True;
          dlg.ShowModal();

          if dlg.SuppressNotifyingThisVersion then begin
             GetSettings().UpdateChkDontNotifyMajorVer := latestVersion.major;
              GetSettings().UpdateChkDontNotifyMinorVer:= latestVersion.minor;
            checked_now   := true;
          end;

        finally
          dlg.Free();
        end;
      end;

    end;
  end;
if checked_now then  GetSettings().lastCheckedForUpdate := now;
end;

procedure TfrmVersionCheck.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TfrmVersionCheck.FormCreate(Sender: TObject);
begin
  lblVersionCurrent.Caption := RS_CHECKING;
  lblVersionLatest.Caption  := RS_CHECKING;

  SDUURLLabel1.URL     := URL_DOWNLOAD;
  SDUURLLabel1.Visible := False;

end;

procedure TfrmVersionCheck.SetAllowSuppress(allow: Boolean);
begin
  ckSuppressNotifyingThisVersion.Visible := allow;
  ckSuppressNotifyingThisVersion.Checked := False;
end;

function TfrmVersionCheck.GetSuppressNotifyingThisVersion(): Boolean;
begin
  Result := ckSuppressNotifyingThisVersion.Checked;
end;

procedure TfrmVersionCheck.FormShow(Sender: TObject);
var
  compareResult: Integer;
  currVersion:   TVersion;
begin
  lblVersionLatest.Caption := 'v' + SDUVersionInfoToString(FLatestVersion);

  if not (SDUGetVersionInfo('', currVersion)) then begin
    lblVersionCurrent.Caption := RS_UNKNOWN;
    SDUMessageDlg(RS_UNABLE_TO_DETERMINE_THIS_VERSION, mtError);
  end else begin
    compareResult := SDUVersionCompareWithBetaFlag(currVersion, FLatestVersion);

    lblVersionCurrent.Caption := 'v' + SDUVersionInfoToString(currVersion, APP_BETA_BUILD);

    if (compareResult > 0) then
      SDUURLLabel1.Visible := True;

  end;

end;

end.
