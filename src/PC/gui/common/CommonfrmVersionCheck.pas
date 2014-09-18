unit CommonfrmVersionCheck;

interface

uses
  Classes, ComCtrls, CommonSettings, Controls, Dialogs, ExtCtrls,
  Forms,
  Graphics, Messages, OTFEFreeOTFE_InstructionRichEdit,
  SDUForms, SDUStdCtrls, StdCtrls, SysUtils, Variants, Windows;

type
  TfrmVersionCheck = class (TSDUForm)
    Label2:                         TLabel;
    Label3:                         TLabel;
    lblVersionCurrent:              TLabel;
    pbClose:                        TButton;
    SDUURLLabel1:                   TSDUURLLabel;
    ckSuppressNotifyingThisVersion: TCheckBox;
    lblVersionLatest:               TLabel;
    procedure pbCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    FLatestMajorVersion, FLatestMinorVersion: Integer;
  PROTECTED
    procedure SetAllowSuppress(allow: Boolean);
    function GetSuppressNotifyingThisVersion(): Boolean;
  PUBLIC
    property AllowSuppress: Boolean Write SetAllowSuppress;
    property LatestMajorVersion: Integer Read FLatestMajorVersion Write FLatestMajorVersion;
    property LatestMinorVersion: Integer Read FLatestMinorVersion Write FLatestMinorVersion;

    property SuppressNotifyingThisVersion: Boolean Read GetSuppressNotifyingThisVersion;

  end;

procedure CheckForUpdates_UserCheck(PADURL: String);
procedure CheckForUpdates_AutoCheck(PADURL: String;
  var Frequency: TUpdateFrequency;
  var LastChecked: TDate;
  var SuppressNotifyVerMajor: Integer;
  var SuppressNotifyVerMinor:
  Integer);

implementation

{$R *.dfm}

uses
  ShellAPI,  // Required for ShellExecute
  CommonConsts,
{$IFDEF FREEOTFE_MAIN}
  FreeOTFEConsts,
  FreeOTFESettings,
{$ENDIF}
{$IFDEF FREEOTFE_EXPLORER}
  FreeOTFEExplorerConsts,
  FreeOTFEExplorerSettings,
{$ENDIF}
  SDUGeneral,
  SDUi18n,
  SDUWinHttp,
  SDUDialogs;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

resourcestring
  RS_CHECKING                         = '<checking...>';
  RS_CONFIRM_AUTOCHECK                = 'Do you want to automatically check for updates in the future?';
  RS_UNABLE_TO_DETERMINE_THIS_VERSION = 'Unable to determine which version this software is.';


 // Manual check algorithm:
 //
 // if user forced check:
 // hit WWW site for latest version
 //  - if user cancel, exit
 //  - if can't connect, tell user then exit
 //  - if got data:
 //     - report version in use and latest version
procedure CheckForUpdates_UserCheck(PADURL: String);
var
  dlg:                TfrmVersionCheck;
  wwwResult:          TTimeoutGet;
  latestMajorVersion: Integer;
  latestMinorVersion: Integer;
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

  wwwResult := SDUGetPADFileVersionInfo(PADURL,
    latestMajorVersion,
    latestMinorVersion,
    Application.Title + '/' + SDUGetVersionInfoString(''),
    True);
  if (wwwResult = tgCancel) then begin
    // Do nothing; just fall out of procedure
  end else
  if (wwwResult <> tgOK) then begin
    SDUMessageDlg(
      SDUParamSubstitute(
      _('Unable to determine latest release of %1.'),
      [Application.Title]),
      mtError
      );
  end else begin
    dlg := TfrmVersionCheck.Create(nil);
    try
      dlg.LatestMajorVersion := latestMajorVersion;
      dlg.LatestMinorVersion := latestMinorVersion;
      dlg.AllowSuppress      := False;
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
procedure CheckForUpdates_AutoCheck(PADURL: String;
  var Frequency: TUpdateFrequency;
  var LastChecked: TDate;
  var SuppressNotifyVerMajor: Integer;
  var SuppressNotifyVerMinor:
  Integer);
var
  dlg:                TfrmVersionCheck;
  wwwResult:          TTimeoutGet;
  latestMajorVersion: Integer;
  latestMinorVersion: Integer;
  currMajorVersion:   Integer;
  currMinorVersion:   Integer;
  compareResult:      Integer;
begin
  if not (SDUWinHTTPSupported) then begin
    // No changes to var parameters - just exit
    exit;
  end;

  //prompt to connect to net
  { TODO 1 -otdk -cenhance : remember result }
  if not (SDUConfirmYN(_(
    'A check for an update is due, which requires a connection to the internet. Continue?')
    )) then
    wwwResult := tgCancel

  else

    wwwResult := SDUGetPADFileVersionInfo(PADURL,
      latestMajorVersion,
      latestMinorVersion,
      Application.Title + '/' + SDUGetVersionInfoString(''),
      True);
  if (wwwResult = tgCancel) then begin
    if not (SDUConfirmYN(_('Canceled checking for updated version') +
      SDUCRLF + SDUCRLF + RS_CONFIRM_AUTOCHECK
      )) then begin
      Frequency := ufNever;
    end;
  end else
  if (wwwResult <> tgOK) then begin
    if not (SDUErrorYN(SDUParamSubstitute(
      _('Unable to determine latest release of %1.'),
      [
      Application.Title]) + SDUCRLF +
      SDUCRLF + RS_CONFIRM_AUTOCHECK
      )) then begin
      Frequency := ufNever;
    end;
  end else begin
    // If user doesn't want to be informed of this version...
    if (SDUVersionCompare(latestMajorVersion,
      latestMinorVersion, SuppressNotifyVerMajor,
      SuppressNotifyVerMinor) >= 0) then begin
      // Do nothing; just update last checked
      LastChecked := now;
    end else begin
      if not (SDUGetVersionInfo('',
        currMajorVersion,
        currMinorVersion)) then begin
        SDUMessageDlg(RS_UNABLE_TO_DETERMINE_THIS_VERSION, mtError);
      end;

      compareResult := SDUVersionCompareWithBetaFlag(
        currMajorVersion, currMinorVersion,
        APP_BETA_BUILD, latestMajorVersion,
        latestMinorVersion);

      if (compareResult = 0) then begin
        // This software is the latest version
        LastChecked := now;
      end else
      if (compareResult < 0) then begin
        // This software is the prerelease version
        LastChecked := now;
      end else begin
        // This software is an old version

        dlg := TfrmVersionCheck.Create(nil);
        try
          dlg.LatestMajorVersion := latestMajorVersion;
          dlg.LatestMinorVersion := latestMinorVersion;
          dlg.AllowSuppress      := True;
          dlg.ShowModal();

          if dlg.SuppressNotifyingThisVersion then begin
            SuppressNotifyVerMajor := latestMajorVersion;
            SuppressNotifyVerMinor := latestMinorVersion;
            LastChecked            := now;
          end;

        finally
          dlg.Free();
        end;
      end;

    end;
  end;

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
  compareResult:    Integer;
  currMajorVersion: Integer;
  currMinorVersion: Integer;
begin
  lblVersionLatest.Caption := 'v' + SDUVersionInfoToString(FLatestMajorVersion,
    FLatestMinorVersion);

  if not (SDUGetVersionInfo('',
    currMajorVersion,
    currMinorVersion)) then begin
    lblVersionCurrent.Caption := RS_UNKNOWN;
    SDUMessageDlg(RS_UNABLE_TO_DETERMINE_THIS_VERSION, mtError);
  end else begin
    compareResult := SDUVersionCompareWithBetaFlag(
      currMajorVersion, currMinorVersion, APP_BETA_BUILD,
      FLatestMajorVersion,
      FLatestMinorVersion);

    lblVersionCurrent.Caption := 'v' + SDUVersionInfoToString(currMajorVersion,
      currMinorVersion, APP_BETA_BUILD);

    if (compareResult > 0) then begin
      SDUURLLabel1.Visible := True;
    end;

  end;

end;

end.
