unit FreeOTFEExplorerfrmWebDAVStatus;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SDUForms,
  FreeOTFEExplorerWebDAV,
  OTFE_U,
  //sdu
  sdugeneral;

type
  TfrmWebDAVStatus = class(TSDUForm)
    pbClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    edShareName: TEdit;
    edMappedDrive: TEdit;
    pbBrowse: TButton;
    pbExplore: TButton;
    Label3: TLabel;
    edServiceWebClient: TEdit;
    Label4: TLabel;
    edServiceMRxDAV: TEdit;
    edIPAddress: TEdit;
    Label5: TLabel;
    edWebDAVServerState: TEdit;
    Label6: TLabel;
    pbStartWebClient: TButton;
    pbStopWebClient: TButton;
    pbStopMRxDAV: TButton;
    pbStartMRxDAV: TButton;
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure pbExploreClick(Sender: TObject);
  protected
    function GetServiceStatus(serviceName: string): string;
    procedure EnableDisableControls();
  public
    WebDAVObj: TFreeOTFEExplorerWebDAV;
    MappedDrive: DriveLetterChar;
  end;

implementation

{$R *.dfm}

uses
  ShellAPI,  // For ShellExecute(...)
  IdSocketHandle,
  SDUi18n,
//   SDUWinSvc,
  SDUDialogs,

  FreeOTFEExplorerConsts;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

procedure TfrmWebDAVStatus.FormShow(Sender: TObject);
var
  boundSktHandle: TIdSocketHandle;
begin
  // Controls not yet implemented...
  pbStartWebClient.visible := FALSE;
  pbStopWebClient.visible := FALSE;
  pbStopMRxDAV.visible := FALSE;
  pbStartMRxDAV.visible := FALSE;

  // Controls are all readonly...
  SDUReadonlyControl(edWebDAVServerState, TRUE);
  SDUReadonlyControl(edIPAddress, TRUE);
  SDUReadonlyControl(edShareName, TRUE);
  SDUReadonlyControl(edMappedDrive, TRUE);
  SDUReadonlyControl(edWebDAVServerState, TRUE);
  SDUReadonlyControl(edIPAddress, TRUE);
  SDUReadonlyControl(edShareName, TRUE);
  SDUReadonlyControl(edMappedDrive, TRUE);
  SDUReadonlyControl(edServiceWebClient, TRUE);
  SDUReadonlyControl(edServiceMRxDAV, TRUE);

  // Populate display...
  edWebDAVServerState.text := _('Not running');
  edIPAddress.text := 'n/a';
  edShareName.text := 'n/a';
  edMappedDrive.text:= _('Not mapped');

  if WebDAVObj.Active then
    begin
    edWebDAVServerState.text := _('Running');


    if (WebDAVObj.Bindings.count > 0) then
      begin
      boundSktHandle:= WebDAVObj.Bindings[0];
      edIPAddress.text := boundSktHandle.IP
//      boundSktHandle.Port := WebDAVObj.DefaultPort;
      end;

    edShareName.text:= WebDAVObj.ShareName;

    if (MappedDrive <> #0) then
      begin
      edMappedDrive.text:= MappedDrive+':\';
      end;

    end;

  edServiceWebClient.text:= GetServiceStatus(SERVICE_WEBCLIENT);
  edServiceMRxDAV.text:= GetServiceStatus(SERVICE_MRXDAV);

  EnableDisableControls();

end;

procedure TfrmWebDAVStatus.EnableDisableControls();
begin
  SDUEnableControl(pbBrowse, WebDAVObj.Active);
  SDUEnableControl(pbExplore, (MappedDrive <> #0));
end;

procedure TfrmWebDAVStatus.pbBrowseClick(Sender: TObject);
var
  URL: string;
begin
  URL := 'http://'+WebDAVObj.Bindings[0].IP+'/'+WebDAVObj.ShareName+'/';
  ShellExecute(
               self.Handle,
               PChar('open'),
               PChar(URL),
               PChar(''),
               PChar(''),
               SW_SHOW
              );
end;

procedure TfrmWebDAVStatus.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TfrmWebDAVStatus.pbExploreClick(Sender: TObject);
var
  explorerCommandLine: string;
begin
  explorerCommandLine := 'explorer '+MappedDrive+':\';

  if not(SDUWinExecNoWait32(explorerCommandLine, SW_RESTORE)) then
    begin
    SDUMessageDlg(_('Error running Explorer'), mtError);
    end;

end;

function TfrmWebDAVStatus.GetServiceStatus(serviceName: string): string;
var
  sc: TSDUServiceControl;
  serviceState: cardinal;
  retval: string;
begin
  retval := _('Unable to get service state');

  try
    sc:= TSDUServiceControl.Create();
  except
    // Problem getting service control manager - ignore the error; retval
    // already set to FALSE
    sc := nil;
  end;

  if (sc <> nil) then
  begin
    try
      // We'll process all errors...
      sc.Silent := TRUE;

      if sc.GetServiceState(serviceName, serviceState) then
        begin
        retval := ServiceStateTitle(serviceState);
        end;

    finally
      sc.free;
    end;

  end;

  Result := retval;

end;

END.

