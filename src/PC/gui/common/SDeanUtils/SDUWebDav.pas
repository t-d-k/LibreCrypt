// my stub as not in sdeanutils
unit SDUWebDav;

interface

uses
  Windows, Classes,
  //3rd party
  IdContext,
  IdCustomHTTPServer,
  IdWebDav,
  IdSocketHandle,
  //sdu
  SDFilesystem,SDFilesystem_FAT;

type

     LogRec = record
    FileName:string;
  end;




  TSDUWebDAV = class (TOBject)
  PROTECTED
    FileSystem: TSDCustomFilesystem;

    function GetPropFindXMLSingle_FSGetItem(fileOrDir: String; var item: TSDDirItem): Boolean;
      VIRTUAL; ABSTRACT;
    // Overridden inherited...
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); VIRTUAL; ABSTRACT;
    procedure DoCommandDELETE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); VIRTUAL; ABSTRACT;
    procedure DoCommandMOVE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); VIRTUAL; ABSTRACT;
    procedure DoCommandPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); VIRTUAL; ABSTRACT;
    procedure DoCommandPROPFIND(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); VIRTUAL; ABSTRACT;
    procedure DoCommandPROPPATCH(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); VIRTUAL; ABSTRACT;
    function HTTPReqDocToLocalFile(Document: String): String;
    function GetPropFindXMLSingle(host: String; fileOrDir: String; item: TSDDirItem): String;
    procedure AddDebugLog(cmnt: String);

  PUBLIC
    Bindings:  TIdSocketHandles;
    ShareName: String;
    fServerSoftware : String;
      fLogAccess:LogRec;
  fLogDebug:LogRec;
  fDefaultPort :integer;
      fFilesystem:     TSDFilesystem_FAT;
      fOnlyPermitLocal :boolean;
    function IsActive: Boolean;
    procedure Activate;
     procedure DeActivate;
    constructor Create(AOwner: TComponent); VIRTUAL;
    destructor Destroy();override;




  end;

function   SDURandomHexString(digits:integer):string;

procedure SDUPause(pause : integer);

const DEFAULT_HTTP_PORT      = 80;

implementation

function   SDURandomHexString(digits:integer):string;
begin
  result := '1234'; { TODO -otdk -ccomplete : webdav }
end;
procedure SDUPause(pause : integer);
begin

end;

{ TSDUWebDAV }



procedure TSDUWebDAV.Activate;
begin

end;



procedure TSDUWebDAV.AddDebugLog(cmnt: String);
begin

end;

constructor TSDUWebDAV.Create(AOwner: TComponent);
begin
Bindings:=  TIdSocketHandles.Create(nil);
end;

procedure TSDUWebDAV.DeActivate;
begin

end;

destructor TSDUWebDAV.Destroy;
begin
  Bindings.Free;
  inherited;
end;



function TSDUWebDAV.GetPropFindXMLSingle(host, fileOrDir: String;
  item: TSDDirItem): String;
begin

end;

function TSDUWebDAV.HTTPReqDocToLocalFile(Document: String): String;
begin

end;

function TSDUWebDAV.IsActive: Boolean;
begin
  result:= false;
end;

end.
