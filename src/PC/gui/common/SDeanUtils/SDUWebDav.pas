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
  SDFilesystem;

type



  TSDUWebDAV = class (TIdWebDav)
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

    constructor Create(AOwner: TComponent); VIRTUAL; ABSTRACT;
    destructor Destroy(); VIRTUAL; ABSTRACT;
    function Active: Boolean;



  end;


implementation

{ TSDUWebDAV }

function TSDUWebDAV.Active: Boolean;
begin

end;

procedure TSDUWebDAV.AddDebugLog(cmnt: String);
begin

end;

function TSDUWebDAV.GetPropFindXMLSingle(host, fileOrDir: String; item: TSDDirItem): String;
begin

end;

function TSDUWebDAV.HTTPReqDocToLocalFile(Document: String): String;
begin

end;



end.
