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



TSDUWebDAV = class(TIdWebDav)
protected
FileSystem : TSDCustomFilesystem;

    function GetPropFindXMLSingle_FSGetItem(fileOrDir: string; var item: TSDDirItem): boolean; virtual;abstract;
        // Overridden inherited...
    procedure DoCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual;abstract;
    procedure DoCommandDELETE(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual;abstract;
    procedure DoCommandMOVE(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual;abstract;
    procedure DoCommandPUT(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual;abstract;
    procedure DoCommandPROPFIND(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual;abstract;
    procedure DoCommandPROPPATCH(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo); virtual;abstract;
    function  HTTPReqDocToLocalFile(Document: string):string;
    function  GetPropFindXMLSingle(host: string; fileOrDir: string ;item: TSDDirItem):string;
   procedure AddDebugLog(cmnt: string);

  public
     Bindings: TIdSocketHandles;
     ShareName :string;

    constructor Create(AOwner: TComponent); virtual;abstract;
    destructor  Destroy(); virtual;abstract;
   function Active:boolean;



end;


implementation

{ TSDUWebDAV }

function TSDUWebDAV.Active: boolean;
begin

end;

procedure TSDUWebDAV.AddDebugLog(cmnt: string);
begin

end;

function TSDUWebDAV.GetPropFindXMLSingle(host, fileOrDir: string;
  item: TSDDirItem): string;
begin

end;

function TSDUWebDAV.HTTPReqDocToLocalFile(Document: string):string;
begin

end;




end.
