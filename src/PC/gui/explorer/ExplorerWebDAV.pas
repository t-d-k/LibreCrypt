unit ExplorerWebDAV;

interface

uses
  Classes,
  IdContext,
  IdCustomHTTPServer,
  SDFilesystem, SDUGeneral,
  SDUWebDAV,
  Windows;

type
  TRequestedItem = record
    Filename: Ansistring;
    IsFile:   Boolean;
    Size:     ULONGLONG;
  end;
  PRequestedItem = ^TRequestedItem;


  TExplorerWebDAV = class (TSDUWebDAV)
  PROTECTED
    FReturnGarbage:  Boolean;
    FRequestedItems: TStringList;

    procedure SetReturnGarbage(newValue: Boolean);

    procedure Overwrite(pItem: PRequestedItem);

    procedure AddRequestedItemToList(const filename: String; const isFile: Boolean;
      itemSize: ULONGLONG);
    function GetRequestedItemFromList(const filename: String): PRequestedItem;

    function GetPropFindXMLSingle_FSGetItem(fileOrDir: String; var item: TSDDirItem): Boolean;
      OVERRIDE;
    function GetPropFindXMLSingle_FSGetItem_Dummy(fileOrDir: String;
      var item: TSDDirItem): Boolean;
    function GetPropFindXMLSingle_Dummy(host: String; fileOrDir: String): String;

    // Overridden inherited...
    procedure DoCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); OVERRIDE;
    procedure DoCommandDELETE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); OVERRIDE;
    procedure DoCommandMOVE(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); OVERRIDE;
    procedure DoCommandPUT(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); OVERRIDE;
    procedure DoCommandPROPFIND(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); OVERRIDE;
    procedure DoCommandPROPPATCH(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); OVERRIDE;

  PUBLIC
    constructor Create(AOwner: TComponent); OVERRIDE;
    destructor Destroy(); OVERRIDE;

    property ReturnGarbage: Boolean Read FReturnGarbage Write SetReturnGarbage;
    property RequestedItems: TStringList Read FRequestedItems;

    procedure ClearDownRequestedItemsList();
  end;

implementation

uses
  SDUClasses, SDUHTTPServer,
  Shredder,
  SysUtils;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


const
  SECTOR_SIZE = 512;

constructor TExplorerWebDAV.Create(AOwner: TComponent);
begin
  inherited;
  ReturnGarbage := False;

  FRequestedItems        := TStringList.Create();
  FRequestedItems.Sorted := True;
end;

destructor TExplorerWebDAV.Destroy();
begin
  ClearDownRequestedItemsList();

  FRequestedItems.Free();

  inherited;
end;

function TExplorerWebDAV.GetRequestedItemFromList(const filename: String): PRequestedItem;
var
  idx:         Integer;
  useFilename: String;
begin
  Result := nil;

  useFilename := filename;
  if not (FileSystem.CaseSensitive) then begin
    useFilename := uppercase(useFilename);
  end;

  idx := FRequestedItems.IndexOf(useFilename);
  if (idx >= 0) then begin
    Result := PRequestedItem(FRequestedItems.Objects[idx]);
  end;


end;

procedure TExplorerWebDAV.AddRequestedItemToList(const filename: String;
  const isFile: Boolean; itemSize: ULONGLONG);
var
  pItem: PRequestedItem;
begin
  pItem := GetRequestedItemFromList(filename);

  if (pItem = nil) then begin
    new(pItem);

    // Note: This IS case sensitive - and should be as WebDAV is
    FRequestedItems.AddObject(filename, TObject(pItem));
  end;

  pItem.Filename := filename;
  pItem.IsFile   := isFile;
  pItem.Size     := itemSize;

end;

procedure TExplorerWebDAV.ClearDownRequestedItemsList();
var
  pItem: PRequestedItem;
  i:     Integer;
begin
  // Destroy all associated objects
  for i := 0 to (FRequestedItems.Count - 1) do begin
    pItem := PRequestedItem(FRequestedItems.Objects[i]);
    Overwrite(pItem);
    Dispose(pItem);
  end;

  Shredder.Overwrite(FRequestedItems);

  FRequestedItems.Clear();

end;

procedure TExplorerWebDAV.SetReturnGarbage(newValue: Boolean);
begin
  if newValue then begin
    if (Filesystem <> nil) then begin
      Filesystem.ReadOnly := True;
    end;
  end;

  FReturnGarbage := newValue;
end;

procedure TExplorerWebDAV.Overwrite(pItem: PRequestedItem);
begin
  Shredder.Overwrite(pItem.Filename);
  pItem.IsFile := False;
  pItem.Size   := 0;
end;

procedure TExplorerWebDAV.DoCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  garbageContent: TSDUMemoryStream;
  useSize:        ULONGLONG;
  pItem:          PRequestedItem;
  localFilename:  String;
  //  ptrMem: Pointer;
  ptrByte:        PByte;
begin
  localFilename := HTTPReqDocToLocalFile(ARequestInfo.Document);

  if (not (ReturnGarbage) or (ARequestInfo.Command <> HTTPCMD_GET)) then begin
    inherited;

    if (AResponseInfo.ResponseNo = HTTPSTATUS_OK) then begin
      if (AResponseInfo.ContentStream <> nil) then begin
        AddRequestedItemToList(
          localFilename,
          FileSystem.FileExists(localFilename),
          AResponseInfo.ContentStream.Size
          );
      end;
    end;
  end else begin
    useSize := 0;
    pItem   := GetRequestedItemFromList(localFilename);
    if (pItem <> nil) then begin
      useSize := pItem.Size;
    end;

    // Round up to nearest SECTOR_SIZE byte boundry
    // Note: This sets useSize to SECTOR_SIZE at the minimum - not zero; that's
    //       OK though, and probably better than setting it to 0
    useSize := useSize + (ULONGLONG(SECTOR_SIZE) - ULONGLONG((useSize mod SECTOR_SIZE)));

    garbageContent := TSDUMemoryStream.Create();
    garbageContent.SetSize(useSize);
    if (useSize > 0) then begin
      garbageContent.Position := 0;
      ptrByte                 := garbageContent.memory;
      FillChar(ptrByte^, useSize, 0);
    end;

    garbageContent.Position         := 0;
    AResponseInfo.ContentStream     := garbageContent;
    // The response object frees off "fileContent"
    AResponseInfo.FreeContentStream := True;
    AResponseInfo.ResponseNo        := HTTPSTATUS_OK;
  end;

end;


function TExplorerWebDAV.GetPropFindXMLSingle_Dummy(host: String;
  fileOrDir: String): String;
const
  LOCALE_ENGLISH_UNITED_STATES = 1033;
var
  item:   TSDDirItem;
begin
  Result := '';

  item := TSDDirItem.Create();
  try
    GetPropFindXMLSingle_FSGetItem_Dummy(fileOrDir, item);
    Result := GetPropFindXMLSingle(host, fileOrDir, item);

  finally
    item.Free();
  end;


end;

procedure TExplorerWebDAV.DoCommandPROPFIND(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  localFilename: String;
  outputXML:     String;
begin
  if ReturnGarbage then begin
    // Generate generic "OK" reply
    AResponseInfo.ResponseNo := HTTPSTATUS_OK;
    localFilename            := HTTPReqDocToLocalFile(ARequestInfo.Document);

    // lplp - implement functionality so that can take a list of properties interested in

    AddDebugLog('+++localFilename@@:' + localFilename);
    outputXML := GetPropFindXMLSingle_Dummy(ARequestInfo.Host, localFilename);

    outputXML := '<?xml version="1.0"?>' + SDUCRLF +
      //x             '<D:multistatus xmlns:D="DAV:">'+SDUCRLF+
      // urn:uuid:c2f41010-65b3-11d1-a29f-00aa00c14882 detailed at: http://xml.coverpages.org/xmlData980105.html and inclued in IIS responses
      '<D:multistatus xmlns:D="DAV:" xmlns:b="urn:uuid:c2f41010-65b3-11d1-a29f-00aa00c14882/">'
      +
      SDUCRLF + outputXML + SDUCRLF + '</D:multistatus>';

    AResponseInfo.ResponseNo   := HTTPSTATUS_MULTI_STATUS;
    AResponseInfo.ResponseText := HTTPSTATUS_TEXT_MULTI_STATUS;
    AResponseInfo.ContentType  := 'text/xml';
    //      outputXML := CompressXML(outputXML);
    AResponseInfo.ContentText  := outputXML;
    AddDebugLog('XML:');
    AddDebugLog(outputXML);
  end else begin
    inherited;
  end;

end;

procedure TExplorerWebDAV.DoCommandPROPPATCH(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if ReturnGarbage then begin
    // Just ignore request, and say it was handled
    AResponseInfo.ResponseNo := HTTPSTATUS_OK;
  end else begin
    inherited;
  end;

end;

procedure TExplorerWebDAV.DoCommandPUT(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  localFilename: String;
  useSize:       ULONGLONG;
begin
  if ReturnGarbage then begin
    // Just ignore request, and say it was stored
    AResponseInfo.ResponseNo := HTTPSTATUS_OK;
  end else begin
    inherited;

    if (AResponseInfo.ResponseNo = HTTPSTATUS_OK) then begin
      localFilename := HTTPReqDocToLocalFile(ARequestInfo.Document);

      // Sanity check - if poststream is nil, create zero length stream and use
      // that instead
      useSize := 0;
      if (ARequestInfo.PostStream <> nil) then begin
        useSize := ARequestInfo.PostStream.Size;
      end;

      AddRequestedItemToList(
        localFilename,
        True,
        useSize
        );
    end;
  end;

end;

procedure TExplorerWebDAV.DoCommandDELETE(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if ReturnGarbage then begin
    // Just ignore request, and say it was deleted
    AResponseInfo.ResponseNo := HTTPSTATUS_OK;
  end else begin
    inherited;
  end;

end;

procedure TExplorerWebDAV.DoCommandMOVE(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if ReturnGarbage then begin
    // Just ignore request, and say it was deleted
    AResponseInfo.ResponseNo := HTTPSTATUS_OK;
  end else begin
    inherited;
  end;

end;

function TExplorerWebDAV.GetPropFindXMLSingle_FSGetItem(fileOrDir: String;
  var item: TSDDirItem): Boolean;
begin
  Result := inherited GetPropFindXMLSingle_FSGetItem(fileOrDir, item);

  if ReturnGarbage and Result then begin
    item.TimestampLastModified := DateTimeToTimeStamp(now());
  end;


end;

function TExplorerWebDAV.GetPropFindXMLSingle_FSGetItem_Dummy(fileOrDir: String;
  var item: TSDDirItem): Boolean;
begin
  item.Filename              := ExtractFilename(fileOrDir);
  item.Size                  := SECTOR_SIZE;
  item.IsFile                := True;
  item.IsDirectory           := False;
  item.TimestampLastModified := DateTimeToTimeStamp(now());
  //    item.CreationDate := DateTimeToTimeStamp(now());
  item.IsHidden              := False;

  Result := True;
end;

end.
