unit SDUWinHttp;

interface

uses
  Windows, Classes,
  SDUWinHttp_API;

const
  DEFAULT_HTTP_USERAGENT       = 'SDeanComponents/1.0';
  DEFAULT_HTTP_REQUESTVERSION  = '1.1';
  DEFAULT_HTTP_METHOD          = 'GET';

type
  SDUWinHTTPTimeouts = record
    Name: integer;
    Connect: integer;
    Send: integer;
    Receive: integer;
  end;

 TTimeoutGet = (tgOK, tgCancel, tgTimeout, tgFailure);

const
  DEFAULT_HTTP_TIMEOUTS: SDUWinHTTPTimeouts = (
                                               Name: NAME_RESOLUTION_TIMEOUT;
                                               Connect: WINHTTP_DEFAULT_TIMEOUT_CONNECT;
                                               Send: WINHTTP_DEFAULT_TIMEOUT_SEND;
                                               Receive: WINHTTP_DEFAULT_TIMEOUT_RECEIVE
                                              );

function SDUWinHTTPSupported(): boolean;

function SDUWinHTTPRequest(
  URL: WideString;
  out ReturnedBody: string
): boolean; overload;

function SDUWinHTTPRequest_WithUserAgent(
  URL: WideString;
  UserAgent: WideString;
  out ReturnedBody: string
): boolean;

function SDUWinHTTPRequest(
  URL: WideString;

  Method: WideString;
  DataToSendPresent: boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: string;
  ReturnedHeaders: TStrings  // May be set to nil
): boolean; overload;

function SDUWinHTTPRequest(
  ServerName: WideString;
  ServerPort: integer;
  ServerPath: WideString;

  Method: WideString;
  DataToSendPresent: boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: string;
  ReturnedHeaders: TStrings  // May be set to nil
): boolean; overload;

function SDUWinHTTPRequest(
  URL: WideString;

  Method: WideString;
  DataToSendPresent: boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  Timeouts: SDUWinHTTPTimeouts;

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: string;
  ReturnedHeaders: TStrings  // May be set to nil
): boolean; overload;

function SDUWinHTTPRequest(
  ServerName: WideString;
  ServerPort: integer;
  ServerPath: WideString;

  Method: WideString;
  DataToSendPresent: boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  Timeouts: SDUWinHTTPTimeouts;

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: string;
  ReturnedHeaders: TStrings  // May be set to nil
): boolean; overload;

function SDUGetURLProgress(
  DialogTitle: string;
  URL: WideString;
  out ReturnedBody: string
): TTimeoutGet;

function SDUGetURLProgress_WithUserAgent(
  DialogTitle: string;
  URL: WideString;
  out ReturnedBody: string;
  UserAgent: WideString
): TTimeoutGet;


implementation

uses
  Forms, Controls,
  SDUGeneral,
  SDUProgressDlg;

type
  TThreadHTTPGet = class(tthread)
  public
    URL: WideString;
    UserAgent: WideString;

    ModalForm: TForm;

    RetrievedOK: boolean;
    ReturnedBody: String;
    
    procedure Execute(); override;
    procedure snc();
  end;


function SDUWinHTTPSupported(): boolean;
begin
{$IFDEF WINHTTP_DLL_STATIC}
  // Application would crash on startup if it wasn't supported...
  Result := TRUE;
{$ELSE}
  Result := FALSE;
  try
    Result := WinHttpCheckPlatform();
  except
    // Just swallow exception - Result already set to FALSE
  end;
{$ENDIF}


end;

function SDUWinHTTPRequest(
  URL: WideString;
  out ReturnedBody: string
): boolean;
begin
  Result := SDUWinHTTPRequest_WithUserAgent(
                                            URL,
                                            DEFAULT_HTTP_USERAGENT,
                                            ReturnedBody
                                           );
end;

function SDUWinHTTPRequest_WithUserAgent(
  URL: WideString;
  UserAgent: WideString;
  out ReturnedBody: string
): boolean;
var
  httpStatusCode: DWORD;
begin
  Result := FALSE;
  if SDUWinHTTPRequest(
                  URL,

                  DEFAULT_HTTP_METHOD,
                  FALSE,
                  '',

                  DEFAULT_HTTP_USERAGENT,
                  DEFAULT_HTTP_REQUESTVERSION,
                  nil,

                  httpStatusCode,
                  ReturnedBody,
                  nil
                 ) then
    begin
    Result := (httpStatusCode = HTTP_STATUS_OK);
    end;


end;

function SDUWinHTTPRequest(
  URL: WideString;

  Method: WideString;
  DataToSendPresent: boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: string;
  ReturnedHeaders: TStrings  // May be set to nil
): boolean;
begin
  Result := SDUWinHTTPRequest(
                              URL,

                              Method,
                              DataToSendPresent,
                              DataToSend,

                              UserAgent,
                              RequestVersion,
                              AdditionalReqHeaders,

                              DEFAULT_HTTP_TIMEOUTS,

                              ReturnedStatusCode,
                              ReturnedBody,
                              ReturnedHeaders
                            );

end;

function SDUWinHTTPRequest(
  URL: WideString;

  Method: WideString;
  DataToSendPresent: boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  Timeouts: SDUWinHTTPTimeouts;

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: string;
  ReturnedHeaders: TStrings  // May be set to nil
): boolean;
var

  splitURL: URL_COMPONENTS;

  serverName: WideString;
  serverPort: integer;
  serverPath: WideString;
begin
  Result := FALSE;

  ZeroMemory(@splitURL, sizeof(splitURL));
  splitURL.dwStructSize := sizeof(splitURL);

  splitURL.dwSchemeLength    := $FFFFFFFF;
  splitURL.dwHostNameLength  := $FFFFFFFF;
  splitURL.dwUrlPathLength   := $FFFFFFFF;
  splitURL.dwExtraInfoLength := $FFFFFFFF;

  if WinHttpCrackUrl(
                     PWideString(URL),
                     length(URL),
                     0,
                     @splitURL
                    ) then
    begin
    serverName := Copy(splitURL.lpszHostName, 1, splitURL.dwHostNameLength);
    serverPort := splitURL.nPort;
    serverPath := Copy(splitURL.lpszUrlPath, 1, splitURL.dwUrlPathLength);

    Result := SDUWinHTTPRequest(
                          serverName,
                          serverPort,
                          serverPath,

                          Method,
                          DataToSendPresent,
                          DataToSend,

                          UserAgent,
                          RequestVersion,
                          AdditionalReqHeaders,

                          Timeouts,

                          ReturnedStatusCode,
                          ReturnedBody,
                          ReturnedHeaders
                         );
    end;


end;

function SDUWinHTTPRequest(
  ServerName: WideString;
  ServerPort: integer;
  ServerPath: WideString;

  Method: WideString;
  DataToSendPresent: boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: string;
  ReturnedHeaders: TStrings  // May be set to nil
): boolean;
begin
  Result := SDUWinHTTPRequest(
                              ServerName,
                              ServerPort,
                              ServerPath,

                              Method,
                              DataToSendPresent,
                              DataToSend,

                              UserAgent,
                              RequestVersion,
                              AdditionalReqHeaders,

                              DEFAULT_HTTP_TIMEOUTS,

                              ReturnedStatusCode,
                              ReturnedBody,
                              ReturnedHeaders
                            );

end;

function SDUWinHTTPRequest(
  ServerName: WideString;
  ServerPort: integer;
  ServerPath: WideString;

  Method: WideString;
  DataToSendPresent: boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  Timeouts: SDUWinHTTPTimeouts;

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: string;
  ReturnedHeaders: TStrings  // May be set to nil
): boolean;
var
  hSession: HINTERNET;
  hConnect: HINTERNET;
  hRequest: HINTERNET;

  allOK: boolean;
  i: integer;

  idx: DWORD;
  wstrTmp: WideString;

  byteReceived: DWORD;
  page: Ansistring;
  strHeaders: WideString;
begin
  // Bail out early if it's just *not* going to work...
  if not(WinHttpCheckPlatform()) then
    begin
    Result := FALSE;
    exit;
    end;

  ReturnedStatusCode := 0;

  // Call the following:
  //   WinHttpOpen
  //   WinHttpSetTimeouts
  //   WinHttpConnect
  //   WinHttpOpenRequest
  //   WinHttpAddRequestHeaders
  //   WinHttpWriteData
  //   WinHttpSendRequest
  //   WinHttpReceiveResponse
  //   WinHttpQueryHeaders
  //   WinHttpQueryDataAvailable
  //   WinHttpReadData
  //   WinHttpCloseHandle

  Result := FALSE;
  ReturnedBody := '';

  hSession := WinHttpOpen(
                          PWideString(UserAgent),
                          WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
                          WINHTTP_NO_PROXY_NAME,
                          WINHTTP_NO_PROXY_BYPASS,
                          0
                         );
  if (hSession <> 0) then
    begin
    WinHttpSetTimeouts(
                       hSession,
                       Timeouts.Name,
                       Timeouts.Connect,
                       Timeouts.Send,
                       Timeouts.Receive
                      );

    hConnect := WinHttpConnect(
                               hSession,
                               PWideString(ServerName),
                               ServerPort,
                               0
                              );
    if (hConnect <> 0) then
      begin
      { TODO 1 -otdk -cenhance : set flags according to whether https or http }
      hRequest := WinHttpOpenRequest(
                                     hConnect,
                                     // "nil" as second parameter causes it to use "GET"
                                     PWideString(Method),
                                     PWideString(ServerPath),
                                     nil,
                                     WINHTTP_NO_REFERER,
                                     WINHTTP_DEFAULT_ACCEPT_TYPES,
                                     WINHTTP_FLAG_REFRESH  or WINHTTP_FLAG_SECURE  //  use flag WINHTTP_FLAG_SECURE for https
                                    );
      if (hRequest <> 0) then
        begin
        allOK := TRUE;
        if (AdditionalReqHeaders <> nil) then
          begin
          if (AdditionalReqHeaders.count > 0) then
            begin
            strHeaders := '';
            for i:=0 to (AdditionalReqHeaders.count - 1) do
              begin
              if (strHeaders <> '') then
                begin
                strHeaders := strHeaders + wchar(#13) + wchar(#10);
                end;

              strHeaders := strHeaders + AdditionalReqHeaders[i];
              end;

            allOK := WinHttpAddRequestHeaders(
                                              hRequest,
                                              PWideString(strHeaders),
                                              length(strHeaders),
                                              WINHTTP_ADDREQ_FLAG_REPLACE
                                             );
            end;
          end;

        if allOK then
          begin
          if DataToSendPresent then
            begin
            allOK := WinHttpWriteData(
                               hRequest,
                               PAnsiChar(DataToSend),
                               length(DataToSend),
                               @byteReceived
                              );
            end;
          end;

        if allOK then
          begin
          if WinHttpSendRequest(
                                hRequest,
                                WINHTTP_NO_ADDITIONAL_HEADERS,
                                0,
                                WINHTTP_NO_REQUEST_DATA,
                                0,
                                0,
                                nil
                               ) then
            begin
            if WinHttpReceiveResponse(hRequest, nil) then
              begin
              byteReceived := sizeof(ReturnedStatusCode);
              WinHttpQueryHeaders(
                                  hRequest,
                                  (WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER),
                                  nil,
                                  @ReturnedStatusCode,
                                  @byteReceived,
                                  nil
                                 );

              if (returnedHeaders <> nil) then
                begin
                idx := 0;
                byteReceived := 0;
                WinHttpQueryHeaders(
                                    hRequest,
                                    WINHTTP_QUERY_RAW_HEADERS_CRLF,
                                    WINHTTP_HEADER_NAME_BY_INDEX,
                                    WINHTTP_NO_OUTPUT_BUFFER,
                                    @byteReceived,
                                    @idx
                                   );

                if (byteReceived > 0) then
                  begin
                  wstrTmp := SDUWideStringOfWideChar('X', byteReceived);
                  if WinHttpQueryHeaders(
                                    hRequest,
                                    WINHTTP_QUERY_RAW_HEADERS_CRLF,
                                    WINHTTP_HEADER_NAME_BY_INDEX,
                                    PWideString(wstrTmp),
                                    @byteReceived,
                                    @idx
                                   ) then
                    begin
                    returnedHeaders.Text := wstrTmp;
                    end;
                  end;
                end;


              repeat
                byteReceived := 0;
                if WinHttpQueryDataAvailable(hRequest, @byteReceived) then
                  begin
                  // Call StringOfChar(...) to allocate buffer to be used
                  page := StringOfChar(AnsiChar('X'), byteReceived);
                  if WinHttpReadData(
                                     hRequest,
                                     PAnsiChar(page),
                                     byteReceived,
                                     @byteReceived
                                    ) then
                    begin
                    ReturnedBody := ReturnedBody + page;
                    Result := TRUE;
                    end;

                  end;
              until (byteReceived <= 0);

              end;
            end;
          end;

        WinHttpCloseHandle(hRequest);
        end;

      WinHttpCloseHandle(hConnect);
      end;

    WinHttpCloseHandle(hSession);
    end;


end;


function SDUGetURLProgress(
  DialogTitle: string;
  URL: WideString;
  out ReturnedBody: string
): TTimeoutGet;
begin
  Result := SDUGetURLProgress_WithUserAgent(
                                            DialogTitle,
                                            URL,
                                            ReturnedBody,
                                            DEFAULT_HTTP_USERAGENT
                                           );
end;

function SDUGetURLProgress_WithUserAgent(
  DialogTitle: string;
  URL: WideString;
  out ReturnedBody: string;
  UserAgent: WideString
): TTimeoutGet;
var
  ProgressDlg: TSDUProgressDialog;
  progResult: word;
  thrHTTPGet: TThreadHTTPGet;
begin
  Result := tgFailure;
ProgressDlg:= TSDUProgressDialog.Create(nil);
  try



  ProgressDlg.Title := DialogTitle;
  ProgressDlg.Min := 0;
  ProgressDlg.Max := 100;
  ProgressDlg.Position := 0;
  ProgressDlg.Indeterminate := TRUE;
  ProgressDlg.IndeterminateRunning := TRUE;
  ProgressDlg.CancelSetsModalResult := TRUE;

  thrHTTPGet := TThreadHTTPGet.Create(TRUE);
  thrHTTPGet.ModalForm := ProgressDlg;
  thrHTTPGet.URL := URL;
  thrHTTPGet.UserAgent := UserAgent;
  thrHTTPGet.Resume;

  try
    try
    progResult := ProgressDlg.ShowModal();
    finally
      thrHTTPGet.ModalForm := nil;
    end;
  except
    if thrHTTPGet.RetrievedOK then      begin
      progResult := mrOK;
      end    else      begin
      progResult := mrAbort;
      end;
  end;

  ProgressDlg.IndeterminateRunning := FALSE;
  finally
    ProgressDlg.Free;
  end;

  if (progResult = mrCancel) then        begin
    Result := tgCancel;
    end  else if (progResult = mrAbort) then    begin
    Result := tgTimeout;
    end  else if (progResult = mrOK) then    begin
    // In this case, the thread terminated
    if thrHTTPGet.RetrievedOK then       begin
      Result := tgOK;
      ReturnedBody := thrHTTPGet.ReturnedBody;
      end    else      begin
      Result := tgFailure;
      end;
    end;
   thrHTTPGet.Free(); // blocks till terminates  - but if no internet?
 // if thrHTTPGet.Terminated then      begin
//   thrHTTPGet.Free();
//    end    else    begin
//    // We no longer care if it returns or not...
//    thrHTTPGet.FreeOnTerminate := TRUE;
//    // Yes, this is right - don't terminate only sets a flag, it doesn't
//    // terminate the running thread
//    thrHTTPGet.Terminate();
//    end;


end;


procedure TThreadHTTPGet.Execute();
begin
  RetrievedOK := FALSE;
  
  RetrievedOK := SDUWinHTTPRequest_WithUserAgent(
                                                 URL,
                                                 UserAgent,
                                                 ReturnedBody
                                                );

  Synchronize(snc);
end;

procedure TThreadHTTPGet.snc();
begin
  if not(Terminated) then
    begin
    if (ModalForm <> nil) then
      begin
      if (ModalForm.modalresult = mrNone) then
        begin
        ModalForm.modalresult := mrok;
        end;
      end;
    end;

end;


END.

