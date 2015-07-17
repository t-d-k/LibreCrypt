unit SDUWinHttp;

interface

uses
  Classes,
  SDUWinHttp_API, Windows;

const
  DEFAULT_HTTP_USERAGENT      = 'SDeanComponents/1.0';
  DEFAULT_HTTP_REQUESTVERSION = '1.1';
  DEFAULT_HTTP_METHOD         = 'GET';

type
  SDUWinHTTPTimeouts = record
    Name:    Integer;
    Connect: Integer;
    Send:    Integer;
    Receive: Integer;
  end;

  TTimeoutGet = (tgOK, tgCancel, tgTimeout, tgFailure);

const
  DEFAULT_HTTP_TIMEOUTS: SDUWinHTTPTimeouts = (
    Name: NAME_RESOLUTION_TIMEOUT;
    Connect: WINHTTP_DEFAULT_TIMEOUT_CONNECT;
    Send: WINHTTP_DEFAULT_TIMEOUT_SEND;
    Receive: WINHTTP_DEFAULT_TIMEOUT_RECEIVE
    );

function SDUWinHTTPSupported(): Boolean;

function SDUWinHTTPRequest(
  URL: WideString;
  out ReturnedBody: String): Boolean; overload;

function SDUWinHTTPRequest_WithUserAgent(
  URL: WideString;
  UserAgent: WideString;
  out ReturnedBody: String): Boolean;

function SDUWinHTTPRequest(
  URL: WideString;

  Method: WideString;
  DataToSendPresent: Boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: String;
  ReturnedHeaders: TStrings  // May be set to nil
  ): Boolean; overload;

function SDUWinHTTPRequest(
  ServerName: WideString;
  ServerPort: Integer;
  ServerPath: WideString;

  Method: WideString;
  DataToSendPresent: Boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: String;
  ReturnedHeaders: TStrings  // May be set to nil
  ): Boolean; overload;

function SDUWinHTTPRequest(
  URL: WideString;

  Method: WideString;
  DataToSendPresent: Boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  Timeouts: SDUWinHTTPTimeouts;

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: String;
  ReturnedHeaders: TStrings  // May be set to nil
  ): Boolean; overload;

function SDUWinHTTPRequest(
  ServerName: WideString;
  ServerPort: Integer;
  ServerPath: WideString;

  Method: WideString;
  DataToSendPresent: Boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  Timeouts: SDUWinHTTPTimeouts;

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: String;
  ReturnedHeaders: TStrings  // May be set to nil
  ): Boolean; overload;

function SDUGetURLProgress(
  DialogTitle: String;
  URL: WideString;
  out ReturnedBody: String): TTimeoutGet;

function SDUGetURLProgress_WithUserAgent(
  DialogTitle: String;
  URL: WideString;
  out ReturnedBody: String;
  UserAgent: WideString): TTimeoutGet;


implementation

uses
  Controls,
  Forms, SDUGeneral,
  dlgProgress;

type
  TThreadHTTPGet = class (tthread)
  public
    URL:       WideString;
    UserAgent: WideString;

    ModalForm: TForm;

    RetrievedOK:  Boolean;
    ReturnedBody: String;

    procedure Execute(); override;
    procedure snc();
  end;


function SDUWinHTTPSupported(): Boolean;
begin
{$IFDEF WINHTTP_DLL_STATIC}
  // Application would crash on startup if it wasn't supported...
  Result := TRUE;
{$ELSE}
  Result := False;
  try
    Result := WinHttpCheckPlatform();
  except
    // Just swallow exception - Result already set to FALSE
  end;
{$ENDIF}

end;

function SDUWinHTTPRequest(
  URL: WideString;
  out ReturnedBody: String): Boolean;
begin
  Result := SDUWinHTTPRequest_WithUserAgent(URL,
    DEFAULT_HTTP_USERAGENT,
    ReturnedBody
    );
end;

function SDUWinHTTPRequest_WithUserAgent(
  URL: WideString;
  UserAgent: WideString;
  out ReturnedBody: String): Boolean;
var
  httpStatusCode: DWORD;
begin
  Result := False;
  if SDUWinHTTPRequest(URL, DEFAULT_HTTP_METHOD,
    False, '', DEFAULT_HTTP_USERAGENT,
    DEFAULT_HTTP_REQUESTVERSION, nil,
    httpStatusCode, ReturnedBody,
    nil) then begin
    Result := (httpStatusCode = HTTP_STATUS_OK);
  end;

end;

function SDUWinHTTPRequest(
  URL: WideString;

  Method: WideString;
  DataToSendPresent: Boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: String;
  ReturnedHeaders: TStrings  // May be set to nil
  ): Boolean;
begin
  Result := SDUWinHTTPRequest(URL,
    Method, DataToSendPresent,
    DataToSend, UserAgent,
    RequestVersion, AdditionalReqHeaders,
    DEFAULT_HTTP_TIMEOUTS,
    ReturnedStatusCode, ReturnedBody,
    ReturnedHeaders);

end;

function SDUWinHTTPRequest(
  URL: WideString;

  Method: WideString;
  DataToSendPresent: Boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  Timeouts: SDUWinHTTPTimeouts;

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: String;
  ReturnedHeaders: TStrings  // May be set to nil
  ): Boolean;
var

  splitURL: URL_COMPONENTS;

  serverName: WideString;
  serverPort: Integer;
  serverPath: WideString;
begin
  Result := False;

  ZeroMemory(@splitURL, sizeof(splitURL));
  splitURL.dwStructSize := sizeof(splitURL);

  splitURL.dwSchemeLength    := $FFFFFFFF;
  splitURL.dwHostNameLength  := $FFFFFFFF;
  splitURL.dwUrlPathLength   := $FFFFFFFF;
  splitURL.dwExtraInfoLength := $FFFFFFFF;

  if WinHttpCrackUrl(PWideString(URL), length(URL),
    0, @splitURL) then begin
    serverName := Copy(splitURL.lpszHostName, 1, splitURL.dwHostNameLength);
    serverPort := splitURL.nPort;
    serverPath := Copy(splitURL.lpszUrlPath, 1, splitURL.dwUrlPathLength);

    Result := SDUWinHTTPRequest(serverName,
      serverPort, serverPath,
      Method, DataToSendPresent,
      DataToSend, UserAgent,
      RequestVersion, AdditionalReqHeaders,
      Timeouts, ReturnedStatusCode,
      ReturnedBody, ReturnedHeaders
      );
  end;

end;

function SDUWinHTTPRequest(
  ServerName: WideString;
  ServerPort: Integer;
  ServerPath: WideString;

  Method: WideString;
  DataToSendPresent: Boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: String;
  ReturnedHeaders: TStrings  // May be set to nil
  ): Boolean;
begin
  Result := SDUWinHTTPRequest(ServerName,
    ServerPort, ServerPath,
    Method, DataToSendPresent,
    DataToSend, UserAgent,
    RequestVersion, AdditionalReqHeaders,
    DEFAULT_HTTP_TIMEOUTS,
    ReturnedStatusCode, ReturnedBody,
    ReturnedHeaders);

end;

function SDUWinHTTPRequest(
  ServerName: WideString;
  ServerPort: Integer;
  ServerPath: WideString;

  Method: WideString;
  DataToSendPresent: Boolean;
  DataToSend: Ansistring;

  UserAgent: WideString;
  RequestVersion: WideString;
  AdditionalReqHeaders: TStrings;  // May be set to nil

  Timeouts: SDUWinHTTPTimeouts;

  out ReturnedStatusCode: DWORD;
  out ReturnedBody: String;
  ReturnedHeaders: TStrings  // May be set to nil
  ): Boolean;
var
  hSession: HINTERNET;
  hConnect: HINTERNET;
  hRequest: HINTERNET;


  i: Integer;

  idx:     DWORD;
  wstrTmp: WideString;

  byteReceived: DWORD;
  page:         Ansistring;
  strHeaders:   WideString;
begin
  // Bail out early if it's just *not* going to work...
  if not (WinHttpCheckPlatform()) then begin
    Result := False;
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

  Result       := False;
  ReturnedBody := '';

  hSession := WinHttpOpen(PWideString(UserAgent),
    WINHTTP_ACCESS_TYPE_DEFAULT_PROXY,
    WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS,
    0);
  if (hSession <> 0) then begin
    WinHttpSetTimeouts(
      hSession,
      Timeouts.Name,
      Timeouts.Connect,
      Timeouts.Send,
      Timeouts.Receive
      );

    hConnect := WinHttpConnect(hSession,
      PWideString(ServerName), ServerPort,
      0);
    if (hConnect <> 0) then begin
      { TODO 1 -otdk -cenhance : set flags according to whether https or http }
      hRequest := WinHttpOpenRequest(hConnect,
        // "nil" as second parameter causes it to use "GET"
        PWideString(Method),
        PWideString(ServerPath),
        nil, WINHTTP_NO_REFERER,
        WINHTTP_DEFAULT_ACCEPT_TYPES,
        WINHTTP_FLAG_REFRESH or WINHTTP_FLAG_SECURE  //  use flag WINHTTP_FLAG_SECURE for https
        );
      if (hRequest <> 0) then begin
        Result := True;
        if (AdditionalReqHeaders <> nil) then begin
          if (AdditionalReqHeaders.Count > 0) then begin
            strHeaders := '';
            for i := 0 to (AdditionalReqHeaders.Count - 1) do begin
              if (strHeaders <> '') then begin
                strHeaders := strHeaders + wchar(#13) + wchar(#10);
              end;

              strHeaders := strHeaders + AdditionalReqHeaders[i];
            end;

            Result := WinHttpAddRequestHeaders(
              hRequest,
              PWideString(strHeaders), length(strHeaders),
              WINHTTP_ADDREQ_FLAG_REPLACE
              );
          end;
        end;

        if Result then begin
          if DataToSendPresent then begin
            Result := WinHttpWriteData(hRequest,
              PAnsiChar(DataToSend),
              length(DataToSend),
              @byteReceived);
          end;
        end;

        if Result then begin
          if WinHttpSendRequest(hRequest,
            WINHTTP_NO_ADDITIONAL_HEADERS,
            0, WINHTTP_NO_REQUEST_DATA,
            0, 0,
            nil) then begin
            if WinHttpReceiveResponse(hRequest, nil) then begin
              byteReceived := sizeof(ReturnedStatusCode);
              WinHttpQueryHeaders(
                hRequest,
                (WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER),
                nil, @ReturnedStatusCode,
                @byteReceived,
                nil
                );

              if (returnedHeaders <> nil) then begin
                idx          := 0;
                byteReceived := 0;
                WinHttpQueryHeaders(
                  hRequest,
                  WINHTTP_QUERY_RAW_HEADERS_CRLF,
                  WINHTTP_HEADER_NAME_BY_INDEX,
                  WINHTTP_NO_OUTPUT_BUFFER,
                  @byteReceived, @idx
                  );

                if (byteReceived > 0) then begin
                  wstrTmp := SDUWideStringOfWideChar('X', byteReceived);
                  if WinHttpQueryHeaders(hRequest,
                    WINHTTP_QUERY_RAW_HEADERS_CRLF,
                    WINHTTP_HEADER_NAME_BY_INDEX, PWideString(wstrTmp),
                    @byteReceived,
                    @idx) then begin
                    returnedHeaders.Text := wstrTmp;
                  end;
                end;
              end;


              repeat
                byteReceived := 0;
                if WinHttpQueryDataAvailable(hRequest, @byteReceived) then begin
                  // Call StringOfChar(...) to allocate buffer to be used
                  page := StringOfChar(AnsiChar('X'), byteReceived);
                  if WinHttpReadData(hRequest,
                    PAnsiChar(page),
                    byteReceived,
                    @byteReceived) then begin
                    ReturnedBody := ReturnedBody + page;
                    Result       := True;
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
  DialogTitle: String;
  URL: WideString;
  out ReturnedBody: String): TTimeoutGet;
begin
  Result := SDUGetURLProgress_WithUserAgent(
    DialogTitle,
    URL, ReturnedBody,
    DEFAULT_HTTP_USERAGENT
    );
end;

function SDUGetURLProgress_WithUserAgent(
  DialogTitle: String;
  URL: WideString;
  out ReturnedBody: String;
  UserAgent: WideString): TTimeoutGet;
var
  ProgressDlg: TdlgProgress;
  progResult:  Word;
  thrHTTPGet:  TThreadHTTPGet;
begin
  Result      := tgFailure;
  ProgressDlg := TdlgProgress.Create(nil);
  try



    ProgressDlg.Title                 := DialogTitle;
    ProgressDlg.Min                   := 0;
    ProgressDlg.Max                   := 100;
    ProgressDlg.Position              := 0;
    ProgressDlg.Indeterminate         := True;
    ProgressDlg.IndeterminateRunning  := True;
    ProgressDlg.CancelSetsModalResult := True;

    thrHTTPGet           := TThreadHTTPGet.Create(True);
    thrHTTPGet.ModalForm := ProgressDlg;
    thrHTTPGet.URL       := URL;
    thrHTTPGet.UserAgent := UserAgent;
    thrHTTPGet.Resume;

    try
      try
        progResult := ProgressDlg.ShowModal();
      finally
        thrHTTPGet.ModalForm := nil;
      end;
    except
      if thrHTTPGet.RetrievedOK then begin
        progResult := mrOk;
      end else begin
        progResult := mrAbort;
      end;
    end;

    ProgressDlg.IndeterminateRunning := False;
  finally
    ProgressDlg.Free;
  end;

  if (progResult = mrCancel) then begin
    Result := tgCancel;
  end else
  if (progResult = mrAbort) then begin
    Result := tgTimeout;
  end else
  if (progResult = mrOk) then begin
    // In this case, the thread terminated
    if thrHTTPGet.RetrievedOK then begin
      Result       := tgOK;
      ReturnedBody := thrHTTPGet.ReturnedBody;
    end else begin
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
  RetrievedOK := False;

  RetrievedOK := SDUWinHTTPRequest_WithUserAgent(
    URL,
    UserAgent, ReturnedBody
    );

  Synchronize(snc);
end;

procedure TThreadHTTPGet.snc();
begin
  if not (Terminated) then begin
    if (ModalForm <> nil) then begin
      if (ModalForm.modalresult = mrNone) then begin
        ModalForm.modalresult := mrOk;
      end;
    end;
  end;

end;


end.
