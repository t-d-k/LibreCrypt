unit SDUNamedPipe_U;
// Description: Named Pipes
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


// A simple component to wrap up named pipes
// Only tested under NT, but you should be able to use it under Windows 9x,
// although under Windows 9x, you can only use named pipes as a client (Windows
// limitation)


// Typical usage of the SDUNamedPipe component:

// Client:
//   ClientConnect(pipename)
//     - Client will now block until a connection to the pipe is established
//   WriteString(data)
//     - Client will write to the pipe
//   ReadString(data)
//     - Client will read from the pipe
//   Close();
//     - Connection will terminate

// Server:
//   CreatePipe(pipename)
//     - Pipe is created
//   ServerConnect()
//     - Server will block until a client connects to the pipe
//   WriteString(data)
//     - Server will write to the pipe
//   ReadString(data)
//     - Server will read from the pipe
//   Disconnect()
//     - Server will disconnect from the client
//     - After this point, the server will probably want to loop back to the
//       ServerConnect() step
//   Close();
//     - Pipe is closed and released


interface

uses
  classes,
  windows; // Required for THandle

const
  NAMEDPIPE_OUT_BUFFER_SIZE = 1024;
  NAMEDPIPE_IN_BUFFER_SIZE  = 1024;
  NAMEDPIPE_DEFAULT_TIMEOUT = 5000; // in ms

type
  TSDUNamedPipe = class(TComponent)
    private
      fServer: boolean;
      fName: string;
      fFlushBuffersAfterSend: boolean;

      function CheckPipeHandle(): boolean;

    public
      PipeHandle: THandle;

      constructor Create(AOwner: TComponent); override;
      destructor  Destroy(); override;

      // Create the specified named pipe
      function CreatePipe(): boolean; overload;
      function CreatePipe(theName: string): boolean; overload;

      // ClientConnect connects to an existing pipe (CLIENT)
      function ClientConnect(): boolean; overload;
      function ClientConnect(theName: string): boolean; overload;

      // ServerConnect connects to an existing pipe (SERVER)
      // The pipe must be created with CreatePipe before this is called
      function ServerConnect(): boolean;

      function Disconnect(): boolean;

      // Writes may block until data is written
      function Write(data: string; theLength: integer): boolean;
      function WriteString(msg: string): boolean;

      // Reads may block until data is read
      function Read(toReadLength: integer; var data: string; var haveReadLength: integer): boolean;
      function ReadString(var msg: string): boolean;

      function Flush(): boolean;
      function Close(): boolean;

      property Name: string read fName write fName;
      property FlushBuffersAfterSend: boolean read fFlushBuffersAfterSend write fFlushBuffersAfterSend;

  end;

procedure Register;

implementation

uses
  sysutils; // required for Exception;

// -----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUNamedPipe]);
end;

// -----------------------------------------------------------------------------
constructor TSDUNamedPipe.Create(AOwner: TComponent);
begin
  inherited;
  fServer := FALSE;
  PipeHandle := INVALID_HANDLE_VALUE;
  fFlushBuffersAfterSend:= TRUE;
  Name := '';
end;

// -----------------------------------------------------------------------------
destructor  TSDUNamedPipe.Destroy();
begin
  inherited;
end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.CheckPipeHandle(): boolean;
begin
  Result := TRUE;
  if PipeHandle=INVALID_HANDLE_VALUE then
    begin
    raise Exception.Create('Pipe not connected');

    Result := FALSE;
    end;
    
end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.ClientConnect(): boolean;
var
  pipeName: string;
begin
  pipeName:= '\\.\pipe\'+Name;

  PipeHandle := CreateFile(
                           PChar(pipeName),
                           GENERIC_READ or GENERIC_WRITE,
                           FILE_SHARE_READ or FILE_SHARE_WRITE,
                           nil,
                           OPEN_EXISTING,
                           0,
                           0
                          );

  Result := (PipeHandle<>INVALID_HANDLE_VALUE)    and
            (PipeHandle<>ERROR_INVALID_PARAMETER);

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.ClientConnect(theName: string): boolean;
begin
  Name := theName;
  Result := ClientConnect();

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.ServerConnect(): boolean;
begin
  Result := FALSE;
  if not(CheckPipeHandle()) then
    begin
    exit;
    end;

  Result := ConnectNamedPipe(PipeHandle, nil);

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.Disconnect(): boolean;
begin
  Result := FALSE;
  if not(CheckPipeHandle()) then
    begin
    exit;
    end;

  Result := DisconnectNamedPipe(PipeHandle);

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.Close(): boolean;
begin
  Result := FALSE;
  if not(CheckPipeHandle()) then
    begin
    exit;
    end;

  Result := CloseHandle(PipeHandle);

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.WriteString(msg: string): boolean;
begin
  Result := Write(msg+#0, length(msg)+1);

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.Write(data: string; theLength: integer): boolean;
var
  bytesSent: cardinal;
  i: integer;
  totalSent: cardinal;
begin
  Result := FALSE;
  if not(CheckPipeHandle()) then
    begin
    exit;
    end;

  totalSent := 0;

  for i:=1 to theLength do
    begin
    WriteFile(PipeHandle,
              data[i],
              1,
              bytesSent,
              nil);

    inc(totalSent, bytesSent);
    end;

  if FlushBuffersAfterSend then
    begin
    Flush();
    end;

  Result := (totalSent=cardinal(theLength));

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.Read(toReadLength: integer; var data: string; var haveReadLength: integer): boolean;
begin
  raise Exception.Create('TSDUNamedPipe.Read not currently implemented');

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.ReadString(var msg: string): boolean;
var
  buf: array [0..1024] of byte;
  bytesRead: cardinal;
begin
  Result := FALSE;
  if not(CheckPipeHandle()) then
    begin
    exit;
    end;

  bytesRead := 1;
  buf[0] := 1;
  msg := '';
  while ((bytesRead>0) and (buf[0]<>0)) do
    begin
    ReadFile(PipeHandle,
             buf,
             1,
             bytesRead,
             nil);

    if ((bytesRead>0) and (buf[0]<>0)) then
      begin
      msg := msg + char(buf[0]);
      end;

    end;

  Result := (buf[0]=0);

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.Flush(): boolean;
begin
  Result := FALSE;
  if not(CheckPipeHandle()) then
    begin
    exit;
    end;

  Result := FlushFileBuffers(PipeHandle);
end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.CreatePipe(): boolean;
var
  pipeName: string;
  sa: SECURITY_ATTRIBUTES;
  pSD: PSECURITY_DESCRIPTOR;
  mSD: SECURITY_DESCRIPTOR;
begin

  pipeName:= '\\.\pipe\'+Name;

  pSD := @mSD;

  if not(InitializeSecurityDescriptor(pSD, SECURITY_DESCRIPTOR_REVISION)) then
    begin
    raise Exception.Create('Bad InitializeSecurityDescriptor');
    end;

  // add a NULL disc. ACL to the security descriptor.
  if not(SetSecurityDescriptorDacl(pSD, TRUE, nil, FALSE)) then
    begin
    raise Exception.Create('Bad SetSecurityDescriptorDacl');
    end;

  sa.nLength := sizeof(sa);
  sa.lpSecurityDescriptor := pSD;
  sa.bInheritHandle := TRUE;


  PipeHandle := CreateNamedPipe(
                                PChar(pipeName),
                                PIPE_ACCESS_DUPLEX,
                                //                                PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_NOWAIT,
                                PIPE_TYPE_BYTE,
                                PIPE_UNLIMITED_INSTANCES,
                                NAMEDPIPE_OUT_BUFFER_SIZE,
                                NAMEDPIPE_IN_BUFFER_SIZE,
                                NAMEDPIPE_DEFAULT_TIMEOUT,
                                pSD
                                );

  Result := (PipeHandle<>INVALID_HANDLE_VALUE)    and
            (PipeHandle<>ERROR_INVALID_PARAMETER);

end;

// -----------------------------------------------------------------------------
function TSDUNamedPipe.CreatePipe(theName: string): boolean;
begin
  Name := theName;
  Result := CreatePipe();
end;

// -----------------------------------------------------------------------------


END.

