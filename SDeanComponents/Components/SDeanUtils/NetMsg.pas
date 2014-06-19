unit NetMsg;

// *****************************************************************************
// *****************************************************************************
//                                  IMPORTANT!
// *****************************************************************************
// *****************************************************************************

// The NetAPI only operates with Windows NT/2000.

// If you wish to create an application using this unit, you have a choice
// between statically and dynamically linking with NetAPI32.dll.

// IF YOUR COMPILED EXECUTABLE IS ONLY EVER GOING TO BE RUN UNDER
// WINDOWS NT/2000:
// It is recommended that you *statically* link with this DLL.
// In order to statically link your executable to this DLL, you must define the
// compiler directive "_NETMSG_STATIC".
// To do this, goto "Project options", "Directories/Conditionals" tab and
// include "_NETMSG_STATIC" in the "Conditional defines" list
// Apart from this, all you need do is include this unit in your "uses" clause


// IF YOUR COMPILED EXECUTABLE WILL AT ANY TIME BE RUN UNDER WINDOWS 9X/ME:
// Your executable must *dynamically* link with this DLL, otherwise you'll get
// a DLL error when your program starts up, and it will simply crash and die.
// In order to dynamically link your executable to this DLL, you don't need
// to do anything other than include this unit in your "uses" clause.

interface

uses
  Windows;

const
   NETAPI_DLL = 'NetAPI32.dll';

type

  NET_API_STATUS = DWORD;


  // ---------------------------------------------------------------------------
  // Net API interface

  function NetMessageNameAdd(
                             servername:PWChar;
                             msgname:PWChar
                             ) : NET_API_STATUS;
                             stdcall;

  function NetMessageBufferSend(
                                servername :PWChar;
                                msgname :PWChar;
                                fromname :PWChar;
                                buf : PByte;
                                buflen :DWORD
                                ) : NET_API_STATUS;
                                stdcall;

  function NetMessageNameDel(
                             servername :PWChar;
                             msgname :PWChar
                             ) : NET_API_STATUS;
                             stdcall;

  function NetMessageNameEnum(
                              servername : PWChar;
                              level : DWORD;
                              bufptr : PByte;
                              prefmaxlen : DWORD;
                              entriesread : LPDWORD;
                              totalentries : LPDWORD;
                              resume_handle : LPDWORD
                              ) : NET_API_STATUS;
                              stdcall;

  function NetMessageNameGetInfo(
                                 servername: PWChar;
                                 msgname: PWChar;
                                 level : DWORD;
                                 bufptr : PByte
                                 ) : NET_API_STATUS;
                                 stdcall;

implementation

{$ifdef _NETMSG_STATIC}

// -----------------------------------------------------------------------------
// Net API implementation - staticly linked DLL

function NetMessageNameAdd;
         external NETAPI_DLL name 'NetMessageNameAdd';

function NetMessageBufferSend;
         external NETAPI_DLL name 'NetMessageBufferSend';

function NetMessageNameDel;
         external NETAPI_DLL name 'NetMessageNameDel';

function NetMessageNameEnum;
         external NETAPI_DLL name 'NetMessageNameEnum';

function NetMessageNameGetInfo;
         external NETAPI_DLL name 'NetMessageNameGetInfo';

// -----------------------------------------------------------------------------
{$else}
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Net API implementation - dynamicly linked DLL

type
  TNetMessageNameAdd = function (
                                 servername:PWChar;
                                 msgname:PWChar
                                ) : NET_API_STATUS;
                                stdcall;

  TNetMessageBufferSend = function (
                                    servername :PWChar;
                                    msgname :PWChar;
                                    fromname :PWChar;
                                    buf : PByte;
                                    buflen :DWORD
                                   ) : NET_API_STATUS;
                                   stdcall;

  TNetMessageNameDel= function (
                                servername :PWChar;
                                msgname :PWChar
                               ) : NET_API_STATUS;
                               stdcall;

  TNetMessageNameEnum = function (
                                  servername : PWChar;
                                  level : DWORD;
                                  bufptr : PByte;
                                  prefmaxlen : DWORD;
                                  entriesread : LPDWORD;
                                  totalentries : LPDWORD;
                                  resume_handle : LPDWORD
                                 ) : NET_API_STATUS;
                                 stdcall;

  TNetMessageNameGetInfo = function (
                                     servername: PWChar;
                                     msgname: PWChar;
                                     level : DWORD;
                                     bufptr : PByte
                                    ) : NET_API_STATUS;
                                    stdcall;

// -----------------------------------------------------------------------------
function NetMessageNameAdd(
                           servername:PWChar;
                           msgname:PWChar
                           ) : NET_API_STATUS;
                           stdcall;
var
  libHandle: THandle;
  libFunction: TNetMessageNameAdd;
begin

  libHandle := LoadLibrary(NETAPI_DLL);
  if libHandle = 0 then
    begin
    Result := GetLastError;
    exit;
    end;

  try

    @libFunction := GetProcAddress(libHandle, 'NetMessageNameAdd');
    if @libFunction = nil then
      begin
      Result := GetLastError;
      exit;
      end;

    Result := libFunction(servername, msgname);

  finally
    FreeLibrary(libHandle);
  end;
end;





function NetMessageBufferSend(
                              servername :PWChar;
                              msgname :PWChar;
                              fromname :PWChar;
                              buf : PByte;
                              buflen :DWORD
                              ) : NET_API_STATUS;
                              stdcall;
var
  libHandle: THandle;
  libFunction: TNetMessageBufferSend;
begin

  libHandle := LoadLibrary(NETAPI_DLL);
  if libHandle = 0 then
    begin
    Result := GetLastError;
    exit;
    end;

  try

    @libFunction := GetProcAddress(libHandle, 'NetMessageBufferSend');
    if @libFunction = nil then
      begin
      Result := GetLastError;
      exit;
      end;

    Result := libFunction(servername, msgname, fromname, buf, buflen);

  finally
    FreeLibrary(libHandle);
  end;
end;





function NetMessageNameDel(
                           servername :PWChar;
                           msgname :PWChar
                           ) : NET_API_STATUS;
                           stdcall;
var
  libHandle: THandle;
  libFunction: TNetMessageNameDel;
begin

  libHandle := LoadLibrary(NETAPI_DLL);
  if libHandle = 0 then
    begin
    Result := GetLastError;
    exit;
    end;

  try

    @libFunction := GetProcAddress(libHandle, 'NetMessageNameDel');
    if @libFunction = nil then
      begin
      Result := GetLastError;
      exit;
      end;

    Result := libFunction(servername, msgname);

  finally
    FreeLibrary(libHandle);
  end;
end;




function NetMessageNameEnum(
                            servername : PWChar;
                            level : DWORD;
                            bufptr : PByte;
                            prefmaxlen : DWORD;
                            entriesread : LPDWORD;
                            totalentries : LPDWORD;
                            resume_handle : LPDWORD
                            ) : NET_API_STATUS;
                            stdcall;
var
  libHandle: THandle;
  libFunction: TNetMessageNameEnum;
begin

  libHandle := LoadLibrary(NETAPI_DLL);
  if libHandle = 0 then
    begin
    Result := GetLastError;
    exit;
    end;

  try

    @libFunction := GetProcAddress(libHandle, 'NetMessageNameEnum');
    if @libFunction = nil then
      begin
      Result := GetLastError;
      exit;
      end;

    Result := libFunction(servername,
                          level,
                          bufptr,
                          prefmaxlen,
                          entriesread,
                          totalentries,
                          resume_handle);

  finally
    FreeLibrary(libHandle);
  end;
end;



function NetMessageNameGetInfo(
                               servername: PWChar;
                               msgname: PWChar;
                               level : DWORD;
                               bufptr : PByte
                               ) : NET_API_STATUS;
                               stdcall;
var
  libHandle: THandle;
  libFunction: TNetMessageNameGetInfo;
begin

  libHandle := LoadLibrary(NETAPI_DLL);
  if libHandle = 0 then
    begin
    Result := GetLastError;
    exit;
    end;

  try

    @libFunction := GetProcAddress(libHandle, 'NetMessageNameGetInfo');
    if @libFunction = nil then
      begin
      Result := GetLastError;
      exit;
      end;

    Result := libFunction(servername, msgname, level, bufptr);

  finally
    FreeLibrary(libHandle);
  end;
end;





{$endif}

END.

