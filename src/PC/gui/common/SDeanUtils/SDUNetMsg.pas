unit SDUNetMsg;
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


// *****************************************************************************
// *****************************************************************************
//                                  IMPORTANT!
// *****************************************************************************
// *****************************************************************************

// It is *vital* that you read the comments made in "NetMsg.pas" BEFORE
// attempting to use this unit.

// Failure to do so could lead to your program simply crashing with DLL errors
// when it starts up under Windows 9x/Me.


interface


// Send a message out over the network
// Equiv to "net send"
// Only works under Windows NT/2000, and requires the Messenger Service to be
// running
function SDUNetSend(msg: string; msgTo: string; msgFrom: string = ''): boolean;



implementation


uses
  sysutils,
  Windows,
  NetMsg;


// Ensure we're running under Windows NT, raise an exception if we're not.
procedure SDUNetMsgCheckRunningNT();
var
  OSversion: DWORD;
begin
  OSversion := GetVersion();
  if not(OSversion<$80000000) then
    begin
    raise Exception.Create('SDUNetMsg requires Windows NT/2000');
    end;

end;


function SDUNetSend(msg: string; msgTo: string; msgFrom: string = ''): boolean;
var
  sentOK: NET_API_STATUS;
  Recipient: array[0..64] of WideChar;
  From: array[0..64] of WideChar;
  Buffer: array[0..500] of WideChar;
begin
  SDUNetMsgCheckRunningNT();


  StringToWideChar(msgFrom, @From, SizeOf(From));
  StringToWideChar(msgTo, @Recipient, SizeOf(Recipient));
  StringToWideChar(msg, @Buffer, SizeOf(Buffer));

  if msgFrom='' then
    begin
    sentOK := NetMessageBufferSend(nil,
                                   @Recipient,
                                   nil,
                                   @Buffer,
                                   length(msg)*2);
    end
  else
    begin
    sentOK := NetMessageBufferSend(nil,
                                   @Recipient,
                                   @From,
                                   @Buffer,
                                   length(msg)*2);
    end;


  Result := (sentOK=0);
//  if not(Return = NERR_Success) then ShowMessage('Error');

end;


END.

