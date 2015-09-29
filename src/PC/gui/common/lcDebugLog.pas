unit lcDebugLog;

interface

uses
//sdu,lcutils
lcTypes;

 // the higher 'level' the mor important
procedure DebugMsg(msg: String;level :integer = 1);    overload;
procedure DebugMsg(Value: Boolean); overload;
procedure DebugMsg(uid: TGUID); overload;
procedure DebugMsg(msg: TSDUBytes); overload;
procedure DebugMsgBinary(msg: TSDUBytes); overload;
procedure DebugMsg(msg: array of Byte); overload;
procedure DebugMsg(Value: Integer); overload;
procedure DebugMsgBinary(msg: Ansistring); overload;
procedure DebugMsgBinaryPtr(msg: PAnsiChar; len: Integer);
procedure StopDebugging;
procedure DebugFlush();
procedure DebugNewFile();



implementation

uses
  //delphi
  system.Classes, vcl.Dialogs, system.Win.comobj, system.SysUtils, vcl.Forms,
  //sdu
  sdugeneral
  ;

var

    {$IFDEF FREEOTFE_DEBUG}
     _DebugStrings:      TStrings;
  _debugShowMessage:  Boolean;
  {$ENDIF}
  _debugLogfile:      String;
  _debugLevel : integer = 1; // set debug level to conrol amount. the smaller th more
/////////////////// private fns ////////////////////////////

procedure _MakeLogFileName;
begin

  _debugLogfile :=  ExtractFilePath(Application.ExeName) +'..\..\';
 {$IFDEF DEBUG}
     _debugLogfile :=  _debugLogfile +'..\..\';
   {$ENDIF}
   _debugLogfile :=  _debugLogfile +'logs\debug'+ '_'+IntToStr(random(1000))+'.log'
{  dlg:= TSaveDialog.Create(nil);
  try
    dlg.Options := dlg.Options + [ofDontAddToRecent];
    if dlg.Execute() then
      _debugLogfile := dlg.Filename;

  finally
    dlg.Free();
  end;  }
end;

procedure _EnsureLogCreated;
begin
  {$IFDEF FREEOTFE_DEBUG}
  _DebugShowMessage := False;
  // xxx - disable showmessages(...) of debug if built with debug on

  if _DebugStrings = nil then begin
     _DebugStrings := TStringList.Create();
    _MakeLogFileName;
  end;

{$ENDIF}
end;

procedure StopDebugging;
begin
  {$IFDEF FREEOTFE_DEBUG}
  _DebugStrings.Free();
{$ENDIF}
end;

/////////////////////////////////////////////////////////
procedure DebugMsg(msg: String;level :integer = 1);
begin

{$IFDEF FREEOTFE_DEBUG}
if level >= _debugLevel  then  begin

    _EnsureLogCreated;
  _DebugStrings.Add(msg);
{$IFDEF _GEXPERTS}
  SendDebug(msg);
{$ENDIF}
  if _debugShowMessage then
    showmessage(msg);
  end;
{$ENDIF}
end;

procedure DebugMsg(uid: TGUID);
begin
{$IFDEF FREEOTFE_DEBUG}
   DebugMsg(GUIDToString(uid));
{$ENDIF}
end;

procedure DebugMsg(msg: array of Byte);
{$IFDEF FREEOTFE_DEBUG}
var
msgstr: ansistring;
 {$ENDIF}
begin

{$IFDEF FREEOTFE_DEBUG}
   _EnsureLogCreated;
msgstr := PAnsiChar(@msg[0]);
setlength(msgstr,length(msg));
   DebugMsgBinary(msgstr);
{$ENDIF}
end;

procedure DebugMsg(Value: Integer);
begin
{$IFDEF FREEOTFE_DEBUG}
   DebugMsg(IntToStr(value));
{$ENDIF}
end;



procedure DebugMsg(Value: Boolean);
begin
  {$IFDEF FREEOTFE_DEBUG}
   DebugMsg(BoolToStr(value,true));
{$ENDIF}
end;

procedure DebugFlush();
begin
{$IFDEF FREEOTFE_DEBUG}
  _EnsureLogCreated;
     if _DebugStrings.Count> 0 then
  _DebugStrings.SaveToFile(_debugLogfile);

  {$ENDIF}
end;

procedure DebugNewFile();
begin
{$IFDEF FREEOTFE_DEBUG}

{$IFNDEF _GEXPERTS}
  if (_debugLogfile <> '') then begin
       DebugFlush();
    _MakeLogFileName;
  end;
{$ENDIF}
  if _DebugStrings <> nil then
  _DebugStrings.Clear();
  {$ENDIF}
end;
procedure DebugMsgBinary(msg: TSDUBytes); overload;
begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsgBinary(SDUBytesToString(msg));
 {$ENDIF}
end;
procedure DebugMsg(msg: TSDUBytes);
begin
{$IFDEF FREEOTFE_DEBUG}
DebugMsgBinary(SDUBytesToString(msg));
 {$ENDIF}
end;

procedure DebugMsgBinary(msg: Ansistring);
{$IFDEF FREEOTFE_DEBUG}
var
  prettyStrings: TStringList;
  i: integer;
 {$ENDIF}
begin

{$IFDEF FREEOTFE_DEBUG}
   _EnsureLogCreated;
  prettyStrings := TStringList.Create();
  try
    SDUPrettyPrintHex(msg, 0, Length(msg), TStringList(prettyStrings), 16);
    for i := 0 to (prettyStrings.Count - 1) do       begin
{$IFDEF _GEXPERTS}
      SendDebug(prettyStrings[i]);
{$ENDIF}
      end;
    _DebugStrings.AddStrings(prettyStrings);
    if _debugShowMessage then      begin
      showmessage(prettyStrings.Text);
      end;
  finally
    prettyStrings.Free();
  end;
 {$ENDIF}
end;

procedure DebugMsgBinaryPtr(msg: PAnsiChar; len: Integer);
{$IFDEF FREEOTFE_DEBUG}
var
  prettyStrings: TStringList;
  i: integer;
 {$ENDIF}
begin

{$IFDEF FREEOTFE_DEBUG}
    _EnsureLogCreated;
  prettyStrings := TStringList.Create();
  try
    SDUPrettyPrintHex(Pointer(msg), 0, len, TStringList(prettyStrings), 16);
    for i := 0 to (prettyStrings.Count - 1) do      begin
{$IFDEF _GEXPERTS}
      SendDebug(prettyStrings[i]);
{$ENDIF}
      end;
    _DebugStrings.AddStrings(prettyStrings);
    if _debugShowMessage then
      showmessage(prettyStrings.Text);

  finally
    prettyStrings.Free();
  end;
 {$ENDIF}
end;

initialization
{$IFDEF FREEOTFE_DEBUG}
  _DebugStrings := nil;
 {$ENDIF}

finalization
  StopDebugging; // stop pos. memory leak

end.
