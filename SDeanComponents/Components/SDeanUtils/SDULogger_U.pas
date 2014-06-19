unit SDULogger_U;
// Description: Sarah Dean's Logging Object
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

type
{$TYPEINFO ON} // Needed to allow "published"

  // Forward declaration...
  TSDULogger = class;

  TSDULogMessageEvent = procedure(Sender: TSDULogger; Msg: string) of object;

  TSDULogger = class
  private
    FFilename: string;
    FOnLogMessage: TSDULogMessageEvent;
    FIndent: integer;

  public
    constructor Create(); overload;
    constructor Create(fname: string); overload;
    destructor  Destroy(); override;


    // Log a message, together with time/date
    // If "timedAt" isn't specified, the current time will be used
    function  LogMessage(msg: string; timedAt: TDateTime = 0): boolean;

    procedure LogIndent();
    procedure LogOutdent();

    function FormatMsg(msg: string; timedAt: TDateTime = 0): string;

  published
    property Filename: string read FFilename write FFilename;
    property OnLogMessage: TSDULogMessageEvent read FOnLogMessage write FOnLogMessage;
  end;

implementation

uses
  // If this compiler directive is set, then the logger will attempt to send
  // messages to the GExperts debug window
  // See: www.gexperts.org
{$IFDEF _GEXPERTS_DEBUG}
  DbugIntf,
{$ENDIF}
  sysutils;

constructor TSDULogger.Create();
begin
  Create('');
end;


constructor TSDULogger.Create(fname: string);
begin
  inherited Create();
  Filename := fname;
  FOnLogMessage := nil;
  FIndent := 0;

end;


destructor TSDULogger.Destroy();
begin
  inherited;

end;


function TSDULogger.FormatMsg(msg: string; timedAt: TDateTime = 0): string;
begin
  if (timedAt = 0) then
    begin
    timedAt := Now();
    end;
    
  Result := '['+DateTimeToStr(timedAt)+'] '+StringOfChar(' ', (2 * FIndent))+msg;
end;


// If "timedAt" isn't specified, the current time will be used
function TSDULogger.LogMessage(msg: string; timedAt: TDateTime = 0): boolean;
var
  logfile: TextFile;
  fmtMsg: string;
begin
  fmtMsg := FormatMsg(msg, timedAt);

{$IFDEF _GEXPERTS_DEBUG}
  // Send debug message to GExperts debug window
  SendDebug(fmtMsg);
{$ENDIF}

  if Filename<>'' then
    begin
    AssignFile(logfile, Filename);
    if FileExists(Filename) then
      begin
      Append(logfile);
      end
    else
      begin
      Rewrite(logfile);
      end;
    Writeln(logfile, fmtMsg);
    CloseFile(logfile);
    end;

  if assigned(FOnLogMessage) then
    begin
    FOnLogMessage(self, msg);
    end;

  Result := TRUE;

end;

procedure TSDULogger.LogIndent();
begin
  inc(FIndent);
end;

procedure TSDULogger.LogOutdent();
begin
  dec(FIndent);
end;

END.

