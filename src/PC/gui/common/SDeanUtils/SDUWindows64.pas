unit SDUWindows64;

interface

function SDUIsWow64Process(const hProcess: THandle): boolean;
function SDUWow64EnableWow64FsRedirection(Enable: boolean): boolean;
function SDUWow64RevertWow64FsRedirection(const OldValue: Pointer): boolean;
function SDUWow64DisableWow64FsRedirection(out OldValue: Pointer): boolean;

function SDULoadLibKernel32(): boolean;
procedure SDUFreeLibKernel32();

implementation

uses
  Windows;

const
  KERNEL32_DLL = 'Kernel32.dll';

type
  PPointer = ^Pointer;

  TIsWow64PRocess = function (
                              hProcess: THandle;
                              Wow64Process: PBoolean
                             ): boolean; stdcall;
  TWow64EnableWow64FsRedirection = function (
                                             Enable: boolean
                                            ): boolean; stdcall;

  TWow64RevertWow64FsRedirection = function (
                                             OldValue: Pointer
                                            ): boolean; stdcall;

  TWow64DisableWow64FsRedirection = function (
                                              OldValue: PPointer
                                             ): boolean; stdcall;


var
  hLibKernel32: THandle;
  fnIsWow64Process: TIsWow64PRocess;
  fnWow64EnableWow64FsRedirection: TWow64EnableWow64FsRedirection;
  fnWow64RevertWow64FsRedirection: TWow64RevertWow64FsRedirection;
  fnWow64DisableWow64FsRedirection: TWow64DisableWow64FsRedirection;

function SDULoadLibKernel32(): boolean;
begin
  hLibKernel32 := LoadLibrary(KERNEL32_DLL);
  Result := (hLibKernel32 <> 0);
  if not Result then  begin
    SDUFreeLibKernel32();
    end   else    begin
    @fnIsWow64Process                 := GetProcAddress(hLibKernel32, 'IsWow64Process');
    @fnWow64EnableWow64FsRedirection  := GetProcAddress(hLibKernel32, 'Wow64EnableWow64FsRedirection');
    @fnWow64RevertWow64FsRedirection  := GetProcAddress(hLibKernel32, 'Wow64RevertWow64FsRedirection');
    @fnWow64DisableWow64FsRedirection := GetProcAddress(hLibKernel32, 'Wow64DisableWow64FsRedirection');
   end;


end;

procedure SDUFreeLibKernel32();
begin
  if (hLibKernel32 <> 0) then
    begin
    FreeLibrary(hLibKernel32);
    hLibKernel32 := 0;
    end;

  @fnIsWow64Process                 := nil;
  @fnWow64EnableWow64FsRedirection  := nil;
  @fnWow64RevertWow64FsRedirection  := nil;
  @fnWow64DisableWow64FsRedirection := nil;

end;

function SDUIsWow64Process(const hProcess: THandle): boolean;
var
  wow64Retval: boolean;
begin
  Result := FALSE;

  if (@fnIsWow64Process <> nil) then  begin
    Result := fnIsWow64Process(hProcess, @wow64Retval);
    // if no result assume false
    if Result then Result :=   wow64Retval;
   end;


end;

function SDUWow64EnableWow64FsRedirection(Enable: boolean): boolean;
begin
  Result := FALSE;

  if (@fnWow64EnableWow64FsRedirection <> nil) then
    begin
    Result := fnWow64EnableWow64FsRedirection(Enable);
    end;


end;

function SDUWow64RevertWow64FsRedirection(const OldValue: Pointer): boolean;
begin
  Result := FALSE;

  if (@fnWow64RevertWow64FsRedirection <> nil) then
    begin
    Result := fnWow64RevertWow64FsRedirection(OldValue);
    end;


end;

function SDUWow64DisableWow64FsRedirection(out OldValue: Pointer): boolean;
begin
  Result := FALSE;

  if (@fnWow64DisableWow64FsRedirection <> nil) then
    begin
    Result := fnWow64DisableWow64FsRedirection(@OldValue);
    end;


end;


initialization
  SDULoadLibKernel32();

finalization
  SDUFreeLibKernel32();

END.

