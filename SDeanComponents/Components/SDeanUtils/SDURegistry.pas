unit SDURegistry;
// Description: Sarah Dean's Registry Object
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
// This unit extends TRegistry to allow passing default values in if the named
// value doens't exist

interface

uses
  Registry;

type
  TSDURegistry = class(TRegistry)
  public
    function ReadBinaryData(const Name: String; var Buffer; BufSize: Integer; var Default; DefaultSize: Integer): Integer; overload;
    function ReadBool(const Name: String; Default: Boolean): Boolean; overload;
    function ReadCurrency(const Name: String; Default: Currency): Currency; overload;
    function ReadDate(const Name: String; Default: TDateTime): TDateTime; overload;
    function ReadDateTime(const Name: String; Default: TDateTime): TDateTime; overload;
    function ReadFloat(const Name: String; Default: Double): Double; overload;
    function ReadInteger(const Name: String; Default: Integer): Integer; overload;
    function ReadString(const Name: String; Default: String): String; overload;
    function ReadTime(const Name: String; Default: TDateTime): TDateTime; overload;

  end;

implementation

uses
  Consts,  // Required for SInvalidRegType
  SysUtils; // Required for StrMove

function TSDURegistry.ReadBinaryData(const Name: String; var Buffer; BufSize: Integer; var Default; DefaultSize: Integer): Integer;
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadBinaryData(Name, Buffer, BufSize);
    end
  else
    begin
    if (BufSize >= DefaultSize) then
      begin
      StrMove(PChar(Buffer), PChar(Default), DefaultSize);
      Result := DefaultSize;
      end
    else
      begin
      raise ERegistryException.CreateResFmt(@SInvalidRegType, [Name]);
      end;

    end;

end;

function TSDURegistry.ReadBool(const Name: String; Default: Boolean): Boolean; 
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadBool(Name);
    end
  else
    begin
    Result := Default;
    end;

end;

function TSDURegistry.ReadCurrency(const Name: String; Default: Currency): Currency;
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadCurrency(Name);
    end
  else
    begin
    Result := Default;
    end;

end;

function TSDURegistry.ReadDate(const Name: String; Default: TDateTime): TDateTime;
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadDate(Name);
    end
  else
    begin
    Result := Default;
    end;

end;

function TSDURegistry.ReadDateTime(const Name: String; Default: TDateTime): TDateTime;
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadDateTime(Name);
    end
  else
    begin
    Result := Default;
    end;

end;

function TSDURegistry.ReadFloat(const Name: String; Default: Double): Double;
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadFloat(Name);
    end
  else
    begin
    Result := Default;
    end;

end;

function TSDURegistry.ReadInteger(const Name: String; Default: Integer): Integer;
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadInteger(Name);
    end
  else
    begin
    Result := Default;
    end;

end;

function TSDURegistry.ReadString(const Name: String; Default: String): String;
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadString(Name);
    end
  else
    begin
    Result := Default;
    end;

end;

function TSDURegistry.ReadTime(const Name: String; Default: TDateTime): TDateTime;
begin
  if (ValueExists(Name)) then
    begin
    Result := ReadTime(Name);
    end
  else
    begin
    Result := Default;
    end;

end;


END.


