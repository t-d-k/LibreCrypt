unit SDPartitionImage_File;

interface

uses
  Classes, SDPartitionImage,
  SDUGeneral, Windows;

type
  EPartitionUnableToOpen = class (EPartitionError);

  TSDPartitionImage_File = class (TSDPartitionImage)
  PROTECTED
    FFilename:   String;
    FFileHandle: THandle;
    FSize:       ULONGLONG;
  PUBLIC
    constructor Create(); OVERRIDE;
    destructor Destroy(); OVERRIDE;

    function DoMount(): Boolean; OVERRIDE;
    procedure DoDismount(); OVERRIDE;

    function GetSize(): ULONGLONG; OVERRIDE;

    function ReadConsecutiveSectors(startSectorID: uint64; sectors: TStream;
      maxSize: Integer = -1): Boolean; OVERRIDE;
    function WriteConsecutiveSectors(startSectorID: uint64; sectors: TStream;
      maxSize: Integer = -1): Boolean; OVERRIDE;

  PUBLISHED
    property Filename: String Read FFilename Write FFilename;
  end;

implementation

uses
  Math,
  SDUClasses;

constructor TSDPartitionImage_File.Create();
begin
  inherited;
  FFilename   := '';
  FFileHandle := INVALID_HANDLE_VALUE;
  FSize       := 0;
end;

destructor TSDPartitionImage_File.Destroy();
begin
  inherited;
end;

function TSDPartitionImage_File.DoMount(): Boolean;
var
  retval: Boolean;
begin
  FSize := SDUGetFileSize(Filename);

  FFileHandle := CreateFile(PChar(Filename), (GENERIC_READ or GENERIC_WRITE),
    //GENERIC_READ,
    FILE_SHARE_READ, nil, OPEN_EXISTING,
    // (FILE_FLAG_RANDOM_ACCES or FILE_FLAG_NO_BUFFERING)
    FILE_ATTRIBUTE_NORMAL, 0);
  if (FFileHandle = INVALID_HANDLE_VALUE) then begin
    raise EPartitionUnableToOpen.Create('Unable to open partition file');
  end else begin
    retval := True;
  end;

  Result := retval;
end;

procedure TSDPartitionImage_File.DoDismount();
begin
  if (FFileHandle <> INVALID_HANDLE_VALUE) then begin
    CloseHandle(FFileHandle);
    FFileHandle := INVALID_HANDLE_VALUE;
  end;

end;

function TSDPartitionImage_File.ReadConsecutiveSectors(startSectorID: uint64;
  sectors: TStream; maxSize: Integer = -1): Boolean;
var
  hs:        THandleStream;
  retval:    Boolean;
  useLength: Integer;
begin
  // Short circuit...
  if (maxSize = 0) then begin
    retval := True;
  end else begin
    FSerializeCS.Acquire();
    try
      hs := THandleStream.Create(FFileHandle);
      try
        useLength := SectorSize;
        if (maxSize > 0) then begin
          useLength := maxSize;
        end;
        hs.Position := (startSectorID * SectorSize);
        sectors.CopyFrom(hs, min(SectorSize, useLength));
      finally
        hs.Free();
      end;

      retval := True;
    finally
      FSerializeCS.Release();
    end;

  end;

  Result := retval;
end;

function TSDPartitionImage_File.WriteConsecutiveSectors(startSectorID: uint64;
  sectors: TStream; maxSize: Integer = -1): Boolean;
var
  hs:        THandleStream;
  retval:    Boolean;
  useLength: Integer;
begin
  if ReadOnly then begin
    retval := False;
  end // Short circuit...
  else
  if (maxSize = 0) then begin
    retval := True;
  end else begin
    FSerializeCS.Acquire();
    try
      hs := THandleStream.Create(FFileHandle);
      try
        useLength := SectorSize;
        if (maxSize > 0) then begin
          useLength := maxSize;
        end;
        hs.Position := (startSectorID * SectorSize);
        hs.CopyFrom(sectors, min(SectorSize, useLength));
      finally
        hs.Free();
      end;

      retval := True;
    finally
      FSerializeCS.Release();
    end;

  end;

  Result := retval;
end;

function TSDPartitionImage_File.GetSize(): ULONGLONG;
begin
  Result := FSize;
end;

end.
