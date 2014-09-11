unit SDPartitionImage_File;

interface

uses
  Classes, Windows,
  SDPartitionImage,
  SDUGeneral;

type
  EPartitionUnableToOpen = class(EPartitionError);

  TSDPartitionImage_File = class(TSDPartitionImage)
  protected
    FFilename: string;
    FFileHandle: THandle;
    FSize: ULONGLONG;
  public
    constructor Create(); override;
    destructor  Destroy(); override;

    function  DoMount(): boolean; override;
    procedure DoDismount(); override;

    function  GetSize(): ULONGLONG; override;

    function  ReadConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean; override;
    function  WriteConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean; override;

  published
    property  Filename: string read FFilename write FFilename;
  end;

implementation

uses
  Math,
  SDUClasses;

constructor TSDPartitionImage_File.Create();
begin
  inherited;
  FFilename := '';
  FFileHandle := INVALID_HANDLE_VALUE;
  FSize := 0;
end;

destructor TSDPartitionImage_File.Destroy();
begin
  inherited;
end;

function TSDPartitionImage_File.DoMount(): boolean;
var
  retval: boolean;
begin
  FSize := SDUGetFileSize(Filename);

  FFileHandle := CreateFile(
                            PChar(Filename),
                            (GENERIC_READ or GENERIC_WRITE),
                            //GENERIC_READ,
                            FILE_SHARE_READ,
                            nil,
                            OPEN_EXISTING,
                            // (FILE_FLAG_RANDOM_ACCES or FILE_FLAG_NO_BUFFERING)
                            FILE_ATTRIBUTE_NORMAL,
                            0
                           );
  if (FFileHandle = INVALID_HANDLE_VALUE) then
    begin
    raise EPartitionUnableToOpen.Create('Unable to open partition file');
    end
  else
    begin
    retval := TRUE;
    end;

  Result := retval;
end;

procedure TSDPartitionImage_File.DoDismount();
begin
  if (FFileHandle <> INVALID_HANDLE_VALUE) then
    begin
    CloseHandle(FFileHandle);
    FFileHandle := INVALID_HANDLE_VALUE;
    end;
    
end;

function TSDPartitionImage_File.ReadConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean;
var
  hs: THandleStream;
  retval: boolean;
  useLength: integer;
begin
  // Short circuit...
  if (maxSize = 0) then
    begin
    retval := TRUE;
    end
  else
    begin
    FSerializeCS.Acquire();
    try
      hs:= THandleStream.Create(FFileHandle);
      try
        useLength := SectorSize;
        if (maxSize > 0) then
          begin
          useLength := maxSize;
          end;
        hs.Position := (startSectorID * SectorSize);
        sectors.CopyFrom(hs, min(SectorSize, useLength));
      finally
        hs.Free();
      end;

      retval := TRUE;
    finally
      FSerializeCS.Release();
    end;

    end;

  Result := retval;
end;

function TSDPartitionImage_File.WriteConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean;
var
  hs: THandleStream;
  retval: boolean;
  useLength: integer;
begin
  if ReadOnly then
    begin
    retval := FALSE;
    end
  // Short circuit...
  else if (maxSize = 0) then
    begin
    retval := TRUE;
    end
  else
    begin
    FSerializeCS.Acquire();
    try
      hs:= THandleStream.Create(FFileHandle);
      try
        useLength := SectorSize;
        if (maxSize > 0) then
          begin
          useLength := maxSize;
          end;
        hs.Position := (startSectorID * SectorSize);
        hs.CopyFrom(sectors, min(SectorSize, useLength));
      finally
        hs.Free();
      end;

      retval := TRUE;
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

END.

