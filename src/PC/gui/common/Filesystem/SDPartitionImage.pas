unit SDPartitionImage;

interface

uses
  Classes, SDUGeneral, SyncObjs, SysUtils, Windows;

const
  DEFAULT_SECTOR_SIZE = 512;

type
  EPartitionError = class (Exception);
  EPartitionNotMounted = class (EPartitionError);

{$M+}// Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"
  TSDPartitionImage = class
  PROTECTED
    FMounted:    Boolean;
    FSectorSize: Integer;
    FReadOnly:   Boolean;

    FSerializeCS: TCriticalSection; // Protect by serializing read/write
                                    // operations in multithreaded operations

    procedure SetMounted(newMounted: Boolean);
    procedure AssertMounted();

    function DoMount(): Boolean; VIRTUAL;
    procedure DoDismount(); VIRTUAL;

    function GetSize(): ULONGLONG; VIRTUAL; ABSTRACT;

  PUBLIC
    constructor Create(); VIRTUAL;
    destructor Destroy(); OVERRIDE;

    function ReadSector(sectorID: uint64; sector: TStream; maxSize: Integer = -1): Boolean;
      VIRTUAL;
    function WriteSector(sectorID: uint64; sector: TStream; maxSize: Integer = -1): Boolean;
      VIRTUAL;
    function ReadConsecutiveSectors(startSectorID: uint64; sectors: TStream;
      maxSize: Integer = -1): Boolean; VIRTUAL; ABSTRACT;
    function WriteConsecutiveSectors(startSectorID: uint64; sectors: TStream;
      maxSize: Integer = -1): Boolean; VIRTUAL; ABSTRACT;

    function CopySector(srcSectorID: uint64; destSectorID: uint64): Boolean;

  PUBLISHED
    property Mounted: Boolean Read FMounted Write SetMounted;
    property ReadOnly: Boolean Read FReadOnly Write FReadOnly;
    property Size: ULONGLONG Read GetSize;
    property SectorSize: Integer Read FSectorSize Write FSectorSize;

  end;

implementation

uses
  SDUClasses;

constructor TSDPartitionImage.Create();
begin
  inherited;
  FMounted     := False;
  FSectorSize  := DEFAULT_SECTOR_SIZE;
  FSerializeCS := TCriticalSection.Create();
end;

destructor TSDPartitionImage.Destroy();
begin
  Mounted := False;
  FSerializeCS.Free();
  inherited;
end;

function TSDPartitionImage.DoMount(): Boolean;
begin
  FSerializeCS := TCriticalSection.Create();
  Result       := True;
end;

procedure TSDPartitionImage.DoDismount();
begin
  FSerializeCS.Free();
end;

procedure TSDPartitionImage.SetMounted(newMounted: Boolean);
begin
  inherited;
  if (newMounted <> Mounted) then begin
    if newMounted then begin
      FMounted := DoMount();
    end else begin
      DoDismount();
      FMounted := False;
    end;

  end;
end;

procedure TSDPartitionImage.AssertMounted();
begin
  inherited;

  if not (Mounted) then begin
    raise EPartitionNotMounted.Create('Partition not mounted');
  end;
end;


function TSDPartitionImage.ReadSector(sectorID: uint64; sector: TStream;
  maxSize: Integer = -1): Boolean;
begin
  Result := ReadConsecutiveSectors(sectorID, sector, maxSize);
end;

function TSDPartitionImage.WriteSector(sectorID: uint64; sector: TStream;
  maxSize: Integer = -1): Boolean;
begin
  Result := WriteConsecutiveSectors(sectorID, sector, maxSize);
end;

function TSDPartitionImage.CopySector(srcSectorID: uint64; destSectorID: uint64): Boolean;
var
  allOK:     Boolean;
  tmpSector: TSDUMemoryStream;
begin
  allOK := False;

  tmpSector := TSDUMemoryStream.Create();
  try
    tmpSector.Position := 0;
    if ReadSector(srcSectorID, tmpSector) then begin
      tmpSector.Position := 0;
      allOK              := WriteSector(srcSectorID, tmpSector);
    end;
  finally
    tmpSector.Free();
  end;

  Result := allOK;
end;

end.
