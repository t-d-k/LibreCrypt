unit SDPartitionImage;

interface

uses
  SysUtils, Classes, SyncObjs, Windows,
  SDUGeneral;

const
  DEFAULT_SECTOR_SIZE = 512;
  
type
  EPartitionError = class(Exception);
  EPartitionNotMounted = class(EPartitionError);

{$M+}  // Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"
  TSDPartitionImage = class
  protected
    FMounted: boolean;
    FSectorSize: integer;
    FReadOnly: boolean;

    FSerializeCS: TCriticalSection; // Protect by serializing read/write
                                    // operations in multithreaded operations

    procedure SetMounted(newMounted: boolean);
    procedure AssertMounted();

    function  DoMount(): boolean; virtual;
    procedure DoDismount(); virtual;

    function  GetSize(): ULONGLONG; virtual; abstract;

  public
    constructor Create(); virtual;
    destructor  Destroy(); override;

    function  ReadSector(sectorID: uint64; sector: TStream; maxSize: integer = -1): boolean; virtual;
    function  WriteSector(sectorID: uint64; sector: TStream; maxSize: integer = -1): boolean; virtual;
    function  ReadConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean; virtual; abstract;
    function  WriteConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean; virtual; abstract;

    function  CopySector(srcSectorID: uint64; destSectorID: uint64): boolean;

  published
    property  Mounted: boolean read FMounted write SetMounted;
    property  ReadOnly: boolean read FReadOnly write FReadOnly;
    property  Size: ULONGLONG read GetSize;
    property  SectorSize: integer read FSectorSize write FSectorSize;

  end;

implementation

uses
  SDUClasses;

constructor TSDPartitionImage.Create();
begin
  inherited;
  FMounted := FALSE;
  FSectorSize := DEFAULT_SECTOR_SIZE;
  FSerializeCS:= TCriticalSection.Create();
end;

destructor TSDPartitionImage.Destroy();
begin
  Mounted := FALSE;
  FSerializeCS.Free();
  inherited;
end;

function TSDPartitionImage.DoMount(): boolean;
begin
  FSerializeCS:= TCriticalSection.Create();
  Result := TRUE;
end;

procedure TSDPartitionImage.DoDismount();
begin
  FSerializeCS.Free();
end;

procedure TSDPartitionImage.SetMounted(newMounted: boolean);
begin
  inherited;
  if (newMounted <> Mounted) then
    begin
    if newMounted then
      begin
      FMounted := DoMount();
      end
    else
      begin
      DoDismount();
      FMounted := FALSE;
      end;

    end;
end;

procedure TSDPartitionImage.AssertMounted();
begin
  inherited;

  if not(Mounted) then
    begin
    raise EPartitionNotMounted.Create('Partition not mounted');
    end;
end;


function TSDPartitionImage.ReadSector(sectorID: uint64; sector: TStream; maxSize: integer = -1): boolean;
begin
  Result := ReadConsecutiveSectors(sectorID, sector, maxSize);
end;

function TSDPartitionImage.WriteSector(sectorID: uint64; sector: TStream; maxSize: integer = -1): boolean;
begin
  Result := WriteConsecutiveSectors(sectorID, sector, maxSize);
end;

function TSDPartitionImage.CopySector(srcSectorID: uint64; destSectorID: uint64): boolean;
var
  allOK: boolean;
  tmpSector: TSDUMemoryStream;
begin
  allOK := FALSE;

  tmpSector:= TSDUMemoryStream.Create();
  try
    tmpSector.Position := 0;
    if ReadSector(srcSectorID, tmpSector) then
      begin
      tmpSector.Position := 0;
      allOK := WriteSector(srcSectorID, tmpSector);
      end;
  finally
    tmpSector.Free();
  end;

  Result := allOK;
end;

END.

