unit OTFEFreeOTFEDLL_PartitionImage;

interface

uses
  Classes, Windows,
  OTFEFreeOTFEDLL_U,
  SDPartitionImage,
  SDUGeneral;

type
  TOTFEFreeOTFEDLL_PartitionImage = class(TSDPartitionImage)
  protected
    FFilename: Ansistring;
    FSize: ULONGLONG;
    fMountedAs: char;
    FBytesPerSector: integer;

    FFOTFEMountedOnPartitionMount: boolean;
  public
    FreeOTFEObj: TOTFEFreeOTFEDLL;

    constructor Create(); override;
    destructor  Destroy(); override;

    function  DoMount(): boolean; override;
    procedure DoDismount(); override;

    function  GetSize(): ULONGLONG; override;

    // maxSize - Size of data to transfer, in *bytes*, not sectors
    function  ReadConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean; override;
    // maxSize - Size of data to transfer, in *bytes*, not sectors
    function  WriteConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean; override;
  published
    property  Filename: Ansistring read FFilename write FFilename;
    property  MountedAs: char read fMountedAs write FMountedAs;
  end;

implementation

uses
  Math,
  SDUClasses;

constructor TOTFEFreeOTFEDLL_PartitionImage.Create();
begin
  inherited;
  FMountedAs := #0;
  FSize := 0;
end;

destructor TOTFEFreeOTFEDLL_PartitionImage.Destroy();
begin
  //lplp - TO IMPLEMENT
  inherited;
end;

function TOTFEFreeOTFEDLL_PartitionImage.DoMount(): boolean;
var
  diskGeometry: TSDUDiskGeometry;
begin
  Result := FALSE;

  FFOTFEMountedOnPartitionMount := TRUE;
  if (FMountedAs = #0) then
    begin
    FFOTFEMountedOnPartitionMount := FALSE;
    FMountedAs := FreeOTFEObj.Mount(Filename);
    end;
    
  if (FMountedAs = #0) then
    begin
    FBytesPerSector := 0;
    end
  else
    begin
    if FreeOTFEObj.GetDiskGeometry(FMountedAs, diskGeometry) then
      begin
      FBytesPerSector := diskGeometry.BytesPerSector;
      FSize := (
                diskGeometry.Cylinders.QuadPart *
                diskGeometry.TracksPerCylinder *
                diskGeometry.SectorsPerTrack *
                diskGeometry.BytesPerSector
               );
               
      Result := TRUE;
      end
    else
      begin
      // Dismount, but otherwise do nothing; Result already set to FALSE
      DoDismount();
      end;
    end;


end;

procedure TOTFEFreeOTFEDLL_PartitionImage.DoDismount();
begin
  // If this object mounted the FreeOTFE volume when this object was mounted,
  // this object dismounts the volume
  // This is so that if the volume was mounted by the *caller* (not this
  // object), mounted as a partition, dismounted as a partition, then it
  // FreeOTFE keeps the volume mounted - it's for the *caller* to dismount it, not us
  if not(FFOTFEMountedOnPartitionMount) then
    begin
    FreeOTFEObj.Dismount(FMountedAs);
    end;
    
  FMountedAs := #0;
end;

// maxSize - Size of data to transfer, in *bytes*, not sectors
function TOTFEFreeOTFEDLL_PartitionImage.ReadConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean;
begin
  // Short circuit...
  if (maxSize = 0) then
    begin
    Result := TRUE;
    exit;
    end;

  if (maxSize <= 0) then
    begin
    maxSize := FBytesPerSector;
    end;

  Result := FreeOTFEObj.ReadData_Bytes(
                                       FMountedAs,
                                       (startSectorID * FBytesPerSector),
                                       maxSize,
                                       sectors
                                      );


end;

// maxSize - Size of data to transfer, in *bytes*, not sectors
function TOTFEFreeOTFEDLL_PartitionImage.WriteConsecutiveSectors(startSectorID: uint64; sectors: TStream; maxSize: integer = -1): boolean;
begin
  // Short circuit...
  if (maxSize = 0) then
    begin
    Result := TRUE;
    exit;
    end;

  if (maxSize <= 0) then
    begin
    maxSize := FBytesPerSector;
    end;

  Result := FreeOTFEObj.WriteData_Bytes(
                                       FMountedAs,
                                       (startSectorID * FBytesPerSector),
                                       maxSize,
                                       sectors
                                      );


end;

function TOTFEFreeOTFEDLL_PartitionImage.GetSize(): ULONGLONG;
begin
  Result := FSize;
end;

END.

