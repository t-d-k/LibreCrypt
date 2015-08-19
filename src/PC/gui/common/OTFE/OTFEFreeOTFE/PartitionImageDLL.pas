unit PartitionImageDLL;

interface

uses
 //delphi / libs
  Classes,
   Windows,
  //sdu & LibreCrypt utils
        lcTypes,
   OTFEFreeOTFEDLL_U,
  SDPartitionImage
   // LibreCrypt forms

;

type
  TPartitionImageDLL = class (TSDPartitionImage)
  protected
    FFilename:       Ansistring;
    FSize:           ULONGLONG;
    fMountedAs:      DriveLetterChar;
    FBytesPerSector: Integer;

    FFOTFEMountedOnPartitionMount: Boolean;
  public

    constructor Create(); override;
    destructor Destroy(); override;

    function DoMount(): Boolean; override;
    procedure DoDismount(); override;

    function GetSize(): ULONGLONG; override;

    // maxSize - Size of data to transfer, in *bytes*, not sectors
    function ReadConsecutiveSectors(startSectorID: uint64; sectors: TStream;
      maxSize: Integer = -1): Boolean; override;
    // maxSize - Size of data to transfer, in *bytes*, not sectors
    function WriteConsecutiveSectors(startSectorID: uint64; sectors: TStream;
      maxSize: Integer = -1): Boolean; override;
  published
    property Filename: Ansistring Read FFilename Write FFilename;
    property MountedAs: DriveLetterChar Read fMountedAs Write FMountedAs;
  end;

function PostMount_Format_using_dll(drive: DriveLetterChar): Boolean;

implementation

uses
 //delphi / libs
    Math,
  //sdu & LibreCrypt utils
      sduGeneral,
  PartitionTools,
  OTFEFreeOTFEBase_U, SDFilesystem_FAT, SDUClasses,
   // LibreCrypt forms
   frmSelectVolumeType


;

constructor TPartitionImageDLL.Create();
begin
  inherited;
  FMountedAs := #0;
  FSize      := 0;
end;

destructor TPartitionImageDLL.Destroy();
begin
  //lplp - TO IMPLEMENT
  inherited;
end;

function TPartitionImageDLL.DoMount(): Boolean;
var
  diskGeometry: TSDUDiskGeometry;
  FreeOTFEObj:  TOTFEFreeOTFEDLL;
begin
  FreeOTFEObj := TOTFEFreeOTFEDLL(GetFreeOTFEBase());

  Result := False;

  FFOTFEMountedOnPartitionMount := True;
  if (FMountedAs = #0) then begin
    FFOTFEMountedOnPartitionMount := False;
    FMountedAs                    := frmSelectVolumeType.Mount(fFilename);
  end;

  if (FMountedAs = #0) then begin
    FBytesPerSector := 0;
  end else begin
    if FreeOTFEObj.GetDiskGeometry(FMountedAs, diskGeometry) then begin
      FBytesPerSector := diskGeometry.BytesPerSector;
      FSize           := (diskGeometry.Cylinders.QuadPart *
        diskGeometry.TracksPerCylinder * diskGeometry.SectorsPerTrack *
        diskGeometry.BytesPerSector);

      Result := True;
    end else begin
      // Dismount, but otherwise do nothing; Result already set to FALSE
      DoDismount();
    end;
  end;

end;

procedure TPartitionImageDLL.DoDismount();
begin
  // If this object mounted the FreeOTFE volume when this object was mounted,
  // this object dismounts the volume
  // This is so that if the volume was mounted by the *caller* (not this
  // object), mounted as a partition, dismounted as a partition, then it
  // FreeOTFE keeps the volume mounted - it's for the *caller* to dismount it, not us
  if not (FFOTFEMountedOnPartitionMount) then
    TOTFEFreeOTFEDLL(GetFreeOTFEBase()).Dismount(FMountedAs);

  FMountedAs := #0;
end;

// maxSize - Size of data to transfer, in *bytes*, not sectors
function TPartitionImageDLL.ReadConsecutiveSectors(startSectorID: uint64;
  sectors: TStream; maxSize: Integer = -1): Boolean;
begin
  // Short circuit...
  if (maxSize = 0) then begin
    Result := True;
    exit;
  end;

  if (maxSize <= 0) then
    maxSize := FBytesPerSector;


  Result := TOTFEFreeOTFEDLL(GetFreeOTFEBase()).ReadData_Bytes(
    FMountedAs, (startSectorID * FBytesPerSector), maxSize, sectors);

end;

// maxSize - Size of data to transfer, in *bytes*, not sectors
function TPartitionImageDLL.WriteConsecutiveSectors(startSectorID: uint64;
  sectors: TStream; maxSize: Integer = -1): Boolean;
begin
  // Short circuit...
  if (maxSize = 0) then begin
    Result := True;
    exit;
  end;

  if (maxSize <= 0) then
    maxSize := FBytesPerSector;

  Result := TOTFEFreeOTFEDLL(GetFreeOTFEBase()).WriteData_Bytes(
    FMountedAs, (startSectorID * FBytesPerSector), maxSize, sectors);

end;

function TPartitionImageDLL.GetSize(): ULONGLONG;
begin
  Result := FSize;
end;


// Format the volume specified
function PostMount_Format_using_dll(drive: DriveLetterChar): Boolean;
var
  PartitionImage: TPartitionImageDLL;
  Filesystem:     TSDFilesystem_FAT;
begin
  Result := False;
  if (GetFreeOTFEBase() is TOTFEFreeOTFEDLL) then begin
    Result         := True;
    PartitionImage := TPartitionImageDLL.Create();
    try
      PartitionImage.MountedAs := drive;
      // end else begin
      //   //  TODO test this   - could reformat wrong drive! -for now use format_drive for non dll volumes
      //
      //    PartitionImage := TSDPartitionImage_File.Create();
      ////    PartitionImage.GetFreeOTFEBase() := fFreeOTFEObj;
      //      (PartitionImage as TSDPartitionImage_File).Filename := VolFilename;
      //
      //    end;

      PartitionImage.Mounted := True;
      if not PartitionImage.Mounted then begin
        Result := False;
      end else begin
        Filesystem := TSDFilesystem_FAT.Create();
        try
          Filesystem.PartitionImage := PartitionImage;
          Result                    := Filesystem.FormatFS();
          PartitionImage.Mounted    := False;
        finally
          Filesystem.Free;
        end;
      end;
    finally
      PartitionImage.Free;
    end;
  end;
end;

end.
