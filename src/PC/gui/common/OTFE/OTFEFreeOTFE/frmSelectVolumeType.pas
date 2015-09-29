unit frmSelectVolumeType;
// Description:
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
  {choose container type if can't be otherwise detected (either plain dm-crypt or FreeOTFE)}

interface

uses
     //delphi & libs
       Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  //sdu & LibreCrypt utils
        SDUForms ,lcTypes
   // LibreCrypt forms
   ;

type
  TfrmSelectVolumeType = class(TSDUForm)
    rgVolumeType: TRadioGroup;
    pbCancel: TButton;
    pbOK: TButton;
    Label1: TLabel;
  private
    { Private declarations }
  public
    // Returns TRUE only if the user selected FreeOTFE
    function IsFreeOTFEVolume(): boolean;
    // Returns TRUE only if the user selected Linux
    function IsLinuxVolume(): boolean;
  end;


    // Prompt the user for a password (and drive letter if necessary), then
    // mount the specified volume file
    // Returns the drive letter of the mounted volume on success, #0 on failure
//    function  Mount(volumeFilename: ansistring; readonly: boolean = FALSE): char; overload; virtual; abstract;

    // As previous Mount, but more than one volumes is specified. Volumes are
    // mounted using the same password
    // Sets mountedAs to the drive letters of the mounted volumes, in order.
    // Volumes that could not be mounted have #0 as their drive letter
    // Returns TRUE if any of the volumes mounted correctly, otherwise FALSE
//    function  Mount(volumeFilename: string; var mountedAs: DriveLetterChar; readonly: boolean = FALSE): boolean; overload; virtual; abstract;

    // Example:
    //   Set:
    //     volumeFilenames[0] = c:\test0.dat
    //     volumeFilenames[1] = c:\test1.dat
    //     volumeFilenames[2] = c:\test2.dat
    //   Call Mount described above in which:
    //     volume test0.dat was sucessfully mounted as W:
    //     volume test1.dat failed to mount
    //     volume test2.dat was sucessfully mounted as X:
    //   Then this function should set:
    //     mountedAs = 'W.X' (where '.' is #0)
    function Mount(volumeFilename: Ansistring; ReadOnly: Boolean = False): Char;
      overload;
    function Mount(volumeFilename: String;
      var mountedAs: DriveLetterChar;
      ReadOnly: Boolean = False): TMountResult; overload;


    // Prompt the user for a device (if appropriate) and password (and drive
    // letter if necessary), then mount the device selected
    // Returns the drive letter of the mounted devices on success, #0 on failure
     function MountDevices(): DriveLetterChar;

implementation

{$R *.DFM}

uses
     //delphi & libs

  //sdu & LibreCrypt utils
    OTFEFreeOTFEBase_U ,OTFEConsts_U,LUKSTools,
   // LibreCrypt forms
 frmKeyEntryFreeOTFE, frmKeyEntryLinux,frmSelectPartition;

// Returns TRUE only if the user selected FreeOTFE
function TfrmSelectVolumeType.IsFreeOTFEVolume(): boolean;
begin
  Result := (rgVolumeType.ItemIndex = 0);

end;


// Returns TRUE only if the user selected Linux
function TfrmSelectVolumeType.IsLinuxVolume(): boolean;
begin
  Result := (rgVolumeType.ItemIndex = 1);

end;



function Mount(volumeFilename: String; var mountedAs: DriveLetterChar;
  ReadOnly: Boolean = False): TMountResult;
var
  frmSelectVolumeType: TfrmSelectVolumeType;
  mr:                  Integer;
begin
  Result := morFail;

  GetFreeOTFEBase().CheckActive();

  frmSelectVolumeType := TfrmSelectVolumeType.Create(nil);
  try
    mr := frmSelectVolumeType.ShowModal();
    if (mr = mrCancel) then begin
//      GetFreeOTFEBase().LastErrorCode := OTFE_ERR_USER_CANCEL;
      Result := morCancel;
    end else
    if (mr = mrOk) then begin
      if frmSelectVolumeType.IsFreeOTFEVolume then begin
        Result := frmKeyEntryFreeOTFE.MountFreeOTFE(volumeFilename, mountedAs, ReadOnly);
      end else begin
        Result := frmKeyEntryLinux.MountPlainLinux(volumeFilename, mountedAs, ReadOnly);
      end;

    end;

  finally
    frmSelectVolumeType.Free();
  end;

end;
function Mount(volumeFilename: Ansistring; ReadOnly: Boolean = False): Char;
var
  mountedAs: DriveLetterChar;
begin
  Result := #0;

  if Mount(volumeFilename, mountedAs, ReadOnly) = morOK then
    Result := mountedAs;
end;


 // -----------------------------------------------------------------------------
 // Prompt the user for a device (if appropriate) and password (and drive
 // letter if necessary), then mount the device selected
 // Returns the drive letter of the mounted devices on success, #0 on failure
function MountDevices(): DriveLetterChar;
var
  selectedPartition:   String;
  //  mountedAs:           DriveLetterChar;
  frmSelectVolumeType: TfrmSelectVolumeType;
  mr:                  Integer;
begin
  //  mountedAs := '';
  Result := #0;
  GetFreeOTFEBase().CheckActive();

  if GetFreeOTFEBase().CanMountDevice() then begin
    // Ask the user if they want to mount their partitions as FreeOTFE or Linux
    // partitions
    frmSelectVolumeType := TfrmSelectVolumeType.Create(nil);
    try
      mr := frmSelectVolumeType.ShowModal();
      if (mr = mrCancel) then begin
//        GetFreeOTFEBase().LastErrorCode := OTFE_ERR_USER_CANCEL;
      end else
      if (mr = mrOk) then begin
        // Ask the user which partition they want to mount
        selectedPartition := frmSelectPartition.SelectPartition();
        if (selectedPartition = '') then begin
//          GetFreeOTFEBase().LastErrorCode := OTFE_ERR_USER_CANCEL;
        end else begin
          // Mount the partition as the appropriate type
          if frmSelectVolumeType.IsFreeOTFEVolume then begin
            Result := MountFreeOTFE(selectedPartition, False);
          end else begin
            if LUKSTools.IsLUKSVolume(selectedPartition) then begin
               LUKSTools.MountLUKS(selectedPartition, Result)
            end else begin
              if frmKeyEntryLinux.MountPlainLinux(selectedPartition, Result)<> morOK then
                Result := #0;
            end;
          end;
        end;

      end;

    finally
      frmSelectVolumeType.Free();
    end;

  end;

end;


END.


