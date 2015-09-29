unit frmVolProperties;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
//delphi
  Classes, Controls, Dialogs,
  Forms, Graphics, Messages, StdCtrls,
  SysUtils, Windows,
   //sdu, lc utils
  lcTypes,
      OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  SDUForms, SDUGeneral
 //LibreCrypt forms
;

type
  TfrmVolProperties = class (TSDUForm)
    pbOK:           TButton;
    Label15:        TLabel;
    Label22:        TLabel;
    Label5:         TLabel;
    Label7:         TLabel;
    Label6:         TLabel;
    edDrive:        TEdit;
    edDeviceName:   TEdit;
    edVolumeFile:   TEdit;
    edReadOnly:     TEdit;
    edMainCypher:   TEdit;
    pbInfoMainCypher: TButton;
    pbInfoIVHash:   TButton;
    lblIVHash:      TLabel;
    edIVHash:       TEdit;
    Label2:         TLabel;
    edSectorIVGenMethod: TEdit;
    pbInfoIVCypher: TButton;
    edIVCypher:     TEdit;
    lblIVCypher:    TLabel;
    Label1:         TLabel;
    edHiddenOffset: TEdit;
    procedure FormShow(Sender: TObject);
    procedure pbInfoIVHashClick(Sender: TObject);
    procedure pbInfoMainCypherClick(Sender: TObject);
    procedure pbInfoIVCypherClick(Sender: TObject);
  private
    fDriveLetter: DriveLetterChar;


    { Private declarations }
  public
    property DriveLetter: DriveLetterChar Read fDriveLetter Write fDriveLetter;
  end;

    function GetHiddenOffset(DriveLetter: DriveLetterChar): Int64;
implementation

{$R *.DFM}


uses
//delphi
  ActiveX,  // Required for IsEqualGUID
  ComObj,   // Required for StringToGUID

            //sdu
  lcDialogs,
  SDUi18n,  DriverAPI,
  //LibreCrypt forms
  frmCypherInfo, frmHashInfo;  // Required for NULL_GUID


procedure TfrmVolProperties.FormShow(Sender: TObject);
var
  volumeInfo:    TOTFEFreeOTFEVolumeInfo;
  hashDetails:   TFreeOTFEHash;
  cypherDetails: TFreeOTFECypher_v3;
begin
  edDrive.Text := fDriveLetter + ':';

  edVolumeFile.Text        := RS_UNKNOWN;
  edReadOnly.Text          := RS_UNKNOWN;
  edIVHash.Text            := RS_UNKNOWN;
  edIVCypher.Text          := RS_UNKNOWN;
  edMainCypher.Text        := RS_UNKNOWN;
  edDeviceName.Text        := RS_UNKNOWN;
  edSectorIVGenMethod.Text := RS_UNKNOWN;

  lblIVHash.Enabled    := False;
  edIVHash.Enabled     := False;
  pbInfoIVHash.Enabled := False;

  lblIVCypher.Enabled    := False;
  edIVCypher.Enabled     := False;
  pbInfoIVCypher.Enabled := False;


  if GetFreeOTFEBase().GetVolumeInfo(fDriveLetter, volumeInfo) then begin
    edVolumeFile.Text := volumeInfo.Filename;
    if volumeInfo.ReadOnly then begin
      edReadOnly.Text := _('Readonly');
    end else begin
      edReadOnly.Text := _('Read/write');
    end;


    edSectorIVGenMethod.Text := FreeOTFESectorIVGenMethodTitle[volumeInfo.SectorIVGenMethod];


    if ((volumeInfo.IVHashDevice = '') and IsEqualGUID(volumeInfo.IVHashGUID,
      StringToGUID(NULL_GUID))) then begin
      edIVHash.Text := _('n/a');
    end else begin
      lblIVHash.Enabled    := True;
      edIVHash.Enabled     := True;
      pbInfoIVHash.Enabled := True;

      if GetFreeOTFEBase().GetSpecificHashDetails(volumeInfo.IVHashDevice,
        volumeInfo.IVHashGUID, hashDetails) then begin
        edIVHash.Text := GetFreeOTFEBase().GetHashDisplayTitle(hashDetails);
      end;

    end;



    if ((volumeInfo.IVCypherDevice = '') and IsEqualGUID(volumeInfo.IVCypherGUID,
      StringToGUID(NULL_GUID))) then begin
      edIVCypher.Text := _('n/a');
    end else begin
      lblIVCypher.Enabled    := True;
      edIVCypher.Enabled     := True;
      pbInfoIVCypher.Enabled := True;

      if GetFreeOTFEBase().GetSpecificCypherDetails(volumeInfo.IVCypherDevice,
        volumeInfo.IVCypherGUID, cypherDetails) then begin
        edIVCypher.Text := GetFreeOTFEBase().GetCypherDisplayTitle(cypherDetails);
      end;

    end;



    if GetFreeOTFEBase().GetSpecificCypherDetails(volumeInfo.MainCypherDevice,
      volumeInfo.MainCypherGUID, cypherDetails) then begin
      edMainCypher.Text := GetFreeOTFEBase().GetCypherDisplayTitle(cypherDetails);
    end;

    edDeviceName.Text := volumeInfo.DeviceName;

    edHiddenOffset.Text :=  IntToStrWithCommas(GetHiddenOffset(fDriveLetter));
  end else begin
    SDUMessageDlg(
      Format(_('Unable to get drive properties for drive %s:'), [fDriveLetter]),
      mtError
      );
  end;

end;


procedure TfrmVolProperties.pbInfoIVHashClick(Sender: TObject);
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  if GetFreeOTFEBase().GetVolumeInfo(fDriveLetter, volumeInfo) then begin
    frmHashInfo.ShowHashDetailsDlg(volumeInfo.IVHashDevice, volumeInfo.IVHashGUID);
  end;

end;

procedure TfrmVolProperties.pbInfoMainCypherClick(Sender: TObject);
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  if GetFreeOTFEBase().GetVolumeInfo(fDriveLetter, volumeInfo) then begin
    frmCypherInfo.ShowCypherDetailsDlg(volumeInfo.MainCypherDevice, volumeInfo.MainCypherGUID);
  end;

end;

procedure TfrmVolProperties.pbInfoIVCypherClick(Sender: TObject);
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  if GetFreeOTFEBase().GetVolumeInfo(fDriveLetter, volumeInfo) then begin
    frmCypherInfo.ShowCypherDetailsDlg(volumeInfo.IVCypherDevice, volumeInfo.IVCypherGUID);
  end;

end;

//todo: put in other unit - to keep gui separate
function GetHiddenOffset(DriveLetter: DriveLetterChar): Int64;
var
  Total, Avail, Used: Int64;
  Disk:               Byte;
begin
  Disk   := Byte(DriveLetter) - $40;
  Total  := DiskSize(Disk);
  Avail  := DiskFree(Disk);
  Used   := Total - Avail;
  { algorithm is : truncate to whole Mbs, add 2 Mb (i.e. round up to nearest Mb and add 1)
    and convert back to bytes
    must match that in common.sh linux script }
  Result := ((Used div (1024 * 1024)) + 2) * 1024 * 1024;
end;


end.
