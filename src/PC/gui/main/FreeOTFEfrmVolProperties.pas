unit FreeOTFEfrmVolProperties;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.FreeOTFE.org/
//
// -----------------------------------------------------------------------------
//


interface                          

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  OTFEFreeOTFEBase_U,
  OTFEFreeOTFE_U,
  SDUForms;

type
  TfrmFreeOTFEVolProperties = class(TSDUForm)
    pbOK: TButton;
    Label15: TLabel;
    Label22: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    edDrive: TEdit;
    edDeviceName: TEdit;
    edVolumeFile: TEdit;
    edReadOnly: TEdit;
    edMainCypher: TEdit;
    pbInfoMainCypher: TButton;
    pbInfoIVHash: TButton;
    lblIVHash: TLabel;
    edIVHash: TEdit;
    Label2: TLabel;
    edSectorIVGenMethod: TEdit;
    pbInfoIVCypher: TButton;
    edIVCypher: TEdit;
    lblIVCypher: TLabel;
    Label1: TLabel;
    edHiddenOffset: TEdit;
    procedure FormShow(Sender: TObject);
    procedure pbInfoIVHashClick(Sender: TObject);
    procedure pbInfoMainCypherClick(Sender: TObject);
    procedure pbInfoIVCypherClick(Sender: TObject);
  private
    function GetHiddenOffset(): Int64;
    { Private declarations }
  public
    DriveLetter: char;
    OTFEFreeOTFE: TOTFEFreeOTFE;
  end;

implementation

{$R *.DFM}


uses
  ActiveX,  // Required for IsEqualGUID
  ComObj,  // Required for StringToGUID
  SDUi18n,
  SDUDialogs,
  SDUGeneral,
  OTFEFreeOTFE_DriverAPI;  // Required for NULL_GUID


procedure TfrmFreeOTFEVolProperties.FormShow(Sender: TObject);
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
  hashDetails: TFreeOTFEHash;
  cypherDetails: TFreeOTFECypher_v3;
begin
  edDrive.text := DriveLetter+':';

  edVolumeFile.text := RS_UNKNOWN;
  edReadOnly.text   := RS_UNKNOWN;
  edIVHash.text     := RS_UNKNOWN;
  edIVCypher.text   := RS_UNKNOWN;
  edMainCypher.text := RS_UNKNOWN;
  edDeviceName.text := RS_UNKNOWN;
  edSectorIVGenMethod.Text := RS_UNKNOWN;

  lblIVHash.Enabled    := FALSE;
  edIVHash.Enabled     := FALSE;
  pbInfoIVHash.Enabled := FALSE;
  
  lblIVCypher.Enabled    := FALSE;
  edIVCypher.Enabled     := FALSE;
  pbInfoIVCypher.Enabled := FALSE;


  if OTFEFreeOTFE.GetVolumeInfo(DriveLetter, volumeInfo) then
    begin
    edVolumeFile.text := volumeInfo.Filename;
    if volumeInfo.ReadOnly then
      begin
      edReadOnly.text := _('Readonly');
      end
    else
      begin
      edReadOnly.text := _('Read/write');
      end;


    edSectorIVGenMethod.Text := FreeOTFESectorIVGenMethodTitle[volumeInfo.SectorIVGenMethod];


    if (
        (volumeInfo.IVHashDevice = '') and
        IsEqualGUID(volumeInfo.IVHashGUID, StringToGUID(NULL_GUID))
       ) then
      begin
      edIVHash.Text := _('n/a');
      end
    else
      begin
      lblIVHash.Enabled    := TRUE;
      edIVHash.Enabled     := TRUE;
      pbInfoIVHash.Enabled := TRUE;

      if OTFEFreeOTFE.GetSpecificHashDetails(
                                             volumeInfo.IVHashDevice,
                                             volumeInfo.IVHashGUID,
                                             hashDetails
                                            ) then
        begin
        edIVHash.Text := OTFEFreeOTFE.GetHashDisplayTitle(hashDetails);
        end;

      end;



    if (
        (volumeInfo.IVCypherDevice = '') and
        IsEqualGUID(volumeInfo.IVCypherGUID, StringToGUID(NULL_GUID))
       ) then
      begin
      edIVCypher.Text := _('n/a');
      end
    else
      begin
      lblIVCypher.Enabled    := TRUE;
      edIVCypher.Enabled     := TRUE;
      pbInfoIVCypher.Enabled := TRUE;

      if OTFEFreeOTFE.GetSpecificCypherDetails(
                                             volumeInfo.IVCypherDevice,
                                             volumeInfo.IVCypherGUID,
                                             cypherDetails
                                            ) then
        begin
        edIVCypher.Text := OTFEFreeOTFE.GetCypherDisplayTitle(cypherDetails);
        end;

      end;



    if OTFEFreeOTFE.GetSpecificCypherDetails(
                                             volumeInfo.MainCypherDevice,
                                             volumeInfo.MainCypherGUID,
                                             cypherDetails
                                            ) then
      begin
      edMainCypher.Text := OTFEFreeOTFE.GetCypherDisplayTitle(cypherDetails);
      end;

    edDeviceName.text := volumeInfo.DeviceName;

    edHiddenOffset.Text := IntToStr( GetHiddenOffset());
    end
  else
    begin
    SDUMessageDlg(
                  SDUParamSubstitute(_('Unable to get drive properties for drive %1:'), [DriveLetter]),
                  mtError
                 );
    end;

end;


procedure TfrmFreeOTFEVolProperties.pbInfoIVHashClick(Sender: TObject);
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  if OTFEFreeOTFE.GetVolumeInfo(DriveLetter, volumeInfo) then
    begin
    OTFEFreeOTFE.ShowHashDetailsDlg(volumeInfo.IVHashDevice, volumeInfo.IVHashGUID);
    end;

end;

procedure TfrmFreeOTFEVolProperties.pbInfoMainCypherClick(Sender: TObject);
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  if OTFEFreeOTFE.GetVolumeInfo(DriveLetter, volumeInfo) then
    begin
    OTFEFreeOTFE.ShowCypherDetailsDlg(volumeInfo.MainCypherDevice, volumeInfo.MainCypherGUID);
    end;

end;

procedure TfrmFreeOTFEVolProperties.pbInfoIVCypherClick(Sender: TObject);
var
  volumeInfo: TOTFEFreeOTFEVolumeInfo;
begin
  if OTFEFreeOTFE.GetVolumeInfo(DriveLetter, volumeInfo) then
    begin
    OTFEFreeOTFE.ShowCypherDetailsDlg(volumeInfo.IVCypherDevice, volumeInfo.IVCypherGUID);
    end;

end;

//todo: put in library - to keep gui separate
function TfrmFreeOTFEVolProperties.GetHiddenOffset(): Int64;
var
  Total, Avail, Used: Int64;
  Disk :Byte;
begin
  Disk  := Byte( DriveLetter) - $40;
  Total:=DiskSize(Disk) ;
  Avail:=DiskFree(Disk) ;
  Used := Total - Avail;
  { algorithm is : truncate to whole Mbs, add 2 Mb (i.e. round up to nearest Mb and add 1)
    and convert back to bytes
    must match that in common.sh linux script }
  result := ((Used div (1024 * 1024)) +2) * 1024  * 1024  ;
end;


END.

