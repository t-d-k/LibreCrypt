unit OTFEFreeOTFE_frmHashInfo;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U,
  SDUForms;

type
  TfrmHashInfo = class(TSDUForm)
    gbHashDriver: TGroupBox;
    lblDeviceName: TLabel;
    lblDeviceUserModeName: TLabel;
    lblDeviceKernelModeName: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    edDriverDeviceName: TEdit;
    edDriverDeviceUserModeName: TEdit;
    edDriverDeviceKernelModeName: TEdit;
    edDriverTitle: TEdit;
    edDriverVersionID: TEdit;
    edDriverHashCount: TEdit;
    gbHash: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edHashGUID: TEdit;
    edHashTitle: TEdit;
    edHashLength: TEdit;
    edHashVersionID: TEdit;
    pbClose: TButton;
    edDriverGUID: TEdit;
    Label9: TLabel;
    edHashBlockSize: TEdit;
    Label12: TLabel;
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    OTFEFreeOTFEObj: TOTFEFreeOTFEBase;

    // These two items uniquely identify which should be shown
    ShowDriverName: string;
    ShowGUID: TGUID;
  end;


implementation

{$R *.DFM}

uses
  SDUi18n,
  ComObj,  // Required for GUIDToString(...)
  ActiveX,  // Required for IsEqualGUID
  SDUGeneral,
  OTFEFreeOTFEDLL_U;

resourcestring
  RS_UNABLE_LOCATE_HASH_DRIVER = '<Unable to locate correct hash driver>';
  RS_UNABLE_LOCATE_HASH        = '<Unable to locate hash>';

procedure TfrmHashInfo.pbCloseClick(Sender: TObject);
begin
  Close();
  
end;

procedure TfrmHashInfo.FormShow(Sender: TObject);
var
  hashDrivers: array of TFreeOTFEHashDriver;
  i, j: integer;
  currHashDriver: TFreeOTFEHashDriver;
  currHash: TFreeOTFEHash;
  tmpString: string;
begin
  // Blank out in case none found
  edDriverGUID.Text                 := RS_UNABLE_LOCATE_HASH_DRIVER;
  edDriverDeviceName.Text           := RS_UNABLE_LOCATE_HASH_DRIVER;
  edDriverDeviceKernelModeName.Text := RS_UNABLE_LOCATE_HASH_DRIVER;
  edDriverDeviceUserModeName.Text   := RS_UNABLE_LOCATE_HASH_DRIVER;
  edDriverTitle.Text                := RS_UNABLE_LOCATE_HASH_DRIVER;
  edDriverVersionID.Text            := RS_UNABLE_LOCATE_HASH_DRIVER;
  edDriverHashCount.Text            := RS_UNABLE_LOCATE_HASH_DRIVER;

  edHashGUID.Text       := RS_UNABLE_LOCATE_HASH;
  edHashTitle.Text      := RS_UNABLE_LOCATE_HASH;
  edHashVersionID.Text  := RS_UNABLE_LOCATE_HASH;
  edHashLength.Text     := RS_UNABLE_LOCATE_HASH;
  edHashBlockSize.Text  := RS_UNABLE_LOCATE_HASH;

  if (OTFEFreeOTFEObj is TOTFEFreeOTFEDLL) then
    begin
    lblDeviceName.caption := _('Library:');

    lblDeviceUserModeName.visible := FALSE;
    edDriverDeviceUserModeName.visible := FALSE;

    lblDeviceKernelModeName.visible := FALSE;
    edDriverDeviceKernelModeName.visible := FALSE;
    end;

  SetLength(hashDrivers, 0);
  if OTFEFreeOTFEObj.GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers)) then
    begin
    for i:=low(hashDrivers) to high(hashDrivers) do
      begin
      currHashDriver := hashDrivers[i];
      
      if (
          (currHashDriver.LibFNOrDevKnlMdeName = ShowDriverName) OR
          (currHashDriver.DeviceUserModeName = ShowDriverName)
         ) then
        begin
        edDriverGUID.Text                 := GUIDToString(currHashDriver.DriverGUID);
        if (OTFEFreeOTFEObj is TOTFEFreeOTFEDLL) then
          begin
          edDriverDeviceName.Text           := currHashDriver.LibFNOrDevKnlMdeName;
          end
        else
          begin
          edDriverDeviceName.Text           := currHashDriver.DeviceName;
          edDriverDeviceKernelModeName.Text := currHashDriver.LibFNOrDevKnlMdeName;
          edDriverDeviceUserModeName.Text   := currHashDriver.DeviceUserModeName;
          end;
        edDriverTitle.Text                := currHashDriver.Title;
        edDriverVersionID.Text            := OTFEFreeOTFEObj.VersionIDToStr(currHashDriver.VersionID);
        edDriverHashCount.Text            := inttostr(currHashDriver.HashCount);

        for j:=low(hashDrivers[i].Hashes) to high(hashDrivers[i].Hashes) do
          begin
          currHash := hashDrivers[i].Hashes[j];

          if (IsEqualGUID(currHash.HashGUID, ShowGUID)) then
            begin
            edHashGUID.Text       := GUIDToString(currHash.HashGUID);
            edHashTitle.Text      := currHash.Title;
            edHashVersionID.Text  := OTFEFreeOTFEObj.VersionIDToStr(currHash.VersionID);

            tmpString := SDUParamSubstitute(COUNT_BITS, [currHash.Length]);
            if (currHash.Length = -1) then
              begin
              tmpString := tmpString + ' '+ _('(length of hash returned may vary)');
              end;
            edHashLength.Text := tmpString;

            tmpString := SDUParamSubstitute(COUNT_BITS, [currHash.BlockSize]);
            if (currHash.BlockSize = -1) then
              begin
              tmpString := tmpString + ' '+ _('(n/a)');
              end;
            edHashBlockSize.Text := tmpString;
            end;

          end;

        end;

      end;

    end;

end;


END.

