unit OTFEFreeOTFE_frmCypherInfo;
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
  TfrmCypherInfo = class(TSDUForm)
    gbCypherDriver: TGroupBox;
    gbCypher: TGroupBox;
    edDriverDeviceName: TEdit;
    lblDeviceName: TLabel;
    lblDeviceUserModeName: TLabel;
    lblDeviceKernelModeName: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edDriverDeviceUserModeName: TEdit;
    edDriverDeviceKernelModeName: TEdit;
    edDriverTitle: TEdit;
    edDriverVersionID: TEdit;
    edDriverCypherCount: TEdit;
    edCypherGUID: TEdit;
    edCypherTitle: TEdit;
    edCypherMode: TEdit;
    edCypherBlockSize: TEdit;
    edCypherVersionID: TEdit;
    pbClose: TButton;
    Label12: TLabel;
    edDriverGUID: TEdit;
    edCypherKeySize: TEdit;
    Label13: TLabel;
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
  OTFEFreeOTFE_DriverCypherAPI,
  OTFEFreeOTFEDLL_U;

resourcestring
  RS_UNABLE_LOCATE_CYPHER_DRIVER = '<Unable to locate correct cypher driver>';
  RS_UNABLE_LOCATE_CYPHER        = '<Unable to locate cypher>';

procedure TfrmCypherInfo.pbCloseClick(Sender: TObject);
begin
  Close();

end;

procedure TfrmCypherInfo.FormShow(Sender: TObject);
var
  cypherDrivers: array of TFreeOTFECypherDriver;
  i, j: integer;
  currCypherDriver: TFreeOTFECypherDriver;
  currCypher: TFreeOTFECypher_v3;
  tmpString: string;
begin
  // Blank out in case none found
  edDriverGUID.Text                 := RS_UNABLE_LOCATE_CYPHER_DRIVER;
  edDriverDeviceName.Text           := RS_UNABLE_LOCATE_CYPHER_DRIVER;
  edDriverDeviceKernelModeName.Text := RS_UNABLE_LOCATE_CYPHER_DRIVER;
  edDriverDeviceUserModeName.Text   := RS_UNABLE_LOCATE_CYPHER_DRIVER;
  edDriverTitle.Text                := RS_UNABLE_LOCATE_CYPHER_DRIVER;
  edDriverVersionID.Text            := RS_UNABLE_LOCATE_CYPHER_DRIVER;
  edDriverCypherCount.Text          := RS_UNABLE_LOCATE_CYPHER_DRIVER;

  edCypherGUID.Text      := RS_UNABLE_LOCATE_CYPHER;
  edCypherTitle.Text     := RS_UNABLE_LOCATE_CYPHER;
  edCypherMode.Text      := RS_UNABLE_LOCATE_CYPHER;
  edCypherVersionID.Text := RS_UNABLE_LOCATE_CYPHER;
  edCypherKeySize.Text   := RS_UNABLE_LOCATE_CYPHER;
  edCypherBlockSize.Text := RS_UNABLE_LOCATE_CYPHER;

  if (OTFEFreeOTFEObj is TOTFEFreeOTFEDLL) then
    begin
    lblDeviceName.caption := _('Library:');

    lblDeviceUserModeName.visible := FALSE;
    edDriverDeviceUserModeName.visible := FALSE;

    lblDeviceKernelModeName.visible := FALSE;
    edDriverDeviceKernelModeName.visible := FALSE;
    end;

  SetLength(cypherDrivers, 0);
  if OTFEFreeOTFEObj.GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers)) then
    begin
    for i:=low(cypherDrivers) to high(cypherDrivers) do
      begin
      currCypherDriver := cypherDrivers[i];

      if (
          (currCypherDriver.LibFNOrDevKnlMdeName = ShowDriverName) OR
          (currCypherDriver.DeviceUserModeName = ShowDriverName)
          ) then
        begin
        edDriverGUID.Text                 := GUIDToString(currCypherDriver.DriverGUID);
        if (OTFEFreeOTFEObj is TOTFEFreeOTFEDLL) then
          begin
          edDriverDeviceName.Text           := currCypherDriver.LibFNOrDevKnlMdeName;
          end
        else
          begin
          edDriverDeviceName.Text           := currCypherDriver.DeviceName;
          edDriverDeviceKernelModeName.Text := currCypherDriver.LibFNOrDevKnlMdeName;
          edDriverDeviceUserModeName.Text   := currCypherDriver.DeviceUserModeName;
          end;
        edDriverTitle.Text                := currCypherDriver.Title;
        edDriverVersionID.Text            := OTFEFreeOTFEObj.VersionIDToStr(currCypherDriver.VersionID);
        edDriverCypherCount.Text          := inttostr(currCypherDriver.CypherCount);


        for j:=low(cypherDrivers[i].Cyphers) to high(cypherDrivers[i].Cyphers) do
          begin
          currCypher := cypherDrivers[i].Cyphers[j];

          if (IsEqualGUID(currCypher.CypherGUID, ShowGUID)) then
            begin
            edCypherGUID.Text      := GUIDToString(currCypher.CypherGUID);
            edCypherTitle.Text     := currCypher.Title;
            edCypherMode.Text      := FreeOTFECypherModeTitle(currCypher.Mode);

            tmpString := SDUParamSubstitute(COUNT_BITS, [currCypher.KeySizeUnderlying]);
            if (currCypher.KeySizeUnderlying = 0) then
              begin
              tmpString := tmpString + ' '+ _('(null key only)');
              end
            else if (currCypher.KeySizeUnderlying = -1) then
              begin
              tmpString := tmpString + ' '+ _('(arbitary keysize allowed)');
              end;
            edCypherKeySize.Text   := tmpString;

            tmpString := SDUParamSubstitute(COUNT_BITS, [currCypher.BlockSize]);
            if (currCypher.BlockSize = -1) then
              begin
              tmpString := tmpString + ' '+ _('(arbitary blocksize allowed)');
              end;
            edCypherBlockSize.Text := tmpString;

            edCypherVersionID.Text := OTFEFreeOTFEObj.VersionIDToStr(currCypher.VersionID);
            end;

          end;

        end;

      end;

    end;

end;


END.

