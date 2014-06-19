unit CriticalBlockTest_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// Enabled compiler define: FREEOTFE_DEBUG if needed


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEBase_U, Spin64;

type
  TCriticalBlockTest_F = class(TForm)
    pbClose: TButton;
    reReport: TRichEdit;
    OpenDialog1: TOpenDialog;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    edFilename: TEdit;
    edUserPassword: TEdit;
    pbBrowse: TButton;
    pcSpecific: TPageControl;
    tsWrite: TTabSheet;
    tsRead: TTabSheet;
    Label6: TLabel;
    Label7: TLabel;
    Label10: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    sePartitionLength: TSpinEdit64;
    seVolumeFlags: TSpinEdit64;
    edMasterKey: TEdit;
    edHashDriver: TEdit;
    edHashGUID: TEdit;
    edCypherDriver: TEdit;
    edCypherGUID: TEdit;
    edRandomData: TEdit;
    edSalt: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    seSaltLength: TSpinEdit64;
    Label16: TLabel;
    pbCreateFile: TButton;
    pbWriteCriticalData: TButton;
    pbReadCriticalData: TButton;
    pbClear: TButton;
    edDriveLetter: TEdit;
    Label18: TLabel;
    Label19: TLabel;
    pbReadRawData: TButton;
    se64Offset: TSpinEdit64;
    pbWriteRawData: TButton;
    seKeyIterations: TSpinEdit64;
    Label20: TLabel;
    edVolumeIV: TEdit;
    Label21: TLabel;
    procedure pbReadCriticalDataClick(Sender: TObject);
    procedure pbWriteCriticalDataClick(Sender: TObject);
    procedure pbCreateFileClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure pbReadRawDataClick(Sender: TObject);
    procedure pbWriteRawDataClick(Sender: TObject);
  private
    { Private declarations }
  public
    OTFEFreeOTFE1: TOTFEFreeOTFEBase;
  end;


implementation

{$R *.DFM}


uses
  ComObj,  // Required for StringToGUID
  SDUGeneral,
  OTFEFreeOTFE_VolumeFileAPI,
  OTFEFreeOTFE_DriverAPI;


procedure TCriticalBlockTest_F.pbReadCriticalDataClick(Sender: TObject);
var
  dumpStrings: TStringList;
  dumpFilename: string;
begin
  dumpFilename := 'C:\VolumeCDBDump.txt';
  if MessageDlg(
                '!!! WARNING !!!'+SDUCRLF+
                SDUCRLF+
                'Due to the way in which this test is implemented, a PLAINTEXT '+SDUCRLF+
                'version of your CDB''s contents dumped to a temporary file '+SDUCRLF+
                'before being read back in and the temporary file deleted.'+SDUCRLF+
                SDUCRLF+
                'Do you wish to proceed?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
    if OTFEFreeOTFE1.DumpCriticalDataToFile(
                               edFilename.text,
                               se64Offset.Value,
                               edUserPassword.text,
                               seSaltLength.value,
                               seKeyIterations.value,
                               dumpFilename
                              ) then
      begin
      dumpStrings := TStringList.Create();
      try
        dumpStrings.LoadFromFile(dumpFilename);
        reReport.lines.AddStrings(dumpStrings);
      finally
        dumpStrings.Free();
      end;
      
      DeleteFile(dumpFilename);
      end
    else
      begin
      reReport.lines.add('Dump FAILED (Dump attempted to '+dumpFilename+')');
      end;

    end;

end;


procedure TCriticalBlockTest_F.pbWriteCriticalDataClick(
  Sender: TObject);
var
  volumeDetails: TVolumeDetailsBlock;
  CDBMetaData: TCDBMetaData;
begin
  // These are ignored; populated automatically on writing
  CDBMetaData.MACAlgorithm:= fomacUnknown;
  CDBMetaData.KDFAlgorithm:= fokdfUnknown;

  CDBMetaData.HashDriver   := edHashDriver.text;
  CDBMetaData.HashGUID     := StringToGUID(edHashGUID.text);
  CDBMetaData.CypherDriver := edCypherDriver.text;
  CDBMetaData.CypherGUID   := StringToGUID(edCypherGUID.text);

  
  // This is ignored; populated automatically on writing
  volumeDetails.CDBFormatID:= 0;

  volumeDetails.PartitionLen:= sePartitionLength.value;
  volumeDetails.VolumeFlags:= seVolumeFlags.value;
  volumeDetails.MasterKeyLength:= (Length(edMasterKey.Text)*8);
  volumeDetails.MasterKey:= edMasterKey.Text;
  volumeDetails.VolumeIVLength:= (Length(edVolumeIV.Text)*8);
  volumeDetails.VolumeIV:= edVolumeIV.Text;
  volumeDetails.RequestedDriveLetter := #0;
  if (edDriveLetter.text <> '') then
    begin
    volumeDetails.RequestedDriveLetter := edDriveLetter.text[1];
    end;


  if OTFEFreeOTFE1.WriteVolumeCriticalData(
                                           edFilename.text,
                                           se64Offset.Value,
                                           edUserPassword.text,
                                           edSalt.Text,
                                           seKeyIterations.Value,
                                           volumeDetails,
                                           CDBMetaData,
                                           edRandomData.Text
                                          ) then
    begin
    showmessage('Written OK');
    end
  else
    begin
    showmessage('Write FAILED.');
    end;

end;

procedure TCriticalBlockTest_F.pbCreateFileClick(Sender: TObject);
var
  userCancel: boolean;
begin
  if SDUCreateLargeFile(edFilename.text, sePartitionLength.value + (CRITICAL_DATA_LENGTH div 8), FALSE, userCancel) then
    begin
    showmessage('File created OK');
    end
  else
    begin
    showmessage('File create FAILED.');
    end;


end;

procedure TCriticalBlockTest_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edFilename.text;
  if OpenDialog1.execute() then
    begin
    edFilename.text := OpenDialog1.FileName;
    end;

end;

procedure TCriticalBlockTest_F.pbClearClick(Sender: TObject);
begin
  reReport.Lines.Clear();
  
end;

procedure TCriticalBlockTest_F.pbCloseClick(Sender: TObject);
begin
  Close();

end;

procedure TCriticalBlockTest_F.pbReadRawDataClick(Sender: TObject);
{$IFDEF FREEOTFE_DEBUG}
var
  prettyData: TStringList;
  data: string;
{$ENDIF}
begin
{$IFDEF FREEOTFE_DEBUG}
  reReport.Lines.Add('Reading raw data from: '+edFilename.text+' (offset: '+inttostr(se64Offset.Value)+')');
  if OTFEFreeOTFE1.DEBUGReadRawVolumeCriticalData(edFilename.text, se64Offset.Value, data) then
    begin
    reReport.Lines.Add('Raw data read:');
    prettyData:= TStringList.Create();
    try
      SDUPrettyPrintHex(data, 0, length(data), prettyData);
      reReport.Lines.AddStrings(prettyData);
    finally
      prettyData.Free();
    end;

    end
  else
    begin
    reReport.Lines.Add('FAILED to read data.');
    end;
{$ELSE}
  showmessage('FREEOTFE_DEBUG compiler flag must be set to test this function');
{$ENDIF}

end;

procedure TCriticalBlockTest_F.pbWriteRawDataClick(
  Sender: TObject);
{$IFDEF FREEOTFE_DEBUG}
var
  data: string;
  i: integer;
{$ENDIF}
begin
{$IFDEF FREEOTFE_DEBUG}
  if (MessageDlg(
             '!!! DANGER !!!'+CRLF+
             CRLF+
             'This will write test data to:'+CRLF+
             CRLF+
             edFilename.Text+CRLF+
             CRLF+
             'If you specified a partition or drive, test data will be written to '+CRLF+
             'that area.'+CRLF+
             CRLF+
             'DO YOU WANT TO PROCEED?',
             mtWarning, [mbYes,mbNo], 0) = mrYes) then
    begin
    reReport.Lines.Add('Writing raw data : '+edFilename.text+' (offset: '+inttostr(se64Offset.Value)+')');

    for i:=0 to ((CRITICAL_DATA_LENGTH div 8)-1) do
      begin
      data := data + chr(ord('a') + (i mod 26));
      end;

    if OTFEFreeOTFE1.DEBUGWriteRawVolumeCriticalData(edFilename.text, se64Offset.Value, data) then
      begin
      reReport.Lines.Add('Raw data written OK');
      end
    else
      begin
      reReport.Lines.Add('FAILED to write data.');
      end;
      
    end;
{$ELSE}
  showmessage('FREEOTFE_DEBUG compiler flag must be set to test this function');
{$ENDIF}


end;


END.

