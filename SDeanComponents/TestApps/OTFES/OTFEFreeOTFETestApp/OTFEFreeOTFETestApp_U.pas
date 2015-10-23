unit OTFEFreeOTFETestApp_U;
// Description: 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

{$IFDEF LINUX}
This software is only intended for use under MS Windows
{$ENDIF}
{$WARN UNIT_PLATFORM OFF}  // Useless warning about platform - we're already
                           // protecting against that!

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  OTFE_U,
  SDUClasses,
  OTFEFreeOTFE_U,
  OTFEFreeOTFEDLL_U,
  DriverAPI, Grids, FileCtrl, Spin64, OTFEFreeOTFEBase_U;

type
  TOTFEFreeOTFETestApp_F = class(TForm)
    pbClose: TButton;
    pbVersion: TButton;
    pbIsEncryptedVolFile: TButton;
    edTestIfMtdVolFile: TEdit;
    DriveComboBox1: TDriveComboBox;
    OpenDialog1: TOpenDialog;
    pbBrowse: TButton;
    pbDisountVolume: TButton;
    pbMountVolume: TButton;
    pbDismountDrive: TButton;
    pbClear: TButton;
    rgActive: TRadioGroup;
    ckDismountDriveEmergency: TCheckBox;
    pbGetFileMountedForDrive: TButton;
    pbGetDriveMountedForFile: TButton;
    pbNumDrivesMounted: TButton;
    pbRefresh: TButton;
    pbGetDrivesMounted: TButton;
    pbIsDriverInstalled: TButton;
    pbDismountAll: TButton;
    RichEdit1: TRichEdit;
    pbDriveInfo: TButton;
    gbVolInfo: TGroupBox;
    lblReadOnly: TLabel;
    lblDriveMountedAs: TLabel;
    Label15: TLabel;
    Label22: TLabel;
    Label1: TLabel;
    lblIVHashDevice: TLabel;
    Label9: TLabel;
    lblIVHashGUID: TLabel;
    Label2: TLabel;
    lblVolumeFile: TLabel;
    OTFEFreeOTFE_PC_DRIVER: TOTFEFreeOTFE;
    pbVersionStr: TButton;
    lblDevice: TLabel;
    Label7: TLabel;
    pbGetCypherDrivers: TButton;
    pbGetHashDrivers: TButton;
    Label3: TLabel;
    Label8: TLabel;
    lblMainCypherGUID: TLabel;
    lblMainCypherDevice: TLabel;
    pcTest: TPageControl;
    tsCypher: TTabSheet;
    tsHash: TTabSheet;
    GroupBox2: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    edCypherDriver: TEdit;
    edCypherGUID: TEdit;
    edCypherInFilename: TEdit;
    pbEncryptFile: TButton;
    edCypherOutFilename: TEdit;
    edCypherKey: TEdit;
    edCypherIV: TEdit;
    pbDecryptFile: TButton;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label10: TLabel;
    edHashDriver: TEdit;
    edHashGUID: TEdit;
    edHashData: TEdit;
    pbHashData: TButton;
    pbCreateFreeOTFEVolumeWizard: TButton;
    pbCDBTests: TButton;
    ckDebugShowmessage: TCheckBox;
    tsMAC: TTabSheet;
    GroupBox3: TGroupBox;
    Label18: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    edMACHashDriver: TEdit;
    edMACHashGUID: TEdit;
    pbMACData: TButton;
    edMACData: TEdit;
    Label23: TLabel;
    Label24: TLabel;
    seMACTruncate: TSpinEdit64;
    reMACKey: TRichEdit;
    tsKDF: TTabSheet;
    GroupBox4: TGroupBox;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    edKDFHashDriver: TEdit;
    edKDFHashGUID: TEdit;
    pbDeriveKey: TButton;
    edKDFSalt: TEdit;
    seKDFdkLen: TSpinEdit64;
    reKDFPassword: TRichEdit;
    seKDFIterations: TSpinEdit64;
    Label30: TLabel;
    Label31: TLabel;
    ckKDFPasswordHex: TCheckBox;
    ckKDFSaltHex: TCheckBox;
    cbMAC: TComboBox;
    cbKDF: TComboBox;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    pbTestHCSelect: TButton;
    pbSelectPartition: TButton;
    DbgClr: TButton;
    DbgDisp: TButton;
    Label6: TLabel;
    reMetaData: TRichEdit;
    Label11: TLabel;
    Label35: TLabel;
    lblIVCypherDevice: TLabel;
    lblIVCypherGUID: TLabel;
    pbCryptlibTests: TButton;
    pbRNGTests: TButton;
    Label36: TLabel;
    edCypherSectorID: TEdit;
    edCypherSectorSize: TEdit;
    Label37: TLabel;
    pbLRWTest: TButton;
    Label38: TLabel;
    Label39: TLabel;
    pbEncryptData: TButton;
    pbDecryptData: TButton;
    Button1: TButton;
    OTFEFreeOTFE_PC_DLL: TOTFEFreeOTFEDLL;
    pbDLLREADMounted: TButton;
    pbDLLWRITEMounted: TButton;
    procedure pbCloseClick(Sender: TObject);
    procedure pbVersionClick(Sender: TObject);
    procedure pbIsEncryptedVolFileClick(Sender: TObject);
    procedure pbBrowseClick(Sender: TObject);
    procedure pbMountVolumeClick(Sender: TObject);
    procedure pbDisountVolumeClick(Sender: TObject);
    procedure pbDismountDriveClick(Sender: TObject);
    procedure pbClearClick(Sender: TObject);
    procedure rgActiveClick(Sender: TObject);
    procedure pbGetFileMountedForDriveClick(Sender: TObject);
    procedure pbGetDriveMountedForFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbNumDrivesMountedClick(Sender: TObject);
    procedure pbRefreshClick(Sender: TObject);
    procedure pbGetDrivesMountedClick(Sender: TObject);
    procedure pbIsDriverInstalledClick(Sender: TObject);
    procedure pbDismountAllClick(Sender: TObject);
    procedure pbDriveInfoClick(Sender: TObject);
    procedure pbVersionStrClick(Sender: TObject);
    procedure pbGetCypherDriversClick(Sender: TObject);
    procedure pbGetHashDriversClick(Sender: TObject);
    procedure pbHashDataClick(Sender: TObject);
    procedure pbEncryptFileClick(Sender: TObject);
    procedure pbDecryptFileClick(Sender: TObject);
    procedure pbCreateFreeOTFEVolumeWizardClick(Sender: TObject);
    procedure pbCDBTestsClick(Sender: TObject);
    procedure pbMACDataClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pbDeriveKeyClick(Sender: TObject);
    procedure pbSelectPartitionClick(Sender: TObject);
    procedure pbTestHCSelectClick(Sender: TObject);
    procedure ckDebugShowmessageClick(Sender: TObject);
    procedure DbgDispClick(Sender: TObject);
    procedure DbgClrClick(Sender: TObject);
    procedure pbCryptlibTestsClick(Sender: TObject);
    procedure pbRNGTestsClick(Sender: TObject);
    procedure pbLRWTestClick(Sender: TObject);
    procedure pbEncryptDataClick(Sender: TObject);
    procedure pbDecryptDataClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure pbDLLREADMountedClick(Sender: TObject);
    procedure pbDLLWRITEMountedClick(Sender: TObject);
  private
    prettyPrintStrings: TStringList;
    TimeStamp: TDateTime;
    OTFEFreeOTFE1: TOTFEFreeOTFEBase;
    stm: TSDUMemoryStream;

    lastMountedDrive: char;

    procedure ReportWhetherActive();
    procedure RefreshDriveComboBox();

    // Set "encryptFlag" to TRUE to encrypt; FALSE to decrypt
procedure EncryptDecryptFile(
  encryptFlag: boolean;
  inFile: string;
  outFile: string
);

function EncryptDecryptData(
  encryptFlag: boolean;
  inData: string;
  var outData: string
): boolean;

    procedure TimingStart();
    procedure TimingStopAndReport();

    procedure PopulateMACs();
    procedure PopulateKDFs();

    function GetMAC(): TFreeOTFEMACAlgorithm;
    function GetKDF(): TFreeOTFEKDFAlgorithm;
  public
    { Public declarations }
  end;

var
  OTFEFreeOTFETestApp_F: TOTFEFreeOTFETestApp_F;

implementation

{$R *.DFM}


uses
  ComObj,  // Required for GUIDToString(...)
  ShellAPI,
  SDUGeneral,
  CriticalBlockTest_U,
  OTFEFreeOTFE_frmSelectHashCypher,
  cryptlibAPITest_U,
  RNGTest_U;


const
  DUMMY_GUID = '{00000000-0000-0000-0000-000000000000}';

  

procedure TOTFEFreeOTFETestApp_F.ReportWhetherActive();
begin
  if OTFEFreeOTFE1.Active then
    begin
    RichEdit1.lines.add('FreeOTFE component ACTIVE');
    rgActive.ItemIndex:=0;
    end
  else
    begin
    RichEdit1.lines.add('FreeOTFE component NOT Active');
    rgActive.ItemIndex:=1;
    end;

end;

procedure TOTFEFreeOTFETestApp_F.pbCloseClick(Sender: TObject);
begin
  Close;
  
end;

procedure TOTFEFreeOTFETestApp_F.pbVersionClick(Sender: TObject);
begin
  RichEdit1.lines.add('Driver version: 0x'+inttohex(OTFEFreeOTFE1.Version(), 8));

end;

procedure TOTFEFreeOTFETestApp_F.pbIsEncryptedVolFileClick(Sender: TObject);
var
  filename: string;
  output: string;
begin
  filename := edTestIfMtdVolFile.text;

  output := filename;
  if OTFEFreeOTFE1.IsEncryptedVolFile(filename) then
    begin
    output := output + ' IS ';
    end
  else
    begin
    output := output + ' is NOT ';
    end;

  output := output + 'a FreeOTFE volume file';
  
  RichEdit1.lines.add(output);


end;

procedure TOTFEFreeOTFETestApp_F.pbBrowseClick(Sender: TObject);
begin
  OpenDialog1.Filename := edTestIfMtdVolFile.text;
  if OpenDialog1.execute() then
    begin
    edTestIfMtdVolFile.text := OpenDialog1.FileName;
    end;

end;

procedure TOTFEFreeOTFETestApp_F.pbMountVolumeClick(Sender: TObject);
begin
  try
    lastMountedDrive := OTFEFreeOTFE1.Mount(edTestIfMtdVolFile.text);
    if lastMountedDrive<>#0 then
      begin
      RichEdit1.lines.add(edTestIfMtdVolFile.text+' mounted OK');
      end
    else
      begin
      RichEdit1.lines.add(edTestIfMtdVolFile.text+' mount failed');
      end;
  except
    RichEdit1.lines.add('************************************');
    RichEdit1.lines.add('************************************');
    RichEdit1.lines.add('************************************');
    RichEdit1.lines.add('***  UNHANDLED EXCEPTION CAUGHT  ***');
    RichEdit1.lines.add('************************************');
    RichEdit1.lines.add('************************************');
    RichEdit1.lines.add('************************************');
  end;
  
  RefreshDriveComboBox();

end;

procedure TOTFEFreeOTFETestApp_F.pbDisountVolumeClick(Sender: TObject);
begin
  if OTFEFreeOTFE1.Dismount(edTestIfMtdVolFile.text, ckDismountDriveEmergency.checked) then
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismounted OK');
    end
  else
    begin
    RichEdit1.lines.add(edTestIfMtdVolFile.text+' dismount failed');
    end;

  RefreshDriveComboBox();

end;


procedure TOTFEFreeOTFETestApp_F.pbDismountDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFEFreeOTFE1.Dismount(DriveComboBox1.drive, ckDismountDriveEmergency.checked) then
    begin
    output := output + 'dismounted OK';
    end
  else
    begin
    output := output + 'dismount FAILED';
    end;

  RichEdit1.lines.add(output);

  RefreshDriveComboBox();

end;

procedure TOTFEFreeOTFETestApp_F.RefreshDriveComboBox();
var
  origCase: TTextCase;
begin
  origCase := DriveComboBox1.TextCase;

  DriveComboBox1.TextCase := tcUpperCase;
  DriveComboBox1.TextCase := tcLowerCase;

  DriveComboBox1.TextCase := origCase;
end;

procedure TOTFEFreeOTFETestApp_F.pbClearClick(Sender: TObject);
begin
  RichEdit1.lines.Clear();

end;

procedure TOTFEFreeOTFETestApp_F.rgActiveClick(Sender: TObject);
begin
  OTFEFreeOTFE_PC_DLL.ExeDir := ExtractFilePath(ParamStr(0));

xxx - Set "OTFEFreeOTFE1" as appropriate to test either the DLL or driver version

  OTFEFreeOTFE1 := OTFEFreeOTFE_PC_DLL;
  edMACHashDriver.text := '.\DLL\FreeOTFEHashSHA.dll';
  edKDFHashDriver.text := '.\DLL\FreeOTFEHashSHA.dll';

//  OTFEFreeOTFE1 := OTFEFreeOTFE_PC_DRIVER;

  try
    OTFEFreeOTFE1.Active := (rgActive.ItemIndex=0);
  finally
    ReportWhetherActive();
  end;

end;


procedure TOTFEFreeOTFETestApp_F.pbGetFileMountedForDriveClick(Sender: TObject);
var
  output: string;
begin
  output := DriveComboBox1.drive +
            ': is FreeOTFE volume file: "' +
            OTFEFreeOTFE1.GetVolFileForDrive(DriveComboBox1.drive) +
            '"';

  RichEdit1.lines.add(output);

end;

procedure TOTFEFreeOTFETestApp_F.pbGetDriveMountedForFileClick(Sender: TObject);
var
  output: string;
begin
  output := '"' +
            edTestIfMtdVolFile.text +
            '" is mounted as: '+
            OTFEFreeOTFE1.GetDriveForVolFile(edTestIfMtdVolFile.text) +
            ':';

  RichEdit1.lines.add(output);


end;

procedure TOTFEFreeOTFETestApp_F.FormCreate(Sender: TObject);
begin
  prettyPrintStrings:= TStringList.Create();

  edTestIfMtdVolFile.text := 'C:\OTFE_VOL_FILE.dat';
  edTestIfMtdVolFile.text := 'C:\test_twofish.dat';

  PopulateMACs();
  PopulateKDFs();

  seMACTruncate.Increment := 8;

{$IFDEF FREEOTFE_DEBUG}
OTFEFreeOTFE1.DebugClear();
OTFEFreeOTFE1.DebugShowMessage := ckDebugShowmessage.checked;
{$ENDIF}

  stm:=TSDUMemoryStream.Create();

end;

procedure TOTFEFreeOTFETestApp_F.pbNumDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'Number of FreeOTFE volumes mounted: ';
  output := output + inttostr(OTFEFreeOTFE1.CountDrivesMounted());

  RichEdit1.lines.add(output);

end;

procedure TOTFEFreeOTFETestApp_F.pbRefreshClick(Sender: TObject);
begin
  RefreshDriveComboBox();

end;

procedure TOTFEFreeOTFETestApp_F.pbGetDrivesMountedClick(Sender: TObject);
var
  output: string;
begin
  output := 'FreeOTFE volumes mounted: ';
  output := output + OTFEFreeOTFE1.DrivesMounted();

  RichEdit1.lines.add(output);

end;

procedure TOTFEFreeOTFETestApp_F.pbIsDriverInstalledClick(Sender: TObject);
begin
  if OTFEFreeOTFE1.IsDriverInstalled() then
    begin
    RichEdit1.lines.add('Driver installed');
    end
  else
    begin
    RichEdit1.lines.add('Driver NOT installed');
    end;

end;

procedure TOTFEFreeOTFETestApp_F.pbDismountAllClick(
  Sender: TObject);
var
  dismountFailedFor: string;
begin
  dismountFailedFor := OTFEFreeOTFE1.DismountAll(ckDismountDriveEmergency.checked);

  if dismountFailedFor='' then
    begin
    RichEdit1.lines.add('DismountAll OK');
    end
  else
    begin
    RichEdit1.lines.add('DismountAll failed for drives: '+dismountFailedFor);
    end;

end;


procedure TOTFEFreeOTFETestApp_F.pbDriveInfoClick(Sender: TObject);
var
  output: string;
  volumeInfo: TOTFEFreeOTFEvolumeInfo;
begin
  output := DriveComboBox1.drive + ': ';
  if OTFEFreeOTFE1.GetVolumeInfo(DriveComboBox1.drive, volumeInfo) then
    begin
    output := output + 'Got drive info OK';

    lblDriveMountedAs.caption := volumeInfo.DriveLetter+':';
    lblVolumeFile.caption := volumeInfo.Filename;
    lblDevice.caption := volumeInfo.DeviceName;
    if volumeInfo.ReadOnly then
      begin
      lblReadOnly.caption := 'readonly';
      end
    else
      begin
      lblReadOnly.caption := 'read/write';
      end;

    lblIVHashDevice.caption     := volumeInfo.IVHashDevice;
    lblIVHashGUID.caption       := GUIDToString(volumeInfo.IVHashGUID);
    lblIVCypherDevice.caption   := volumeInfo.IVCypherDevice;
    lblIVCypherGUID.caption     := GUIDToString(volumeInfo.IVCypherGUID);
    lblMainCypherDevice.caption := volumeInfo.MainCypherDevice;
    lblMainCypherGUID.caption   := GUIDToString(volumeInfo.MainCypherGUID);
    reMetaData.Text := volumeInfo.Metadata;
    end
  else
    begin
    output := output + 'DriveInfo FAILED';
    end;

  RichEdit1.lines.add(output);

  RefreshDriveComboBox();

end;



procedure TOTFEFreeOTFETestApp_F.pbVersionStrClick(Sender: TObject);
begin
  RichEdit1.lines.add('Driver version string: '+OTFEFreeOTFE1.VersionStr());

end;


procedure TOTFEFreeOTFETestApp_F.pbGetCypherDriversClick(Sender: TObject);
var
  cypherDrivers: array of TFreeOTFECypherDriver;
  i, j: integer;
  currCypherDriver: TFreeOTFECypherDriver;
  currCypher: TFreeOTFECypher_v3;
begin
  SetLength(cypherDrivers, 0);
  if OTFEFreeOTFE1.GetCypherDrivers(TFreeOTFECypherDriverArray(cypherDrivers)) then
    begin
    RichEdit1.lines.add('======================================================');
    RichEdit1.lines.add('Got cypher drivers OK');
    RichEdit1.lines.add('Cypher driver count: '+inttostr(length(cypherDrivers)));
    RichEdit1.lines.add('');

    for i:=low(cypherDrivers) to high(cypherDrivers) do
      begin
      currCypherDriver := cypherDrivers[i];
      RichEdit1.lines.add('- - - - - - - - - - - - - - - - - - - - - - - - - - ');
      RichEdit1.lines.add('Driver GUID                : '+GUIDToString(currCypherDriver.DriverGUID));
      RichEdit1.lines.add('Driver DeviceName          : '+currCypherDriver.DeviceName);
      RichEdit1.lines.add('Driver DeviceKernelModeName: '+currCypherDriver.LibFNOrDevKnlMdeName);
      RichEdit1.lines.add('Driver DeviceUserModeName  : '+currCypherDriver.DeviceUserModeName);
      RichEdit1.lines.add('Driver Title               : '+currCypherDriver.Title);
      RichEdit1.lines.add('Driver Version             : 0x'+inttohex(currCypherDriver.VersionID, 8));
      RichEdit1.lines.add('Driver CypherCount         : '+inttostr(currCypherDriver.CypherCount));
      RichEdit1.lines.add('');

      for j:=low(cypherDrivers[i].Cyphers) to high(cypherDrivers[i].Cyphers) do
        begin
        currCypher := cypherDrivers[i].Cyphers[j];


        RichEdit1.lines.add('Cypher GUID                : '+GUIDToString(currCypher.CypherGUID));
        RichEdit1.lines.add('Cypher Title               : '+currCypher.Title);
        RichEdit1.lines.add('Cypher Mode                : '+FreeOTFECypherModeTitle(currCypher.Mode));
        RichEdit1.lines.add('Cypher Underlying Keysize  : '+inttostr(currCypher .KeysizeUnderlying)+' bits');
        RichEdit1.lines.add('Cypher Required Keysize    : '+inttostr(currCypher.KeysizeRequired)+' bits');
        RichEdit1.lines.add('Cypher BlockSize           : '+inttostr(currCypher.BlockSize)+' bits');
        RichEdit1.lines.add('Cypher Version             : 0x'+inttohex(currCypher.VersionID, 8));
        RichEdit1.lines.add('');
        end;

      end;

    end
  else
    begin
    RichEdit1.lines.add('cypher drivers FAILED');
    end;

end;

procedure TOTFEFreeOTFETestApp_F.pbGetHashDriversClick(Sender: TObject);
var
  hashDrivers: array of TFreeOTFEHashDriver;
  i, j: integer;
  currHashDriver: TFreeOTFEHashDriver;
  currHash: TFreeOTFEHash;
begin
  SetLength(hashDrivers, 0);
  if OTFEFreeOTFE1.GetHashDrivers(TFreeOTFEHashDriverArray(hashDrivers)) then
    begin
    RichEdit1.lines.add('======================================================');
    RichEdit1.lines.add('Got hash drivers OK');
    RichEdit1.lines.add('Hash driver count: '+inttostr(length(hashDrivers)));
    RichEdit1.lines.add('');

    for i:=low(hashDrivers) to high(hashDrivers) do
      begin
      currHashDriver := hashDrivers[i];
      RichEdit1.lines.add('- - - - - - - - - - - - - - - - - - - - - - - - - - ');
      RichEdit1.lines.add('Driver GUID                : '+GUIDToString(currHashDriver.DriverGUID));
      RichEdit1.lines.add('Driver DeviceName          : '+currHashDriver.DeviceName);
      RichEdit1.lines.add('Driver DeviceKernelModeName: '+currHashDriver.LibFNOrDevKnlMdeName);
      RichEdit1.lines.add('Driver DeviceUserModeName  : '+currHashDriver.DeviceUserModeName);
      RichEdit1.lines.add('Driver Title               : '+currHashDriver.Title);
      RichEdit1.lines.add('Driver Version             : 0x'+inttohex(currHashDriver.VersionID, 8));
      RichEdit1.lines.add('Driver HashCount           : '+inttostr(currHashDriver.HashCount));
      RichEdit1.lines.add('');

      for j:=low(hashDrivers[i].Hashes) to high(hashDrivers[i].Hashes) do
        begin
        currHash := hashDrivers[i].Hashes[j];


        RichEdit1.lines.add('Hash GUID      : '+GUIDToString(currHash.HashGUID));
        RichEdit1.lines.add('Hash Title     : '+currHash.Title);
        RichEdit1.lines.add('Hash Version   : 0x'+inttohex(currHash.VersionID, 8));
        RichEdit1.lines.add('Hash Length    : '+inttostr(currHash.Length)+' bits');
        RichEdit1.lines.add('Hash BlockSize : '+inttostr(currHash.Blocksize)+' bits');
        RichEdit1.lines.add('');
        end;

      end;

    end
  else
    begin
    RichEdit1.lines.add('hash drivers FAILED');
    end;

end;


procedure TOTFEFreeOTFETestApp_F.pbHashDataClick(Sender: TObject);
var
  hashValue: string;
  tmpData: string;
begin
  TimingStart();

  tmpData := edHashData.text;
  RichEdit1.lines.Add('Hashing using:');
  RichEdit1.lines.Add('Device:'+edHashDriver.text);
  RichEdit1.lines.Add('Hash:'+GUIDToString(Str  ingToGUID(edHashGUID.text)));

  if OTFEFreeOTFE1.HashData(edHashDriver.text, StringToGUID(edHashGUID.text), tmpData, hashValue) then
    begin
    RichEdit1.lines.add('Hash is '+inttostr((Length(hashValue)*8))+' bits long');
    prettyPrintStrings.Clear();
    if SDUPrettyPrintHex(hashValue, 0, Length(hashValue), prettyPrintStrings) then
      begin
      RichEdit1.lines.AddStrings(prettyPrintStrings);
      end
    else
      begin
      RichEdit1.lines.add('hash *****prettyprint***** FAILED');
      end;

    end
  else
    begin
    RichEdit1.lines.add('hash FAILED');
    end;

  TimingStopAndReport();
end;


// Set "encryptFlag" to TRUE to encrypt; FALSE to decrypt
procedure TOTFEFreeOTFETestApp_F.EncryptDecryptFile(
  encryptFlag: boolean;
  inFile: string;
  outFile: string
);
var
  datafile: TFileStream;
  fileLen: longint;
  inData: string;
  outData: string;
  allOK: boolean;
begin
  // Read in data from the inFile
  datafile := TFileStream.Create(inFile, fmOpenRead);
  try
    // Determine file size...
    datafile.Seek(0, soFromEnd);
    fileLen := datafile.Position;

    inData := StringOfChar('Z', fileLen);

    // Read in the whole file...
    datafile.Seek(0, soFromBeginning);
    if (datafile.Read(inData[1], fileLen) <> fileLen) then
      begin
      RichEdit1.lines.Add('Unable to read in entire file ('+inttostr(fileLen)+' bytes)');
      end;

  finally
    datafile.Free();
  end;


  allOK := EncryptDecryptData(encryptFlag, inData, outData);

  if allOK then
    begin
    // Write out data to the outFile
    datafile := TFileStream.Create(outFile, fmCreate);
    try
      // Write the whole file...
      datafile.Seek(0, soFromBeginning);
      if (datafile.Write(outData[1], Length(outData)) <> Length(outData)) then
        begin
        RichEdit1.lines.Add('Unable to write out processed data ('+inttostr(Length(outData))+' bytes)');
        end;

    finally
      datafile.Free();
    end;
    end;

end;


// Set "encryptFlag" to TRUE to encrypt; FALSE to decrypt
function TOTFEFreeOTFETestApp_F.EncryptDecryptData(
  encryptFlag: boolean;
  inData: string;
  var outData: string
): boolean;
var
  key: string;
  IV: string;
  dataProcessedOK: boolean;
  liSectorID: LARGE_INTEGER;
  i: integer;
begin
  TimingStart();

  RichEdit1.lines.Add('Cypher using:');
  RichEdit1.lines.Add('Device:'+edCypherDriver.text);
  RichEdit1.lines.Add('Cypher:'+GUIDToString(StringToGUID(edCypherGUID.text)));

  prettyPrintStrings.Clear();
  if SDUParseASCIIToData(edCypherkey.text, key) then
    begin
    RichEdit1.lines.add('cypher key is '+inttostr((Length(key)*8))+' bits long');

    prettyPrintStrings.Clear();
    if SDUPrettyPrintHex(key, 0, Length(key), prettyPrintStrings) then
      begin
      RichEdit1.lines.AddStrings(prettyPrintStrings);
      end
    else
      begin
      RichEdit1.lines.add('cypher key *****prettyprint***** FAILED');
      end;

    end
  else
    begin
    RichEdit1.lines.add('cypher key PARSE FAILED');
    Result := FALSE;
    exit;
    end;

  if SDUParseASCIIToData(edCypherIV.text, IV) then
    begin
    RichEdit1.lines.add('IV is '+inttostr((Length(IV)*8))+' bits long');

    prettyPrintStrings.Clear();
    if SDUPrettyPrintHex(IV, 0, Length(IV), prettyPrintStrings) then
      begin
      RichEdit1.lines.AddStrings(prettyPrintStrings);
      end
    else
      begin
      RichEdit1.lines.add('IV *****prettyprint***** FAILED');
      end;

    end
  else
    begin
    RichEdit1.lines.add('IV PARSE FAILED');
    Result := FALSE;
    exit;
    end;


  liSectorID.QuadPart := strtoint64(edCypherSectorID.Text);

  // Call the appropriate FreeOTFE component's function...
  if (encryptFlag) then
    begin
    dataProcessedOK := OTFEFreeOTFE1.EncryptSectorData(
                                           edCypherDriver.text,
                                           StringToGUID(edCypherGUID.text),
                                           liSectorID,
                                           strtoint(edCypherSectorSize.text),
                                           key,
                                           IV,
                                           inData,
                                           outData
                                          );
    end
  else
    begin
    dataProcessedOK := OTFEFreeOTFE1.DecryptSectorData(
                                           edCypherDriver.text,
                                           StringToGUID(edCypherGUID.text),
                                           liSectorID,
                                           strtoint(edCypherSectorSize.text),
                                           key,
                                           IV,
                                           inData,
                                           outData
                                          );
    end;


  if (dataProcessedOK) then
    begin
    RichEdit1.lines.add('cypher OK');

    prettyPrintStrings.Clear();
    if SDUPrettyPrintHex(outData, 0, Length(outData), prettyPrintStrings) then
      begin
      if (prettyPrintStrings.count > 5) then
        begin
        for i:=(prettyPrintStrings.count - 1) downto 5 do
          begin
          prettyPrintStrings.delete(i);
          end;
        // The display is limited to the first 5 lines; if the output is 2MB,
        // the richedit control will just sit there and hang...
        //prettyPrintStrings.Add('(Display limited to first 5 lines)');
        end;
      RichEdit1.lines.AddStrings(prettyPrintStrings);
      end
    else
      begin
      RichEdit1.lines.add('cypher *****prettyprint***** FAILED');
      end;

    end
  else
    begin
    RichEdit1.lines.add('cypher FAILED');
    end;

  TimingStopAndReport();

  Result := dataProcessedOK;
end;

procedure TOTFEFreeOTFETestApp_F.pbEncryptDataClick(Sender: TObject);
var
  inData: string;
  outData: string;
begin
  if SDUParseASCIIToData(edCypherInFilename.text, inData) then
    begin
    EncryptDecryptData(
                       TRUE,
                       inData,
                       outData
                      );
    end
    else
    begin
      RichEdit1.lines.add('Can''t parse indata; must be ASCII hex string');
    end;


end;


procedure TOTFEFreeOTFETestApp_F.pbDecryptDataClick(Sender: TObject);
var
  inData: string;
  outData: string;
begin
  if SDUParseASCIIToData(edCypherInFilename.text, inData) then
    begin
    EncryptDecryptData(
                       FALSE,
                       inData,
                       outData
                      );
    end
    else
    begin
      RichEdit1.lines.add('Can''t parse indata; must be ASCII hex string');
    end;


end;


procedure TOTFEFreeOTFETestApp_F.pbEncryptFileClick(Sender: TObject);
begin
  EncryptDecryptFile(
                     TRUE,
                     edCypherInFilename.text,
                     edCypherOutFilename.text
                    );

end;

procedure TOTFEFreeOTFETestApp_F.pbDecryptFileClick(Sender: TObject);
begin
  EncryptDecryptFile(
                     FALSE,
                     edCypherInFilename.text,
                     edCypherOutFilename.text
                    );

end;


procedure TOTFEFreeOTFETestApp_F.pbCreateFreeOTFEVolumeWizardClick(
  Sender: TObject);
begin
  if OTFEFreeOTFE1.CreateFreeOTFEVolumeWizard() then
    begin
    RichEdit1.Lines.Add('Volume created');
    end
  else
    begin
    RichEdit1.Lines.Add('Volume ***NOT*** created');
    end;

end;


procedure TOTFEFreeOTFETestApp_F.pbCDBTestsClick(Sender: TObject);
var
  CriticalBlockTest_F: TCriticalBlockTest_F;
begin
  CriticalBlockTest_F:= TCriticalBlockTest_F.Create(nil);
  try
    CriticalBlockTest_F.OTFEFreeOTFE1:= OTFEFreeOTFE1;
    CriticalBlockTest_F.ShowModal();

  finally
    CriticalBlockTest_F.Free();
  end;

end;


procedure TOTFEFreeOTFETestApp_F.pbMACDataClick(Sender: TObject);
var
  MACValue: string;
  key: string;
  data: string;
begin
  TimingStart();

  RichEdit1.lines.Add('MACing using:');
  RichEdit1.lines.Add('Device:'+edMACHashDriver.text);
  RichEdit1.lines.Add('Hash:'+GUIDToString(StringToGUID(edMACHashGUID.text)));


  data := edMACData.text;
{
  SDUParseASCIIToData(reMACData.text, data);
  RichEdit1.lines.add('MAC data is '+inttostr((Length(data)*8))+' bits long');
  prettyPrintStrings.Clear();
  if SDUPrettyPrintHex(data, 0, Length(data), prettyPrintStrings) then
    begin
    RichEdit1.lines.AddStrings(prettyPrintStrings);
    end
  else
    begin
    RichEdit1.lines.add('MAC data *****prettyprint***** FAILED');
    end;
}

  
  if SDUParseASCIIToData(reMACKey.text, key) then
    begin
    RichEdit1.lines.add('MAC key is '+inttostr((Length(key)*8))+' bits long');

    prettyPrintStrings.Clear();
    if SDUPrettyPrintHex(key, 0, Length(key), prettyPrintStrings) then
      begin
      RichEdit1.lines.AddStrings(prettyPrintStrings);
      end
    else
      begin
      RichEdit1.lines.add('MAC key *****prettyprint***** FAILED');
      end;

    end
  else
    begin
    RichEdit1.lines.add('MAC key PARSE FAILED');
    exit;
    end;


  if OTFEFreeOTFE1.MACData(
                           GetMAC(),
                           edMACHashDriver.text,
                           StringToGUID(edMACHashGUID.text),
                           '',
                           StringToGUID(DUMMY_GUID),
                           key,
                           data,
                           MACValue,
                           seMACTruncate.Value
                          ) then
    begin
    RichEdit1.lines.add('MAC is '+inttostr((Length(MACValue)*8))+' bits long');
    
    prettyPrintStrings.Clear();
    if SDUPrettyPrintHex(MACValue, 0, Length(MACValue), prettyPrintStrings) then
      begin
      RichEdit1.lines.AddStrings(prettyPrintStrings);
      end
    else
      begin
      RichEdit1.lines.add('MAC *****prettyprint***** FAILED');
      end;

    end
  else
    begin
    RichEdit1.lines.add('MAC FAILED');
    end;


  TimingStopAndReport();
end;

procedure TOTFEFreeOTFETestApp_F.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  prettyPrintStrings.Free();

end;

procedure TOTFEFreeOTFETestApp_F.pbDeriveKeyClick(Sender: TObject);
var
  derivedKeyValue: string;
  password: string;
  salt: string;
begin
  TimingStart();

  RichEdit1.lines.Add('Deriving key using:');
  RichEdit1.lines.Add('Device:'+edKDFHashDriver.text);
  RichEdit1.lines.Add('Hash:'+GUIDToString(StringToGUID(edKDFHashGUID.text)));


  if (ckKDFPasswordHex.checked) then
    begin
    if not(SDUParseASCIIToData(reKDFPassword.text, password)) then
      begin
      RichEdit1.lines.add('KDF password PARSE FAILED');
      exit;
      end;
    end
  else
    begin
    password := reKDFPassword.text;
    end;
  RichEdit1.lines.add('KDF password is '+inttostr((Length(password)*8))+' bits long');
  prettyPrintStrings.Clear();
  if SDUPrettyPrintHex(password, 0, Length(password), prettyPrintStrings) then
    begin
    RichEdit1.lines.AddStrings(prettyPrintStrings);
    end
  else
    begin
    RichEdit1.lines.add('KDF password *****prettyprint***** FAILED');
    end;



  if (ckKDFSaltHex.checked) then
    begin
    if not(SDUParseASCIIToData(edKDFSalt.text, salt)) then
      begin
      RichEdit1.lines.add('KDF salt PARSE FAILED');
      exit;
      end;
    end
  else
    begin
    salt := edKDFSalt.text;
    end;
  RichEdit1.lines.add('KDF salt is '+inttostr((Length(salt)*8))+' bits long');
  prettyPrintStrings.Clear();
  if SDUPrettyPrintHex(salt, 0, Length(salt), prettyPrintStrings) then
    begin
    RichEdit1.lines.AddStrings(prettyPrintStrings);
    end
  else
    begin
    RichEdit1.lines.add('KDF salt *****prettyprint***** FAILED');
    end;




  if OTFEFreeOTFE1.DeriveKey(
                          GetKDF(),
                          edKDFHashDriver.text,
                          StringToGUID(edKDFHashGUID.text),
                          '',
                          StringToGUID(DUMMY_GUID),
                          password,
                          salt,
                          seKDFIterations.Value,
                          (seKDFdkLen.Value * 8),
                          derivedKeyValue
                         ) then
    begin
    RichEdit1.lines.add('KDF is '+inttostr((Length(derivedKeyValue)*8))+' bits long');
    
    prettyPrintStrings.Clear();
    if SDUPrettyPrintHex(derivedKeyValue, 0, Length(derivedKeyValue), prettyPrintStrings) then
      begin
      RichEdit1.lines.AddStrings(prettyPrintStrings);
      end
    else
      begin
      RichEdit1.lines.add('KDF *****prettyprint***** FAILED');
      end;

    end
  else
    begin
    RichEdit1.lines.add('KDF FAILED');
    end;


  TimingStopAndReport();
end;

procedure TOTFEFreeOTFETestApp_F.TimingStart();
begin
  TimeStamp:= Now();
end;

procedure TOTFEFreeOTFETestApp_F.TimingStopAndReport();
var
  stopTime: TDateTime;
  diffTime: TDateTime;
  Hour, Min, Sec, MSec: Word;
begin
  stopTime:= Now();

  diffTime := (stopTime - TimeStamp);
  DecodeTime(diffTime, Hour, Min, Sec, MSec);
  RichEdit1.lines.add('Time taken: '+inttostr(Hour)+' hours, '+inttostr(Min)+' mins, '+inttostr(Sec)+'.'+inttostr(MSec)+' secs');

end;


procedure TOTFEFreeOTFETestApp_F.PopulateMACs();
var
  i: TFreeOTFEMACAlgorithm;
begin
  cbMAC.Items.Clear();
  for i:=low(TFreeOTFEMACAlgorithm) to high(TFreeOTFEMACAlgorithm) do
    begin
    if (i = fomacUnknown) then
      begin
      Continue;
      end;

    cbMAC.Items.Add(FreeOTFEMACTitle(i));

    if (i = fomacHMAC) then
      begin
      cbMAC.ItemIndex := cbMAC.Items.Count - 1;
      end;
    end;

end;

procedure TOTFEFreeOTFETestApp_F.PopulateKDFs();
var
  i: TFreeOTFEKDFAlgorithm;
begin
  cbKDF.Items.Clear();
  for i:=low(TFreeOTFEKDFAlgorithm) to high(TFreeOTFEKDFAlgorithm) do
    begin
    if (i = fokdfUnknown) then
      begin
      Continue;
      end;

    cbKDF.Items.Add(FreeOTFEKDFTitle(i));

    if (i = fokdfPBKDF2) then
      begin
      cbKDF.ItemIndex := cbKDF.Items.Count - 1;
      end;
    end;

end;

function TOTFEFreeOTFETestApp_F.GetMAC(): TFreeOTFEMACAlgorithm;
var
  retVal: TFreeOTFEMACAlgorithm;
  i: TFreeOTFEMACAlgorithm;
begin
  retVal := fomacUnknown;

  for i:=low(TFreeOTFEMACAlgorithm) to high(TFreeOTFEMACAlgorithm) do
    begin
    if (cbMAC.Items[cbMAC.ItemIndex] = FreeOTFEMACTitle(i)) then
      begin
      retVal := i;
      break;
      end;
    end;

  Result := retVal;
end;

function TOTFEFreeOTFETestApp_F.GetKDF(): TFreeOTFEKDFAlgorithm;
var
  retVal: TFreeOTFEKDFAlgorithm;
  i: TFreeOTFEKDFAlgorithm;
begin
  retVal := fokdfUnknown;

  for i:=low(TFreeOTFEKDFAlgorithm) to high(TFreeOTFEKDFAlgorithm) do
    begin
    if (cbKDF.Items[cbKDF.ItemIndex] = FreeOTFEKDFTitle(i)) then
      begin
      retVal := i;
      break;
      end;
    end;

  Result := retVal;
end;


procedure TOTFEFreeOTFETestApp_F.pbSelectPartitionClick(Sender: TObject);
var
  partition: string;
begin
  partition:= OTFEFreeOTFE1.SelectPartition();
  if (partition = '') then
    begin
    RichEdit1.Lines.Add('No partition selected');
    end
  else
    begin
    RichEdit1.Lines.Add('Partition selected: '+partition);
    end;

end;


procedure TOTFEFreeOTFETestApp_F.pbTestHCSelectClick(Sender: TObject);
var
  hashCypherSelectDlg: TfrmSelectHashCypher;
begin
  hashCypherSelectDlg:= TfrmSelectHashCypher.Create(nil);
  try
    hashCypherSelectDlg.FreeOTFEObj := OTFEFreeOTFE1;
    hashCypherSelectDlg.AddCombination(
                                       '\Device\FreeOTFE\Hash\{00000000-0000-0000-0000-000000000025}',
                                       StringToGUID('{00000000-0000-0000-0000-000000000026}'),
                                       '\Device\FreeOTFE\Cypher\{00000000-0000-0000-0000-000000000003}',
                                       StringToGUID('{00000000-0000-0000-0000-000000000006}')
                                      );
    hashCypherSelectDlg.AddCombination(
                                       '\Device\FreeOTFE\Hash\{00000000-0000-0000-0000-000000000025}',
                                       StringToGUID('{00000000-0000-0000-0000-000000000026}'),
                                       '\Device\FreeOTFE\Cypher\{00000000-0000-0000-0000-000000000070}',
                                       StringToGUID('{00000000-0000-0000-0000-000000000073}')
                                      );

    if (hashCypherSelectDlg.ShowModal() <> mrOK) then
      begin
      RichEdit1.Lines.Add('User cancelled');
      end
    else
      begin
      RichEdit1.Lines.Add('User selected combination:');
      RichEdit1.Lines.Add('Hash:');
      RichEdit1.Lines.Add(hashCypherSelectDlg.SelectedHashDriverKernelModeName());
      RichEdit1.Lines.Add(GUIDToString(hashCypherSelectDlg.SelectedHashGUID()));
      RichEdit1.Lines.Add('Cypher:');
      RichEdit1.Lines.Add(hashCypherSelectDlg.SelectedCypherDriverKernelModeName());
      RichEdit1.Lines.Add(GUIDToString(hashCypherSelectDlg.SelectedCypherGUID()));
      end;

  finally
    hashCypherSelectDlg.Free();
  end;

end;


procedure TOTFEFreeOTFETestApp_F.pbLRWTestClick(Sender: TObject);
begin

{
AES Key           4562ac25f828176d4c268414b5680185
Tweak Key         258e2a05e73e9d03ee5a830ccc094c87
Tweak Location    00000000000000000000000000000001
Plaintext P       30313233343536373839414243444546
Tweak value T     258e2a05e73e9d03ee5a830ccc094c87
P^T               15bf1836d30bab34d663c24e8f4d09c1
AES Encrypt(P^T)  d43c59c8829d425c0707cb9e986a023f
Ciphertext C      f1b273cd65a3df5fe95d489254634eb8
}

  edCypherDriver.text      := '\Device\FreeOTFE\Cypher\{00000000-0000-0000-0000-000000010001}';
  edCypherGUID.text        := '{00000000-0000-0000-0000-000000010005}';
//  edCypherSectorID.text    := '00000000000000000000000000000001';
  edCypherSectorID.text    := '0000000000000001';  // 64 bit only

  edCypherSectorSize.text  := '512';
  edCypherInFilename.text  := '30313233343536373839414243444546';
  edCypherOutFilename.text := '(n/a)';
  edCypherKey.text         := '4562ac25f828176d4c268414b5680185' +
                              '258e2a05e73e9d03ee5a830ccc094c87';
  edCypherIV.text          := '00000000000000000000000000000000';


  edCypherDriver.text      := '\Device\FreeOTFE\Cypher\{00000000-0000-0000-0000-000000010001}';
  edCypherGUID.text        := '{00000000-0000-0000-0000-000000010005}';
//  edCypherSectorID.text    := '00000000000000000000000000000001';
  edCypherSectorID.text    := inttostr($200000000);

  edCypherSectorSize.text  := '512';
  edCypherInFilename.text  := '30313233343536373839414243444546';
  edCypherOutFilename.text := '(n/a)';
  edCypherKey.text         := 'd82a9134b26a565030fe69e2377f9847' +
                              '4eb55d3105973a3f5e23dafb5a45d6c0';
  edCypherIV.text          := '00000000000000000000000000000000';

end;


procedure TOTFEFreeOTFETestApp_F.Button1Click(Sender: TObject);
var
  data: string;
  asciiRep: string;
//  byteToChange: byte;  
begin
  asciiRep := edCypherSectorID.text;
  SDUParseASCIIToData(asciiRep, data);


//  byteToChange:= (byteToChange and $80) shr 8;
//  ChangeByteEndian(a: byte
  
  
  SDUParseDataToASCII(data, asciiRep);
  edCypherSectorID.text := asciiRep;
end;

procedure TOTFEFreeOTFETestApp_F.ckDebugShowmessageClick(Sender: TObject);
begin
{$IFDEF FREEOTFE_DEBUG}
OTFEFreeOTFE1.DebugClear();
OTFEFreeOTFE1.DebugShowMessage := ckDebugShowmessage.checked;
{$ELSE}
showmessage('Not built with "FREEOTFE_DEBUG" compiler option');
{$ENDIF};

end;

procedure TOTFEFreeOTFETestApp_F.DbgDispClick(Sender: TObject);
begin
{$IFDEF FREEOTFE_DEBUG}
  RichEdit1.lines.add('DEBUG:');
  RichEdit1.lines.AddStrings(OTFEFreeOTFE1.debugStrings);
{$ELSE}
showmessage('Not built with "FREEOTFE_DEBUG" compiler option');
{$ENDIF}

end;

procedure TOTFEFreeOTFETestApp_F.DbgClrClick(Sender: TObject);
begin
{$IFDEF FREEOTFE_DEBUG}
OTFEFreeOTFE1.DebugClear();
{$ELSE}
showmessage('Not built with "FREEOTFE_DEBUG" compiler option');
{$ENDIF}

end;

procedure TOTFEFreeOTFETestApp_F.pbCryptlibTestsClick(Sender: TObject);
var
  dlg: TcryptlibAPITest;
begin
  dlg:= TcryptlibAPITest.Create(nil);
  try
    dlg.ShowModal();

  finally
    dlg.Free();
  end;

end;


procedure TOTFEFreeOTFETestApp_F.pbRNGTestsClick(Sender: TObject);
var
  dlg: TRNGTest;
begin
  dlg:= TRNGTest.Create(nil);
  try
    dlg.ShowModal();

  finally
    dlg.Free();
  end;

end;


// -----------------------------------------------------------------------------
procedure TOTFEFreeOTFETestApp_F.pbDLLREADMountedClick(Sender: TObject);
begin
  stm.Position := 0;
  if not(OTFEFreeOTFE_PC_DLL.ReadData(lastMountedDrive, 0, 1, stm)) then
    begin
    showmessage('bad-1');
    end
  else
    begin
    stm.Position := 0;
    showmessage(SDUPrettyPrintHexStr(stm, 0, 512));
    end;

end;

procedure TOTFEFreeOTFETestApp_F.pbDLLWRITEMountedClick(Sender: TObject);
begin
  stm.Position := 0;
  stm.WriteString('This is a test string');
  stm.Position := 0;
  if not(OTFEFreeOTFE_PC_DLL.WriteData(lastMountedDrive, 0, 1, stm)) then
    begin
    showmessage('bad-1');
    end
  else
    begin
    showmessage('ok');
    end;

end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
{$WARN UNIT_PLATFORM ON}
// -----------------------------------------------------------------------------

END.


