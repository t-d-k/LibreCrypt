unit frmFreeOTFEHdrDump;
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
  Classes, ComCtrls, Buttons,
  Controls, Dialogs,
  Forms, Graphics, Messages, PasswordRichEdit, Spin64,
  StdCtrls, SysUtils, Windows,
  //sdu / lc utils
  lcTypes, OTFEFreeOTFEBase_U, lcDialogs, SDUFilenameEdit_U, SDUForms, SDUFrames,
  SDUGeneral, SDUSpin64Units, OTFE_U,
  //librecrypt forms

  frmHdrDump,
  fmeVolumeSelect,
  fmePassword;

type
  TfrmFreeOTFEHdrDump = class (TfrmHdrDump)
    lblOffset:         TLabel;
    seSaltLength:      TSpinEdit64;
    lblSaltLengthBits: TLabel;
    lblSaltLength:     TLabel;
    seKeyIterations:   TSpinEdit64;
    lblKeyIterations:  TLabel;
    se64UnitOffset:    TSDUSpin64Unit_Storage;
    frmePassword1:     TfrmePassword;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure pbOKClick(Sender: TObject);
  private
    function GetUserKey(): TSDUBytes;

    function GetOffset(): Int64;
    function GetSaltLength(): Integer;
    function GetKeyIterations(): Integer;

  protected
    procedure _EnableDisableControls(); override;

    function _DumpHdrDataToFile(): Boolean; override;
    procedure SetPassword(const Value: String); override;
  public

  end;

//add header to stringlist
procedure AddStdDumpHeader(content: TStringList; title: String);

implementation

{$R *.DFM}

uses
     //delphi & libs
  //sdu & LibreCrypt utils
  DriverAPI,
  OTFEConsts_U,
  OTFEFreeOTFE_U,
  VolumeFileAPI,
  lcConsts, SDUi18n,
   // LibreCrypt forms
   frmVersionCheck
   ;



{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}


procedure _AddStdDumpSection(content: TStringList; sectionTitle: String);
begin
  content.Add('');
  content.Add('');
  content.Add(sectionTitle);
  content.Add(StringOfChar('-', length(sectionTitle)));
end;

procedure AddStdDumpHeader(content: TStringList; title: String);
var
  driverVersion:      String;
  platformID:         String;
  envARCHITECTURE:    String;
  envARCH_W3264:      String;
  useEnvARCHITECTURE: String;
  useEnvARCH_W3264:   String;
begin
  // This function may be called when the driver isn't running
  try
    driverVersion := GetFreeOTFEBase().VersionStr();
  except
    driverVersion := RS_UNKNOWN;
  end;

  content.Add(title);
  content.Add(StringOfChar('=', length(title)));
  // content.Add(''); - Newlines not needed; AddStdDumpSection(..) adds newlines
  _AddStdDumpSection(content, _('Dump Created By'));
  useEnvARCHITECTURE := '---';
  if SDUGetEnvironmentVar(EVN_VAR_PROC_ARCHITECTURE, envARCHITECTURE) then begin
    useEnvARCHITECTURE := envARCHITECTURE;
  end;
  useEnvARCH_W3264 := '---';
  if SDUGetEnvironmentVar(EVN_VAR_PROC_ARCH_W3264, envARCH_W3264) then begin
    useEnvARCH_W3264 := envARCH_W3264;
  end;
  platformID := 'PC ' + GetFreeOTFEBase().GetDriverType() + ' (' +
    INSTALLED_OS_TITLE[SDUInstalledOS()] + '; ' + IntToStr(SDUOSCPUSize()) +
    ' bit [' + useEnvARCHITECTURE + '/' + useEnvARCH_W3264 + ']' + ')';
  content.Add(Format(_('Platform              : %s'), [platformID]));
  content.Add(Format(_('Application version   : %s'),
    ['v' + SDUGetVersionInfoString(ParamStr(0))]));
  content.Add(Format(_('Driver ID             : %s'), [driverVersion]));
  //  content.Add(''); // Newlines not needed - added by following section header
end;


 // Return a nicely formatted string, decoding one bit of a bitmapped value
 // bit - The zero-offset bit (i.e. the LSB is bit 0)
function _DumpCriticalDataToFileBitmap(Value: DWORD;
  bit: Integer; unsetMeaning: String; setMeaning: String): String;
var
  x:          DWORD;
  i:          Integer;
  val:        Integer;
  useMeaning: String;
begin
  Result := '';

  x := 1;

  for i := 1 to bit do begin
    x := x * 2;
  end;
  { TODO -otdk -crefactor : or x shl bit ... }
  assert(x = (1 shl bit));

  val        := 0;
  useMeaning := unsetMeaning;
  if ((Value and x) = x) then begin
    val        := 1;
    useMeaning := setMeaning;
  end;

  Result := Format(_('Bit %d: %d (%s)'), [bit, val, useMeaning]);

end;

function _DumpCriticalDataToFile(volFilename: String;
  offsetWithinFile: Int64; userPassword: TSDUBytes; saltLength: Integer;  // In bits
  keyIterations: Integer; dumpFilename: String): Boolean;
var
  volumeDetails:        TVolumeDetailsBlock;
  CDBMetaData:          TCDBMetaData;
  dumpReport:           TStringList;
  prettyPrintData:      TStringList;
  criticalDataBuffer:   Ansistring;
  hashTitle:            String;
  cypherTitle:          String;
  hashDetails:          TFreeOTFEHash;
  cypherDetails:        TFreeOTFECypher_v3;
  readTimeStart:        TDateTime;
  readTimeStop:         TDateTime;
  readTimeDiff:         TDateTime;
  Hour, Min, Sec, MSec: Word;


begin
  GetFreeOTFE().LastErrorCode := OTFE_ERR_UNKNOWN_ERROR;
  Result := False;

  GetFreeOTFE().CheckActive();

  prettyPrintData := TStringList.Create();
  try
    GetFreeOTFE().fdumpFlag := True;

    readTimeStart := Now();

    // Read in the raw CDB to display the encrypted version, and then read it in
    // again to get the decrypt it
    if (GetFreeOTFE().ReadRawVolumeCriticalData(volFilename, offsetWithinFile,
      criticalDataBuffer) and GetFreeOTFE().ReadVolumeCriticalData(volFilename,
      offsetWithinFile, userPassword, saltLength,  // In bits
      keyIterations, volumeDetails, CDBMetaData)) then begin
      readTimeStop := Now();

      // Generate the report
      dumpReport := TStringList.Create();
      try
        AddStdDumpHeader(dumpReport, _('FreeOTFE header Dump'));

        _AddStdDumpSection(dumpReport, _('User Supplied Information'));
        dumpReport.Add(Format(_('Filename (user mode)  : %s'), [volFilename]));
        dumpReport.Add(Format(_('Filename (kernel mode): %s'),
          [GetFreeOTFE().GetKernelModeVolumeFilename(volFilename)]));
        dumpReport.Add(_('Password              : '));
        dumpReport.Add('  ' + Format(_('Length: %u bits'), [(Length(userPassword) * 8)]));
        dumpReport.Add('  ' + _('Data  : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          userPassword,
          0,
          Length(userPassword),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(Format(_('Offset                : %d bytes'), [offsetWithinFile]));
        dumpReport.Add(Format(_('Salt length           : %d bits'), [saltLength]));
        dumpReport.Add(Format(_('Key iterations        : %d'), [keyIterations]));

        _AddStdDumpSection(dumpReport, _('Plaintext Information'));
        dumpReport.Add(_('Salt data             :'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          criticalDataBuffer,
          0,
          (saltLength div 8),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);


        _AddStdDumpSection(dumpReport, _('Dump Performance'));
        readTimeDiff := (readTimeStop - readTimeStart);
        DecodeTime(readTimeDiff, Hour, Min, Sec, MSec);
        dumpReport.Add(Format(_('Time to process header   : %u hours, %u mins, %u.%u secs'),
          [Hour, Min, Sec, MSec]));

        _AddStdDumpSection(dumpReport, _('Autodetermined Information'));

        hashTitle := _('ERROR: Unable to determine hash title?!');
        if GetFreeOTFE().GetSpecificHashDetails(CDBMetaData.HashDriver,
          CDBMetaData.HashGUID, hashDetails) then begin
          hashTitle := GetFreeOTFE().GetHashDisplayTechTitle(hashDetails);
        end;

        cypherTitle := _('ERROR: Unable to determine cypher title?!');
        if GetFreeOTFE().GetSpecificCypherDetails(CDBMetaData.CypherDriver,
          CDBMetaData.CypherGUID, cypherDetails) then begin
          cypherTitle := GetFreeOTFE().GetCypherDisplayTechTitle(cypherDetails);
        end;

        dumpReport.Add(Format(_('Hash pretty title        : %s'), [hashTitle]));
        dumpReport.Add(Format(_('Hash driver lib/KM name  : %s'),
          [CDBMetaData.HashDriver]));
        dumpReport.Add(Format(_('Hash GUID                : %s'),
          [GUIDToString(CDBMetaData.HashGUID)]));
        dumpReport.Add(Format(_('Cypher pretty title      : %s'), [cypherTitle]));
        dumpReport.Add(Format(_('Cypher driver lib/KM name: %s'),
          [CDBMetaData.CypherDriver]));
        dumpReport.Add(Format(_('Cypher GUID              : %s'),
          [GUIDToString(CDBMetaData.CypherGUID)]));
        dumpReport.Add(_('Critical data key     : '));
        dumpReport.Add('  ' + Format(_('KDF   : %s'),
          [FreeOTFEKDFTitle(CDBMetaData.KDFAlgorithm)]));
        dumpReport.Add('  ' + Format(_('Length: %d bits'),
          [(Length(GetFreeOTFE().fdumpCriticalDataKey) * 8)]));
        dumpReport.Add('  ' + _('Key   : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          GetFreeOTFE().fdumpCriticalDataKey,
          0,
          Length(GetFreeOTFE().fdumpCriticalDataKey),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Plaintext encrypted block: '));
        dumpReport.Add('  ' + Format(_('Length: %d bits'),
          [(Length(GetFreeOTFE().fdumpPlaintextEncryptedBlock) * 8)]));
        dumpReport.Add('  ' + _('Data  : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          GetFreeOTFE().fdumpPlaintextEncryptedBlock,
          0,
          Length(GetFreeOTFE().fdumpPlaintextEncryptedBlock),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Check MAC             : '));
        dumpReport.Add('  ' + Format(_('MAC algorithm: %s'),
          [FreeOTFEMACTitle(CDBMetaData.MACAlgorithm)]));
        dumpReport.Add('  ' + Format(_('Length       : %d bits'),
          [(Length(GetFreeOTFE().fdumpCheckMAC) * 8)]));
        dumpReport.Add('  ' + _('Data         : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          GetFreeOTFE().fdumpCheckMAC,
          0,
          Length(GetFreeOTFE().fdumpCheckMAC),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(_('Container details block  : '));
        dumpReport.Add('  ' + Format(_('Length       : %d bits'),
          [(Length(GetFreeOTFE().fdumpVolumeDetailsBlock) * 8)]));
        dumpReport.Add('  ' + _('Data         : '));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          GetFreeOTFE().fdumpVolumeDetailsBlock,
          0,
          Length(GetFreeOTFE().fdumpVolumeDetailsBlock),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);


        _AddStdDumpSection(dumpReport, _('Container Details Block'));
        dumpReport.Add(Format(_('Header format ID         : %d'),
          [volumeDetails.CDBFormatID]));

        // Decode the VolumeFlags to human-readable format
        dumpReport.Add(Format(_('Container flags          : %d'),
          [volumeDetails.VolumeFlags]));
        dumpReport.Add(_('                        ') + _DumpCriticalDataToFileBitmap(
          volumeDetails.VolumeFlags, VOL_FLAGS_SECTOR_ID_ZERO_VOLSTART,
          _('Sector ID zero is at the start of the encrypted data'), _(
          'Sector ID zero is at the start of the host file/partition')));

        dumpReport.Add(Format(_('Partition length      : %d bytes'),
          [volumeDetails.PartitionLen]));

        dumpReport.Add(_('Master key            : '));
        dumpReport.Add('  ' + Format(_('Length: %d bits'),
          [volumeDetails.MasterKeyLength]));
        dumpReport.Add('  ' + _('Key   :'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          volumeDetails.MasterKey,
          0,
          (volumeDetails.MasterKeyLength div 8),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);
        dumpReport.Add(Format(_('Sector IV generation  : %s'),
          [FreeOTFESectorIVGenMethodTitle[volumeDetails.SectorIVGenMethod]]));
        dumpReport.Add(_('Container IV             : '));
        dumpReport.Add('  ' + Format(_('Length : %d bits'),
          [volumeDetails.VolumeIVLength]));
        dumpReport.Add('  ' + _('IV data:'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          volumeDetails.VolumeIV,
          0,
          (volumeDetails.VolumeIVLength div 8),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);

        if (volumeDetails.RequestedDriveLetter = #0) then begin
          dumpReport.Add(_('Requested drive letter: None; use default'));
        end else begin
          dumpReport.Add(Format(_('Requested drive letter: %s'),
            [volumeDetails.RequestedDriveLetter + ':']));
        end;

        _AddStdDumpSection(dumpReport, _('Encrypted Header'));
        prettyPrintData.Clear();
        SDUPrettyPrintHex(
          criticalDataBuffer,
          0,
          (CRITICAL_DATA_LENGTH div 8),
          prettyPrintData
          );
        dumpReport.AddStrings(prettyPrintData);


        // Save the report out to disk...
        dumpReport.SaveToFile(dumpFilename);
        Result := True;
      finally
        GetFreeOTFE().fdumpFlag := False;
        SDUZeroBuffer(GetFreeOTFE().fdumpCriticalDataKey);
        GetFreeOTFE().fdumpCheckMAC                := '';
        GetFreeOTFE().fdumpPlaintextEncryptedBlock := '';
        GetFreeOTFE().fdumpVolumeDetailsBlock      := '';

        dumpReport.Clear();
        dumpReport.Free();
      end;

    end;

  finally
    prettyPrintData.Free();
  end;

  if Result then
    GetFreeOTFE().LastErrorCode := OTFE_ERR_SUCCESS;

end;



function TfrmFreeOTFEHdrDump.GetUserKey(): TSDUBytes;
begin
  Result := frmePassword1.GetKeyPhrase;
end;

function TfrmFreeOTFEHdrDump.GetOffset(): Int64;
begin
  Result := se64UnitOffset.Value;
end;

function TfrmFreeOTFEHdrDump.GetSaltLength(): Integer;
begin
  Result := seSaltLength.Value;
end;

function TfrmFreeOTFEHdrDump.GetKeyIterations(): Integer;
begin
  Result := seKeyIterations.Value;
end;

procedure TfrmFreeOTFEHdrDump.FormCreate(Sender: TObject);
begin
  inherited;

  self.Caption := _('Dump FreeOTFE Header');


  // se64UnitOffset set in OnShow event (otherwise units not shown correctly)

  seSaltLength.Increment := 8;
  seSaltLength.Value     := DEFAULT_SALT_LENGTH;

  seKeyIterations.MinValue  := 1;
  seKeyIterations.MaxValue  := 999999;
  // Need *some* upper value, otherwise setting MinValue won't work properly
  seKeyIterations.Increment := DEFAULT_KEY_ITERATIONS_INCREMENT;
  seKeyIterations.Value     := DEFAULT_KEY_ITERATIONS;

end;

procedure TfrmFreeOTFEHdrDump._EnableDisableControls();
begin
  inherited;
  pbOK.Enabled := pbOK.Enabled and (GetKeyIterations > 0);
end;

procedure TfrmFreeOTFEHdrDump.FormShow(Sender: TObject);
begin
  inherited;

  se64UnitOffset.Value := 0;

  _EnableDisableControls();

end;

procedure TfrmFreeOTFEHdrDump.ControlChanged(Sender: TObject);
begin
  _EnableDisableControls();
end;

function TfrmFreeOTFEHdrDump._DumpHdrDataToFile(): Boolean;
begin
  Result := _DumpCriticalDataToFile(GetVolumeFilename, GetOffset, GetUserKey,
    GetSaltLength,  // In bits
    GetKeyIterations, GetDumpFilename);
end;


procedure TfrmFreeOTFEHdrDump.pbOKClick(Sender: TObject);
begin
  _PromptDumpData('FreeOTFE');
end;

procedure TfrmFreeOTFEHdrDump.SetPassword(const Value: String);
begin
  inherited;
  frmePassword1.SetKeyPhrase(Value);
end;



end.
