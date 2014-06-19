unit Form_Main;
// Description: HMAC Test Vector Suite
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// This test application verifies the HMAC implementation by checking it's
// output against known test vectors stored separatly in a file


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  HashAlgUnified_U;

type
  TTestDetails = record
    FieldsPopulated: integer;

    Hash: string;
    TestCaseNo: integer;
    Key: string;
    KeyLength: integer;
    Data: string;
    DataLength: integer;
    Digest: string;
  end;


  TfrmMain = class(TForm)
    pbTestHashes: TButton;
    mmoReport: TMemo;
    edFilename: TEdit;
    Label1: TLabel;
    pbBrowse: TButton;
    OpenDialog1: TOpenDialog;
    pbClose: TButton;
    procedure pbTestHashesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pbBrowseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    HashObj: THashAlgUnified;

    function ExecuteTest(Test: TTestDetails; var generatedHMAC: string; var failureReason: string): boolean;
    function ConvertToBinary(var data: string): boolean;
  public
    function SelfTest(filename: string): boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  SDUGeneral,
  HashValue_U,
  HMAC;

const
  FIELD_POPULATED_HASH       =  1;
  FIELD_POPULATED_TESTCASENO =  2;
  FIELD_POPULATED_KEY        =  4;
  FIELD_POPULATED_KEYLEN     =  8;
  FIELD_POPULATED_DATA       = 16;
  FIELD_POPULATED_DATALEN    = 32;
  FIELD_POPULATED_DIGEST     = 64;

  FIELDS_POPULATED_NONE = 0;
  FIELDS_POPULATED_ALL  = FIELD_POPULATED_HASH       +
                          FIELD_POPULATED_TESTCASENO +
                          FIELD_POPULATED_KEY        +
                          FIELD_POPULATED_KEYLEN     +
                          FIELD_POPULATED_DATA       +
                          FIELD_POPULATED_DATALEN    +
                          FIELD_POPULATED_DIGEST;


procedure TfrmMain.pbTestHashesClick(Sender: TObject);
begin
  SelfTest(edFilename.text);

end;

function TfrmMain.SelfTest(filename: string): boolean;
const
  DETAIL_HASH         = 'hash';
  DETAIL_TEST_CASE_NO = 'test_case';
  DETAIL_KEY          = 'key';
  DETAIL_KEY_LEN      = 'key_len';
  DETAIL_DATA         = 'data';
  DETAIL_DATA_LEN     = 'data_len';
  DETAIL_KEY_DIGEST   = 'key_digest';
  DETAIL_DIGEST       = 'digest';
  DETAIL_DIGEST96     = 'digest-96';
var
  currTest: TTestDetails;
  stlTestData: TStringList;
  i: integer;
  cntOK: integer;
  cntFailed: integer;
  cntTotal: integer;
  dataType: string;
  UCdataType: string;
  data: string;
  digest: THashValue;
  testResult: boolean;
  failureReason: string;
  generatedHMAC: string;
begin
  cntOK := 0;
  cntFailed := 0;
  cntTotal := 0;

  stlTestData:= TStringList.Create();
  try
    mmoReport.lines.Clear();
    mmoReport.lines.add('Reading test data from file:');
    mmoReport.lines.add(filename);
    mmoReport.lines.add('');

    stlTestData.LoadFromFile(filename);

    currTest.FieldsPopulated := FIELDS_POPULATED_NONE;

    digest:= THashValue.Create();
    try
      for i:=0 to (stlTestData.count-1) do
        begin
        // Disregard comment and blank lines
        if (
            (Trim(stlTestData[i]) = '') or
            (Pos('#', Trim(stlTestData[i])) = 1)
           ) then
          begin
          continue;
          end;

        // File consists of blocks of test data as given in the following
        // example:
        //
        //   -- begin test block --
        //   hash =          md5
        //   test_case =     1
        //   key =           0x0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b
        //   key_len =       16
        //   data =          "Hi There"
        //   data_len =      8
        //   digest =        0x9294727a3638bb1c13f48ef8158bfc9d
        //   -- end test block --

        // Split line
        SDUSplitString(stlTestData[i], dataType, data, '=');
        dataType := Trim(dataType);
        data     := Trim(data);

        UCdataType := uppercase(dataType);


        if (UCdataType = uppercase(DETAIL_HASH)) then
          begin
          currTest.Hash := data;
          currTest.FieldsPopulated := currTest.FieldsPopulated + FIELD_POPULATED_HASH;
          end
        else if (UCdataType = uppercase(DETAIL_TEST_CASE_NO)) then
          begin
          currTest.TestCaseNo := strtoint(data);
          currTest.FieldsPopulated := currTest.FieldsPopulated + FIELD_POPULATED_TESTCASENO;
          end
        else if (UCdataType = uppercase(DETAIL_KEY)) then
          begin
          currTest.Key := data;
          currTest.FieldsPopulated := currTest.FieldsPopulated + FIELD_POPULATED_KEY;
          end
        else if (UCdataType = uppercase(DETAIL_KEY_LEN)) then
          begin
          currTest.KeyLength := strtoint(data);
          currTest.FieldsPopulated := currTest.FieldsPopulated + FIELD_POPULATED_KEYLEN;
          end
        else if (UCdataType = uppercase(DETAIL_DATA)) then
          begin
          currTest.Data := data;
          currTest.FieldsPopulated := currTest.FieldsPopulated + FIELD_POPULATED_DATA;
          end
        else if (UCdataType = uppercase(DETAIL_DATA_LEN)) then
          begin
          currTest.DataLength := strtoint(data);
          currTest.FieldsPopulated := currTest.FieldsPopulated + FIELD_POPULATED_DATALEN;
          end
        else if (UCdataType = uppercase(DETAIL_DIGEST)) then
          begin
          currTest.Digest := data;
          currTest.FieldsPopulated := currTest.FieldsPopulated + FIELD_POPULATED_DIGEST;
          end
        else if (
                 (UCdataType = uppercase(DETAIL_KEY_DIGEST)) or
                 (UCdataType = uppercase(DETAIL_DIGEST96))
                ) then
          begin
          // Ignored...
          end
        else
          begin
          mmoReport.lines.add('ABORTING: Unrecognised line in test file, line '+inttostr(i));
          Result := FALSE;
          exit;
          end;


        if (currTest.FieldsPopulated = FIELDS_POPULATED_ALL) then
          begin
          inc(cntTotal);

          mmoReport.lines.add('Hash          : '+currTest.Hash);
          mmoReport.lines.add('TestCaseNo    : '+inttostr(currTest.TestCaseNo));
          mmoReport.lines.add('Key           : '+currTest.Key);
          mmoReport.lines.add('KeyLength     : '+inttostr(currTest.KeyLength)+' bytes');
          mmoReport.lines.add('Data          : '+currTest.Data);
          mmoReport.lines.add('DataLength    : '+inttostr(currTest.DataLength)+' bytes');
          mmoReport.lines.add('Expected HMAC : '+currTest.Digest);

          testResult := ExecuteTest(currTest, generatedHMAC, failureReason);
          mmoReport.lines.add('Generated HMAC: '+generatedHMAC);
          if testResult then
            begin
            mmoReport.lines.add('Result        : PASS');
            inc(cntOK);
            end
          else
            begin
            mmoReport.lines.add('Result        : FAILURE: '+failureReason);
            inc(cntFailed);
            end;

          mmoReport.lines.add('');
          currTest.FieldsPopulated := FIELDS_POPULATED_NONE;
          end;

        end;

    finally
      digest.Free();
    end;

  finally
    stlTestData.Free();
  end;

  mmoReport.lines.add('');
  mmoReport.lines.add('');
  mmoReport.lines.add('RESULTS SUMMARY');
  mmoReport.lines.add('===============');
  mmoReport.lines.add('Total tests: '+inttostr(cntTotal));
  mmoReport.lines.add('  Passed: '+inttostr(cntOK));
  mmoReport.lines.add('  Failed: '+inttostr(cntFailed));

  Result := (cntFailed = 0);
end;

// Generate HMAC, returning TRUE/FALSE if the generated HMAC matches the
// expected one.
// Either way, "generatedHMAC" will be set to the generated HMAC
// On failure, "failureReason" will be set to the reason why the HMAC failed
function TfrmMain.ExecuteTest(Test: TTestDetails; var generatedHMAC: string; var failureReason: string): boolean;
var
  retVal: boolean;
  MACValue: TMACValue;
  hashType: fhHashType;
  key: string;
  data: string;
  expected: string;
begin
  retVal := TRUE;
  failureReason := '';
  generatedHMAC := '';

  MACValue := TMACValue.Create();
  try
    key := Test.Key;
    data:= Test.Data;
    expected:= Test.Digest;

    if not(ConvertToBinary(key)) then
      begin
      failureReason := 'Unable to understand key';
      retVal := FALSE;
      end
    else if not(ConvertToBinary(data)) then
      begin
      failureReason := 'Unable to understand data';
      retVal := FALSE;
      end
    else if not(ConvertToBinary(expected)) then
      begin
      failureReason := 'Unable to understand expected digest';
      retVal := FALSE;
      end;

    // Handle special cases

    // Sanity checks...
    if (length(key) <> Test.KeyLength) then
      begin
      failureReason := 'Key length incorrect';
      retVal := FALSE;
      end
    else if (length(data) <> Test.DataLength) then
      begin
      failureReason := 'Data length incorrect';
      retVal := FALSE;
      end;

    if (retVal) then
      begin
      hashType := HashObj.GetHashTypeForTitle(Test.Hash);
      HashObj.ActiveHash := hashType;

      retVal := HMACString(key, data, HashObj, MACValue);

      if not(retVal) then
        begin
        failureReason := 'Unable to generate HMAC';
        end
      else
        begin
        generatedHMAC := lowercase('0x'+MACValue.ValueAsASCIIHex);
        
        if (MACValue.ValueAsBinary <> expected) then
          begin
          failureReason := 'HMAC value calculated doens''t match expected value';
          retVal := FALSE;
          end;
        end;

      end;

  finally
    MACValue.Free();
  end;

  Result := retVal;
end;


// Convert data from test file into binary string
function TfrmMain.ConvertToBinary(var data: string): boolean;
const
  // This is crude, but...
  SPECIAL_dd_x_50 = '0xdd repeated 50 times';
  SPECIAL_cd_x_50 = '0xcd repeated 50 times';
  SPECIAL_aa_x_80 = '0xaa repeated 80 times';
var
  retVal: boolean;
  tmpData: string;
begin
  retVal := FALSE;

  if (uppercase(data) = uppercase(SPECIAL_dd_x_50)) then
    begin
    data := StringOfChar(#$dd, 50);
    retVal := TRUE;
    end
  else if (uppercase(data) = uppercase(SPECIAL_cd_x_50)) then
    begin
    data := StringOfChar(#$cd, 50);
    retVal := TRUE;
    end
  else if (uppercase(data) = uppercase(SPECIAL_aa_x_80)) then
    begin
    data := StringOfChar(#$aa, 80);
    retVal := TRUE;
    end
  else if (Pos('0x', data) = 1) then
    begin
    // ASCII encoded hex values...
    // e.g. 0x123456...
    Delete(data, 1, 2);
    retVal := SDUParseASCIIToData(data, tmpData);
    data := tmpData;
    end
  else if (Pos('"', data) = 1) then
    begin
    // Literal string
    // e.g. "what do ya want for nothing?"
    Delete(data, 1, 1);
    Delete(data, Length(data), 1);
    retVal := TRUE;
    end;

  Result := retVal;
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  HashObj := THashAlgUnified.Create(nil);

  self.caption := Application.Title;

  edFilename.text := '..\docs\RFC2202_machine_readable_tests.txt';

  mmoReport.lines.clear();
  // Fixed width font
  mmoReport.Font.Name := 'Courier';

end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  HashObj.Free();
  
end;

procedure TfrmMain.pbBrowseClick(Sender: TObject);
begin
  SDUOpenSaveDialogSetup(OpenDialog1, edFilename.text);

  if OpenDialog1.Execute then
    begin
    edFilename.text := OpenDialog1.FileName;
    end;

end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  SDUCenterControl(pbTestHashes, ccHorizontal);

end;

END.

