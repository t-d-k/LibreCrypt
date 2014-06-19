unit Form_Main;
// Description: Hash Test Vector Suite
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//
// This test application verifies the hash implementations by checking their
// output against known test vectors stored separatly in a file


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  HashAlgUnified_U;

type
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
    procedure pbCloseClick(Sender: TObject);
  private
    HashObj: THashAlgUnified;
  public
    function SelfTest(filename: string): boolean;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  SDUGeneral,
  HashValue_U;


procedure TfrmMain.pbTestHashesClick(Sender: TObject);
begin
  SelfTest(edFilename.text);

end;

function TfrmMain.SelfTest(filename: string): boolean;
const
  TEST_1_MILLION_a = '<1 million "a" characters>';
  TEST_64K_OF_BINARY_DATA = '<64K OF BINARY DATA>';
  TEST_DATA_LINE = 'Test data';
var
  stlTestData: TStringList;
  i, j: integer;
  testData: string;
  cntOK: integer;
  cntFailed: integer;
  cntTotalHashes: integer;
  cntTotalTestStrings: integer;
  dataType: string;
  data: string;
  digest: THashValue;
  currHashType: fhHashType;
  calcHash: string;
  ht: fhHashType;
  cntOKByHash: array [fhHashType] of integer;
  cntFailedByHash: array [fhHashType] of integer;
begin
  cntOK := 0;
  cntFailed := 0;
  cntTotalHashes := 0;
  cntTotalTestStrings := 0;
  for ht:=low(fhHashType) to high(fhHashType) do
    begin
    cntOKByHash[ht] := 0;
    cntFailedByHash[ht] := 0;
    end;


  stlTestData:= TStringList.Create();
  try
    mmoReport.lines.Clear();
    mmoReport.lines.add('Reading test data from file:');
    mmoReport.lines.add(filename);
    mmoReport.lines.add('');

    stlTestData.LoadFromFile(filename);

    digest:= THashValue.Create();
    try
      for i:=0 to (stlTestData.count-1) do
        begin
        // Disregard comment and blank lines
        if (
            (Trim(stlTestData[i]) = '') or
            (Pos('#', Trim(stlTestData[i])) > 0)
           ) then
          begin
          continue;
          end;

        // Split into test

        SDUSplitString(stlTestData[i], dataType, data, ':');
        dataType := Trim(dataType);
        data     := Trim(data);


        if (uppercase(dataType) = uppercase(TEST_DATA_LINE)) then
          begin
          inc(cntTotalTestStrings);

          mmoReport.lines.add('');
          mmoReport.lines.add('--------------');

          // If line is input data, store
          // Special case, 1 million "a" characters
          if (uppercase(data) = uppercase(TEST_1_MILLION_a)) then
            begin
            testData := StringOfChar('a', 1000000);
            mmoReport.lines.add('Test data: '+TEST_1_MILLION_a);
            mmoReport.lines.add('');
            end
          else if (uppercase(data) = uppercase(TEST_64K_OF_BINARY_DATA)) then
            begin
            testData := '';
            for j:=0 to 65535 do
              begin
              testData := testData + char(j and $FF);
              end;

            mmoReport.lines.add('Test data: '+TEST_64K_OF_BINARY_DATA);
            mmoReport.lines.add('');
            end
          else
            begin
            // Strip off any quotes
            testData := Copy(data, 2, (length(data)-2));
            mmoReport.lines.add('Test data: "'+testData+'"');
            end;

          mmoReport.lines.add('');
          continue;
          end;

        // Calculate hash and compare
        inc(cntTotalHashes);

        // Setup correct hash...
        data := lowercase(data);
        mmoReport.lines.add('Hash: '+dataType);
        mmoReport.lines.add('Expecting : '+data);

        currHashType := HashObj.GetHashTypeForTitle(dataType);
        HashObj.ActiveHash := currHashType;

        if HashObj.HashString(testData, digest) then
          begin
          calcHash := lowercase(HashObj.PrettyPrintHashValue(digest));
          mmoReport.lines.add('Calculated: '+calcHash);

          if (uppercase(calcHash) = uppercase(data)) then
            begin
            mmoReport.lines.add('Result: PASS');
            inc(cntOK);
            inc(cntOKByHash[currHashType]);
            end
          else
            begin
            mmoReport.lines.add('Result: FAILURE: Incorrect hash value generated');
            inc(cntFailed);
            inc(cntFailedByHash[currHashType]);
            end;

          end
        else
          begin
          mmoReport.lines.add('Result: FAILURE: Unable to generate hash value');
          inc(cntFailed);
          inc(cntFailedByHash[currHashType]);
          end;

          mmoReport.lines.add('');
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
  mmoReport.lines.add('Total test strings: '+inttostr(cntTotalTestStrings));
  mmoReport.lines.add('');

  mmoReport.lines.add('Total hashes to calculate: '+inttostr(cntTotalHashes));
  mmoReport.lines.add('  Passed: '+inttostr(cntOK));
  mmoReport.lines.add('  Failed: '+inttostr(cntFailed));
  mmoReport.lines.add('');
  mmoReport.lines.add('Breakdown by hash algorithm:');
  mmoReport.lines.add(Format(
                             '%-20s   %5s   %5s',
                             [
                              'Hash',
                              'Passed',
                              'Failed'
                             ]
                            ));
  for ht:=low(fhHashType) to high(fhHashType) do
    begin
    mmoReport.lines.add(Format(
                               '%-20s   %5s   %5s',
                               [
                                HashObj.GetHashTitleForType(ht),
                                inttostr(cntOKByHash[ht]),
                                inttostr(cntFailedByHash[ht])
                               ]
                              ));
    end;


  Result := (cntFailed = 0);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  HashObj := THashAlgUnified.Create(nil);

  self.caption := Application.Title;

  edFilename.text := '..\docs\TestVectors.txt';

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

procedure TfrmMain.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  SDUCenterControl(pbTestHashes, ccHorizontal);

end;

END.

