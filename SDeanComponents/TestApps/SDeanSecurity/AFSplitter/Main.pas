unit Main;
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
  StdCtrls, Spin64,
  HashAlg_U,
  HashAlgUnified_U, SDUDialogs;


type
  TfrmMain = class(TForm)
    seStripeCount: TSpinEdit64;
    Label1: TLabel;
    edFilenameInput: TEdit;
    Label2: TLabel;
    edFilenameOutput: TEdit;
    Label3: TLabel;
    pbSplit: TButton;
    pbMerge: TButton;
    pbBrowseInput: TButton;
    pbBrowseOutput: TButton;
    cbHash: TComboBox;
    Label4: TLabel;
    OpenDialog1: TSDUOpenDialog;
    SaveDialog1: TSDUSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure pbSplitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pbMergeClick(Sender: TObject);
    procedure pbBrowseInputClick(Sender: TObject);
    procedure pbBrowseOutputClick(Sender: TObject);
  private
    hashAlg: THashAlgUnified;

    procedure Execute(splitNotMerge: boolean);
    procedure PopulateHashes();
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  AFSplitter,
  SDUGeneral;


procedure TfrmMain.FormShow(Sender: TObject);
begin
  self.Caption := Application.Title;
  
end;

procedure TfrmMain.pbSplitClick(Sender: TObject);
begin
  Execute(TRUE);

end;

procedure TfrmMain.pbMergeClick(Sender: TObject);
begin
    Execute(FALSE);

end;


// splitNotMerge - Set to TRUE to split, FALSE to merge
procedure TfrmMain.Execute(splitNotMerge: boolean);
var
  allOK: boolean;
  input, output: string;
  datafile: TFileStream;
  fileLen: Longint;
  currHashAlg: fhHashType;
begin
  allOK := TRUE;

  // Read in the input file...
  if (allOK) then
    begin
    // Read in data from the inFile
    datafile := TFileStream.Create(edFilenameInput.text, fmOpenRead);
    try
      // Determine file size...
      datafile.Seek(0, soFromEnd);
      fileLen := datafile.Position;

      input := StringOfChar('Z', fileLen);

      // Read in the whole file...
      datafile.Seek(0, soFromBeginning);
      if (datafile.Read(input[1], fileLen) <> fileLen) then
        begin
        MessageDlg('Unable to read in entire file ('+inttostr(fileLen)+' bytes)', mtError, [mbOK], 0);
        allOK := FALSE;
        end;

    finally
      datafile.Free();
    end;

    end;


  // Setup the hash object...
  for currHashAlg:=low(fhHashType) to high(fhHashType) do
    begin
    THashAlgUnified(hashAlg).ActiveHash := currHashAlg;
    if (cbHash.Items[cbHash.ItemIndex] = hashAlg.Title) then
      begin
      // Located appropriate algorithm; exit loop.
      break;
      end;
    end;

    showmessage(inttostr(hashAlg.Digestsize));
    
  // Split/merge...
  if (allOK) then
    begin
    if splitNotMerge then
      begin
      allOK := AFSplit(
                       input,
                       seStripeCount.value,
                       hashAlg,
                       output
                      );
      end
    else
      begin
      allOK := AFMerge(
                       input,
                       seStripeCount.value,
                       hashAlg,
                       output
                      );
      end;

    end;


  // Write out the processed data...
  if (allOK) then
    begin
    // Write out data to the outFile
    datafile := TFileStream.Create(edFilenameOutput.text, fmCreate);
    try
      // Write the whole file...
      datafile.Seek(0, soFromBeginning);
      if (datafile.Write(output[1], Length(output)) <> Length(output)) then
        begin
        MessageDlg('Unable to write out processed data ('+inttostr(Length(output))+' bytes)', mtError, [mbOK], 0);
        allOK := FALSE;
        end;

    finally
      datafile.Free();
    end;
         
    end;

    
  // Report to user.
  if (allOK) then
    begin
    MessageDlg('Operation succeeded OK.', mtError, [mbOK], 0);
    end
  else
    begin
    MessageDlg('Operation failed.', mtInformation, [mbOK], 0);
    end;

end;


procedure TfrmMain.PopulateHashes();
var
  currHashAlg: fhHashType;
begin
  cbHash.Items.Clear();
  for currHashAlg:=low(fhHashType) to high(fhHashType) do
    begin
    THashAlgUnified(hashAlg).ActiveHash := currHashAlg;
    cbHash.Items.Add(hashAlg.Title);
    end;
    
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  hashAlg := THashAlgUnified.Create(nil);

  PopulateHashes();

  // Setup sensible defaults...
  edFilenameInput.text := '';
  edFilenameOutput.text := '';
  seStripeCount.value := 10;
  cbHash.ItemIndex := cbHash.Items.IndexOf('SHA-1');


end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  hashAlg.Free();

end;


procedure TfrmMain.pbBrowseInputClick(Sender: TObject);
begin
  SDUOpenSaveDialogSetup(OpenDialog1, edFilenameInput.Text);
  if OpenDialog1.Execute() then
    begin
    edFilenameInput.Text := OpenDialog1.Filename;
    end;

end;

procedure TfrmMain.pbBrowseOutputClick(Sender: TObject);
begin
  SDUOpenSaveDialogSetup(SaveDialog1, edFilenameOutput.Text);
  if SaveDialog1.Execute() then
    begin
    edFilenameOutput.Text := SaveDialog1.Filename;
    end;

end;

END.


