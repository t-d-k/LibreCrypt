unit frmSelectOverwriteMethod;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  //delphi & libs
  Classes, ComCtrls, Controls, Dialogs,
  Forms, Graphics, Messages, StdCtrls,
  SysUtils, Windows,
  //sdu & LibreCrypt utils
  OTFEFreeOTFE_InstructionRichEdit, OTFEFreeOTFE_U, lcTypes,
  // LibreCrypt forms
  SDUForms;

type
  TfrmSelectOverwriteMethod = class (TSDUForm)
    pbOK:      TButton;
    pbCancel:  TButton;
    GroupBox1: TGroupBox;
    rbDataEncrypted: TRadioButton;
    rbDataPseudorandom: TRadioButton;
    cbCypher:  TComboBox;
    pbCypherDetails: TButton;
    lblInstructOverwriteType1: TLabel;
    lblInstructOverwriteType2: TLabel;
    lblInstructOverwriteType3: TLabel;
    procedure pbOKClick(Sender: TObject);
    procedure pbCypherDetailsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fCypherKernelModeDriverNames: TStringList;
    fCypherGUIDList: TStringList;

    fOverwriteWithEncryptedData: Boolean;
    fcypherDriver: Ansistring;
    fCypherGUID:   TGUID;
    fdrive:        DriveLetterChar; //drive to overwrite
    foverwriteEntireDrive: Boolean;
    procedure _EnableDisableControls();
  public

  end;

 // overwriteEntireDrive - Set to TRUE to overwrite the entire drive; this will
 // destroy all data on the drive, and requiring it to be
 // reformatted. Set to FALSE to simply overwrite the
 // free space on the drive
procedure OverwriteDrive(drive: DriveLetterChar;
  overwriteEntireDrive: Boolean;
  Aowner: TComponent);


implementation

{$R *.DFM}

uses
           //delphi & libs (0)
  ComObj,  // Required for StringToGUID
           //sdu & LibreCrypt utils (1)

  lcConsts,
  lcDialogs,
  SDUGeneral, SDUi18n, OTFEFreeOTFEBase_U,
  Shredder,
  MouseRNGDialog_U,
  // LibreCrypt forms and frames (2)
  frmCypherInfo;

{$IFDEF _NEVER_DEFINED}
// This is just a dummy const to fool dxGetText when extracting message
// information
// This const is never used; it's #ifdef'd out - SDUCRLF in the code refers to
// picks up SDUGeneral.SDUCRLF
const
  SDUCRLF = ''#13#10;
{$ENDIF}

procedure TfrmSelectOverwriteMethod.pbOKClick(Sender: TObject);
var
  allOK:                Boolean;
  chaff_cypher_details: TFreeOTFECypher_v3;
  chaff_cypher_key:     TSDUBytes;
  reqRandomBits:        Integer;
  aMouseRNGDialog:      TMouseRNGDialog;
  randomBuffer:         array of Byte;
  aShredder:            TShredder;
  i:                    Integer;
  //  currDrive:            DriveLetterChar;
  failMsg:              String;
  currDriveDevice:      String;
  overwriteOK:          TShredResult;
begin
  allOK       := True;
  //overwriteOK := srSuccess;
  fOverwriteWithEncryptedData := rbDataEncrypted.Checked;
  if fOverwriteWithEncryptedData then begin
    fcypherDriver := fcypherKernelModeDriverNames[cbCypher.ItemIndex];
    fCypherGUID   := StringToGUID(fCypherGUIDList[cbCypher.ItemIndex]);
  end;

  if (fOverwriteWithEncryptedData) then begin
    //          chaff_cypher_driver := frmOverWriteMethod.fcypherDriver;
    //          chaff_cypher_GUID   := frmOverWriteMethod.fCypherGUID;

    GetFreeOTFE().GetSpecificCypherDetails(fcypherDriver,
      fCypherGUID,
      chaff_cypher_details);

    // Initilize zeroed IV for encryption
    // ftempCypherEncBlockNo := 0;

    // Get *real* random data for encryption key
    reqRandomBits := 0;
    if (chaff_cypher_details.KeySizeRequired < 0) then begin
      // -ve keysize = arbitary keysize supported - just use 512 bits
      reqRandomBits := 512;
    end;

    if (chaff_cypher_details.KeySizeRequired > 0) then begin
      // +ve keysize = specific keysize - get sufficient bits
      reqRandomBits := chaff_cypher_details.KeySizeRequired;
    end;

    aMouseRNGDialog := TMouseRNGDialog.Create();
    try


      { TODO 1 -otdk -cenhance : use cryptoapi etc if avail }
      if (reqRandomBits > 0) then begin
        aMouseRNGDialog.RequiredBits := reqRandomBits;
        allOK := aMouseRNGDialog.Execute();
      end;

      if (allOK) then begin
        setlength(randomBuffer, (reqRandomBits div 8));
        aMouseRNGDialog.RandomData(reqRandomBits, randomBuffer);

        for i := low(randomBuffer) to high(randomBuffer) do begin
          SDUAddByte(chaff_cypher_key, randomBuffer[i]);
          // Overwrite the temp buffer...
          randomBuffer[i] := Random(256);
        end;
        setlength(randomBuffer, 0);
      end;

    finally
      aMouseRNGDialog.Free;
    end;
  end;

  if (allOK) then begin
    if SDUConfirmYN(_('Overwriting a large container may take a while.') +
      SDUCRLF + SDUCRLF + _('Are you sure you wish to proceed?')) then begin

      aShredder := TShredder.Create();
      try
        aShredder.IntMethod := smPseudorandom;
        aShredder.IntPasses := 1;

        if fOverwriteWithEncryptedData then begin
          // Note: Setting this event overrides shredder.IntMethod
          aShredder.OnTweakEncryptDataEvent := GetFreeOtfeBase().EncryptSectorData;
          aShredder.WipeCypherBlockSize     := chaff_cypher_details.BlockSize;
          aShredder.WipeCypherKey           := chaff_cypher_key;
          aShredder.wipeCypherDriver        := fcypherDriver;
          aShredder.WipeCypherGUID          := fCypherGUID;

          // shredder.OnOverwriteDataReq := GenerateOverwriteData;
        end else begin
          aShredder.OnTweakEncryptDataEvent := nil;
        end;

        //        for i := 1 to length(fdrives) do begin
        //          currDrive := fdrive;

        if foverwriteEntireDrive then begin
          currDriveDevice := '\\.\' + fdrive + ':';
          aShredder.DestroyDevice(currDriveDevice, False, False);
          { done 2 -otdk -cbug : only looks at last result }
          overwriteOK := aShredder.LastIntShredResult;
        end else begin
          overwriteOK := aShredder.WipeDriveFreeSpace(fdrive, False);
        end;

        //        end;

        if (overwriteOK = srSuccess) then begin
          SDUMessageDlg(_('Wipe operation complete.'), mtInformation);
          if foverwriteEntireDrive then begin
            SDUMessageDlg(_(
              'Please reformat the drive just wiped before attempting to use it.'),
              mtInformation);
          end;
        end;

        if (overwriteOK = srError) then begin
          failMsg := _('Wipe operation FAILED.');
          if not (foverwriteEntireDrive) then begin
            failMsg := failMsg + SDUCRLF + SDUCRLF +
              _('Did you remember to format this drive first?');
          end;

          SDUMessageDlg(failMsg, mtError);
        end;

        if (overwriteOK = srUserCancel) then begin
          SDUMessageDlg(_('Wipe operation cancelled by user.'),
            mtInformation);
          //        end else begin
          //          SDUMessageDlg(_('No drives selected to overwrite?'),
          //            mtInformation);
        end;
      finally
        aShredder.Free();
      end;

    end; // if (SDUMessageDlg(
  end;   // if (allOK) then

  fCypherGUID := StringToGUID('{00000000-0000-0000-0000-000000000000}');
  SDUZeroBuffer(chaff_cypher_key);
  ModalResult := mrOk;

end;


procedure TfrmSelectOverwriteMethod.pbCypherDetailsClick(Sender: TObject);
begin
  frmCypherInfo.ShowCypherDetailsDlg(
    fcypherKernelModeDriverNames[cbCypher.ItemIndex],
    StringToGUID(fCypherGUIDList[cbCypher.ItemIndex])
    );

end;

procedure TfrmSelectOverwriteMethod.FormShow(Sender: TObject);
var
  tmpDisplayTitles: TStringList;
begin
  { TODO 2 -otdk -csecurity : default to secure wipe }


  tmpDisplayTitles := TStringList.Create();
  try
    if (GetFreeOTFE().GetCypherList(tmpDisplayTitles, fcypherKernelModeDriverNames,
      fCypherGUIDList)) then begin
      cbCypher.Items.Clear();
      cbCypher.Items.AddStrings(tmpDisplayTitles);
    end else begin
      SDUMessageDlg(
        _('Unable to obtain list of cyphers.') + SDUCRLF + SDUCRLF +
        _('Please ensure that you have one or more LibreCrypt cypher drivers installed and started.') + SDUCRLF + SDUCRLF + _(
        'If you have only just installed LibreCrypt, you may need to restart your computer.'),
        mtError
        );
    end;
  finally
    tmpDisplayTitles.Free();
  end;

  _EnableDisableControls();

end;

procedure TfrmSelectOverwriteMethod.FormCreate(Sender: TObject);
begin
  inherited;
  fcypherKernelModeDriverNames := TStringList.Create();
  fCypherGUIDList              := TStringList.Create();
end;

procedure TfrmSelectOverwriteMethod.FormDestroy(Sender: TObject);
begin
  inherited;
  fcypherKernelModeDriverNames.Free();
  fCypherGUIDList.Free();
end;

procedure TfrmSelectOverwriteMethod.ControlChanged(Sender: TObject);
begin
  _EnableDisableControls();
end;


procedure TfrmSelectOverwriteMethod._EnableDisableControls();
begin
  cbCypher.Enabled        := rbDataEncrypted.Checked;
  pbCypherDetails.Enabled := (cbCypher.ItemIndex >= 0) and cbCypher.Enabled;

  // Either the pseudorandom radiobutton is checked, or a cypher has been
  // selected (if the pseudorandom radiobutton *isn't* checked, it's implicit
  // that the encrypted data radiobutton *is* checked)
  pbOK.Enabled := (rbDataPseudorandom.Checked or (cbCypher.ItemIndex >= 0));

end;


 { TODO 2 -otdk -cui : do one drive at a time only - too risky for many }
 // overwriteEntireDrive - Set to TRUE to wipe the entire drive; this will
 // destroy all data on the drive, and requiring it to be
 // reformatted. Set to FALSE to simply overwrite the
 // free space on the drive
procedure OverwriteDrive(drive: DriveLetterChar;
  overwriteEntireDrive: Boolean;
  Aowner: TComponent);
var
  frmOverWriteMethod: TfrmSelectOverwriteMethod;
begin

  frmOverWriteMethod := TfrmSelectOverwriteMethod.Create(Aowner);
  try
    frmOverWriteMethod.fdrive                := drive;
    frmOverWriteMethod.foverwriteEntireDrive := overwriteEntireDrive;
    frmOverWriteMethod.ShowModal();

  finally
    frmOverWriteMethod.Free();
  end;
end;


end.
