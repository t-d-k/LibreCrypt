unit OTFEFreeOTFE_frmNewVolumeSize;
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
  StdCtrls, Spin64, SDUForms, SDUFrames, SDUSpin64Units, SDUFilenameEdit_U,
  OTFEFreeOTFEBase_U;

type
  TfrmNewVolumeSize = class(TSDUForm)
    pbCancel: TButton;
    pbOK: TButton;
    Label2: TLabel;
    Label3: TLabel;
    se64UnitSize: TSDUSpin64Unit_Storage;
    feFilename: TSDUFilenameEdit;
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFilename: string;
    FVolumeSize: int64;
  protected
    function  GetFilter(): string;
    procedure SetFilter(new: string);
    function  GetDefaultExt(): string;
    procedure SetDefaultExt(new: string);

  public
    property Filename: string read FFilename write FFilename;
    property VolumeSize: int64 read FVolumeSize write FVolumeSize;

    property Filter: string read GetFilter write SetFilter;
    property DefaultExt: string read GetDefaultExt write SetDefaultExt;
  end;


implementation

{$R *.DFM}

uses
  SDUi18n,
  SDUGeneral,
  SDUDialogs,
  OTFEFreeOTFE_U;

procedure TfrmNewVolumeSize.pbOKClick(Sender: TObject);
begin
  if (feFilename.Filename = '') then
    begin
    SDUMessageDlg(
               _('Please specify a filename for the new volume file by clicking the "..." button.'),
               mtWarning
              );
    exit;
    end;

  // Calculate the number of bytes...
  // Note: The in64(...) cast is REQUIRED, otherwise Delphi will calculate the
  //       value in 32 bits, and assign it to the 64 bit VolumeSize
  VolumeSize := se64UnitSize.Value;

  Filename := feFilename.Filename;
   
  ModalResult := mrOK;

end;

procedure TfrmNewVolumeSize.FormShow(Sender: TObject);
begin
  se64UnitSize.Value := DEFAULT_VOLUME_SIZE;
  feFilename.Filename := '';
  feFilename.OpenDialog.Options := feFilename.OpenDialog.Options + [ofDontAddToRecent];
  feFilename.SaveDialog.Options := feFilename.SaveDialog.Options + [ofDontAddToRecent];

end;

function TfrmNewVolumeSize.GetFilter(): string;
begin
  Result := feFilename.Filter;
end;

procedure TfrmNewVolumeSize.SetFilter(new: string);
begin
  feFilename.Filter := new;
end;

function TfrmNewVolumeSize.GetDefaultExt(): string;
begin
  Result := feFilename.DefaultExt;
end;

procedure TfrmNewVolumeSize.SetDefaultExt(new: string);
begin
  feFilename.DefaultExt := new;
end;

END.



