unit frmNewVolumeSize;
 // Description: 
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  Classes, Controls, Dialogs,
  Forms, Graphics, Messages, OTFEFreeOTFEBase_U, SDUFilenameEdit_U,
  SDUForms, SDUFrames, SDUSpin64Units, Spin64, StdCtrls, SysUtils, Windows;

type
  TfrmNewVolumeSize = class (TSDUForm)
    pbCancel:     TButton;
    pbOK:         TButton;
    Label2:       TLabel;
    Label3:       TLabel;
    se64UnitSize: TSDUSpin64Unit_Storage;
    feFilename:   TSDUFilenameEdit;
    procedure pbOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FFilename:   String;
    FVolumeSize: Int64;
  protected
    function GetFilter(): String;
    procedure SetFilter(new: String);
    function GetDefaultExt(): String;
    procedure SetDefaultExt(new: String);

  public
    property Filename: String Read FFilename Write FFilename;
    property VolumeSize: Int64 Read FVolumeSize Write FVolumeSize;

    property Filter: String Read GetFilter Write SetFilter;
    property DefaultExt: String Read GetDefaultExt Write SetDefaultExt;
  end;


implementation

{$R *.DFM}

uses
  lcDialogs,
  OTFEFreeOTFE_U, SDUGeneral,
  SDUi18n;

procedure TfrmNewVolumeSize.pbOKClick(Sender: TObject);
begin
  if (feFilename.Filename = '') then begin
    SDUMessageDlg(
      _('Please specify a filename for the new container by clicking the "..." button.'),
      mtWarning
      );
    exit;
  end;

  // Calculate the number of bytes...
  // Note: The in64(...) cast is REQUIRED, otherwise Delphi will calculate the
  //       value in 32 bits, and assign it to the 64 bit VolumeSize
  VolumeSize := se64UnitSize.Value;

  Filename := feFilename.Filename;

  ModalResult := mrOk;

end;

procedure TfrmNewVolumeSize.FormShow(Sender: TObject);
begin
  se64UnitSize.Value            := DEFAULT_VOLUME_SIZE;
  feFilename.Filename           := '';
  feFilename.OpenDialog.Options := feFilename.OpenDialog.Options + [ofDontAddToRecent];
  feFilename.SaveDialog.Options := feFilename.SaveDialog.Options + [ofDontAddToRecent];

end;

function TfrmNewVolumeSize.GetFilter(): String;
begin
  Result := feFilename.Filter;
end;

procedure TfrmNewVolumeSize.SetFilter(new: String);
begin
  feFilename.Filter := new;
end;

function TfrmNewVolumeSize.GetDefaultExt(): String;
begin
  Result := feFilename.DefaultExt;
end;

procedure TfrmNewVolumeSize.SetDefaultExt(new: String);
begin
  feFilename.DefaultExt := new;
end;

end.
