unit FreeOTFEExplorerfrmPropertiesDlg_Base;

interface

uses
  Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, Forms,
  Graphics, Messages, SDFilesystem_FAT, SDUForms,
  StdCtrls, SysUtils, Variants, Windows;

type
  TfrmPropertiesDialog_Base = class (TSDUForm)
    pbOK:          TButton;
    pbApply:       TButton;
    pbCancel:      TButton;
    PageControl1:  TPageControl;
    tsGeneral:     TTabSheet;
    edFileType:    TLabel;
    lblFileType:   TLabel;
    edLocation:    TLabel;
    lblSizeOnDisk: TLabel;
    edSizeOnDisk:  TLabel;
    lblSize:       TLabel;
    edSize:        TLabel;
    lblLocation:   TLabel;
    lblAttributes: TLabel;
    ckReadOnly:    TCheckBox;
    ckHidden:      TCheckBox;
    edFilename:    TEdit;
    Panel2:        TPanel;
    Panel3:        TPanel;
    imgFileType:   TImage;
    ckArchive:     TCheckBox;
    procedure pbOKClick(Sender: TObject);
    procedure pbApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PROTECTED
    procedure SetupCtlCheckbox(chkBox: TCheckbox);
    procedure SetupCtlSeparatorPanel(pnl: TPanel);
  PUBLIC
    Filesystem: TSDFilesystem_FAT;
  end;

implementation

{$R *.dfm}

uses
  SDUGeneral,
  SDUGraphics,
  SDUi18n;

procedure TfrmPropertiesDialog_Base.FormShow(Sender: TObject);
begin
  PageControl1.ActivePage := tsGeneral;

  SetupCtlSeparatorPanel(Panel2);
  SetupCtlSeparatorPanel(Panel3);

  // Should be enabled for individual files and directories, but since we
  // don't support allowing the user to change the directory/filenames from
  // here (yet)...
  edFilename.ReadOnly := True;
  edFilename.Color    := clBtnFace;
  edFilename.Text     := '';

  SetupCtlCheckbox(ckReadOnly);
  SetupCtlCheckbox(ckHidden);

  edSizeOnDisk.Caption := _('<not calculated>');

  // Updating not currently supported.
  SDUEnableControl(pbOK, False);
  SDUEnableControl(pbApply, False);

end;

procedure TfrmPropertiesDialog_Base.pbApplyClick(Sender: TObject);
begin
  // Do nothing...
end;

procedure TfrmPropertiesDialog_Base.pbOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmPropertiesDialog_Base.SetupCtlCheckbox(chkBox: TCheckbox);
begin
  // Nothing - should really set to readonly, since we don't support letting
  // the user change these
end;

procedure TfrmPropertiesDialog_Base.SetupCtlSeparatorPanel(pnl: TPanel);
begin
  pnl.Caption    := '';
  pnl.Height     := 3;
  pnl.BevelOuter := bvLowered;
  pnl.BevelInner := bvNone;
end;

end.
