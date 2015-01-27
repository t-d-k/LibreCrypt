unit CommonfrmAbout;
 // Description: About Dialog
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //


interface

uses
  Classes, Controls, Dialogs,
  ExtCtrls,
  Forms, Graphics, Messages, OTFEFreeOTFEBase_U, SDUForms, SDUStdCtrls, StdCtrls,
  SysUtils, Windows;

type
  TfrmAbout = class (TForm)
    pbOK:                TButton;
    imgIcon:             TImage;
    lblAppID:            TLabel;
    lblDescription:      TLabel;
    lblBeta:             TLabel;
    lblDriverVersion:    TLabel;
    lblAuthor:           TLabel;
    SDUURLLabel1:        TSDUURLLabel;
    lblTranslatorCredit: TLabel;
    Label1:              TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
  protected
  procedure SetDescription(value: String);
  PUBLIC
    FreeOTFEObj: TOTFEFreeOTFEBase;
    BetaNumber:  Integer;
    property description :string write SetDescription;
  end;

implementation

{$R *.DFM}

uses
  ShellApi,  // Needed for ShellExecute
  CommonConsts, SDUGeneral,
  SDUi18n;

procedure TfrmAbout.FormShow(Sender: TObject);
const
  CONTROL_MARGIN = 10;
var
  majorVersion:    Integer;
  minorVersion:    Integer;
  revisionVersion: Integer;
  buildVersion:    Integer;
  OTFEVersion:     String;
//  descAdjustDown:  Integer;
begin
  self.Caption     := Format(_('About %s'), [Application.Title]);
//  lblTitle.Caption := Application.Title;

//  lblAppID.left := lblTitle.left + lblTitle.Width + CONTROL_MARGIN;

  lblTranslatorCredit.Visible := False;
  if ((SDUGetCurrentLanguageCode() <> '') and not
    (SDUIsLanguageCodeEnglish(SDUGetCurrentLanguageCode()))) then begin
    lblTranslatorCredit.Visible := True;
    lblTranslatorCredit.Caption :=
      Format(_('%s translation by %s'),
      [_(CONST_LANGUAGE_ENGLISH), SDUGetTranslatorName()]);
  end;

  imgIcon.Picture.Assign(Application.Icon);

  SDUGetVersionInfo('', majorVersion, minorVersion, revisionVersion, buildVersion);
  lblAppID.Caption := 'v' + SDUGetVersionInfoString('');
  if BetaNumber > -1 then begin
    lblAppID.Caption := lblAppID.Caption + ' BETA ' + IntToStr(BetaNumber);
  end;

  lblBeta.Visible := (BetaNumber > -1);


  if FreeOTFEObj.Active then begin
    OTFEVersion := FreeOTFEObj.VersionStr();
    if (OTFEVersion <> '') then begin
      OTFEVersion := Format(_('FreeOTFE driver: %s'), [OTFEVersion]);
    end;
  end else begin
    OTFEVersion := _('The main DoxBox driver is either not installed, or not started');
  end;




  // Some translated languages may increase the number of lines the
  // description takes up. Here we increase the height of the dialog to
  // compensate, and nudge the controls below it down
  {
  descAdjustDown := (lblDescription.Top + lblDescription.Height + CONTROL_MARGIN) -
    pnlDividerUpper.Top;

  self.Height := self.Height + descAdjustDown;

  pnlDividerUpper.Top  := pnlDividerUpper.Top + descAdjustDown;
  SDUURLLabel1.Top     := SDUURLLabel1.Top + descAdjustDown;
  pnlDividerLower.Top  := pnlDividerLower.Top + descAdjustDown;
  lblDriverVersion.Top := lblDriverVersion.Top + descAdjustDown;
  pbOK.Top             := pbOK.Top + descAdjustDown;


  SDUCenterControl(lblDescription, ccHorizontal);  }

  lblDriverVersion.Caption := OTFEVersion;
//  SDUCenterControl(lblDriverVersion, ccHorizontal);

//  pnlDividerUpper.Caption := '';
//  pnlDividerLower.Caption := '';

end;

procedure TfrmAbout.SetDescription(value: String);
begin
    lblDescription.Caption := value;
end;

end.
