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
  TfrmAbout = class (TSDUForm)
    pbOK:                TButton;
    imgIcon:             TImage;
    lblAppID:            TLabel;
    lblDescription:      TLabel;
    lblTitle:            TLabel;
    lblBeta:             TLabel;
    lblDriverVersion:    TLabel;
    lblAuthor:           TLabel;
    pnlDividerUpper:     TPanel;
    pnlDividerLower:     TPanel;
    Label3:              TLabel;
    SDUURLLabel1:        TSDUURLLabel;
    lblTranslatorCredit: TLabel;
    Label1:              TLabel;
    procedure FormShow(Sender: TObject);
  PRIVATE
  PUBLIC
    FreeOTFEObj: TOTFEFreeOTFEBase;
    BetaNumber:  Integer;
    Description: String;
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
  descAdjustDown:  Integer;
begin
  self.Caption     := SDUParamSubstitute(_('About %1'), [Application.Title]);
  lblTitle.Caption := Application.Title;

  lblAppID.left := lblTitle.left + lblTitle.Width + CONTROL_MARGIN;

  lblTranslatorCredit.Visible := False;
  if ((SDUGetCurrentLanguageCode() <> '') and not
    (SDUIsLanguageCodeEnglish(SDUGetCurrentLanguageCode()))) then begin
    lblTranslatorCredit.Visible := True;
    lblTranslatorCredit.Caption :=
      SDUParamSubstitute(_('%1 translation by %2'),
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
      OTFEVersion := SDUParamSubstitute(_('DoxBox driver: %1'), [OTFEVersion]);
    end;
  end else begin
    OTFEVersion := _('The main DoxBox driver is either not installed, or not started');
  end;


  lblDescription.Caption := Description;

  // Some translated languages may increase the number of lines the
  // description takes up. Here we increase the height of the dialog to
  // compensate, and nudge the controls below it down
  descAdjustDown := (lblDescription.Top + lblDescription.Height + CONTROL_MARGIN) -
    pnlDividerUpper.Top;

  self.Height := self.Height + descAdjustDown;

  pnlDividerUpper.Top  := pnlDividerUpper.Top + descAdjustDown;
  SDUURLLabel1.Top     := SDUURLLabel1.Top + descAdjustDown;
  pnlDividerLower.Top  := pnlDividerLower.Top + descAdjustDown;
  lblDriverVersion.Top := lblDriverVersion.Top + descAdjustDown;
  pbOK.Top             := pbOK.Top + descAdjustDown;


  SDUCenterControl(lblDescription, ccHorizontal);

  lblDriverVersion.Caption := OTFEVersion;
  SDUCenterControl(lblDriverVersion, ccHorizontal);

  pnlDividerUpper.Caption := '';
  pnlDividerLower.Caption := '';

end;

end.
