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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,
  OTFEFreeOTFEBase_U, SDUStdCtrls, SDUForms;

type
  TfrmAbout = class(TSDUForm)
    pbOK: TButton;
    imgIcon: TImage;
    lblAppID: TLabel;
    lblDescription: TLabel;
    lblTitle: TLabel;
    lblBeta: TLabel;
    lblDriverVersion: TLabel;
    lblAuthor: TLabel;
    pnlDividerUpper: TPanel;
    pnlDividerLower: TPanel;
    Label3: TLabel;
    SDUURLLabel1: TSDUURLLabel;
    lblTranslatorCredit: TLabel;
    procedure FormShow(Sender: TObject);
  private
  public
    FreeOTFEObj: TOTFEFreeOTFEBase;
    BetaNumber: integer;
    Description: string;
  end;

implementation

{$R *.DFM}

uses
  ShellApi,  // Needed for ShellExecute
  SDUi18n,
  SDUGeneral,
  CommonConsts;

procedure TfrmAbout.FormShow(Sender: TObject);
const
  CONTROL_MARGIN = 10;
var
  majorVersion   : integer;
  minorVersion   : integer;
  revisionVersion: integer;
  buildVersion   : integer;
  OTFEVersion: string;
  descAdjustDown: integer;
begin
  self.Caption := SDUParamSubstitute(_('About %1'), [Application.Title]);
  lblTitle.caption := Application.Title;

  lblAppID.left := lblTitle.left + lblTitle.width + CONTROL_MARGIN;

  lblTranslatorCredit.Visible := FALSE;
  if (
      (SDUGetCurrentLanguageCode() <> '') and
      not(SDUIsLanguageCodeEnglish(SDUGetCurrentLanguageCode()))
     ) then
    begin
    lblTranslatorCredit.Visible := TRUE;
    lblTranslatorCredit.Caption := SDUParamSubstitute(
                                                      _('%1 translation by %2'),
                                                      [_(CONST_LANGUAGE_ENGLISH), SDUGetTranslatorName()]
                                                     );
    end;

  imgIcon.Picture.Assign(Application.Icon);

  SDUGetVersionInfo('', majorVersion, minorVersion, revisionVersion, buildVersion);
  lblAppID.caption := 'v'+SDUGetVersionInfoString('');
  if BetaNumber>-1 then
    begin
    lblAppID.caption := lblAppID.caption + ' BETA '+inttostr(BetaNumber);
    end;

  lblBeta.visible := (BetaNumber>-1);


  if FreeOTFEObj.Active then
    begin
    OTFEVersion := FreeOTFEObj.VersionStr();
    if (OTFEVersion<>'') then
      begin
      OTFEVersion := SDUParamSubstitute(_('FreeOTFE driver: %1'), [OTFEVersion]);
      end;
    end
  else
    begin
    OTFEVersion := _('The main FreeOTFE driver is either not installed, or not started');
    end;


  lblDescription.caption := Description;
  
  // Some translated languages may increase the number of lines the
  // description takes up. Here we increase the height of the dialog to
  // compensate, and nudge the controls below it down
  descAdjustDown := (
                     lblDescription.Top +
                     lblDescription.Height +
                     CONTROL_MARGIN
                    ) - pnlDividerUpper.Top;

  self.height := self.height + descAdjustDown;

  pnlDividerUpper.Top  := pnlDividerUpper.Top + descAdjustDown;
  SDUURLLabel1.Top     := SDUURLLabel1.Top + descAdjustDown;
  pnlDividerLower.Top  := pnlDividerLower.Top + descAdjustDown;
  lblDriverVersion.Top := lblDriverVersion.Top + descAdjustDown;
  pbOK.Top             := pbOK.Top + descAdjustDown;


  SDUCenterControl(lblDescription, ccHorizontal);

  lblDriverVersion.caption := OTFEVersion;
  SDUCenterControl(lblDriverVersion, ccHorizontal);

  pnlDividerUpper.Caption := '';
  pnlDividerLower.Caption := '';

end;

END.

