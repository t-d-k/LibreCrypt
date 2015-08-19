unit frmAbout;
 // Description: About Dialog
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.FreeOTFE.org/
 //
 // -----------------------------------------------------------------------------
 //
  // layer 2

interface

uses
  //delphi and 3rd party libs - layer 0
  Classes, Controls, Dialogs,
  ExtCtrls,
  Forms, Graphics, Messages, StdCtrls,
  SysUtils, Windows,
  //sdu  - layer 1
  SDUForms, SDUStdCtrls,
  //LibreCrypt utils -also layer 1
  OTFEFreeOTFEBase_U
   // LibreCrypt forms - layer 2
    //main form - layer 3
  ;

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
    SDUURLLabel2:        TSDUURLLabel;
    SDUURLLabel3:        TSDUURLLabel;
    SDUURLLabel4:        TSDUURLLabel;
    SDUURLLabel5:        TSDUURLLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  PROTECTED

    //     procedure SetDescription(value: String);
  PUBLIC

  end;


implementation

{$R *.DFM}

uses
             //delphi and 3rd party libs - layer 0
  ShellApi,  // Needed for ShellExecute
  CommonConsts,
  //sdu & LibreCrypt utils  - layer 1
  SDUGeneral,
  SDUi18n,
  lcConsts    // for APP_BETA_BUILD
  // LibreCrypt forms - layer 2
  //main form - layer 3
  ;

const
   {$IFDEF FREEOTFE_MAIN}
  APP_DESCRIPTION = 'LibreCrypt: Open-Source Transparent Encryption';
  {$ELSE}
  APP_DESCRIPTION =
    'LibreCrypt Explorer: Access containers without administrator rights';
  {$ENDIF}

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  lblDescription.Caption := _(APP_DESCRIPTION);

end;

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
  self.Caption := Format(_('About %s'), [Application.Title]);
  //  lblTitle.Caption := Application.Title;

  //  lblAppID.left := lblTitle.left + lblTitle.Width + CONTROL_MARGIN;

  //  lblTranslatorCredit.Visible := False;
  if ((SDUGetCurrentLanguageCode() <> '') and not
    (SDUIsLanguageCodeEnglish(SDUGetCurrentLanguageCode()))) then begin
    //    lblTranslatorCredit.Visible := True;
    lblTranslatorCredit.Caption :=
      Format(_('%s translation by %s'), [_(CONST_LANGUAGE_ENGLISH), SDUGetTranslatorName()]);
  end;

  imgIcon.Picture.Assign(Application.Icon);

  SDUGetVersionInfo('', majorVersion, minorVersion, revisionVersion, buildVersion);
  lblAppID.Caption := 'v' + SDUGetVersionInfoString('');
  if APP_BETA_BUILD > -1 then begin
    lblAppID.Caption := lblAppID.Caption + ' BETA ' + IntToStr(APP_BETA_BUILD);
  end;

  lblBeta.Visible := (APP_BETA_BUILD > -1);


  if GetFreeOTFEBase().Active then begin
    OTFEVersion := GetFreeOTFEBase().VersionStr();
    if (OTFEVersion <> '') then begin
      OTFEVersion := Format(_('FreeOTFE driver: %s'), [OTFEVersion]);
    end;
  end else begin
    OTFEVersion := _('The main LibreCrypt driver is either not installed, or not started');
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



end.
