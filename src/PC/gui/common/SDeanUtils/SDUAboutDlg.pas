unit SDUAboutDlg;
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
  SDUStdCtrls, ComCtrls;

type
  TSDUAboutDialog = class(TForm)
    pbOK: TButton;
    imIcon: TImage;
    lblVersion: TLabel;
    lblTitle: TLabel;
    lblBeta: TLabel;
    lblAuthor: TLabel;
    pnlSeparatorUpper: TPanel;
    pnlSeparatorLower: TPanel;
    lblURL: TSDUURLLabel;
    reBlub: TRichEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTitle: WideString;
    FVersion: WideString;
    FBetaNumber: integer;
    FAuthor: WideString;
    FDescription: WideString;
    FURL: string;
    FURLCaption: WideString;

  published
    property Title: WideString read FTitle write FTitle;
    property Version: WideString read FVersion write FVersion;
    property BetaNumber: integer read FBetaNumber write FBetaNumber;
    property Author: WideString read FAuthor write FAuthor;
    property Description: WideString read FDescription write FDescription;
    property URL: string read FURL write FURL;
    property URLCaption: WideString read FURLCaption write FURLCaption;
  end;

implementation

{$R *.DFM}

uses
  SDUi18n,
  SDUGeneral;

procedure TSDUAboutDialog.FormShow(Sender: TObject);
begin
  self.Caption := SDUParamSubstitute(_('About %1'), [Title]);
  lblTitle.caption := Title;

  imIcon.picture.graphic := Application.Icon;

  lblVersion.caption := Version;

  if (BetaNumber > 0) then
    begin
    lblVersion.caption := lblVersion.caption + ' BETA '+inttostr(BetaNumber);
    end;
  lblBeta.visible := (BetaNumber > 0);

  // If there's no version ID, move the authors name up a bit so it's under the
  // application title
  if (lblVersion.caption = '') then
    begin
    lblVersion.visible := FALSE;
    lblAuthor.Top := lblVersion.Top;
    end;

  lblAuthor.Caption := SDUParamSubstitute(_('by %1'), [Author]);

  reBlub.Lines.Text := Description;


  lblURL.URL := URL;
  lblURL.Caption := FURLCaption;
  if (FURLCaption = '') then
    begin
    lblURL.Caption := URL;
    end;
  // If there's no version ID, move the authors name up a bit so it's under the
  // application title
  if (lblURL.caption = '') then
    begin
    lblVersion.visible := FALSE;
    pnlSeparatorUpper.visible := FALSE;
    pnlSeparatorLower.visible := FALSE;
    end
  else
    begin
    SDUCenterControl(lblURL, ccHorizontal);
    end;

  pnlSeparatorUpper.caption := '';
  pnlSeparatorLower.caption := '';

end;

procedure TSDUAboutDialog.FormCreate(Sender: TObject);
begin
  reBlub.Plaintext := TRUE;
  reBlub.Readonly := TRUE;
  reBlub.Scrollbars := ssNone;
  reBlub.Color := self.Color;
  reBlub.BorderStyle := bsNone;
  reBlub.Enabled := FALSE;

  // Setup defaults...
  Author      := 'Sarah Dean';  // Note: Not translated
  Title       := Application.Title;
  Version     := SDUGetVersionInfoString('');
  // Note: Version information may not have been set
  if (Version <> '') then
    begin
    Version := 'v'+Version;
    end;
  BetaNumber := -1;
  Description := 'Software description goes here...';
  URL         := 'http://www.SDean12.org/';
  URLCaption  := '';

end;

END.

