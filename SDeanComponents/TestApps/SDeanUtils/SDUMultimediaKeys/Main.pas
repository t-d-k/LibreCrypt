unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SDUMultimediaKeys, StdCtrls, ComCtrls,
  SDUForms;

type
  TForm1 = class(TSDUForm)
    SDUMultimediaKeys1: TSDUMultimediaKeys;
    reReport: TRichEdit;
    Label1: TLabel;
    pbClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure SDUMultimediaKeys1MultimediaKeypress(Sender: TObject;
      var MultimediaKey: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
begin
  SDUMultimediaKeys1.Active := TRUE;
  reReport.Lines.Clear();
  self.Caption := Application.title;
end;

procedure TForm1.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TForm1.SDUMultimediaKeys1MultimediaKeypress(Sender: TObject;
  var MultimediaKey: Word; Shift: TShiftState);
var
  keyAsText: string;
begin
  case MultimediaKey of
    APPCOMMAND_BROWSER_BACKWARD:     keyAsText := 'Browser backward';
    APPCOMMAND_BROWSER_FORWARD:      keyAsText := 'Browser forward';
    APPCOMMAND_BROWSER_REFRESH:      keyAsText := 'Browser refresh';
    APPCOMMAND_BROWSER_STOP:         keyAsText := 'Browser stop';
    APPCOMMAND_BROWSER_SEARCH:       keyAsText := 'Browser search';
    APPCOMMAND_BROWSER_FAVORITES:    keyAsText := 'Browser favorites';
    APPCOMMAND_BROWSER_HOME:         keyAsText := 'Browser home';
    APPCOMMAND_VOLUME_MUTE:          keyAsText := 'Volume mute';
    APPCOMMAND_VOLUME_DOWN:          keyAsText := 'Volume down';
    APPCOMMAND_VOLUME_UP:            keyAsText := 'Volume up';
    APPCOMMAND_MEDIA_NEXTTRACK:      keyAsText := 'Media next track';
    APPCOMMAND_MEDIA_PREVIOUSTRACK:  keyAsText := 'Media previous track';
    APPCOMMAND_MEDIA_STOP:           keyAsText := 'Media stop';
    APPCOMMAND_MEDIA_PLAY_PAUSE:     keyAsText := 'Media play/pause';
    APPCOMMAND_LAUNCH_MAIL:          keyAsText := 'Launch mail';
    APPCOMMAND_LAUNCH_MEDIA_SELECT:  keyAsText := 'Launch media select';
    APPCOMMAND_LAUNCH_APP1:          keyAsText := 'Launch app1';
    APPCOMMAND_LAUNCH_APP2:          keyAsText := 'Launch app2';
    APPCOMMAND_BASS_DOWN:            keyAsText := 'Bass down';
    APPCOMMAND_BASS_BOOST:           keyAsText := 'Bass boost';
    APPCOMMAND_BASS_UP:              keyAsText := 'Bass up';
    APPCOMMAND_TREBLE_DOWN:          keyAsText := 'Treble down';
    APPCOMMAND_TREBLE_UP:            keyAsText := 'Treble up';
  end;

  reReport.lines.add(keyAsText);

end;

END.

