unit Main;
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
  StdCtrls, ComCtrls, OTFE_U, OTFEFreeOTFE_U;

type
  TfrmMain = class(TForm)
    OTFEFreeOTFE1: TOTFEFreeOTFE;
    reReport: TRichEdit;
    pbGo: TButton;
    edFilename: TEdit;
    Label1: TLabel;
    edUserKey: TEdit;
    Label2: TLabel;
    edTestFilename: TEdit;
    Label3: TLabel;
    edDriveLetter: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    procedure pbGoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

procedure TfrmMain.pbGoClick(Sender: TObject);
var
  dl: string;
begin
  dl := edDriveLetter.text;
  showmessage(dl);
  OTFEFreeOTFE1.Active := TRUE;
  reReport.lines.Add((OTFEFreeOTFE1.DetectLinux(
    edFilename.text,
    edUserKey.text,
    '',
    0,
    0,
    0,
    dl[1],
    fomaFixedDisk,
    edTestFilename.text
   )));

end;

END.


