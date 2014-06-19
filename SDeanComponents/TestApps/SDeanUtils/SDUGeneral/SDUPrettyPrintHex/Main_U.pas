unit Main_U;
// Description: SDUPrettyPrint Test Application 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Spin64;

type
  TMain_F = class(TForm)
    pbDisplayFile: TButton;
    reReport: TRichEdit;
    edFilename: TEdit;
    Label1: TLabel;
    seWidth: TSpinEdit64;
    Label2: TLabel;
    Label3: TLabel;
    procedure pbDisplayFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  Main_F: TMain_F;

implementation

{$R *.DFM}


uses
  SDUGeneral;



procedure TMain_F.pbDisplayFileClick(Sender: TObject);
var
  inputFile: TFileStream;
  hexPrettyPrint: TStringList;
begin
  inputFile:= TFileStream.Create(edFilename.text, (fmOpenRead or fmShareDenyNone));
  try
    hexPrettyPrint:= TStringList.Create();
    try
      SDUPrettyPrintHex(inputFile, 0, -1, hexPrettyPrint, seWidth.value);
      reReport.lines.Add('Printed-printed hex representation of "'+edFilename.text+'":');
      reReport.lines.AddStrings(hexPrettyPrint);
      reReport.lines.Add('');
    finally
      hexPrettyPrint.Free();
    end;
  finally
    inputFile.Free();
  end;


end;



END.


