unit OTFEPGPDiskMounting_U;
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TOTFEPGPDiskMounting_F = class(TForm)
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    MonitorProcess: THandle;
  end;

implementation

{$R *.DFM}

uses SDUGeneral;

procedure TOTFEPGPDiskMounting_F.FormShow(Sender: TObject);
begin
  Timer1.enabled := TRUE;

end;

procedure TOTFEPGPDiskMounting_F.FormCreate(Sender: TObject);
begin
  width := 0;
  height := 0;

end;

procedure TOTFEPGPDiskMounting_F.Timer1Timer(Sender: TObject);
var
  exeExitStatus: DWORD;
begin
  Timer1.enabled := FALSE;
  if not(GetExitCodeProcess(MonitorProcess, exeExitStatus)) then
    begin
    Timer1.enabled := TRUE;
    end
  else
    begin
    if (exeExitStatus=STILL_ACTIVE) then
      begin
      Timer1.enabled := TRUE;
      end
    else
      begin
      ModalResult := mrOK;
      end;
    end;

end;

END.
