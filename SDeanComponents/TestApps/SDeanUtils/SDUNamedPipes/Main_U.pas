unit Main_U;
// Description: Named Pipes Test Application 
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  SDUNamedPipe_U, ExtCtrls;

type
  TMain_F = class(TForm)
    pbConnectClient: TButton;
    reReport: TRichEdit;
    edPipeName: TEdit;
    pbSend: TButton;
    Label1: TLabel;
    edMessage: TEdit;
    Label2: TLabel;
    pbClose: TButton;
    ckServer: TCheckBox;
    pbReceive: TButton;
    RichEdit1: TRichEdit;
    pbCreatePipe: TButton;
    pbConnectServer: TButton;
    pbDisconnect: TButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label4: TLabel;
    Label5: TLabel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Label3: TLabel;
    procedure pbConnectClientClick(Sender: TObject);
    procedure pbSendClick(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure pbReceiveClick(Sender: TObject);
    procedure pbCreatePipeClick(Sender: TObject);
    procedure ckServerClick(Sender: TObject);
    procedure pbConnectServerClick(Sender: TObject);
    procedure pbDisconnectClick(Sender: TObject);
  private
    SendPipe: TSDUNamedPipe;
  public
    { Public declarations }
  end;

var
  Main_F: TMain_F;

implementation

{$R *.DFM}

uses
  SDUGeneral;

procedure TMain_F.pbConnectClientClick(Sender: TObject);
var
  oksofar: boolean;
begin
  oksofar:= SendPipe.ClientConnect(edPipeName.text);
  if oksofar then
    begin
    reReport.lines.add('PIPE: Connected OK');
    end
  else
    begin
    reReport.lines.add('PIPE: Failed to connect');
    exit;
    end;

end;


procedure TMain_F.pbSendClick(Sender: TObject);
var
  oksofar: boolean;
begin
  oksofar:= SendPipe.WriteString(edMessage.text);
  if oksofar then
    begin
    reReport.lines.add('PIPE: Sent OK');
    end
  else
    begin
    reReport.lines.add('PIPE: Send failed');
    exit;
    end;

end;

procedure TMain_F.pbCloseClick(Sender: TObject);
var
  oksofar: boolean;
begin
  oksofar:= SendPipe.Close();
  if oksofar then
    begin
    reReport.lines.add('PIPE: Closed OK');
    end
  else
    begin
    reReport.lines.add('PIPE: Close failed');
    exit;
    end;

end;

procedure TMain_F.FormShow(Sender: TObject);
begin
  SendPipe:= TSDUNamedPipe.Create(nil);

end;

procedure TMain_F.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SendPipe.Free();

end;

procedure TMain_F.pbReceiveClick(Sender: TObject);
var
  oksofar: boolean;
  msg: string;
begin
  oksofar:= SendPipe.ReadString(msg);
  if oksofar then
    begin
    reReport.lines.add('PIPE: Received OK');
    reReport.lines.Add(msg);
    end
  else
    begin
    reReport.lines.add('PIPE: Receive failed');
    exit;
    end;

end;

procedure TMain_F.pbCreatePipeClick(Sender: TObject);
var
  oksofar: boolean;
begin
  oksofar:= SendPipe.CreatePipe(edPipeName.text);
  if oksofar then
    begin
    reReport.lines.add('PIPE: Created OK');
    end
  else
    begin
    reReport.lines.add('PIPE: Create failed');
    exit;
    end;

end;

procedure TMain_F.ckServerClick(Sender: TObject);
begin

  SDUEnableControl(pbConnectClient, not(ckServer.checked));

  SDUEnableControl(pbCreatePipe, ckServer.checked);
  SDUEnableControl(pbConnectServer, ckServer.checked);
  SDUEnableControl(pbDisconnect, ckServer.checked);

end;

procedure TMain_F.pbConnectServerClick(Sender: TObject);
var
  oksofar: boolean;
begin
  oksofar:= SendPipe.ServerConnect();
  if oksofar then
    begin
    reReport.lines.add('PIPE: Connected OK');
    end
  else
    begin
    reReport.lines.add('PIPE: Failed to connect');
    exit;
    end;

end;

procedure TMain_F.pbDisconnectClick(Sender: TObject);
var
  oksofar: boolean;
begin
  oksofar:= SendPipe.Disconnect();
  if oksofar then
    begin
    reReport.lines.add('PIPE: Disconnected OK');
    end
  else
    begin
    reReport.lines.add('PIPE: Failed to disconnect');
    exit;
    end;

end;


END.


