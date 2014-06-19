unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    pbWithStatusText: TButton;
    pbNoStatusText: TButton;
    pbTestSDUWindowsProgressDialog: TButton;
    pbClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure pbWithStatusTextClick(Sender: TObject);
    procedure pbNoStatusTextClick(Sender: TObject);
    procedure pbTestSDUWindowsProgressDialogClick(Sender: TObject);
    procedure pbCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  ComCtrls,
  SDUGeneral,
  SDUDialogs,
  SDUProgressDlg;

procedure TForm2.FormCreate(Sender: TObject);
begin
  self.Caption := Application.Title;
end;

procedure TForm2.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TForm2.pbNoStatusTextClick(Sender: TObject);
var
  progressDlg: TSDUProgressDialog;
  i: integer;
begin

  progressDlg := TSDUProgressDialog.Create(nil);
  try
    progressDlg.Min := 0;
    progressDlg.Max := 10;
    progressDlg.Title := 'Demo one...';
    progressDlg.Show();
    Application.ProcessMessages();

    for i:=0 to 10 do
      begin
      progressDlg.Position := i;
      Application.ProcessMessages();

      if progressDlg.Cancel then
        begin
        break;
        end;

      sleep(1000);
      end;

  finally
    progressDlg.Free();
  end;

end;

procedure TForm2.pbWithStatusTextClick(Sender: TObject);
var
  progressDlg: TSDUProgressDialog;
  i: integer;
begin

  progressDlg := TSDUProgressDialog.Create(nil);
  try
    progressDlg.Min := 0;
    progressDlg.Max := 10;
    progressDlg.Title := 'Demo one...';
    progressDlg.ShowStatusText := TRUE;
    progressDlg.Show();
    Application.ProcessMessages();

    for i:=0 to 10 do
      begin
      progressDlg.Position := i;
      progressDlg.StatusText := 'ABC '+inttostr(i)+' XYZ';
      Application.ProcessMessages();

      if progressDlg.Cancel then
        begin
        break;
        end;

      sleep(1000);
      end;

  finally
    progressDlg.Free();
  end;

end;

procedure TForm2.pbTestSDUWindowsProgressDialogClick(Sender: TObject);
const
  NUM_STEPS = 200;
var
  progressDlg: TSDUWindowsProgressDialog;
  i: integer;
  userCancelled: boolean;
begin
  userCancelled := FALSE;

  progressDlg:= TSDUWindowsProgressDialog.Create();
  try
    progressDlg.Title := 'MY TITLE HERE';
    progressDlg.LineText[1] := 'MY LINE 1 HERE';
    progressDlg.LineText[2] := 'MY LINE 2 HERE';
    //progressDlg.LineText[3] := 'MY LINE 3 HERE';  // Automatically overwritten with "time remaining"
    progressDlg.CommonAVI := aviCopyFile;
    progressDlg.CancelMsg := 'Please wait while the current operation is cleaned up...';
    progressDlg.StartProgressDialog();
    // Optionally perform time-consuming task to determine total here
    progressDlg.Total := NUM_STEPS;
    // In case the determination of what "Total" should be set to, prior to
    // setting it above, too awhile, we reset the progress dialog's internal
    // timer, so that the "time remaining" display is more accurate
    progressDlg.Timer(PDTIMER_RESET);

    // Important: DO NOT SET "progressDlg.Progress := 0;"! From the MSDN
    // documentation, this can confuse the "time remaining" timer

    // Carry out processing here, updating "progressDlg.Progress" as
    // progress is made - checking "progressDlg.HasUserCancelled" in case
    // the user's cancelled the operation.
    // LineText[1..3] may also be updated here.
    for i:=1 to NUM_STEPS do
      begin
      sleep(50); // Replace this line with processing.

      if progressDlg.HasUserCancelled then
        begin
        userCancelled := TRUE;
        break;
        end;

      progressDlg.Progress := i;
      progressDlg.LineText[2] := inttostr(i) + ' / ' + inttostr(NUM_STEPS);
      end;


  finally
    progressDlg.StopProgressDialog();
    progressDlg.Free();
  end;

  if userCancelled then
    begin
    SDUMessageDlg('User cancelled.', mtInformation, [mbOK], 0);
    end
  else
    begin
    SDUMessageDlg('Done!', mtInformation, [mbOK], 0);
    end;

end;

END.

