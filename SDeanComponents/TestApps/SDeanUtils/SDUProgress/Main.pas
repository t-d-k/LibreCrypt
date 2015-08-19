unit Main;

interface

uses
  Classes, Controls, Dialogs, Forms,
  Graphics, Messages, StdCtrls, SysUtils, Variants, Windows;

type
  TForm2 = class (TForm)
    GroupBox1:        TGroupBox;
    GroupBox2:        TGroupBox;
    pbprogress:       TButton;
    pbTestSDUWindowsProgressDialog: TButton;
    pbClose:          TButton;
    cbShowTimeRemaining: TCheckBox;
    cbShowStatusText: TCheckBox;

    procedure pbprogressClick(Sender: TObject);
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
  //delphi
  vcl.ComCtrls,

  //  SDU
  dlgProgress;

procedure TForm2.pbCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TForm2.pbprogressClick(Sender: TObject);
var
  progressDlg: TdlgProgress;
  i:           Integer;
begin

  progressDlg := TdlgProgress.Create(nil);
  try
    progressDlg.Min               := 0;
    progressDlg.Max               := 100;
    progressDlg.Title             := 'Demo one...';
    progressDlg.ShowTimeRemaining := cbShowTimeRemaining.Checked;
    progressDlg.ShowStatusText    := cbShowStatusText.Checked;
    progressDlg.Show();
    Application.ProcessMessages();

    for i := 0 to 100 do begin
      sleep(1000);
      progressDlg.Position   := i;
      progressDlg.StatusText := 'ABC ' + IntToStr(i) + ' XYZ';
      Application.ProcessMessages();

      if progressDlg.Cancel then
        break;
    end;

  finally
    progressDlg.Free();
  end;
end;

procedure TForm2.pbTestSDUWindowsProgressDialogClick(Sender: TObject);
const
  NUM_STEPS = 200;
var
  progressDlg:   TSDUWindowsProgressDialog;
  i:             Integer;
  userCancelled: Boolean;
begin
  userCancelled := False;

  progressDlg := TSDUWindowsProgressDialog.Create();
  try
    progressDlg.Title       := 'MY TITLE HERE';
    progressDlg.LineText[1] := 'MY LINE 1 HERE';
    progressDlg.LineText[2] := 'MY LINE 2 HERE';
    //progressDlg.LineText[3] := 'MY LINE 3 HERE';  // Automatically overwritten with "time remaining"
    progressDlg.CommonAVI   := aviCopyFile;
    progressDlg.CancelMsg   := 'Please wait while the current operation is cleaned up...';
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
    for i := 1 to NUM_STEPS do begin
      sleep(50); // Replace this line with processing.

      if progressDlg.HasUserCancelled then begin
        userCancelled := True;
        break;
      end;

      progressDlg.Progress    := i;
      progressDlg.LineText[2] := IntToStr(i) + ' / ' + IntToStr(NUM_STEPS);
    end;


  finally
    progressDlg.StopProgressDialog();
    progressDlg.Free();
  end;

  if userCancelled then begin
    MessageDlg('User cancelled.', mtInformation, [mbOK], 0);
  end else begin
    MessageDlg('Done!', mtInformation, [mbOK], 0);
  end;

end;

end.
