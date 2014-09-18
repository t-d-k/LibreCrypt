unit SDUProgressDlg;
 // Description: Progress bar dialog
 // By Sarah Dean
 // Email: sdean12@sdean12.org
 // WWW:   http://www.SDean12.org/
 //
 // -----------------------------------------------------------------------------
 //

 // This unit defines two different progress dialogs:
 //
 //   TSDUProgressDialog        - A basic "progress" dialog. Based on a Delphi
 //                               form
 //   TSDUWindowsProgressDialog - The MS Windows standard progress dialog, as
 //                               seen when copying files within MS Windows
 //                               Explorer. Based on Windows IProgressDialog.


 // TSDUWindowsProgressDialog usage:
 //
 //   var
 //     progressDlg: TSDUWindowsProgressDialog;
 //
 //   progressDlg:= TSDUWindowsProgressDialog.Create(self);
 //   try
 //     progressDlg.Title := 'MY TITLE HERE';
 //     progressDlg.LineText[1] := 'MY LINE 1 HERE';
 //     progressDlg.LineText[2] := 'MY LINE 2 HERE';
//     //progressDlg.LineText[3] := 'MY LINE 3 HERE';  // Automatically overwritten with "time remaining"
 //     progressDlg.CommonAVI := aviCopyFile;  // From ComCtrls.pas
 //     progressDlg.CancelMsg := 'Please wait while the current operation is cleaned up...';
 //     progressDlg.StartProgressDialog();
 //     // Optionally perform time-consuming task to determine total here
 //     progressDlg.Total := 100;
 //     // In case the determination of what "Total" should be set to, prior to
 //     // setting it above, too awhile, we reset the progress dialog's internal
 //     // timer, so that the "time remaining" display is more accurate
 //     progressDlg.Timer(PDTIMER_RESET);
 //
 //     Important: DO NOT SET "progressDlg.Progress := 0;"! From the MSDN
 //     documentation, this can confuse the "time remaining" timer
 //
 //     // Carry out processing here, updating "progressDlg.Progress" as
 //     // progress is made - checking "progressDlg.HasUserCancelled" in case
 //     // the user's cancelled the operation.
 //     // LineText[1..3] may also be updated here.
 //     ...
 //
 //   finally
 //     progressDlg.StopProgressDialog();
 //     progressDlg.Free();
 //   end;
 //
 // to finish.
 //
 // NOTICE: Windows only shows the dialog after a few seconds; presumably do it
 //         doesn't flicker for operations which take only a second or so 
 //
 // NOTICE: CancelMsg is only shown if ShowAutoTime is set to FALSE!
 //
 //
 //
 // From: shlobj.h:
 //
 //    USAGE:
 //        This is how the dialog is used during operations that require progress
 //    and the ability to cancel:
 //    {
 //        DWORD dwComplete, dwTotal;
 //        IProgressDialog * ppd;
//        CoCreateInstance(CLSID_ProgressDialog, NULL, CLSCTX_INPROC_SERVER, IID_IProgressDialog, (void **)&ppd);
//        ppd->SetTitle(L"My Slow Operation");                                // Set the title of the dialog.
//        ppd->SetAnimation(hInstApp, IDA_OPERATION_ANIMATION);               // Set the animation to play.
//        ppd->StartProgressDialog(hwndParent, punk, PROGDLG_AUTOTIME, NULL); // Display and enable automatic estimated time remaining.
//        ppd->SetCancelMsg(L"Please wait while the current operation is cleaned up", NULL);   // Will only be displayed if Cancel button is pressed.
 //
 //        dwComplete = 0;
 //        dwTotal = CalcTotalUnitsToDo();
 //
 //        // Reset because CalcTotalUnitsToDo() took a long time and the estimated time
 //        // is based on the time between ::StartProgressDialog() and the first
 //        // ::SetProgress() call.
 //        ppd->Timer(PDTIMER_RESET, NULL);
 //
 //        for (nIndex = 0; nIndex < nTotal; nIndex++)
 //        {
 //            if (TRUE == ppd->HasUserCancelled())
 //                break;
 //
 //            ppd->SetLine(2, L"I'm processing item n", FALSE, NULL);
 //            dwComplete += DoSlowOperation();
 //
 //            ppd->SetProgress(dwCompleted, dwTotal);
 //        }
 //
 //        ppd->StopProgressDialog();
 //        ppd->Release();
 //    }


interface

uses
  Classes, ComCtrls, Controls, Dialogs,
  ExtCtrls, Forms, Graphics, Messages, SDUComCtrls,
  SDUForms, SDUGeneral, SDUStdCtrls, StdCtrls, SysUtils, Windows;

type
  TSDUProgressDialog = class (TSDUForm)
    pnlProgressBar:            TPanel;
    pbCancel:                  TButton;
    pnlStatusText:             TPanel;
    lblStatus:                 TSDUTruncatingLabel;
    lblEstTimeRemainText:      TLabel;
    lblEstTimeRemaining:       TLabel;
    pnlProgressBarPlaceholder: TPanel;
    pgbOverall:                TProgressBar;
    pgbIndeterminate:          TSDProgressBarIndeterminate;
    procedure pbCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  PRIVATE
    fShowStatusText:        Boolean;
    fConfirmCancel:         Boolean;
    i64MinValue:            Int64;
    i64MaxValue:            Int64;
    i64PositionValue:       Int64;
    fShowTimeRemaining:     Boolean;
    fStartTime:             TDateTime;
    fCancelSetsModalResult: Boolean;

    procedure SetTitle(title: String);
    function GetTitle(): String;

    function GetIndeterminate(): Boolean;
    procedure SetIndeterminate(newValue: Boolean);
    function GetIndeterminateRunning(): Boolean;
    procedure SetIndeterminateRunning(newValue: Boolean);
    function GetIndeterminateUpdate(): Integer;
    procedure SetIndeterminateUpdate(newValue: Integer);

    procedure SetShowTimeRemaining(Value: Boolean);


    procedure SetShowStatusText(showStatusText: Boolean);
    procedure SetStatusText(statusText: String);
    function GetStatusText(): String;

    procedure UpdateI64ProgressBar();

    procedure iSetMax(max: Integer);
    procedure iSetMin(min: Integer);
    procedure iSetPosition(position: Integer);
    procedure iSetInversePosition(position: Integer);

    procedure i64SetMax(max: Int64);
    procedure i64SetMin(min: Int64);
    procedure i64SetPosition(position: Int64);
    procedure i64SetInversePosition(position: Int64);
  PUBLIC
    Cancel: Boolean;

    constructor Create(AOwner: TComponent); OVERRIDE;
    destructor Destroy(); OVERRIDE;

    property Title: String Read GetTitle Write SetTitle;

    property Indeterminate: Boolean Read GetIndeterminate Write SetIndeterminate;
    property IndeterminateRunning: Boolean Read GetIndeterminateRunning
      Write SetIndeterminateRunning;
    property IndeterminateUpdate: Integer Read GetIndeterminateUpdate
      Write SetIndeterminateUpdate;

    property ShowTimeRemaining: Boolean Read fShowTimeRemaining Write SetShowTimeRemaining;

    property Max: Integer Write iSetMax;
    property Min: Integer Write iSetMin;
    property Position: Integer Write iSetPosition;
    property InversePosition: Integer Write iSetInversePosition;
    procedure IncPosition();

    property i64Max: Int64 Read i64MaxValue Write i64SetMax;
    property i64Min: Int64 Read i64MinValue Write i64SetMin;
    property i64Position: Int64 Read i64PositionValue Write i64SetPosition;
    property i64InversePosition: Int64 Write i64SetInversePosition;
    procedure i64IncPosition();

    property ConfirmCancel: Boolean Write fConfirmCancel DEFAULT True;

    property ShowStatusText: Boolean Read fShowStatusText Write SetShowStatusText;
    property StatusText: String Read GetStatusText Write SetStatusText;

    property CancelSetsModalResult: Boolean Read fCancelSetsModalResult
      Write fCancelSetsModalResult;
  end;


const
                                    // Values to be used with TSDUWindowsProgressDialog.Timer(...)
                                    // Time Actions (dwTimerAction)
  PDTIMER_RESET  = $00000001;       // Reset the timer so the progress will be
                                    // calculated from now until the first
                                    // ::SetProgress() is called so those
                                    // this time will correspond to the values
                                    // passed to ::SetProgress().  Only do
                                    // this before ::SetProgress() is called.
                                    // Windows Vista and later only...
  PDTIMER_PAUSE  = $00000002;
  PDTIMER_RESUME = $00000003;


// From: shlguid.h:
const
  //if (_WIN32_IE >= 0x0500)
  /// IProgressDialog
  // {F8383852-FCD3-11d1-A6B9-006097DF5BD4}
  CLSID_ProgressDialog: TGUID = '{F8383852-FCD3-11d1-A6B9-006097DF5BD4}';
  // {EBBC7C04-315E-11d2-B62F-006097DF5BD4}
  IID_IProgressDialog: TGUID  = '{EBBC7C04-315E-11d2-B62F-006097DF5BD4}';


  //
  // Progress objects exposed via QueryService
  //
  SID_SProgressUI = '{F8383852-FCD3-11d1-A6B9-006097DF5BD4}';

  ProgressDialog_MAXLINES = 3;

type
  {$EXTERNALSYM IProgressDialog}
  IProgressDialog = interface (IUnknown)
    [SID_SProgressUI]
    function StartProgressDialog(hwndParent: HWND; punkEnableModless: IUnknown;
      dwFlags: DWORD; pvResevered: Pointer): HResult; STDCALL;
    function StopProgressDialog(): HResult; STDCALL;
    function SetTitle(pwzTitle: PWideChar): HResult; STDCALL;
    function SetAnimation(hInstAnimation: HINST; idAnimation: UINT): HResult; STDCALL;
    function HasUserCancelled(): BOOL; STDCALL;
    function SetProgress(dwCompleted: DWORD; dwTotal: DWORD): HResult; STDCALL;
    function SetProgress64(ullCompleted: ULONGLONG; ullTotal: ULONGLONG): HResult; STDCALL;
    function SetLine(dwLineNum: DWORD; pwzString: PWideChar; fCompactPath: BOOL;
      pvResevered: Pointer): HResult; STDCALL;
    function SetCancelMsg(pwzCancelMsg: PWideChar; pvResevered: Pointer): HResult; STDCALL;
    function Timer(dwTimerAction: DWORD; pvResevered: Pointer): HResult; STDCALL;
  end;


{$M+}// Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"
  TSDUWindowsProgressDialog = class
  PROTECTED
    FintfProgressDialog: IProgressDialog;
    FParentHWnd:         THandle;

    FShowAsModal:        Boolean;
    FShowAutoTime:       Boolean;
    FShowTime:           Boolean;
    FShowMinimize:       Boolean;
    FShowProgressBar:    Boolean;
    FShowMarqeeProgress: Boolean;
    FShowCancel:         Boolean;

    FFileName:  String;
    FCommonAVI: TCommonAVI;
    FResHandle: THandle;
    FResId:     Integer;
    FResName:   String;

    FProgress:   DWORD;
    FProgress64: ULONGLONG;
    FTotal:      DWORD;
    FTotal64:    ULONGLONG;

    FLineText:    array [1..ProgressDialog_MAXLINES] of WideString;
    FLineCompact: array [1..ProgressDialog_MAXLINES] of Boolean;

    procedure SetTitle(pwzTitle: WideString);

    procedure SetLineText(idx: Integer; Value: WideString);
    function GetLineText(idx: Integer): WideString;
    procedure SetLineCompact(idx: Integer; Value: Boolean);
    function GetLineCompact(idx: Integer): Boolean;

    procedure SetCommonAVI(Value: TCommonAVI);
    procedure SetResHandle(newValue: THandle);
    procedure SetResID(newValue: Integer);
    procedure SetResName(newValue: String);

    procedure SetProgress(dwCompleted: DWORD);
    function GetProgress(): DWORD;
    procedure SetProgress64(ullCompleted: ULONGLONG);
    function GetProgress64(): ULONGLONG;
    procedure SetTotal(dwTotal: DWORD);
    function GetTotal(): DWORD;
    procedure SetTotal64(ullTotal: ULONGLONG);
    function GetTotal64(): ULONGLONG;

    procedure SetCancelMsg(pwzCancelMsg: WideString);

    function GetActualResHandle: THandle;
    function GetActualResId: Integer;

  PUBLIC
    constructor Create();
    destructor Destroy(); OVERRIDE;

    // Note: idx may only be one of 1..3 (1..ProgressDialog_MAXLINES)
    property LineText[idx: Integer]: WideString Read GetLineText Write SetLineText;
    property LineCompact[idx: Integer]: Boolean Read GetLineCompact Write SetLineCompact;

    function StartProgressDialog(): HResult;
    function StopProgressDialog(): HResult;

    function HasUserCancelled(): Boolean;
    function Timer(dwTimerAction: DWORD): HResult;

  PUBLISHED
    // Set this to Application.Handle to prevent any MessageDlg(...) dialog
    // boxes shown during the long operation getting hidden if the user clicks
    // the main window.
    // Should probably be set to the window's handle if non modal...
    // Defaults to Application.Handle
    property ParentHWnd: THandle Read FParentHWnd Write FParentHWnd;

    property Title: WideString Write SetTitle;

    // Animation.
    // These properties operate in the same manner as for Delphi's TAnimate

    // Note: from the MSDN:
    //   Audio-Video Interleaved (AVI) clip that runs in the dialog box.
    //   Note  This method is not supported in Windows Vista or later versions.
    //   * Clips cannot include sound.
    //   * The size of the AVI clip cannot exceed 272 by 60 pixels. Smaller
    //     rectangles can be used, but they might not be properly centered.
    //   * AVI clips must either be uncompressed or compressed with run-length
    //     (BI_RLE8) encoding. If you attempt to use an unsupported compression
    //     type, no animation is displayed.
    property CommonAVI: TCommonAVI Write SetCommonAVI DEFAULT aviNone;
    property ResHandle: THandle Write SetResHandle;
    property ResID: Integer Write SetResID;
    property ResName: String Write SetResName;

    property Progress: DWORD Read GetProgress Write SetProgress;
    property Progress64: ULONGLONG Read GetProgress64 Write SetProgress64;
    property Total: DWORD Read GetTotal Write SetTotal;
    property Total64: ULONGLONG Read GetTotal64 Write SetTotal64;

    // Note: The cancel message appears to only be shown if ShowAutoTime is
    //       FALSE - but that means you don't get "time remaining..."
    //       calculated
    property CancelMsg: WideString Write SetCancelMsg;

    // Note: These properties *must* be set *before* calling StartProgressDialog(...)
    // The progress dialog box will be modal to the window specified by hwndParent. By default, a progress dialog box is modeless.
    property ShowAsModal: Boolean Read FShowAsModal Write FShowAsModal;
    // Automatically estimate the remaining time and display the estimate on line 3. If this flag is set, IProgressDialog::SetLine can be used only to display text on lines 1 and 2.
    property ShowAutoTime: Boolean Read FShowAutoTime Write FShowAutoTime;
    // Show the "time remaining" text.
    property ShowTime: Boolean Read FShowTime Write FShowTime;
    // Display a minimize button on the dialog box's caption bar.
    property ShowMinimize: Boolean Read FShowMinimize Write FShowMinimize;
    // Display a progress bar. Typically, an application can quantitatively determine how much of the operation remains and periodically pass that value to IProgressDialog::SetProgress. The progress dialog box uses this information to update its progress bar. This flag is typically set when the calling application must wait for an operation to finish, but does not have any quantitative information it can use to update the dialog box.
    property ShowProgressBar: Boolean Read FShowProgressBar Write FShowProgressBar;

    // Note: These properties *must* be set *before* calling StartProgressDialog(...)
    // The next two are for Windows Vista and later only
    // Set the progress bar to marquee mode. This causes the progress bar to scroll horizontally, similar to a marquee display. Use this when you wish to indicate that progress is being made, but the time required for the operation is unknown.
    property ShowMarqeeProgress: Boolean Read FShowMarqeeProgress Write FShowMarqeeProgress;
    // Show a cancel button. The operation cannot be canceled. Use this only when absolutely necessary.
    property ShowCancel: Boolean Read FShowCancel Write FShowCancel;
  end;


procedure Register;

implementation

{$R *.DFM}

uses
  ActiveX,
  ComObj, DateUtils,
  math,
  SDUDialogs,
  SDUGraphics,
  SDUi18n;


// From: shlobj.h:
const
  // Flags for IProgressDialog::StartProgressDialog() (dwFlags)
  PROGDLG_NORMAL          = $00000000;      // default normal progress dlg
  // behavior
  PROGDLG_MODAL           = $00000001;      // the dialog is modal to its
  // hwndParent (default is modeless)
  PROGDLG_AUTOTIME        = $00000002;      // automatically updates the
  // "Line3" text with the "time
  // remaining" (you cant call
  // SetLine3 if you passs this!)
  PROGDLG_NOTIME          = $00000004;      // we dont show the "time
  // remaining" if this is set. We
  // need this if
  // dwTotal < dwCompleted for sparse
  // files
  PROGDLG_NOMINIMIZE      = $00000008;      // Do not have a minimize button in
  // the caption bar.
  PROGDLG_NOPROGRESSBAR   = $00000010;      // Don't display the progress bar
  // The following are only available if (_WIN32_IE >= 0x0700)
  PROGDLG_MARQUEEPROGRESS = $00000020;
  PROGDLG_NOCANCEL        = $00000040;


var
  ShellModule: THandle;

// Ripped from Delphi's TAnimate...
function GetShellModule: THandle;
begin
  if ShellModule = 0 then begin
    ShellModule := SafeLoadLibrary(DLL_SHELL32);
    if ShellModule <= HINSTANCE_ERROR then
      ShellModule := 0;
  end;
  Result := ShellModule;
end;

procedure Register;
begin
  RegisterComponents('SDeanUtils', [TSDUProgressDialog]);
end;

constructor TSDUProgressDialog.Create(AOwner: TComponent);
begin
  inherited;
  SDUClearPanel(pnlProgressBarPlaceholder);

  pgbOverall.Align := alClient;

  pgbIndeterminate.Visible := False;
  pgbIndeterminate.Align   := alClient;

  fCancelSetsModalResult := False;

end;

destructor TSDUProgressDialog.Destroy();
begin
  inherited;
end;

procedure TSDUProgressDialog.SetTitle(title: String);
begin
  self.Caption := title;
end;

function TSDUProgressDialog.GetTitle(): String;
begin
  Result := self.Caption;
end;

procedure TSDUProgressDialog.UpdateI64ProgressBar();
var
  barLength:  Int64;
  barPos:     Int64;
  percentPos: Integer;
begin
  iSetMin(0);
  iSetMax(100);

  barLength := i64MaxValue - i64MinValue;
  barPos    := i64PositionValue - i64MinValue;

  if (i64MaxValue <> 0) then begin
    percentPos := ((barPos * 100) div barLength);
    iSetPosition(percentPos);
  end;

end;



procedure TSDUProgressDialog.iSetMax(max: Integer);
begin
  pgbOverall.Max := max;
end;

procedure TSDUProgressDialog.iSetMin(min: Integer);
begin
  pgbOverall.Min := min;
end;

procedure TSDUProgressDialog.iSetPosition(position: Integer);
var
  currTime:      TDateTime;
  timeDone:      TDateTime;
  newLabel:      String;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  timeRemaining: TDateTime;
  cntDone:       Integer;
  cntTotal:      Integer;
  cntRemaining:  Integer;
begin
  if position > pgbOverall.Max then begin
    pgbOverall.position := pgbOverall.Max;
  end else
  if position < pgbOverall.Min then begin
    pgbOverall.position := pgbOverall.Min;
  end else begin
    pgbOverall.position := position;
  end;

  if fShowTimeRemaining then begin
    newLabel := RS_UNKNOWN;

    currTime := Now;
    if (currTime <> fStartTime) then begin
      timeDone := currTime - fStartTime;

      cntDone      := pgbOverall.Position - pgbOverall.Min;
      cntTotal     := pgbOverall.Max - pgbOverall.Min;
      cntRemaining := cntTotal - cntDone;

      if (cntDone <> 0) then begin
        timeRemaining := (timeDone / cntDone) * cntRemaining;
        DecodeDateTime(
          timeRemaining,
          AYear,
          AMonth,
          ADay,
          AHour,
          AMinute,
          ASecond,
          AMilliSecond
          );
        ADay := trunc(timeRemaining);
        // Only display two most significant units; anything beyond that is
        // not particularly significant
        if (ADay > 0) then begin
          newLabel := SDUParamSubstitute(_('%1 days, %2 hours'),
            [ADay, AHour]);
        end else
        if (AHour > 0) then begin
          newLabel := SDUParamSubstitute(_('%1 hours, %2 minutes'),
            [AHour, AMinute]);
        end else
        if (AMinute > 0) then begin
          newLabel := SDUParamSubstitute(_('%1 minutes, %2 seconds'),
            [AMinute, ASecond]);
        end else begin
          newLabel := SDUParamSubstitute(_('%1 seconds'), [ASecond]);
        end;

      end;

      lblEstTimeRemaining.Caption := newLabel;
    end;
  end;

  self.refresh;
  Application.ProcessMessages();
end;

procedure TSDUProgressDialog.iSetInversePosition(position: Integer);
begin
  iSetPosition(pgbOverall.Max - position);

end;

procedure TSDUProgressDialog.IncPosition();
begin
  iSetPosition(pgbOverall.Position + 1);

end;


procedure TSDUProgressDialog.i64SetMax(max: Int64);
begin
  i64MaxValue := max;
  UpdateI64ProgressBar();

end;

procedure TSDUProgressDialog.i64SetMin(min: Int64);
begin
  i64MinValue := min;
  UpdateI64ProgressBar();

end;

procedure TSDUProgressDialog.i64SetPosition(position: Int64);
begin
  i64PositionValue := position;
  UpdateI64ProgressBar();

end;

procedure TSDUProgressDialog.i64SetInversePosition(position: Int64);
begin
  i64PositionValue := i64MaxValue - position;
  UpdateI64ProgressBar();

end;

procedure TSDUProgressDialog.i64IncPosition();
begin
  i64PositionValue := i64PositionValue + 1;
  UpdateI64ProgressBar();

end;


procedure TSDUProgressDialog.pbCancelClick(Sender: TObject);
begin
  Cancel           := True;
  pbCancel.Enabled := False;
  if fConfirmCancel then begin
    SDUMessageDlg(_('Operation cancelled by user'));
  end;

  if fCancelSetsModalResult then begin
    ModalResult := mrCancel;
  end;

end;

procedure TSDUProgressDialog.FormCreate(Sender: TObject);
begin
  pbCancel.Enabled := True;
  Cancel           := False;

  pnlStatusText.Caption     := '';
  pnlStatusText.BevelInner  := bvNone;
  pnlStatusText.BevelOuter  := bvNone;
  pnlProgressBar.Caption    := '';
  pnlProgressBar.BevelInner := bvNone;
  pnlProgressBar.BevelOuter := bvNone;

  lblStatus.Caption  := '';
  lblStatus.AutoSize := False;
  lblStatus.Width    := pnlStatusText.Width - (2 * lblStatus.left);

  fShowStatusText := True;   // The form is designed with it shown
  ShowStatusText  := False;  // Default to just the progress bar
  StatusText      := '';

  lblEstTimeRemaining.Caption  := RS_UNKNOWN;
  lblEstTimeRemaining.Visible  := False;
  lblEstTimeRemainText.Visible := False;

end;

procedure TSDUProgressDialog.SetShowStatusText(showStatusText: Boolean);
begin
  if (fShowStatusText <> showStatusText) then begin
    if showStatusText then begin
      self.Height           := self.Height + pnlStatusText.Height;
      pnlStatusText.Visible := True;
    end else begin
      self.Height           := self.Height - pnlStatusText.Height;
      pnlStatusText.Visible := False;
    end;
  end;

  fShowStatusText := showStatusText;
end;

procedure TSDUProgressDialog.SetStatusText(statusText: String);
begin
  lblStatus.Caption := statusText;

end;

function TSDUProgressDialog.GetStatusText(): String;
begin
  Result := lblStatus.Caption;

end;

procedure TSDUProgressDialog.FormShow(Sender: TObject);
begin
  fStartTime := Now;
end;

procedure TSDUProgressDialog.SetShowTimeRemaining(Value: Boolean);
begin
  fShowTimeRemaining := Value;

  lblEstTimeRemaining.Visible  := fShowTimeRemaining;
  lblEstTimeRemainText.Visible := fShowTimeRemaining;
end;

function TSDUProgressDialog.GetIndeterminate(): Boolean;
begin
  Result := pgbIndeterminate.Visible;
end;

procedure TSDUProgressDialog.SetIndeterminate(newValue: Boolean);
begin
  pgbIndeterminate.Visible := newValue;
  pgbOverall.Visible       := not (pgbIndeterminate.Visible);
end;

function TSDUProgressDialog.GetIndeterminateRunning(): Boolean;
begin
  Result := pgbIndeterminate.Marquee;
end;

procedure TSDUProgressDialog.SetIndeterminateRunning(newValue: Boolean);
begin
  pgbIndeterminate.Marquee := newValue;
end;

function TSDUProgressDialog.GetIndeterminateUpdate(): Integer;
begin
  Result := pgbIndeterminate.MarqueeUpdate;
end;

procedure TSDUProgressDialog.SetIndeterminateUpdate(newValue: Integer);
begin
  pgbIndeterminate.MarqueeUpdate := newValue;
end;


 // -----------------------------------------------------------------------------
 // -----------------------------------------------------------------------------
constructor TSDUWindowsProgressDialog.Create();
var
  i: Integer;
begin
  //  inherited;  // Don't call if inherit from TObject!

  FParentHWnd := Application.Handle;

  FShowAsModal        := True;
  FShowAutoTime       := True;
  FShowTime           := True;
  FShowMinimize       := False;
  FShowProgressBar    := True;
  FShowMarqeeProgress := False;
  ShowCancel          := True;

  FCommonAVI := aviNone;
  FFileName  := '';
  FResHandle := 0;
  FResName   := '';
  FResId     := 0;

  for i := low(FLineText) to high(FLineText) do begin
    FLineText[i]    := '';
    FLineCompact[i] := False;
  end;

  // This next line crashes under Windows Vista/Windows 7 64 bit.
  // Use CoCreateInstance(...) instead
  //  FintfProgressDialog := CreateComObject(CLSID_ProgressDialog) as IProgressDialog;
  CoCreateInstance(
    CLSID_ProgressDialog,
    nil,
    CLSCTX_ALL,
    IID_IProgressDialog,
    FintfProgressDialog
    );

end;

destructor TSDUWindowsProgressDialog.Destroy();
begin
  StopProgressDialog();
  inherited;
end;

// Ripped from Delphi's TAnimate...
function TSDUWindowsProgressDialog.GetActualResHandle: THandle;
begin
  if FCommonAVI <> aviNone then
    Result := GetShellModule
  else
  if FResHandle <> 0 then
    Result := FResHandle
  else
  if MainInstance <> 0 then
    Result := MainInstance
  else
    Result := HInstance;
end;

// Ripped from Delphi's TAnimate...
function TSDUWindowsProgressDialog.GetActualResId: Integer;
const
  CommonAVIId: array[TCommonAVI] of Integer = (0, 150, 151, 152, 160, 161, 162,
    163, 164);
begin
  if FCommonAVI <> aviNone then
    Result := CommonAVIId[FCommonAVI]
  else
  if FFileName <> '' then
    Result := Integer(FFileName)
  else
  if FResName <> '' then
    Result := Integer(FResName)
  else
    Result := FResId;
end;

function TSDUWindowsProgressDialog.StartProgressDialog(): HResult;
var
  dwFlags: DWORD;
begin
  dwFlags := PROGDLG_NORMAL;
  if ShowAsModal then begin
    dwFlags := dwFlags + PROGDLG_MODAL;
  end;
  if ShowAutoTime then begin
    dwFlags := dwFlags + PROGDLG_AUTOTIME;
  end;
  if not (ShowTime) then begin
    dwFlags := dwFlags + PROGDLG_NOTIME;
  end;
  if not (ShowMinimize) then begin
    dwFlags := dwFlags + PROGDLG_NOMINIMIZE;
  end;
  if not (ShowProgressBar) then begin
    dwFlags := dwFlags + PROGDLG_NOPROGRESSBAR;
  end;
  if ShowMarqeeProgress then begin
    dwFlags := dwFlags + PROGDLG_MARQUEEPROGRESS;
  end;
  if not (ShowCancel) then begin
    dwFlags := dwFlags + PROGDLG_NOCANCEL;
  end;

  Result := FintfProgressDialog.StartProgressDialog(FParentHWnd, nil, dwFlags, nil);
end;

function TSDUWindowsProgressDialog.StopProgressDialog(): HResult;
begin
  Result := FintfProgressDialog.StopProgressDialog();
end;

procedure TSDUWindowsProgressDialog.SetTitle(pwzTitle: WideString);
begin
  FintfProgressDialog.SetTitle(PWideChar(pwzTitle));
end;

procedure TSDUWindowsProgressDialog.SetLineText(idx: Integer; Value: WideString);
begin
  FLineText[idx] := Value;
  FintfProgressDialog.SetLine(idx, PWideChar(FLineText[idx]), FLineCompact[idx], nil);
end;

function TSDUWindowsProgressDialog.GetLineText(idx: Integer): WideString;
begin
  Result := FLineText[idx];
end;

procedure TSDUWindowsProgressDialog.SetLineCompact(idx: Integer; Value: Boolean);
begin
  FLineCompact[idx] := Value;
  FintfProgressDialog.SetLine(idx, PWideChar(FLineText[idx]), FLineCompact[idx], nil);
end;

function TSDUWindowsProgressDialog.GetLineCompact(idx: Integer): Boolean;
begin
  Result := FLineCompact[idx];
end;

procedure TSDUWindowsProgressDialog.SetCommonAVI(Value: TCommonAVI);
begin
  // Animations not supported under Windows Vista and later
  if not (SDUOSVistaOrLater()) then begin
    FCommonAVI := Value;
    FFileName  := '';
    FResHandle := 0;
    FResName   := '';
    FResId     := 0;

    FintfProgressDialog.SetAnimation(GetActualResHandle, GetActualResID);
  end;
end;

procedure TSDUWindowsProgressDialog.SetResHandle(newValue: THandle);
begin
end;

procedure TSDUWindowsProgressDialog.SetResID(newValue: Integer);
begin
end;

procedure TSDUWindowsProgressDialog.SetResName(newValue: String);
begin
end;

function TSDUWindowsProgressDialog.HasUserCancelled(): Boolean;
begin
  Result := FintfProgressDialog.HasUserCancelled();
end;

procedure TSDUWindowsProgressDialog.SetProgress(dwCompleted: DWORD);
begin
  FProgress := dwCompleted;
  FintfProgressDialog.SetProgress(FProgress, FTotal);
end;

function TSDUWindowsProgressDialog.GetProgress(): DWORD;
begin
  Result := FProgress;
end;

procedure TSDUWindowsProgressDialog.SetProgress64(ullCompleted: ULONGLONG);
begin
  FProgress64 := ullCompleted;
  FintfProgressDialog.SetProgress(FProgress64, FTotal64);
end;

function TSDUWindowsProgressDialog.GetProgress64(): ULONGLONG;
begin
  Result := FProgress64;
end;

procedure TSDUWindowsProgressDialog.SetTotal(dwTotal: DWORD);
begin
  FTotal := dwTotal;

  // Note: This does NOT call SetProgress(...)! The total will be updated the
  //       next time "Progress"/"Progress64" is set. This is do that the "time
  //       remaining" timer doesn't get confused by the extra SetProgress(...)
  //       call
  //  FintfProgressDialog.SetProgress(FProgress, FTotal);
end;

function TSDUWindowsProgressDialog.GetTotal(): DWORD;
begin
  Result := FTotal;
end;

procedure TSDUWindowsProgressDialog.SetTotal64(ullTotal: ULONGLONG);
begin
  FTotal64 := ullTotal;

  // Note: This does NOT call SetProgress64(...)! The total will be updated the
  //       next time "Progress"/"Progress64" is set. This is do that the "time
  //       remaining" timer doesn't get confused by the extra SetProgress(...)
  //       call
  //  FintfProgressDialog.SetProgress64(FProgress64, FTotal64);
end;

function TSDUWindowsProgressDialog.GetTotal64(): ULONGLONG;
begin
  Result := FTotal64;
end;

procedure TSDUWindowsProgressDialog.SetCancelMsg(pwzCancelMsg: WideString);
begin
  FintfProgressDialog.SetCancelMsg(PWideChar(pwzCancelMsg), nil);
end;

function TSDUWindowsProgressDialog.Timer(dwTimerAction: DWORD): HResult;
begin
  Result := FintfProgressDialog.Timer(dwTimerAction, nil);
end;

 // -----------------------------------------------------------------------------
 // -----------------------------------------------------------------------------

initialization
  ShellModule := 0;

finalization
  if (ShellModule <> 0) then begin
    FreeLibrary(ShellModule);
  end;

end.
