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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, SDUForms, SDUStdCtrls, SDUComCtrls,
  SDUGeneral;

type
  TSDUProgressDialog = class(TSDUForm)
    pnlProgressBar: TPanel;
    pbCancel: TButton;
    pnlStatusText: TPanel;
    lblStatus: TSDUTruncatingLabel;
    lblEstTimeRemainText: TLabel;
    lblEstTimeRemaining: TLabel;
    pnlProgressBarPlaceholder: TPanel;
    pgbOverall: TProgressBar;
    pgbIndeterminate: TSDProgressBarIndeterminate;
    procedure pbCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fShowStatusText: boolean;
    fConfirmCancel: boolean;
    i64MinValue: int64;
    i64MaxValue: int64;
    i64PositionValue: int64;
    fShowTimeRemaining: boolean;
    fStartTime: TDateTime;
    fCancelSetsModalResult: boolean;

    procedure SetTitle(title: string);
    function  GetTitle(): string;

    function GetIndeterminate(): boolean;
    procedure SetIndeterminate(newValue: boolean);
    function GetIndeterminateRunning(): boolean;
    procedure SetIndeterminateRunning(newValue: boolean);
    function GetIndeterminateUpdate(): integer;
    procedure SetIndeterminateUpdate(newValue: integer);

    procedure SetShowTimeRemaining(value: boolean);


    procedure SetShowStatusText(showStatusText: boolean);
    procedure SetStatusText(statusText: string);
    function  GetStatusText(): string;

    procedure UpdateI64ProgressBar();

    procedure iSetMax(max: integer);
    procedure iSetMin(min: integer);
    procedure iSetPosition(position: integer);
    procedure iSetInversePosition(position: integer);

    procedure i64SetMax(max: int64);
    procedure i64SetMin(min: int64);
    procedure i64SetPosition(position: int64);
    procedure i64SetInversePosition(position: int64);
  public
    Cancel: boolean;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    property  Title: string read GetTitle write SetTitle;

    property  Indeterminate: boolean read GetIndeterminate write SetIndeterminate;
    property  IndeterminateRunning: boolean read GetIndeterminateRunning write SetIndeterminateRunning;
    property  IndeterminateUpdate: integer read GetIndeterminateUpdate write SetIndeterminateUpdate;

    property  ShowTimeRemaining: boolean read fShowTimeRemaining write SetShowTimeRemaining;

    property  Max: integer write iSetMax;
    property  Min: integer write iSetMin;
    property  Position: integer write iSetPosition;
    property  InversePosition: integer write iSetInversePosition;
    procedure IncPosition();

    property  i64Max: int64 read i64MaxValue write i64SetMax;
    property  i64Min: int64 read i64MinValue write i64SetMin;
    property  i64Position: int64 read i64PositionValue write i64SetPosition;
    property  i64InversePosition: int64 write i64SetInversePosition;
    procedure i64IncPosition();

    property  ConfirmCancel: boolean write fConfirmCancel default TRUE;

    property  ShowStatusText: boolean read fShowStatusText write SetShowStatusText;
    property  StatusText: string read GetStatusText write SetStatusText;

    property  CancelSetsModalResult: boolean read fCancelSetsModalResult write fCancelSetsModalResult;
  end;


const
  // Values to be used with TSDUWindowsProgressDialog.Timer(...)
  // Time Actions (dwTimerAction)
  PDTIMER_RESET   = $00000001;       // Reset the timer so the progress will be
                                     // calculated from now until the first
                                     // ::SetProgress() is called so those
                                     // this time will correspond to the values
                                     // passed to ::SetProgress().  Only do
                                     // this before ::SetProgress() is called.
  // Windows Vista and later only...
  PDTIMER_PAUSE   = $00000002;
  PDTIMER_RESUME  = $00000003;


// From: shlguid.h:
const
  //if (_WIN32_IE >= 0x0500)
  /// IProgressDialog
  // {F8383852-FCD3-11d1-A6B9-006097DF5BD4}
  CLSID_ProgressDialog: TGUID = '{F8383852-FCD3-11d1-A6B9-006097DF5BD4}';
  // {EBBC7C04-315E-11d2-B62F-006097DF5BD4}
  IID_IProgressDialog: TGUID = '{EBBC7C04-315E-11d2-B62F-006097DF5BD4}';


  //
  // Progress objects exposed via QueryService
  //
  SID_SProgressUI = '{F8383852-FCD3-11d1-A6B9-006097DF5BD4}';

  ProgressDialog_MAXLINES = 3;

type
  {$EXTERNALSYM IProgressDialog}
  IProgressDialog = interface(IUnknown)
    [SID_SProgressUI]
    function StartProgressDialog(hwndParent: HWND; punkEnableModless: IUnknown; dwFlags: DWORD; pvResevered: Pointer): HResult; stdcall;
    function StopProgressDialog(): HResult; stdcall;
    function SetTitle(pwzTitle: PWideChar): HResult; stdcall;
    function SetAnimation(hInstAnimation: HINST; idAnimation: UINT): HResult; stdcall;
    function HasUserCancelled(): BOOL; stdcall;
    function SetProgress(dwCompleted: DWORD; dwTotal: DWORD): HResult; stdcall;
    function SetProgress64(ullCompleted: ULONGLONG; ullTotal: ULONGLONG): HResult; stdcall;
    function SetLine(dwLineNum: DWORD; pwzString: PWideChar; fCompactPath: BOOL; pvResevered: Pointer): HResult; stdcall;
    function SetCancelMsg(pwzCancelMsg: PWideChar; pvResevered: Pointer): HResult; stdcall;
    function Timer(dwTimerAction: DWORD; pvResevered: Pointer): HResult; stdcall;
  end;


{$M+}  // Required to get rid of compiler warning "W1055 PUBLISHED caused RTTI ($M+) to be added to type '%s'"
  TSDUWindowsProgressDialog = class
  protected
    FintfProgressDialog: IProgressDialog;
    FParentHWnd: THandle;

    FShowAsModal: boolean;
    FShowAutoTime: boolean;
    FShowTime: boolean;
    FShowMinimize: boolean;
    FShowProgressBar: boolean;
    FShowMarqeeProgress: boolean;
    FShowCancel: boolean;
    
    FFileName: string;
    FCommonAVI: TCommonAVI;
    FResHandle: THandle;
    FResId: Integer;
    FResName: string;

    FProgress: DWORD;
    FProgress64: ULONGLONG;
    FTotal: DWORD;
    FTotal64: ULONGLONG;

    FLineText: array [1..ProgressDialog_MAXLINES] of WideString;
    FLineCompact: array [1..ProgressDialog_MAXLINES] of boolean;

    procedure SetTitle(pwzTitle: WideString);

    procedure SetLineText(idx: integer; Value: WideString);
    function  GetLineText(idx: integer): WideString;
    procedure SetLineCompact(idx: integer; Value: boolean);
    function  GetLineCompact(idx: integer): boolean;

    procedure SetCommonAVI(Value: TCommonAVI);
    procedure SetResHandle(newValue: THandle);
    procedure SetResID(newValue: Integer);
    procedure SetResName(newValue: string);

    procedure SetProgress(dwCompleted: DWORD);
    function  GetProgress(): DWORD;
    procedure SetProgress64(ullCompleted: ULONGLONG);
    function  GetProgress64(): ULONGLONG;
    procedure SetTotal(dwTotal: DWORD);
    function  GetTotal(): DWORD;
    procedure SetTotal64(ullTotal: ULONGLONG);
    function  GetTotal64(): ULONGLONG;

    procedure SetCancelMsg(pwzCancelMsg: WideString);

    function GetActualResHandle: THandle;
    function GetActualResId: Integer;
    
  public
    constructor Create();
    destructor Destroy(); override;

    // Note: idx may only be one of 1..3 (1..ProgressDialog_MAXLINES)
    property LineText[idx: integer]: WideString read GetLineText write SetLineText;
    property LineCompact[idx: integer]: boolean read GetLineCompact write SetLineCompact;

    function StartProgressDialog(): HResult;
    function StopProgressDialog(): HResult;

    function HasUserCancelled(): boolean;
    function Timer(dwTimerAction: DWORD): HResult;

  published
    // Set this to Application.Handle to prevent any MessageDlg(...) dialog
    // boxes shown during the long operation getting hidden if the user clicks
    // the main window.
    // Should probably be set to the window's handle if non modal...
    // Defaults to Application.Handle
    property ParentHWnd: THandle read FParentHWnd write FParentHWnd;

    property Title: WideString write SetTitle;

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
    property CommonAVI: TCommonAVI write SetCommonAVI default aviNone;
    property ResHandle: THandle write SetResHandle;
    property ResID: Integer write SetResID;
    property ResName: string write SetResName;

    property Progress: DWORD read GetProgress write SetProgress;
    property Progress64: ULONGLONG read GetProgress64 write SetProgress64;
    property Total: DWORD read GetTotal write SetTotal;
    property Total64: ULONGLONG read GetTotal64 write SetTotal64;

    // Note: The cancel message appears to only be shown if ShowAutoTime is
    //       FALSE - but that means you don't get "time remaining..."
    //       calculated
    property CancelMsg: WideString write SetCancelMsg;

    // Note: These properties *must* be set *before* calling StartProgressDialog(...)
    // The progress dialog box will be modal to the window specified by hwndParent. By default, a progress dialog box is modeless.
    property ShowAsModal: boolean read FShowAsModal write FShowAsModal;
    // Automatically estimate the remaining time and display the estimate on line 3. If this flag is set, IProgressDialog::SetLine can be used only to display text on lines 1 and 2.
    property ShowAutoTime: boolean read FShowAutoTime write FShowAutoTime;
    // Show the "time remaining" text.
    property ShowTime: boolean read FShowTime write FShowTime;
    // Display a minimize button on the dialog box's caption bar.
    property ShowMinimize: boolean read FShowMinimize write FShowMinimize;
    // Display a progress bar. Typically, an application can quantitatively determine how much of the operation remains and periodically pass that value to IProgressDialog::SetProgress. The progress dialog box uses this information to update its progress bar. This flag is typically set when the calling application must wait for an operation to finish, but does not have any quantitative information it can use to update the dialog box.
    property ShowProgressBar: boolean read FShowProgressBar write FShowProgressBar;

    // Note: These properties *must* be set *before* calling StartProgressDialog(...)
    // The next two are for Windows Vista and later only
    // Set the progress bar to marquee mode. This causes the progress bar to scroll horizontally, similar to a marquee display. Use this when you wish to indicate that progress is being made, but the time required for the operation is unknown.
    property ShowMarqeeProgress: boolean read FShowMarqeeProgress write FShowMarqeeProgress;
    // Show a cancel button. The operation cannot be canceled. Use this only when absolutely necessary.
    property ShowCancel: boolean read FShowCancel write FShowCancel;
  end;


procedure Register;

implementation

{$R *.DFM}

uses
  SDUDialogs,
  SDUGraphics,
  SDUi18n,
  ActiveX,
  math,
  DateUtils,
  ComObj;


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
  if ShellModule = 0 then
  begin
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

  pgbIndeterminate.Visible := FALSE;
  pgbIndeterminate.Align := alClient;

  fCancelSetsModalResult := FALSE;

end;

destructor TSDUProgressDialog.Destroy();
begin
  inherited;
end;

procedure TSDUProgressDialog.SetTitle(title: string);
begin
  self.Caption := title;
end;

function  TSDUProgressDialog.GetTitle(): string;
begin
  Result := self.Caption;
end;

procedure TSDUProgressDialog.UpdateI64ProgressBar();
var
  barLength: int64;
  barPos: int64;
  percentPos: integer;
begin
  iSetMin(0);
  iSetMax(100);

  barLength := i64MaxValue - i64MinValue;
  barPos := i64PositionValue - i64MinValue;

  if (i64MaxValue<>0) then
    begin
    percentPos := ((barPos*100) div barLength);
    iSetPosition(percentPos);
    end;

end;



procedure TSDUProgressDialog.iSetMax(max: integer);
begin
  pgbOverall.Max := max;
end;

procedure TSDUProgressDialog.iSetMin(min: integer);
begin
  pgbOverall.Min := min;
end;

procedure TSDUProgressDialog.iSetPosition(position: integer);
var
  currTime: TDateTime;
  timeDone: TDateTime;
  newLabel: string;
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
  timeRemaining: TDateTime;
  cntDone: integer;
  cntTotal: integer;
  cntRemaining: integer;
begin
  if position>pgbOverall.Max then
    begin
    pgbOverall.position := pgbOverall.Max
    end
  else if position<pgbOverall.Min then
    begin
    pgbOverall.position := pgbOverall.Min
    end
  else
    begin
    pgbOverall.position := position;
    end;

  if fShowTimeRemaining then
    begin
    newLabel := RS_UNKNOWN;

    currTime := Now;
    if (currTime <> fStartTime) then
      begin
      timeDone     := currTime - fStartTime;

      cntDone      := pgbOverall.Position - pgbOverall.Min;
      cntTotal     := pgbOverall.Max - pgbOverall.Min;
      cntRemaining := cntTotal - cntDone;

      if (cntDone <> 0) then
        begin
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
        if (ADay > 0) then
          begin
          newLabel := SDUParamSubstitute(
                             _('%1 days, %2 hours'),
                             [ADay, AHour]
                            );
          end
        else if (AHour > 0) then
          begin
          newLabel := SDUParamSubstitute(
                             _('%1 hours, %2 minutes'),
                             [AHour, AMinute]
                            );
          end
        else if (AMinute > 0) then
          begin
          newLabel := SDUParamSubstitute(
                             _('%1 minutes, %2 seconds'),
                             [AMinute, ASecond]
                            );
          end
        else
          begin
          newLabel := SDUParamSubstitute(_('%1 seconds'), [ASecond]);
          end;

        end;

      lblEstTimeRemaining.Caption := newLabel;
      end;
    end;

  self.refresh;
  Application.ProcessMessages();
end;

procedure TSDUProgressDialog.iSetInversePosition(position: integer);
begin
  iSetPosition(pgbOverall.Max-position);

end;

procedure TSDUProgressDialog.IncPosition();
begin
  iSetPosition(pgbOverall.Position+1);

end;


procedure TSDUProgressDialog.i64SetMax(max: int64);
begin
  i64MaxValue := max;
  UpdateI64ProgressBar();

end;

procedure TSDUProgressDialog.i64SetMin(min: int64);
begin
  i64MinValue := min;
  UpdateI64ProgressBar();

end;

procedure TSDUProgressDialog.i64SetPosition(position: int64);
begin
  i64PositionValue := position;
  UpdateI64ProgressBar();

end;

procedure TSDUProgressDialog.i64SetInversePosition(position: int64);
begin
  i64PositionValue := i64MaxValue-position;
  UpdateI64ProgressBar();

end;

procedure TSDUProgressDialog.i64IncPosition();
begin
  i64PositionValue := i64PositionValue + 1;
  UpdateI64ProgressBar();

end;


procedure TSDUProgressDialog.pbCancelClick(Sender: TObject);
begin
  Cancel := TRUE;
  pbCancel.Enabled := FALSE;
  if fConfirmCancel then
    begin
    SDUMessageDlg(_('Operation cancelled by user'));
    end;

  if fCancelSetsModalResult then
    begin
    ModalResult := mrCancel;
    end;

end;

procedure TSDUProgressDialog.FormCreate(Sender: TObject);
begin
  pbCancel.Enabled := TRUE;
  Cancel := FALSE;

  pnlStatusText.Caption := '';
  pnlStatusText.BevelInner := bvNone;
  pnlStatusText.BevelOuter := bvNone;
  pnlProgressBar.Caption := '';
  pnlProgressBar.BevelInner := bvNone;
  pnlProgressBar.BevelOuter := bvNone;

  lblStatus.Caption := '';
  lblStatus.AutoSize := FALSE;
  lblStatus.Width := pnlStatusText.Width - (2 * lblStatus.left);

  fShowStatusText := TRUE;  // The form is designed with it shown
  ShowStatusText := FALSE;  // Default to just the progress bar
  StatusText := '';

  lblEstTimeRemaining.Caption := RS_UNKNOWN;
  lblEstTimeRemaining.Visible := FALSE;
  lblEstTimeRemainText.Visible := FALSE;

end;

procedure TSDUProgressDialog.SetShowStatusText(showStatusText: boolean);
begin
  if (fShowStatusText <> showStatusText) then
    begin
    if showStatusText then
      begin
      self.Height := self.Height + pnlStatusText.Height;
      pnlStatusText.Visible := TRUE;
      end
    else
      begin
      self.Height := self.Height - pnlStatusText.Height;
      pnlStatusText.Visible := FALSE;
      end;
    end;

  fShowStatusText := showStatusText;
end;

procedure TSDUProgressDialog.SetStatusText(statusText: string);
begin
  lblStatus.caption := statusText;

end;

function TSDUProgressDialog.GetStatusText(): string;
begin
  Result := lblStatus.caption;

end;

procedure TSDUProgressDialog.FormShow(Sender: TObject);
begin
  fStartTime := Now;
end;

procedure TSDUProgressDialog.SetShowTimeRemaining(value: boolean);
begin
  fShowTimeRemaining := value;

  lblEstTimeRemaining.Visible := fShowTimeRemaining;
  lblEstTimeRemainText.Visible := fShowTimeRemaining;
end;

function TSDUProgressDialog.GetIndeterminate(): boolean;
begin
  Result := pgbIndeterminate.Visible;
end;

procedure TSDUProgressDialog.SetIndeterminate(newValue: boolean);
begin
  pgbIndeterminate.Visible := newValue;
  pgbOverall.Visible := not(pgbIndeterminate.Visible);
end;

function TSDUProgressDialog.GetIndeterminateRunning(): boolean;
begin
  Result := pgbIndeterminate.Marquee;
end;

procedure TSDUProgressDialog.SetIndeterminateRunning(newValue: boolean);
begin
  pgbIndeterminate.Marquee := newValue;
end;

function TSDUProgressDialog.GetIndeterminateUpdate(): integer;
begin
  Result := pgbIndeterminate.MarqueeUpdate;
end;

procedure TSDUProgressDialog.SetIndeterminateUpdate(newValue: integer);
begin
  pgbIndeterminate.MarqueeUpdate := newValue;
end;


// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------
constructor TSDUWindowsProgressDialog.Create();
var
  i: integer;
begin
//  inherited;  // Don't call if inherit from TObject!

  FParentHWnd := Application.Handle;

  FShowAsModal:= TRUE;
  FShowAutoTime:= TRUE;
  FShowTime:= TRUE;
  FShowMinimize:= FALSE;
  FShowProgressBar:= TRUE;
  FShowMarqeeProgress:= FALSE;
  ShowCancel:= TRUE;

  FCommonAVI := aviNone;
  FFileName := '';
  FResHandle := 0;
  FResName := '';
  FResId := 0;

  for i:=low(FLineText) to high(FLineText) do
    begin
    FLineText[i] := '';
    FLineCompact[i] := FALSE;
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
  if FCommonAVI <> aviNone then Result := GetShellModule
  else if FResHandle <> 0 then Result := FResHandle
  else if MainInstance <> 0 then Result := MainInstance
  else Result := HInstance;
end;

// Ripped from Delphi's TAnimate...
function TSDUWindowsProgressDialog.GetActualResId: Integer;
const
  CommonAVIId: array[TCommonAVI] of Integer = (0, 150, 151, 152, 160, 161, 162,
    163, 164);
begin
  if FCommonAVI <> aviNone then Result := CommonAVIId[FCommonAVI]
  else if FFileName <> '' then Result := Integer(FFileName)
  else if FResName <> '' then Result := Integer(FResName)
  else Result := FResId;
end;

function TSDUWindowsProgressDialog.StartProgressDialog(): HResult;
var
  dwFlags: DWORD;
begin
  dwFlags := PROGDLG_NORMAL;
  if ShowAsModal then
    begin
    dwFlags := dwFlags + PROGDLG_MODAL;
    end;
  if ShowAutoTime then
    begin
    dwFlags := dwFlags + PROGDLG_AUTOTIME;
    end;
  if not(ShowTime) then
    begin
    dwFlags := dwFlags + PROGDLG_NOTIME;
    end;
  if not(ShowMinimize) then
    begin
    dwFlags := dwFlags + PROGDLG_NOMINIMIZE;
    end;
  if not(ShowProgressBar) then
    begin
    dwFlags := dwFlags + PROGDLG_NOPROGRESSBAR;
    end;
  if ShowMarqeeProgress then
    begin
    dwFlags := dwFlags + PROGDLG_MARQUEEPROGRESS;
    end;
  if not(ShowCancel) then
    begin
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

procedure TSDUWindowsProgressDialog.SetLineText(idx: integer; Value: WideString);
begin
  FLineText[idx] := Value;
  FintfProgressDialog.SetLine(idx, PWideChar(FLineText[idx]), FLineCompact[idx], nil);
end;

function TSDUWindowsProgressDialog.GetLineText(idx: integer): WideString;
begin
  Result := FLineText[idx];
end;

procedure TSDUWindowsProgressDialog.SetLineCompact(idx: integer; Value: boolean);
begin
  FLineCompact[idx] := Value;
  FintfProgressDialog.SetLine(idx, PWideChar(FLineText[idx]), FLineCompact[idx], nil);
end;

function TSDUWindowsProgressDialog.GetLineCompact(idx: integer): boolean;
begin
  Result := FLineCompact[idx];
end;

procedure TSDUWindowsProgressDialog.SetCommonAVI(Value: TCommonAVI);
begin
  // Animations not supported under Windows Vista and later
  if not(SDUOSVistaOrLater()) then
    begin
    FCommonAVI := Value;
    FFileName := '';
    FResHandle := 0;
    FResName := '';
    FResId := 0;

    FintfProgressDialog.SetAnimation(GetActualResHandle, GetActualResID);
    end;
end;

procedure TSDUWindowsProgressDialog.SetResHandle(newValue: THandle);
begin
end;

procedure TSDUWindowsProgressDialog.SetResID(newValue: Integer);
begin
end;

procedure TSDUWindowsProgressDialog.SetResName(newValue: string);
begin
end;

function TSDUWindowsProgressDialog.HasUserCancelled(): boolean;
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
  if (ShellModule <> 0) then
    begin
    FreeLibrary(ShellModule);
    end;

END.


