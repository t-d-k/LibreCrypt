unit sdMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, shellapi, ExtCtrls, ToolWin, ComCtrls,

  sdGlobals, sdPassword, sdAbout,

  StdCtrls, ImgList, OTFE_U, OTFEScramDisk_U, SdStructures_U;

type
  TMainForm = class(TForm)
    StatusBar1 : TStatusBar;
    ControlBar1 : TControlBar;
    ToolBar1 : TToolBar;
    ToolBar2 : TToolBar;
    Panel1 : TPanel;
    MainMenu1 : TMainMenu;
    Disk1 : TMenuItem;
    Mount1 : TMenuItem;
    Dismount1 : TMenuItem;
    Dismountbrutal1 : TMenuItem;
    N1 : TMenuItem;
    Exit1 : TMenuItem;
    Passwords1 : TMenuItem;
    SetPasswords1 : TMenuItem;
    SetPasswordsinRedScreen1 : TMenuItem;
    ClearPasswords1 : TMenuItem;
    Help1 : TMenuItem;
    AboutTkrScramDisk1 : TMenuItem;
    Tools1 : TMenuItem;
    UpdateDiskinformation1 : TMenuItem;
    Registersvl1 : TMenuItem;
    ToolButton1 : TToolButton;
    ToolButton2 : TToolButton;
    ToolButton3 : TToolButton;
    ToolButton4 : TToolButton;
    MountPartitions1 : TMenuItem;
    N2 : TMenuItem;
    OpenDialog1 : TOpenDialog;
    ListBox1 : TListBox;
    Splitter1 : TSplitter;
    ImageList1 : TImageList;
    ToolButton5 : TToolButton;
    ToolButton6 : TToolButton;
    ToolButton7 : TToolButton;
    ToolButton8 : TToolButton;
    ToolButton9 : TToolButton;
    ToolButton10 : TToolButton;
    ToolButton11 : TToolButton;
    ToolButton12 : TToolButton;
    ToolButton13 : TToolButton;
    ToolButton14 : TToolButton;
    GroupBox1 : TGroupBox;
    lblComments : TLabel;
    Label2 : TLabel;
    lblFileName : TLabel;
    lblCipher : TLabel;
    Label4 : TLabel;
    Label3 : TLabel;
    lblPrefer : TLabel;
    lblNoAccesses : TLabel;
    Label6 : TLabel;
    lblLastAccess : TLabel;
    Label7 : TLabel;
    Label1 : TLabel;
    lblMountType : TLabel;
    PopupMenu1: TPopupMenu;
    DismountSlot1: TMenuItem;
    DismountSlotBrutal1: TMenuItem;
    OpenExplorer: TMenuItem;
    N01: TMenuItem;
    DismountAll1: TMenuItem;
    DismountAllBrutal1: TMenuItem;
    N3: TMenuItem;
    ScramDisk: TOTFEScramDisk;
    procedure SetPasswords1Click(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure SetPasswordsinRedScreen1Click(Sender : TObject);
    procedure ClearPasswords1Click(Sender : TObject);
    procedure Mount1Click(Sender : TObject);
    procedure MountPartitions1Click(Sender : TObject);
    procedure Dismountbrutal1Click(Sender : TObject);
    procedure UpdateDiskinformation1Click(Sender : TObject);
    procedure AboutTkrScramDisk1Click(Sender : TObject);
    procedure Exit1Click(Sender : TObject);
    procedure Dismount1Click(Sender : TObject);
    procedure ListBox1DrawItem(Control : TWinControl; Index : Integer;
      Rect : TRect; State : TOwnerDrawState);
    procedure Splitter1Moved(Sender : TObject);
    procedure FormResize(Sender : TObject);
    procedure ListBox1Click(Sender : TObject);
    procedure ControlBar1Resize(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OpenExplorerClick(Sender: TObject);
    procedure DismountAll1Click(Sender: TObject);
    procedure DismountAllBrutal1Click(Sender: TObject);
  private
    procedure UpdateCurrentSlotInformation;
    procedure ClearSlotDisplay;
  public
    { Public declarations }
  end;

var
  MainForm : TMainForm;

implementation

{$R *.DFM}

{$R DISKS.RES}

procedure TMainForm.SetPasswords1Click(Sender : TObject);
begin
  if TPasswordForm.Execute(Password1, Password2, Password3, Password4) then
    ScramDisk.SetPasswords(Password1, Password2, Password3, Password4);
end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  //Delphi has a problem with TToolbars moving around.
  ToolBar1.Left := 11;
  ToolBar2.Left := 11;
  ScramDisk.Active := true;
  UpdateDiskinformation1Click(nil);
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  ScramDisk.Active := false;
end;

procedure TMainForm.SetPasswordsinRedScreen1Click(Sender : TObject);
begin
  ScramDisk.SetPasswordsRedScreen('Enter your passwords')
end;

procedure TMainForm.ClearPasswords1Click(Sender : TObject);
begin
  ScramDisk.ClearAllPasswords;
end;

procedure TMainForm.Mount1Click(Sender : TObject);
begin
  if OpenDialog1.Execute then
    ScramDisk.Mount(OpenDialog1.FileName);
  UpdateDiskinformation1Click(nil);
end;

procedure TMainForm.MountPartitions1Click(Sender : TObject);
begin
  ScramDisk.MountPartitions;
  UpdateDiskinformation1Click(nil);
end;

procedure TMainForm.Dismountbrutal1Click(Sender : TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    if MessageDlg('Are you sure you want to dismount Slot ' + IntToStr(ListBox1.ItemIndex) + '?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        ScramDisk.Dismount(ListBox1.ItemIndex,true);
        UpdateDiskinformation1Click(nil);
      end;
  UpdateDiskinformation1Click(nil);
  ListBox1.ItemIndex:=-1;
  ClearSlotDisplay;
end;

procedure TMainForm.UpdateDiskinformation1Click(Sender : TObject);
var
  Slot : Integer;
  CurrentSel:integer;
  SlotInfo : TSlotInfo;
begin
  currentsel:=ListBox1.ItemIndex;
  ScramDisk.UpdateSlotInfo;
  ListBox1.Items.Clear;
  for Slot := 0 to 7 do
    begin
      SlotInfo := ScramDisk.GetSlotInfo(Slot);
      if SlotInfo.MountType <> mtNotMounted then
        ListBox1.Items.Add(IntToStr(Slot) + ' ' + SlotInfo.DriveMountedAs + ':' +
          '|' + IntToStr(Ord(SlotInfo.MountType)))
      else
        ListBox1.Items.Add(IntToStr(Slot) + ' Not mounted');
    end;
  ClearSlotDisplay;
  ListBox1.ItemIndex:=CurrentSel;
end;

procedure TMainForm.AboutTkrScramDisk1Click(Sender : TObject);
begin
  TAboutForm.Execute;
end;

procedure TMainForm.Exit1Click(Sender : TObject);
begin
  Close;
end;

procedure TMainForm.Dismount1Click(Sender : TObject);
begin
  ScramDisk.Dismount(ListBox1.ItemIndex);
  Application.ProcessMessages;
  UpdateDiskinformation1Click(nil);
  ListBox1.ItemIndex:=-1;
  ClearSlotDisplay;
end;

procedure TMainForm.ListBox1DrawItem(Control : TWinControl; Index : Integer;
  Rect : TRect; State : TOwnerDrawState);
const
  Margin = 2;
var
  ListBox : TListBox;
  Slot : string;
  VolumeName : string;
  MountType : TMountType;
  MountText : string;
  DriveImage : TBitmap;
  PositionOfPipe : Integer;
  PositionOfColon : Integer;
  DriveLetter : string;
  HoldTextColor : TColor;
  HoldTextSize : Integer;
  DestRect : TRect;
begin
  ListBox := Control as TListBox;
  ListBox.Canvas.FillRect(Rect);
  Slot := Copy(ListBox1.Items[Index], 1, 1);
  ListBox.Canvas.TextOut(Rect.Left + Margin, Rect.Top + Margin, Slot);
  PositionOfPipe := Pos('|', ListBox1.Items[Index]);
  if PositionOfPipe > 0 then
    begin
      MountText := Copy(ListBox1.Items[Index], Pos('|', ListBox1.Items[Index]) + 1, 1);
      MountType := TMountType(StrToInt(MountText));
      DriveImage := TBitmap.Create;
      try
        if MountType = mtWAVFile then
          DriveImage.LoadFromResourceName(hInstance, 'WAVDISK')
        else
          DriveImage.LoadFromResourceName(hInstance, 'DISK');
        DestRect.Left := (Rect.Right - Rect.Left) div 2 - DriveImage.Width div 2;
        DestRect.Top := Rect.Top + Margin + ListBox.Canvas.TextHeight('0');
        DestRect.Right := DestRect.Left + DriveImage.Width;
        DestRect.Bottom := DestRect.Top + DriveImage.Height;
        ListBox.Canvas.BrushCopy(DestRect, DriveImage, DriveImage.Canvas.ClipRect, clWhite);
        {ListBox.Canvas.Draw((Rect.Right-Rect.Left) div 2 - DriveImage.Width div 2,
          Rect.Top + Margin + ListBox.Canvas.TextHeight('0'),
          DriveImage);}
      finally
        DriveImage.Free;
      end;
    end
  else
    begin
      HoldTextColor := ListBox.Canvas.Font.Color;
      HoldTextSize := ListBox.Canvas.Font.Size;
      ListBox.Canvas.Font.Color := clGray;
      ListBox.Canvas.Font.Size := 20;
      ListBox.Canvas.TextOut((Rect.Right - Rect.Left) div 2 - ListBox.Canvas.TextWidth('EMPTY') div 2,
        Rect.Bottom - (Margin + ListBox.Canvas.TextHeight('EMPTY')), 'EMPTY');
      ListBox.Canvas.Font.Color := HoldTextColor;
      ListBox.Canvas.Font.Size := HoldTextSize;
    end;
  PositionOfColon := Pos(':', ListBox1.Items[Index]);
  if PositionOfColon > 0 then
    begin
      DriveLetter := Copy(ListBox1.Items[Index], PositionOfColon - 1, 2);
      ListBox.Canvas.TextOut(Rect.Right - (Margin + ListBox.Canvas.TextWidth('W:')),
        Rect.Top + Margin, DriveLetter);
      VolumeName := Copy(ListBox1.Items[Index], PositionOfColon + 1, PositionOfPipe - PositionOfCOlon - 1);
      ListBox.Canvas.TextOut((Rect.Right - Rect.Left) div 2 - ListBox.Canvas.TextWidth(VolumeName) div 2,
        Rect.Bottom - (Margin + ListBox.Canvas.TextHeight(VolumeName)), VolumeName);
    end;
end;

procedure TMainForm.Splitter1Moved(Sender : TObject);
begin
  ListBox1.Invalidate;
  GroupBox1.Width := Panel1.Width - 13;
  lblComments.Width := GroupBox1.Width - 20;
end;

procedure TMainForm.FormResize(Sender : TObject);
begin
  ListBox1.Invalidate;
end;

procedure TMainForm.ListBox1Click(Sender : TObject);
begin
  UpdateCurrentSlotInformation;
end;

procedure TMainForm.UpdateCurrentSlotInformation;
var
  SlotInfo : TSlotInfo;
begin
  SlotInfo := ScramDisk.GetSlotInfo(ListBox1.ItemIndex);
  if SlotInfo.MountType <> mtNotMounted then
    begin
      lblComments.Caption := SlotInfo.Comments;
      lblFileName.Caption := SlotInfo.FileName;
      lblCipher.Caption := SlotInfo.CipherType;
      lblPrefer.Caption := SlotInfo.PreferredDrive;
      lblNoAccesses.Caption := IntToStr(SlotInfo.NoAccesses);
      lblLastAccess.Caption := FormatDateTime('dd mmmm yyyy "at" hh:nn', SlotInfo.LastAccess);
      case SlotInfo.MountType of
        mtPartition : lblMountType.Caption := 'Partition';
        mtSVLFile : lblMountType.Caption := 'Container File';
        mtWAVFile : lblMountType.Caption := 'WAV/Stenanography File';
      end;
    end
  else
    ClearSlotDisplay;
end;

procedure TMainForm.ControlBar1Resize(Sender: TObject);
begin
  ListBox1.Invalidate;
end;

procedure TMainForm.PopupMenu1Popup(Sender: TObject);
begin
  DismountSlot1.Enabled := ListBox1.ItemIndex <> -1;
  DismountSlotBrutal1.Enabled := ListBox1.ItemIndex <> -1;
end;

procedure TMainForm.ClearSlotDisplay;
begin
  lblComments.Caption := '';
  lblFileName.Caption := '';
  lblCipher.Caption := '';
  lblPrefer.Caption := '';
  lblNoAccesses.Caption := '';
  lblLastAccess.Caption := '';
  lblMountType.Caption := '';
end;

procedure TMainForm.ListBox1DblClick(Sender: TObject);
begin
  OpenExplorerClick(nil);
end;

procedure TMainForm.ListBox1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbRight then
    with Sender as TListBox do
      if ItemAtPos(Point(x,y),true) <> -1 then
        begin
          ItemIndex:=ItemAtPos(Point(x,y),true);
          UpdateCurrentSlotInformation;
        end;
end;

procedure TMainForm.OpenExplorerClick(Sender: TObject);
var
  SlotInfo : TSlotInfo;
begin
  SlotInfo := ScramDisk.GetSlotInfo(ListBox1.ItemIndex);
  if SlotInfo.MountType <> mtNotMounted then
  begin
    ShellExecute(Application.Handle, nil, pchar(slotinfo.DriveMountedAs+':\'),
      nil, nil, SW_SHOWNOACTIVATE);
  end;
end;

procedure TMainForm.DismountAll1Click(Sender: TObject);
var
  count:integer;
begin
  for count:=0 to 7 do
    scramdisk.dismount(count);
end;

procedure TMainForm.DismountAllBrutal1Click(Sender: TObject);
var
  count:integer;
begin
  for count:=0 to 7 do
    scramdisk.dismount(count,true);
end;

end.

