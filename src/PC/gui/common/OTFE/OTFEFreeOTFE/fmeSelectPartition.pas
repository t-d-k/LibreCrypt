unit fmeSelectPartition;

interface

 // NOTE: The "Tabs" property of the TTabControl has the tab captions as it's
 //       strings (as per normal), but each of the TStringList's Object's is a
 //       DWORD.
 //       If the DWORD's highest WORD is HIWORD_CDROM, the lowest WORD is an
 //       index into FRemovableDevices
 //       If the DWORD's highest WORD is HIWORD_DISK, the lowest WORD is a disk
 //       number

 // NOTE: This frame can act "oddly" if it's put on a TTabSheet; controls set
 //       as "Visible := FALSE" were being set to non-visible, but were still
 //       visible on the display. To fix this, the code segment marked
 //       "[TAB_FIX]" in this source was put in, which seems to cure things
 //       PROVIDED THAT Initialize(...) is called after the tab is selected for
 //       the first time.
 //       THIS *SHOULDN'T* BE NEEDED - but is!!!

uses
  ActnList, Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, fmeDiskPartitionsPanel, fmeSDUBlocks, fmeSDUDiskPartitions, Forms,
  Graphics, ImgList, Menus, Messages, OTFEFreeOTFEBase_U,
  SDUGeneral, StdCtrls, SysUtils, Variants, Windows;

type
  TfmeSelectPartition = class (TFrame)
    TabControl1:   TTabControl;
    pnlNoPartitionDisplay: TPanel;
    ckShowCDROM:   TCheckBox;
    ckEntireDisk:  TCheckBox;
    lblErrorWarning: TLabel;
    ilErrorWarning: TImageList;
    imgErrorWarning: TImage;
    PopupMenu1:    TPopupMenu;
    ActionList1:   TActionList;
    actProperties: TAction;
    Properties1:   TMenuItem;
    SDUDiskPartitionsPanel1: TfmeDiskPartitionsPanel;
    procedure TabControl1Change(Sender: TObject);
    procedure ckShowCDROMClick(Sender: TObject);
    procedure ckEntireDiskClick(Sender: TObject);
    procedure SDUDiskPartitionsPanel1Changed(Sender: TObject);
    procedure actPropertiesExecute(Sender: TObject);
    procedure FrameResize(Sender: TObject);
  private
    FAllowCDROM: Boolean;
    FOnChange:   TNotifyEvent;

    FRemovableDevices: TStringList;

    function GetSelectedDevice(): String;

    procedure SetPartitionDisplayDisk(diskNo: Integer);
    procedure SetAllowCDROM(allow: Boolean);
    procedure ShowPartitionControl();

    function GetSyntheticDriveLayout(): Boolean;

    procedure PopulateTabs();

    procedure NotifyChanged(Sender: TObject);
    procedure UpdateErrorWarning();
//    procedure CenterErrorWarning();

    procedure EnableDisableControls();

    function IsCDROMTabSelected(): Boolean;
    procedure SelectionErrorWarning(var msg: String; var isWarning: Boolean; var isError: Boolean);
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    procedure Initialize();
    function SelectedSize(): ULONGLONG;
  published
    property SelectedDevice: String Read GetSelectedDevice;
    property AllowCDROM: Boolean Read FAllowCDROM Write SetAllowCDROM default True;
    property OnChange: TNotifyEvent Read FOnChange Write FOnChange;

    property SyntheticDriveLayout: Boolean Read GetSyntheticDriveLayout;

  end;

implementation

{$R *.dfm}

uses
  GraphUtil,  // Required for GetHighLightColor(...)
  SDUDiskPropertiesDlg,
  SDUi18n,
  SDUPartitionPropertiesDlg;

const
  COLOR_USABLE   = TColor($00C000); // clLime too bright, clGreen too dark
  COLOR_UNUSABLE = TColor($0000E0); // clRed too bright

  // Bitmask - these go up 1, 2, 4, 8, 16, 32, etc in the hi-word
  HIWORD_DISK  = $00010000;
  HIWORD_CDROM = $00020000;
  HIWORD_OTHER = $00040000;
  HIWORD_etc   = $00080000;

  // Images indexes within ilErrorWarning
  ICON_NONE    = 0;
  ICON_WARNING = 1;
  ICON_ERROR   = 2;

resourcestring
  RS_NO_PARTITIONS_FOUND = '<No partitions found>';

  RS_CANT_GET_DISK_LAYOUT = '<Unable to get disk layout information for disk #%1>';
  RS_ENTIRE_DISK_X        = '<Entire disk #%1>';
  RS_CDROM_IN_X           = '<CDROM/DVD in %1>';

  RS_DBLCLK_PROMPT_PARTITION = 'Doubleclick partition to display properties';
  RS_DBLCLK_PROMPT_DISK      = 'Doubleclick to display disk properties';

  RS_PARTITION = 'Partition #%1';

  RS_PART_ERROR_NO_PARTITION_NO      =
    'Windows has not allocated this partition a partition number';
  RS_PART_ERROR_SYSTEM_PARTITION     = 'This is where Windows is installed';
  RS_PART_ERROR_PROG_FILES_PARTITION = 'This is where program files are installed';
  RS_PART_WARN_BOOTABLE              = 'Partition is marked as bootable or required';


procedure TfmeSelectPartition.ckEntireDiskClick(Sender: TObject);
begin
  ShowPartitionControl();
  NotifyChanged(self);
end;

procedure TfmeSelectPartition.ckShowCDROMClick(Sender: TObject);
begin
  PopulateTabs();
  TabControl1Change(nil);
  NotifyChanged(self);
end;

constructor TfmeSelectPartition.Create(AOwner: TComponent);
begin
  inherited;
  FRemovableDevices                 := TStringList.Create();
  SDUDiskPartitionsPanel1.OnChanged := SDUDiskPartitionsPanel1Changed;
end;

destructor TfmeSelectPartition.Destroy();
begin
  FRemovableDevices.Free();
  inherited;
end;

procedure TfmeSelectPartition.Initialize();

  procedure ToggleControl(ctrl: TControl);
  var
    prevValue: Boolean;
  begin
    prevValue    := ctrl.Visible;
    ctrl.Visible := False;
    ctrl.Visible := True;
    ctrl.Visible := prevValue;
  end;

begin
  actProperties.Enabled := False;

  // -- begin [TAB_FIX] --
  ToggleControl(SDUDiskPartitionsPanel1);
  ToggleControl(pnlNoPartitionDisplay);
  ToggleControl(ckShowCDROM);
  // -- end [TAB_FIX] --

  // Fallback...
  SDUDiskPartitionsPanel1.SetCaption(RS_NO_PARTITIONS_FOUND);

  SDUDiskPartitionsPanel1.Align := alClient;
  pnlNoPartitionDisplay.Align   := alClient;

  PopulateTabs();
  TabControl1Change(nil);

  pnlNoPartitionDisplay.Color      := GetHighLightColor(COLOR_USABLE);
  pnlNoPartitionDisplay.Font.Style := pnlNoPartitionDisplay.Font.Style + [fsBold];

  // Yes, this is stupid, but required if frame is on a tab?!!
  AllowCDROM := AllowCDROM;

  // Sanity...
  imgErrorWarning.Width  := ilErrorWarning.Width;
  imgErrorWarning.Height := ilErrorWarning.Height;

  // Set the background color for the error/warning images
  ilErrorWarning.BkColor      := self.color;
  imgErrorWarning.Transparent := True;

  // Nudge frame so it sizes itself correctly
  self.Resize();

end;

procedure TfmeSelectPartition.PopulateTabs();
var
  diskNo:          TSDUArrayInteger;
  cdromDevices:    TStringList;
  title:           TStringList;
  deviceIndicator: DWORD;
  i:               Integer;
begin
  TabControl1.Tabs.Clear();
  FRemovableDevices.Clear();

  if GetFreeOTFEBase().HDDNumbersList(diskNo) then begin
    for i := low(diskNo) to high(diskNo) do begin
      deviceIndicator := HIWORD_DISK + DWORD(diskNo[i]);
      TabControl1.Tabs.AddObject(SDUParamSubstitute(_('Disk #%1'), [diskNo[i]]),
        TObject(deviceIndicator));
    end;
  end;

  if (AllowCDROM and ckShowCDROM.Checked) then begin
    cdromDevices := TStringList.Create();
    title        := TStringList.Create();
    try
      if GetFreeOTFEBase().CDROMDeviceList(cdromDevices, title) then begin
        FRemovableDevices.Assign(cdromDevices);
        for i := 0 to (title.Count - 1) do begin
          deviceIndicator := HIWORD_CDROM + DWORD(i);
          TabControl1.Tabs.AddObject(title[i], TObject(deviceIndicator));
        end;
      end;
    finally
      title.Free();
      cdromDevices.Free();
    end;
  end;

  if (TabControl1.Tabs.Count > 0) then begin
    TabControl1.TabIndex := 0;
  end else begin
    TabControl1.TabIndex := -1;
  end;

end;

procedure TfmeSelectPartition.SDUDiskPartitionsPanel1Changed(Sender: TObject);
begin
  NotifyChanged(self);
end;

procedure TfmeSelectPartition.TabControl1Change(Sender: TObject);
var
  deviceIndicator: DWORD;
  useDevice:       Integer;
begin
  useDevice := NO_DISK;

  if (TabControl1.TabIndex >= 0) then begin
    deviceIndicator := DWORD(TabControl1.Tabs.Objects[TabControl1.TabIndex]);

    if ((deviceIndicator and HIWORD_DISK) = HIWORD_DISK) then begin
      useDevice := (deviceIndicator and $FFFF);
    end;
  end;

  SetPartitionDisplayDisk(useDevice);
  ShowPartitionControl();

  // We reset this control as if switching from to new disk, we don't want the
  // whole disk used, unless the user explicitly selects it
  // If CDROM/DVDROM selected, must use whole CD/DVD
  ckEntireDisk.Checked := IsCDROMTabSelected();

  EnableDisableControls();
  NotifyChanged(self);
end;

// Set to NO_PARTITION to indicate control should display no partition
procedure TfmeSelectPartition.SetPartitionDisplayDisk(diskNo: Integer);
var
  i:   Integer;
  blk: TBlock;
begin
  // We know what we're doing...
  // We trap partitions with partition number zero, and we also paint them
  // COLOR_UNUSABLE; see below
  SDUDiskPartitionsPanel1.ShowPartitionsWithPNZero := True;

  try
    SDUDiskPartitionsPanel1.DiskNumber := diskNo;
  except
    on E: Exception do begin
      // Swallow any exception
    end;
  end;

  // Setup the partition colours we want to be used
  for i := 0 to (SDUDiskPartitionsPanel1.Count - 1) do begin
    blk := SDUDiskPartitionsPanel1.Item[i];

    blk.BkColor := COLOR_UNUSABLE;
    // Only partitions numbered 1 and above are usable
    if (SDUDiskPartitionsPanel1.PartitionInfo[i].PartitionNumber > 0) then begin
      blk.BkColor := COLOR_USABLE;
    end;

    // If synthetic layout, clear block captions
    if SDUDiskPartitionsPanel1.SyntheticDriveLayout then begin
      blk.Caption    := SDUParamSubstitute(RS_PARTITION,
        [SDUDiskPartitionsPanel1.PartitionInfo[i].PartitionNumber]);
      blk.SubCaption := '';
    end;

    SDUDiskPartitionsPanel1.Item[i] := blk;
  end;

  UpdateErrorWarning();

end;

procedure TfmeSelectPartition.ShowPartitionControl();
var
  showPartCtrl: Boolean;
begin
  showPartCtrl := (
    // We have a disk number...
    (SDUDiskPartitionsPanel1.DiskNumber > NO_DISK) and
    // The partition display is valid...
    SDUDiskPartitionsPanel1.DriveLayoutInformationValid and
    // We're not using the entire drive
    not (ckEntireDisk.Checked));

  SDUDiskPartitionsPanel1.Visible := showPartCtrl;
  pnlNoPartitionDisplay.Visible   := not (showPartCtrl);

  actProperties.Visible := True;
  if (SDUDiskPartitionsPanel1.DiskNumber > NO_DISK) then begin
    if (ckEntireDisk.Checked or not (SDUDiskPartitionsPanel1.DriveLayoutInformationValid)
      // Display msg for full disk if no partitions displayed
      ) then begin
      // actProperties.Visible already set to TRUE
    end else begin
      // Further information on partitions not available if synthetic drive layout
      actProperties.Visible := not (SDUDiskPartitionsPanel1.SyntheticDriveLayout);
    end;
  end;

  pnlNoPartitionDisplay.Caption := '';
  if (SDUDiskPartitionsPanel1.DiskNumber < 0) then begin
    // Sanity check in case there's no tabs
    if (TabControl1.TabIndex >= 0) then begin
      // CD/DVD drive selected
      pnlNoPartitionDisplay.Caption :=
        SDUParamSubstitute(RS_CDROM_IN_X, [TabControl1.Tabs[TabControl1.TabIndex]]);
    end;
  end else begin
    if not (SDUDiskPartitionsPanel1.DriveLayoutInformationValid) then begin
      pnlNoPartitionDisplay.Caption :=
        SDUParamSubstitute(RS_CANT_GET_DISK_LAYOUT, [SDUDiskPartitionsPanel1.DiskNumber]);
    end else begin
      pnlNoPartitionDisplay.Caption :=
        SDUParamSubstitute(RS_ENTIRE_DISK_X, [SDUDiskPartitionsPanel1.DiskNumber]);
    end;
  end;

end;

function TfmeSelectPartition.GetSelectedDevice(): String;
var
  deviceIndicator: DWORD;
  useDevice:       Integer;
  partInfo:        TPartitionInformationEx;
  partitionNo:     Integer;
begin
  Result := '';

  if (TabControl1.TabIndex >= 0) then begin
    deviceIndicator := DWORD(TabControl1.Tabs.Objects[TabControl1.TabIndex]);
    useDevice       := (deviceIndicator and $FFFF);

    if ((deviceIndicator and HIWORD_CDROM) = HIWORD_CDROM) then begin
      // Device name can be found in FRemovableDevices
      Result := FRemovableDevices[useDevice];
    end else
    if ((deviceIndicator and HIWORD_DISK) = HIWORD_DISK) then begin
      partitionNo := NO_PARTITION;
      if SDUDiskPartitionsPanel1.DriveLayoutInformationValid then begin
        if ckEntireDisk.Checked then begin
          partitionNo := 0;
        end else
        if (SDUDiskPartitionsPanel1.Selected > NO_PARTITION) then begin
          partInfo := SDUDiskPartitionsPanel1.PartitionInfo[SDUDiskPartitionsPanel1.Selected];
          // Sanity...
          if (partInfo.PartitionNumber <> 0) then begin
            partitionNo := partInfo.PartitionNumber;
          end;
        end;
      end;

      if (partitionNo > NO_PARTITION) then
        Result := SDUDeviceNameForPartition(useDevice, partitionNo);

    end;
  end;

end;

procedure TfmeSelectPartition.UpdateErrorWarning();
var
  msg:             String;
  showIconWarning: Boolean;
  showIconError:   Boolean;
  useIconidx:      Integer;
  tmpImg:          TIcon;
//  imgLabelDiff:    Integer;
begin
//  imgLabelDiff         := lblErrorWarning.left - (imgErrorWarning.left + imgErrorWarning.Width);
//  imgErrorWarning.left := 0;
//  lblErrorWarning.left := 0;

  SelectionErrorWarning(msg, showIconWarning, showIconError);

  lblErrorWarning.Caption := msg;
  useIconidx              := ICON_NONE;
  if showIconError then
    useIconidx := ICON_ERROR
  else
  if showIconWarning then
    useIconidx := ICON_WARNING;


  tmpImg := TIcon.Create();
  try
    ilErrorWarning.GetIcon(useIconidx, tmpImg);
    imgErrorWarning.Picture.Assign(tmpImg);
    imgErrorWarning.Refresh();
  finally
    tmpImg.Free();
  end;

//  lblErrorWarning.left := imgErrorWarning.left + imgErrorWarning.Width + imgLabelDiff;
//  CenterErrorWarning();

end;
//
//procedure TfmeSelectPartition.CenterErrorWarning();
//var
//  ctrlArr: TControlArray;
//begin
//  // Center the icon and message
//  SetLength(ctrlArr, 2);
//  ctrlArr[0] := imgErrorWarning;
//  ctrlArr[1] := lblErrorWarning;
//  SDUCenterControl(ctrlArr, ccHorizontal);
//end;

procedure TfmeSelectPartition.NotifyChanged(Sender: TObject);
begin
  UpdateErrorWarning();
  actProperties.Enabled := (ckEntireDisk.Checked and
    (SDUDiskPartitionsPanel1.DiskNumber >= 0)) or (not (ckEntireDisk.Checked) and
    // 0 is the entire disk, partitions start at 1
    (SDUDiskPartitionsPanel1.Selected >= 0));

  if Assigned(FOnChange) then begin
    FOnChange(self);
  end;
end;

procedure TfmeSelectPartition.SelectionErrorWarning(var msg: String;
  var isWarning: Boolean; var isError: Boolean);
var
  selectedDriveLetters: String;
  bootableFlag:         Boolean;
  badPartitionNumber:   Boolean;
  IsGptDisc:            Boolean;

  // Returns TRUE if the special folder path identified by "clsid" is stored on
  // one of the drives listed in "checkDriveLetters". Otherwise returns FALSE
  function SystemUsesDrive(clsid: Integer; checkDriveLetters: String): Boolean;
  var
    path:      String;
    pathDrive: Char;
  begin
    Result := False;

    path := SDUGetSpecialFolderPath(clsid);
    if (length(path) > 1) then begin
      pathDrive := path[1];
      Result    := (Pos(pathDrive, checkDriveLetters) > 0);
    end;
  end;

  procedure UpdateFlagsForPartition(partno: Integer);
  var
    partInfo: TPartitionInformationEx;
  begin
    selectedDriveLetters := selectedDriveLetters +
      SDUDiskPartitionsPanel1.DriveLetter[partno];
    partInfo             := SDUDiskPartitionsPanel1.PartitionInfo[partno];
    if partInfo.PartitionStyle = PARTITION_STYLE_MBR then
      bootableFlag := (bootableFlag or partInfo.mbr.BootIndicator)
    else

    if partInfo.PartitionStyle = PARTITION_STYLE_GPT then begin
      // no 'bootable' fag for gpt - but 'required' flag
      bootableFlag :=
        bootableFlag or ((partInfo.gpt.Attributes and GPT_ATTRIBUTE_PLATFORM_REQUIRED) > 0);
      IsGptDisc    := True;
    end;

    badPartitionNumber := (partInfo.PartitionNumber = 0);
  end;

var
  i: Integer;

begin
  msg       := '';
  isWarning := False;
  isError   := False;
  IsGptDisc := False;

  if (SDUDiskPartitionsPanel1.DiskNumber >= 0) then begin
    if not (SDUDiskPartitionsPanel1.DriveLayoutInformationValid) then begin
      msg     := Format(UNABLE_TO_GET_DISK_LAYOUT,     [SDUDiskPartitionsPanel1.DiskNumber]);
      isError := True;
    end else begin
      selectedDriveLetters := '';
      bootableFlag         := False;
      badPartitionNumber   := False;
      if ckEntireDisk.Checked then begin
        for i := 0 to (SDUDiskPartitionsPanel1.Count - 1) do
          UpdateFlagsForPartition(i);
        badPartitionNumber := False;
        // badPartitionNumber never set to TRUE here; entire drive being used
      end else begin

        if (SDUDiskPartitionsPanel1.Selected >= 0) then
          UpdateFlagsForPartition(SDUDiskPartitionsPanel1.Selected);
      end;

      if badPartitionNumber then begin
        // This is *really* fatal - a partition with no partition number?!
        // Allowing the user to continue from here could result in the user
        // inadvertently overwriting their entire drive, not just a single
        // partition!
        msg     := RS_PART_ERROR_NO_PARTITION_NO;
        isError := True;
      end else begin
        if (SystemUsesDrive(SDU_CSIDL_WINDOWS, selectedDriveLetters) or
          SystemUsesDrive(SDU_CSIDL_SYSTEM, selectedDriveLetters)) then begin
          msg       := RS_PART_ERROR_SYSTEM_PARTITION;
          // Note: This is a WARNING and *NOT* an error, as user can have a hidden
          //       volume stored on this partition
          isWarning := True;
        end else begin
          if SystemUsesDrive(SDU_CSIDL_PROGRAM_FILES, selectedDriveLetters) then begin
            msg       := RS_PART_ERROR_PROG_FILES_PARTITION;
            // Note: This is a WARNING and *NOT* an error, as user can have a hidden
            //       volume stored on this partition
            isWarning := True;
          end else begin
            if bootableFlag then begin
              // Warn user; this could be something like a bootable Linux partition
              msg       := RS_PART_WARN_BOOTABLE;
              isWarning := True;
            end;
          end;
        end;
      end;
    end;
  end;

end;

procedure TfmeSelectPartition.EnableDisableControls();
begin
  SDUEnableControl(
    ckEntireDisk,
    (not (IsCDROMTabSelected()) and SDUDiskPartitionsPanel1.DriveLayoutInformationValid)
    );

end;

procedure TfmeSelectPartition.FrameResize(Sender: TObject);
begin
//  CenterErrorWarning();
end;

function TfmeSelectPartition.IsCDROMTabSelected(): Boolean;
var
  deviceIndicator: DWORD;
begin
  Result := False;

  if (TabControl1.TabIndex >= 0) then begin
    deviceIndicator := DWORD(TabControl1.Tabs.Objects[TabControl1.TabIndex]);
    Result          := ((deviceIndicator and HIWORD_CDROM) = HIWORD_CDROM);
  end;

end;

procedure TfmeSelectPartition.SetAllowCDROM(allow: Boolean);
begin
  FAllowCDROM := allow;

  ckShowCDROM.Visible := allow;
  if not (allow) then
    ckShowCDROM.Checked := False;
end;

function TfmeSelectPartition.SelectedSize(): ULONGLONG;
var
  partInfo:     TPartitionInformationEx;
  diskGeometry: TSDUDiskGeometry;
begin
  Result := 0;

  if (SDUDiskPartitionsPanel1.DiskNumber > NO_DISK) then begin
    if ckEntireDisk.Checked then begin
      // Return entire disk size
      if SDUGetDiskGeometry(SDUDiskPartitionsPanel1.DiskNumber, diskGeometry) then begin
        Result := diskGeometry.Cylinders.QuadPart * diskGeometry.TracksPerCylinder *
          diskGeometry.SectorsPerTrack * diskGeometry.BytesPerSector;
      end;

    end else begin
      // Return partition size
      if (SDUDiskPartitionsPanel1.Selected >= 0) then begin
        partInfo := SDUDiskPartitionsPanel1.PartitionInfo[SDUDiskPartitionsPanel1.Selected];
        Result   := partInfo.PartitionLength;
      end;
    end;
  end;

end;

function TfmeSelectPartition.GetSyntheticDriveLayout(): Boolean;
begin
  Result := SDUDiskPartitionsPanel1.SyntheticDriveLayout;
end;

procedure TfmeSelectPartition.actPropertiesExecute(Sender: TObject);
var
  dlgPartition: TSDUPartitionPropertiesDialog;
  dlgDisk:      TSDUDiskPropertiesDialog;
begin
  if ckEntireDisk.Checked then begin
    if (SDUDiskPartitionsPanel1.DiskNumber >= 0) then begin
      dlgDisk := TSDUDiskPropertiesDialog.Create(nil);
      try
        dlgDisk.fDiskNumber := SDUDiskPartitionsPanel1.DiskNumber;

        dlgDisk.ShowModal();
      finally
        dlgDisk.Free();
      end;
    end;

  end else begin
    // 0 is the entire disk, partitions start at 1
    if (SDUDiskPartitionsPanel1.Selected >= 0) then begin
      dlgPartition := TSDUPartitionPropertiesDialog.Create(nil);
      try
        dlgPartition.fPartitionInfo  :=
          SDUDiskPartitionsPanel1.PartitionInfo[SDUDiskPartitionsPanel1.Selected];
        dlgPartition.fMountedAsDrive :=
          SDUDiskPartitionsPanel1.DriveLetter[SDUDiskPartitionsPanel1.Selected];

        dlgPartition.ShowModal();
      finally
        dlgPartition.Free();
      end;
    end;

  end;

end;

end.
