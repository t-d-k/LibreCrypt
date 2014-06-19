unit SDUGraphics;
// Description: Sarah Dean's Graphics Utils
// By Sarah Dean
// Email: sdean12@sdean12.org
// WWW:   http://www.SDean12.org/
//
// -----------------------------------------------------------------------------
//


interface

uses
  forms, controls, stdctrls,
  ExtCtrls,
  Windows, // Required for TWIN32FindData in ConvertSFNPartToLFN, and THandle
  classes,
  comctrls, // Required in SDUEnableControl to enable/disable TRichedit controls
  dialogs, // Required for SDUOpenSaveDialogSetup
  graphics, // Required for TFont
  ShlObj,  // Required for SHChangeNotify and SHGetSpecialFolderLocation
  ActiveX, // Required for IMalloc
  Buttons, // Required for TBitBtn
  ActnList,  // Required for TAction
  SysUtils;

const
  // "Shield" icon ID for Windows Vista
  UAC_IDI_SHIELD = 32518;

  BCM_FIRST      = $1600;      // Button control messages
  BCM_SETSHIELD  = (BCM_FIRST + $000C);

  DLL_SHELL32 = 'shell32.dll';
  DLL_SHELL32_DEFAULT_FILE     =    0;
  DLL_SHELL32_FOLDER_CLOSED    =   -4;
  DLL_SHELL32_FOLDER_OPEN      =   -5;
  DLL_SHELL32_HDD              =   -9;
  DLL_SHELL32_REMOVABLE        =  -10;
  DLL_SHELL32_MULTIPLE_FILES   = -133;
  DLL_SHELL32_MAGNIFYING_GLASS = -323;
  DLL_SHELL32_GO_ICON          = -290;

  // WARNING: This list probably isn't exhaustive - later versions of Windows
  //          may have more!
  // This list appears to be more complete, but doesn't use -ve numbers?!
  SI_UNKNOWN                    = 00;  // Unknown File Type
  SI_DEF_DOCUMENT               = 01;  // Default document
  SI_DEF_APPLICATION            = 02;  // Default application
  SI_FOLDER_CLOSED              = 03;  // Closed folder
  SI_FOLDER_OPEN                = 04;  // Open folder
  SI_FLOPPY_514                 = 05;  // 5 1/4 floppy
  SI_FLOPPY_35                  = 06;  // 3 1/2 floppy
  SI_REMOVABLE                  = 07;  // Removable drive
  SI_HDD                        = 08;  // Hard disk drive
  SI_NETWORKDRIVE               = 09;  // Network drive
  SI_NETWORKDRIVE_DISCONNECTED  = 10;  // Network drive offline
  SI_CDROM                      = 11;  // CDROM drive
  SI_RAMDISK                    = 12;  // RAM disk
  SI_NETWORK                    = 13;  // Entire network
  // ???                           = 14;  // Network Pipes
  SI_MYCOMPUTER                 = 15;  // My Computer
  SI_PRINTMANAGER               = 16;  // Printer Manager
  SI_NETWORK_NEIGHBORHOOD       = 17;  // Network Neighborhood
  SI_NETWORK_WORKGROUP          = 18;  // Network Workgroup
  SI_STARTMENU_PROGRAMS         = 19;  // Start Menu Programs
  SI_STARTMENU_DOCUMENTS        = 20;  // Start Menu Documents
  SI_STARTMENU_SETTINGS         = 21;  // Start Menu Settings
  SI_STARTMENU_FIND             = 22;  // Start Menu Find
  SI_STARTMENU_HELP             = 23;  // Start Menu Help
  SI_STARTMENU_RUN              = 24;  // Start Menu Run
  SI_STARTMENU_SUSPEND          = 25;  // Start Menu Suspend
  SI_STARTMENU_DOCKING          = 26;  // Start Menu Docking
  SI_STARTMENU_SHUTDOWN         = 27;  // Start Menu Shutdown
  SI_SHARE                      = 28;  // Sharing overlay (hand)
  SI_SHORTCUT                   = 29;  // Shortcut overlay (small arrow)
  SI_PRINTER_DEFAULT            = 30;  // Default printer overlay (small tick)
  SI_RECYCLEBIN_EMPTY           = 31;  // Recycle bin empty
  SI_RECYCLEBIN_FULL            = 32;  // Recycle bin full
  SI_DUN                        = 33;  // Dial-up Network Folder
  SI_DESKTOP                    = 34;  // Desktop
  SI_CONTROLPANEL               = 35;  // Control Panel
  SI_PROGRAMGROUPS              = 36;  // Program Group
  SI_PRINTER                    = 37;  // Printer
  SI_FONT                       = 38;  // Font Folder
  SI_TASKBAR                    = 39;  // Taskbar
  SI_AUDIO_CD                   = 40;  // Audio CD
  // ???                           = 41;  // ???
  // ???                           = 42;  // ???
  SI_FAVORITES                  = 43;  // IE favorites
  SI_LOGOFF                     = 44;  // Start Menu Logoff
  // ???                           = 45;  // ???
  // ???                           = 46;  // ???
  SI_LOCK                       = 47;  // Lock
  SI_HIBERNATE                  = 48;  // Hibernate


//  49;  // ?????
//  50;  // ?????
//  51;  // ?????
//  52;  // ?????
//  53;  // Drive with red question mark
//  54;  // Multiple blank documents
//  55;  // Search files icon (document with magnifying glass)
//  56;  // Search computer icon (PC with magnifying glass)
//  57;  // Control panel icon
//  58;  // Printer
//  59;  // Print document
//  60;  // Network printer
//  61;  // Printer to file
//  62;  // Recycle bin with document
//  63;  // Recycle bin with folder
//  64;  // Recycle bin with document and folder
//  65;  // File to file???
//  66;  // Old move icon?
//  67;  // Rename?
//  68;  // ?????
//  69;  // Settings file (text file)
//  70;  // Text file
//  71;  // Application
//  72;  // Settings file (binary file)
//  73;  // Font file
//  73;  // Truetype font file
//  75;  // Adobe(?) font file
//  76;  // Run prompt
//  77;  // Shred file/folders
//  78;  // Save to HDD
  DLL_SHELL32_SCANDISK = 79;  // Scandisk
//  80;  // Windows Write file


  // AVI animations...
  DLL_SHELL32_AVI_SEARCHING                   = 150;  //Searching torch
  DLL_SHELL32_AVI_FIND_FILE                   = 151;  //Find file
  DLL_SHELL32_AVI_FIND_COMPUTER               = 152;  //Find computer
  // 153 - 159 are unused
  DLL_SHELL32_AVI_MOVE_FILES                  = 160;  //Move files
  DLL_SHELL32_AVI_COPY_FILE                   = 161;  //Copy file
  DLL_SHELL32_AVI_DELETE_TO_RECYCLE_BIN       = 162;  //Delete file to recycle bin
  DLL_SHELL32_AVI_EMPTY_RECYCLE_BIN           = 163;  //Empty recycle bin
  DLL_SHELL32_AVI_DELETE_FILE                 = 164;  //Delete file
  DLL_SHELL32_AVI_CHECK_FILES                 = 165;  //Check files???
  DLL_SHELL32_AVI_SEARCH_INTERNET             = 166;  //Search internet???
  DLL_SHELL32_AVI_OLD_MOVE_FILES              = 167;  //Move files (old animation)
  DLL_SHELL32_AVI_OLD_COPY_FILE               = 168;  //Copy file (old animation)
  DLL_SHELL32_AVI_OLD_DELETE_FILE             = 169;  //Delete file (old animation)
  DLL_SHELL32_AVI_OLD_SAVE_FILE_FROM_INTERNET = 170;  //Save file from internet (old animation)


  DLL_SCARDDLG = 'scarddlg.dll';
  DLL_SCARDDLG_RES_SLOT_NO_TOKEN   = -142;
  DLL_SCARDDLG_RES_SLOT_WITH_TOKEN = -143;

type
  TImageFlip = (ifNone, ifVertical, ifHorizontal, ifBoth);
  TImageRotate = (rot0, rot90, rot180, rot270);


// Convert from TColor to it's RGB components 
procedure SDUColorToRGB(color: TColor; var Red, Green, Blue: integer);
// Convert RGB values to TColor 
function SDURGBToColor(Red, Green, Blue: integer): TColor;
// "Ghost" the "srcIcon", setting "destIcon" to a "ghosted" version
// normalIcon - Set to TRUE to process as 32x32 icon, FALSE to process as
//              16x16 icon
procedure SDUMakeIconGhosted(srcIcon: TIcon; destIcon: TIcon; normalIcon: boolean);
// Greyscale the specified image
procedure SDUImageGrayscale(image: TBitmap);
// Flip image horizontally/vertically/both
// IMPORTANT: Currently only supports images with .PixelFormat set to one of:
// pf1bit, pf4bit, pf8bit or pf24bit
procedure SDUImageFlip(image: TBitmap; invert: TImageFlip); overload;
procedure SDUImageFlip(image: TIcon; invert: TImageFlip); overload;
// Rotate image *clockwise* by specified amount
procedure SDUImageRotate(image: TBitmap; rotate: TImageRotate); overload;
procedure SDUImageRotate(image: TIcon; rotate: TImageRotate); overload;
// Set bitmap to be mask of icon
procedure SDUCreateMaskFromIcon(
  Icon: TIcon;
  IconWidth: integer;
  IconHeight: integer;
  var MaskBitmap: TBitmap
);
// Set all pixels in image which are masked off (i.e. not in the mask) to black
// i.e. All white pixels in the mask will have their corresponding pixel in the
// image set to black
procedure SDUSetMaskedOffToBlack(
  Bitmap: TBitmap;
  MaskBitmap: TBitmap
);
// Convert bitmap to icon
// Note: Must supply "Icon"; this function doens't create one
function SDUConvertBitmapToIcon(
  Bitmap: TBitmap;
  TransparentColor: TColor;
  Icon: TIcon
): boolean; overload;
function SDUConvertBitmapToIcon(
  Bitmap: TBitmap;
  MaskBitmap: TBitmap;
  Icon: TIcon
): boolean; overload;
// Convert icon to bitmap
// Sets Bitmap and MaskBitmap to be the bitmap and mask of the icon passed in
procedure SDUConvertIconToBitmap(
  Icon: TIcon;
  Bitmap: TBitmap;
  MaskBitmap: TBitmap
); overload;
procedure SDUConvertIconToBitmap(
  Icon: TIcon;
  IconWidth: integer;
  IconHeight: integer;
  Bitmap: TBitmap;
  MaskBitmap: TBitmap
); overload;
// Takes a 16x16 icon (misreported as 32x32 by Delphi's TIcon), and return
// 16x16 bitmap and mask
procedure SDUConvertIconToBitmap_16x16(
  Icon: TIcon;
  Bitmap: TBitmap;
  MaskBitmap: TBitmap
);
// Overlay "OverlayIcon" on top of "MainIcon"
// Defaults to positioning the overlay icon on the bottom right of the main
// icon
procedure SDUOverlayIcon(
                         MainIcon: TIcon;
                         OverlayIcon: TIcon
                        ); overload;
procedure SDUOverlayIcon(
                         MainIcon: TIcon;
                         OverlayIcon: TIcon;
                         OverlayWidth: integer;
                         OverlayHeight: integer
                        ); overload;
procedure SDUOverlayIcon(
                         MainIcon: TIcon;
                         OverlayIcon: TIcon;
                         OverlayPosX: integer;
                         OverlayPosY: integer;
                         OverlayWidth: integer;
                         OverlayHeight: integer
                        ); overload;
// Load icons from file
function SDULoadDLLIcon(dllFilename: string; getSmallIcon: boolean; iconIdx: integer; icon: TIcon): boolean;
function SDULoadDLLIconToList(dllFilename: string; getSmallIcon: boolean; iconIdx: integer; imgList: TImageList): integer;
function SDULoadAllDLLIconsToList(dllFilename: string; getSmallIcons: boolean; imgList: TImageList): integer;
// Remova a Windows Vista "Shield" icon to a TButton
procedure SDUClearUACShieldIcon(button: TButton);
// Add a Windows Vista "Shield" icon to a TButton
procedure SDUSetUACShieldIcon(button: TButton); overload;
// Add a Windows Vista "Shield" icon to a TBitBtn (this is hacky)
procedure SDUSetUACShieldIcon(button: TBitBtn); overload;
// Draw a piechart on the specified canvas within specified bounds
procedure SDUSimplePieChart(
                            ACanvas: TCanvas;
                            Bounds: TRect;
                            Percent: integer;
                            ColourFg: TColor = clRed;
                            ColorBg: TColor = clGreen
                           );


implementation

uses
  ShellAPI, // Required for SDUShowFileProperties
{$IFDEF MSWINDOWS}
{$WARN UNIT_PLATFORM OFF}  // Useless warning about platform - we're already
                           // protecting against that!
  FileCtrl, // Required for TDriveType
{$WARN UNIT_PLATFORM ON}
{$ENDIF}
  registry,
  SDUProgressDlg,
  SDUDialogs,
  SDUi18n,
  Spin64,  // Required for TSpinEdit64
  Math,  // Required for Power(...)
  SDUWindows64,
  SDUSpin64Units,
  SDUGeneral;


// ----------------------------------------------------------------------------
// Draw a piechart on the specified canvas within specified bounds
procedure SDUSimplePieChart(
                         ACanvas: TCanvas;
                         Bounds: TRect;
                         Percent: integer;
                         ColourFg: TColor = clRed;
                         ColorBg: TColor = clGreen
                        );
var
  centerVert, certHoriz: integer;
  Th: double;
  a, o: double;
begin
  // Draw entire "pie"
  if (Percent >= 100) then
    begin
    ACanvas.brush.color := ColourFg;
    end
  else
    begin
    ACanvas.brush.color := ColorBg;
    end;
    
  ACanvas.ellipse(Bounds);
  

  // We only draw a slice if there's a slice to be drawn (e.g. it's not zero,
  // or the entire pie)
  if (
      (Percent > 0) and
      (Percent < 100)
     ) then
    begin
    // Calculate slice
    Th := (Percent/100) * (2 * pi);  // Convert percentage into radians

    a := sin(Th) * 100;
    o := cos(Th) * 100;

    centerVert := Bounds.top + ((Bounds.bottom-Bounds.top) div 2);
    certHoriz  := Bounds.left + ((Bounds.right-Bounds.left) div 2);


    // Draw slice
    ACanvas.brush.color := ColourFg;
    ACanvas.pie(
                // Bounds of entire pie
                Bounds.left,
                Bounds.top,
                Bounds.right,
                Bounds.bottom,

                // X, Y of first line from center
                certHoriz + round(a),
                centerVert + -round(o), // We subtract "o" as co-ordinate Y=0
                                        // it at the top, Y=100 is at the bottom

                // X, Y of top center of the pie
                Bounds.left + ((Bounds.right-Bounds.left) div 2), Bounds.top,
              );
    end;

end;


// ----------------------------------------------------------------------------
// Add a Windows Vista "Shield" icon to a TButton
procedure SDUSetUACShieldIcon(button: TButton);
begin
  if SDUOSVistaOrLater() then
    begin
    SendMessage(button.handle, BCM_SETSHIELD, 0, 1);
    end;
end;

// ----------------------------------------------------------------------------
// Remova a Windows Vista "Shield" icon to a TButton
procedure SDUClearUACShieldIcon(button: TButton);
begin
  if SDUOSVistaOrLater() then
    begin
    SendMessage(button.handle, BCM_SETSHIELD, 0, 0);
    end;
end;

// ----------------------------------------------------------------------------
// Add a Windows Vista "Shield" icon to a TBitBtn
// Returns TRUE if shield added successfully, FALSE if not (or if not needed;
// e.g. pre Windows Vista OS)
procedure SDUSetUACShieldIcon(button: TBitBtn);
var
 iconAsBitmap: TBitmap;
 resizedBitmap: TBitmap;
 iconHandle: HICON;
 anIcon: TIcon;
 tmpRect: TRect;
// retval: boolean;
begin
//  retval := FALSE;

  if SDUOSVistaOrLater() then
    begin
    // For testing purposes, use the executable's icon...
//  iconHandle := ExtractIcon(HInstance, PChar(ParamStr(0)), 0);

    iconHandle := LoadImage(
                            0,
                            PChar(UAC_IDI_SHIELD),
                            IMAGE_ICON,
                            16,
                            16,
                            (LR_CREATEDIBSECTION or LR_SHARED)
//                            (LR_CREATEDIBSECTION or LR_LOADTRANSPARENT or LR_SHARED)
                           );

    if (iconHandle <> 0) then
      begin
      anIcon:= TIcon.Create();
      iconAsBitmap:= TBitmap.Create();
      resizedBitmap:= TBitmap.Create();
      try
        // Setup out TIcon...
        anIcon.ReleaseHandle();
        iconAsBitmap.ReleaseHandle();
        anIcon.handle := iconHandle;

        // Convert to bitmap...
        iconAsBitmap.Width := anIcon.Width;
        iconAsBitmap.Height := anIcon.Height;
        tmpRect := Rect(0, 0, iconAsBitmap.Width, iconAsBitmap.Height);
        iconAsBitmap.Canvas.Brush.Color := clBtnFace;
        iconAsBitmap.Canvas.FillRect(tmpRect);
        iconAsBitmap.Canvas.CopyMode := cmSrcCopy;
        iconAsBitmap.Canvas.Draw(0, 0, anIcon);

        // Resize small enough...
        resizedBitmap.Width := 16;
        resizedBitmap.Height := 16;
        tmpRect := Rect(0, 0, resizedBitmap.Width, resizedBitmap.Height);
        resizedBitmap.Canvas.StretchDraw(tmpRect, iconAsBitmap);

        // Set graphic on button...
        button.Glyph.Assign(resizedBitmap);

        // Release unused handle...
        anIcon.ReleaseHandle();
        DestroyIcon(iconHandle);

//        retval := TRUE;
      finally
        resizedBitmap.Free();
        iconAsBitmap.Free();
        anIcon.Free();
      end;
    end;
  end;

//  Result := retval;
end;


// ----------------------------------------------------------------------------
// Load icon from file
// icon - This will be set to the loaded icon
// iconIdx - Indexed from 0; or -ve number for resource ID
function SDULoadDLLIcon(dllFilename: string; getSmallIcon: boolean; iconIdx: integer; icon: TIcon): boolean;
var
  iconHandleSmall: HICON;
  iconHandleLarge: HICON;
  iconHandleUse: HICON;
  iconHandleUnused: HICON;
  iconsExtracted: integer;
  retval: boolean;
begin
  retval:= FALSE;
  iconsExtracted:= ExtractIconEx(
                                 PChar(dllFilename),
                                 iconIdx, // 0 indexed
                                 iconHandleLarge,
                                 iconHandleSmall,
                                 1
                                );

  if (iconsExtracted > 0) then
    begin
    iconHandleUse := iconHandleLarge;
    iconHandleUnused := iconHandleSmall;
    if getSmallIcon then
      begin
      iconHandleUse := iconHandleSmall;
      iconHandleUnused := iconHandleLarge;
      end;

    if (iconHandleUse <> 0) then
      begin
      // Dump old handle
      icon.ReleaseHandle();

      icon.handle := iconHandleUse;

      // Unload unused small/large icon
      DestroyIcon(iconHandleUnused);

      retval := TRUE;
      end;

    end;

  Result := retval;
end;


// ----------------------------------------------------------------------------
// Load icon from file, and add it to the specified imagelist
// iconIdx - Indexed from 0; or -ve number for resource ID
// Returns the ImageIndex of the icon added to imgList, or -1 on error
function SDULoadDLLIconToList(dllFilename: string; getSmallIcon: boolean; iconIdx: integer; imgList: TImageList): integer;
var
  tmpIcon: TIcon;
  retval: integer;
begin
  retval:= -1;

  tmpIcon:= TIcon.Create();
  try
    if SDULoadDLLIcon(dllFilename, getSmallIcon, iconIdx, tmpIcon) then
      begin
      {
      if (ilToolbarIcons.Width < tmpIcon.Width) then
        begin
        ilToolbarIcons.Width := tmpIcon.Width;
        end;
      if (ilToolbarIcons.Height < tmpIcon.Height) then
        begin
        ilToolbarIcons.Height := tmpIcon.Height;
        end;
      }
      retval := imgList.AddIcon(tmpIcon);
      end;

  finally
    tmpIcon.Free();
  end;

  Result := retval;
end;


// ----------------------------------------------------------------------------
// Load all icons from file, adding them to the imagelist supplied
// Returns the number of icons added
function SDULoadAllDLLIconsToList(dllFilename: string; getSmallIcons: boolean; imgList: TImageList): integer;
var
  dllIconIdx: integer;
  lastImgListIdx: integer;
begin
  dllIconIdx := 0;
  repeat
    lastImgListIdx := SDULoadDLLIconToList(dllFilename, getSmallIcons, dllIconIdx, imgList);

    if (lastImgListIdx >= 0) then
      begin
      inc(dllIconIdx);
      end;

  until (lastImgListIdx < 0);

  Result := dllIconIdx;
end;


// ----------------------------------------------------------------------------
function SDUConvertBitmapToIcon(
  Bitmap: TBitmap;
  MaskBitmap: TBitmap;
  Icon: TIcon
): boolean;
var
  IconInfo: TIconInfo;
begin
  IconInfo.fIcon:= True;
  IconInfo.hbmColor:= Bitmap.Handle;
  IconInfo.hbmMask:= MaskBitmap.Handle;
//  IconInfo.hbmMask:= MaskBitmap.MaskHandle;
  // Icon handle is going to be replaced; dump existing handle
  Icon.ReleaseHandle();
  Icon.Handle:= CreateIconIndirect(IconInfo);
  Result := (Icon.Handle <> 0);
end;

// ----------------------------------------------------------------------------
function SDUConvertBitmapToIcon(
  Bitmap: TBitmap;
  TransparentColor: TColor;
  Icon: TIcon
): boolean;
var
  allOK: boolean;
  imgList: TImageList;
begin
  allOK := FALSE;

  imgList := TImageList.CreateSize(Bitmap.Width, Bitmap.Height);
  try
    imgList.AllocBy := 1;
    imgList.AddMasked(Bitmap, TransparentColor);
    try
      imgList.GetIcon(0, Icon);
      allOK := TRUE;
    except
      // Swallow the exception; we just return FALSE
    end;
  finally
    imgList.Free;
  end;

  Result := allOK;
end;


// ----------------------------------------------------------------------------
// clBlack = the masked area
procedure SDUCreateMaskFromIcon(
  Icon: TIcon;
  IconWidth: integer;
  IconHeight: integer;
  var MaskBitmap: TBitmap
);
var
  iconMaskCompare_1: TBitmap;
  iconMaskCompare_2: TBitmap;
  x: integer;
  y: integer;
begin
  iconMaskCompare_1:= TBitmap.Create();
  iconMaskCompare_2:= TBitmap.Create();
  try
    // Dubious - can't use width and height from the icon as it can be misreported(?!)
    // as 32x32 for a 16x16 icon
    iconMaskCompare_1.Width := IconWidth;
    iconMaskCompare_1.Height := IconHeight;
    iconMaskCompare_1.Monochrome := TRUE;
    iconMaskCompare_1.Transparent := TRUE;
    iconMaskCompare_1.TransparentColor := clBlack;
    iconMaskCompare_1.Canvas.Brush.Color := clBlack;
    iconMaskCompare_1.Canvas.FillRect(Rect(0, 0, iconMaskCompare_1.Width, iconMaskCompare_1.Height));
    DrawIcon(iconMaskCompare_1.Canvas.Handle, 0, 0, Icon.handle);

    iconMaskCompare_2.Width := IconWidth;
    iconMaskCompare_2.Height := IconHeight;
    iconMaskCompare_2.Monochrome := TRUE;
    iconMaskCompare_2.Transparent := TRUE;
    iconMaskCompare_2.TransparentColor := clWhite;
    iconMaskCompare_2.Canvas.Brush.Color := clWhite;
    iconMaskCompare_2.Canvas.FillRect(Rect(0, 0, iconMaskCompare_2.Width, iconMaskCompare_2.Height));
    DrawIcon(iconMaskCompare_2.Canvas.Handle, 0, 0, Icon.handle);

    MaskBitmap.Width := IconWidth;
    MaskBitmap.Height := IconHeight;
    MaskBitmap.PixelFormat := pf1bit;
    MaskBitmap.Monochrome := TRUE;
    MaskBitmap.Canvas.Brush.Color := clBlack;
    MaskBitmap.Canvas.FillRect(Rect(0, 0, MaskBitmap.Width, MaskBitmap.Height));
    // Mark all areas which were transparent as clWhite
    for x:=0 to (MaskBitmap.Width - 1) do
      begin
      for y:=0 to (MaskBitmap.Height - 1) do
        begin
        if (
            (iconMaskCompare_1.Canvas.Pixels[x, y] = clBlack) and
            (iconMaskCompare_2.Canvas.Pixels[x, y] = clWhite)
           ) then
          begin
          MaskBitmap.Canvas.Pixels[x, y] := clWhite;
          end;
        end;
      end;

  finally
    iconMaskCompare_2.Free();
    iconMaskCompare_1.Free();
  end;
end;


// ----------------------------------------------------------------------------
procedure SDUOverlayIcon(
                         MainIcon: TIcon;
                         OverlayIcon: TIcon
                        );
begin
  SDUOverlayIcon(MainIcon, OverlayIcon, OverlayIcon.width, OverlayIcon.height);
end;

// ----------------------------------------------------------------------------
procedure SDUOverlayIcon(
                         MainIcon: TIcon;
                         OverlayIcon: TIcon;
                         OverlayWidth: integer;
                         OverlayHeight: integer
                        );
begin
  // Align the overlay to the bottom right of the main icon
  SDUOverlayIcon(
                 MainIcon,
                 OverlayIcon,
                 (MainIcon.Width - OverlayWidth),
                 (MainIcon.Height - OverlayHeight),
                 OverlayWidth,
                 OverlayHeight
                );
end;


// ----------------------------------------------------------------------------
procedure SDUOverlayIcon(
                         MainIcon: TIcon;
                         OverlayIcon: TIcon;
                         OverlayPosX: integer;
                         OverlayPosY: integer;
                         OverlayWidth: integer;
                         OverlayHeight: integer
                        );
var
  maskMain: TBitmap;
  maskOverlay: TBitmap;
  combinedMask: TBitmap;
  combinedBitmap: TBitmap;
  x: integer;
  y: integer;
begin
  combinedBitmap:= TBitmap.Create();
  combinedMask:= TBitmap.Create();
  try
    // Create bitmap consisting of main icon, with overlay icon placed on top
    // of it
    SDUConvertIconToBitmap(
                           MainIcon,
                           MainIcon.Width,
                           MainIcon.Height,
                           combinedBitmap,
                           nil
                          );
    combinedBitmap.Canvas.Brush.Style := bsClear;
    // Set the pixel format on the combined image, otherwise the overlay icon
    // *may* simply overwrite the main icon's image completely *when the icon
    // is created*
    // Without this, the "combinedBitmap" is shown correctly when used on a
    // TImage control, but the icon created from the call to
    // SDUConvertBitmapToIcon(...) may only show the overlay icon alone,
    // without any of the main icon underneath it
    // (Seen when overlaying a normal icon with a Windows standard icon)
    combinedBitmap.pixelformat := pf24bit;
    DrawIcon(
             combinedBitmap.Canvas.Handle,
             OverlayPosX,
             OverlayPosY,
             OverlayIcon.handle
            );

            
    // Combine icon masks to generate a main icon + overlay icon mask
    maskMain:= TBitmap.Create();
    maskOverlay:= TBitmap.Create();
    try
      SDUCreateMaskFromIcon(MainIcon, MainIcon.Width, MainIcon.Width, maskMain);
      SDUCreateMaskFromIcon(OverlayIcon, OverlayWidth, OverlayHeight, maskOverlay);

      combinedMask.Width := maskMain.Width;
      combinedMask.Height := maskMain.Height;
      combinedMask.Pixelformat := pf1bit;
      combinedMask.Monochrome := TRUE;
      combinedMask.Canvas.Brush.Color := clWhite;
      combinedMask.Canvas.FillRect(Rect(0, 0, combinedMask.Width, combinedMask.Height));
      for x:=0 to (OverlayWidth - 1) do
        begin
        for y:=0 to (OverlayHeight - 1) do
          begin
          if (
              (maskMain.Canvas.Pixels[OverlayPosX+x, OverlayPosY+y] = clBlack) or
              (maskOverlay.Canvas.Pixels[x, y] = clBlack)
             ) then
            begin
            combinedMask.Canvas.Pixels[OverlayPosX+x, OverlayPosY+y] := clBlack;
            end
          else
            begin
            // Set masked off pixels in *image* to black, otherwise they are
            // inverted when displayed (e.g. red is shown as cyan)
            combinedBitmap.Canvas.Pixels[OverlayPosX+x, OverlayPosY+y] := clBlack;
            end;
          end;
        end;


    finally
      maskOverlay.Free();
      maskMain.Free();
    end;


    SDUConvertBitmapToIcon(
                           combinedBitmap,
                           combinedMask,
                           MainIcon
                          );

  finally
    combinedMask.Free();
    combinedBitmap.Free();
  end;

end;

// ----------------------------------------------------------------------------
procedure SDUConvertIconToBitmap(
  Icon: TIcon;
  Bitmap: TBitmap;
  MaskBitmap: TBitmap
);
begin
  SDUConvertIconToBitmap(
                         Icon,
                         Icon.Width,
                         Icon.Height,
                         Bitmap,
                         MaskBitmap
                        );
end;

// ----------------------------------------------------------------------------
procedure SDUConvertIconToBitmap(
  Icon: TIcon;
  IconWidth: integer;
  IconHeight: integer;
  Bitmap: TBitmap;
  MaskBitmap: TBitmap
);
begin
  if (MaskBitmap <> nil) then
    begin
    SDUCreateMaskFromIcon(Icon, IconWidth, IconHeight, MaskBitmap);
    end;

  Bitmap.Width := IconWidth;
  Bitmap.Height := IconHeight;
  Bitmap.Canvas.Draw(0, 0, Icon);
end;


// ----------------------------------------------------------------------------
procedure SDUConvertIconToBitmap_16x16(
  Icon: TIcon;
  Bitmap: TBitmap;
  MaskBitmap: TBitmap
);
var
  resizedBitmap: TBitmap;
  tmpRect: TRect;
begin
  SDUConvertIconToBitmap(
                         Icon,
                         32,  // Treat as 32x32 to produce mask which looks OK,
                         32,  // but scaled up to 32x32
                         Bitmap,
                         MaskBitmap
                        );

  resizedBitmap := tbitmap.create();

  Bitmap.Width := 15;
  Bitmap.Height := 15;

  // The *mask* - but *NOT* the bitmap must be *resized* to 16x16

  // Resize small enough...
  resizedBitmap.Width := 16;
  resizedBitmap.Height := 16;

  resizedBitmap.Pixelformat := pf1bit;
  resizedBitmap.Monochrome := TRUE;
  resizedBitmap.Canvas.Brush.Color := clWhite;

  tmpRect := Rect(0, 0, resizedBitmap.Width, resizedBitmap.Height);
  resizedBitmap.Canvas.StretchDraw(tmpRect, MaskBitmap);

  MaskBitmap.assign(resizedbitmap);

end;


// ----------------------------------------------------------------------------
procedure _SDUImageFlip_FlipScanlineBits(bytesPerScanLine: integer; inputScanLine: PByteArray; outputScanLine: PByteArray; bytesPerPixel: integer);
var
  x: integer;
  tmpByte: byte;
begin
  for x := 0 to (bytesPerScanLine - 1) do
    begin
    tmpByte := inputScanLine[((bytesPerScanLine - 1) - x)];
    if (bytesPerPixel = 2) then
      begin
      tmpByte := (tmpByte shr 4) + (tmpByte shl 4);
      end
    else if (bytesPerPixel = 8) then
      begin
      tmpByte := (
                  ((tmpByte and $80) shr 7) +
                  ((tmpByte and $40) shr 5) +
                  ((tmpByte and $20) shr 3) +
                  ((tmpByte and $10) shr 1) +
                  ((tmpByte and $08) shl 1) +
                  ((tmpByte and $04) shl 3) +
                  ((tmpByte and $02) shl 5) +
                  ((tmpByte and $01) shl 7)
                 );
      end;
      
    outputScanLine[x] := tmpByte;
    end;
end;

procedure _SDUImageFlip_FlipScanlineBytes(imgWidthInPixels: integer; bytesPerPixel: integer; inputScanLine: PByteArray; outputScanLine: PByteArray);
var
  x: integer;
  i: integer;
begin
  for x := 0 to (imgWidthInPixels - 1) do
    begin
    for i:=0 to (bytesPerPixel-1) do
      begin
      outputScanLine[(x*bytesPerPixel)+i] := inputScanLine[(((imgWidthInPixels - 1) - x) * bytesPerPixel) + i];
      end;
    end;
end;

// Flip image horizontally/vertically/both
procedure SDUImageFlip(image: TBitmap; invert: TImageFlip);
var
  y: integer;
  sLine: PByteArray;
  tmpSLine: TByteArray;
  bytesPerScanLine: integer;
begin
{
     - pfDevice	The bitmap is stored as a device-dependent bitmap.
DONE - pf1bit	The bitmap is a device-independent bitmap with one bit per pixel (black and white palette)
DONE - pf4bit	The bitmap is a device-independent bitmap that uses a 16-color palette.
DONE - pf8bit	The bitmap is a device-independent bitmap that uses a 256color palette.
     - pf15bit	The bitmap is a device-independent true-color bitmap that uses 15 bits per pixel (RGB compression).
     - pf16bit	The bitmap is a device-independent true-color bitmap that uses 16 bits per pixel (bitfield compression).
DONE - pf24bit	The bitmap is a device-independent true-color bitmap that uses 24 bits per pixel.
DONE - pf32bit	The bitmap is a device-independent true-color bitmap that uses 32 bits per pixel (RGB compression).
     - pfCustom	The bitmap uses some other format. TBitmap does not support pfCustom.
}
  bytesPerScanLine := 0;
  if (image.PixelFormat = pf1bit) then
    begin
    bytesPerScanLine := image.Width div 8;
    end
  else if (image.PixelFormat = pf4bit) then
    begin
    bytesPerScanLine := image.Width div 2;
    end
  else if (image.PixelFormat = pf8bit) then
    begin
    bytesPerScanLine := image.Width;
    end
  else if (image.PixelFormat = pf24bit) then
    begin
    bytesPerScanLine := image.Width * 3;
    end
  else if (image.PixelFormat = pf32bit) then
    begin
    bytesPerScanLine := image.Width * 4;
    end;

  if (
      (invert = ifVertical) or
      (invert = ifBoth)
     ) then
    begin
    // div 2 here because we swap two scanlines over each iteration of the
    // loop
    for y := 0 to ((image.height div 2) - 1) do
      begin
      CopyMemory(
                 @tmpSLine,
                 image.ScanLine[(image.height - y - 1)],
                 bytesPerScanLine
                );
      CopyMemory(
                 image.ScanLine[(image.height - y - 1)],
                 image.ScanLine[y],
                 bytesPerScanLine
                );
      CopyMemory(
                 image.ScanLine[y],
                 @tmpSLine,
                 bytesPerScanLine
                );
      end;
    end;

  if (
      (invert = ifHorizontal) or
      (invert = ifBoth)
     ) then
    begin
{
     - pfDevice	The bitmap is stored as a device-dependent bitmap.
DONE - pf1bit	The bitmap is a device-independent bitmap with one bit per pixel (black and white palette)
DONE - pf4bit	The bitmap is a device-independent bitmap that uses a 16-color palette.
DONE - pf8bit	The bitmap is a device-independent bitmap that uses a 256color palette.
     - pf15bit	The bitmap is a device-independent true-color bitmap that uses 15 bits per pixel (RGB compression).
     - pf16bit	The bitmap is a device-independent true-color bitmap that uses 16 bits per pixel (bitfield compression).
DONE - pf24bit	The bitmap is a device-independent true-color bitmap that uses 24 bits per pixel.
     - pf32bit	The bitmap is a device-independent true-color bitmap that uses 32 bits per pixel (RGB compression).
     - pfCustom	The bitmap uses some other format. TBitmap does not support pfCustom.
}

    for y := 0 to (image.height - 1) do
      begin
      sLine := image.ScanLine[y];
      CopyMemory(
                 @tmpSLine,
                 sLine,
                 bytesPerScanLine
                );

      if (image.PixelFormat = pf1bit) then
        begin
        _SDUImageFlip_FlipScanlineBits(bytesPerScanLine, @tmpSLine, sLine, 8);
        end
      else if (image.PixelFormat = pf4bit) then
        begin
        _SDUImageFlip_FlipScanlineBits(bytesPerScanLine, @tmpSLine, sLine, 2);
        end
      else if (image.PixelFormat = pf8bit) then
        begin
        _SDUImageFlip_FlipScanlineBytes(image.width, 1, @tmpSLine, sLine);
        end
      else if (image.PixelFormat = pf24bit) then
        begin
        _SDUImageFlip_FlipScanlineBytes(image.width, 3, @tmpSLine, sLine);
        end

      end;
    end;


end;

procedure SDUImageFlip(image: TIcon; invert: TImageFlip);
var
  Bitmap: TBitmap;
  MaskBitmap: TBitmap;
begin
  Bitmap:= TBitmap.Create();
  MaskBitmap:= TBitmap.Create();
  try
    Bitmap.pixelformat := pf24bit;
    SDUConvertIconToBitmap(image, Bitmap, MaskBitmap);

    // This is required...
    MaskBitmap.pixelformat := pf24bit;

    SDUImageFlip(Bitmap, invert);
    SDUImageFlip(MaskBitmap, invert);
    
    SDUSetMaskedOffToBlack(bitmap, maskbitmap);
    SDUConvertBitmapToIcon(Bitmap, MaskBitmap, image);
  finally
    MaskBitmap.Free();
    Bitmap.Free();
  end;

end;

// ----------------------------------------------------------------------------
procedure SDUImageRotate(image: TBitmap; rotate: TImageRotate);
var
  origImage: TBitmap;
  x: integer;
  y: integer;
  mappedX: integer;
  mappedY: integer;
begin
  // Short circuit if no changes
  if (rotate = rot0) then
    begin
    // Do nothing to image
    end
  else
    begin
    origImage:= TBitmap.Create();
    try
      origImage.Assign(image);

      if (
          (rotate = rot90) or
          (rotate = rot270)
         ) then
        begin
        image.Height := origImage.Width;
        image.Width := origImage.Height;
        end;

      for x := 0 to (origImage.Width - 1) do
        begin
        for y := 0 to (origImage.Height - 1) do
          begin
          mappedX := x;
          mappedY := y;
          case rotate of
            rot90:
              begin
              mappedX := (origImage.Height - y - 1);
              mappedY := x;
              end;

            rot180:
              begin
              mappedX := (origImage.Width - x - 1);
              mappedY := (origImage.Height - y - 1);
              end;

            rot270:
              begin
              mappedX := y;
              mappedY := (origImage.Width - x - 1);
              end;

          end;

          image.Canvas.Pixels[mappedX, mappedY] := origImage.Canvas.Pixels[X, Y];
          end;
        end;

    finally
      origImage.Free();
    end;
  end;

end;

procedure SDUImageRotate(image: TIcon; rotate: TImageRotate);
var
  Bitmap: TBitmap;
  MaskBitmap: TBitmap;
begin
  Bitmap:= TBitmap.Create();
  MaskBitmap:= TBitmap.Create();
  try
    Bitmap.pixelformat := pf24bit;
    SDUConvertIconToBitmap(image, Bitmap, MaskBitmap);

    // This is required...
    MaskBitmap.pixelformat := pf24bit;

    SDUImageRotate(Bitmap, rotate);
    SDUImageRotate(MaskBitmap, rotate);

    SDUSetMaskedOffToBlack(bitmap, maskbitmap);
    SDUConvertBitmapToIcon(Bitmap, MaskBitmap, image);
  finally
    MaskBitmap.Free();
    Bitmap.Free();
  end;

end;


// ----------------------------------------------------------------------------
procedure SDUImageGrayscale(image: TBitmap);
var
  x: integer;
  y: integer;
  lum: integer;
  col: TColor;
begin
  for x := 0 to (image.Width - 1) do
    begin
    for y := 0 to (image.Height - 1) do
      begin
      col := image.Canvas.Pixels[x, y];

      // Average RGB values in TColour to get lum 
      lum := (
              (
               ((col       ) and $FF) +  // Red
               ((col shr  8) and $FF) +  // Green
               ((col shr 16) and $FF)    // Blue
              ) div 3                    // / 3 to get average
             );

      // Convert average lum into gray TColor
      col := (
              (lum       ) + // Red
              (lum shl  8) + // Green
              (lum shl 16)   // Blue
             );

      image.Canvas.Pixels[x, y] := col;
      end;
    end;

end;

// ----------------------------------------------------------------------------
// Set all pixels in image which are masked off (i.e. not in the mask) to black
// i.e. All white pixels in the mask will have their corresponding pixel in the
// image set to black
procedure SDUSetMaskedOffToBlack(
  Bitmap: TBitmap;
  MaskBitmap: TBitmap
);
var
  x: integer;
  y: integer;
begin
  for x:=0 to (MaskBitmap.width - 1) do
    begin
    for y:=0 to (MaskBitmap.height - 1) do
      begin
      if (MaskBitmap.Canvas.Pixels[x, y] = clWhite) then
        begin
        Bitmap.Canvas.Pixels[x, y] := clBlack;
        end;
      end;
    end;
end;


// ----------------------------------------------------------------------------
procedure SDUMakeIconGhosted(srcIcon: TIcon; destIcon: TIcon; normalIcon: boolean);
var
  Bitmap: TBitmap;
  MaskBitmap: TBitmap;
  x: integer;
  y: integer;
  Red, Green, Blue: integer;
  multiplier: double;
  plus: integer;
begin
  Bitmap:= TBitmap.Create();
  MaskBitmap:= TBitmap.Create();
  try
    if normalIcon then
      begin
      SDUConvertIconToBitmap(
                                   srcIcon,
                                   Bitmap,
                                   MaskBitmap
                                  );
      end
      else
      begin
      SDUConvertIconToBitmap_16x16(
                                   srcIcon,
                                   Bitmap,
                                   MaskBitmap
                                  );
      end;

    MaskBitmap.Pixelformat := pf1bit;
    MaskBitmap.Monochrome := TRUE;
    MaskBitmap.Canvas.Brush.Color := clWhite;

    multiplier := 0.5;
    plus := 128;

    for y:=0 to (Bitmap.Height - 1) do
      begin
      for x:=0 to (Bitmap.Width - 1) do
        begin
        SDUColorToRGB(Bitmap.Canvas.Pixels[x, y], Red, Green, Blue);

        Red   := min($FF, trunc(Red   * multiplier));
        Green := min($FF, trunc(Green * multiplier));
        Blue  := min($FF, trunc(Blue  * multiplier));

        Red   := min($FF, trunc(Red   + plus));
        Green := min($FF, trunc(Green + plus));
        Blue  := min($FF, trunc(Blue  + plus));

        if (MaskBitmap.Canvas.Pixels[x, y] = clBlack) then
          begin
          Bitmap.Canvas.Pixels[x, y] := SDURGBToColor(Red, Green, Blue);
          end
        else
          begin
          Bitmap.Canvas.Pixels[x, y] := clBlack;
          end;

        end;
      end;

    SDUConvertBitmapToIcon(
                           Bitmap,
                           MaskBitmap,
                           destIcon
                          );

  finally
    Bitmap.Free();
    MaskBitmap.Free();
  end;

end;


// ----------------------------------------------------------------------------
procedure SDUColorToRGB(color: TColor; var Red, Green, Blue: integer);
begin
  Red   := ((color       ) and $FF);
  Green := ((color shr  8) and $FF);
  Blue  := ((color shr 16) and $FF);

end;

function SDURGBToColor(Red, Green, Blue: integer): TColor;
begin
  Result := (
              (Red         ) +
              (Green shl  8) +
              (Blue  shl 16)
             );

end;

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

END.


