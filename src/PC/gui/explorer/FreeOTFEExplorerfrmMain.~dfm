inherited frmFreeOTFEExplorerMain: TfrmFreeOTFEExplorerMain
  Left = 94
  Top = 282
  Width = 808
  Height = 572
  Caption = 'CAPTION SET AUTOMATICALLY'
  OnClose = FormClose
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited StatusBar_Status: TStatusBar
    Top = 495
    Width = 792
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
    OnDrawPanel = StatusBarDrawPanel
  end
  object pnlExplorer: TPanel [1]
    Left = 0
    Top = 59
    Width = 792
    Height = 417
    Align = alClient
    Caption = 'pnlExplorer'
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 217
      Top = 42
      Height = 374
    end
    object SDFilesystemListView1: TSDFilesystemListView
      Left = 220
      Top = 42
      Width = 571
      Height = 374
      Align = alClient
      Columns = <>
      DragMode = dmAutomatic
      FullDrag = True
      MultiSelect = True
      ReadOnly = True
      PopupMenu = pmListView
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = SDFilesystemListView1Change
      OnDblClick = SDFilesystemListView1DblClick
      OnEdited = SDFilesystemListView1Edited
      OnEndDrag = SDFilesystemBothViewEndDrag
      OnEnter = SDFilesystemListView1Enter
      OnDragOver = SDFilesystemListView1DragOver
      OnSelectItem = SDFilesystemListView1SelectItem
      OnStartDrag = SDFilesystemBothViewStartDrag
      SelectedColumn = 0
      FilesystemTreeView = SDFilesystemTreeView1
      HideKnownFileExtns = False
    end
    object SDFilesystemTreeView1: TSDFilesystemTreeView
      Left = 1
      Top = 42
      Width = 216
      Height = 374
      Align = alLeft
      DragMode = dmAutomatic
      HideSelection = False
      Indent = 19
      PopupMenu = pmTreeView
      ReadOnly = True
      ShowRoot = False
      TabOrder = 1
      OnChange = SDFilesystemTreeView1Change
      OnContextPopup = SDFilesystemTreeView1ContextPopup
      OnDragOver = SDFilesystemTreeView1DragOver
      OnEdited = SDFilesystemTreeView1Edited
      OnEndDrag = SDFilesystemBothViewEndDrag
      OnEnter = SDFilesystemTreeView1Enter
      OnStartDrag = SDFilesystemBothViewStartDrag
      FilesystemListView = SDFilesystemListView1
    end
    object pnlAddressBar: TPanel
      Left = 1
      Top = 1
      Width = 790
      Height = 41
      Align = alTop
      Caption = 'pnlAddressBar'
      TabOrder = 2
      DesignSize = (
        790
        41)
      object lblFolder: TLabel
        Left = 3
        Top = 11
        Width = 32
        Height = 13
        Caption = 'Fol&der:'
        FocusControl = edPath
      end
      object edPath: TEdit
        Left = 44
        Top = 7
        Width = 671
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edPathChange
        OnKeyPress = edPathKeyPress
      end
      object pbGo: TBitBtn
        Left = 726
        Top = 8
        Width = 56
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Go'
        TabOrder = 1
        OnClick = pbGoClick
      end
    end
  end
  object ToolbarExplorer: TToolBar [2]
    Left = 0
    Top = 26
    Width = 792
    Height = 26
    AutoSize = True
    Caption = 'ToolbarExplorer'
    Images = ilToolbarIcons_Small
    PopupMenu = pmToolbarMenu
    TabOrder = 4
    object tbbNavigateBack: TToolButton
      Left = 0
      Top = 2
      Action = actNavigateBack
      DropdownMenu = pmToolbarBack
      ImageIndex = 6
      Style = tbsDropDown
    end
    object tbbNavigateForward: TToolButton
      Left = 38
      Top = 2
      Action = actNavigateForward
      DropdownMenu = pmToolbarForward
      ImageIndex = 7
      Style = tbsDropDown
    end
    object tbbUp: TToolButton
      Left = 76
      Top = 2
      Action = actUpDir
      ImageIndex = 8
    end
    object ToolButton3: TToolButton
      Left = 99
      Top = 2
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object tbbExplorerBarFolders: TToolButton
      Left = 107
      Top = 2
      Action = actCheckExplorerBarFolders
      ImageIndex = 16
    end
    object ToolButton9: TToolButton
      Left = 130
      Top = 2
      Width = 8
      Caption = 'ToolButton9'
      ImageIndex = 17
      Style = tbsSeparator
    end
    object tbbMoveTo: TToolButton
      Left = 138
      Top = 2
      Action = actMoveTo
      ImageIndex = 9
    end
    object tbbCopyTo: TToolButton
      Left = 161
      Top = 2
      Action = actCopyTo
      ImageIndex = 10
    end
    object ToolButton1: TToolButton
      Left = 184
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object tbbStore: TToolButton
      Left = 192
      Top = 2
      Caption = '&Store'
      DropdownMenu = pmToolbarStore
      ImageIndex = 14
      Style = tbsDropDown
      OnMouseDown = tbbWithDropDownMouseDown
    end
    object tbbExtract: TToolButton
      Left = 230
      Top = 2
      Action = actExtract
    end
    object ToolButton6: TToolButton
      Left = 253
      Top = 2
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object tbbDelete: TToolButton
      Left = 261
      Top = 2
      Action = actDelete
    end
    object ToolButton4: TToolButton
      Left = 284
      Top = 2
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object tbbViews: TToolButton
      Left = 292
      Top = 2
      Caption = 'Views'
      DropdownMenu = pmToolbarViews
      ImageIndex = 12
      Style = tbsDropDown
      OnMouseDown = tbbWithDropDownMouseDown
    end
    object ToolButton8: TToolButton
      Left = 330
      Top = 2
      Width = 8
      Caption = 'ToolButton8'
      ImageIndex = 14
      Style = tbsSeparator
    end
    object tbbItemProperties: TToolButton
      Left = 338
      Top = 2
      Action = actItemProperties
    end
  end
  object ToolBarVolume: TToolBar [3]
    Left = 0
    Top = 0
    Width = 792
    Height = 26
    AutoSize = True
    Caption = 'ToolBarVolume'
    Images = ilToolbarIcons_Small
    PopupMenu = pmToolbarMenu
    TabOrder = 3
    object tbbNew: TToolButton
      Left = 0
      Top = 2
      Action = actFreeOTFENew
    end
    object ToolButton2: TToolButton
      Left = 23
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      Style = tbsSeparator
    end
    object tbbMountFile: TToolButton
      Left = 31
      Top = 2
      Action = actFreeOTFEMountFile
    end
    object ToolButton5: TToolButton
      Left = 54
      Top = 2
      Width = 8
      Caption = 'ToolButton5'
      Style = tbsSeparator
    end
    object tbbDismount: TToolButton
      Left = 62
      Top = 2
      Action = actDismount
    end
  end
  object pnlTopSpacing: TPanel [4]
    Left = 0
    Top = 52
    Width = 792
    Height = 7
    Align = alTop
    Caption = 'pnlTopSpacing'
    TabOrder = 1
  end
  inherited StatusBar_Hint: TStatusBar
    Top = 476
    Width = 792
  end
  inherited mmMain: TMainMenu
    inherited File1: TMenuItem
      object Plaintextimage1: TMenuItem [5]
        Caption = 'Plaintext &image'
        Hint = 'Contains commands for working with plaintext volumes.'
        object New1: TMenuItem
          Action = actPlaintextNew
        end
        object Mountplaintextimage1: TMenuItem
          Action = actPlaintextMountFile
        end
        object miPlaintextDismount: TMenuItem
          Action = actDismount
        end
      end
    end
    object Edit2: TMenuItem [1]
      Caption = '&Edit'
      object Cut1: TMenuItem
        Action = actCut
      end
      object Copy1: TMenuItem
        Action = actCopy
      end
      object Paste1: TMenuItem
        Action = actPaste
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object CopyToFolder1: TMenuItem
        Action = actCopyTo
      end
      object MoveToFolder1: TMenuItem
        Action = actMoveTo
      end
      object N20: TMenuItem
        Caption = '-'
      end
      object mnuMainStore: TMenuItem
        Caption = '&Store'
        Hint = 
          'Contains commands for copying files and folders to the mounted v' +
          'olume.'
        ImageIndex = 14
        object File2: TMenuItem
          Action = actStoreFile
        end
        object Folder1: TMenuItem
          Action = actStoreDir
        end
      end
      object mnuMainExtract: TMenuItem
        Action = actExtract
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object Selectall1: TMenuItem
        Action = actSelectAll
      end
      object InvertSelection1: TMenuItem
        Action = actInvertSelection
      end
    end
    inherited View1: TMenuItem
      object oolbars1: TMenuItem [0]
        Caption = '&Toolbars'
        Hint = 'Shows or hides toolbars.'
        object Volume1: TMenuItem
          Action = actCheckToolbarVolume
          AutoCheck = True
        end
        object StandardButtons1: TMenuItem
          Action = actCheckToolbarExplorer
          AutoCheck = True
        end
        object Addressbar1: TMenuItem
          Action = actCheckAddressBar
          AutoCheck = True
        end
      end
      object Statusbar2: TMenuItem [1]
        Action = actCheckStatusBar
        AutoCheck = True
      end
      object ExplorerBar1: TMenuItem [2]
        Caption = '&Explorer Bar'
        Hint = 'Shows or hides an Explorer bar.'
        object Folders1: TMenuItem
          Action = actCheckExplorerBarFolders
          AutoCheck = True
        end
      end
      object N2: TMenuItem [3]
        Caption = '-'
      end
      object Icons1: TMenuItem [4]
        Action = actListStyleIcons
        AutoCheck = True
      end
      object SmallIcons1: TMenuItem [5]
        Action = actListStyleSmallIcons
        AutoCheck = True
      end
      object List1: TMenuItem [6]
        Action = actListStyleList
        AutoCheck = True
      end
      object Details1: TMenuItem [7]
        Action = actListStyleDetails
        AutoCheck = True
      end
      object N17: TMenuItem [8]
        Caption = '-'
      end
      object Options1: TMenuItem [9]
        Action = actOptions
      end
      object N1: TMenuItem [10]
        Caption = '-'
      end
    end
    inherited miTools: TMenuItem
      object Showbootsectordetails1: TMenuItem [0]
        Action = actShowBootSector
      end
      object Checkfilesystem1: TMenuItem [1]
        Action = actCheckFilesystem
      end
      object N6: TMenuItem [2]
        Caption = '-'
      end
      object N23: TMenuItem [4]
        Caption = '-'
      end
      object Overwrite1: TMenuItem [5]
        Caption = 'Overwrite'
        Hint = 'Contains commands for overwriting.'
        object Overwritefile1: TMenuItem
          Action = actOverwriteFile
        end
        object Overwritefolder1: TMenuItem
          Action = actOverwriteDir
        end
      end
    end
  end
  inherited ActionList1: TActionList
    object actPlaintextMountFile: TAction [2]
      Caption = '&Mount file...'
      Hint = 'Mount file based plaintext volume'
      OnExecute = actPlaintextMountFileExecute
    end
    inherited actDismount: TAction
      OnExecute = actDismountExecute
    end
    object actLinuxMountHidden: TAction [5]
      Caption = '&Mount file (hidden) ...'
      Hint = 'Mount Linux file and disable LUKS detection'
      OnExecute = actLinuxMountHiddenExecute
    end
    inherited actAbout: TAction
      OnExecute = actAboutExecute
    end
    inherited actOptions: TAction
      OnExecute = actOptionsExecute
    end
    object actSelectAll: TAction [21]
      Caption = 'Select &all'
      Hint = 'Selects all items in the window'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
    end
    object actShowBootSector: TAction [22]
      Caption = '&Show boot sector details...'
      Hint = 'Displays information about the boot sector'
      OnExecute = actShowBootSectorExecute
    end
    object actCheckFilesystem: TAction [23]
      Caption = 'Check filesystem'
      Hint = 'Checks the mounted volume for filesystem errors'
      OnExecute = actCheckFilesystemExecute
    end
    object actListStyleIcons: TAction [24]
      AutoCheck = True
      Caption = 'Ico&ns'
      GroupIndex = 1
      Hint = 'Displays items by using large icons'
      OnExecute = actListStyleExecute
    end
    object actListStyleSmallIcons: TAction [25]
      AutoCheck = True
      Caption = '&Small Icons'
      GroupIndex = 1
      Hint = 'Displays items by using small icons'
      OnExecute = actListStyleExecute
    end
    object actListStyleList: TAction [26]
      AutoCheck = True
      Caption = '&List'
      GroupIndex = 1
      Hint = 'Displays items in a list.'
      OnExecute = actListStyleExecute
    end
    object actListStyleDetails: TAction [27]
      AutoCheck = True
      Caption = '&Details'
      GroupIndex = 1
      Hint = 'Displays information about each item in the window.'
      OnExecute = actListStyleExecute
    end
    object actCheckStatusBar: TAction [28]
      AutoCheck = True
      Caption = 'Status &bar'
      Hint = 'Shows or hides the status bar.'
      OnExecute = actCheckStatusBarExecute
    end
    object actCheckAddressBar: TAction [29]
      AutoCheck = True
      Caption = '&Address Bar'
      Hint = 'Displays the Address bar.'
      OnExecute = actCheckAddressBarExecute
    end
    object actCheckToolbarVolume: TAction [30]
      AutoCheck = True
      Caption = '&FreeOTFE Buttons'
      Hint = 'Displays the FreeOTFE Buttons toolbar.'
      OnExecute = actCheckToolbarVolumeExecute
    end
    object actCheckToolbarExplorer: TAction [31]
      AutoCheck = True
      Caption = '&Standard Buttons'
      Hint = 'Displays the Standard Buttons toolbar.'
      OnExecute = actCheckToolbarExplorerExecute
    end
    object actExtract: TAction [32]
      Caption = 'E&xtract...'
      Hint = 'Extracts the selected items'
      ImageIndex = 13
      OnExecute = actExtractExecute
    end
    object actStoreFile: TAction [33]
      Caption = '&File...'
      Hint = 'Copies one or more files to the mounted volume'
      OnExecute = actStoreFileExecute
    end
    object actStoreDir: TAction [34]
      Caption = 'F&older...'
      Hint = 'Copies a folder to the mounted volume'
      OnExecute = actStoreDirExecute
    end
    object actCreateSubDir: TAction [35]
      Caption = 'Create &subfolder...'
      Hint = 'Creates a new, empty folder.'
      ShortCut = 45
      OnExecute = actCreateSubDirExecute
    end
    object actDelete: TAction [36]
      Caption = '&Delete'
      Hint = 'Deletes the selected items.'
      ImageIndex = 11
      ShortCut = 46
      OnExecute = actDeleteExecute
    end
    object actItemProperties: TAction [37]
      Caption = '&Properties...'
      Hint = 'Displays the properties of the selected items.'
      ImageIndex = 15
      ShortCut = 32781
      OnExecute = actItemPropertiesExecute
    end
    object actUpDir: TAction [38]
      Caption = 'Up'
      OnExecute = actUpDirExecute
    end
    object actNavigateBack: TAction [39]
      Caption = 'Back'
      OnExecute = actNavigateBackExecute
    end
    object actNavigateForward: TAction [40]
      Caption = 'Forward'
      OnExecute = actNavigateForwardExecute
    end
    object actPlaintextNew: TAction [41]
      Caption = '&New file...'
      Hint = 'New plaintext volume'
      OnExecute = actPlaintextNewExecute
    end
    object actCheckExplorerBarFolders: TAction [42]
      AutoCheck = True
      Caption = 'F&olders'
      Hint = 'Shows the Folders bar.'
      OnExecute = actCheckExplorerBarFoldersExecute
    end
    object actRename: TAction [43]
      Caption = 'Rena&me'
      Hint = 'Renames the selected item'
      ShortCut = 113
      OnExecute = actRenameExecute
    end
    object actMoveTo: TAction [44]
      Caption = 'Mo&ve To Folder...'
      Hint = 'Moves the selected items to another location'
      OnExecute = actMoveToExecute
    end
    object actCopyTo: TAction [45]
      Caption = 'Copy To &Folder...'
      Hint = 'Copies the selected items to another location'
      OnExecute = actCopyToExecute
    end
    object actInvertSelection: TAction [46]
      Caption = '&Invert Selection'
      Hint = 'Reverses which items are selected and which are not.'
      OnExecute = actInvertSelectionExecute
    end
    object actCut: TAction [47]
      Caption = 'Cu&t'
      Hint = 'Removes the current selection and copies it to the Clipboard.'
      ShortCut = 16472
      OnExecute = actCutExecute
    end
    object actCopy: TAction [48]
      Caption = '&Copy'
      Hint = 'Copies the current selection onto the Clipboard.'
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
    object actPaste: TAction [49]
      Caption = '&Paste'
      Hint = 
        'Inserts the items you have copied or cut into the specified loca' +
        'tion.'
      ShortCut = 16470
      OnExecute = actPasteExecute
    end
    object actOverwriteFile: TAction [50]
      Caption = '&File...'
      Hint = 'Overwrite a file on your hard drive'
      OnExecute = actOverwriteFileExecute
    end
    object actOverwriteDir: TAction [51]
      Caption = 'F&older...'
      Hint = 'Overwrite a folder on your hard drive'
      OnExecute = actOverwriteDirExecute
    end
  end
  inherited SDUMultimediaKeys1: TSDUMultimediaKeys
    OnBrowserBackward = actNavigateBackExecute
    OnBrowserForward = actNavigateForwardExecute
  end
  object pmTreeView: TPopupMenu
    Images = ilToolbarIcons_Small
    OnPopup = pmViewPopup
    Left = 176
    Top = 256
    object mnuTreeViewExpand: TMenuItem
      Caption = '&Expand'
      OnClick = mnuTreeViewExpandClick
    end
    object mnuTreeViewCollapse: TMenuItem
      Caption = '&Collapse'
      OnClick = mnuTreeViewCollapseClick
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object mnuTreeViewStore: TMenuItem
      Caption = '&Store'
      Hint = 
        'Contains commands for copying files and folders to the mounted v' +
        'olume.'
      ImageIndex = 14
      object mnuTreeViewStoreFile: TMenuItem
        Action = actStoreFile
      end
      object mnuTreeViewStoreDir: TMenuItem
        Action = actStoreDir
      end
    end
    object mnuTreeViewExtract: TMenuItem
      Action = actExtract
    end
    object N11: TMenuItem
      Caption = '-'
    end
    object mnuTreeViewCut: TMenuItem
      Action = actCut
    end
    object mnuTreeViewCopy: TMenuItem
      Action = actCopy
    end
    object mnuTreeViewPaste: TMenuItem
      Action = actPaste
    end
    object N21: TMenuItem
      Caption = '-'
    end
    object mnuTreeViewCreateSubDir: TMenuItem
      Action = actCreateSubDir
    end
    object mnuTreeViewDelete: TMenuItem
      Action = actDelete
    end
    object mnuTreeViewRename: TMenuItem
      Action = actRename
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object mnuTreeViewItemProperties: TMenuItem
      Action = actItemProperties
    end
  end
  object pmListView: TPopupMenu
    Images = ilToolbarIcons_Small
    OnPopup = pmViewPopup
    Left = 230
    Top = 256
    object mnuExploreView: TMenuItem
      Caption = '&Explore'
      OnClick = mnuExploreViewClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mnuListViewStore: TMenuItem
      Caption = '&Store'
      Hint = 
        'Contains commands for copying files and folders to the mounted v' +
        'olume.'
      ImageIndex = 14
      object mnuListViewStoreFile: TMenuItem
        Action = actStoreFile
      end
      object mnuListViewStoreDir: TMenuItem
        Action = actStoreDir
      end
    end
    object mnuListViewExtract: TMenuItem
      Action = actExtract
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object actcut1: TMenuItem
      Action = actCut
    end
    object Copy3: TMenuItem
      Action = actCopy
    end
    object Paste3: TMenuItem
      Action = actPaste
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object mnuViewCreateSubDir: TMenuItem
      Action = actCreateSubDir
    end
    object mnuViewDelete: TMenuItem
      Action = actDelete
    end
    object mnuViewRename: TMenuItem
      Action = actRename
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object mnuViewProperties: TMenuItem
      Action = actItemProperties
    end
  end
  object SDUOpenDialog_MountPlaintextImage: TSDUOpenDialog
    PreserveCWD = False
    Left = 91
    Top = 251
  end
  object ImageList_StatusBar: TImageList
    Left = 330
    Top = 370
    Bitmap = {
      494C010101000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000262626003E3E3E002626260000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000262626008080800062626200808080006262620080808000262626000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002626
      26009E9E9E003E3E3E00000000003E3E3E003E3E3E00000000009E9E9E002626
      2600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008080
      8000000000003E3E3E003E3E3E00808080003E3E3E003E3E3E003E3E3E008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000323232006262
      62003E3E3E003E3E3E006262620000000000626262003E3E3E00000000006262
      6200262626000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000626262008080
      80003E3E3E00A4A0A000000000000000000000000000808080003E3E3E008080
      80003E3E3E000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000323232006E6E
      6E0000000000626262006262620000000000626262003E3E3E003E3E3E006262
      6200262626000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A4A0
      A0003E3E3E003E3E3E0062626200A4A0A0003E3E3E003E3E3E00000000008080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003232
      3200AAAAAA00000000003E3E3E003E3E3E00000000003E3E3E009E9E9E002626
      2600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000062626200A4A0A0006E6E6E008080800062626200808080003E3E3E000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A4A0A0002626260032323200626262003232320032323200808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A4A0A0003E3E3E0000000000000000000000000062626200808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A4A0A0003E3E3E0000000000000000000000000062626200808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000808080006262620026262600000000003232320080808000626262000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000032323200A4A0A00092929200808080008080800080808000262626000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000032323200626262003232320000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00F83F000000000000F01F000000000000
      E00F000000000000C007000000000000C007000000000000C007000000000000
      C007000000000000C007000000000000E00F000000000000E00F000000000000
      E00F000000000000E38F000000000000E38F000000000000E00F000000000000
      F01F000000000000F83F00000000000000000000000000000000000000000000
      000000000000}
  end
  object SDUOpenDialog_Store: TSDUOpenDialog
    PreserveCWD = False
    Left = 72
    Top = 296
  end
  object SDUSaveDialog_Extract: TSDUSaveDialog
    PreserveCWD = False
    Left = 104
    Top = 296
  end
  object SDUDropFilesTreeView: TSDUDropFiles
    Active = False
    DropControl = SDFilesystemTreeView1
    OnItemsDrop = SDUDropFilesTreeViewItemsDrop
    Left = 176
    Top = 296
  end
  object SDUDropFilesListView: TSDUDropFiles
    Active = False
    DropControl = SDFilesystemListView1
    OnItemsDrop = SDUDropFilesListViewItemsDrop
    Left = 228
    Top = 296
  end
  object pmToolbarBack: TPopupMenu
    OnPopup = pmToolbarBackPopup
    Left = 296
    Top = 124
  end
  object pmToolbarViews: TPopupMenu
    Left = 392
    Top = 124
    object Icons2: TMenuItem
      Action = actListStyleIcons
      AutoCheck = True
    end
    object SmallIcons2: TMenuItem
      Action = actListStyleSmallIcons
      AutoCheck = True
    end
    object List2: TMenuItem
      Action = actListStyleList
      AutoCheck = True
    end
    object Details2: TMenuItem
      Action = actListStyleDetails
      AutoCheck = True
    end
  end
  object pmToolbarForward: TPopupMenu
    OnPopup = pmToolbarForwardPopup
    Left = 328
    Top = 124
  end
  object pmToolbarStore: TPopupMenu
    Left = 360
    Top = 124
    object mnuToolbarStoreFile: TMenuItem
      Action = actStoreFile
    end
    object mnuToolbarStoreDir: TMenuItem
      Action = actStoreDir
    end
  end
  object pmToolbarMenu: TPopupMenu
    Left = 416
    Top = 3
    object FreeOTFEButtons1: TMenuItem
      Action = actCheckToolbarVolume
      AutoCheck = True
    end
    object StandardButtons2: TMenuItem
      Action = actCheckToolbarExplorer
      AutoCheck = True
    end
    object AddressBar2: TMenuItem
      Action = actCheckAddressBar
      AutoCheck = True
    end
  end
  object SDUOpenDialog_Overwrite: TSDUOpenDialog
    PreserveCWD = False
    Left = 408
    Top = 212
  end
end
