object frmMain: TfrmMain
  Left = 285
  Top = 139
  BorderStyle = bsDialog
  Caption = 'Title set automatically...'
  ClientHeight = 317
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 453
    Height = 269
    ActivePage = TabSheet1
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Overwrite drive free space'
      object GroupBox1: TGroupBox
        Left = 8
        Top = 20
        Width = 429
        Height = 97
        Caption = 'Overwrite drive free space'
        TabOrder = 0
        object Label1: TLabel
          Left = 12
          Top = 20
          Width = 280
          Height = 13
          Caption = 'Select the drive which is to have it'#39's free space overwritten:'
        end
        object dcbOverwriteDriveFreeSpace: TDriveComboBox
          Left = 76
          Top = 44
          Width = 145
          Height = 19
          TabOrder = 0
        end
        object pbTestOverwriteDriveFreeSpace: TButton
          Left = 252
          Top = 52
          Width = 75
          Height = 25
          Caption = 'Test'
          TabOrder = 2
          OnClick = pbTestOverwriteDriveFreeSpaceClick
        end
        object ckSilentOverwriteDriveFreeSpace: TCheckBox
          Left = 76
          Top = 68
          Width = 97
          Height = 17
          Caption = 'Silent'
          TabOrder = 1
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Overwrite file slack'
      ImageIndex = 1
      object GroupBox2: TGroupBox
        Left = 8
        Top = 8
        Width = 429
        Height = 105
        Caption = 'Overwrite file slack'
        TabOrder = 0
        object Label5: TLabel
          Left = 12
          Top = 20
          Width = 403
          Height = 13
          Caption = 
            'Please specify the full filename of the file which is to have it' +
            #39's slack space overwritten:'
        end
        object edFileSlackFilename: TEdit
          Left = 12
          Top = 40
          Width = 381
          Height = 21
          TabOrder = 0
        end
        object pbBrowseFileSlackFilename: TButton
          Left = 392
          Top = 40
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 1
          OnClick = pbBrowseFileSlackFilenameClick
        end
        object pbOverwriteFileSlack: TButton
          Left = 177
          Top = 68
          Width = 75
          Height = 25
          Caption = 'Test'
          TabOrder = 2
          OnClick = pbOverwriteFileSlackClick
        end
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 120
        Width = 429
        Height = 109
        Caption = 'Overwrite all file slack'
        TabOrder = 1
        object Label4: TLabel
          Left = 16
          Top = 20
          Width = 284
          Height = 13
          Caption = 
            'Select the drive which is to have all it'#39's file slack overwritte' +
            'n:'
        end
        object dcbOverwriteAllFileSlack: TDriveComboBox
          Left = 142
          Top = 44
          Width = 145
          Height = 19
          TabOrder = 0
        end
        object pbOverwriteAllFileSlack: TButton
          Left = 211
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Test'
          TabOrder = 2
          OnClick = pbOverwriteAllFileSlackClick
        end
        object ckSilentOverwriteAllFileSlack: TCheckBox
          Left = 143
          Top = 76
          Width = 58
          Height = 17
          Caption = 'Silent'
          TabOrder = 1
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Destroy file or dir'
      ImageIndex = 3
      object GroupBox4: TGroupBox
        Left = 8
        Top = 20
        Width = 429
        Height = 109
        Caption = 'Destroy file or dir'
        TabOrder = 0
        object Label6: TLabel
          Left = 16
          Top = 20
          Width = 209
          Height = 13
          Caption = 'Please specify the file or directory to destroy:'
        end
        object edDestroyFileDirName: TEdit
          Left = 16
          Top = 40
          Width = 361
          Height = 21
          TabOrder = 0
        end
        object pbBrowseDestroyFilename: TButton
          Left = 376
          Top = 40
          Width = 21
          Height = 21
          Caption = 'F'
          TabOrder = 1
          OnClick = pbBrowseDestroyFilenameClick
        end
        object pbBrowseDestroyDirname: TButton
          Left = 396
          Top = 40
          Width = 21
          Height = 21
          Caption = 'D'
          TabOrder = 2
          OnClick = pbBrowseDestroyDirnameClick
        end
        object pbDestroyFileDir: TButton
          Left = 235
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Test'
          TabOrder = 5
          OnClick = pbDestroyFileDirClick
        end
        object ckSilentDestroyFileDir: TCheckBox
          Left = 119
          Top = 68
          Width = 97
          Height = 17
          Caption = 'Silent'
          TabOrder = 3
        end
        object ckQuickShred: TCheckBox
          Left = 119
          Top = 84
          Width = 97
          Height = 17
          Caption = 'Quickshred'
          TabOrder = 4
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Destroy registry key'
      ImageIndex = 4
      object GroupBox5: TGroupBox
        Left = 8
        Top = 20
        Width = 429
        Height = 101
        Caption = 'Destroy registry key'
        TabOrder = 0
        object Label2: TLabel
          Left = 16
          Top = 20
          Width = 253
          Height = 13
          Caption = 'Enter the full name of the registry key to be destroyed:'
        end
        object Label3: TLabel
          Left = 16
          Top = 68
          Width = 157
          Height = 13
          Caption = 'e.g. HKLM\TestKey\TestSubkey'
        end
        object edRegkey: TEdit
          Left = 16
          Top = 40
          Width = 402
          Height = 21
          TabOrder = 0
        end
        object pbTestDestroyRegkey: TButton
          Left = 344
          Top = 68
          Width = 75
          Height = 25
          Caption = 'Test'
          TabOrder = 1
          OnClick = pbTestDestroyRegkeyClick
        end
      end
    end
  end
  object pbClose: TButton
    Left = 384
    Top = 284
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 1
    OnClick = pbCloseClick
  end
  object ShredderObj: TShredder
    FileDirUseInt = True
    FreeUseInt = True
    ExtShredFilesThenDir = False
    IntFirstBytes = 1048576
    IntGutmann = False
    IntPasses = 1
    IntFreeSpcFileSize = 52428800
    IntFreeSpcSmartFileSize = True
    IntFreeSpcFileCreationBlkSize = 1048576
    IntFileBufferSize = 1048576
    Left = 264
    Top = 284
  end
  object OpenDialog1: TOpenDialog
    Left = 228
    Top = 284
  end
end
