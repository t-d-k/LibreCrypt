inherited frmWizardCreateVolume: TfrmWizardCreateVolume
  Left = 388
  Top = 258
  BorderStyle = bsSizeToolWin
  Caption = 'New DoxBox Wizard'
  ClientHeight = 422
  ClientWidth = 537
  Constraints.MinHeight = 456
  Font.Name = 'MS Sans Serif'
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 553
  ExplicitHeight = 456
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlRight: TPanel
    Width = 537
    Height = 422
    ExplicitWidth = 895
    ExplicitHeight = 546
    inherited bvlLine: TBevel
      Top = 359
      Width = 535
      ExplicitTop = 333
      ExplicitWidth = 725
    end
    inherited pcWizard: TPageControl
      Width = 535
      Height = 358
      ActivePage = tsDriveLetter
      ExplicitWidth = 893
      ExplicitHeight = 482
      object tsFileOrPartition: TTabSheet
        Caption = 'Volume File or Partition'
        ImageIndex = 14
        Constraints.MinHeight = 150
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          330)
        object lblWelcomeBanner: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 10
          Width = 507
          Height = 20
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alTop
          Alignment = taCenter
          Caption = 'Welcome to the New DoxBox Wizard.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitWidth = 296
        end
        object lblWelcomeClickNext: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 307
          Width = 507
          Height = 13
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alBottom
          Alignment = taCenter
          Caption = 'Please click "Next >" to continue.'
          ExplicitWidth = 160
        end
        object lblInstructFileOrPartition: TLabel
          Left = 10
          Top = 56
          Width = 497
          Height = 113
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = 
            'This wizard will guide you through the process of creating a new' +
            ' DoxBox, or a hidden box within an existing box.Firstly, please ' +
            'specify whether the new DoxBox should be created as a file store' +
            'd on your disk, or take up a disk partition.Normally you should ' +
            'use a file; encrypted partitions or  disks may give greater obsc' +
            'urity or speed.'
          WordWrap = True
          ExplicitWidth = 817
        end
        object rgFileOrPartition: TRadioGroup
          Left = 3
          Top = 187
          Width = 514
          Height = 87
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'File or Partition/Entire Disk'
          Items.Strings = (
            '1st of two'
            '2nd of two')
          TabOrder = 0
          OnClick = rgFileOrPartitionClick
          ExplicitWidth = 827
        end
      end
      object tsFilename: TTabSheet
        Caption = 'Volume filename'
        ImageIndex = 1
        Constraints.MinHeight = 139
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        object reInstructFilename: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 243
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Constraints.MinHeight = 61
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            
              'Please specify the filename of the new DoxBox, by clicking the b' +
              'rowse button.'
            
              #39'If you wish to create a hidden DoxBox, please specify your exis' +
              'ting DoxBox file to be used.')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
          ExplicitHeight = 380
        end
        object GroupBox1: TGroupBox
          Left = 0
          Top = 263
          Width = 527
          Height = 67
          Align = alBottom
          Caption = 'Volume filename'
          TabOrder = 1
          ExplicitTop = 387
          ExplicitWidth = 885
          DesignSize = (
            527
            67)
          object pbBrowseFilename: TButton
            Left = 436
            Top = 22
            Width = 75
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Browse...'
            TabOrder = 0
            OnClick = pbBrowseFilenameClick
            ExplicitLeft = 794
          end
          object lblFilename: TEdit
            Left = 16
            Top = 24
            Width = 414
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            TabOrder = 1
            Text = 'lblFilename'
            ExplicitWidth = 772
          end
        end
      end
      object tsPartitionWarning: TTabSheet
        Caption = 'Partition Warning'
        ImageIndex = 16
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        object Label15: TLabel
          Left = 0
          Top = 304
          Width = 527
          Height = 26
          Align = alBottom
          Alignment = taCenter
          Caption = 
            'DO NOT PROCEED UNLESS YOU UNDERSTAND THE FULL IMPLICATIONS OF CR' +
            'EATING ENCRYPTED PARTITIONS.'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentColor = False
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 526
        end
        object lblWarningPartition: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 13
          Align = alTop
          Alignment = taCenter
          Caption = 'WARNING'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold, fsUnderline]
          ParentColor = False
          ParentFont = False
          ExplicitWidth = 61
        end
        object reInstructPartitionWarning: TRichEdit
          Left = 0
          Top = 13
          Width = 527
          Height = 291
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Constraints.MinHeight = 94
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            '1) Creating encrypted partitions is POTENTIALLY DESTRUCTIVE.'
            
              '2) Data stored on the partition you select in the next stage may' +
              ' be OVERWRITTEN with encrypted data.'#39' + '
            'SDUCRLF + '
            'SDUCRLF +'
            
              '3) It is RECOMMENDED that you backup your data as appropriate be' +
              'fore continuing.')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
          ExplicitHeight = 428
        end
      end
      object tsPartitionSelect: TTabSheet
        Caption = 'Partition Select'
        ImageIndex = 15
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        object Label21: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 82
          Width = 517
          Height = 13
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alTop
          Caption = '&Disk/partition to create encrypted volume on:'
          ExplicitWidth = 213
        end
        object reInstructPartitionSelect: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 77
          Align = alTop
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            
              'Please select the partition you wish to create the new DoxBox on' +
              ', and whether you would like to create a '
            '"hidden" DoxBox.')
          ParentFont = False
          TabOrder = 2
          ExplicitWidth = 885
        end
        object ckPartitionHidden: TCheckBox
          AlignWithMargins = True
          Left = 5
          Top = 308
          Width = 517
          Height = 17
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alBottom
          Caption = '&Create hidden volume within selected partition'
          TabOrder = 0
          OnClick = ckPartitionHiddenClick
          ExplicitTop = 432
          ExplicitWidth = 875
        end
        inline fmeSelectPartition: TfmeSelectPartition
          Left = 0
          Top = 100
          Width = 527
          Height = 203
          Align = alClient
          Constraints.MinHeight = 151
          TabOrder = 1
          ExplicitTop = 100
          ExplicitWidth = 527
          ExplicitHeight = 203
          inherited lblErrorWarning: TLabel
            Top = 184
            ExplicitTop = 125
          end
          inherited imgErrorWarning: TImage
            Top = 182
            ExplicitTop = 123
          end
          inherited TabControl1: TTabControl
            Width = 527
            Height = 202
            Constraints.MinHeight = 132
            ExplicitWidth = 527
            ExplicitHeight = 202
            inherited SDUDiskPartitionsPanel1: TOTFEFreeOTFEDiskPartitionsPanel
              Height = 174
              ExplicitHeight = 298
            end
            inherited pnlNoPartitionDisplay: TPanel
              Left = 338
              Height = 174
              ExplicitLeft = 696
              ExplicitHeight = 298
            end
          end
          inherited ckShowCDROM: TCheckBox
            Left = 294
            Top = 164
            ExplicitLeft = 652
            ExplicitTop = 288
          end
          inherited ckEntireDisk: TCheckBox
            Left = 438
            Top = 164
            ExplicitLeft = 796
            ExplicitTop = 288
          end
          inherited ilErrorWarning: TImageList
            Left = 368
            Top = 65533
            Bitmap = {
              494C010103000400D00010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
              0000000000003600000028000000400000001000000001002000000000000010
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000080800000FFFF0000FFFF0000FF
              FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
              FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000000000000080800000FFFF0000FF
              FF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
              FF0000FFFF0000FFFF00000000000000000000000000000000000000FF000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF0000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000000000000080800000FFFF0000FF
              FF0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
              FF0000FFFF0000FFFF000000000000000000000000000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000000000000000000000000000000000000000000000000000080800000FF
              FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
              FF0000FFFF00000000000000000000000000000000000000FF000000FF000000
              FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00FFFFFF00FFFF
              FF000000FF000000FF000000FF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000000000000000000000000000000000000000000000000000080800000FF
              FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
              FF0000FFFF000000000000000000000000000000FF000000FF000000FF000000
              FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000080
              800000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
              FF00000000000000000000000000000000000000FF000000FF000000FF000000
              FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000080
              800000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FF
              FF00000000000000000000000000000000000000FF000000FF000000FF000000
              FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000080800000FFFF0000FFFF00000000000000000000FFFF0000FFFF000000
              0000000000000000000000000000000000000000FF000000FF000000FF000000
              FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              00000080800000FFFF0000FFFF00000000000000000000FFFF0000FFFF000000
              0000000000000000000000000000000000000000FF000000FF000000FF000000
              FF000000FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000080800000FFFF00000000000000000000FFFF00000000000000
              0000000000000000000000000000000000000000FF000000FF000000FF000000
              FF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF00FFFF
              FF000000FF000000FF000000FF000000FF000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000080800000FFFF0000FFFF0000FFFF0000FFFF00000000000000
              000000000000000000000000000000000000000000000000FF000000FF000000
              FF00FFFFFF00FFFFFF000000FF000000FF000000FF000000FF00FFFFFF00FFFF
              FF000000FF000000FF000000FF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000080800000FFFF0000FFFF0000000000000000000000
              000000000000000000000000000000000000000000000000FF000000FF000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF000000FF00000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000080800000FFFF0000FFFF0000000000000000000000
              00000000000000000000000000000000000000000000000000000000FF000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000FF0000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000008080000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
              FF000000FF000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000008080000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
              0000000000000000000000000000000000000000000000000000000000000000
              0000000000000000000000000000000000000000000000000000000000000000
              000000000000000000000000000000000000424D3E000000000000003E000000
              2800000040000000100000000100010000000000800000000000000000000000
              000000000000000000000000FFFFFF00FFFF0000F81F0000FFFF0000E0070000
              FFFF8001C0030000FFFF800180010000FFFFC00380010000FFFFC00300000000
              FFFFE00700000000FFFFE00700000000FFFFF00F00000000FFFFF00F00000000
              FFFFF81F00000000FFFFF81F80010000FFFFFC3F80010000FFFFFC3FC0030000
              FFFFFE7FE0070000FFFFFE7FF81F000000000000000000000000000000000000
              000000000000}
          end
          inherited PopupMenu1: TPopupMenu
            Left = 415
            Top = 1
          end
          inherited ActionList1: TActionList
            Left = 457
            Top = 65526
          end
        end
      end
      object tsSize: TTabSheet
        Caption = 'Size'
        ImageIndex = 6
        Constraints.MinHeight = 172
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          330)
        object Label6: TLabel
          Left = 78
          Top = 282
          Width = 95
          Height = 13
          Anchors = [akRight, akBottom]
          Caption = '&Size of new volume:'
          ExplicitTop = 277
        end
        object lblPartitionDiskSize: TLabel
          Left = 457
          Top = 282
          Width = 53
          Height = 13
          Anchors = [akRight, akBottom]
          Caption = 'Approx: %1'
          ExplicitTop = 277
        end
        object reInstructSize: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 260
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Constraints.MinHeight = 108
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            'Please enter the required size of the new DoxBox.'#39' '
            
              #39'This value must be more than 1 MB, and must be a multiple of 51' +
              '2 bytes.'#39' +'
            
              #39'If you are creating a hidden DoxBox, then this is the size of t' +
              'he hidden Box. Please see the help for minium '
            'sizes of hidden boxes'
            
              #39'Note: The size you give here will be the amount available for y' +
              'our data. The actual file created will be slightly '
            'larger than this.'#39');')
          ParentFont = False
          TabOrder = 2
          ExplicitWidth = 885
          ExplicitHeight = 384
        end
        object ckSizeEntirePartitionDisk: TCheckBox
          Left = 17
          Top = 284
          Width = 141
          Height = 312
          Anchors = [akLeft, akTop, akBottom]
          Caption = '&Use entire partition/disk'
          TabOrder = 0
          OnClick = ckSizeEntirePartitionDiskClick
          ExplicitHeight = 436
        end
        object se64UnitSize: TSDUSpin64Unit_Storage
          Left = 207
          Top = 282
          Width = 226
          Height = 106
          Anchors = [akRight, akBottom]
          TabOrder = 1
          OnExit = seSizeExit
          Units.Strings = (
            'bytes'
            'KB'
            'MB'
            'GB'
            'TB')
          SelectedUnits = 'bytes'
          MaxLength = 0
          ReadOnly = False
          OnChange = SizeChanged
          ExplicitLeft = 565
          ExplicitTop = 406
        end
      end
      object tsPassword: TTabSheet
        Caption = 'Password'
        ImageIndex = 6
        Constraints.MinHeight = 330
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          330)
        object Label17: TLabel
          Left = 3
          Top = 163
          Width = 53
          Height = 13
          Caption = '&Keyphrase:'
        end
        object Label3: TLabel
          Left = 3
          Top = 237
          Width = 91
          Height = 13
          Caption = '&Confirm Keyphrase:'
          FocusControl = preUserKey2
        end
        object lblFinished: TLabel
          Left = 3
          Top = 285
          Width = 506
          Height = 29
          Alignment = taCenter
          Anchors = [akLeft, akRight, akBottom]
          AutoSize = False
          Caption = 
            'Click '#39'Finish'#39' to create your DoxBox, or '#39'Next'#39' for more advance' +
            'd options'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitTop = 311
          ExplicitWidth = 739
        end
        object reInstructPassword: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 145
          Align = alTop
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
        end
        object preUserKey1: TOTFEFreeOTFE_PasswordRichEdit
          Left = 112
          Top = 160
          Width = 397
          Height = 68
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'preUserKey1')
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 1
          OnChange = preUserKeyChange
        end
        object preUserKey2: TOTFEFreeOTFE_PasswordRichEdit
          Left = 112
          Top = 237
          Width = 397
          Height = 42
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'preUserKey2')
          ParentFont = False
          ScrollBars = ssBoth
          TabOrder = 2
          OnChange = preUserKeyChange
        end
      end
      object tsDriveLetter: TTabSheet
        Caption = 'Drive Letter'
        ImageIndex = 17
        Constraints.MinHeight = 166
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object Label11: TLabel
          Left = 313
          Top = 266
          Width = 107
          Height = 13
          Anchors = [akRight, akBottom]
          Caption = '&Requested drive letter:'
          FocusControl = cbDriveLetter
          ExplicitLeft = 414
          ExplicitTop = 287
        end
        object lblInstructDriveLetter: TLabel
          Left = 3
          Top = 93
          Width = 514
          Height = 132
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = 
            'Please enter the drive letter you would normally like this volum' +
            'e mounted under. Note: You will be able to change this after the' +
            ' volume has been created.'
          WordWrap = True
        end
        object reInstructDriveLetter: TRichEdit
          Left = 0
          Top = 0
          Width = 517
          Height = 81
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Lines.Strings = (
            'REWRITTEN'
            
              '_('#39'Please enter the drive letter you would normally like this vo' +
              'lume mounted under.'#39' +'
            
              '    SDUCRLF + SDUCRLF + '#39'Note: You will be able to change this a' +
              'fter the volume has been created.'#39');')
          ParentFont = False
          TabOrder = 0
        end
        object cbDriveLetter: TComboBox
          Left = 435
          Top = 263
          Width = 89
          Height = 21
          Style = csDropDownList
          Anchors = [akRight, akBottom]
          TabOrder = 1
          OnChange = cbDriveLetterChange
        end
      end
      object tsOffset: TTabSheet
        Caption = 'Offset'
        ImageIndex = 7
        Constraints.MinHeight = 343
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          343)
        object Label7: TLabel
          Left = 137
          Top = 316
          Width = 120
          Height = 13
          Anchors = [akRight, akBottom]
          Caption = '&Byte offset within volume:'
          ExplicitTop = 319
        end
        object pnlWarningOffset: TPanel
          Left = 0
          Top = 77
          Width = 527
          Height = 418
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'pnlWarningOffset'
          Constraints.MinHeight = 418
          TabOrder = 1
          ExplicitWidth = 885
          ExplicitHeight = 529
          DesignSize = (
            527
            418)
          object lblWarningOffset: TLabel
            AlignWithMargins = True
            Left = 11
            Top = 11
            Width = 505
            Height = 13
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 10
            Align = alTop
            Alignment = taCenter
            Caption = 'WARNING'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold, fsUnderline]
            ParentFont = False
            ExplicitWidth = 61
          end
          object Label13: TLabel
            AlignWithMargins = True
            Left = 4
            Top = 180
            Width = 285
            Height = 39
            Margins.Left = 10
            Margins.Top = 10
            Margins.Right = 10
            Margins.Bottom = 10
            Alignment = taCenter
            Anchors = [akLeft, akRight, akBottom]
            Caption = 
              'DO NOT PROCEED UNLESS YOU UNDERSTAND THE FULL IMPLICATIONS OF CR' +
              'EATING HIDDEN VOLUMES.'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
            WordWrap = True
          end
          object reInstructWarningOffset: TRichEdit
            Left = 0
            Top = 37
            Width = 525
            Height = 130
            Anchors = [akLeft, akTop, akRight, akBottom]
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            Constraints.MinHeight = 130
            Lines.Strings = (
              'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
              ' 1) Creating hidden Boxes is POTENTIALLY DESTRUCTIVE.'
              
                #39'2) Data within the file/partition specified in the previous sta' +
                'ge may be OVERWRITTEN, starting from byte '
              'offset specified,'
              ' and running for the full length of the new hidden DoxBox.'#39' '
              
                '    '#39'3) If you ae not using the default offset, remember the off' +
                'set entered! If in doubt use the '#39#39'Default Offset'#39#39' '
              'value. '#39'+'
              
                '    '#39'For security reasons, this information is not stored anywhe' +
                're; you will need to type in this offset whenever  '
              'you wish to mount your hidden DoxBox.'#39');')
            ParentFont = False
            TabOrder = 0
            ExplicitWidth = 883
            ExplicitHeight = 241
          end
        end
        object reInstructOffset: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 77
          Align = alTop
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            
              ' The DoxBox file you specified already exists. To create a hidde' +
              'n DoxBox within this file, please specify the '
            'byte offset within this file from where the '
            'new DoxBox should begin.'
            
              #39'If you do NOT wish to create a hidden DoxBox within your existi' +
              'ng file, please click "< Back" and enter a '
            'different filename.')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
        end
        object se64UnitByteOffset: TSDUSpin64Unit_Storage
          Left = 301
          Top = 312
          Width = 215
          Height = 25
          Anchors = [akRight, akBottom]
          TabOrder = 2
          Units.Strings = (
            'bytes'
            'KB'
            'MB'
            'GB'
            'TB')
          SelectedUnits = 'bytes'
          MaxLength = 0
          ReadOnly = False
          OnChange = se64ByteOffsetChange
          ExplicitLeft = 659
          ExplicitTop = 423
        end
      end
      object tsHashCypherIV: TTabSheet
        Caption = 'Hash, cypher and IV'
        ImageIndex = 2
        Constraints.MinHeight = 202
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object Label4: TLabel
          Left = 17
          Top = 221
          Width = 59
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Select &hash:'
          FocusControl = cbHash
          ExplicitTop = 208
        end
        object Label5: TLabel
          Left = 17
          Top = 249
          Width = 68
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Select &cypher:'
          FocusControl = cbCypher
          ExplicitTop = 236
        end
        object lblSectorIVGenMethod: TLabel
          Left = 17
          Top = 277
          Width = 52
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Sector &IVs:'
          FocusControl = cbSectorIVGenMethod
          ExplicitTop = 264
        end
        object cbHash: TComboBox
          Left = 153
          Top = 217
          Width = 145
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akBottom]
          TabOrder = 6
          OnChange = cbHashCypherIVGenChange
        end
        object cbCypher: TComboBox
          Left = 153
          Top = 245
          Width = 145
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akBottom]
          TabOrder = 2
          OnChange = cbHashCypherIVGenChange
        end
        object reInstructHashCypherIV: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 192
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            
              '    _('#39'Please select which security options should be used in se' +
              'curing your new DoxBox.'#39' +'
            
              '    SDUCRLF + SDUCRLF + '#39'You can simply accept the defaults here' +
              '.'#39');')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
          ExplicitHeight = 329
        end
        object pbHashInfo: TButton
          Left = 304
          Top = 218
          Width = 21
          Height = 21
          Anchors = [akLeft, akBottom]
          Caption = '?'
          TabOrder = 1
          OnClick = pbHashInfoClick
          ExplicitTop = 355
        end
        object pbCypherInfo: TButton
          Left = 304
          Top = 245
          Width = 21
          Height = 21
          Anchors = [akLeft, akBottom]
          Caption = '?'
          TabOrder = 4
          OnClick = pbCypherInfoClick
          ExplicitTop = 382
        end
        object ckUsePerVolumeIV: TCheckBox
          Left = 153
          Top = 297
          Width = 260
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Additionally &XOR sector IV with per volume IV'
          TabOrder = 3
          ExplicitTop = 434
        end
        object cbSectorIVGenMethod: TComboBox
          Left = 153
          Top = 273
          Width = 145
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akBottom]
          TabOrder = 5
          OnChange = cbHashCypherIVGenChange
        end
      end
      object tsChaff: TTabSheet
        Caption = 'Chaff'
        ImageIndex = 20
        Constraints.MinHeight = 200
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object lblInstructChaff: TLabel
          Left = 3
          Top = 3
          Width = 521
          Height = 186
          Anchors = [akLeft, akTop, akRight, akBottom]
          AutoSize = False
          Caption = 
            'Choose how to overwrite the file or partition the DoxBox is stor' +
            'ed on.'#13#10'Secure data will make it harder - perhaps impossible - f' +
            'or an attacker to tell if a '#39'hidden'#39' box has been added at a lat' +
            'er date.'#13#10'It will also make it hard to tell the exact size of yo' +
            'ur data. Overwriting with secure data will take longer than with' +
            ' zeros.'#13#10#39'Zeros'#39' will make it easy for an attacker to tell if a ' +
            'hidden box has been added and the amount of data stored in your ' +
            'box, but allow the box to be created faster.'
          Constraints.MinHeight = 113
          WordWrap = True
          ExplicitWidth = 1172
          ExplicitHeight = 334
        end
        object rgOverwriteType: TRadioGroup
          Left = 22
          Top = 215
          Width = 480
          Height = 102
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Type of overwrite data'
          Items.Strings = (
            'Secure pseudorandom data'
            'Zeros')
          TabOrder = 0
          ExplicitTop = 352
          ExplicitWidth = 838
        end
      end
      object tsMasterKeyLength: TTabSheet
        Caption = 'Master Key Length'
        ImageIndex = 11
        Constraints.MinHeight = 77
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object Label2: TLabel
          Left = 17
          Top = 297
          Width = 89
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = '&Size of master key:'
          ExplicitTop = 284
        end
        object Label9: TLabel
          Left = 204
          Top = 297
          Width = 16
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'bits'
          ExplicitTop = 284
        end
        object reInstructMasterKeyLen: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 287
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            'The cypher you selected supports arbitary key lengths.'
            
              'Please specify the length of the master key which will be used t' +
              'o encrypt/decrypt your data.'#39)
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
          ExplicitHeight = 424
        end
        object seMasterKeyLength: TSpinEdit64
          Left = 137
          Top = 293
          Width = 61
          Height = 22
          Anchors = [akLeft, akBottom]
          Increment = 1
          MaxValue = 9999
          MinValue = 1
          TabOrder = 1
          Value = 9999
          OnChange = seMasterKeyLengthChange
          OnExit = seMasterKeyLengthExit
          ExplicitTop = 430
        end
      end
      object tsRNGSelect: TTabSheet
        Caption = 'RNG Select'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        object reInstructRNGSelect: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 192
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Constraints.MinHeight = 111
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            
              '    _('#39'In order to create your new DoxBox, a certain amount of r' +
              'andom data is required.'#39' +'
            
              '    SDUCRLF + SDUCRLF + '#39'This data will be used for the followin' +
              'g:'#39' + SDUCRLF +'
            '    SDUCRLF + '#39'1) Password salting'#39' + SDUCRLF +'
            '    '#39'2) The new DoxBox'#39#39's master key'#39' + SDUCRLF +'
            
              '     '#39'3) To seed pseudorandom '#39#39'chaff'#39#39' data'#39' + SDUCRLF + SDUCRL' +
              'F +'
            
              '    '#39'In order to generate this data, please select which random ' +
              'number generators you wish to use from the '
            'options below.'#39');')
          ParentFont = False
          TabOrder = 1
          ExplicitWidth = 885
          ExplicitHeight = 329
        end
        object gbRNG: TGroupBox
          Left = 0
          Top = 192
          Width = 527
          Height = 125
          Align = alBottom
          Caption = 'Random number generator (RNG)'
          TabOrder = 0
          ExplicitTop = 329
          ExplicitWidth = 885
          object ckRNGMouseMovement: TCheckBox
            Left = 16
            Top = 48
            Width = 141
            Height = 17
            Caption = 'Mouse movement'
            TabOrder = 1
            OnClick = ckRNGClick
          end
          object ckRNGCryptoAPI: TCheckBox
            Left = 16
            Top = 24
            Width = 125
            Height = 17
            Caption = 'Microsoft CryptoAPI'
            Checked = True
            State = cbChecked
            TabOrder = 0
            OnClick = ckRNGClick
          end
          object ckRNGcryptlib: TCheckBox
            Left = 16
            Top = 72
            Width = 97
            Height = 17
            Caption = 'cryptlib'
            TabOrder = 2
            OnClick = ckRNGClick
          end
          object ckRNGPKCS11: TCheckBox
            Left = 16
            Top = 96
            Width = 97
            Height = 17
            Caption = 'PKCS#11 token'
            TabOrder = 3
            OnClick = ckRNGClick
          end
          object ckRNGGPG: TCheckBox
            Left = 101
            Top = 84
            Width = 97
            Height = 17
            Caption = 'GPG - INVISIBLE CHECKBOX'
            TabOrder = 4
            Visible = False
            OnClick = ckRNGClick
          end
        end
      end
      object tsRNGMouseMovement: TTabSheet
        Caption = 'RNG Mouse movement'
        ImageIndex = 8
        Constraints.MinHeight = 236
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        object lblMouseRNGBits: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 294
          Width = 507
          Height = 13
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alBottom
          Alignment = taCenter
          Caption = 'Random bits generated: 9999/9999'
          ExplicitWidth = 169
        end
        object reInstructRNGMouseMovement: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 49
          Align = alTop
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            
              '  _('#39'You have selected mouse movement to generate random data.'#39' ' +
              '+ SDUCRLF +'
            
              '    SDUCRLF + '#39'Please "waggle" the mouse within the area below, ' +
              'until enough random data has been '
            'generated.'#39');')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
        end
        object MouseRNG: TMouseRNG
          Left = 0
          Top = 49
          Width = 527
          Height = 235
          TrailLines = 5
          LineWidth = 5
          LineColor = clNavy
          Align = alClient
          UseDockManager = False
          Enabled = False
          ParentColor = False
          OnByteGenerated = MouseRNGByteGenerated
          ExplicitWidth = 885
          ExplicitHeight = 372
        end
      end
      object tsRNGPKCS11: TTabSheet
        Caption = 'RNG PKCS token'
        ImageIndex = 17
        Constraints.MinHeight = 133
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object lblToken: TLabel
          Left = 7
          Top = 278
          Width = 34
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = '&Token:'
          FocusControl = cbToken
          ExplicitTop = 60
        end
        object cbToken: TComboBox
          Left = 47
          Top = 275
          Width = 145
          Height = 21
          Anchors = [akLeft, akBottom]
          TabOrder = 2
          Text = 'cbToken'
        end
        object pbRefresh: TButton
          Left = 198
          Top = 273
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Refresh'
          TabOrder = 1
          OnClick = pbRefreshClick
          ExplicitTop = 410
        end
        object reInstructRNGPKCS11: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 266
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            
              ' _('#39'Please select the PKCS#11 token you wish to use to generate ' +
              'random data, from the list shown below'#39');')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
          ExplicitHeight = 403
        end
      end
      object tsRNGGPG: TTabSheet
        Caption = 'RNG GPG'
        ImageIndex = 10
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        object reInstructRNGGPG: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 244
          Align = alClient
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Constraints.MinHeight = 42
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.'
            
              #39'In order to use GPG to generate random data, please specify the' +
              ' location of "gpg.exe" by clicking the browse '
            'button.'#39)
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
          ExplicitHeight = 381
        end
        object GroupBox2: TGroupBox
          Left = 0
          Top = 244
          Width = 527
          Height = 73
          Align = alBottom
          Caption = 'GPG Executable'
          TabOrder = 1
          ExplicitTop = 381
          ExplicitWidth = 885
          DesignSize = (
            527
            73)
          object lblGPGFilename: TSDUFilenameLabel
            Left = 3
            Top = 28
            Width = -283
            Height = 13
            Anchors = [akLeft, akRight, akBottom]
            Caption = 'lblGPGFilename'
            WordWrap = True
            ExplicitWidth = 75
          end
          object pbBrowseGPG: TButton
            Left = 417
            Top = 28
            Width = 75
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Browse...'
            TabOrder = 0
            OnClick = pbBrowseGPGClick
            ExplicitLeft = 775
          end
        end
      end
      object tsKeyIterations: TTabSheet
        Caption = 'Key Iterations'
        ImageIndex = 15
        Constraints.MinHeight = 168
        Constraints.MinWidth = 266
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object Label1: TLabel
          Left = 272
          Top = 601
          Width = 66
          Height = 13
          Anchors = [akRight, akBottom]
          Caption = '&Key iterations:'
          FocusControl = seKeyIterations
          ExplicitLeft = 465
          ExplicitTop = 288
        end
        object reInstructKeyIterations: TRichEdit
          Left = 3
          Top = 3
          Width = 517
          Height = 273
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Constraints.MinHeight = 107
          Lines.Strings = (
            
              'Please enter the number of PBKDF2 key iterations to be carried o' +
              'ut on your password to generate an '
            'encryption key.'
            
              'The higher the number, the greater the security offered - but th' +
              'e slower mounting will be.'
            
              'If this value is changed from its default, you will be required ' +
              'to enter it value every time you mount your '
            'volume. For this reason it is recommended that '
            
              'most users leave it at its default value, unless there is a part' +
              'icular need to do otherwise.')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 875
          ExplicitHeight = 410
        end
        object seKeyIterations: TSpinEdit64
          Left = 353
          Top = 295
          Width = 167
          Height = 22
          Anchors = [akRight, akBottom]
          Increment = 512
          MaxValue = 999999
          MinValue = 1
          TabOrder = 1
          ExplicitLeft = 711
          ExplicitTop = 432
        end
      end
      object tsSalt: TTabSheet
        Caption = 'Salt'
        ImageIndex = 16
        Constraints.MinHeight = 156
        ExplicitLeft = 8
        ExplicitTop = 22
        ExplicitWidth = 961
        ExplicitHeight = 330
        DesignSize = (
          527
          317)
        object Label8: TLabel
          Left = 4
          Top = 459
          Width = 67
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = '&Length of salt:'
          FocusControl = seSaltLength
          ExplicitTop = 217
        end
        object Label10: TLabel
          Left = 166
          Top = 459
          Width = 19
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'bits.'
          ExplicitTop = 217
        end
        object lblInstructSalt: TLabel
          Left = 3
          Top = 3
          Width = 517
          Height = 65
          Anchors = [akLeft, akTop, akRight]
          Caption = 
            'Please enter the amount of "salt" to be combined with your passw' +
            'ord to generate an encryption key. "Salting" your password incre' +
            'ases security, and will slow down dictionary attacks against you' +
            'r volume.The value entered must be a multiple of both 8 and the ' +
            'cypher'#39#39's blocksize.If this value is changed from its default, y' +
            'ou will be required to enter it value every time you mount your ' +
            'volume. For this reason it is recommended that most users leave ' +
            'it at its default value, unless there is a particular need to do' +
            ' otherwise.'
          WordWrap = True
        end
        object seSaltLength: TSpinEdit64
          Left = 443
          Top = 91
          Width = 81
          Height = 22
          Anchors = [akRight, akBottom]
          Increment = 8
          MaxValue = 9999
          TabOrder = 0
          Value = 512
          ExplicitTop = 104
        end
      end
      object tsCDBLocation: TTabSheet
        Caption = 'CDB Location'
        ImageIndex = 18
        Constraints.MinHeight = 301
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object reInstructCDBLocation: TRichEdit
          Left = 0
          Top = 0
          Width = 527
          Height = 186
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Constraints.MinHeight = 142
          Lines.Strings = (
            'WILL BE OVERWRITTEN'
            
              ' _('#39'Please specify if you would like your volume'#39#39's critical dat' +
              'a block (CDB) to be written at the start of the '
            
              'new volume, or stored in a separate keyfile.'#39' + SDUCRLF + SDUCRL' +
              'F +'
            
              ' '#39'The CDB is where metadata about your volume is stored, togethe' +
              'r with the master key used to carry out '
            
              'the actual encryption/decryption of your data.'#39' + SDUCRLF + SDUC' +
              'RLF +'
            
              ' '#39'For most users, including the CDB as part of the volume file i' +
              's probably the best option.'#39' + SDUCRLF + '
            'SDUCRLF +'
            
              '    '#39'If you store this information in a keyfile, you will need b' +
              'oth your volume file AND a keyfile in order to '
            'mount your volume.'#39' + SDUCRLF + SDUCRLF +'
            
              '    '#39'Note: Storing the CDB as part of the volume file will incre' +
              'ase your volume'#39#39's size by 512 bytes.'#39');')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 885
          ExplicitHeight = 323
        end
        object rbCDBInVolFile: TRadioButton
          Left = 14
          Top = 205
          Width = 355
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Include CDB as part of volume file'
          TabOrder = 1
          OnClick = rbCDBLocationClick2
          ExplicitTop = 342
        end
        object rbCDBInKeyfile: TRadioButton
          Left = 14
          Top = 228
          Width = 355
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Store CDB as separate keyfile'
          TabOrder = 2
          OnClick = rbCDBLocationClick2
          ExplicitTop = 365
        end
        object gbKeyfile: TGroupBox
          Left = 14
          Top = 251
          Width = 499
          Height = 73
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Keyfile'
          TabOrder = 3
          ExplicitTop = 388
          ExplicitWidth = 857
          DesignSize = (
            499
            73)
          object lblKeyFilename: TSDUFilenameLabel
            Left = 3
            Top = 18
            Width = 321
            Height = 37
            AutoSize = False
            Caption = 'lblKeyFilename'
            WordWrap = True
          end
          object pbBrowseKeyfile: TButton
            Left = 380
            Top = 24
            Width = 75
            Height = 25
            Anchors = [akRight, akBottom]
            Caption = '&Browse...'
            TabOrder = 0
            OnClick = pbBrowseKeyfileClick
            ExplicitLeft = 738
          end
        end
      end
      object tsPadding: TTabSheet
        Caption = 'Padding'
        ImageIndex = 19
        Constraints.MinHeight = 188
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object Label12: TLabel
          Left = 20
          Top = 422
          Width = 143
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Amount of padding to append:'
          ExplicitTop = 296
        end
        object Label14: TLabel
          Left = 328
          Top = 422
          Width = 25
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'bytes'
          ExplicitTop = 296
        end
        object reInstructPadding: TRichEdit
          Left = 3
          Top = 0
          Width = 521
          Height = 289
          Anchors = [akLeft, akTop, akRight, akBottom]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Constraints.MinHeight = 129
          Lines.Strings = (
            'WILL BE OVERWRITTEN'
            
              '  _('#39'"Padding" is additional random data added to the end of a v' +
              'olume file.'#39'+SDUCRLF + SDUCRLF +'
            
              '    '#39'Any padding added will not be available for use as part of ' +
              'the mounted volume, and serves to increase '
            'the size of the volume.'#39' + SDUCRLF + SDUCRLF +'
            
              '     '#39'Encrypted volumes typically have a file size that is a mul' +
              'tiple of 512 bytes, or a "signature size" beyond '
            'the last 1MB boundry.'#39'+'
            
              '     '#39' To prevent this, you may wish to append random "padding" ' +
              'data to the new volume.'#39' + SDUCRLF + '
            'SDUCRLF +'
            
              '     '#39'Padding also reduces the amount of information available t' +
              'o an attacker with respect to the maximum '
            'amount of the encrypted that may '#39'+'
            '     '#39'actually be held within the volume.'#39');')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 879
          ExplicitHeight = 426
        end
        object se64Padding: TSpinEdit64
          Left = 399
          Top = 295
          Width = 121
          Height = 22
          Anchors = [akRight, akBottom]
          Increment = 1
          TabOrder = 1
          ExplicitLeft = 757
          ExplicitTop = 432
        end
      end
      object tsSummary: TTabSheet
        Caption = 'Summary'
        ImageIndex = 9
        Constraints.MinHeight = 238
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 885
        ExplicitHeight = 454
        DesignSize = (
          527
          317)
        object reSummary: TRichEdit
          Left = 4
          Top = 95
          Width = 513
          Height = 195
          Anchors = [akLeft, akTop, akRight, akBottom]
          Color = clBtnFace
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'reSummary')
          ParentFont = False
          PlainText = True
          ReadOnly = True
          ScrollBars = ssBoth
          TabOrder = 1
        end
        object reInstructSummary: TRichEdit
          Left = 0
          Top = 0
          Width = 517
          Height = 89
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          Lines.Strings = (
            'THIS TEXT WILL BE AUTOMATICALLY POPULATED.')
          ParentFont = False
          TabOrder = 0
          ExplicitWidth = 875
        end
        object ckAutoMountAfterCreate: TCheckBox
          Left = 11
          Top = 296
          Width = 309
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = '&Mount volume after creation'
          TabOrder = 2
          ExplicitTop = 433
        end
      end
    end
    inherited pnlButtons: TPanel
      Top = 380
      Width = 535
      Constraints.MinWidth = 535
      ExplicitTop = 504
      ExplicitWidth = 893
      inherited lblStage: TLabel
        Width = 165
        Anchors = [akLeft, akTop, akRight]
        ExplicitWidth = 265
      end
      inherited lblCompleteIndicator: TLabel
        Width = 95
        ExplicitWidth = 95
      end
      inherited pbNext: TButton
        Left = 281
        ExplicitLeft = 639
      end
      inherited pbBack: TButton
        Left = 197
        ExplicitLeft = 555
      end
      inherited pbFinish: TButton
        Left = 365
        ExplicitLeft = 723
      end
      inherited pbCancel: TButton
        Left = 449
        ExplicitLeft = 807
      end
      inherited pbStage: TProgressBar
        Width = 165
        Anchors = [akLeft, akTop, akRight]
        ExplicitWidth = 523
      end
    end
  end
  object SaveDialog: TSDUSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 456
    Top = 24
  end
  object GPGOpenDialog: TSDUOpenDialog
    DefaultExt = 'exe'
    FileName = 'gpg.exe'
    Filter = 'GPG (gpg.exe)|gpg.exe|Executables (*.exe)|*.exe|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 536
    Top = 32
  end
  object keySaveDialog: TSDUSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 408
    Top = 20
  end
end
