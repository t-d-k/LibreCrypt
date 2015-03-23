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
    ExplicitWidth = 537
    ExplicitHeight = 422
    inherited bvlLine: TBevel
      Top = 359
      Width = 535
      ExplicitTop = 333
      ExplicitWidth = 725
    end
    inherited pcWizard: TPageControl
      Width = 535
      Height = 358
      ActivePage = tsFileOrPartition
      ExplicitWidth = 535
      ExplicitHeight = 358
      object tsFileOrPartition: TTabSheet
        Caption = 'Volume File or Partition'
        ImageIndex = 14
        Constraints.MinHeight = 150
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
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
        object lblInstructFileOrPartition1: TLabel
          Left = 0
          Top = 40
          Width = 527
          Height = 13
          Align = alTop
          Caption = 
            'This wizard will guide you through the process of creating a new' +
            ' DoxBox, or a hidden box within an existing box.'
          WordWrap = True
          ExplicitWidth = 526
        end
        object lblInstructFileOrPartition3: TLabel
          Left = 0
          Top = 79
          Width = 527
          Height = 13
          Align = alTop
          Caption = 
            'Normally you should use a file; encrypted partitions or  disks m' +
            'ay give greater obscurity or speed.'
          WordWrap = True
          ExplicitWidth = 452
        end
        object lblInstructFileOrPartition2: TLabel
          Left = 0
          Top = 53
          Width = 527
          Height = 26
          Align = alTop
          Caption = 
            'Firstly, please specify whether the new DoxBox should be created' +
            ' as a file stored on your disk, or take up a disk partition.'
          WordWrap = True
          ExplicitWidth = 525
        end
        object rgFileOrPartition: TRadioGroup
          Left = 10
          Top = 207
          Width = 514
          Height = 87
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'File or Partition/Entire Disk'
          Items.Strings = (
            '1st of two'
            '2nd of two')
          TabOrder = 0
          OnClick = rgFileOrPartitionClick
        end
      end
      object tsFilename: TTabSheet
        Caption = 'Volume filename'
        ImageIndex = 1
        Constraints.MinHeight = 139
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object reInstructFilename1: TLabel
          Left = 0
          Top = 0
          Width = 368
          Height = 13
          Align = alTop
          Caption = 
            'Please specify the filename of the new DoxBox, by clicking the b' +
            'rowse button.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructFilename2: TLabel
          Left = 0
          Top = 13
          Width = 425
          Height = 13
          Align = alTop
          Caption = 
            'If you wish to create a hidden DoxBox, please specify your exist' +
            'ing DoxBox file to be used.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object GroupBox1: TGroupBox
          Left = 0
          Top = 263
          Width = 527
          Height = 67
          Align = alBottom
          Caption = 'Volume filename'
          TabOrder = 0
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
          end
          object lblFilename: TEdit
            Left = 16
            Top = 24
            Width = 414
            Height = 21
            Anchors = [akLeft, akRight, akBottom]
            TabOrder = 1
            Text = 'lblFilename'
          end
        end
      end
      object tsPartitionWarning: TTabSheet
        Caption = 'Partition Warning'
        ImageIndex = 16
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object Label15: TLabel
          Left = 0
          Top = 304
          Width = 526
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
        end
        object lblWarningPartition: TLabel
          Left = 0
          Top = 0
          Width = 61
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
        end
        object reInstructPartitionWarning1: TLabel
          Left = 0
          Top = 13
          Width = 314
          Height = 13
          Align = alTop
          Caption = '1) Creating encrypted partitions is POTENTIALLY DESTRUCTIVE.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructPartitionWarning2: TLabel
          Left = 0
          Top = 26
          Width = 492
          Height = 13
          Align = alTop
          Caption = 
            '2) Data stored on the partition you select in the next stage may' +
            ' be OVERWRITTEN with encrypted data.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructPartitionWarning3: TLabel
          Left = 0
          Top = 39
          Width = 402
          Height = 13
          Align = alTop
          Caption = 
            '3) It is RECOMMENDED that you backup your data as appropriate be' +
            'fore continuing.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
      end
      object tsPartitionSelect: TTabSheet
        Caption = 'Partition Select'
        ImageIndex = 15
        object Label21: TLabel
          AlignWithMargins = True
          Left = 5
          Top = 31
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
        object reInstructPartitionSelect: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 26
          Align = alTop
          Caption = 
            'Please select the partition you wish to create the new DoxBox on' +
            ', and whether you would like to create a "hidden" DoxBox.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 498
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
        end
        inline fmeSelectPartition: TfmeSelectPartition
          Left = 0
          Top = 49
          Width = 527
          Height = 254
          Align = alClient
          Constraints.MinHeight = 151
          TabOrder = 1
          ExplicitTop = 49
          ExplicitWidth = 527
          ExplicitHeight = 254
          inherited lblErrorWarning: TLabel
            Top = 171
            ExplicitTop = 125
          end
          inherited imgErrorWarning: TImage
            Top = 169
            ExplicitTop = 123
          end
          inherited TabControl1: TTabControl
            Width = 527
            Height = 228
            Constraints.MinHeight = 132
            ExplicitWidth = 527
            ExplicitHeight = 228
            inherited SDUDiskPartitionsPanel1: TOTFEFreeOTFEDiskPartitionsPanel
              Height = 200
              ExplicitHeight = 200
            end
            inherited pnlNoPartitionDisplay: TPanel
              Left = 338
              Height = 200
              ExplicitLeft = 338
              ExplicitHeight = 200
            end
          end
          inherited ckShowCDROM: TCheckBox
            Left = 279
            Top = 234
            ExplicitLeft = 279
            ExplicitTop = 234
          end
          inherited ckEntireDisk: TCheckBox
            Left = 442
            Top = 234
            ExplicitLeft = 442
            ExplicitTop = 234
          end
          inherited ilErrorWarning: TImageList
            Left = 368
            Top = 65533
            Bitmap = {
              494C010103000400340110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
        ExplicitWidth = 0
        ExplicitHeight = 0
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
        object reInstructSize: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 108
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 
            'Please enter the required size of the new DoxBox.'#10#13'This value mu' +
            'st be more than 1 MB, and must be a multiple of 512 bytes.'#10#13'If y' +
            'ou are creating a hidden DoxBox, then this is the size of the hi' +
            'dden Box.'#10#13'Please see the help for minium sizes of hidden boxes'#10 +
            #13'Note: The size you give here will be the amount available for y' +
            'our data. The actual file created will be slightly larger than t' +
            'his.'
          Constraints.MinHeight = 108
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 512
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
        end
      end
      object tsPassword: TTabSheet
        Caption = 'Password'
        ImageIndex = 6
        Constraints.MinHeight = 330
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
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
        object reInstructPassword: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 78
          Align = alTop
          Caption = 
            'Please enter the keyphrase to be used for securing your DoxBox. ' +
            #10#13'Try to enter one character for each bit of the cypher keysize.' +
            ' For example for a 256 bit cypher enter a 256 character keyphras' +
            'e.'#10#13'Note: If you forget your keyphrase, you can forget your data' +
            '.'#10#13'Note: Newlines (blank lines) are significant. It is recommend' +
            'ed that you do not press <ENTER> after typing in your keyphrase,' +
            ' as this will add an extra newline to the end of your keyphrase.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 514
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
          TabOrder = 0
          OnChange = preUserKeyChange
        end
      end
      object tsDriveLetter: TTabSheet
        Caption = 'Drive Letter'
        ImageIndex = 17
        Constraints.MinHeight = 166
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object Label11: TLabel
          Left = 313
          Top = 279
          Width = 107
          Height = 13
          Anchors = [akRight, akBottom]
          Caption = '&Requested drive letter:'
          FocusControl = cbDriveLetter
          ExplicitLeft = 414
          ExplicitTop = 287
        end
        object lblInstructDriveLetter2: TLabel
          Left = 0
          Top = 19
          Width = 341
          Height = 13
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 
            'Note: You will be able to change this after the volume has been ' +
            'created.'
          WordWrap = True
        end
        object lblInstructDriveLetter1: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 13
          Align = alTop
          Caption = 
            'Please enter the drive letter you would normally like this volum' +
            'e mounted under.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 385
        end
        object cbDriveLetter: TComboBox
          Left = 435
          Top = 276
          Width = 89
          Height = 21
          Style = csDropDownList
          Anchors = [akRight, akBottom]
          TabOrder = 0
          OnChange = cbDriveLetterChange
        end
      end
      object tsOffset: TTabSheet
        Caption = 'Offset'
        ImageIndex = 7
        Constraints.MinHeight = 330
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object Label7: TLabel
          Left = 137
          Top = 303
          Width = 120
          Height = 13
          Anchors = [akRight, akBottom]
          Caption = '&Byte offset within volume:'
          ExplicitTop = 319
        end
        object reInstructOffset: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 52
          Align = alTop
          Caption = 
            ' The DoxBox file you specified already exists. To create a hidde' +
            'n DoxBox within this file, please specify the byte offset within' +
            ' this file from where the new DoxBox should begin.'#10#13'If you do NO' +
            'T wish to create a hidden DoxBox within your existing file, plea' +
            'se click "< Back" and enter a different filename.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitTop = -2
        end
        object pnlWarningOffset: TPanel
          Left = 0
          Top = 52
          Width = 527
          Height = 405
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'pnlWarningOffset'
          Constraints.MinHeight = 405
          TabOrder = 1
          DesignSize = (
            527
            405)
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
            Top = 164
            Width = 463
            Height = 26
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
          object reInstructWarningOffset1: TLabel
            Left = 1
            Top = 34
            Width = 525
            Height = 13
            Align = alTop
            Caption = '1) Creating hidden Boxes is POTENTIALLY DESTRUCTIVE.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            ExplicitWidth = 286
          end
          object reInstructWarningOffset2: TLabel
            Left = 1
            Top = 47
            Width = 525
            Height = 26
            Align = alTop
            Caption = 
              '2) Data within the file/partition specified in the previous stag' +
              'e may be OVERWRITTEN, starting from byte offset specified, and r' +
              'unning for the full length of the new hidden DoxBox. '
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            ExplicitWidth = 523
          end
          object reInstructWarningOffset4: TLabel
            Left = 1
            Top = 99
            Width = 525
            Height = 39
            Align = alTop
            Caption = 
              #10#13'For security reasons, this information is not stored anywhere;' +
              ' you will need to type in this offset whenever  you wish to moun' +
              't your hidden DoxBox.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            ExplicitWidth = 516
          end
          object reInstructWarningOffset3: TLabel
            Left = 1
            Top = 73
            Width = 525
            Height = 26
            Align = alTop
            Caption = 
              #10#13'3) If you ae not using the default offset, remember the offset' +
              ' entered! If in doubt use the '#39#39'Default Offset" value.'
            Font.Charset = ANSI_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
            WordWrap = True
            ExplicitWidth = 519
          end
        end
        object se64UnitByteOffset: TSDUSpin64Unit_Storage
          Left = 301
          Top = 299
          Width = 215
          Height = 25
          Anchors = [akRight, akBottom]
          TabOrder = 0
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
        end
      end
      object tsHashCypherIV: TTabSheet
        Caption = 'Hash, cypher and IV'
        ImageIndex = 2
        Constraints.MinHeight = 202
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object Label4: TLabel
          Left = 17
          Top = 234
          Width = 59
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Select &hash:'
          FocusControl = cbHash
          ExplicitTop = 208
        end
        object Label5: TLabel
          Left = 17
          Top = 262
          Width = 68
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Select &cypher:'
          FocusControl = cbCypher
          ExplicitTop = 236
        end
        object lblSectorIVGenMethod: TLabel
          Left = 17
          Top = 290
          Width = 52
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Sector &IVs:'
          FocusControl = cbSectorIVGenMethod
          ExplicitTop = 264
        end
        object reInstructHashCypherIV: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 13
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 
            'Please select the advanced security options to use in securing y' +
            'our new DoxBox.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 383
        end
        object cbHash: TComboBox
          Left = 153
          Top = 230
          Width = 145
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akBottom]
          TabOrder = 0
          OnChange = cbHashCypherIVGenChange
        end
        object cbCypher: TComboBox
          Left = 153
          Top = 258
          Width = 145
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akBottom]
          TabOrder = 2
          OnChange = cbHashCypherIVGenChange
        end
        object pbHashInfo: TButton
          Left = 304
          Top = 231
          Width = 21
          Height = 21
          Anchors = [akLeft, akBottom]
          Caption = '?'
          TabOrder = 1
          OnClick = pbHashInfoClick
        end
        object pbCypherInfo: TButton
          Left = 304
          Top = 258
          Width = 21
          Height = 21
          Anchors = [akLeft, akBottom]
          Caption = '?'
          TabOrder = 4
          OnClick = pbCypherInfoClick
        end
        object ckUsePerVolumeIV: TCheckBox
          Left = 153
          Top = 310
          Width = 260
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Additionally &XOR sector IV with per volume IV'
          TabOrder = 3
        end
        object cbSectorIVGenMethod: TComboBox
          Left = 153
          Top = 286
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
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object lblInstructChaff1: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 13
          Align = alTop
          Caption = 
            'Choose how to overwrite the file or partition the DoxBox is stor' +
            'ed on.'
          WordWrap = True
          ExplicitWidth = 321
        end
        object lblInstructChaff4: TLabel
          Left = 0
          Top = 39
          Width = 527
          Height = 26
          Align = alTop
          Caption = 
            #39'Zeros'#39' will make it easy for an attacker to tell if a hidden bo' +
            'x has been added and the amount of data stored in your box, but ' +
            'allow the box to be created faster.'
          WordWrap = True
          ExplicitWidth = 521
        end
        object lblInstructChaff3: TLabel
          Left = 0
          Top = 65
          Width = 527
          Height = 26
          Align = alTop
          Caption = 
            'It will also make it hard to tell the exact size of your data. O' +
            'verwriting with secure data will take longer than with zeros.'
          WordWrap = True
          ExplicitWidth = 523
        end
        object lblInstructChaff2: TLabel
          Left = 0
          Top = 13
          Width = 527
          Height = 26
          Align = alTop
          Caption = 
            'Secure data will make it harder - perhaps impossible - for an at' +
            'tacker to tell if a '#39'hidden'#39' box has been added at a later date.'
          WordWrap = True
          ExplicitWidth = 526
        end
        object rgOverwriteType: TRadioGroup
          Left = 3
          Top = 233
          Width = 521
          Height = 88
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Type of overwrite data'
          Items.Strings = (
            'Secure pseudorandom data'
            'Zeros')
          TabOrder = 0
        end
      end
      object tsMasterKeyLength: TTabSheet
        Caption = 'Master Key Length'
        ImageIndex = 11
        Constraints.MinHeight = 77
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object Label2: TLabel
          Left = 17
          Top = 302
          Width = 89
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = '&Size of master key:'
        end
        object Label9: TLabel
          Left = 204
          Top = 302
          Width = 16
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'bits'
        end
        object reInstructMasterKeyLen: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 26
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 
            'The cypher you selected supports arbitary key lengths.Please spe' +
            'cify the length of the master key which will be used to encrypt/' +
            'decrypt your data.Note: The value entered must be a multiple of ' +
            '8.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 525
        end
        object seMasterKeyLength: TSpinEdit64
          Left = 137
          Top = 297
          Width = 61
          Height = 22
          Anchors = [akLeft, akBottom]
          Increment = 1
          MaxValue = 9999
          MinValue = 1
          TabOrder = 0
          Value = 9999
          OnChange = seMasterKeyLengthChange
          OnExit = seMasterKeyLengthExit
        end
      end
      object tsRNGSelect: TTabSheet
        Caption = 'RNG Select'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object reInstructRNGSelect1: TLabel
          Left = 0
          Top = 0
          Width = 521
          Height = 26
          Align = alTop
          Caption = 
            'In order to create your new DoxBox, a certain amount of random d' +
            'ata is required. This data will be used for the following:'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructRNGSelect2: TLabel
          Left = 0
          Top = 26
          Width = 91
          Height = 13
          Align = alTop
          Caption = '1) Password salting'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructRNGSelect4: TLabel
          Left = 0
          Top = 52
          Width = 183
          Height = 13
          Align = alTop
          Caption = '3) To seed pseudorandom '#39#39'chaff'#39#39' data'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructRNGSelect3: TLabel
          Left = 0
          Top = 39
          Width = 157
          Height = 13
          Align = alTop
          Caption = '2) The new DoxBox'#39#39's master key'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructRNGSelect5: TLabel
          Left = 0
          Top = 65
          Width = 521
          Height = 26
          Align = alTop
          Caption = 
            'In order to generate this data, please select which random numbe' +
            'r generators you wish to use from the options below.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object gbRNG: TGroupBox
          Left = 0
          Top = 205
          Width = 527
          Height = 125
          Align = alBottom
          Caption = 'Random number generator (RNG)'
          TabOrder = 0
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
        ExplicitWidth = 0
        ExplicitHeight = 0
        object lblMouseRNGBits: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 307
          Width = 169
          Height = 13
          Margins.Left = 10
          Margins.Top = 10
          Margins.Right = 10
          Margins.Bottom = 10
          Align = alBottom
          Alignment = taCenter
          Caption = 'Random bits generated: 9999/9999'
        end
        object reInstructRNGMouseMovement1: TLabel
          Left = 0
          Top = 0
          Width = 297
          Height = 13
          Align = alTop
          Caption = 'You have selected mouse movement to generate random data.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object reInstructRNGMouseMovement2: TLabel
          Left = 0
          Top = 13
          Width = 461
          Height = 13
          Align = alTop
          Caption = 
            'Please "waggle" the mouse within the area below, until enough ra' +
            'ndom data has been generated.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object MouseRNG: TMouseRNG
          Left = 0
          Top = 26
          Width = 527
          Height = 271
          TrailLines = 5
          LineWidth = 5
          LineColor = clNavy
          Align = alClient
          UseDockManager = False
          Enabled = False
          ParentColor = False
          OnByteGenerated = MouseRNGByteGenerated
        end
      end
      object tsRNGPKCS11: TTabSheet
        Caption = 'RNG PKCS token'
        ImageIndex = 17
        Constraints.MinHeight = 133
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object lblToken: TLabel
          Left = 7
          Top = 291
          Width = 34
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = '&Token:'
          FocusControl = cbToken
          ExplicitTop = 60
        end
        object reInstructRNGPKCS11: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 13
          Align = alTop
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 
            'Please select the PKCS#11 token you wish to use to generate rand' +
            'om data, from the list shown below'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          ExplicitWidth = 480
        end
        object cbToken: TComboBox
          Left = 47
          Top = 288
          Width = 145
          Height = 21
          Anchors = [akLeft, akBottom]
          TabOrder = 0
          Text = 'cbToken'
        end
        object pbRefresh: TButton
          Left = 198
          Top = 286
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Refresh'
          TabOrder = 1
          OnClick = pbRefreshClick
        end
      end
      object tsRNGGPG: TTabSheet
        Caption = 'RNG GPG'
        ImageIndex = 10
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object reInstructRNGGPG: TLabel
          Left = 0
          Top = 0
          Width = 518
          Height = 42
          Align = alClient
          Caption = 
            'In order to use GPG to generate random data, please specify the ' +
            'location of "gpg.exe" by clicking the browse button.'
          Constraints.MinHeight = 42
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object GroupBox2: TGroupBox
          Left = 0
          Top = 257
          Width = 527
          Height = 73
          Align = alBottom
          Caption = 'GPG Executable'
          TabOrder = 0
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
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object Label1: TLabel
          Left = 272
          Top = 614
          Width = 66
          Height = 13
          Anchors = [akRight, akBottom]
          Caption = '&Key iterations:'
          FocusControl = seKeyIterations
          ExplicitLeft = 465
          ExplicitTop = 288
        end
        object reInstructKeyIterations1: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 26
          Align = alTop
          Caption = 
            'Please enter the number of PBKDF2 key iterations to be carried o' +
            'ut on your password to generate an encryption key.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 489
        end
        object reInstructKeyIterations3: TLabel
          Left = 0
          Top = 52
          Width = 527
          Height = 39
          Align = alTop
          Caption = 
            'If this value is changed from its default, you will be required ' +
            'to enter it value every time you mount your volume. For this rea' +
            'son it is recommended that most users leave it at its default va' +
            'lue, unless there is a particular need to do otherwise.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 503
        end
        object reInstructKeyIterations2: TLabel
          Left = 0
          Top = 26
          Width = 527
          Height = 26
          Align = alTop
          Caption = 
            'The higher the number, the more secure if a weak password has be' +
            'en chosen - but the slower it will be to open a DoxBox.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 509
        end
        object seKeyIterations: TSpinEdit64
          Left = 353
          Top = 308
          Width = 167
          Height = 22
          Anchors = [akRight, akBottom]
          Increment = 512
          MaxValue = 999999
          MinValue = 1
          TabOrder = 0
        end
      end
      object tsSalt: TTabSheet
        Caption = 'Salt'
        ImageIndex = 16
        Constraints.MinHeight = 156
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object Label8: TLabel
          Left = 4
          Top = 472
          Width = 67
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = '&Length of salt:'
          FocusControl = seSaltLength
          ExplicitTop = 217
        end
        object Label10: TLabel
          Left = 166
          Top = 472
          Width = 19
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'bits.'
          ExplicitTop = 217
        end
        object lblInstructSalt1: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 13
          Align = alTop
          Caption = 
            'Please enter the amount of "salt" to be combined with your passw' +
            'ord to generate an encryption key.'
          WordWrap = True
          ExplicitWidth = 472
        end
        object lblInstructSalt4: TLabel
          Left = 0
          Top = 52
          Width = 527
          Height = 39
          Align = alTop
          Caption = 
            'If this value is changed from its default, you will be required ' +
            'to enter it value every time you mount your volume. For this rea' +
            'son it is recommended that most users leave it at its default va' +
            'lue, unless there is a particular need to do otherwise.'
          WordWrap = True
          ExplicitWidth = 525
        end
        object lblInstructSalt3: TLabel
          Left = 0
          Top = 26
          Width = 527
          Height = 26
          Align = alTop
          Caption = 
            'The value entered must be a multiple of both 8 and the cypher'#39#39's' +
            ' blocksize. If this value is changed from its default, you will ' +
            'be required to enter it value every time you mount your volume.'
          WordWrap = True
          ExplicitWidth = 503
        end
        object lblInstructSalt2: TLabel
          Left = 0
          Top = 13
          Width = 527
          Height = 13
          Align = alTop
          Caption = 
            '"Salting" your password increases security, and will slow down d' +
            'ictionary attacks against your volume.'
          WordWrap = True
          ExplicitWidth = 480
        end
        object seSaltLength: TSpinEdit64
          Left = 443
          Top = 104
          Width = 81
          Height = 22
          Anchors = [akRight, akBottom]
          Increment = 8
          MaxValue = 9999
          TabOrder = 0
          Value = 512
        end
      end
      object tsCDBLocation: TTabSheet
        Caption = 'CDB Location'
        ImageIndex = 18
        Constraints.MinHeight = 301
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object reInstructCDBLocation: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 142
          Align = alTop
          Caption = 
            'Please specify if you would like your volume'#39#39's critical data bl' +
            'ock (CDB) to be written at the start of the new volume, or store' +
            'd in a separate keyfile.'#10#13'The CDB is where metadata about your v' +
            'olume is stored, together with the master key used to carry out ' +
            'the actual encryption/decryption of your data.'#10#13'For most users, ' +
            'including the CDB as part of the volume file is probably the bes' +
            't option.'#10#13'If you store this information in a keyfile, you will ' +
            'need both your volume file AND a keyfile in order to mount your ' +
            'volume.'#10#13'Note: Storing the CDB as part of the volume file will i' +
            'ncrease your volume'#39#39's size by 512 bytes.'
          Constraints.MinHeight = 142
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 525
        end
        object rbCDBInVolFile: TRadioButton
          Left = 14
          Top = 218
          Width = 355
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Include CDB as part of volume file'
          TabOrder = 0
          OnClick = rbCDBLocationClick2
        end
        object rbCDBInKeyfile: TRadioButton
          Left = 14
          Top = 241
          Width = 355
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = 'Store CDB as separate keyfile'
          TabOrder = 2
          OnClick = rbCDBLocationClick2
        end
        object gbKeyfile: TGroupBox
          Left = 14
          Top = 264
          Width = 499
          Height = 73
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Keyfile'
          TabOrder = 1
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
          end
        end
      end
      object tsPadding: TTabSheet
        Caption = 'Padding'
        ImageIndex = 19
        Constraints.MinHeight = 188
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object Label12: TLabel
          Left = 20
          Top = 435
          Width = 143
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'Amount of padding to append:'
          ExplicitTop = 296
        end
        object Label14: TLabel
          Left = 328
          Top = 435
          Width = 25
          Height = 13
          Anchors = [akLeft, akBottom]
          Caption = 'bytes'
          ExplicitTop = 296
        end
        object reInstructPadding: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 130
          Align = alTop
          Caption = 
            '"Padding" is additional random data added to the end of a volume' +
            ' file.'#10#13#10#13'Any padding added will not be available for use as par' +
            't of the mounted volume, and serves to increase the size of the ' +
            'volume.'#10#13#10#13'Encrypted volumes typically have a file size that is ' +
            'a multiple of 512 bytes, or a "signature size" beyond the last 1' +
            'MB boundry.'#10#13'To prevent this, you may wish to append random "pad' +
            'ding" data to the new volume.'#10#13'Padding also reduces the amount o' +
            'f information available to an attacker with respect to the maxim' +
            'um amount of the encrypted that may actually be held within the ' +
            'volume.'
          Constraints.MinHeight = 129
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 525
        end
        object se64Padding: TSpinEdit64
          Left = 399
          Top = 308
          Width = 121
          Height = 22
          Anchors = [akRight, akBottom]
          Increment = 1
          TabOrder = 0
        end
      end
      object tsSummary: TTabSheet
        Caption = 'Summary'
        ImageIndex = 9
        Constraints.MinHeight = 238
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          527
          330)
        object reInstructSummary: TLabel
          Left = 0
          Top = 0
          Width = 527
          Height = 39
          Align = alTop
          Caption = 
            'You have now entered all the information required to create a ne' +
            'w DoxBox.'#10#13'Please check the summary shown below and click "Finis' +
            'h" to create the new DoxBox, or use the "Back"/"Next" buttons to' +
            ' modify the details you have entered.'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          WordWrap = True
          ExplicitWidth = 460
        end
        object reSummary: TRichEdit
          Left = 4
          Top = 95
          Width = 513
          Height = 208
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
        object ckAutoMountAfterCreate: TCheckBox
          Left = 11
          Top = 309
          Width = 309
          Height = 17
          Anchors = [akLeft, akBottom]
          Caption = '&Mount volume after creation'
          TabOrder = 0
        end
      end
    end
    inherited pnlButtons: TPanel
      Top = 380
      Width = 535
      Constraints.MinWidth = 535
      ExplicitTop = 380
      ExplicitWidth = 535
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
        ExplicitLeft = 281
      end
      inherited pbBack: TButton
        Left = 197
        ExplicitLeft = 197
      end
      inherited pbFinish: TButton
        Left = 365
        ExplicitLeft = 365
      end
      inherited pbCancel: TButton
        Left = 449
        ExplicitLeft = 449
      end
      inherited pbStage: TProgressBar
        Width = 165
        Anchors = [akLeft, akTop, akRight]
        ExplicitWidth = 165
      end
    end
  end
  object SaveDialog: TSDUSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 216
    Top = 208
  end
  object GPGOpenDialog: TSDUOpenDialog
    DefaultExt = 'exe'
    FileName = 'gpg.exe'
    Filter = 'GPG (gpg.exe)|gpg.exe|Executables (*.exe)|*.exe|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 488
    Top = 240
  end
  object keySaveDialog: TSDUSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 400
    Top = 244
  end
end
