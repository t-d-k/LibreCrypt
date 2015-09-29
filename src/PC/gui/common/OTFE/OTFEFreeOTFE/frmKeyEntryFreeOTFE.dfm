object frmKeyEntryFreeOTFE: TfrmKeyEntryFreeOTFE
  Left = 285
  Top = 236
  BorderStyle = bsDialog
  Caption = 'LibreCrypt Key Dialog'
  ClientHeight = 481
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBasic: TPanel
    Left = 0
    Top = 0
    Width = 570
    Height = 210
    Align = alTop
    Caption = 'pnlBasic'
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 12
      Top = 4
      Width = 545
      Height = 200
      Caption = 'Security details'
      TabOrder = 0
      object Label6: TLabel
        Left = 12
        Top = 83
        Width = 34
        Height = 13
        Caption = '&Keyfile:'
        FocusControl = rbKeyfileFile
      end
      object lblDrive: TLabel
        Left = 12
        Top = 146
        Width = 28
        Height = 13
        Caption = '&Drive:'
        FocusControl = cbDrive
      end
      object cbDrive: TComboBox
        Left = 100
        Top = 143
        Width = 113
        Height = 21
        Style = csDropDownList
        TabOrder = 4
        Items.Strings = (
          'Z:')
      end
      object ckMountReadonly: TCheckBox
        Left = 100
        Top = 170
        Width = 293
        Height = 17
        Caption = 'Open &readonly'
        TabOrder = 5
      end
      object cbPKCS11CDB: TComboBox
        Left = 191
        Top = 116
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 3
        OnChange = cbPKCS11CDBChange
      end
      object rbKeyfileFile: TRadioButton
        Left = 101
        Top = 83
        Width = 85
        Height = 17
        Caption = '&File:'
        TabOrder = 0
        OnClick = rbKeyfileFileClick
      end
      object rbKeyfilePKCS11: TRadioButton
        Left = 100
        Top = 120
        Width = 85
        Height = 17
        Caption = 'PKCS#&11:'
        TabOrder = 2
        OnClick = rbKeyfilePKCS11Click
      end
      inline feKeyfile: TSDUFilenameEdit
        Left = 176
        Top = 81
        Width = 353
        Height = 29
        TabOrder = 1
        ExplicitLeft = 176
        ExplicitTop = 81
        ExplicitWidth = 353
        ExplicitHeight = 29
        DesignSize = (
          353
          29)
        inherited edFilename: TEdit
          Width = 208
          ExplicitWidth = 208
        end
        inherited pbBrowse: TButton
          Left = 230
          Top = 2
          ExplicitLeft = 230
          ExplicitTop = 2
        end
        inherited OpenDialog1: TSDUOpenDialog
          Left = 104
          Top = 0
        end
        inherited SaveDialog1: TSDUSaveDialog
          Left = 140
          Top = 0
        end
      end
      inline frmePassword1: TfrmePassword
        Left = 3
        Top = 20
        Width = 539
        Height = 57
        TabOrder = 6
        ExplicitLeft = 3
        ExplicitTop = 20
        ExplicitWidth = 539
        ExplicitHeight = 57
        inherited lblKeyPhrase: TLabel
          Top = 0
          Width = 56
          Anchors = [akLeft, akTop, akRight]
          ExplicitTop = 0
          ExplicitWidth = 56
        end
        inherited mmShown: TMemo
          Left = 84
          Top = 1
          Width = 442
          Height = 53
          ExplicitLeft = 84
          ExplicitTop = 1
          ExplicitWidth = 442
          ExplicitHeight = 53
        end
      end
    end
  end
  object pnlLower: TPanel
    Left = 0
    Top = 210
    Width = 570
    Height = 271
    Align = alClient
    Caption = 'pnlLower'
    TabOrder = 1
    object pnlButtons: TPanel
      Left = 1
      Top = 226
      Width = 568
      Height = 44
      Align = alClient
      Caption = 'pnlButtons'
      TabOrder = 1
      object pbCancel: TButton
        Left = 481
        Top = 6
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        TabOrder = 2
        OnClick = pbCancelClick
      end
      object pbOK: TButton
        Left = 396
        Top = 6
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 1
        OnClick = pbOKClick
      end
      object pbAdvanced: TButton
        Left = 10
        Top = 6
        Width = 75
        Height = 25
        Caption = 'Advanced >>'
        TabOrder = 0
        OnClick = pbAdvancedClick
      end
    end
    object pnlAdvanced: TPanel
      Left = 1
      Top = 1
      Width = 568
      Height = 225
      Align = alTop
      Caption = 'pnlAdvanced'
      TabOrder = 0
      object gbVolumeOptions: TGroupBox
        Left = 238
        Top = 137
        Width = 319
        Height = 77
        Caption = 'Container options'
        TabOrder = 2
        object Label8: TLabel
          Left = 12
          Top = 24
          Width = 31
          Height = 13
          Caption = '&Offset:'
        end
        object ckOffsetPointsToCDB: TCheckBox
          Left = 96
          Top = 48
          Width = 165
          Height = 17
          Caption = 'Data from offset includes &CDB'
          TabOrder = 1
        end
        object se64UnitOffset: TSDUSpin64Unit_Storage
          Left = 96
          Top = 20
          Width = 217
          Height = 29
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
        end
      end
      object gbMountAs: TGroupBox
        Left = 10
        Top = 137
        Width = 215
        Height = 77
        Caption = 'Open options'
        TabOrder = 1
        object Label9: TLabel
          Left = 12
          Top = 24
          Width = 43
          Height = 13
          Caption = '&Open as:'
          FocusControl = cbMediaType
        end
        object cbMediaType: TComboBox
          Left = 88
          Top = 20
          Width = 113
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = cbMediaTypeChange
        end
        object ckMountForAllUsers: TCheckBox
          Left = 88
          Top = 48
          Width = 121
          Height = 17
          Caption = 'Mount for all &users'
          TabOrder = 1
        end
      end
      object GroupBox3: TGroupBox
        Left = 10
        Top = 4
        Width = 547
        Height = 122
        Caption = 'Advanced security details'
        TabOrder = 0
        object Label2: TLabel
          Left = 10
          Top = 30
          Width = 53
          Height = 13
          Caption = '&Salt length:'
        end
        object Label5: TLabel
          Left = 257
          Top = 30
          Width = 16
          Height = 13
          Caption = 'bits'
        end
        object Label7: TLabel
          Left = 10
          Top = 60
          Width = 66
          Height = 13
          Caption = 'Key &iterations:'
        end
        object Label10: TLabel
          Left = 10
          Top = 90
          Width = 102
          Height = 13
          Caption = 'PKCS#&11 secret key:'
          FocusControl = cbPKCS11SecretKey
        end
        object seSaltLength: TSpinEdit64
          Left = 137
          Top = 25
          Width = 112
          Height = 22
          Increment = 1
          TabOrder = 2
          OnChange = seSaltLengthChange
        end
        object seKeyIterations: TSpinEdit64
          Left = 137
          Top = 55
          Width = 112
          Height = 22
          Increment = 1
          TabOrder = 1
          OnChange = seKeyIterationsChange
        end
        object cbPKCS11SecretKey: TComboBox
          Left = 137
          Top = 85
          Width = 112
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = cbMediaTypeChange
        end
      end
    end
  end
  object SDUDropFiles_Keyfile: TSDUDropFiles
    Active = False
    OnFileDrop = SDUDropFiles_KeyfileFileDrop
    Left = 468
    Top = 116
  end
end
