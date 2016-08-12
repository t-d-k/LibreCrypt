object frmKeyEntryFreeOTFE: TfrmKeyEntryFreeOTFE
  Left = 285
  Top = 236
  Anchors = [akLeft, akBottom]
  BorderStyle = bsSizeToolWin
  Caption = 'LibreCrypt Key Dialog'
  ClientHeight = 308
  ClientWidth = 500
  Color = clBtnFace
  Constraints.MinHeight = 342
  Constraints.MinWidth = 516
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
  object pnlButtons: TPanel
    Left = 0
    Top = 267
    Width = 500
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      500
      41)
    object pbOK: TButton
      Left = 336
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = pbOKClick
    end
    object pbCancel: TButton
      Left = 417
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = pbCancelClick
    end
  end
  object pcKey: TPageControl
    Left = 0
    Top = 0
    Width = 500
    Height = 267
    ActivePage = tsKey
    Align = alClient
    TabOrder = 1
    object tsKey: TTabSheet
      Caption = 'Key'
      DesignSize = (
        492
        239)
      object Label6: TLabel
        Left = 3
        Top = 130
        Width = 34
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '&Keyfile:'
        FocusControl = rbKeyfileFile
        ExplicitTop = 220
      end
      object lblDrive: TLabel
        Left = 98
        Top = 203
        Width = 28
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '&Drive:'
        FocusControl = cbDrive
      end
      inline frmePassword1: TfrmePassword
        Left = 3
        Top = 3
        Width = 479
        Height = 115
        Anchors = [akLeft, akTop, akRight, akBottom]
        DoubleBuffered = False
        Color = clWindow
        ParentBackground = False
        ParentColor = False
        ParentDoubleBuffered = False
        TabOrder = 0
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 479
        ExplicitHeight = 115
        inherited lblKeyPhrase: TLabel
          Left = 4
          Width = 56
          ExplicitLeft = 4
          ExplicitWidth = 56
        end
        inherited preUserKey: TPasswordRichEdit
          Left = 68
          Width = 406
          Height = 105
          ExplicitLeft = 70
          ExplicitWidth = 406
          ExplicitHeight = 109
        end
      end
      inline feKeyfile: TSDUFilenameEdit
        Left = 189
        Top = 124
        Width = 300
        Height = 38
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
        ExplicitLeft = 189
        ExplicitTop = 124
        ExplicitWidth = 300
        ExplicitHeight = 38
        DesignSize = (
          300
          38)
        inherited edFilename: TEdit
          Top = 3
          Width = 235
          Anchors = [akLeft, akRight]
          ExplicitTop = 3
          ExplicitWidth = 235
        end
        inherited pbBrowse: TButton
          Left = 244
          Top = 0
          Anchors = [akRight, akBottom]
          ExplicitLeft = 244
          ExplicitTop = 0
        end
        inherited OpenDialog1: TSDUOpenDialog
          Left = 8
        end
        inherited SaveDialog1: TSDUSaveDialog
          Left = 44
        end
      end
      object rbKeyfileFile: TRadioButton
        Left = 98
        Top = 135
        Width = 85
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = '&File:'
        TabOrder = 2
        OnClick = rbKeyfileFileClick
      end
      object cbPKCS11CDB: TComboBox
        Left = 189
        Top = 168
        Width = 128
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        TabOrder = 3
        OnChange = cbPKCS11CDBChange
      end
      object rbKeyfilePKCS11: TRadioButton
        Left = 98
        Top = 170
        Width = 85
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'PKCS#&11:'
        TabOrder = 4
        OnClick = rbKeyfilePKCS11Click
      end
      object cbDrive: TComboBox
        Left = 189
        Top = 195
        Width = 113
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        TabOrder = 5
        Items.Strings = (
          'Z:')
      end
    end
    object tsAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 1
      object gbAdvanced: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 486
        Height = 122
        Align = alTop
        Caption = 'Security'
        TabOrder = 0
        object Label2: TLabel
          Left = 10
          Top = 30
          Width = 53
          Height = 13
          Caption = '&Salt length:'
        end
        object Label5: TLabel
          Left = 289
          Top = 28
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
          Width = 144
          Height = 22
          Increment = 1
          TabOrder = 2
          OnChange = seSaltLengthChange
        end
        object seKeyIterations: TSpinEdit64
          Left = 137
          Top = 55
          Width = 144
          Height = 22
          Increment = 1
          TabOrder = 1
          OnChange = seKeyIterationsChange
        end
        object cbPKCS11SecretKey: TComboBox
          Left = 137
          Top = 83
          Width = 144
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = cbMediaTypeChange
        end
      end
      object pnlLower: TPanel
        Left = 0
        Top = 128
        Width = 492
        Height = 111
        Align = alClient
        BevelOuter = bvNone
        Constraints.MinWidth = 492
        TabOrder = 1
        object gbMountAs: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 201
          Height = 105
          Align = alLeft
          Anchors = [akLeft, akRight, akBottom]
          Caption = 'Open options'
          Constraints.MinWidth = 201
          TabOrder = 0
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
            Top = 21
            Width = 89
            Height = 21
            Style = csDropDownList
            TabOrder = 0
            OnChange = cbMediaTypeChange
          end
          object ckMountForAllUsers: TCheckBox
            Left = 12
            Top = 48
            Width = 121
            Height = 17
            Caption = 'Mount for all &users'
            TabOrder = 1
          end
          object ckMountReadonly: TCheckBox
            Left = 12
            Top = 71
            Width = 168
            Height = 17
            Caption = 'Open &readonly'
            TabOrder = 2
          end
        end
        object gbOffsetOptions: TGroupBox
          AlignWithMargins = True
          Left = 210
          Top = 3
          Width = 279
          Height = 105
          Align = alRight
          Anchors = [akRight, akBottom]
          Caption = 'Offset'
          TabOrder = 1
          DesignSize = (
            279
            105)
          object Label8: TLabel
            Left = 18
            Top = 24
            Width = 31
            Height = 13
            Caption = '&Offset:'
          end
          object ckOffsetPointsToCDB: TCheckBox
            Left = 18
            Top = 55
            Width = 165
            Height = 17
            Anchors = [akTop, akRight]
            Caption = 'Data from offset includes &CDB'
            TabOrder = 1
          end
          object se64UnitOffset: TSDUSpin64Unit_Storage
            Left = 58
            Top = 20
            Width = 210
            Height = 29
            Anchors = [akTop, akRight]
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
      end
    end
  end
  object SDUDropFiles_Keyfile: TSDUDropFiles
    Active = False
    OnFileDrop = SDUDropFiles_KeyfileFileDrop
    Left = 364
    Top = 76
  end
end
