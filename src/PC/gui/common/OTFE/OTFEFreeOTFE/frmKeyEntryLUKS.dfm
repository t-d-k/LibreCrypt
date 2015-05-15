object frmKeyEntryLUKS: TfrmKeyEntryLUKS
  Left = 388
  Top = 281
  BorderStyle = bsDialog
  Caption = 'Key Phrase For LUKS Container'
  ClientHeight = 399
  ClientWidth = 548
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pbCancel: TButton
    Left = 280
    Top = 360
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = pbCancelClick
  end
  object pbOK: TButton
    Left = 192
    Top = 360
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = pbOKClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 220
    Width = 214
    Height = 125
    Caption = 'Mount options'
    TabOrder = 1
    object lblDrive: TLabel
      Left = 12
      Top = 24
      Width = 28
      Height = 13
      Caption = '&Drive:'
      FocusControl = cbDrive
    end
    object lblMountAs: TLabel
      Left = 12
      Top = 52
      Width = 47
      Height = 13
      Caption = '&Mount as:'
      FocusControl = cbMediaType
    end
    object cbDrive: TComboBox
      Left = 88
      Top = 20
      Width = 113
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = SelectionChange
      Items.Strings = (
        'Z:')
    end
    object ckMountReadonly: TSDUCheckBox
      Left = 88
      Top = 76
      Width = 97
      Height = 17
      Caption = 'Mount &readonly'
      TabOrder = 2
      OnClick = SelectionChange
      AutoSize = True
    end
    object cbMediaType: TComboBox
      Left = 88
      Top = 48
      Width = 113
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = SelectionChange
    end
    object ckMountForAllUsers: TSDUCheckBox
      Left = 88
      Top = 96
      Width = 121
      Height = 17
      Caption = 'Mount for all &users'
      TabOrder = 3
      AutoSize = True
    end
  end
  object GroupBox4: TGroupBox
    Left = 236
    Top = 220
    Width = 302
    Height = 125
    Caption = 'File options'
    TabOrder = 2
    object Label10: TLabel
      Left = 12
      Top = 24
      Width = 40
      Height = 13
      Caption = 'Si&zelimit:'
    end
    object Label22: TLabel
      Left = 80
      Top = 48
      Width = 123
      Height = 13
      Caption = '("0" indicates no sizelimit.)'
    end
    object se64UnitSizeLimit: TSDUSpin64Unit_Storage
      Left = 80
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
  object GroupBox1: TGroupBox
    Left = 11
    Top = 8
    Width = 529
    Height = 201
    Caption = 'Encryption'
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 24
      Width = 21
      Height = 13
      Caption = '&Key:'
    end
    object ckBaseIVCypherOnHashLength: TSDUCheckBox
      Left = 12
      Top = 168
      Width = 265
      Height = 17
      Caption = '&Base IV cypher on hash length (where applicable)'
      TabOrder = 1
      AutoSize = True
    end
    inline frmeLUKSKeyOrKeyfileEntry: TfrmeLUKSKeyOrKeyfileEntry
      Left = 56
      Top = 20
      Width = 464
      Height = 138
      TabOrder = 0
      ExplicitLeft = 56
      ExplicitTop = 20
      inherited lblTreatNewlineAsEOF_1: TLabel
        Left = 128
        Top = 115
        Width = 25
        ExplicitLeft = 128
        ExplicitTop = 115
        ExplicitWidth = 25
      end
      inherited lblTreatNewlineAsEOF_2: TLabel
        Left = 128
        Top = 129
        Width = 35
        ExplicitLeft = 128
        ExplicitTop = 129
        ExplicitWidth = 35
      end
      inherited cbNewlineType: TComboBox
        Left = 181
        Top = 115
        ExplicitLeft = 181
        ExplicitTop = 115
      end
    end
  end
end
