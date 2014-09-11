object frmWizardCreateVolumeAdvanced: TfrmWizardCreateVolumeAdvanced
  Left = 188
  Top = 77
  BorderStyle = bsDialog
  Caption = 'Advanced Options'
  ClientHeight = 428
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcAdvancedOpts: TPageControl
    Left = 9
    Top = 8
    Width = 417
    Height = 365
    ActivePage = tsChaff
    TabOrder = 0
    object tsKeyIterations: TTabSheet
      Caption = 'Key Iterations'
      ImageIndex = 3
      object Label22: TLabel
        Left = 80
        Top = 192
        Width = 70
        Height = 13
        Caption = '&Key iterations:'
        FocusControl = seKeyIterations
      end
      object seKeyIterations: TSpinEdit64
        Left = 184
        Top = 188
        Width = 144
        Height = 22
        Increment = 1
        TabOrder = 0
      end
      object reInstructKeyIterations: TOTFEFreeOTFE_InstructionRichEdit
        Left = 8
        Top = 12
        Width = 389
        Height = 169
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'reInstructKeyIterations')
        ParentFont = False
        TabOrder = 1
      end
    end
    object tsSalt: TTabSheet
      Caption = 'Salt'
      object Label15: TLabel
        Left = 110
        Top = 192
        Width = 70
        Height = 13
        Caption = '&Length of salt:'
        FocusControl = seSaltLength
      end
      object Label18: TLabel
        Left = 278
        Top = 192
        Width = 21
        Height = 13
        Caption = 'bits.'
      end
      object seSaltLength: TSpinEdit64
        Left = 210
        Top = 188
        Width = 61
        Height = 22
        Increment = 1
        MaxValue = 9999
        TabOrder = 0
        Value = 512
      end
      object reInstructSalt: TOTFEFreeOTFE_InstructionRichEdit
        Left = 8
        Top = 12
        Width = 389
        Height = 169
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'reInstructSalt')
        ParentFont = False
        TabOrder = 1
      end
    end
    object tsDriveLetter: TTabSheet
      Caption = 'Drive Letter'
      ImageIndex = 1
      object Label1: TLabel
        Left = 87
        Top = 192
        Width = 112
        Height = 13
        Caption = '&Requested drive letter:'
        FocusControl = cbDriveLetter
      end
      object cbDriveLetter: TComboBox
        Left = 233
        Top = 188
        Width = 89
        Height = 21
        Style = csDropDownList
        TabOrder = 0
      end
      object reInstructDriveLetter: TOTFEFreeOTFE_InstructionRichEdit
        Left = 8
        Top = 12
        Width = 389
        Height = 169
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'reInstructDriveLetter')
        ParentFont = False
        TabOrder = 1
      end
    end
    object tsCDBLocation: TTabSheet
      Caption = 'CDB Location'
      ImageIndex = 2
      object rbCDBInVolFile: TRadioButton
        Left = 14
        Top = 212
        Width = 355
        Height = 17
        Caption = 'Include CDB as part of volume file'
        TabOrder = 0
        OnClick = rbCDBLocationClick
      end
      object rbCDBInKeyfile: TRadioButton
        Left = 14
        Top = 232
        Width = 355
        Height = 17
        Caption = 'Store CDB as separate keyfile'
        TabOrder = 1
        OnClick = rbCDBLocationClick
      end
      object gbKeyfile: TGroupBox
        Left = 34
        Top = 248
        Width = 337
        Height = 73
        Caption = 'Keyfile'
        TabOrder = 2
        object lblKeyFilename: TSDUFilenameLabel
          Left = 8
          Top = 20
          Width = 321
          Height = 37
          AutoSize = False
          Caption = 'lblKeyFilename'
          WordWrap = True
        end
        object pbBrowseKeyfile: TButton
          Left = 252
          Top = 40
          Width = 75
          Height = 25
          Caption = '&Browse...'
          TabOrder = 0
          OnClick = pbBrowseKeyfileClick
        end
      end
      object reInstructCDBLocation: TOTFEFreeOTFE_InstructionRichEdit
        Left = 8
        Top = 12
        Width = 389
        Height = 197
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'reInstructCDBLocation')
        ParentFont = False
        TabOrder = 3
      end
    end
    object tsPadding: TTabSheet
      Caption = 'Padding'
      ImageIndex = 4
      object Label2: TLabel
        Left = 328
        Top = 296
        Width = 27
        Height = 13
        Caption = 'bytes'
      end
      object Label3: TLabel
        Left = 20
        Top = 296
        Width = 147
        Height = 13
        Caption = 'Amount of padding to append:'
      end
      object reInstructPadding: TOTFEFreeOTFE_InstructionRichEdit
        Left = 8
        Top = 12
        Width = 389
        Height = 261
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Lines.Strings = (
          'reInstructPadding')
        ParentFont = False
        TabOrder = 0
      end
      object se64Padding: TSpinEdit64
        Left = 200
        Top = 292
        Width = 121
        Height = 22
        Increment = 1
        TabOrder = 1
      end
    end
    object tsChaff: TTabSheet
      Caption = 'Chaff'
      ImageIndex = 5
      object Label4: TLabel
        Left = 3
        Top = 254
        Width = 115
        Height = 13
        Caption = 'Type of overwrite data:'
      end
      object Label5: TLabel
        Left = 0
        Top = 0
        Width = 409
        Height = 91
        Align = alTop
        Caption = 
          'Choose how to overwrite the file or partition the DoxBox is stor' +
          'ed on.'#13#10'Secure data will make it harder - perhaps impossible - f' +
          'or an attacker to tell if a '#39'hidden'#39' box has been added at a lat' +
          'er date.'#13#10'It will also make it hard to tell the exact size of yo' +
          'ur data. Overwriting with secure data will take longer than with' +
          ' zeros.'#13#10#39'Zeros'#39' will make it easy for an attacker to tell if a ' +
          'hidden box has been added and the amount of data stored in your ' +
          'box, but allow the box to be created faster.'
        WordWrap = True
        ExplicitWidth = 396
      end
      object rbDataEncrypted: TRadioButton
        Left = 192
        Top = 299
        Width = 193
        Height = 17
        Caption = 'Secure pseudorandom data'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rbZeros: TRadioButton
        Left = 192
        Top = 253
        Width = 193
        Height = 17
        Caption = 'Zeros'
        TabOrder = 1
      end
    end
  end
  object pbOK: TButton
    Left = 126
    Top = 388
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 234
    Top = 388
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object SaveDialog: TSDUSaveDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    PreserveCWD = False
    Left = 392
    Top = 292
  end
end
