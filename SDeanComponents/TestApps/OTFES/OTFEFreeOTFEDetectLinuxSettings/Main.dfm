object frmMain: TfrmMain
  Left = 322
  Top = 178
  Width = 870
  Height = 640
  Caption = 'frmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 45
    Height = 13
    Caption = 'Filename:'
  end
  object Label2: TLabel
    Left = 40
    Top = 52
    Width = 21
    Height = 13
    Caption = 'Key:'
  end
  object Label3: TLabel
    Left = 8
    Top = 84
    Width = 66
    Height = 13
    Caption = 'Test filename:'
  end
  object Label4: TLabel
    Left = 20
    Top = 228
    Width = 54
    Height = 13
    Caption = 'Drive letter:'
  end
  object Label5: TLabel
    Left = 64
    Top = 320
    Width = 261
    Height = 52
    Caption = 
      'IMPORTANT! IN ORDER TO USE THIS SOFTWARE, THE OTFEFreeOTFE PACKA' +
      'GE MUST BE BUILT WITH THE COMPILER DIRECTIVE "LINUX_DETECT" SET'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object reReport: TRichEdit
    Left = 388
    Top = 20
    Width = 457
    Height = 569
    Lines.Strings = (
      'reReport')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object pbGo: TButton
    Left = 144
    Top = 124
    Width = 75
    Height = 25
    Caption = 'pbGo'
    TabOrder = 1
    OnClick = pbGoClick
  end
  object edFilename: TEdit
    Left = 84
    Top = 24
    Width = 265
    Height = 21
    TabOrder = 2
    Text = 
      'C:\TEMP\lan\testsets\Linux - Fedora Core 3 test volume set\test_' +
      'volumes__cryptoloop\volumes\vol_aes.vol'
  end
  object edUserKey: TEdit
    Left = 84
    Top = 52
    Width = 265
    Height = 21
    TabOrder = 3
    Text = 'password1234567890ABC'
  end
  object edTestFilename: TEdit
    Left = 84
    Top = 80
    Width = 265
    Height = 21
    TabOrder = 4
    Text = '\SHORT_TEXT.txt'
  end
  object edDriveLetter: TEdit
    Left = 88
    Top = 224
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'V'
  end
  object OTFEFreeOTFE1: TOTFEFreeOTFE
    Active = False
    Left = 56
    Top = 132
  end
end
