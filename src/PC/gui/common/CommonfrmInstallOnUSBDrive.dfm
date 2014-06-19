object frmInstallOnUSBDrive: TfrmInstallOnUSBDrive
  Left = 340
  Top = 283
  BorderStyle = bsDialog
  Caption = 'CAPTION SET AUTOMATICALLY'
  ClientHeight = 242
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 92
    Width = 29
    Height = 13
    Caption = '&Drive:'
    FocusControl = cbDrive
  end
  object Label2: TLabel
    Left = 12
    Top = 124
    Width = 78
    Height = 13
    Caption = '&Path to copy to:'
    FocusControl = edPath
  end
  object pbOK: TButton
    Left = 138
    Top = 204
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 6
    OnClick = pbOKClick
  end
  object pbCancel: TButton
    Left = 230
    Top = 204
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object edPath: TEdit
    Left = 136
    Top = 120
    Width = 265
    Height = 21
    TabOrder = 2
    Text = 'edPath'
    OnChange = edPathChange
  end
  object cbDrive: TComboBox
    Left = 136
    Top = 88
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
  end
  object ckSetupAutoplay: TCheckBox
    Left = 16
    Top = 152
    Width = 385
    Height = 17
    Caption = 'SETUP AUTOINF TO AUTOLAUNCH CAPTION SET AUTOMATICALLY'
    TabOrder = 4
  end
  object pbBrowse: TButton
    Left = 408
    Top = 120
    Width = 21
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = pbBrowseClick
  end
  object pbRefreshDrives: TButton
    Left = 288
    Top = 86
    Width = 75
    Height = 25
    Caption = '&Refresh'
    TabOrder = 1
    OnClick = pbRefreshDrivesClick
  end
  object ckHideAutorunInf: TCheckBox
    Left = 36
    Top = 172
    Width = 365
    Height = 17
    Caption = '&Mark autorun.inf file as hidden'
    TabOrder = 5
  end
  object reInstructCopyToUSBDrive: TOTFEFreeOTFE_InstructionRichEdit
    Left = 12
    Top = 12
    Width = 417
    Height = 69
    TabStop = False
    Lines.Strings = (
      'reInstructCopyToUSBDrive')
    ParentColor = True
    PlainText = True
    ReadOnly = True
    TabOrder = 8
  end
end
