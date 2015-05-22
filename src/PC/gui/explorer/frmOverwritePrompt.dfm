object frmOverwritePrompt: TfrmOverwritePrompt
  Left = 440
  Top = 336
  BorderStyle = bsDialog
  Caption = 'Move deletion method'
  ClientHeight = 157
  ClientWidth = 302
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 284
    Height = 26
    Caption = 
      'What would you like to be done with the original file/folders af' +
      'ter they have been moved?'
    WordWrap = True
  end
  object pbOK: TButton
    Left = 65
    Top = 116
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object pbCancel: TButton
    Left = 161
    Top = 116
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbMoveDeletionMethod: TRadioGroup
    Left = 58
    Top = 44
    Width = 185
    Height = 57
    Caption = 'Deletion options'
    Items.Strings = (
      'OPTION 1'
      'OPTION 2')
    TabOrder = 2
  end
end
