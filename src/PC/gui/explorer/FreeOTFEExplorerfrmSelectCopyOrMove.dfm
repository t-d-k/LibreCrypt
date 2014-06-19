object frmSelectCopyOrMove: TfrmSelectCopyOrMove
  Left = 349
  Top = 417
  BorderStyle = bsDialog
  Caption = 'Copy or move'
  ClientHeight = 100
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 8
    Width = 288
    Height = 39
    Caption = 
      'Do you wish to copy or move the selected files/folders to the mo' +
      'unted volume? (i.e. Do you wish to leave the original files/fold' +
      'ers intact, or remove them?)'
    WordWrap = True
  end
  object pbCopy: TButton
    Left = 12
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Copy'
    Default = True
    TabOrder = 0
    OnClick = pbCopyClick
  end
  object pbCancel: TButton
    Left = 224
    Top = 64
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pbMove: TButton
    Left = 118
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Move'
    TabOrder = 2
    OnClick = pbMoveClick
  end
end
