object OTFEPGPDiskMounting_F: TOTFEPGPDiskMounting_F
  Left = 664
  Top = 446
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Mounting PGPDisk'
  ClientHeight = 112
  ClientWidth = 183
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Timer1: TTimer
    Enabled = False
    Interval = 500
    OnTimer = Timer1Timer
    Left = 60
    Top = 20
  end
end
