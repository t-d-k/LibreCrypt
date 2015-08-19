object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 243
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 112
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  inline frmePassword1: TfrmePassword
    Left = 16
    Top = 8
    Width = 448
    Height = 81
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 8
    ExplicitWidth = 448
    inherited mmShown: TMemo
      Height = 30
      ExplicitHeight = 30
    end
    inherited mmReal: TMemo
      Left = 80
      Top = 39
      Width = 365
      Height = 39
      ExplicitLeft = 80
      ExplicitTop = 39
      ExplicitWidth = 365
      ExplicitHeight = 39
    end
  end
end
