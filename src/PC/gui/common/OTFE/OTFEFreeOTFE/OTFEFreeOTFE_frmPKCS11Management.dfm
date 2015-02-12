object frmPKCS11Management: TfrmPKCS11Management
  Left = 383
  Top = 198
  BorderStyle = bsDialog
  Caption = 'Security Token/Smartcard Management'
  ClientHeight = 383
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    433
    383)
  PixelsPerInch = 96
  TextHeight = 13
  object pbClose: TButton
    Left = 348
    Top = 349
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 417
    Height = 330
    ActivePage = tsSecretKeys
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tsSecretKeys: TTabSheet
      Caption = 'Secret Keys'
      inline fmePKCS11_MgrSecretKey1: TfmePKCS11_MgrSecretKey
        Left = 0
        Top = 0
        Width = 409
        Height = 302
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 409
        ExplicitHeight = 302
        inherited lvSecretKeys: TListView
          Width = 383
          Height = 145
          ExplicitWidth = 383
          ExplicitHeight = 145
        end
        inherited gbSecretKey: TGroupBox
          Top = 188
          ExplicitTop = 188
        end
        inherited gbKeyfile: TGroupBox
          Left = 255
          Top = 188
          ExplicitLeft = 255
          ExplicitTop = 188
        end
      end
    end
    object tsKeyfiles: TTabSheet
      Caption = 'Keyfiles'
      ImageIndex = 1
      inline fmePKCS11_MgrKeyfile1: TfmePKCS11_MgrKeyfile
        Left = 0
        Top = 0
        Width = 409
        Height = 302
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 409
        ExplicitHeight = 302
        inherited pbImport: TButton
          Top = 264
          ExplicitTop = 264
        end
        inherited pbDelete: TButton
          Left = 307
          Top = 264
          Width = 89
          ExplicitLeft = 307
          ExplicitTop = 264
          ExplicitWidth = 89
        end
        inherited pbExport: TButton
          Top = 264
          ExplicitTop = 264
        end
        inherited lbCDB: TListBox
          Width = 383
          Height = 217
          ExplicitWidth = 383
          ExplicitHeight = 217
        end
      end
    end
  end
end
