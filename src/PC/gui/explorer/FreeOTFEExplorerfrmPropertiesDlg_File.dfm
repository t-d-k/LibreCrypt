inherited frmPropertiesDialog_File: TfrmPropertiesDialog_File
  Caption = 'frmPropertiesDialog_File'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited tsGeneral: TTabSheet
      object Image1: TImage [6]
        Left = 108
        Top = 88
        Width = 16
        Height = 16
      end
      object Label9: TLabel [7]
        Left = 12
        Top = 88
        Width = 58
        Height = 13
        Caption = 'Opens with:'
        FocusControl = edOpensWith
      end
      object Label5: TLabel [8]
        Left = 12
        Top = 220
        Width = 43
        Height = 13
        Caption = 'Created:'
        FocusControl = edTimestampCreated
      end
      object Label1: TLabel [9]
        Left = 12
        Top = 244
        Width = 44
        Height = 13
        Caption = 'Modified:'
        FocusControl = edTimestampModified
      end
      object Label11: TLabel [10]
        Left = 12
        Top = 268
        Width = 49
        Height = 13
        Caption = 'Accessed:'
      end
      object edOpensWith: TLabel [15]
        Left = 132
        Top = 88
        Width = 65
        Height = 13
        Caption = 'edOpensWith'
      end
      object edTimestampCreated: TLabel [16]
        Left = 108
        Top = 220
        Width = 102
        Height = 13
        Caption = 'edTimestampCreated'
      end
      object edTimestampModified: TLabel [17]
        Left = 108
        Top = 244
        Width = 103
        Height = 13
        Caption = 'edTimestampModified'
      end
      object edTimestampAccessed: TLabel [18]
        Left = 108
        Top = 268
        Width = 108
        Height = 13
        Caption = 'edTimestampAccessed'
      end
      inherited ckReadOnly: TCheckBox
        TabOrder = 5
      end
      inherited ckHidden: TCheckBox
        TabOrder = 6
      end
      inherited Panel2: TPanel
        TabOrder = 4
      end
      inherited ckArchive: TCheckBox
        TabOrder = 7
      end
      object Panel1: TPanel
        Left = 12
        Top = 204
        Width = 317
        Height = 9
        Caption = 'Panel1'
        TabOrder = 3
      end
      object Panel4: TPanel
        Left = 12
        Top = 116
        Width = 317
        Height = 9
        Caption = 'Panel4'
        TabOrder = 2
      end
    end
  end
end
