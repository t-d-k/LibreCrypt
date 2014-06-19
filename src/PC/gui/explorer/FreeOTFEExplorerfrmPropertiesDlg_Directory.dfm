inherited frmPropertiesDialog_Directory: TfrmPropertiesDialog_Directory
  Caption = 'frmPropertiesDialog_Directory'
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited tsGeneral: TTabSheet
      inherited lblSizeOnDisk: TLabel
        Top = 136
      end
      inherited lblSize: TLabel
        Top = 112
      end
      inherited lblLocation: TLabel
        Top = 88
      end
      inherited lblAttributes: TLabel
        Top = 248
      end
      object Label1: TLabel [6]
        Left = 12
        Top = 160
        Width = 46
        Height = 13
        Caption = 'Contains:'
        FocusControl = edContains
      end
      object Label4: TLabel [7]
        Left = 12
        Top = 204
        Width = 43
        Height = 13
        Caption = 'Created:'
        FocusControl = edTimestampCreated
      end
      inherited edLocation: TLabel
        Top = 88
      end
      inherited edSizeOnDisk: TLabel
        Top = 136
      end
      inherited edSize: TLabel
        Top = 112
      end
      object edContains: TLabel [12]
        Left = 108
        Top = 160
        Width = 54
        Height = 13
        Caption = 'edContains'
      end
      object edTimestampCreated: TLabel [13]
        Left = 108
        Top = 204
        Width = 102
        Height = 13
        Caption = 'edTimestampCreated'
      end
      inherited ckReadOnly: TCheckBox
        Left = 108
        Top = 248
        Width = 205
        TabOrder = 4
      end
      inherited ckHidden: TCheckBox
        Left = 108
        Top = 268
        Width = 197
        TabOrder = 5
      end
      inherited Panel2: TPanel
        Top = 228
        TabOrder = 3
      end
      inherited ckArchive: TCheckBox
        Left = 108
        Top = 288
        Width = 185
        TabOrder = 6
      end
      object Panel1: TPanel
        Left = 12
        Top = 188
        Width = 317
        Height = 9
        Caption = 'Panel1'
        TabOrder = 2
      end
    end
  end
end
