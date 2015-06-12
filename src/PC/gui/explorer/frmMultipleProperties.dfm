inherited frmMultipleProperties: TfrmMultipleProperties
  Caption = 'frmPropertiesDialog_Multiple'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  ExplicitWidth = 320
  ExplicitHeight = 240
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl1: TPageControl
    inherited tsGeneral: TTabSheet
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      inherited lblSizeOnDisk: TLabel
        Top = 136
        ExplicitTop = 136
      end
      inherited lblSize: TLabel
        Top = 112
        ExplicitTop = 112
      end
      inherited lblLocation: TLabel
        Top = 88
        ExplicitTop = 88
      end
      inherited lblAttributes: TLabel
        Top = 176
        ExplicitTop = 176
      end
      inherited edLocation: TLabel
        Top = 88
        ExplicitTop = 88
      end
      inherited edSizeOnDisk: TLabel
        Top = 136
        ExplicitTop = 136
      end
      inherited edSize: TLabel
        Top = 112
        ExplicitTop = 112
      end
      inherited ckReadOnly: TCheckBox
        Left = 108
        Top = 176
        Width = 193
        ExplicitLeft = 108
        ExplicitTop = 176
        ExplicitWidth = 193
      end
      inherited ckHidden: TCheckBox
        Left = 108
        Top = 200
        Width = 205
        ExplicitLeft = 108
        ExplicitTop = 200
        ExplicitWidth = 205
      end
      inherited Panel2: TPanel
        Top = 160
        ExplicitTop = 160
      end
      inherited ckArchive: TCheckBox
        Left = 108
        Top = 224
        Width = 213
        ExplicitLeft = 108
        ExplicitTop = 224
        ExplicitWidth = 213
      end
    end
  end
end
