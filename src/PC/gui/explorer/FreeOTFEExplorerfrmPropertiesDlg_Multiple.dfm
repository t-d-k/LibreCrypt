inherited frmPropertiesDialog_Multiple: TfrmPropertiesDialog_Multiple
  Caption = 'frmPropertiesDialog_Multiple'
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
        Top = 176
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
      inherited ckReadOnly: TCheckBox
        Left = 108
        Top = 176
        Width = 193
      end
      inherited ckHidden: TCheckBox
        Left = 108
        Top = 200
        Width = 205
      end
      inherited Panel2: TPanel
        Top = 160
      end
      inherited ckArchive: TCheckBox
        Left = 108
        Top = 224
        Width = 213
      end
    end
  end
end
