inherited frmOptions_FreeOTFEExplorer: TfrmOptions_FreeOTFEExplorer
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited pcOptions: TPageControl
    ActivePage = tsAdvanced
    object tsGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 1
      inline fmeOptions_FreeOTFEExplorerGeneral1: TfmeOptions_FreeOTFEExplorerGeneral
        Left = -12
        Top = 24
        Width = 504
        Height = 321
        TabOrder = 0
        inherited gbGeneral: TGroupBox
          inherited lblChkUpdatesFreq: TLabel
            Width = 90
          end
          inherited Label1: TLabel
            Width = 164
          end
        end
      end
    end
    object tsAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 2
      inline fmeOptions_FreeOTFEExplorerAdvanced1: TfmeOptions_FreeOTFEExplorerAdvanced
        Left = -12
        Top = -34
        Width = 504
        Height = 382
        TabOrder = 0
      end
    end
  end
end
