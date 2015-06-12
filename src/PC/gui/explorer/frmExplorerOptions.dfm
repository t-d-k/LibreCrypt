inherited frmExplorerOptions: TfrmExplorerOptions
  OnCreate = FormCreate
  ExplicitWidth = 519
  ExplicitHeight = 516
  PixelsPerInch = 96
  TextHeight = 13
  inherited pcOptions: TPageControl
    ActivePage = tsWebDAV
    object tsGeneral: TTabSheet
      Caption = 'General'
      ImageIndex = 1
      inline fmeOptions_FreeOTFEExplorerGeneral1: TfmeExplorerOptions
        Left = -12
        Top = 24
        Width = 504
        Height = 321
        TabOrder = 0
        ExplicitLeft = -12
        ExplicitTop = 24
        inherited gbGeneral: TGroupBox
          inherited lblChkUpdatesFreq: TLabel
            Width = 90
            ExplicitWidth = 90
          end
          inherited Label1: TLabel
            Width = 164
            ExplicitWidth = 164
          end
        end
      end
    end
    object tsAdvanced: TTabSheet
      Caption = 'Advanced'
      ImageIndex = 2
      inline fmeOptions_FreeOTFEExplorerAdvanced1: TfmeAdvancedExplorerOptions
        Left = -12
        Top = -34
        Width = 504
        Height = 382
        TabOrder = 0
        ExplicitLeft = -12
        ExplicitTop = -34
        inherited gbAdvanced: TGroupBox
          inherited lblMRUMaxItemCountInst: TLabel
            Width = 134
            ExplicitWidth = 134
          end
          inherited lblMRUMaxItemCount: TLabel
            Width = 158
            ExplicitWidth = 158
          end
          inherited lblOverwritePasses: TLabel
            Width = 84
            ExplicitWidth = 84
          end
          inherited lblOverwriteMethod: TLabel
            Width = 86
            ExplicitWidth = 86
          end
          inherited lblMoveDeletionMethod: TLabel
            Width = 147
            ExplicitWidth = 147
          end
        end
      end
    end
    object tsWebDAV: TTabSheet
      Caption = 'Drive'
      ImageIndex = 3
      inline fmeOptions_FreeOTFEExplorerWebDAV1: TfmeExplorerWebDAVOptions
        Left = 6
        Top = 13
        Width = 486
        Height = 335
        TabOrder = 0
        ExplicitLeft = 6
        ExplicitTop = 13
        ExplicitHeight = 335
        inherited gbWebDAV: TGroupBox
          Enabled = False
          inherited lblDefaultDriveLetter: TLabel
            Width = 89
            ExplicitWidth = 89
          end
          inherited gbWebDAVAdvanced: TGroupBox
            inherited Label6: TLabel
              Width = 86
              ExplicitWidth = 86
            end
          end
        end
      end
    end
    object tsAutorun: TTabSheet
      Caption = 'Autorun'
      ImageIndex = 4
      inline fmeOptions_Autorun1: TfmeAutorunOptions
        Left = 21
        Top = -10
        Width = 471
        Height = 358
        TabOrder = 0
        ExplicitLeft = 21
        ExplicitTop = -10
        inherited gbAutorun: TGroupBox
          inherited Label33: TLabel
            Width = 56
            ExplicitWidth = 56
          end
          inherited Label34: TLabel
            Width = 64
            ExplicitWidth = 64
          end
          inherited Label35: TLabel
            Width = 69
            ExplicitWidth = 69
          end
        end
      end
    end
  end
end
