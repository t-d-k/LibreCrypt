object frmWebDAVStatus: TfrmWebDAVStatus
  Left = 333
  Top = 182
  BorderStyle = bsDialog
  Caption = 'Network Service Status'
  ClientHeight = 222
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 68
    Width = 61
    Height = 13
    Caption = 'Share &name:'
    FocusControl = edShareName
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 69
    Height = 13
    Caption = '&Mapped drive:'
    FocusControl = edMappedDrive
  end
  object Label3: TLabel
    Left = 8
    Top = 152
    Width = 90
    Height = 13
    Caption = '&WebClient service:'
  end
  object Label4: TLabel
    Left = 8
    Top = 124
    Width = 82
    Height = 13
    Caption = 'MR&xDAV service:'
  end
  object Label5: TLabel
    Left = 8
    Top = 40
    Width = 55
    Height = 13
    Caption = '&IP address:'
    FocusControl = edIPAddress
  end
  object Label6: TLabel
    Left = 8
    Top = 12
    Width = 64
    Height = 13
    Caption = '&Server state:'
    FocusControl = edWebDAVServerState
  end
  object pbClose: TButton
    Left = 154
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    Default = True
    TabOrder = 12
    OnClick = pbCloseClick
  end
  object edShareName: TEdit
    Left = 112
    Top = 64
    Width = 181
    Height = 21
    TabOrder = 2
    Text = 'edShareName'
  end
  object edMappedDrive: TEdit
    Left = 112
    Top = 92
    Width = 181
    Height = 21
    TabOrder = 4
    Text = 'edMappedDrive'
  end
  object pbBrowse: TButton
    Left = 300
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Browse'
    TabOrder = 3
    OnClick = pbBrowseClick
  end
  object pbExplore: TButton
    Left = 300
    Top = 92
    Width = 75
    Height = 25
    Caption = '&Explore'
    TabOrder = 5
    OnClick = pbExploreClick
  end
  object edServiceWebClient: TEdit
    Left = 112
    Top = 148
    Width = 181
    Height = 21
    TabOrder = 9
    Text = 'edServiceWebClient'
  end
  object edServiceMRxDAV: TEdit
    Left = 112
    Top = 120
    Width = 181
    Height = 21
    TabOrder = 6
    Text = 'edServiceMRxDAV'
  end
  object edIPAddress: TEdit
    Left = 112
    Top = 36
    Width = 181
    Height = 21
    TabOrder = 1
    Text = 'edIPAddress'
  end
  object edWebDAVServerState: TEdit
    Left = 112
    Top = 8
    Width = 181
    Height = 21
    TabOrder = 0
    Text = 'edWebDAVServerState'
  end
  object pbStartWebClient: TButton
    Left = 300
    Top = 148
    Width = 33
    Height = 25
    Caption = 'Start'
    TabOrder = 10
  end
  object pbStopWebClient: TButton
    Left = 340
    Top = 148
    Width = 33
    Height = 25
    Caption = 'Stop'
    TabOrder = 11
  end
  object pbStopMRxDAV: TButton
    Left = 340
    Top = 120
    Width = 33
    Height = 25
    Caption = 'Stop'
    TabOrder = 8
  end
  object pbStartMRxDAV: TButton
    Left = 300
    Top = 120
    Width = 33
    Height = 25
    Caption = 'Start'
    TabOrder = 7
  end
end
