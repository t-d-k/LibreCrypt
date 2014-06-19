object Main_F: TMain_F
  Left = 221
  Top = 240
  Width = 663
  Height = 492
  Caption = 'Named Pipes Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 44
    Top = 348
    Width = 53
    Height = 13
    Caption = '&Pipe name:'
    FocusControl = edPipeName
  end
  object Label2: TLabel
    Left = 44
    Top = 392
    Width = 46
    Height = 13
    Caption = '&Message:'
    FocusControl = edMessage
  end
  object Bevel1: TBevel
    Left = 488
    Top = 216
    Width = 2
    Height = 33
  end
  object Bevel2: TBevel
    Left = 394
    Top = 20
    Width = 2
    Height = 281
  end
  object Label4: TLabel
    Left = 432
    Top = 28
    Width = 26
    Height = 13
    Caption = 'Client'
  end
  object Label5: TLabel
    Left = 524
    Top = 28
    Width = 31
    Height = 13
    Caption = 'Server'
  end
  object Bevel3: TBevel
    Left = 488
    Top = 280
    Width = 2
    Height = 21
  end
  object Bevel4: TBevel
    Left = 584
    Top = 24
    Width = 2
    Height = 273
  end
  object Bevel5: TBevel
    Left = 488
    Top = 24
    Width = 2
    Height = 121
  end
  object Label3: TLabel
    Left = 348
    Top = 312
    Width = 38
    Height = 13
    Caption = 'Results:'
  end
  object pbConnectClient: TButton
    Left = 404
    Top = 116
    Width = 75
    Height = 25
    Caption = '&Connect Client'
    TabOrder = 0
    OnClick = pbConnectClientClick
  end
  object reReport: TRichEdit
    Left = 348
    Top = 328
    Width = 297
    Height = 129
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object edPipeName: TEdit
    Left = 100
    Top = 344
    Width = 205
    Height = 21
    TabOrder = 2
    Text = 'PGC_PIPE'
  end
  object pbSend: TButton
    Left = 452
    Top = 156
    Width = 75
    Height = 25
    Caption = '&Send'
    TabOrder = 3
    OnClick = pbSendClick
  end
  object edMessage: TEdit
    Left = 100
    Top = 388
    Width = 205
    Height = 21
    TabOrder = 4
    Text = 'THIS IS THE MESSAGE'
  end
  object pbClose: TButton
    Left = 452
    Top = 252
    Width = 75
    Height = 25
    Caption = '&Close'
    TabOrder = 5
    OnClick = pbCloseClick
  end
  object ckServer: TCheckBox
    Left = 152
    Top = 312
    Width = 61
    Height = 17
    Caption = 'Server'
    TabOrder = 6
    OnClick = ckServerClick
  end
  object pbReceive: TButton
    Left = 452
    Top = 188
    Width = 75
    Height = 25
    Caption = 'Receive'
    TabOrder = 7
    OnClick = pbReceiveClick
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 8
    Width = 361
    Height = 289
    Lines.Strings = (
      'Instructions'
      ''
      'Server: '
      ''
      '1) Launch the application'
      '2) Check the "Server" checkbox'
      
        '3) Click "Create Pipe" to create the named pipe (the name of the' +
        ' pipe is '
      'specified by the "Pipe name" editbox)'
      
        '4) Click "Connect Server" to connect to a client. Application sh' +
        'ould '
      'block until a client connects'
      
        '5) Click "Send" to send to the client. Application should block ' +
        'until the '
      'client receives the message'
      '6) Click "Receive" to receive from the client'
      
        '7) Click "Disconnect" to disconnect from the client. From here, ' +
        'a server '
      'will normally goto step 4'
      '8) Click "Close" to close the pipe'
      ''
      'Client:'
      ''
      '1) Launch the application'
      '2) Uncheck the "Server" checkbox'
      
        '3) Click "Connect Client" to connect to a server via a named pip' +
        'e (the '
      'name of the pipe is specified by the "Pipe name" editbox)'
      
        '4) Click "Send" to send to the server. Application should block ' +
        'until the '
      'server receives the message'
      '5) Click "Receive" to receive from the server'
      '6) Click "Close" to disconnect from the server'
      '')
    ScrollBars = ssVertical
    TabOrder = 8
  end
  object pbCreatePipe: TButton
    Left = 500
    Top = 84
    Width = 75
    Height = 25
    Caption = 'Create Pipe'
    Enabled = False
    TabOrder = 9
    OnClick = pbCreatePipeClick
  end
  object pbConnectServer: TButton
    Left = 500
    Top = 116
    Width = 75
    Height = 25
    Caption = 'Connect Server'
    Enabled = False
    TabOrder = 10
    OnClick = pbConnectServerClick
  end
  object pbDisconnect: TButton
    Left = 500
    Top = 220
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 11
    OnClick = pbDisconnectClick
  end
end
