object PingPongDemo: TPingPongDemo
  Left = 192
  Top = 103
  Width = 870
  Height = 640
  Caption = 'Ping Pong Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object StartPing: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Start Ping'
      TabOrder = 0
      OnClick = StartPingClick
    end
    object StopPing: TButton
      Left = 74
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Stop Ping'
      TabOrder = 1
      OnClick = StopPingClick
    end
    object NextFib: TButton
      Left = 148
      Top = 0
      Width = 80
      Height = 25
      Caption = 'Next Fibonacci'
      TabOrder = 2
      OnClick = NextFibClick
    end
  end
  object Log: TMemo
    Left = 0
    Top = 25
    Width = 862
    Height = 588
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
