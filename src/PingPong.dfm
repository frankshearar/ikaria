object PingPongDemo: TPingPongDemo
  Left = 192
  Top = 103
  Caption = 'Ping Pong Demo'
  ClientHeight = 613
  ClientWidth = 862
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
    object Go: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = 'Go'
      TabOrder = 0
      OnClick = GoClick
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
    TabOrder = 1
  end
end
