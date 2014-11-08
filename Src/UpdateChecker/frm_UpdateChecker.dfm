object frmUpdateChecker: TfrmUpdateChecker
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'SAS.Planet'
  ClientHeight = 130
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblCurVer: TLabel
    Left = 16
    Top = 12
    Width = 64
    Height = 13
    Caption = 'Your version:'
  end
  object lblNewVer: TLabel
    Left = 16
    Top = 34
    Width = 85
    Height = 13
    Caption = 'Available version:'
  end
  object lblCurVerValue: TLabel
    Left = 144
    Top = 12
    Width = 69
    Height = 13
    Caption = 'lblCurVerValue'
  end
  object lblNewVerValue: TLabel
    Left = 144
    Top = 34
    Width = 73
    Height = 13
    Caption = 'lblNewVerValue'
  end
  object lblProgressInfo: TLabel
    Left = 16
    Top = 76
    Width = 96
    Height = 13
    Caption = 'Download progress:'
  end
  object pbDownloadProgress: TProgressBar
    Left = 16
    Top = 53
    Width = 376
    Height = 17
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    Position = 100
    Smooth = True
    Step = 1
    TabOrder = 0
    ExplicitWidth = 402
  end
  object btnDownload: TButton
    Left = 174
    Top = 100
    Width = 106
    Height = 25
    Align = alCustom
    Anchors = [akTop, akRight]
    Caption = 'Download'
    TabOrder = 1
    OnClick = btnDownloadClick
    ExplicitLeft = 200
  end
  object btnClose: TButton
    Left = 286
    Top = 100
    Width = 106
    Height = 25
    Align = alCustom
    Anchors = [akTop, akRight]
    Caption = 'Close'
    TabOrder = 2
    OnClick = btnCloseClick
    ExplicitLeft = 312
  end
  object cbbChannel: TComboBox
    Left = 16
    Top = 100
    Width = 152
    Height = 21
    ItemHeight = 13
    TabOrder = 3
    Text = 'Nightly (testing)'
    OnChange = cbbChannelChange
    Items.Strings = (
      'Nightly (testing)'
      'Release (stable)'
      '')
  end
  object tmrCheckState: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrCheckStateTimer
    Left = 352
    Top = 8
  end
end
