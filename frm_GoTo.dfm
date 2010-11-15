object frmGoTo: TfrmGoTo
  Left = 295
  Top = 179
  Hint = 'Close'
  AlphaBlendValue = 220
  BorderStyle = bsToolWindow
  Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082'...'
  ClientHeight = 207
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 270
    Height = 45
    Align = alTop
    TabOrder = 1
    object ComboBox1: TComboBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 260
      Height = 21
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnEnter = ComboBox1Enter
    end
  end
  object RB3: TRadioButton
    Left = 19
    Top = 2
    Width = 124
    Height = 17
    Caption = #1057#1086#1093#1088#1072#1085#1077#1085#1085#1099#1077' '#1084#1077#1090#1082#1080
    TabOrder = 0
  end
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 54
    Width = 270
    Height = 49
    Align = alTop
    TabOrder = 4
    object EditGF: TEdit
      AlignWithMargins = True
      Left = 7
      Top = 18
      Width = 256
      Height = 24
      Margins.Left = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      TabOrder = 0
      OnClick = EditGFClick
      ExplicitHeight = 21
    end
  end
  object RB2: TRadioButton
    Left = 19
    Top = 53
    Width = 62
    Height = 17
    Caption = 'Google!'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 109
    Width = 270
    Height = 64
    Align = alClient
    TabOrder = 6
    object pnlLonLat: TPanel
      Left = 2
      Top = 15
      Width = 266
      Height = 47
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
  object RB1: TRadioButton
    Left = 19
    Top = 107
    Width = 85
    Height = 17
    Caption = #1050#1086#1086#1088#1076#1080#1085#1072#1090#1099
    TabOrder = 5
  end
  object RB4: TRadioButton
    Left = 80
    Top = 53
    Width = 65
    Height = 17
    Caption = #1071#1085#1076#1077#1082#1089
    TabOrder = 3
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 176
    Width = 276
    Height = 31
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 7
    object Label9: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 58
      Height = 25
      Align = alLeft
      Alignment = taRightJustify
      Caption = #1052#1072#1089#1096#1090#1072#1073': x'
      Layout = tlCenter
      ExplicitLeft = 2
      ExplicitHeight = 13
    end
    object CBzoom: TComboBox
      AlignWithMargins = True
      Left = 67
      Top = 3
      Width = 41
      Height = 21
      Align = alLeft
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '01'
      Items.Strings = (
        '01'
        '02'
        '03'
        '04'
        '05'
        '06'
        '07'
        '08'
        '09'
        '10'
        '11'
        '12'
        '13'
        '14'
        '15'
        '16'
        '17'
        '18'
        '19'
        '20'
        '21'
        '22'
        '23'
        '24')
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 198
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      ModalResult = 2
      TabOrder = 1
    end
    object BGo: TButton
      AlignWithMargins = True
      Left = 117
      Top = 3
      Width = 75
      Height = 25
      Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1079#1072#1076#1072#1085#1085#1091#1102' '#1090#1086#1095#1082#1091
      Align = alRight
      Caption = #1055#1077#1088#1077#1081#1090#1080
      Default = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = BGoClick
    end
  end
end
