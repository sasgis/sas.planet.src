object FGoTo: TFGoTo
  Left = 295
  Top = 179
  Hint = 'Close'
  AlphaBlendValue = 220
  BorderStyle = bsToolWindow
  Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082'...'
  ClientHeight = 217
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label9: TLabel
    Left = 8
    Top = 188
    Width = 57
    Height = 13
    Caption = #1052#1072#1089#1096#1090#1072#1073': x'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 4
    Width = 297
    Height = 45
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object ComboBox1: TComboBox
      Left = 8
      Top = 16
      Width = 281
      Height = 21
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
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object BGo: TButton
    Left = 149
    Top = 184
    Width = 75
    Height = 25
    Hint = #1055#1077#1088#1077#1081#1090#1080' '#1074' '#1079#1072#1076#1072#1085#1085#1091#1102' '#1090#1086#1095#1082#1091
    Caption = #1055#1077#1088#1077#1081#1090#1080
    Default = True
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = BGoClick
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 56
    Width = 297
    Height = 49
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    object EditGF: TEdit
      Left = 8
      Top = 16
      Width = 281
      Height = 21
      TabOrder = 0
      OnClick = EditGFClick
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
    Left = 8
    Top = 109
    Width = 297
    Height = 68
    TabOrder = 6
    object Label21: TLabel
      Left = 34
      Top = 19
      Width = 41
      Height = 13
      Alignment = taRightJustify
      Caption = #1064#1080#1088#1086#1090#1072':'
      Color = clBtnFace
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label22: TLabel
      Left = 30
      Top = 41
      Width = 46
      Height = 13
      Alignment = taRightJustify
      Caption = #1044#1086#1083#1075#1086#1090#1072':'
      Color = clBtnFace
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lat_ns: TComboBox
      Left = 80
      Top = 16
      Width = 33
      Height = 21
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      Style = csDropDownList
      BiDiMode = bdLeftToRight
      Ctl3D = False
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ItemIndex = 0
      ParentBiDiMode = False
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      Text = 'N'
      OnClick = Lat1Click
      Items.Strings = (
        'N'
        'S')
    end
    object Lon_we: TComboBox
      Left = 80
      Top = 38
      Width = 33
      Height = 21
      BevelInner = bvNone
      BevelKind = bkSoft
      BevelOuter = bvNone
      Style = csDropDownList
      BiDiMode = bdLeftToRight
      Ctl3D = False
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ItemIndex = 0
      ParentBiDiMode = False
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 4
      Text = 'E'
      OnClick = Lat1Click
      Items.Strings = (
        'E'
        'W')
    end
    object lat2: TCurrencyEdit
      Left = 161
      Top = 16
      Width = 48
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 5
      DisplayFormat = '0.#####`'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 2
      OnClick = Lat1Click
    end
    object lat3: TCurrencyEdit
      Left = 209
      Top = 16
      Width = 48
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 4
      DisplayFormat = '0.####``'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 3
      OnClick = Lat1Click
    end
    object lon1: TCurrencyEdit
      Left = 113
      Top = 38
      Width = 48
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 8
      DisplayFormat = '0.########'#176
      FormatOnEditing = True
      MaxValue = 180.000000000000000000
      ParentCtl3D = False
      TabOrder = 5
      OnClick = Lat1Click
    end
    object lon2: TCurrencyEdit
      Left = 161
      Top = 38
      Width = 48
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 5
      DisplayFormat = '0.#####`'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 6
      OnClick = Lat1Click
    end
    object lon3: TCurrencyEdit
      Left = 209
      Top = 38
      Width = 48
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 4
      DisplayFormat = '0.####``'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 7
      OnClick = Lat1Click
    end
    object Lat1: TCurrencyEdit
      Left = 113
      Top = 16
      Width = 48
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 8
      DisplayFormat = '0.########'#176
      FormatOnEditing = True
      MaxValue = 180.000000000000000000
      ParentCtl3D = False
      TabOrder = 1
      OnClick = Lat1Click
    end
  end
  object RB1: TRadioButton
    Left = 19
    Top = 107
    Width = 85
    Height = 17
    Caption = #1050#1086#1086#1088#1076#1080#1085#1072#1090#1099
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object CBzoom: TComboBox
    Left = 72
    Top = 185
    Width = 41
    Height = 21
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 9
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
  object RB4: TRadioButton
    Left = 80
    Top = 53
    Width = 65
    Height = 17
    Caption = #1071#1085#1076#1077#1082#1089
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 230
    Top = 184
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 8
  end
end
