object FSelLonLat: TFSelLonLat
  Left = 192
  Top = 289
  BorderStyle = bsDialog
  Caption = #1042#1099#1076#1077#1083#1077#1085#1080#1077' '#1087#1086' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1072#1084
  ClientHeight = 177
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 96
    Top = 152
    Width = 75
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 176
    Top = 152
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 1
    OnClick = Button2Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 5
    Width = 241
    Height = 68
    Caption = ' '#1051#1077#1074#1099#1081' '#1074#1077#1088#1093#1085#1080#1081' '#1091#1075#1086#1083' '
    TabOrder = 2
    object Label21: TLabel
      Left = 10
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
      Left = 6
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
    object _lat_ns: TComboBox
      Left = 56
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
      Items.Strings = (
        'N'
        'S')
    end
    object _Lon_we: TComboBox
      Left = 56
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
      Items.Strings = (
        'E'
        'W')
    end
    object _lat2: TCurrencyEdit
      Left = 137
      Top = 16
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 5
      DisplayFormat = '0.#####`'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 2
    end
    object _lat3: TCurrencyEdit
      Left = 185
      Top = 16
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 4
      DisplayFormat = '0.####``'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 3
    end
    object _lon1: TCurrencyEdit
      Left = 89
      Top = 38
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 8
      DisplayFormat = '0.########'#176
      FormatOnEditing = True
      MaxValue = 180.000000000000000000
      ParentCtl3D = False
      TabOrder = 5
    end
    object _lon2: TCurrencyEdit
      Left = 137
      Top = 38
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 5
      DisplayFormat = '0.#####`'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 6
    end
    object _lon3: TCurrencyEdit
      Left = 185
      Top = 38
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 4
      DisplayFormat = '0.####``'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 7
    end
    object _Lat1: TCurrencyEdit
      Left = 89
      Top = 16
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 8
      DisplayFormat = '0.########'#176
      FormatOnEditing = True
      MaxValue = 180.000000000000000000
      ParentCtl3D = False
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 77
    Width = 241
    Height = 68
    Caption = ' '#1055#1088#1072#1074#1099#1081' '#1085#1080#1078#1085#1080#1081' '#1091#1075#1086#1083' '
    TabOrder = 3
    object Label1: TLabel
      Left = 10
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
    object Label2: TLabel
      Left = 6
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
      Left = 56
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
      Items.Strings = (
        'N'
        'S')
    end
    object Lon_we: TComboBox
      Left = 56
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
      Items.Strings = (
        'E'
        'W')
    end
    object lat2: TCurrencyEdit
      Left = 137
      Top = 16
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 5
      DisplayFormat = '0.#####`'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 2
    end
    object lat3: TCurrencyEdit
      Left = 185
      Top = 16
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 4
      DisplayFormat = '0.####``'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 3
    end
    object lon1: TCurrencyEdit
      Left = 89
      Top = 38
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 8
      DisplayFormat = '0.########'#176
      FormatOnEditing = True
      MaxValue = 180.000000000000000000
      ParentCtl3D = False
      TabOrder = 5
    end
    object lon2: TCurrencyEdit
      Left = 137
      Top = 38
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 5
      DisplayFormat = '0.#####`'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 6
    end
    object lon3: TCurrencyEdit
      Left = 185
      Top = 38
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 4
      DisplayFormat = '0.####``'
      FormatOnEditing = True
      MaxValue = 60.000000000000000000
      ParentCtl3D = False
      TabOrder = 7
    end
    object Lat1: TCurrencyEdit
      Left = 89
      Top = 16
      Width = 48
      Height = 21
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = True
      DecimalPlaces = 8
      DisplayFormat = '0.########'#176
      FormatOnEditing = True
      MaxValue = 180.000000000000000000
      ParentCtl3D = False
      TabOrder = 1
    end
  end
end
