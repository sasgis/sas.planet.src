object FaddPoint: TFaddPoint
  Left = 355
  Top = 213
  BorderStyle = bsDialog
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1084#1077#1090#1082#1091
  ClientHeight = 402
  ClientWidth = 340
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 34
    Width = 25
    Height = 13
    Caption = #1048#1084#1103':'
  end
  object Label2: TLabel
    Left = 8
    Top = 132
    Width = 53
    Height = 13
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077':'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 56
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel2: TBevel
    Left = 8
    Top = 336
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel3: TBevel
    Left = 8
    Top = 112
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel4: TBevel
    Left = 8
    Top = 252
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel5: TBevel
    Left = 8
    Top = 360
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Label21: TLabel
    Left = 65
    Top = 75
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
    Left = 60
    Top = 97
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
  object Label3: TLabel
    Left = 19
    Top = 272
    Width = 62
    Height = 13
    Alignment = taRightJustify
    Caption = #1062#1074#1077#1090' '#1090#1077#1082#1089#1090#1072
  end
  object Label4: TLabel
    Left = 30
    Top = 296
    Width = 51
    Height = 13
    Alignment = taRightJustify
    Caption = #1062#1074#1077#1090' '#1090#1077#1085#1080
  end
  object Label5: TLabel
    Left = 192
    Top = 272
    Width = 81
    Height = 13
    Alignment = taRightJustify
    Caption = #1056#1072#1079#1084#1077#1088' '#1096#1088#1080#1092#1090#1072
  end
  object Label6: TLabel
    Left = 195
    Top = 296
    Width = 78
    Height = 13
    Alignment = taRightJustify
    Caption = #1056#1072#1079#1084#1077#1088' '#1080#1082#1086#1085#1082#1080
  end
  object Label7: TLabel
    Left = 112
    Top = 320
    Width = 83
    Height = 13
    Alignment = taRightJustify
    Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
  end
  object SpeedButton1: TSpeedButton
    Left = 126
    Top = 269
    Width = 17
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object SpeedButton2: TSpeedButton
    Left = 126
    Top = 293
    Width = 17
    Height = 22
    Caption = '...'
    OnClick = SpeedButton2Click
  end
  object Label8: TLabel
    Left = 72
    Top = 10
    Width = 56
    Height = 13
    Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
  end
  object EditName: TEdit
    Left = 104
    Top = 32
    Width = 233
    Height = 21
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnKeyPress = EditCommentKeyPress
  end
  object EditComment: TMemo
    Left = 8
    Top = 152
    Width = 329
    Height = 99
    ScrollBars = ssVertical
    TabOrder = 1
    OnKeyDown = EditCommentKeyDown
    OnKeyPress = EditCommentKeyPress
  end
  object Badd: TButton
    Left = 184
    Top = 376
    Width = 73
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 2
    OnClick = BaddClick
  end
  object Button2: TButton
    Left = 264
    Top = 376
    Width = 73
    Height = 25
    Hint = #1054#1090#1084#1077#1085#1080#1090#1100
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 3
    OnClick = Button2Click
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 347
    Width = 329
    Height = 17
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 4
  end
  object lat_ns: TComboBox
    Left = 112
    Top = 72
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
    TabOrder = 5
    Text = 'N'
    Items.Strings = (
      'N'
      'S')
  end
  object Lat1: TCurrencyEdit
    Left = 145
    Top = 72
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
    TabOrder = 6
  end
  object lat2: TCurrencyEdit
    Left = 193
    Top = 72
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
    TabOrder = 7
  end
  object lat3: TCurrencyEdit
    Left = 241
    Top = 72
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
    TabOrder = 8
  end
  object lon1: TCurrencyEdit
    Left = 145
    Top = 94
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
    TabOrder = 9
  end
  object lon2: TCurrencyEdit
    Left = 193
    Top = 94
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
    TabOrder = 10
  end
  object lon3: TCurrencyEdit
    Left = 241
    Top = 94
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
    TabOrder = 11
  end
  object Lon_we: TComboBox
    Left = 112
    Top = 94
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
    TabOrder = 12
    Text = 'E'
    Items.Strings = (
      'E'
      'W')
  end
  object ColorBox1: TColorBox
    Left = 88
    Top = 269
    Width = 38
    Height = 22
    Selected = clYellow
    Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 13
  end
  object SpinEdit1: TSpinEdit
    Left = 280
    Top = 269
    Width = 41
    Height = 22
    MaxValue = 24
    MinValue = 0
    TabOrder = 14
    Value = 11
  end
  object ColorBox2: TColorBox
    Left = 88
    Top = 293
    Width = 38
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 15
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 8
    Width = 57
    Height = 44
    Hint = #1042#1099#1073#1077#1088#1080#1090#1077' '#1075#1088#1072#1092#1080#1095#1077#1089#1082#1086#1077' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077
    Style = csOwnerDrawVariable
    ItemHeight = 38
    ParentShowHint = False
    ShowHint = False
    TabOrder = 16
    OnDrawItem = ComboBox1DrawItem
  end
  object SpinEdit2: TSpinEdit
    Left = 280
    Top = 293
    Width = 41
    Height = 22
    MaxValue = 64
    MinValue = 1
    TabOrder = 17
    Value = 32
  end
  object SEtransp: TSpinEdit
    Left = 200
    Top = 317
    Width = 41
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 18
    Value = 35
  end
  object CBKateg: TComboBox
    Left = 136
    Top = 8
    Width = 201
    Height = 21
    ItemHeight = 13
    TabOrder = 19
    Text = #1053#1086#1074#1072#1103' '#1082#1072#1090#1077#1075#1086#1088#1080#1103
  end
  object TBXToolbar1: TTBXToolbar
    Left = 164
    Top = 125
    Width = 173
    Height = 22
    Images = Fmain.EditCommentsImgs
    TabOrder = 20
    Caption = 'TBXToolbar1'
    object TBXItem3: TTBXItem
      ImageIndex = 0
      OnClick = TBXItem3Click
      Caption = ''
      Hint = ''
    end
    object TBXItem2: TTBXItem
      Tag = 1
      ImageIndex = 1
      OnClick = TBXItem3Click
      Caption = ''
      Hint = ''
    end
    object TBXItem1: TTBXItem
      Tag = 2
      ImageIndex = 2
      OnClick = TBXItem3Click
      Caption = ''
      Hint = ''
    end
    object TBXSeparatorItem1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object TBXItem4: TTBXItem
      Tag = 3
      ImageIndex = 3
      OnClick = TBXItem3Click
      Caption = ''
      Hint = ''
    end
    object TBXItem5: TTBXItem
      Tag = 4
      ImageIndex = 4
      OnClick = TBXItem3Click
      Caption = ''
      Hint = ''
    end
    object TBXItem6: TTBXItem
      Tag = 5
      ImageIndex = 5
      OnClick = TBXItem3Click
      Caption = ''
      Hint = ''
    end
    object TBXSeparatorItem2: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object TBXItem7: TTBXItem
      Tag = 6
      ImageIndex = 7
      OnClick = TBXItem3Click
      Caption = ''
      Hint = ''
    end
  end
  object ColorDialog1: TColorDialog
    Left = 40
    Top = 376
  end
end
