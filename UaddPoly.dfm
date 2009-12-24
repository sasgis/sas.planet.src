object FAddPoly: TFAddPoly
  Left = 360
  Top = 40
  BorderStyle = bsDialog
  Caption = 'FAddPoly'
  ClientHeight = 361
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 34
    Width = 25
    Height = 13
    Caption = #1048#1084#1103':'
  end
  object Label2: TLabel
    Left = 8
    Top = 76
    Width = 53
    Height = 13
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077':'
  end
  object Bevel2: TBevel
    Left = 8
    Top = 288
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel3: TBevel
    Left = 8
    Top = 56
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel4: TBevel
    Left = 8
    Top = 196
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel5: TBevel
    Left = 8
    Top = 312
    Width = 329
    Height = 9
    Shape = bsBottomLine
  end
  object Label3: TLabel
    Left = 8
    Top = 224
    Width = 25
    Height = 13
    Caption = #1062#1074#1077#1090
  end
  object Label5: TLabel
    Left = 114
    Top = 224
    Width = 39
    Height = 13
    Caption = #1064#1080#1088#1080#1085#1072
  end
  object Label4: TLabel
    Left = 210
    Top = 224
    Width = 83
    Height = 13
    Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
  end
  object SpeedButton1: TSpeedButton
    Left = 75
    Top = 221
    Width = 17
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
  end
  object Label6: TLabel
    Left = 72
    Top = 272
    Width = 25
    Height = 13
    Caption = #1062#1074#1077#1090
  end
  object Label8: TLabel
    Left = 170
    Top = 272
    Width = 83
    Height = 13
    Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
  end
  object SpeedButton2: TSpeedButton
    Left = 139
    Top = 269
    Width = 17
    Height = 22
    Caption = '...'
    OnClick = SpeedButton2Click
  end
  object Label9: TLabel
    Left = 8
    Top = 206
    Width = 35
    Height = 13
    Caption = #1051#1080#1085#1080#1103':'
  end
  object Label10: TLabel
    Left = 8
    Top = 254
    Width = 64
    Height = 13
    Caption = #1047#1072#1087#1086#1083#1085#1077#1085#1080#1077':'
  end
  object Label7: TLabel
    Left = 8
    Top = 10
    Width = 56
    Height = 13
    Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
  end
  object EditName: TEdit
    Left = 40
    Top = 32
    Width = 297
    Height = 21
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object EditComment: TMemo
    Left = 8
    Top = 96
    Width = 329
    Height = 100
    ScrollBars = ssVertical
    TabOrder = 1
    OnKeyDown = EditCommentKeyDown
  end
  object Badd: TButton
    Left = 184
    Top = 328
    Width = 73
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    ModalResult = 1
    TabOrder = 2
    OnClick = BaddClick
  end
  object Button2: TButton
    Left = 264
    Top = 328
    Width = 73
    Height = 25
    Hint = #1054#1090#1084#1077#1085#1080#1090#1100
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    ModalResult = 2
    TabOrder = 3
    OnClick = Button2Click
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 299
    Width = 329
    Height = 17
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 4
  end
  object ColorBox1: TColorBox
    Left = 37
    Top = 221
    Width = 38
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 5
  end
  object SpinEdit1: TSpinEdit
    Left = 157
    Top = 221
    Width = 41
    Height = 22
    MaxValue = 24
    MinValue = 1
    TabOrder = 6
    Value = 2
  end
  object SEtransp: TSpinEdit
    Left = 296
    Top = 221
    Width = 41
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 7
    Value = 35
  end
  object ColorBox2: TColorBox
    Left = 101
    Top = 269
    Width = 38
    Height = 22
    Selected = clWhite
    Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 16
    ParentFont = False
    TabOrder = 8
  end
  object SEtransp2: TSpinEdit
    Left = 256
    Top = 269
    Width = 41
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 9
    Value = 80
  end
  object CBKateg: TComboBox
    Left = 72
    Top = 8
    Width = 265
    Height = 21
    ItemHeight = 13
    TabOrder = 10
    Text = #1053#1086#1074#1072#1103' '#1082#1072#1090#1077#1075#1086#1088#1080#1103
  end
  object TBXToolbar1: TTBXToolbar
    Left = 164
    Top = 69
    Width = 173
    Height = 22
    Images = Fmain.EditCommentsImgs
    TabOrder = 11
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
  object OpenDialog1: TOpenDialog
    Filter = 'PNG|*.png'
    Left = 8
    Top = 328
  end
  object ColorDialog1: TColorDialog
    Left = 40
    Top = 328
  end
end
