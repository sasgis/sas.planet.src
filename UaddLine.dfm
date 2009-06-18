object FaddLine: TFaddLine
  Left = 452
  Top = 241
  BorderStyle = bsDialog
  Caption = 'FaddLine'
  ClientHeight = 310
  ClientWidth = 321
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
    Top = 68
    Width = 53
    Height = 13
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077':'
  end
  object Bevel2: TBevel
    Left = 8
    Top = 240
    Width = 305
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel3: TBevel
    Left = 8
    Top = 56
    Width = 305
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel4: TBevel
    Left = 8
    Top = 196
    Width = 305
    Height = 9
    Shape = bsBottomLine
  end
  object Bevel5: TBevel
    Left = 8
    Top = 264
    Width = 305
    Height = 9
    Shape = bsBottomLine
  end
  object Label3: TLabel
    Left = 8
    Top = 216
    Width = 25
    Height = 13
    Caption = #1062#1074#1077#1090
  end
  object Label5: TLabel
    Left = 98
    Top = 216
    Width = 39
    Height = 13
    Caption = #1064#1080#1088#1080#1085#1072
  end
  object Label4: TLabel
    Left = 186
    Top = 216
    Width = 83
    Height = 13
    Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
  end
  object SpeedButton1: TSpeedButton
    Left = 75
    Top = 213
    Width = 17
    Height = 22
    Caption = '...'
    OnClick = SpeedButton1Click
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
    Width = 273
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
    Top = 83
    Width = 305
    Height = 113
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Badd: TButton
    Left = 160
    Top = 280
    Width = 73
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    TabOrder = 2
    OnClick = BaddClick
  end
  object Button2: TButton
    Left = 240
    Top = 280
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
    Top = 251
    Width = 217
    Height = 17
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 4
  end
  object ColorBox1: TColorBox
    Left = 37
    Top = 213
    Width = 38
    Height = 22
    Selected = clRed
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
    Left = 141
    Top = 213
    Width = 41
    Height = 22
    MaxValue = 24
    MinValue = 1
    TabOrder = 6
    Value = 2
  end
  object SEtransp: TSpinEdit
    Left = 272
    Top = 213
    Width = 41
    Height = 22
    MaxValue = 100
    MinValue = 0
    TabOrder = 7
    Value = 35
  end
  object CBKateg: TComboBox
    Left = 72
    Top = 8
    Width = 241
    Height = 21
    ItemHeight = 13
    TabOrder = 8
    Text = #1053#1086#1074#1072#1103' '#1082#1072#1090#1077#1075#1086#1088#1080#1103
  end
  object OpenDialog1: TOpenDialog
    Filter = 'PNG|*.png'
    Left = 8
    Top = 280
  end
  object ColorDialog1: TColorDialog
    Left = 56
    Top = 280
  end
end
