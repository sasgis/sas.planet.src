object FEditMap: TFEditMap
  Left = 288
  Top = 302
  BorderStyle = bsDialog
  ClientHeight = 276
  ClientWidth = 438
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 22
    Height = 13
    Caption = 'URL'
  end
  object Label2: TLabel
    Left = 8
    Top = 67
    Width = 88
    Height = 13
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077' '#1074' '#1082#1101#1096#1077
  end
  object Label3: TLabel
    Left = 8
    Top = 99
    Width = 134
    Height = 13
    Caption = #1056#1086#1076#1080#1090#1077#1083#1100#1089#1082#1080#1081' '#1087#1091#1085#1082#1090' '#1084#1077#1085#1102
  end
  object Label4: TLabel
    Left = 8
    Top = 131
    Width = 59
    Height = 13
    Caption = #1043#1086#1088#1103#1095#1072#1103' '#1082#1083'.'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 240
    Width = 425
    Height = 9
    Shape = bsTopLine
  end
  object Label6: TLabel
    Left = 272
    Top = 131
    Width = 31
    Height = 13
    Caption = #1055#1072#1091#1079#1072
  end
  object EditNameinCache: TEdit
    Left = 104
    Top = 64
    Width = 305
    Height = 21
    TabOrder = 0
  end
  object EditParSubMenu: TEdit
    Left = 152
    Top = 96
    Width = 257
    Height = 21
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 216
    Width = 329
    Height = 17
    Caption = #1044#1086#1073#1072#1074#1083#1103#1090#1100' '#1074' '#1084#1077#1085#1102' '#1088#1072#1079#1076#1077#1083#1080#1090#1077#1083#1100' '#1087#1086#1089#1083#1077' '#1085#1072#1079#1074#1072#1085#1080#1103' '#1101#1090#1086#1081' '#1082#1072#1088#1090#1099
    TabOrder = 2
  end
  object EditHotKey: THotKey
    Left = 72
    Top = 128
    Width = 105
    Height = 21
    HotKey = 0
    Modifiers = []
    TabOrder = 3
  end
  object Button1: TButton
    Left = 280
    Top = 248
    Width = 75
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 360
    Top = 248
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    ModalResult = 2
    TabOrder = 5
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 248
    Width = 105
    Height = 25
    Caption = #1042#1089#1077' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
    TabOrder = 6
    OnClick = Button3Click
  end
  object Button6: TButton
    Left = 413
    Top = 8
    Width = 21
    Height = 21
    Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
    Caption = '<>'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = Button6Click
  end
  object Button4: TButton
    Left = 413
    Top = 64
    Width = 21
    Height = 21
    Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
    Caption = '<>'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 8
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 413
    Top = 96
    Width = 21
    Height = 21
    Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
    Caption = '<>'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 9
    OnClick = Button5Click
  end
  object Button7: TButton
    Left = 181
    Top = 128
    Width = 21
    Height = 21
    Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
    Caption = '<>'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 10
    OnClick = Button7Click
  end
  object EditURL: TMemo
    Left = 40
    Top = 8
    Width = 369
    Height = 49
    ScrollBars = ssVertical
    TabOrder = 11
    WantReturns = False
  end
  object SESleep: TSpinEdit
    Left = 312
    Top = 128
    Width = 97
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 12
    Value = 0
  end
  object Button8: TButton
    Left = 413
    Top = 128
    Width = 21
    Height = 21
    Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
    Caption = '<>'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 13
    OnClick = Button8Click
  end
  object RBCacheType: TRadioGroup
    Left = 9
    Top = 152
    Width = 424
    Height = 57
    Caption = ' '#1058#1080#1087' '#1082#1101#1096#1072' '
    Columns = 3
    ItemIndex = 1
    Items.Strings = (
      #1055#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
      'GoogleMV'
      'SAS.'#1055#1083#1072#1085#1077#1090#1072
      'EarthSlicer 1.95'
      'Googe maps tiles'
      'Google Earth')
    TabOrder = 14
  end
end
