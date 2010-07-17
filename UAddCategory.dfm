object FAddCategory: TFAddCategory
  Left = 208
  Top = 318
  BorderStyle = bsDialog
  Caption = 'FAddCategory'
  ClientHeight = 149
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 50
    Height = 13
    Caption = #1053#1072#1079#1074#1072#1085#1080#1077
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 115
    Height = 13
    Caption = #1052#1072#1089#1096#1090#1072#1073#1099' '#1074#1080#1076#1080#1084#1086#1089#1090#1080':'
  end
  object Label3: TLabel
    Left = 144
    Top = 56
    Width = 6
    Height = 13
    Caption = 'c'
  end
  object Label4: TLabel
    Left = 208
    Top = 56
    Width = 12
    Height = 13
    Caption = #1076#1086
  end
  object Bevel5: TBevel
    Left = 8
    Top = 106
    Width = 369
    Height = 9
    Shape = bsBottomLine
  end
  object EditName: TEdit
    Left = 72
    Top = 16
    Width = 297
    Height = 21
    TabOrder = 0
  end
  object CBShow: TCheckBox
    Left = 16
    Top = 88
    Width = 137
    Height = 17
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 1
  end
  object EditS1: TSpinEdit
    Left = 157
    Top = 53
    Width = 41
    Height = 22
    MaxValue = 24
    MinValue = 1
    TabOrder = 2
    Value = 3
  end
  object EditS2: TSpinEdit
    Left = 229
    Top = 53
    Width = 41
    Height = 22
    MaxValue = 24
    MinValue = 1
    TabOrder = 3
    Value = 18
  end
  object Badd: TButton
    Left = 224
    Top = 119
    Width = 73
    Height = 25
    Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    ModalResult = 1
    TabOrder = 4
    OnClick = BaddClick
  end
  object Button2: TButton
    Left = 304
    Top = 119
    Width = 73
    Height = 25
    Hint = #1054#1090#1084#1077#1085#1080#1090#1100
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    ModalResult = 2
    TabOrder = 5
  end
end
