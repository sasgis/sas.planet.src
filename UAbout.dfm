object Fabout: TFabout
  Left = 268
  Top = 257
  BorderStyle = bsToolWindow
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 219
  ClientWidth = 317
  Color = clWhite
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 184
    Width = 321
    Height = 10
    Shape = bsTopLine
  end
  object Label1: TLabel
    Left = 120
    Top = 40
    Width = 40
    Height = 13
    Alignment = taRightJustify
    Caption = #1042#1077#1088#1089#1080#1103':'
  end
  object Label3: TLabel
    Left = 97
    Top = 56
    Width = 63
    Height = 13
    Alignment = taRightJustify
    Caption = #1056#1072#1079#1088#1072#1073#1086#1090#1082#1072':'
  end
  object Label4: TLabel
    Left = 133
    Top = 72
    Width = 27
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = #1057#1072#1081#1090':'
    ParentBiDiMode = False
  end
  object Label5: TLabel
    Left = 127
    Top = 88
    Width = 33
    Height = 13
    Alignment = taRightJustify
    Caption = #1055#1086#1095#1090#1072':'
  end
  object Label6: TLabel
    Left = 95
    Top = 104
    Width = 65
    Height = 13
    Alignment = taRightJustify
    Caption = #1055#1086#1084#1086#1095#1100' '#1085#1072#1084':'
  end
  object LabelName: TLabel
    Left = 0
    Top = 8
    Width = 321
    Height = 25
    Alignment = taCenter
    AutoSize = False
    Caption = 'SAS.'#1055#1083#1072#1085#1077#1090#1072
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Reference Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 102
    Top = 120
    Width = 58
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'WebMoney:'
    ParentBiDiMode = False
  end
  object LabelVer: TLabel
    Left = 163
    Top = 40
    Width = 3
    Height = 13
  end
  object Label2: TLabel
    Left = 163
    Top = 56
    Width = 59
    Height = 13
    Caption = #1043#1088#1091#1087#1087#1072' SAS'
  end
  object Label8: TLabel
    Left = 163
    Top = 88
    Width = 63
    Height = 13
    Cursor = crHandPoint
    Caption = 'az@sasgis.ru'
    Color = clWhite
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    OnClick = Label8Click
  end
  object Label9: TLabel
    Left = 78
    Top = 163
    Width = 82
    Height = 13
    Alignment = taRightJustify
    Caption = #1071#1085#1076#1077#1082#1089'.'#1044#1077#1085#1100#1075#1080':'
  end
  object Label10: TLabel
    Left = 163
    Top = 72
    Width = 72
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://sasgis.ru'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = Label10Click
  end
  object Button1: TButton
    Left = 120
    Top = 192
    Width = 81
    Height = 25
    Cancel = True
    Caption = #1047#1072#1082#1088#1099#1090#1100
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 163
    Top = 148
    Width = 118
    Height = 15
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 1
    Text = 'WME: E382109079322'
  end
  object Edit2: TEdit
    Left = 163
    Top = 134
    Width = 118
    Height = 15
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 2
    Text = 'WMZ: Z122595752786'
  end
  object Edit5: TEdit
    Left = 163
    Top = 163
    Width = 126
    Height = 15
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 3
    Text = '41001292446592'
  end
  object Edit6: TEdit
    Left = 163
    Top = 120
    Width = 118
    Height = 15
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    Ctl3D = False
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 4
    Text = 'WMR: R112587212279'
  end
end
