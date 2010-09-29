object FaddLine: TFaddLine
  Left = 187
  Top = 189
  BorderStyle = bsSizeToolWin
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1091#1090#1100
  ClientHeight = 330
  ClientWidth = 328
  Color = clBtnFace
  Constraints.MinHeight = 271
  Constraints.MinWidth = 336
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
  object Bevel2: TBevel
    Left = 0
    Top = 258
    Width = 328
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 240
    ExplicitWidth = 329
  end
  object Bevel5: TBevel
    Left = 0
    Top = 290
    Width = 328
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 264
    ExplicitWidth = 329
  end
  object CheckBox2: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 270
    Width = 322
    Height = 17
    Align = alBottom
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 0
    ExplicitTop = 187
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label7: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 56
      Height = 19
      Align = alLeft
      Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object CBKateg: TComboBox
      AlignWithMargins = True
      Left = 65
      Top = 3
      Width = 260
      Height = 21
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      Text = #1053#1086#1074#1072#1103' '#1082#1072#1090#1077#1075#1086#1088#1080#1103
    end
  end
  object pnlName: TPanel
    Left = 0
    Top = 25
    Width = 328
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 25
      Height = 21
      Align = alLeft
      Caption = #1048#1084#1103':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object EditName: TEdit
      AlignWithMargins = True
      Left = 34
      Top = 3
      Width = 291
      Height = 21
      Align = alClient
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 52
    Width = 328
    Height = 178
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitHeight = 95
  end
  object flwpnlStyle: TFlowPanel
    Left = 0
    Top = 230
    Width = 328
    Height = 28
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitTop = 147
    object Label3: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 25
      Height = 13
      Caption = #1062#1074#1077#1090
    end
    object ColorBox1: TColorBox
      AlignWithMargins = True
      Left = 34
      Top = 3
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
      TabOrder = 0
    end
    object SpeedButton1: TSpeedButton
      AlignWithMargins = True
      Left = 78
      Top = 3
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object Label5: TLabel
      AlignWithMargins = True
      Left = 101
      Top = 3
      Width = 39
      Height = 13
      Caption = #1064#1080#1088#1080#1085#1072
    end
    object SpinEdit1: TSpinEdit
      AlignWithMargins = True
      Left = 146
      Top = 3
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 1
      Value = 2
    end
    object Label4: TLabel
      AlignWithMargins = True
      Left = 193
      Top = 3
      Width = 83
      Height = 13
      Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
    end
    object SEtransp: TSpinEdit
      AlignWithMargins = True
      Left = 282
      Top = 3
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 35
    end
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 299
    Width = 328
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
    ExplicitTop = 216
    object Badd: TButton
      AlignWithMargins = True
      Left = 173
      Top = 3
      Width = 73
      Height = 25
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      TabOrder = 0
      OnClick = BaddClick
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 252
      Top = 3
      Width = 73
      Height = 25
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 1
      OnClick = Button2Click
    end
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
