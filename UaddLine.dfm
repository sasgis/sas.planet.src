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
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object CheckBox2: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 249
    Width = 322
    Height = 17
    Align = alBottom
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 4
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label7: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 58
      Height = 19
      Align = alLeft
      Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object CBKateg: TComboBox
      AlignWithMargins = True
      Left = 67
      Top = 3
      Width = 258
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
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 23
      Height = 21
      Align = alLeft
      Caption = #1048#1084#1103':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object EditName: TEdit
      AlignWithMargins = True
      Left = 32
      Top = 3
      Width = 293
      Height = 21
      Align = alClient
      TabOrder = 0
    end
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 52
    Width = 328
    Height = 194
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
  end
  object flwpnlStyle: TFlowPanel
    Left = 0
    Top = 269
    Width = 328
    Height = 30
    Align = alBottom
    AutoSize = True
    AutoWrap = False
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 3
    object Label3: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 26
      Height = 13
      Align = alLeft
      Caption = #1062#1074#1077#1090
      Layout = tlCenter
    end
    object ColorBox1: TColorBox
      AlignWithMargins = True
      Left = 35
      Top = 3
      Width = 38
      Height = 22
      Selected = clRed
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object SpeedButton1: TSpeedButton
      AlignWithMargins = True
      Left = 79
      Top = 3
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = SpeedButton1Click
    end
    object Label5: TLabel
      AlignWithMargins = True
      Left = 102
      Top = 3
      Width = 40
      Height = 13
      Caption = #1064#1080#1088#1080#1085#1072
      Layout = tlCenter
    end
    object SpinEdit1: TSpinEdit
      AlignWithMargins = True
      Left = 148
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
      Left = 195
      Top = 3
      Width = 85
      Height = 13
      Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
      Layout = tlCenter
    end
    object SEtransp: TSpinEdit
      AlignWithMargins = True
      Left = 286
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
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 5
    object Badd: TButton
      AlignWithMargins = True
      Left = 173
      Top = 3
      Width = 73
      Height = 23
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      Default = True
      TabOrder = 0
      OnClick = BaddClick
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 252
      Top = 3
      Width = 73
      Height = 23
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 1
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
