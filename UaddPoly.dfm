object FAddPoly: TFAddPoly
  Left = 360
  Top = 40
  BorderStyle = bsSizeToolWin
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1086#1083#1080#1075#1086#1085
  ClientHeight = 351
  ClientWidth = 323
  Color = clBtnFace
  Constraints.MinHeight = 375
  Constraints.MinWidth = 331
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel2: TBevel
    Left = 0
    Top = 279
    Width = 323
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 288
    ExplicitWidth = 329
  end
  object Bevel5: TBevel
    Left = 0
    Top = 311
    Width = 323
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 312
    ExplicitWidth = 329
  end
  object CheckBox2: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 291
    Width = 317
    Height = 17
    Align = alBottom
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 5
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 320
    Width = 323
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
    object Badd: TButton
      AlignWithMargins = True
      Left = 168
      Top = 3
      Width = 73
      Height = 25
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = BaddClick
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 247
      Top = 3
      Width = 73
      Height = 25
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlFill: TPanel
    Left = 0
    Top = 232
    Width = 323
    Height = 47
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 4
    object Label10: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 317
      Height = 13
      Align = alTop
      Caption = #1047#1072#1087#1086#1083#1085#1077#1085#1080#1077':'
      ExplicitWidth = 64
    end
    object flwpnlFill: TFlowPanel
      Left = 0
      Top = 19
      Width = 323
      Height = 28
      Align = alTop
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      object Label6: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 25
        Height = 13
        Caption = #1062#1074#1077#1090
      end
      object ColorBox2: TColorBox
        AlignWithMargins = True
        Left = 34
        Top = 3
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
        TabOrder = 0
      end
      object SpeedButton2: TSpeedButton
        AlignWithMargins = True
        Left = 78
        Top = 3
        Width = 17
        Height = 22
        Caption = '...'
        OnClick = SpeedButton2Click
      end
      object Label8: TLabel
        AlignWithMargins = True
        Left = 101
        Top = 3
        Width = 83
        Height = 13
        Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
      end
      object SEtransp2: TSpinEdit
        AlignWithMargins = True
        Left = 190
        Top = 3
        Width = 41
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 1
        Value = 80
      end
    end
  end
  object pnlLine: TPanel
    Left = 0
    Top = 185
    Width = 323
    Height = 47
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 3
    object Label9: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 317
      Height = 13
      Align = alTop
      Caption = #1051#1080#1085#1080#1103':'
      ExplicitWidth = 35
    end
    object flwpnlLine: TFlowPanel
      Left = 0
      Top = 19
      Width = 323
      Height = 28
      Align = alTop
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
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
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 52
    Width = 323
    Height = 133
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 323
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label7: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 56
      Height = 13
      Align = alLeft
      Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
    end
    object CBKateg: TComboBox
      AlignWithMargins = True
      Left = 65
      Top = 3
      Width = 255
      Height = 21
      Align = alClient
      ItemHeight = 0
      TabOrder = 0
      Text = #1053#1086#1074#1072#1103' '#1082#1072#1090#1077#1075#1086#1088#1080#1103
    end
  end
  object pnlName: TPanel
    Left = 0
    Top = 25
    Width = 323
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 25
      Height = 13
      Align = alLeft
      Caption = #1048#1084#1103':'
    end
    object EditName: TEdit
      AlignWithMargins = True
      Left = 34
      Top = 3
      Width = 286
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
  object OpenDialog1: TOpenDialog
    Filter = 'PNG|*.png'
    Left = 40
    Top = 248
  end
  object ColorDialog1: TColorDialog
    Left = 88
    Top = 264
  end
end
