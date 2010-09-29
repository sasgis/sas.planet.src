object FAddPoly: TFAddPoly
  Left = 360
  Top = 40
  BorderStyle = bsDialog
  Caption = 'FAddPoly'
  ClientHeight = 350
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
  object Bevel2: TBevel
    Left = 0
    Top = 278
    Width = 345
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 288
    ExplicitWidth = 329
  end
  object Bevel5: TBevel
    Left = 0
    Top = 310
    Width = 345
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
    Top = 290
    Width = 339
    Height = 17
    Align = alBottom
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 299
    ExplicitWidth = 329
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 319
    Width = 345
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 430
    ExplicitWidth = 487
    object Badd: TButton
      AlignWithMargins = True
      Left = 190
      Top = 3
      Width = 73
      Height = 25
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      ModalResult = 1
      TabOrder = 0
      OnClick = BaddClick
      ExplicitLeft = 221
      ExplicitTop = 8
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 269
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
      ExplicitLeft = 360
      ExplicitTop = 8
    end
  end
  object pnlFill: TPanel
    Left = 0
    Top = 231
    Width = 345
    Height = 47
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 297
    ExplicitWidth = 487
    object Label10: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 339
      Height = 13
      Align = alTop
      Caption = #1047#1072#1087#1086#1083#1085#1077#1085#1080#1077':'
      ExplicitLeft = 8
      ExplicitTop = 79
      ExplicitWidth = 64
    end
    object flwpnlFill: TFlowPanel
      Left = 0
      Top = 19
      Width = 345
      Height = 28
      Align = alTop
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 487
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
    Top = 184
    Width = 345
    Height = 47
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 280
    ExplicitWidth = 487
    object Label9: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 339
      Height = 13
      Align = alTop
      Caption = #1051#1080#1085#1080#1103':'
      ExplicitLeft = 8
      ExplicitTop = 49
      ExplicitWidth = 35
    end
    object flwpnlLine: TFlowPanel
      Left = 0
      Top = 19
      Width = 345
      Height = 28
      Align = alTop
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 487
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
    Width = 345
    Height = 132
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitTop = 192
    ExplicitWidth = 487
    ExplicitHeight = 103
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 345
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 5
    ExplicitLeft = 2
    ExplicitTop = 96
    ExplicitWidth = 477
    object Label7: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 56
      Height = 19
      Align = alLeft
      Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
      ExplicitLeft = 8
      ExplicitTop = 10
      ExplicitHeight = 13
    end
    object CBKateg: TComboBox
      AlignWithMargins = True
      Left = 65
      Top = 3
      Width = 277
      Height = 21
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      Text = #1053#1086#1074#1072#1103' '#1082#1072#1090#1077#1075#1086#1088#1080#1103
      ExplicitLeft = 72
      ExplicitTop = 8
      ExplicitWidth = 265
    end
  end
  object pnlName: TPanel
    Left = 0
    Top = 25
    Width = 345
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 6
    ExplicitLeft = 8
    ExplicitTop = 72
    ExplicitWidth = 457
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 25
      Height = 21
      Align = alLeft
      Caption = #1048#1084#1103':'
      ExplicitLeft = 9
      ExplicitTop = 14
      ExplicitHeight = 13
    end
    object EditName: TEdit
      AlignWithMargins = True
      Left = 34
      Top = 3
      Width = 308
      Height = 21
      Align = alClient
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      ExplicitLeft = 40
      ExplicitTop = 6
      ExplicitWidth = 297
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
