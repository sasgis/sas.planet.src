object FAddPoly: TFAddPoly
  Left = 360
  Top = 40
  BorderStyle = bsSizeToolWin
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1086#1083#1080#1075#1086#1085
  ClientHeight = 351
  ClientWidth = 327
  Color = clBtnFace
  Constraints.MinHeight = 375
  Constraints.MinWidth = 335
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object chkVisible: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 300
    Width = 321
    Height = 17
    Align = alBottom
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 5
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 320
    Width = 327
    Height = 31
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 6
    object btnOk: TButton
      AlignWithMargins = True
      Left = 172
      Top = 3
      Width = 73
      Height = 23
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 251
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
  object pnlFill: TPanel
    Left = 0
    Top = 248
    Width = 327
    Height = 49
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 4
    object lblFill: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 321
      Height = 13
      Align = alTop
      Caption = #1047#1072#1087#1086#1083#1085#1077#1085#1080#1077':'
      ExplicitWidth = 64
    end
    object flwpnlFill: TFlowPanel
      Left = 0
      Top = 19
      Width = 327
      Height = 30
      Align = alTop
      AutoSize = True
      AutoWrap = False
      BevelEdges = [beBottom]
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
      object lblFillColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 26
        Height = 13
        Caption = #1062#1074#1077#1090
      end
      object clrbxFillColor: TColorBox
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 38
        Height = 22
        Selected = clWhite
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 0
      end
      object btnFillColor: TSpeedButton
        AlignWithMargins = True
        Left = 79
        Top = 3
        Width = 17
        Height = 22
        Caption = '...'
        OnClick = btnFillColorClick
      end
      object lblFillTransp: TLabel
        AlignWithMargins = True
        Left = 102
        Top = 3
        Width = 85
        Height = 13
        Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
      end
      object seFillTransp: TSpinEdit
        AlignWithMargins = True
        Left = 193
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
    Top = 201
    Width = 327
    Height = 47
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 3
    object lblLine: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 321
      Height = 13
      Align = alTop
      Caption = #1051#1080#1085#1080#1103':'
      ExplicitWidth = 35
    end
    object flwpnlLine: TFlowPanel
      Left = 0
      Top = 19
      Width = 327
      Height = 28
      Align = alTop
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      object lblLineColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 26
        Height = 13
        Caption = #1062#1074#1077#1090
      end
      object clrbxLineColor: TColorBox
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 38
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 0
      end
      object btnLineColor: TSpeedButton
        AlignWithMargins = True
        Left = 79
        Top = 3
        Width = 17
        Height = 22
        Caption = '...'
        OnClick = btnLineColorClick
      end
      object lblLineWidth: TLabel
        AlignWithMargins = True
        Left = 102
        Top = 3
        Width = 40
        Height = 13
        Caption = #1064#1080#1088#1080#1085#1072
      end
      object seLineWidth: TSpinEdit
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
      object lblLineTransp: TLabel
        AlignWithMargins = True
        Left = 195
        Top = 3
        Width = 85
        Height = 13
        Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
      end
      object seLineTransp: TSpinEdit
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
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 52
    Width = 327
    Height = 149
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 327
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblCategory: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 58
      Height = 19
      Align = alLeft
      Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
      ExplicitHeight = 13
    end
    object CBKateg: TComboBox
      AlignWithMargins = True
      Left = 67
      Top = 3
      Width = 257
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
    Width = 327
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblName: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 23
      Height = 21
      Align = alLeft
      Caption = #1048#1084#1103':'
      ExplicitHeight = 13
    end
    object edtName: TEdit
      AlignWithMargins = True
      Left = 32
      Top = 3
      Width = 292
      Height = 21
      Align = alClient
      TabOrder = 0
    end
  end
  object ColorDialog1: TColorDialog
    Left = 88
    Top = 264
  end
end
