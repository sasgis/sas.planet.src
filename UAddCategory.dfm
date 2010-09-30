object FAddCategory: TFAddCategory
  Left = 208
  Top = 318
  BorderStyle = bsDialog
  Caption = 'FAddCategory'
  ClientHeight = 140
  ClientWidth = 295
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
  object Label2: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 30
    Width = 289
    Height = 13
    Align = alTop
    Caption = #1052#1072#1089#1096#1090#1072#1073#1099' '#1074#1080#1076#1080#1084#1086#1089#1090#1080':'
    ExplicitLeft = 16
    ExplicitTop = 56
    ExplicitWidth = 115
  end
  object Bevel5: TBevel
    Left = 0
    Top = 100
    Width = 295
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 106
    ExplicitWidth = 369
  end
  object CBShow: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 80
    Width = 289
    Height = 17
    Align = alBottom
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 88
    ExplicitWidth = 137
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 109
    Width = 295
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 299
    ExplicitWidth = 382
    object Button2: TButton
      AlignWithMargins = True
      Left = 219
      Top = 3
      Width = 73
      Height = 25
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 0
      OnClick = Button2Click
      ExplicitLeft = 304
      ExplicitTop = 6
    end
    object Badd: TButton
      AlignWithMargins = True
      Left = 140
      Top = 3
      Width = 73
      Height = 25
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      ModalResult = 1
      TabOrder = 1
      OnClick = BaddClick
      ExplicitLeft = 229
      ExplicitTop = 6
    end
  end
  object flwpnlZooms: TFlowPanel
    Left = 0
    Top = 46
    Width = 295
    Height = 31
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 24
    ExplicitTop = 152
    ExplicitWidth = 265
    ExplicitHeight = 41
    object Label3: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 6
      Height = 13
      Caption = 'c'
    end
    object EditS1: TSpinEdit
      AlignWithMargins = True
      Left = 15
      Top = 3
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 0
      Value = 3
    end
    object Label4: TLabel
      AlignWithMargins = True
      Left = 62
      Top = 3
      Width = 12
      Height = 13
      Caption = #1076#1086
    end
    object EditS2: TSpinEdit
      AlignWithMargins = True
      Left = 80
      Top = 3
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 1
      Value = 18
    end
  end
  object pnlName: TPanel
    Left = 0
    Top = 0
    Width = 295
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitWidth = 382
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 50
      Height = 21
      Align = alLeft
      Caption = #1053#1072#1079#1074#1072#1085#1080#1077
      Layout = tlCenter
      ExplicitLeft = 16
      ExplicitTop = -4
      ExplicitHeight = 33
    end
    object EditName: TEdit
      AlignWithMargins = True
      Left = 59
      Top = 3
      Width = 233
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 45
      ExplicitTop = 16
      ExplicitWidth = 297
    end
  end
end
