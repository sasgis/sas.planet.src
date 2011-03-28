object FAddCategory: TFAddCategory
  Left = 208
  Top = 318
  BorderStyle = bsDialog
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1091#1102' '#1082#1072#1090#1077#1075#1086#1088#1080#1102
  ClientHeight = 140
  ClientWidth = 295
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
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
    ExplicitWidth = 114
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
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 109
    Width = 295
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel: TButton
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
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 140
      Top = 3
      Width = 73
      Height = 25
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      Default = True
      ModalResult = 1
      TabOrder = 1
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
    object Label3: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 5
      Height = 13
      Caption = 'c'
    end
    object EditS1: TSpinEdit
      AlignWithMargins = True
      Left = 14
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
      Left = 61
      Top = 3
      Width = 13
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
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 48
      Height = 21
      Align = alLeft
      Caption = #1053#1072#1079#1074#1072#1085#1080#1077
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object EditName: TEdit
      AlignWithMargins = True
      Left = 57
      Top = 3
      Width = 235
      Height = 21
      Align = alClient
      TabOrder = 0
    end
  end
end
