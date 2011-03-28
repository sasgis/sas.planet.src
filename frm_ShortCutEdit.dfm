object frmShortCutEdit: TfrmShortCutEdit
  Left = 669
  Top = 266
  BorderStyle = bsDialog
  Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1075#1086#1088'. '#1082#1083'.'
  ClientHeight = 81
  ClientWidth = 189
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poDesktopCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 183
    Height = 44
    Align = alClient
    Caption = #1053#1072#1079#1085#1072#1095#1080#1090#1100' '#1089#1086#1095#1077#1090#1072#1085#1080#1077
    TabOrder = 0
    ExplicitLeft = 8
    object HotKey: THotKey
      AlignWithMargins = True
      Left = 8
      Top = 16
      Width = 167
      Height = 19
      Modifiers = []
      TabOrder = 0
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 50
    Width = 189
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 172
    ExplicitWidth = 201
    object btnClear: TSpeedButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 25
      Align = alLeft
      Caption = #1053#1077#1090
      Flat = True
      OnClick = btnClearClick
      ExplicitLeft = 8
      ExplicitTop = 6
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 121
      Top = 3
      Width = 65
      Height = 25
      Align = alRight
      Caption = #1055#1088#1080#1085#1103#1090#1100
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 128
      ExplicitTop = 16
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 50
      Top = 3
      Width = 65
      Height = 25
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 56
      ExplicitTop = 6
    end
  end
end
