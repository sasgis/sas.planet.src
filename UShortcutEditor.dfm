object FShortcutChange: TFShortcutChange
  Left = 669
  Top = 266
  BorderStyle = bsDialog
  Caption = #1048#1079#1084#1077#1085#1080#1090#1100' '#1075#1086#1088'. '#1082#1083'.'
  ClientHeight = 88
  ClientWidth = 201
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 8
    Top = 56
    Width = 41
    Height = 25
    Caption = #1053#1077#1090
    Flat = True
    OnClick = Button3Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 185
    Height = 41
    Caption = ' '#1053#1072#1079#1072#1085#1095#1080#1090#1100' '#1089#1086#1095#1077#1090#1072#1085#1080#1077' '
    TabOrder = 0
    object HotKey: THotKey
      Left = 8
      Top = 16
      Width = 169
      Height = 19
      Modifiers = []
      TabOrder = 0
    end
  end
  object Button1: TButton
    Left = 128
    Top = 56
    Width = 65
    Height = 25
    Caption = #1055#1088#1080#1085#1103#1090#1100
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 56
    Top = 56
    Width = 65
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    ModalResult = 2
    TabOrder = 2
  end
end
