object frmAbout: TfrmAbout
  Left = 268
  Top = 257
  BorderStyle = bsToolWindow
  Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
  ClientHeight = 223
  ClientWidth = 314
  Color = clWhite
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 186
    Width = 314
    Height = 3
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 179
  end
  object lblVersionCatpion: TLabel
    Left = 121
    Top = 40
    Width = 39
    Height = 13
    Alignment = taRightJustify
    Caption = #1042#1077#1088#1089#1080#1103':'
  end
  object lblAuthorCaption: TLabel
    Left = 97
    Top = 56
    Width = 63
    Height = 13
    Alignment = taRightJustify
    Caption = #1056#1072#1079#1088#1072#1073#1086#1090#1082#1072':'
  end
  object lblWebSiteCaption: TLabel
    Left = 131
    Top = 72
    Width = 29
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = #1057#1072#1081#1090':'
    ParentBiDiMode = False
  end
  object lblEMailCaption: TLabel
    Left = 125
    Top = 88
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Caption = #1055#1086#1095#1090#1072':'
  end
  object lblDonateCaption: TLabel
    Left = 98
    Top = 104
    Width = 62
    Height = 13
    Alignment = taRightJustify
    Caption = #1055#1086#1084#1086#1095#1100' '#1085#1072#1084':'
  end
  object lblProgramName: TLabel
    Left = 0
    Top = 0
    Width = 314
    Height = 34
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'SAS.'#1055#1083#1072#1085#1077#1090#1072
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Reference Sans Serif'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object lblWMCaption: TLabel
    Left = 102
    Top = 120
    Width = 58
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'WebMoney:'
    ParentBiDiMode = False
  end
  object lblVersion: TLabel
    Left = 163
    Top = 40
    Width = 3
    Height = 13
  end
  object lblAuthor: TLabel
    Left = 163
    Top = 56
    Width = 58
    Height = 13
    Caption = #1043#1088#1091#1087#1087#1072' SAS'
  end
  object lblEMail: TLabel
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
    OnClick = lblEMailClick
  end
  object lblYandexMoneyCaption: TLabel
    Left = 78
    Top = 163
    Width = 82
    Height = 13
    Alignment = taRightJustify
    Caption = #1071#1085#1076#1077#1082#1089'.'#1044#1077#1085#1100#1075#1080':'
  end
  object lblWebSite: TLabel
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
    OnClick = lblWebSiteClick
  end
  object edtWME: TEdit
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
    TabOrder = 0
    Text = 'WME: E382109079322'
  end
  object edtWMZ: TEdit
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
    TabOrder = 1
    Text = 'WMZ: Z122595752786'
  end
  object edtYandexMoney: TEdit
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
    TabOrder = 2
    Text = '41001292446592'
  end
  object edtWMR: TEdit
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
    TabOrder = 3
    Text = 'WMR: R112587212279'
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 189
    Width = 314
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 4
    object btnClose: TButton
      AlignWithMargins = True
      Left = 100
      Top = 3
      Width = 114
      Height = 28
      Margins.Left = 100
      Margins.Right = 100
      Align = alClient
      Cancel = True
      Caption = #1047#1072#1082#1088#1099#1090#1100
      TabOrder = 0
      OnClick = btnCloseClick
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitWidth = 312
      ExplicitHeight = 25
    end
  end
end
