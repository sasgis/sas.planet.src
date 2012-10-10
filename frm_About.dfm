object frmAbout: TfrmAbout
  Left = 268
  Top = 257
  BorderStyle = bsToolWindow
  Caption = 'About'
  ClientHeight = 157
  ClientWidth = 337
  Color = clWhite
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 123
    Width = 337
    Height = 3
    Align = alBottom
    Shape = bsTopLine
  end
  object lblVersionCatpion: TLabel
    Left = 129
    Top = 61
    Width = 39
    Height = 13
    Alignment = taRightJustify
    Caption = 'Version:'
  end
  object lblWebSiteCaption: TLabel
    Left = 125
    Top = 80
    Width = 43
    Height = 13
    Alignment = taRightJustify
    BiDiMode = bdLeftToRight
    Caption = 'Website:'
    ParentBiDiMode = False
  end
  object lblEMailCaption: TLabel
    Left = 140
    Top = 96
    Width = 28
    Height = 13
    Alignment = taRightJustify
    Caption = 'Email:'
  end
  object lblProgramName: TLabel
    Left = 0
    Top = 0
    Width = 337
    Height = 34
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'SAS.Planet'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'MS Reference Sans Serif'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object lblVersion: TLabel
    Left = 171
    Top = 61
    Width = 3
    Height = 13
  end
  object lblEMail: TLabel
    Left = 171
    Top = 96
    Width = 63
    Height = 13
    Cursor = crHandPoint
    Caption = 'az@sasgis.ru'
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentColor = False
    ParentFont = False
    OnClick = lblEMailClick
  end
  object lblWebSite: TLabel
    Left = 171
    Top = 80
    Width = 72
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://sasgis.ru'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblWebSiteClick
  end
  object Label1: TLabel
    Left = 0
    Top = 34
    Width = 337
    Height = 13
    Align = alTop
    Alignment = taCenter
    Caption = 'Copyright (C) 2007-2012, SAS.Planet development team'
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 126
    Width = 337
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    Padding.Bottom = 2
    ParentColor = True
    TabOrder = 0
    DesignSize = (
      337
      31)
    object btnClose: TButton
      Left = 121
      Top = 2
      Width = 95
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Cancel = True
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
end
