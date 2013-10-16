object frmAbout: TfrmAbout
  Left = 268
  Top = 257
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 243
  ClientWidth = 336
  Color = clWhite
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 209
    Width = 336
    Height = 3
    Align = alBottom
    Shape = bsTopLine
    ExplicitTop = 123
    ExplicitWidth = 337
  end
  object lblVersionCatpion: TLabel
    Left = 16
    Top = 128
    Width = 39
    Height = 13
    Alignment = taRightJustify
    Caption = 'Version:'
  end
  object lblProgramName: TLabel
    Left = 0
    Top = 0
    Width = 336
    Height = 33
    Align = alCustom
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'SAS.Planet'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Layout = tlCenter
  end
  object lblVersion: TLabel
    Left = 130
    Top = 128
    Width = 198
    Height = 13
  end
  object lblWebSite: TLabel
    Left = 99
    Top = 70
    Width = 229
    Height = 13
    Cursor = crHandPoint
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblWebSiteClick
  end
  object lblCopyright: TLabel
    Left = 99
    Top = 36
    Width = 229
    Height = 33
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    WordWrap = True
  end
  object lblLicense: TLabel
    Left = 16
    Top = 88
    Width = 300
    Height = 26
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This program is freeware, opensource and released under the GNU ' +
      'General Public License (GPLv3)'
    WordWrap = True
  end
  object lblCompiler: TLabel
    Left = 16
    Top = 176
    Width = 45
    Height = 13
    Caption = 'Compiler:'
  end
  object lblTimeStamp: TLabel
    Left = 16
    Top = 144
    Width = 51
    Height = 13
    Caption = 'Build date:'
  end
  object lblBuildInfo: TLabel
    Left = 16
    Top = 160
    Width = 47
    Height = 13
    Caption = 'Build info:'
  end
  object lblBuildTimeValue: TLabel
    Left = 130
    Top = 144
    Width = 198
    Height = 13
    AutoSize = False
  end
  object lblBuildInfoValue: TLabel
    Left = 130
    Top = 160
    Width = 198
    Height = 13
    AutoSize = False
  end
  object lblCompilerValue: TLabel
    Left = 130
    Top = 176
    Width = 198
    Height = 13
    AutoSize = False
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 212
    Width = 336
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    Padding.Bottom = 2
    ParentColor = True
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 132
    ExplicitWidth = 337
    object btnClose: TButton
      Left = 198
      Top = 2
      Width = 130
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
      ExplicitLeft = 209
    end
    object btnLicense: TButton
      Left = 8
      Top = 3
      Width = 130
      Height = 25
      Align = alCustom
      Anchors = [akLeft, akBottom]
      Caption = 'License'
      TabOrder = 1
      OnClick = btnLicenseClick
    end
  end
  object imgLogo: TImage32
    Left = 16
    Top = 8
    Width = 64
    Height = 64
    Margins.Bottom = 5
    Align = alCustom
    Bitmap.DrawMode = dmBlend
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 1
  end
end
