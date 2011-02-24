object FProgress: TFProgress
  Left = 226
  Top = 306
  BorderStyle = bsToolWindow
  Caption = #1055#1086#1078#1072#1083#1091#1081#1089#1090#1072' '#1087#1086#1076#1086#1078#1076#1080#1090#1077'...'
  ClientHeight = 250
  ClientWidth = 330
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 330
    Height = 250
    Align = alClient
    TabOrder = 0
    object RProgr: TRarProgress
      Left = 7
      Top = 204
      Width = 314
      Height = 17
      Min = 0
      Max = 100
      Progress1 = 50
      Progress2 = 30
      Double = True
      LightColor1 = 16770764
      DarkColor1 = 13395456
      LightColor2 = 16768959
      FrameColor1 = 16758122
      FrameColor2 = 16747546
      FillColor1 = 16757606
      FillColor2 = 16749867
      BackFrameColor1 = 16633762
      BackFrameColor2 = 16634540
      BackFillColor = 16635571
      ShadowColor = clGray
    end
    object LabelValue0: TLabel
      Left = 315
      Top = 121
      Width = 6
      Height = 13
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelValue1: TLabel
      Left = 315
      Top = 137
      Width = 6
      Height = 13
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelValue2: TLabel
      Left = 315
      Top = 153
      Width = 6
      Height = 13
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelValue3: TLabel
      Left = 315
      Top = 169
      Width = 6
      Height = 13
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName0: TLabel
      Left = 11
      Top = 121
      Width = 6
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName1: TLabel
      Left = 11
      Top = 137
      Width = 3
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = ' '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName2: TLabel
      Left = 11
      Top = 153
      Width = 6
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName3: TLabel
      Left = 11
      Top = 169
      Width = 9
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = '   '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelName4: TLabel
      Left = 11
      Top = 185
      Width = 9
      Height = 13
      BiDiMode = bdLeftToRight
      Caption = '   '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object LabelValue4: TLabel
      Left = 315
      Top = 185
      Width = 6
      Height = 13
      Alignment = taRightJustify
      BiDiMode = bdLeftToRight
      Caption = '  '
      ParentBiDiMode = False
      Layout = tlCenter
    end
    object Memo1: TMemo
      Left = 8
      Top = 8
      Width = 313
      Height = 105
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Button1: TButton
      Left = 168
      Top = 223
      Width = 73
      Height = 20
      Caption = #1057#1090#1086#1087
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 248
      Top = 223
      Width = 73
      Height = 20
      Caption = #1042#1099#1093#1086#1076
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 8
      Top = 223
      Width = 73
      Height = 20
      Caption = #1057#1074#1077#1088#1085#1091#1090#1100
      TabOrder = 3
      OnClick = Button3Click
    end
    object ButtonSave: TButton
      Left = 88
      Top = 223
      Width = 73
      Height = 20
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1090#1077#1082#1091#1097#1091#1102' '#1089#1077#1089#1089#1080#1102
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      TabOrder = 4
      OnClick = ButtonSaveClick
    end
  end
  object SaveSessionDialog: TSaveDialog
    DefaultExt = '*.sls'
    Filter = #1057#1077#1089#1089#1080#1103' '#1079#1072#1075#1088#1091#1079#1082#1080' (*.sls)|*.sls'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 280
    Top = 184
  end
  object UpdateTimer: TTimer
    OnTimer = UpdateTimerTimer
    Left = 240
    Top = 184
  end
end
