object frmProgressSimple: TfrmProgressSimple
  Left = 207
  Top = 161
  BorderStyle = bsToolWindow
  Caption = #1055#1086#1078#1072#1083#1091#1081#1089#1090#1072' '#1087#1086#1076#1086#1078#1076#1080#1090#1077'...'
  ClientHeight = 49
  ClientWidth = 329
  Color = clBtnFace
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  ParentFont = True
  Position = poScreenCenter
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TRarProgress
    Left = 6
    Top = 30
    Width = 315
    Height = 17
    Min = 0
    Max = 100
    Progress1 = 50
    Progress2 = 30
    Double = False
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
  object MemoInfo: TMemo
    Left = 8
    Top = 1
    Width = 313
    Height = 30
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      '')
    ReadOnly = True
    TabOrder = 0
    OnChange = MemoInfoChange
  end
end
