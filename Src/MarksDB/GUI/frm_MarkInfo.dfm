object frmMarkInfo: TfrmMarkInfo
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Mark Info'
  ClientHeight = 333
  ClientWidth = 401
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  OnClose = FormClose
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object splDesc: TSplitter
    Left = 0
    Top = 94
    Width = 401
    Height = 4
    Cursor = crVSplit
    Align = alTop
  end
  object mmoInfo: TMemo
    Left = 0
    Top = 0
    Width = 401
    Height = 94
    Align = alTop
    ReadOnly = True
    TabOrder = 0
  end
  object pnlDesc: TPanel
    Left = 0
    Top = 98
    Width = 401
    Height = 235
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
end
