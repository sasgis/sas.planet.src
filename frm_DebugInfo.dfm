object frmDebugInfo: TfrmDebugInfo
  Left = 0
  Top = 0
  Caption = ''
  ClientHeight = 301
  ClientWidth = 566
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object sgrdDebugInfo: TStringGrid
    Left = 0
    Top = 0
    Width = 566
    Height = 272
    Align = alClient
    ColCount = 4
    DefaultColWidth = 130
    DefaultRowHeight = 20
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 272
    Width = 566
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 562
    object btnRefresh: TButton
      AlignWithMargins = True
      Left = 489
      Top = 2
      Width = 75
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = btnRefreshClick
      ExplicitLeft = 384
      ExplicitTop = 0
    end
  end
end
