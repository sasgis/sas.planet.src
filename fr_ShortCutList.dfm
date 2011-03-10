object frShortCutList: TfrShortCutList
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object lstShortCutList: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 28
    Width = 445
    Height = 273
    Style = lbOwnerDrawFixed
    Align = alClient
    ItemHeight = 20
    TabOrder = 0
    OnDblClick = lstShortCutListDblClick
    OnDrawItem = lstShortCutListDrawItem
  end
  object pnlHotKeysHeader: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblOperation: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 50
      Height = 19
      Align = alLeft
      Caption = #1054#1087#1077#1088#1072#1094#1080#1103
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object lblHotKey: TLabel
      AlignWithMargins = True
      Left = 359
      Top = 3
      Width = 89
      Height = 19
      Align = alRight
      Caption = #1043#1086#1088#1103#1095#1072#1103' '#1082#1083#1072#1074#1080#1096#1072
      Layout = tlCenter
      ExplicitLeft = 365
      ExplicitHeight = 13
    end
  end
end
