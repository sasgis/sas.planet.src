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
      Width = 22
      Height = 13
      Align = alLeft
      Caption = 'Task'
      Layout = tlCenter
    end
    object lblHotKey: TLabel
      AlignWithMargins = True
      Left = 414
      Top = 3
      Width = 34
      Height = 13
      Align = alRight
      Caption = 'Hotkey'
      Layout = tlCenter
    end
  end
end
