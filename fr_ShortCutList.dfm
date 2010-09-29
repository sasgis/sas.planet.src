object frShortCutList: TfrShortCutList
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object lstShortCutList: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 28
    Width = 445
    Height = 273
    Style = lbOwnerDrawFixed
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ItemHeight = 20
    ParentFont = False
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
      Width = 57
      Height = 13
      Align = alLeft
      Caption = #1054#1087#1077#1088#1072#1094#1080#1103
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object lblHotKey: TLabel
      AlignWithMargins = True
      Left = 405
      Top = 3
      Width = 43
      Height = 13
      Align = alRight
      Caption = #1043#1086#1088'. '#1082#1083'.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
  end
end
