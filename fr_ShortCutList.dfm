object frShortCutList: TfrShortCutList
  Left = 0
  Top = 0
  Width = 567
  Height = 392
  TabOrder = 0
  object lstShortCutList: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 28
    Width = 561
    Height = 361
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
    ExplicitLeft = -39
    ExplicitTop = 43
    ExplicitWidth = 606
    ExplicitHeight = 275
  end
  object pnlHotKeysHeader: TPanel
    Left = 0
    Top = 0
    Width = 567
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -45
    ExplicitTop = 15
    ExplicitWidth = 612
    object lblOperation: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 57
      Height = 19
      Align = alLeft
      Caption = #1054#1087#1077#1088#1072#1094#1080#1103
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object lblHotKey: TLabel
      AlignWithMargins = True
      Left = 521
      Top = 3
      Width = 43
      Height = 19
      Align = alRight
      Caption = #1043#1086#1088'. '#1082#1083'.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 566
      ExplicitHeight = 13
    end
  end
end
