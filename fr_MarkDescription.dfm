object frMarkDescription: TfrMarkDescription
  Left = 0
  Top = 0
  Width = 319
  Height = 213
  Align = alClient
  TabOrder = 0
  object EditComment: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 31
    Width = 313
    Height = 179
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = EditCommentKeyDown
    ExplicitLeft = -2
    ExplicitWidth = 322
    ExplicitHeight = 57
  end
  object pnlDescriptionTop: TPanel
    Left = 0
    Top = 0
    Width = 319
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -8
    ExplicitWidth = 328
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 53
      Height = 22
      Align = alLeft
      Caption = #1054#1087#1080#1089#1072#1085#1080#1077':'
      Layout = tlBottom
      ExplicitHeight = 13
    end
    object TBXToolbar1: TTBXToolbar
      AlignWithMargins = True
      Left = 143
      Top = 3
      Width = 173
      Height = 22
      Align = alRight
      Images = Fmain.EditCommentsImgs
      TabOrder = 0
      ExplicitLeft = 152
      object TBXItem3: TTBXItem
        ImageIndex = 0
        OnClick = TBXItem1Click
        Caption = ''
        Hint = ''
      end
      object TBXItem2: TTBXItem
        Tag = 1
        ImageIndex = 1
        OnClick = TBXItem1Click
        Caption = ''
        Hint = ''
      end
      object TBXItem1: TTBXItem
        Tag = 2
        ImageIndex = 2
        OnClick = TBXItem1Click
        Caption = ''
        Hint = ''
      end
      object TBXSeparatorItem1: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object TBXItem4: TTBXItem
        Tag = 3
        ImageIndex = 3
        OnClick = TBXItem1Click
        Caption = ''
        Hint = ''
      end
      object TBXItem5: TTBXItem
        Tag = 4
        ImageIndex = 4
        OnClick = TBXItem1Click
        Caption = ''
        Hint = ''
      end
      object TBXItem6: TTBXItem
        Tag = 5
        ImageIndex = 5
        OnClick = TBXItem1Click
        Caption = ''
        Hint = ''
      end
      object TBXSeparatorItem2: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object TBXItem7: TTBXItem
        Tag = 6
        ImageIndex = 7
        OnClick = TBXItem1Click
        Caption = ''
        Hint = ''
      end
    end
  end
end
