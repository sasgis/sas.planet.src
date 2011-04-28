object frMarkDescription: TfrMarkDescription
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object EditComment: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 31
    Width = 445
    Height = 270
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
    OnKeyDown = EditCommentKeyDown
  end
  object pnlDescriptionTop: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 53
      Height = 22
      Align = alLeft
      Caption = 'Description:'
      Layout = tlBottom
      ExplicitHeight = 13
    end
    object TBXToolbar1: TTBXToolbar
      AlignWithMargins = True
      Left = 252
      Top = 3
      Width = 196
      Height = 22
      Align = alRight
      Images = frmMain.EditCommentsImgs
      TabOrder = 0
      ExplicitLeft = 388
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
      object tbxtmInsertUrl: TTBXItem
        Tag = 7
        ImageIndex = 8
        OnClick = TBXItem1Click
        Caption = ''
        Hint = ''
      end
    end
  end
end
