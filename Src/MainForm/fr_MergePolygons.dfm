object frMergePolygons: TfrMergePolygons
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object tvPolygonsList: TTreeView
    Left = 0
    Top = 23
    Width = 451
    Height = 281
    Align = alClient
    Indent = 19
    MultiSelect = True
    ReadOnly = True
    RightClickSelect = True
    RowSelect = True
    ShowRoot = False
    TabOrder = 0
    OnAddition = tvPolygonsListAddition
    OnDblClick = tvPolygonsListDblClick
  end
  object tbTop: TTBXToolbar
    Left = 0
    Top = 0
    Width = 451
    Height = 23
    Align = alTop
    TabOrder = 1
    Caption = 'tbTop'
    object tbxUp: TTBXItem
      OnClick = tbxUpClick
      Caption = 'Up'
      Hint = ''
    end
    object tbxDown: TTBXItem
      OnClick = tbxDownClick
      Caption = 'Down'
      Hint = ''
    end
    object tbxSep1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbxDel: TTBXItem
      OnClick = tbxDelClick
      Caption = 'Del'
      Hint = ''
    end
    object tbxSep2: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbxOperation: TTBXComboBoxItem
      DropDownList = True
      Lines.Strings = (
        'AND'
        'OR'
        'NOT'
        'XOR')
      Caption = ''
      Hint = ''
    end
    object tbxMerge: TTBXItem
      OnClick = tbxMergeClick
      Caption = 'Merge'
      Hint = ''
    end
  end
end
