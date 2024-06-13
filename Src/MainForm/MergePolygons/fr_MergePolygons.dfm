object frMergePolygons: TfrMergePolygons
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object tvPolygonsList: TTreeView
    Left = 0
    Top = 24
    Width = 451
    Height = 280
    Align = alClient
    DragMode = dmAutomatic
    Indent = 19
    MultiSelect = True
    ReadOnly = True
    RightClickSelect = True
    RowSelect = True
    ShowRoot = False
    TabOrder = 1
    OnAddition = tvPolygonsListAddition
    OnDblClick = tvPolygonsListDblClick
    OnDragDrop = tvPolygonsListDragDrop
    OnDragOver = tvPolygonsListDragOver
    OnKeyDown = tvPolygonsListKeyDown
  end
  object tbTop: TTBXToolbar
    Left = 0
    Top = 0
    Width = 451
    Height = 24
    Align = alTop
    BorderStyle = bsNone
    Images = frmMain.MenusImageList
    ItemTransparency = itEnable
    Stretch = True
    TabOrder = 0
    object tbUp: TTBXItem
      Hint = 'Move selected item Up (Shift + Up Arrow)'
      ImageIndex = 47
      OnClick = tbUpClick
    end
    object tbDown: TTBXItem
      Hint = 'Move selected item Down (Shift + Down Arrow)'
      ImageIndex = 48
      OnClick = tbDownClick
    end
    object tbxSep1: TTBXSeparatorItem
    end
    object tbDel: TTBXItem
      Hint = 'Remove selected (Delete)'
      ImageIndex = 50
      OnClick = tbDelClick
    end
    object tbtmClear: TTBXItem
      Hint = 'Remove All'
      ImageIndex = 49
      OnClick = tbtmClearClick
    end
    object tbxSep2: TTBXSeparatorItem
    end
    object tbOperationType: TTBXSubmenuItem
      ImageIndex = 60
      Options = [tboDropdownArrow]
      object tbtmAND: TTBXItem
        Caption = 'Intersection (AND)'
        ImageIndex = 58
        OnClick = OnOperationClick
      end
      object tbtmOR: TTBXItem
        Tag = 1
        Caption = 'Union (OR)'
        ImageIndex = 60
        OnClick = OnOperationClick
      end
      object tbtmNOT: TTBXItem
        Tag = 2
        Caption = 'Difference (NOT)'
        ImageIndex = 59
        OnClick = OnOperationClick
      end
      object tbtmXOR: TTBXItem
        Tag = 3
        Caption = 'Exclusive or (XOR)'
        ImageIndex = 61
        OnClick = OnOperationClick
      end
      object tbSep4: TTBXSeparatorItem
      end
      object tbtmGroup: TTBXItem
        Tag = 4
        Caption = 'Group'
        ImageIndex = 56
        OnClick = OnOperationClick
      end
    end
    object tbMerge: TTBXItem
      Caption = 'Merge polygons'
      ImageIndex = 38
      OnClick = tbMergeClick
    end
    object tbSep3: TTBXSeparatorItem
    end
    object tbtmSelect: TTBXItem
      Hint = 'Selection Manager'
      ImageIndex = 10
      OnClick = tbtmSelectClick
    end
    object tbtmSave: TTBXItem
      Hint = 'Save merged polygon as...'
      ImageIndex = 25
      OnClick = tbtmSaveClick
    end
  end
  object tmrProgressCheck: TTimer
    Enabled = False
    Interval = 150
    OnTimer = tmrProgressCheckTimer
    Left = 8
    Top = 32
  end
end
