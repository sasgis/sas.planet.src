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
    TabOrder = 0
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
    Images = frmMain.MenusImageList
    TabOrder = 1
    Caption = 'tbTop'
    object tbUp: TTBItem
      ImageIndex = 47
      OnClick = tbUpClick
      Caption = ''
      Hint = 'Move selected item Up (Shift + Up Arrow)'
    end
    object tbDown: TTBItem
      ImageIndex = 48
      OnClick = tbDownClick
      Caption = ''
      Hint = 'Move selected item Down (Shift + Down Arrow)'
    end
    object tbxSep1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbDel: TTBItem
      ImageIndex = 50
      OnClick = tbDelClick
      Caption = ''
      Hint = 'Remove selected (Delete)'
    end
    object tbtmClear: TTBItem
      ImageIndex = 49
      OnClick = tbtmClearClick
      Caption = ''
      Hint = 'Remove All'
    end
    object tbxSep2: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbOperationType: TTBSubmenuItem
      DropdownCombo = True
      ImageIndex = 60
      Caption = ''
      Hint = ''
      object tbtmAND: TTBItem
        ImageIndex = 58
        OnClick = OnOperationClick
        Caption = 'Intersection (AND)'
        Hint = ''
      end
      object tbtmOR: TTBItem
        Tag = 1
        ImageIndex = 60
        OnClick = OnOperationClick
        Caption = 'Union (OR)'
        Hint = ''
      end
      object tbtmNOT: TTBItem
        Tag = 2
        ImageIndex = 59
        OnClick = OnOperationClick
        Caption = 'Difference (NOT)'
        Hint = ''
      end
      object tbtmXOR: TTBItem
        Tag = 3
        ImageIndex = 61
        OnClick = OnOperationClick
        Caption = 'Exclusive or (XOR)'
        Hint = ''
      end
      object tbSep4: TTBSeparatorItem
        Caption = ''
        Hint = ''
      end
      object tbtmGroup: TTBItem
        Tag = 4
        ImageIndex = 56
        OnClick = OnOperationClick
        Caption = 'Group'
        Hint = ''
      end
    end
    object tbMerge: TTBItem
      ImageIndex = 38
      OnClick = tbMergeClick
      Caption = 'Merge polygons'
      Hint = ''
    end
    object tbSep3: TTBSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbtmSelect: TTBItem
      ImageIndex = 10
      OnClick = tbtmSelectClick
      Caption = ''
      Hint = 'Selection Manager'
    end
    object tbtmSave: TTBItem
      ImageIndex = 25
      OnClick = tbtmSaveClick
      Caption = ''
      Hint = 'Save merged polygon as...'
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
