object frMapSelect: TfrMapSelect
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object cbbMap: TComboBox
    Left = 0
    Top = 0
    Width = 448
    Height = 21
    Align = alCustom
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 16
    ItemHeight = 13
    PopupMenu = FilterPopupMenu
    TabOrder = 0
    OnChange = cbbMapChange
  end
  object FilterPopupMenu: TTBXPopupMenu
    Tag = 5
    Left = 408
    Top = 224
    object TBX_All: TTBXItem
      Tag = 1
      Caption = 'All'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = RefreshList
    end
    object TBX_Maps: TTBXItem
      Tag = 2
      Caption = 'Maps'
      GroupIndex = 1
      RadioItem = True
      OnClick = RefreshList
    end
    object TBX_Layers: TTBXItem
      Tag = 3
      Caption = 'Layers'
      GroupIndex = 1
      RadioItem = True
      OnClick = RefreshList
    end
    object TBX_Active: TTBXItem
      Tag = 4
      Caption = 'Active'
      GroupIndex = 1
      RadioItem = True
      OnClick = RefreshList
    end
    object TBSeparatorItem1: TTBSeparatorItem
    end
    object TBX_Filter: TTBXItem
      Tag = 5
      Caption = 'Filter'
      GroupIndex = 1
      RadioItem = True
      OnClick = RefreshList
    end
    object TBX_AFilter: TTBXEditItem
      Tag = 5
      OnChange = ApplyFilter
    end
  end
end
