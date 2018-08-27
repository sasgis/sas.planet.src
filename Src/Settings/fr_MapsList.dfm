object frMapsList: TfrMapsList
  Left = 0
  Top = 0
  Width = 580
  Height = 304
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 451
  object pnlMapsRightButtons: TPanel
    Left = 455
    Top = 0
    Width = 125
    Height = 304
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object lblSortingOrder: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 127
      Width = 119
      Height = 13
      Align = alTop
      Caption = 'Order by:'
      ExplicitWidth = 47
    end
    object btnSettings: TButton
      AlignWithMargins = True
      Left = 3
      Top = 65
      Width = 119
      Height = 25
      Align = alTop
      Caption = 'Settings'
      TabOrder = 2
      OnClick = btnSettingsClick
      ExplicitWidth = 91
    end
    object btnDown: TButton
      AlignWithMargins = True
      Left = 3
      Top = 34
      Width = 119
      Height = 25
      Align = alTop
      Caption = 'Down'
      TabOrder = 1
      OnClick = btnDownClick
      ExplicitWidth = 91
    end
    object btnUp: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 119
      Height = 25
      Align = alTop
      Caption = 'Up'
      TabOrder = 0
      OnClick = btnUpClick
      ExplicitWidth = 91
    end
    object btnMapInfo: TButton
      AlignWithMargins = True
      Left = 3
      Top = 96
      Width = 119
      Height = 25
      Align = alTop
      Caption = 'Info'
      TabOrder = 3
      OnClick = btnMapInfoClick
      ExplicitWidth = 91
    end
    object cbbSortingOrder: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 143
      Width = 119
      Height = 21
      Margins.Top = 0
      Align = alTop
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 4
      Text = 'Map Number'
      OnChange = cbbSortingOrderChange
      Items.Strings = (
        'Map Number'
        'Map Name'
        'Zmp Name')
      ExplicitLeft = 8
      ExplicitTop = 152
      ExplicitWidth = 145
    end
  end
  object MapList: TListView
    Left = 0
    Top = 0
    Width = 455
    Height = 304
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Caption = '#'
      end
      item
        Caption = 'Map Name'
        Width = 170
      end
      item
        Caption = 'Cache Folder'
        Width = 85
      end
      item
        Caption = 'Menu'
        Width = 110
      end
      item
        Caption = 'Hotkey'
        Width = 53
      end
      item
        Caption = 'ZMP Filename'
        Width = 100
      end>
    FlatScrollBars = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = MapListChange
    OnColumnClick = MapListColumnClick
    OnCustomDrawItem = MapListCustomDrawItem
    OnCustomDrawSubItem = MapListCustomDrawSubItem
    OnDblClick = MapListDblClick
    ExplicitWidth = 354
  end
end
