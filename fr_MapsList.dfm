object frMapsList: TfrMapsList
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object pnlMapsRightButtons: TPanel
    Left = 609
    Top = 0
    Width = 97
    Height = 298
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object Button15: TButton
      AlignWithMargins = True
      Left = 3
      Top = 65
      Width = 91
      Height = 25
      Align = alTop
      Caption = 'Settings'
      TabOrder = 0
      OnClick = Button15Click
    end
    object Button11: TButton
      AlignWithMargins = True
      Left = 3
      Top = 34
      Width = 91
      Height = 25
      Align = alTop
      Caption = 'Down'
      TabOrder = 1
      OnClick = Button11Click
    end
    object Button12: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 91
      Height = 25
      Align = alTop
      Caption = 'Up'
      TabOrder = 2
      OnClick = Button12Click
    end
    object btnMapInfo: TButton
      AlignWithMargins = True
      Left = 3
      Top = 96
      Width = 91
      Height = 25
      Align = alTop
      Caption = 'Info'
      TabOrder = 3
      OnClick = btnMapInfoClick
    end
  end
  object MapList: TListView
    Left = 0
    Top = 0
    Width = 609
    Height = 298
    Align = alClient
    Columns = <
      item
        Caption = 'Map Name'
        Width = 130
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
      end
      item
        Caption = 'Enabled'
      end>
    FlatScrollBars = True
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = MapListChange
    OnCustomDrawSubItem = MapListCustomDrawSubItem
    OnDblClick = MapListDblClick
  end
end
