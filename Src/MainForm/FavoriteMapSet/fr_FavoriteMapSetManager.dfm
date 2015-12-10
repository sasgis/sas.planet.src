object frFavoriteMapSetManager: TfrFavoriteMapSetManager
  Left = 0
  Top = 0
  Width = 625
  Height = 438
  Align = alClient
  TabOrder = 0
  ExplicitTop = 3
  ExplicitWidth = 0
  ExplicitHeight = 0
  object spl1: TSplitter
    Left = 0
    Top = 229
    Width = 625
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = -9
    ExplicitWidth = 563
  end
  object pnlRightButtons: TPanel
    Left = 524
    Top = 0
    Width = 101
    Height = 229
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 363
    ExplicitHeight = 216
    object btnEdit: TButton
      AlignWithMargins = True
      Left = 3
      Top = 127
      Width = 95
      Height = 25
      Align = alTop
      Caption = 'Edit'
      TabOrder = 0
      OnClick = btnEditClick
      ExplicitLeft = 6
      ExplicitTop = 51
      ExplicitWidth = 135
    end
    object btnAdd: TButton
      AlignWithMargins = True
      Left = 3
      Top = 65
      Width = 95
      Height = 25
      Align = alTop
      Caption = 'Add'
      TabOrder = 1
      OnClick = btnAddClick
      ExplicitTop = 43
      ExplicitWidth = 132
    end
    object btnDelete: TButton
      AlignWithMargins = True
      Left = 3
      Top = 96
      Width = 95
      Height = 25
      Align = alTop
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteClick
      ExplicitTop = 25
      ExplicitWidth = 135
    end
    object btnUp: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 95
      Height = 25
      Align = alTop
      Caption = 'Up'
      TabOrder = 3
      OnClick = btnUpClick
      ExplicitWidth = 138
    end
    object btnDown: TButton
      AlignWithMargins = True
      Left = 3
      Top = 34
      Width = 95
      Height = 25
      Align = alTop
      Caption = 'Down'
      TabOrder = 4
      OnClick = btnDownClick
      ExplicitTop = 15
      ExplicitWidth = 132
    end
    object btnSwitchOn: TButton
      AlignWithMargins = True
      Left = 3
      Top = 158
      Width = 95
      Height = 25
      Align = alTop
      Caption = 'Switch On'
      TabOrder = 5
      OnClick = btnSwitchOnClick
      ExplicitLeft = 24
      ExplicitTop = 168
      ExplicitWidth = 75
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 232
    Width = 625
    Height = 183
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'pnlBottom'
    TabOrder = 1
    ExplicitWidth = 420
    object lvInfo: TListView
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 619
      Height = 177
      Align = alClient
      Columns = <
        item
          Caption = 'Parameter'
          Width = 120
        end
        item
          AutoSize = True
          Caption = 'Value'
        end>
      FlatScrollBars = True
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      ExplicitTop = 6
      ExplicitWidth = 458
      ExplicitHeight = 156
    end
  end
  object pnlMapSets: TPanel
    Left = 0
    Top = 0
    Width = 524
    Height = 229
    Align = alClient
    BevelOuter = bvNone
    Caption = 'pnlMapSets'
    TabOrder = 2
    ExplicitLeft = 32
    ExplicitTop = 72
    ExplicitWidth = 185
    ExplicitHeight = 41
    object lvMapSets: TListView
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 518
      Height = 223
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          Width = 400
        end
        item
          Caption = 'Hotkey'
          Width = 100
        end>
      FlatScrollBars = True
      GridLines = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = lvMapSetsClick
      OnDblClick = lvMapSetsDblClick
      ExplicitWidth = 313
    end
  end
  object chkEditByDblClick: TCheckBox
    AlignWithMargins = True
    Left = 6
    Top = 418
    Width = 616
    Height = 17
    Margins.Left = 6
    Align = alBottom
    Caption = 'Edit / Switch On by double click'
    Checked = True
    State = cbChecked
    TabOrder = 3
    ExplicitLeft = 16
    ExplicitTop = 375
    ExplicitWidth = 420
  end
end
