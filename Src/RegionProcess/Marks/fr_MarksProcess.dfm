object frMarksProcess: TfrMarksProcess
  Left = 0
  Top = 0
  Width = 480
  Height = 156
  Align = alClient
  Constraints.MinHeight = 100
  Constraints.MinWidth = 480
  TabOrder = 0
  object chkIncludeHidden: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 88
    Width = 474
    Height = 17
    Align = alTop
    Caption = 'Include hidden placemarks'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object pnlTop: TPanel
    Left = 0
    Top = 29
    Width = 480
    Height = 56
    Align = alTop
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 0
    object chkPlacemarks: TCheckBox
      Left = 0
      Top = 0
      Width = 476
      Height = 17
      Align = alTop
      Caption = 'Placemarks'
      TabOrder = 0
    end
    object chkPaths: TCheckBox
      Left = 0
      Top = 17
      Width = 476
      Height = 17
      Align = alTop
      Caption = 'Paths'
      TabOrder = 1
    end
    object chkPolygons: TCheckBox
      Left = 0
      Top = 34
      Width = 476
      Height = 17
      Align = alTop
      Caption = 'Polygons'
      TabOrder = 2
    end
  end
  object pnlOperation: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 29
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    object lblOperation: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 52
      Height = 26
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Operation:'
      Constraints.MinWidth = 50
      Layout = tlCenter
    end
    object cbbOperation: TComboBox
      AlignWithMargins = True
      Left = 58
      Top = 3
      Width = 419
      Height = 21
      Align = alClient
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Export'
      OnChange = cbbOperationChange
      Items.Strings = (
        'Export'
        'Copy'
        'Move'
        'Delete')
    end
  end
  object pnlCategory: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 111
    Width = 474
    Height = 30
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 3
  end
end
