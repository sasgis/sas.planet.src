object frExport: TfrExport
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 552
  ExplicitHeight = 240
  object Bevel5: TBevel
    Left = 0
    Top = 0
    Width = 552
    Height = 31
    Align = alTop
    Shape = bsBottomLine
    ExplicitLeft = -244
    ExplicitWidth = 564
  end
  object Label9: TLabel
    Left = 5
    Top = 8
    Width = 125
    Height = 13
    Caption = 'Export selection to format'
  end
  object CBFormat: TComboBox
    Left = 71
    Top = 4
    Width = 249
    Height = 21
    Style = csDropDownList
    DropDownCount = 11
    ItemHeight = 0
    TabOrder = 0
    OnChange = CBFormatChange
  end
  object pnlExport: TPanel
    Left = 0
    Top = 31
    Width = 451
    Height = 273
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -244
    ExplicitTop = 20
    ExplicitWidth = 564
    ExplicitHeight = 220
  end
end
