object frExport: TfrExport
  Left = 0
  Top = 0
  Width = 458
  Height = 273
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 451
  ExplicitHeight = 304
  object pnlExport: TPanel
    Left = 0
    Top = 30
    Width = 458
    Height = 243
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = -3
    ExplicitTop = 41
    ExplicitWidth = 452
    ExplicitHeight = 232
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 458
    Height = 30
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 452
    object Label9: TLabel
      Left = 1
      Top = 1
      Width = 125
      Height = 28
      Align = alLeft
      Caption = 'Export selection to format'
      Layout = tlCenter
      ExplicitLeft = 5
      ExplicitTop = 8
      ExplicitHeight = 13
    end
    object CBFormat: TComboBox
      AlignWithMargins = True
      Left = 129
      Top = 4
      Width = 325
      Height = 21
      Align = alClient
      Style = csDropDownList
      DropDownCount = 11
      ItemHeight = 13
      TabOrder = 0
      OnChange = CBFormatChange
      ExplicitLeft = 203
      ExplicitTop = 5
      ExplicitWidth = 249
    end
  end
end
