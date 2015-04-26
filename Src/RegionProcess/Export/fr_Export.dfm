object frExport: TfrExport
  Left = 0
  Top = 0
  Width = 480
  Height = 80
  Align = alClient
  Constraints.MinHeight = 80
  Constraints.MinWidth = 480
  TabOrder = 0
  object pnlExport: TPanel
    Left = 0
    Top = 30
    Width = 480
    Height = 50
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 30
    Align = alTop
    TabOrder = 1
    object Label9: TLabel
      Left = 1
      Top = 1
      Width = 125
      Height = 28
      Align = alLeft
      Caption = 'Export selection to format'
      Layout = tlCenter
    end
    object CBFormat: TComboBox
      AlignWithMargins = True
      Left = 129
      Top = 4
      Width = 347
      Height = 21
      Align = alClient
      Style = csDropDownList
      DropDownCount = 12
      ItemHeight = 13
      TabOrder = 0
      OnChange = CBFormatChange
    end
  end
end
