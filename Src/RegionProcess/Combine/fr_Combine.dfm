object frCombine: TfrCombine
  Left = 0
  Top = 0
  Width = 480
  Height = 60
  Align = alClient
  Constraints.MinHeight = 60
  Constraints.MinWidth = 480
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    AutoSize = True
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = 'Stitch selection'
    TabOrder = 0
  end
  object pnlOutputFormat: TPanel
    Left = 0
    Top = 22
    Width = 480
    Height = 27
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object lblOutputFormat: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 73
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Output format:'
      Layout = tlCenter
    end
    object cbbOutputFormat: TComboBox
      Left = 79
      Top = 3
      Width = 398
      Height = 21
      Align = alClient
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbOutputFormatChange
    end
  end
  object pnlExport: TPanel
    Left = 0
    Top = 49
    Width = 480
    Height = 11
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
  end
end
