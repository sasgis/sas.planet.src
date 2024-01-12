object frImageFormatSelect: TfrImageFormatSelect
  Left = 0
  Top = 0
  Width = 370
  Height = 99
  TabOrder = 0
  object pnlImageFormat: TPanel
    Left = 0
    Top = 0
    Width = 370
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblCompression: TLabel
      Left = 156
      Top = 3
      Width = 73
      Height = 15
      Caption = 'Compression:'
      Layout = tlCenter
    end
    object lblImageFormat: TLabel
      Left = 0
      Top = 3
      Width = 75
      Height = 15
      Caption = 'Image format:'
      Layout = tlCenter
    end
    object seCompression: TSpinEdit
      Left = 156
      Top = 19
      Width = 150
      Height = 24
      MaxValue = 9
      MinValue = 0
      TabOrder = 1
      Value = 6
    end
    object cbbImageFormat: TComboBox
      Left = 0
      Top = 19
      Width = 150
      Height = 23
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbbImageFormatChange
    end
  end
end
