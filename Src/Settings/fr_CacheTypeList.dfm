object frCacheTypeList: TfrCacheTypeList
  Left = 0
  Top = 0
  Align = alClient
  Height = 70
  Width = 350
  object cbbCacheType: TComboBox
    Left = 0
    Top = 0
    Width = 350
    Height = 21
    Align = alTop
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbbCacheTypeChange
  end
end
