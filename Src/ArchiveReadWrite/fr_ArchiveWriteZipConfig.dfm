object frArchiveWriteZipConfig: TfrArchiveWriteZipConfig
  Left = 0
  Top = 0
  Width = 336
  Height = 109
  TabOrder = 0
  object lblCompressLevel: TLabel
    Left = 3
    Top = 5
    Width = 93
    Height = 13
    Caption = 'Compression Level:'
  end
  object lblCompressMethod: TLabel
    Left = 3
    Top = 33
    Width = 104
    Height = 13
    Caption = 'Compression Method:'
  end
  object lblSplitToVolumes: TLabel
    Left = 3
    Top = 60
    Width = 100
    Height = 13
    Caption = 'Split to volumes, Mb:'
  end
  object cbbCompressLevel: TComboBox
    Left = 188
    Top = 3
    Width = 145
    Height = 21
    Align = alCustom
    Style = csDropDownList
    Anchors = [akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbbCompressLevelChange
  end
  object cbbCompressMethod: TComboBox
    Left = 188
    Top = 30
    Width = 145
    Height = 21
    Align = alCustom
    Style = csDropDownList
    Anchors = [akRight]
    ItemHeight = 13
    TabOrder = 1
  end
  object cbbVolumeSize: TComboBox
    Left = 3
    Top = 79
    Width = 330
    Height = 21
    Align = alCustom
    AutoComplete = False
    Anchors = [akLeft, akRight]
    CharCase = ecUpperCase
    ItemHeight = 13
    TabOrder = 2
  end
end
