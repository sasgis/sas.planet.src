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
    ItemHeight = 0
    TabOrder = 0
    OnChange = cbbCompressLevelChange
  end
  object cbbCompressMethod: TComboBox
    Left = 188
    Top = 30
    Width = 145
    Height = 21
    ItemHeight = 0
    TabOrder = 1
  end
  object tbxToolbar: TTBXToolbar
    Left = 3
    Top = 79
    Width = 330
    Height = 21
    Options = [tboNoRotation]
    Stretch = True
    TabOrder = 2
    TabStop = True
    object tbxcbbVolumeSize: TTBXComboBoxItem
      AutoCheck = True
      CharCase = ecUpperCase
      DisplayMode = nbdmTextOnly
      EditWidth = 330
      MaskOptions = [tboNoRotation]
      Options = [tboNoAutoHint, tboNoRotation]
      ExtendedAccept = True
      AutoComplete = False
      MinListWidth = 330
      Caption = ''
      Hint = ''
    end
  end
end
