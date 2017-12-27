object frMapCombineCustomOptions: TfrMapCombineCustomOptions
  Left = 0
  Top = 0
  Width = 385
  Height = 169
  TabOrder = 0
  object flwpnlCompression: TFlowPanel
    Left = 0
    Top = 84
    Width = 385
    Height = 25
    Align = alTop
    AutoWrap = False
    BevelOuter = bvNone
    TabOrder = 0
    object lblCompression: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 65
      Height = 13
      Caption = 'Compression:'
    end
    object cbbCompression: TComboBox
      Left = 71
      Top = 0
      Width = 140
      Height = 21
      ItemHeight = 13
      ItemIndex = 2
      TabOrder = 0
      Text = 'LZW'
      Items.Strings = (
        'None'
        'ZIP (Deflate)'
        'LZW'
        'JPEG')
    end
  end
  object flwpnlFormat: TFlowPanel
    Left = 0
    Top = 59
    Width = 385
    Height = 25
    Align = alTop
    AutoWrap = False
    BevelOuter = bvNone
    TabOrder = 1
    object lblFormat: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 38
      Height = 13
      Caption = 'Format:'
    end
    object cbbFormat: TComboBox
      Left = 44
      Top = 0
      Width = 140
      Height = 21
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Auto'
      Items.Strings = (
        'Auto'
        'TIFF'
        'BigTIFF')
    end
  end
  object flwpnlJpegQuality: TFlowPanel
    Left = 0
    Top = 34
    Width = 385
    Height = 25
    Align = alTop
    AutoSize = True
    AutoWrap = False
    BevelOuter = bvNone
    Constraints.MinHeight = 25
    Padding.Top = 2
    TabOrder = 2
    object lblJpgQulity: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 52
      Height = 13
      Margins.Left = 0
      Margins.Right = 5
      Alignment = taRightJustify
      Caption = 'Quality, %'
      Layout = tlCenter
    end
    object seJpgQuality: TSpinEdit
      Left = 57
      Top = 2
      Width = 53
      Height = 22
      MaxValue = 100
      MinValue = 1
      TabOrder = 0
      Value = 95
    end
  end
  object chkPngWithAlpha: TCheckBox
    Left = 0
    Top = 0
    Width = 385
    Height = 17
    Align = alTop
    Caption = 'Include alpha channel'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object chkSaveGeoRefInfoToJpegExif: TCheckBox
    Left = 0
    Top = 17
    Width = 385
    Height = 17
    Align = alTop
    Caption = 'Save GeoRef info to Exif'
    TabOrder = 4
  end
  object flwpnlThreadCount: TFlowPanel
    Left = 0
    Top = 109
    Width = 385
    Height = 25
    Align = alTop
    AutoSize = True
    AutoWrap = False
    BevelOuter = bvNone
    Constraints.MinHeight = 25
    Padding.Top = 2
    TabOrder = 5
    object lblThreadCount: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 5
      Width = 64
      Height = 13
      Margins.Left = 0
      Margins.Right = 5
      Alignment = taRightJustify
      Caption = 'Thread count'
      Layout = tlCenter
    end
    object seThreadCount: TSpinEdit
      Left = 69
      Top = 2
      Width = 53
      Height = 22
      MaxValue = 32
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
  end
end
