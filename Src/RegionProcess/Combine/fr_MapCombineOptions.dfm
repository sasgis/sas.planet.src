object frMapCombineCustomOptions: TfrMapCombineCustomOptions
  Left = 0
  Top = 0
  Width = 385
  Height = 169
  TabOrder = 0
  object flwpnlJpegQuality: TFlowPanel
    AlignWithMargins = True
    Left = 0
    Top = 37
    Width = 382
    Height = 26
    Margins.Left = 0
    Align = alTop
    AutoSize = True
    AutoWrap = False
    BevelOuter = bvNone
    Constraints.MinHeight = 25
    Padding.Top = 2
    TabOrder = 2
    object lblJpgQulity: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 5
      Width = 54
      Height = 15
      Margins.Bottom = 3
      Alignment = taRightJustify
      Caption = 'Quality, %'
      Layout = tlCenter
    end
    object seJpgQuality: TSpinEdit
      Left = 60
      Top = 2
      Width = 53
      Height = 24
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
    TabOrder = 0
  end
  object chkSaveGeoRefInfoToJpegExif: TCheckBox
    Left = 0
    Top = 17
    Width = 385
    Height = 17
    Align = alTop
    Caption = 'Save GeoRef info to Exif'
    TabOrder = 1
  end
  object flwpnlThreadCount: TFlowPanel
    AlignWithMargins = True
    Left = 0
    Top = 69
    Width = 382
    Height = 26
    Margins.Left = 0
    Align = alTop
    AutoSize = True
    AutoWrap = False
    BevelOuter = bvNone
    Constraints.MinHeight = 25
    Padding.Top = 2
    TabOrder = 3
    object lblThreadCount: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 5
      Width = 73
      Height = 15
      Margins.Bottom = 3
      Caption = 'Thread count:'
    end
    object seThreadCount: TSpinEdit
      Left = 79
      Top = 2
      Width = 53
      Height = 24
      MaxValue = 32
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
  end
  object flwpnlFormatOptions: TFlowPanel
    AlignWithMargins = True
    Left = 3
    Top = 130
    Width = 379
    Height = 28
    Align = alTop
    AutoSize = True
    AutoWrap = False
    BevelOuter = bvNone
    Constraints.MinHeight = 25
    Padding.Top = 2
    TabOrder = 4
    object btnFormatOptions: TTBXButton
      Left = 0
      Top = 2
      Width = 120
      Height = 26
      Align = alLeft
      Alignment = taLeftJustify
      Caption = 'Format Options'
      ImageIndex = 20
      Images = frmMain.MenusImageList
      TabOrder = 0
    end
  end
  object flwpnlKmzTileSize: TFlowPanel
    AlignWithMargins = True
    Left = 0
    Top = 101
    Width = 382
    Height = 23
    Margins.Left = 0
    Align = alTop
    AutoSize = True
    AutoWrap = False
    BevelOuter = bvNone
    TabOrder = 5
    object lblKmzTileSize: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 65
      Height = 15
      Caption = 'Tile size, pix:'
    end
    object cbbKmzTileSize: TComboBox
      Left = 71
      Top = 0
      Width = 98
      Height = 23
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 0
      Text = '1024x1024'
      Items.Strings = (
        '256x256'
        '512x512'
        '1024x1024'
        '2048x2048'
        '4096x4096'
        '8192x8192')
    end
  end
end
