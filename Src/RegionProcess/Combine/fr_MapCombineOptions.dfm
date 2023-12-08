object frMapCombineCustomOptions: TfrMapCombineCustomOptions
  Left = 0
  Top = 0
  Width = 385
  Height = 169
  TabOrder = 0
  object flwpnlJpegQuality: TFlowPanel
    Left = 0
    Top = 34
    Width = 385
    Height = 26
    Align = alTop
    AutoSize = True
    AutoWrap = False
    BevelOuter = bvNone
    Constraints.MinHeight = 25
    Padding.Top = 2
    TabOrder = 2
    object lblJpgQulity: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 5
      Width = 54
      Height = 15
      Margins.Left = 1
      Margins.Right = 5
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
    Left = 0
    Top = 60
    Width = 385
    Height = 26
    Align = alTop
    AutoSize = True
    AutoWrap = False
    BevelOuter = bvNone
    Constraints.MinHeight = 25
    Padding.Top = 2
    TabOrder = 3
    object lblThreadCount: TLabel
      AlignWithMargins = True
      Left = 1
      Top = 5
      Width = 73
      Height = 15
      Margins.Left = 1
      Margins.Right = 5
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
    Left = 0
    Top = 86
    Width = 385
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
end
