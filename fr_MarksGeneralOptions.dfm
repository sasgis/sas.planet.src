object frMarksGeneralOptions: TfrMarksGeneralOptions
  Left = 0
  Top = 0
  Width = 505
  Height = 354
  TabOrder = 0
  object grpPoint: TGroupBox
    Left = 3
    Top = 57
    Width = 489
    Height = 81
    Caption = 'Placemark parameters'
    TabOrder = 0
    object lblPointTextColor: TLabel
      Left = 128
      Top = 24
      Width = 48
      Height = 13
      Caption = 'Text color'
    end
    object lblPointShadowColor: TLabel
      Left = 128
      Top = 48
      Width = 64
      Height = 13
      Caption = 'Shadow color'
    end
    object lblPointFontSize: TLabel
      Left = 256
      Top = 24
      Width = 43
      Height = 13
      Caption = 'Font size'
    end
    object lblPointIconSize: TLabel
      Left = 256
      Top = 48
      Width = 42
      Height = 13
      Caption = 'Icon size'
    end
    object lblPointTextTransp: TLabel
      Left = 392
      Top = 24
      Width = 51
      Height = 13
      Caption = 'Opacity %'
    end
    object btnPointTextColor: TSpeedButton
      Left = 230
      Top = 21
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnPointTextColorClick
    end
    object btnPointShadowColor: TSpeedButton
      Left = 230
      Top = 45
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnPointShadowColorClick
    end
    object lblPointIcon: TLabel
      Left = 16
      Top = 32
      Width = 21
      Height = 13
      Caption = 'Icon'
    end
    object clrbxPointTextColor: TColorBox
      Left = 192
      Top = 21
      Width = 38
      Height = 22
      Selected = clYellow
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object sePointFontSize: TSpinEdit
      Left = 344
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 0
      TabOrder = 1
      Value = 11
    end
    object clrbxPointShadowColor: TColorBox
      Left = 192
      Top = 45
      Width = 38
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 2
    end
    object sePointIconSize: TSpinEdit
      Left = 344
      Top = 45
      Width = 41
      Height = 22
      MaxValue = 64
      MinValue = 1
      TabOrder = 3
      Value = 32
    end
    object sePointTextTransp: TSpinEdit
      Left = 432
      Top = 43
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 4
      Value = 35
    end
    object cbbPointIcon: TComboBox
      Left = 64
      Top = 21
      Width = 57
      Height = 44
      Hint = 'Select image'
      Style = csOwnerDrawVariable
      ItemHeight = 38
      ParentShowHint = False
      ShowHint = False
      TabOrder = 5
      OnDrawItem = cbbPointIconDrawItem
    end
  end
  object grpLine: TGroupBox
    Left = 3
    Top = 169
    Width = 489
    Height = 57
    Caption = 'Path parameters'
    TabOrder = 1
    object lblLineColor: TLabel
      Left = 88
      Top = 24
      Width = 25
      Height = 13
      Caption = 'Color'
    end
    object lblLineWidth: TLabel
      Left = 194
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Width'
    end
    object lblLineTransp: TLabel
      Left = 298
      Top = 24
      Width = 51
      Height = 13
      Caption = 'Opacity %'
    end
    object btnLineColor: TSpeedButton
      Left = 155
      Top = 21
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnLineColorClick
    end
    object clrbxLineColor: TColorBox
      Left = 117
      Top = 21
      Width = 38
      Height = 22
      Selected = clRed
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object seLineWidth: TSpinEdit
      Left = 237
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 1
      Value = 2
    end
    object seLineTransp: TSpinEdit
      Left = 384
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 35
    end
  end
  object grpPoly: TGroupBox
    Left = 3
    Top = 257
    Width = 489
    Height = 81
    Caption = 'Polygon parameters'
    TabOrder = 2
    object lblPolyLineColor: TLabel
      Left = 88
      Top = 24
      Width = 25
      Height = 13
      Caption = 'Color'
    end
    object lblPolyLineWidth: TLabel
      Left = 194
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Width'
    end
    object lblPolyLineTransp: TLabel
      Left = 298
      Top = 24
      Width = 51
      Height = 13
      Caption = 'Opacity %'
    end
    object btnPolyLineColor: TSpeedButton
      Left = 155
      Top = 21
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnPolyLineColorClick
    end
    object lblPolyFillColor: TLabel
      Left = 88
      Top = 48
      Width = 25
      Height = 13
      Caption = 'Color'
    end
    object lblPolyFillTransp: TLabel
      Left = 194
      Top = 48
      Width = 51
      Height = 13
      Caption = 'Opacity %'
    end
    object btnPolyFillColor: TSpeedButton
      Left = 155
      Top = 45
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnPolyFillColorClick
    end
    object lblPolyLine: TLabel
      Left = 8
      Top = 22
      Width = 23
      Height = 13
      Caption = 'Line:'
    end
    object lblPolyFill: TLabel
      Left = 8
      Top = 46
      Width = 16
      Height = 13
      Caption = 'Fill:'
    end
    object clrbxPolyLineColor: TColorBox
      Left = 117
      Top = 21
      Width = 38
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object sePolyLineWidth: TSpinEdit
      Left = 237
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 1
      Value = 2
    end
    object sePolyLineTransp: TSpinEdit
      Left = 384
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 35
    end
    object clrbxPolyFillColor: TColorBox
      Left = 117
      Top = 45
      Width = 38
      Height = 22
      Selected = clWhite
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 3
    end
    object sePolyFillTransp: TSpinEdit
      Left = 280
      Top = 45
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 4
      Value = 80
    end
  end
  object chkPointIgnore: TCheckBox
    Left = 3
    Top = 33
    Width = 129
    Height = 17
    Caption = 'Ignore placemarks'
    TabOrder = 3
  end
  object chkLineIgnore: TCheckBox
    Left = 3
    Top = 145
    Width = 129
    Height = 17
    Caption = 'Ignore paths'
    TabOrder = 4
  end
  object chkPolyIgnore: TCheckBox
    Left = 3
    Top = 233
    Width = 153
    Height = 17
    Caption = 'Ignore polygons'
    TabOrder = 5
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 6
    ExplicitLeft = -5
    ExplicitTop = 31
  end
  object ColorDialog1: TColorDialog
    Left = 470
    Top = 25
  end
end
