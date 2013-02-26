object frMarksGeneralOptions: TfrMarksGeneralOptions
  Left = 0
  Top = 0
  Width = 615
  Height = 321
  Align = alClient
  Constraints.MinHeight = 321
  Constraints.MinWidth = 615
  TabOrder = 0
  object grpPoint: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 52
    Width = 609
    Height = 81
    Align = alTop
    Caption = 'Placemark parameters'
    TabOrder = 0
    object lblPointIcon: TLabel
      Left = 2
      Top = 15
      Width = 21
      Height = 64
      Align = alLeft
      Alignment = taCenter
      Caption = 'Icon'
      Layout = tlCenter
    end
    object pnlImage: TPanel
      AlignWithMargins = True
      Left = 26
      Top = 18
      Width = 58
      Height = 58
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 0
      OnResize = pnlImageResize
    end
    object pnlPointParams: TPanel
      Left = 87
      Top = 15
      Width = 520
      Height = 64
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object flwpnlPointText: TFlowPanel
        Left = 0
        Top = 0
        Width = 520
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblPointTextColor: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 48
          Height = 13
          Caption = 'Text color'
        end
        object clrbxPointTextColor: TColorBox
          AlignWithMargins = True
          Left = 57
          Top = 3
          Width = 120
          Height = 22
          Selected = clYellow
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
        end
        object btnPointTextColor: TSpeedButton
          AlignWithMargins = True
          Left = 183
          Top = 3
          Width = 17
          Height = 22
          Caption = '...'
          OnClick = btnPointTextColorClick
        end
        object lblPointTextTransp: TLabel
          AlignWithMargins = True
          Left = 206
          Top = 3
          Width = 51
          Height = 13
          Caption = 'Opacity %'
        end
        object sePointTextTransp: TSpinEdit
          AlignWithMargins = True
          Left = 263
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 1
          Value = 35
        end
        object lblPointFontSize: TLabel
          AlignWithMargins = True
          Left = 310
          Top = 3
          Width = 43
          Height = 13
          Caption = 'Font size'
        end
        object sePointFontSize: TSpinEdit
          AlignWithMargins = True
          Left = 359
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 24
          MinValue = 0
          TabOrder = 2
          Value = 11
        end
      end
      object flwpnlPointShadowParams: TFlowPanel
        Left = 0
        Top = 28
        Width = 520
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lblPointShadowColor: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 64
          Height = 13
          Caption = 'Shadow color'
        end
        object clrbxPointShadowColor: TColorBox
          AlignWithMargins = True
          Left = 73
          Top = 3
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
        end
        object btnPointShadowColor: TSpeedButton
          AlignWithMargins = True
          Left = 199
          Top = 3
          Width = 17
          Height = 22
          Caption = '...'
          OnClick = btnPointShadowColorClick
        end
        object lblPointShadowAlfa: TLabel
          AlignWithMargins = True
          Left = 222
          Top = 3
          Width = 51
          Height = 13
          Caption = 'Opacity %'
        end
        object sePointShadowAlfa: TSpinEdit
          AlignWithMargins = True
          Left = 279
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 1
          Value = 35
        end
        object lblPointIconSize: TLabel
          AlignWithMargins = True
          Left = 326
          Top = 3
          Width = 42
          Height = 13
          Caption = 'Icon size'
        end
        object sePointIconSize: TSpinEdit
          AlignWithMargins = True
          Left = 374
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 64
          MinValue = 1
          TabOrder = 2
          Value = 32
        end
      end
    end
  end
  object grpLine: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 162
    Width = 609
    Height = 48
    Align = alTop
    Caption = 'Path parameters'
    TabOrder = 1
    object flwpnlPath: TFlowPanel
      Left = 2
      Top = 15
      Width = 605
      Height = 31
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object lblLineColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 25
        Height = 13
        Caption = 'Color'
      end
      object clrbxLineColor: TColorBox
        AlignWithMargins = True
        Left = 34
        Top = 3
        Width = 120
        Height = 22
        Selected = clRed
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 0
      end
      object btnLineColor: TSpeedButton
        AlignWithMargins = True
        Left = 160
        Top = 3
        Width = 17
        Height = 22
        Caption = '...'
        OnClick = btnLineColorClick
      end
      object lblLineTransp: TLabel
        AlignWithMargins = True
        Left = 183
        Top = 3
        Width = 51
        Height = 13
        Caption = 'Opacity %'
      end
      object seLineTransp: TSpinEdit
        AlignWithMargins = True
        Left = 240
        Top = 3
        Width = 41
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 1
        Value = 35
      end
      object lblLineWidth: TLabel
        AlignWithMargins = True
        Left = 287
        Top = 3
        Width = 28
        Height = 13
        Caption = 'Width'
      end
      object seLineWidth: TSpinEdit
        AlignWithMargins = True
        Left = 321
        Top = 3
        Width = 41
        Height = 22
        MaxValue = 24
        MinValue = 1
        TabOrder = 2
        Value = 2
      end
    end
  end
  object grpPoly: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 239
    Width = 609
    Height = 79
    Align = alTop
    Caption = 'Polygon parameters'
    TabOrder = 2
    object flwpnlPlygonLine: TFlowPanel
      Left = 2
      Top = 15
      Width = 605
      Height = 27
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblPolyLine: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 23
        Height = 13
        Caption = 'Line:'
      end
      object lblPolyLineColor: TLabel
        AlignWithMargins = True
        Left = 32
        Top = 3
        Width = 25
        Height = 13
        Caption = 'Color'
      end
      object clrbxPolyLineColor: TColorBox
        AlignWithMargins = True
        Left = 63
        Top = 3
        Width = 120
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 0
      end
      object btnPolyLineColor: TSpeedButton
        AlignWithMargins = True
        Left = 189
        Top = 3
        Width = 17
        Height = 22
        Caption = '...'
        OnClick = btnPolyLineColorClick
      end
      object lblPolyLineTransp: TLabel
        AlignWithMargins = True
        Left = 212
        Top = 3
        Width = 51
        Height = 13
        Caption = 'Opacity %'
      end
      object sePolyLineTransp: TSpinEdit
        AlignWithMargins = True
        Left = 269
        Top = 3
        Width = 41
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 1
        Value = 35
      end
      object lblPolyLineWidth: TLabel
        AlignWithMargins = True
        Left = 316
        Top = 3
        Width = 28
        Height = 13
        Caption = 'Width'
      end
      object sePolyLineWidth: TSpinEdit
        AlignWithMargins = True
        Left = 350
        Top = 3
        Width = 41
        Height = 22
        MaxValue = 24
        MinValue = 1
        TabOrder = 2
        Value = 2
      end
    end
    object flwpnlPolygonFill: TFlowPanel
      Left = 2
      Top = 42
      Width = 605
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object lblPolyFill: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 16
        Height = 13
        Caption = 'Fill:'
      end
      object lblPolyFillColor: TLabel
        AlignWithMargins = True
        Left = 25
        Top = 3
        Width = 25
        Height = 13
        Caption = 'Color'
      end
      object clrbxPolyFillColor: TColorBox
        AlignWithMargins = True
        Left = 56
        Top = 3
        Width = 120
        Height = 22
        Selected = clWhite
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 0
      end
      object btnPolyFillColor: TSpeedButton
        AlignWithMargins = True
        Left = 182
        Top = 3
        Width = 17
        Height = 22
        Caption = '...'
        OnClick = btnPolyFillColorClick
      end
      object lblPolyFillTransp: TLabel
        AlignWithMargins = True
        Left = 205
        Top = 3
        Width = 51
        Height = 13
        Caption = 'Opacity %'
      end
      object sePolyFillTransp: TSpinEdit
        AlignWithMargins = True
        Left = 262
        Top = 3
        Width = 41
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 1
        Value = 80
      end
    end
  end
  object chkPointIgnore: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 29
    Width = 609
    Height = 17
    Align = alTop
    Caption = 'Ignore placemarks'
    TabOrder = 3
  end
  object chkLineIgnore: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 139
    Width = 609
    Height = 17
    Align = alTop
    Caption = 'Ignore paths'
    TabOrder = 4
  end
  object chkPolyIgnore: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 216
    Width = 609
    Height = 17
    Align = alTop
    Caption = 'Ignore polygons'
    TabOrder = 5
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 615
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 6
  end
  object ColorDialog1: TColorDialog
    Left = 470
    Top = 25
  end
end
