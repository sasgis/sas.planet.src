object frElevationProfile: TfrElevationProfile
  Left = 0
  Top = 0
  Width = 667
  Height = 277
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Align = alClient
  PopupMenu = pmMain
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 667
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblInfo: TLabel
      Left = 8
      Top = 1
      Width = 3
      Height = 13
    end
    object btnClose: TTBXButton
      Left = 650
      Top = 0
      Width = 17
      Height = 17
      Align = alRight
      BorderSize = 0
      ButtonStyle = bsFlat
      GlyphSpacing = 0
      ImageIndex = 35
      Images = frmMain.MenusImageList
      ShowFocusRect = False
      TabOrder = 0
      OnClick = btnCloseClick
    end
  end
  object chtProfile: TChart
    Left = 0
    Top = 17
    Width = 667
    Height = 260
    Foot.Visible = False
    Legend.Visible = False
    MarginUnits = muPixels
    SubFoot.Visible = False
    SubTitle.Visible = False
    Title.Text.Strings = (
      ' ')
    Title.Visible = False
    OnScroll = chtProfileScroll
    OnUndoZoom = chtProfileUndoZoom
    OnZoom = chtProfileZoom
    BottomAxis.Axis.Visible = False
    BottomAxis.MinorTicks.Visible = False
    BottomAxis.Ticks.Visible = False
    BottomAxis.TicksInner.Visible = False
    BottomAxis.Title.Visible = False
    DepthAxis.Axis.Visible = False
    DepthAxis.Grid.Visible = False
    DepthTopAxis.Axis.Visible = False
    DepthTopAxis.Grid.Visible = False
    LeftAxis.Axis.Width = 1
    LeftAxis.Axis.Visible = False
    LeftAxis.LabelsAlign = alOpposite
    LeftAxis.MaximumOffset = 5
    LeftAxis.MinimumOffset = -5
    LeftAxis.MinorTicks.Visible = False
    LeftAxis.Ticks.Visible = False
    LeftAxis.TicksInner.Visible = False
    LeftAxis.Title.Visible = False
    Panning.InsideBounds = True
    RightAxis.Axis.Visible = False
    RightAxis.MinorTicks.Visible = False
    RightAxis.Ticks.Visible = False
    RightAxis.TicksInner.Visible = False
    RightAxis.Title.Visible = False
    Shadow.Visible = False
    TopAxis.Axis.Width = 1
    TopAxis.Axis.Visible = False
    TopAxis.Grid.Visible = False
    TopAxis.Grid.ZZero = True
    TopAxis.MinorTicks.Visible = False
    TopAxis.Ticks.Visible = False
    TopAxis.TicksInner.Visible = False
    View3D = False
    View3DOptions.Orthogonal = False
    View3DWalls = False
    Zoom.Pen.Color = clSilver
    Zoom.Pen.Width = 3
    OnAfterDraw = chtProfileAfterDraw
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    OnContextPopup = chtProfileContextPopup
    OnMouseDown = chtProfileMouseDown
    OnResize = chtProfileResize
    DefaultCanvas = 'TGDIPlusCanvas'
    PrintMargins = (
      15
      30
      15
      30)
    ColorPaletteIndex = 13
    object pnlPointInfo: TPanel
      Left = 48
      Top = 48
      Width = 57
      Height = 25
      BevelOuter = bvNone
      Color = clInfoBk
      ParentBackground = False
      TabOrder = 0
      Visible = False
      object lblPointInfo: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 4
        Height = 14
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
    end
    object pnlPointLine: TPanel
      Left = 48
      Top = 88
      Width = 1
      Height = 25
      BevelOuter = bvNone
      Color = clRed
      ParentBackground = False
      TabOrder = 1
      Visible = False
    end
  end
  object pmMain: TPopupMenu
    Left = 448
    Top = 65
    object mniShowSpeed: TMenuItem
      AutoCheck = True
      Caption = 'Show Speed'
      OnClick = mniShowSpeedClick
    end
    object mniShowElevation: TMenuItem
      AutoCheck = True
      Caption = 'Show Elevation'
      OnClick = mniShowElevationClick
    end
    object mniN3: TMenuItem
      Caption = '-'
    end
    object mniElevationSource: TMenuItem
      Caption = 'Elevation Source'
      object mniTrackData: TMenuItem
        Caption = 'GPS Data'
        GroupIndex = 1
        RadioItem = True
        OnClick = mniTrackDataClick
      end
      object mniDEMData: TMenuItem
        Caption = 'DEM Data'
        GroupIndex = 1
        RadioItem = True
        OnClick = mniDEMDataClick
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mniFilterData: TMenuItem
      AutoCheck = True
      Caption = 'Filter Data'
      OnClick = mniFilterDataClick
    end
    object mniCenterMap: TMenuItem
      AutoCheck = True
      Caption = 'Center Map'
      OnClick = mniCenterMapClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object mniZoomWithMouseWheel: TMenuItem
      AutoCheck = True
      Caption = 'Zoom with Mouse Wheel'
      OnClick = mniZoomWithMouseWheelClick
    end
    object mniKeepAspectRatio: TMenuItem
      AutoCheck = True
      Caption = 'Keep Aspect Ratio'
      OnClick = mniKeepAspectRatioClick
    end
    object mniScaleElevToDist: TMenuItem
      Caption = 'Scale Elevation to Distance as 1:1'
      OnClick = mniScaleElevToDistClick
    end
    object mniResetZoom: TMenuItem
      Caption = 'Reset Zoom'
      OnClick = mniResetZoomClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mniStatistics: TMenuItem
      Caption = 'Statistics'
      object mniTrackName: TMenuItem
        Caption = 'Track Name'
        OnClick = mniStatisticsItemClick
      end
      object mniDistance: TMenuItem
        Tag = 1
        Caption = 'Distance'
        OnClick = mniStatisticsItemClick
      end
      object mniDuration: TMenuItem
        Tag = 2
        Caption = 'Duration'
        OnClick = mniStatisticsItemClick
      end
      object mniElevationMinAvgMax: TMenuItem
        Tag = 3
        Caption = 'Elevation Min, Avg, Max'
        OnClick = mniStatisticsItemClick
      end
      object mniAscentDescent: TMenuItem
        Tag = 4
        Caption = 'Ascent and Descent'
        OnClick = mniStatisticsItemClick
      end
      object mniMaxSlope: TMenuItem
        Tag = 5
        Caption = 'Max Slope'
        OnClick = mniStatisticsItemClick
      end
      object mniAverageSlope: TMenuItem
        Tag = 6
        Caption = 'Average Slope'
        OnClick = mniStatisticsItemClick
      end
      object mniSpeedMinAvgMax: TMenuItem
        Tag = 7
        Caption = 'Speed Min, Avg, Max'
        OnClick = mniStatisticsItemClick
      end
    end
  end
end
