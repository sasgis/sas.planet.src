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
    BackWall.Pen.Color = clSilver
    BackWall.Pen.Style = psDash
    BottomWall.Visible = False
    Foot.Visible = False
    LeftWall.Visible = False
    Legend.CurrentPage = False
    Legend.Visible = False
    MarginBottom = 1
    MarginLeft = 0
    MarginRight = 1
    MarginTop = 1
    ScrollMouseButton = mbLeft
    SubFoot.Visible = False
    SubTitle.Visible = False
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    BottomAxis.Axis.Visible = False
    BottomAxis.Grid.Style = psDash
    Frame.Color = clSilver
    Frame.Style = psDash
    LeftAxis.Axis.Visible = False
    LeftAxis.Grid.Style = psDash
    LeftAxis.LabelsFormat.Frame.Visible = False
    LeftAxis.LabelsFormat.Margins.Left = 0
    LeftAxis.LabelsFormat.Margins.Top = 0
    LeftAxis.LabelsFormat.Margins.Right = 0
    LeftAxis.LabelsFormat.Margins.Bottom = 0
    LeftAxis.Title.Visible = False
    Pages.ScaleLastPage = False
    Panning.InsideBounds = True
    Panning.MouseWheel = pmwNone
    RightAxis.Axis.Visible = False
    Shadow.Visible = False
    TopAxis.Automatic = False
    TopAxis.AutomaticMaximum = False
    TopAxis.AutomaticMinimum = False
    TopAxis.Visible = False
    View3D = False
    View3DOptions.Orthogonal = False
    Zoom.MouseButton = mbRight
    Zoom.MouseWheel = pmwNormal
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DefaultCanvas = 'TGDIPlusCanvas'
    PrintMargins = (
      15
      31
      15
      31)
    ColorPaletteIndex = 13
  end
  object pmMain: TPopupMenu
    Left = 464
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
    object N1: TMenuItem
      Caption = '-'
    end
    object mniResetZoom: TMenuItem
      Caption = 'Reset Zoom'
      OnClick = mniResetZoomClick
    end
  end
end
