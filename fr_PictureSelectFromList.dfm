object frPictureSelectFromList: TfrPictureSelectFromList
  Left = 0
  Top = 0
  Width = 357
  Height = 335
  TabOrder = 0
  OnEnter = FrameEnter
  OnResize = FrameResize
  object drwgrdIcons: TDrawGrid
    Left = 0
    Top = 0
    Width = 357
    Height = 335
    Align = alClient
    ColCount = 8
    Ctl3D = False
    DefaultColWidth = 36
    DefaultRowHeight = 36
    FixedCols = 0
    FixedRows = 0
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goThumbTracking]
    ParentCtl3D = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    OnDrawCell = drwgrdIconsDrawCell
    OnKeyDown = drwgrdIconsKeyDown
    OnMouseMove = drwgrdIconsMouseMove
    OnMouseUp = drwgrdIconsMouseUp
  end
end
