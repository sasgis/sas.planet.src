object frGpsSatellites: TfrGpsSatellites
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object SatellitePaintBox: TImage32
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 445
    Height = 236
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnResize = SatellitePaintBoxResize
  end
  object pnlSatInfoLegend: TPanel
    Left = 0
    Top = 242
    Width = 451
    Height = 62
    Align = alBottom
    AutoSize = True
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 1
    object pnlSatInfoActive: TPanel
      Left = 0
      Top = 0
      Width = 451
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblSatInfoActive: TLabel
        AlignWithMargins = True
        Left = 31
        Top = 3
        Width = 74
        Height = 13
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 0
        Align = alLeft
        Caption = 'Satellites in use'
      end
      object shpSatInfoActive: TShape
        AlignWithMargins = True
        Left = 6
        Top = 4
        Width = 12
        Height = 12
        Margins.Top = 1
        Margins.Bottom = 1
        Align = alLeft
        Brush.Color = clGreen
      end
    end
    object pnlSatInfoVisible: TPanel
      Left = 0
      Top = 20
      Width = 451
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object shpSatInfoVisible: TShape
        AlignWithMargins = True
        Left = 6
        Top = 4
        Width = 12
        Height = 12
        Margins.Top = 1
        Margins.Bottom = 1
        Align = alLeft
        Brush.Color = clYellow
      end
      object lblSatInfoVisible: TLabel
        AlignWithMargins = True
        Left = 31
        Top = 3
        Width = 85
        Height = 13
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 0
        Align = alLeft
        Caption = 'Satellites in range'
      end
    end
    object pnlSatInfoZeroSignal: TPanel
      Left = 0
      Top = 40
      Width = 451
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 2
      object lblSatInfoZeroSignal: TLabel
        AlignWithMargins = True
        Left = 31
        Top = 3
        Width = 104
        Height = 13
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 0
        Align = alLeft
        Caption = 'Satellites not in range'
      end
      object shpSatInfoZeroSignal: TShape
        AlignWithMargins = True
        Left = 6
        Top = 4
        Width = 12
        Height = 12
        Margins.Top = 1
        Margins.Bottom = 1
        Align = alLeft
        Brush.Color = clRed
      end
    end
  end
end
