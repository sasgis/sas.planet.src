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
    Height = 227
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
    Top = 233
    Width = 451
    Height = 71
    Align = alBottom
    AutoSize = True
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 1
    object pnlSatInfoActive: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 449
      Height = 21
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblSatInfoActive: TLabel
        Left = 31
        Top = 3
        Width = 80
        Height = 15
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 0
        Align = alCustom
        Caption = 'Satellites in use'
      end
      object shpSatInfoActive: TShape
        Left = 6
        Top = 5
        Width = 12
        Height = 12
        Margins.Top = 1
        Margins.Bottom = 1
        Align = alCustom
        Brush.Color = clGreen
      end
    end
    object pnlSatInfoVisible: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 24
      Width = 449
      Height = 21
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object shpSatInfoVisible: TShape
        Left = 6
        Top = 5
        Width = 12
        Height = 12
        Align = alCustom
        Brush.Color = clYellow
      end
      object lblSatInfoVisible: TLabel
        Left = 31
        Top = 3
        Width = 92
        Height = 15
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 0
        Align = alCustom
        Caption = 'Satellites in range'
      end
    end
    object pnlSatInfoZeroSignal: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 47
      Width = 449
      Height = 21
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 2
      object lblSatInfoZeroSignal: TLabel
        Left = 31
        Top = 3
        Width = 113
        Height = 15
        Margins.Left = 10
        Margins.Top = 0
        Margins.Right = 0
        Align = alCustom
        Caption = 'Satellites not in range'
      end
      object shpSatInfoZeroSignal: TShape
        Left = 6
        Top = 3
        Width = 12
        Height = 12
        Align = alCustom
        Brush.Color = clRed
      end
    end
  end
end
