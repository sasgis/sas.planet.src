object frDeleteMarks: TfrDeleteMarks
  Left = 0
  Top = 0
  Width = 480
  Height = 100
  Align = alClient
  Constraints.MinHeight = 100
  Constraints.MinWidth = 480
  TabOrder = 0
  object chkDelHidden: TCheckBox
    Left = 0
    Top = 56
    Width = 480
    Height = 17
    Align = alTop
    Caption = 'Delete hidden placemarks'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object PnlTop: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 56
    Align = alTop
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 1
    object chkPlacemarks: TCheckBox
      Left = 0
      Top = 0
      Width = 476
      Height = 17
      Align = alTop
      Caption = 'Placemarks'
      TabOrder = 0
    end
    object chkPaths: TCheckBox
      Left = 0
      Top = 17
      Width = 476
      Height = 17
      Align = alTop
      Caption = 'Paths'
      TabOrder = 1
    end
    object chkPolygons: TCheckBox
      Left = 0
      Top = 34
      Width = 476
      Height = 17
      Align = alTop
      Caption = 'Polygons'
      TabOrder = 2
    end
  end
end
