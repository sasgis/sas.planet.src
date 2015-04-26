object frDelete: TfrDelete
  Left = 0
  Top = 0
  Width = 300
  Height = 80
  Align = alClient
  Constraints.MinHeight = 80
  Constraints.MinWidth = 300
  TabOrder = 0
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 300
    Height = 80
    ActivePage = tsTiles
    Align = alClient
    TabOrder = 0
    object tsTiles: TTabSheet
      Caption = 'Tiles'
    end
    object tsMarks: TTabSheet
      Caption = 'Placemarks'
      ImageIndex = 1
    end
  end
end
