object frDelete: TfrDelete
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Align = alClient
  TabOrder = 0
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 320
    Height = 240
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
