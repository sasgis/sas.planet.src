object frProjectionSelect: TfrProjectionSelect
  Left = 0
  Top = 0
  Width = 451
  Height = 20
  Align = alClient
  TabOrder = 0
  object lblProjection: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 0
    Width = 52
    Height = 18
    Margins.Top = 0
    Margins.Right = 5
    Margins.Bottom = 2
    Align = alLeft
    Caption = 'Projection:'
    Layout = tlCenter
    ExplicitHeight = 13
  end
  object cbbProjection: TComboBox
    Left = 63
    Top = 0
    Width = 385
    Height = 21
    Align = alCustom
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbbProjectionChange
  end
end
