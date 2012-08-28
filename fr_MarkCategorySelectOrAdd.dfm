object frMarkCategorySelectOrAdd: TfrMarkCategorySelectOrAdd
  Left = 0
  Top = 0
  Width = 451
  Height = 27
  Align = alTop
  AutoSize = True
  TabOrder = 0
  object lblCategory: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 49
    Height = 21
    Align = alLeft
    Caption = 'Category:'
    Layout = tlCenter
    ExplicitHeight = 13
  end
  object CBKateg: TComboBox
    AlignWithMargins = True
    Left = 58
    Top = 3
    Width = 390
    Height = 21
    Align = alClient
    DropDownCount = 20
    ItemHeight = 13
    TabOrder = 0
    Text = 'New Category'
  end
end
