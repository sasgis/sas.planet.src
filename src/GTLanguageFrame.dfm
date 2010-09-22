object GTfraLanguage: TGTfraLanguage
  Left = 0
  Top = 0
  Width = 245
  Height = 21
  AutoSize = True
  TabOrder = 0
  OnResize = FrameResize
  object lblLanguage: TLabel
    Left = 0
    Top = 4
    Width = 86
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Language'
  end
  object cmbLanguage: TComboBox
    Left = 100
    Top = 0
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnSelect = cmbLanguageSelect
  end
end
