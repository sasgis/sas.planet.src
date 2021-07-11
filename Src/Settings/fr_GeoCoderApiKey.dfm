object frGeoCoderApiKey: TfrGeoCoderApiKey
  Left = 0
  Top = 0
  Width = 462
  Height = 97
  Align = alClient
  TabOrder = 0
  object lblGeoCoderApiKey: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 456
    Height = 13
    Align = alTop
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 3
  end
  object pnlKey: TPanel
    Left = 0
    Top = 16
    Width = 462
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object chkShowKey: TCheckBox
      Left = 445
      Top = 0
      Width = 17
      Height = 23
      Hint = 'Show / Hide'
      Align = alRight
      TabOrder = 0
      OnClick = chkShowKeyClick
      ExplicitLeft = 443
      ExplicitTop = 5
      ExplicitHeight = 17
    end
    object edtApiKey: TEdit
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 440
      Height = 23
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alClient
      PasswordChar = '*'
      TabOrder = 1
      ExplicitTop = 3
      ExplicitWidth = 437
      ExplicitHeight = 21
    end
  end
end
