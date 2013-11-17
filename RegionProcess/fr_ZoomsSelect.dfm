object frZoomsSelect: TfrZoomsSelect
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object lblZooms: TLabel
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 451
    Height = 14
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Align = alTop
    AutoSize = False
    Caption = 'Zooms:'
    ExplicitTop = 20
  end
  object chkAllZooms: TCheckBox
    Left = 0
    Top = 287
    Width = 451
    Height = 17
    Align = alBottom
    Caption = 'All'
    TabOrder = 0
    OnClick = chkAllZoomsClick
  end
  object chklstZooms: TCheckListBox
    Left = 0
    Top = 17
    Width = 451
    Height = 264
    OnClickCheck = chklstZoomsClick
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnDblClick = chklstZoomsDblClick
  end
end
