object frTilesCopy: TfrTilesCopy
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 49
    Width = 451
    Height = 255
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlRight: TPanel
      Left = 376
      Top = 0
      Width = 75
      Height = 255
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblZooms: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 69
        Height = 14
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Zooms:'
      end
      object chkAllZooms: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 235
        Width = 69
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = 'All'
        TabOrder = 0
        OnClick = chkAllZoomsClick
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 20
        Width = 69
        Height = 212
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        OnDblClick = chklstZoomsDblClick
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 255
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object lblNamesType: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 370
        Height = 14
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Output format'
      end
      object cbbNamesType: TComboBox
        Left = 3
        Top = 20
        Width = 370
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 0
        Text = 'SAS.Planet'
        Items.Strings = (
          'GoogleMV'
          'SAS.Planet'
          'ES1.95'
          'GMT (GlobalMapper >=10.02)'
          'BerkeleyDB')
      end
      object chkDeleteSource: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 44
        Width = 370
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Move'
        TabOrder = 1
      end
      object chkReplaseTarget: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 61
        Width = 370
        Height = 17
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Overwrite if equal'
        TabOrder = 2
      end
      object chkAllMaps: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 235
        Width = 370
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = 'All'
        TabOrder = 3
        OnClick = chkAllMapsClick
      end
      object chklstMaps: TCheckListBox
        Left = 3
        Top = 81
        Width = 370
        Height = 151
        Align = alClient
        ItemHeight = 13
        TabOrder = 4
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 22
    Width = 451
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object lblTargetPath: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 18
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetPath: TEdit
      Left = 47
      Top = 3
      Width = 380
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetPath: TButton
      Left = 427
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetPathClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = 'Copy tiles from selection to folder'
    TabOrder = 2
  end
end
