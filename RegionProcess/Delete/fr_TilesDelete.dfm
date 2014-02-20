object frTilesDelete: TfrTilesDelete
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlTop: TPanel
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
    Caption = 'Delete map tiles'
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 65
    Width = 451
    Height = 239
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 451
      Height = 239
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      DesignSize = (
        451
        239)
      object lblStat: TLabel
        Left = 10
        Top = 66
        Width = 3
        Height = 13
        Anchors = [akLeft, akTop, akRight]
      end
      object flwpnlDelBySize: TFlowPanel
        AlignWithMargins = True
        Left = 6
        Top = 70
        Width = 439
        Height = 24
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        Padding.Top = 2
        TabOrder = 0
        object chkDelBySize: TCheckBox
          Left = 0
          Top = 2
          Width = 13
          Height = 21
          TabOrder = 0
        end
        object lblDelSize: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 5
          Width = 148
          Height = 13
          Caption = 'Delete only tiles of size, bytes:'
        end
        object seDelSize: TSpinEdit
          Left = 167
          Top = 2
          Width = 69
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
      object rgTarget: TRadioGroup
        Left = 3
        Top = 3
        Width = 445
        Height = 64
        Align = alTop
        Caption = 'Delete target'
        ItemIndex = 0
        Items.Strings = (
          'Delete tiles'
          'Delete tne'
          'Delete both')
        TabOrder = 1
      end
    end
  end
  object pnlMapSelect: TPanel
    Left = 0
    Top = 22
    Width = 451
    Height = 43
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      451
      41)
    object pnlZoom: TPanel
      Left = 392
      Top = 0
      Width = 59
      Height = 41
      Align = alRight
      Alignment = taLeftJustify
      BevelEdges = []
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
      object Labelzoom: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 1
        Width = 30
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Caption = 'Zoom:'
      end
      object cbbZoom: TComboBox
        Left = 3
        Top = 17
        Width = 53
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pnlFrame: TPanel
      Left = 3
      Top = 4
      Width = 392
      Height = 44
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 1
      object lblMapCaption: TLabel
        Left = 0
        Top = 0
        Width = 392
        Height = 13
        Align = alTop
        Caption = 'Map:'
        ExplicitWidth = 24
      end
    end
  end
end
