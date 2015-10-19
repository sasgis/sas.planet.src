object frTilesGenPrev: TfrTilesGenPrev
  Left = 0
  Top = 0
  Width = 503
  Height = 230
  Align = alClient
  Constraints.MinHeight = 230
  Constraints.MinWidth = 500
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlBottom: TPanel
    Left = 0
    Top = 65
    Width = 503
    Height = 165
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlRight: TPanel
      Left = 389
      Top = 0
      Width = 114
      Height = 165
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblZooms: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 6
        Width = 108
        Height = 13
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'To zooms:'
      end
      object chkAllZooms: TCheckBox
        Left = 3
        Top = 145
        Width = 108
        Height = 17
        Align = alBottom
        Caption = 'All'
        TabOrder = 0
        OnClick = chkAllZoomsClick
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 19
        Width = 108
        Height = 126
        OnClickCheck = chklstZoomsClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 389
      Height = 165
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      DesignSize = (
        389
        165)
      object lblStat: TLabel
        Left = 10
        Top = 100
        Width = 55
        Height = 13
        Anchors = [akLeft, akTop, akRight]
      end
      object lblResampler: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 6
        Width = 383
        Height = 13
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Resize algorithm:'
      end
      object Bevel1: TBevel
        AlignWithMargins = True
        Left = 3
        Top = 45
        Width = 383
        Height = 3
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Shape = bsTopLine
      end
      object cbbResampler: TComboBox
        Left = 3
        Top = 19
        Width = 383
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Box'
        Items.Strings = (
          'Box'
          'Linear'
          'Cosine'
          'Spline'
          'Mitchell'
          'Cubic'
          'Hermite'
          'Lanczos'
          'Gaussian'
          'Blackman'
          'Hann'
          'Hamming'
          'Sinsh')
      end
      object chkReplace: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 51
        Width = 383
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Overwrite existing tiles'
        TabOrder = 1
        OnClick = chkReplaceClick
      end
      object chkSaveFullOnly: TCheckBox
        Left = 3
        Top = 91
        Width = 383
        Height = 17
        Align = alTop
        Caption = 'Save only complete tiles'
        TabOrder = 3
      end
      object chkFromPrevZoom: TCheckBox
        Left = 3
        Top = 108
        Width = 383
        Height = 17
        Align = alTop
        Caption = 'Generate each zoom from previous one'
        TabOrder = 4
        OnClick = chkFromPrevZoomClick
      end
      object pnlUsePrevTiles: TPanel
        Left = 3
        Top = 68
        Width = 383
        Height = 23
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        Padding.Left = 15
        TabOrder = 2
        object chkUsePrevTiles: TCheckBox
          AlignWithMargins = True
          Left = 18
          Top = 3
          Width = 362
          Height = 17
          Align = alTop
          Caption = 'Use tile from lower zoom if tile not available'
          Enabled = False
          TabOrder = 0
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 503
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = 'Generate Lower Zooms for Selection'
    TabOrder = 1
  end
  object pnlMapSelect: TPanel
    Left = 0
    Top = 22
    Width = 503
    Height = 43
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    DesignSize = (
      503
      41)
    object pnlZoom: TPanel
      Left = 392
      Top = 3
      Width = 108
      Height = 35
      Align = alRight
      Alignment = taLeftJustify
      BevelEdges = []
      BevelKind = bkTile
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 0
      object lblFromZoom: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 0
        Width = 56
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Caption = 'From zoom:'
      end
      object cbbFromZoom: TComboBox
        Left = 1
        Top = 14
        Width = 106
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbFromZoomChange
      end
    end
    object pnlFrame: TPanel
      Left = 3
      Top = 4
      Width = 387
      Height = 40
      Alignment = taLeftJustify
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 1
      object lblMapCaption: TLabel
        Left = 0
        Top = 0
        Width = 387
        Height = 13
        Align = alTop
        Caption = 'Map/Overlay layer:'
        ExplicitWidth = 93
      end
    end
  end
end
