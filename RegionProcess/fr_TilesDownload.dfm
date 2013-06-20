object frTilesDownload: TfrTilesDownload
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
    Caption = 'Download Tiles'
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 84
    Width = 451
    Height = 220
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 451
      Height = 220
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object chkReplace: TCheckBox
        Left = 5
        Top = 37
        Width = 441
        Height = 16
        Align = alTop
        Caption = 'Overwrite old tiles'
        TabOrder = 0
        OnClick = chkReplaceClick
      end
      object chkTryLoadIfTNE: TCheckBox
        Left = 5
        Top = 21
        Width = 441
        Height = 16
        Align = alTop
        Caption = 'Try to re-download missing tiles'
        TabOrder = 1
      end
      object pnlTileReplaceCondition: TPanel
        Left = 5
        Top = 53
        Width = 441
        Height = 40
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        Padding.Left = 15
        TabOrder = 2
        object chkReplaceIfDifSize: TCheckBox
          Left = 18
          Top = 3
          Width = 420
          Height = 13
          Align = alTop
          Caption = 'only if different'
          Enabled = False
          TabOrder = 0
        end
        object pnlReplaceOlder: TPanel
          Left = 18
          Top = 16
          Width = 420
          Height = 21
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object lblReplaceOlder: TLabel
            Left = 16
            Top = 0
            Width = 95
            Height = 21
            Align = alLeft
            Caption = 'only created before'
            Layout = tlCenter
            ExplicitHeight = 13
          end
          object chkReplaceOlder: TCheckBox
            Left = 0
            Top = 0
            Width = 16
            Height = 21
            Align = alLeft
            Enabled = False
            TabOrder = 0
            OnClick = chkReplaceOlderClick
          end
          object dtpReplaceOlderDate: TDateTimePicker
            Left = 111
            Top = 0
            Width = 81
            Height = 21
            Align = alLeft
            Date = 39513.436381111110000000
            Time = 39513.436381111110000000
            Enabled = False
            TabOrder = 1
          end
        end
      end
      object chkStartPaused: TCheckBox
        Left = 5
        Top = 5
        Width = 441
        Height = 16
        Align = alTop
        Caption = 'Start paused'
        TabOrder = 3
      end
    end
  end
  object pnlMapSelect: TPanel
    Left = 0
    Top = 22
    Width = 451
    Height = 62
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      451
      60)
    object lblStat: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 44
      Width = 451
      Height = 13
      Margins.Left = 0
      Margins.Right = 0
      Align = alBottom
      Caption = '_'
      ExplicitWidth = 6
    end
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
      BorderWidth = 3
      TabOrder = 0
      object lblZoom: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 53
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Zoom:'
        ExplicitWidth = 30
      end
      object cbbZoom: TComboBox
        Left = 3
        Top = 17
        Width = 53
        Height = 21
        Align = alBottom
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbZoomChange
      end
    end
    object pnlFrame: TPanel
      Left = 3
      Top = 4
      Width = 392
      Height = 43
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
