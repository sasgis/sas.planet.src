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
    Top = 22
    Width = 451
    Height = 282
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlRight: TPanel
      Left = 392
      Top = 0
      Width = 59
      Height = 282
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object lblZoom: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 49
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Zoom:'
      end
      object cbbZoom: TComboBox
        Left = 5
        Top = 21
        Width = 49
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbZoomChange
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 392
      Height = 282
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 1
      object lblStat: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 45
        Width = 382
        Height = 13
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        Caption = '_'
      end
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 5
        Top = 5
        Width = 382
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Map:'
      end
      object Bevel1: TBevel
        Left = 5
        Top = 61
        Width = 382
        Height = 5
        Align = alTop
        Shape = bsTopLine
      end
      object cbbMap: TComboBox
        Left = 5
        Top = 21
        Width = 382
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
      end
      object chkReplace: TCheckBox
        Left = 5
        Top = 98
        Width = 382
        Height = 16
        Align = alTop
        Caption = 'Overwrite old tiles'
        TabOrder = 1
        OnClick = chkReplaceClick
      end
      object chkTryLoadIfTNE: TCheckBox
        Left = 5
        Top = 82
        Width = 382
        Height = 16
        Align = alTop
        Caption = 'Try to re-download missing tiles'
        TabOrder = 2
      end
      object pnlTileReplaceCondition: TPanel
        Left = 5
        Top = 114
        Width = 382
        Height = 40
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        Padding.Left = 15
        TabOrder = 3
        object chkReplaceIfDifSize: TCheckBox
          Left = 18
          Top = 3
          Width = 361
          Height = 13
          Align = alTop
          Caption = 'only if different'
          Enabled = False
          TabOrder = 0
        end
        object pnlReplaceOlder: TPanel
          Left = 18
          Top = 16
          Width = 361
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
        Top = 66
        Width = 382
        Height = 16
        Align = alTop
        Caption = 'Start paused'
        TabOrder = 4
      end
    end
  end
end
