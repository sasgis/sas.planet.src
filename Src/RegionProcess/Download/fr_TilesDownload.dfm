object frTilesDownload: TfrTilesDownload
  Left = 0
  Top = 0
  Width = 480
  Height = 355
  Align = alClient
  Constraints.MinHeight = 200
  Constraints.MinWidth = 480
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 480
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
  object pnlZoom: TPanel
    Left = 376
    Top = 22
    Width = 104
    Height = 333
    Align = alRight
    Alignment = taLeftJustify
    BevelEdges = []
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 22
    Width = 376
    Height = 333
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMapSelect: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 62
      Align = alTop
      BevelEdges = [beBottom]
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
      object lblStat: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 45
        Width = 373
        Height = 15
        Margins.Right = 0
        Align = alBottom
        Caption = '_'
      end
      object pnlFrame: TPanel
        Left = 0
        Top = 0
        Width = 376
        Height = 43
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblMapCaption: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 370
          Height = 15
          Align = alTop
          Caption = 'Map/Overlay layer:'
        end
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 62
      Width = 376
      Height = 271
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 1
      object lblSplitRegionHint: TLabel
        AlignWithMargins = True
        Left = 8
        Top = 231
        Width = 360
        Height = 13
        Margins.Top = 0
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object chkReplace: TCheckBox
        Left = 5
        Top = 81
        Width = 366
        Height = 16
        Align = alTop
        Caption = 'Overwrite old tiles'
        TabOrder = 4
        OnClick = chkReplaceClick
      end
      object chkTryLoadIfTNE: TCheckBox
        Left = 5
        Top = 37
        Width = 366
        Height = 16
        Align = alTop
        Caption = 'Try download if tne exists'
        TabOrder = 2
        OnClick = chkTryLoadIfTNEClick
      end
      object pnlTileReplaceCondition: TPanel
        Left = 5
        Top = 97
        Width = 366
        Height = 44
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        Padding.Left = 15
        TabOrder = 5
        object chkReplaceIfDifSize: TCheckBox
          Left = 18
          Top = 3
          Width = 345
          Height = 16
          Align = alTop
          Caption = 'only if different'
          Enabled = False
          TabOrder = 0
        end
        object pnlReplaceOlder: TPanel
          Left = 18
          Top = 19
          Width = 345
          Height = 22
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object lblReplaceOlder: TLabel
            AlignWithMargins = True
            Left = 17
            Top = 0
            Width = 102
            Height = 22
            Margins.Left = 1
            Margins.Top = 0
            Align = alLeft
            Caption = 'only created before'
            Layout = tlCenter
          end
          object chkReplaceOlder: TCheckBox
            Left = 0
            Top = 0
            Width = 16
            Height = 22
            Align = alLeft
            Enabled = False
            TabOrder = 0
            OnClick = chkReplaceOlderClick
          end
          object dtpReplaceOlderDate: TDateTimePicker
            Left = 122
            Top = 0
            Width = 81
            Height = 22
            Align = alLeft
            Date = 39513.000000000000000000
            Time = 0.436381111110677000
            Enabled = False
            TabOrder = 1
          end
        end
      end
      object chkStartPaused: TCheckBox
        Left = 5
        Top = 5
        Width = 366
        Height = 16
        Align = alTop
        Caption = 'Start paused'
        TabOrder = 0
      end
      object pnlLoadIfTneParams: TPanel
        Left = 5
        Top = 53
        Width = 366
        Height = 28
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        Padding.Left = 15
        TabOrder = 3
        object pnlLoadIfTneOld: TPanel
          Left = 18
          Top = 3
          Width = 345
          Height = 22
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblLoadIfTneOld: TLabel
            AlignWithMargins = True
            Left = 17
            Top = 0
            Width = 102
            Height = 22
            Margins.Left = 1
            Margins.Top = 0
            Align = alLeft
            Caption = 'only created before'
            Layout = tlCenter
          end
          object chkLoadIfTneOld: TCheckBox
            Left = 0
            Top = 0
            Width = 16
            Height = 22
            Align = alLeft
            Enabled = False
            TabOrder = 0
            OnClick = chkLoadIfTneOldClick
          end
          object dtpLoadIfTneOld: TDateTimePicker
            Left = 122
            Top = 0
            Width = 81
            Height = 22
            Align = alLeft
            Date = 39513.000000000000000000
            Time = 0.436381111110677000
            Enabled = False
            TabOrder = 1
          end
        end
      end
      object flwpnlSplitRegionParams: TFlowPanel
        Left = 5
        Top = 199
        Width = 366
        Height = 32
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        Padding.Top = 2
        TabOrder = 8
        object chkSplitRegion: TCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 5
          Width = 17
          Height = 22
          Margins.Left = 0
          Margins.Right = 0
          Margins.Bottom = 0
          TabOrder = 0
          OnClick = chkSplitRegionClick
        end
        object lblSplitRegion: TLabel
          AlignWithMargins = True
          Left = 18
          Top = 9
          Width = 127
          Height = 15
          Margins.Left = 1
          Margins.Top = 7
          Caption = 'Split selection to, parts*:'
        end
        object sePartsCount: TSpinEdit
          AlignWithMargins = True
          Left = 151
          Top = 5
          Width = 50
          Height = 24
          MaxValue = 64
          MinValue = 2
          TabOrder = 1
          Value = 2
        end
      end
      object chkCloseAfterFinish: TCheckBox
        Left = 5
        Top = 21
        Width = 366
        Height = 16
        Align = alTop
        Caption = 'Close download window once finish'
        TabOrder = 1
      end
      object pnlAutosaveSession: TFlowPanel
        Left = 5
        Top = 141
        Width = 366
        Height = 30
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 6
        object chkAutosaveSession: TCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 3
          Width = 17
          Height = 22
          Margins.Left = 0
          Margins.Right = 0
          Margins.Bottom = 0
          TabOrder = 0
          OnClick = chkAutosaveSessionClick
        end
        object lblAutoSaveSession: TLabel
          AlignWithMargins = True
          Left = 18
          Top = 7
          Width = 120
          Height = 15
          Margins.Left = 1
          Margins.Top = 7
          Caption = 'Autosave session, min:'
        end
        object seAutosaveSession: TSpinEdit
          AlignWithMargins = True
          Left = 144
          Top = 3
          Width = 50
          Height = 24
          MaxValue = 999
          MinValue = 1
          TabOrder = 1
          Value = 15
        end
      end
      object pnlAutoSaveSessionPrefix: TFlowPanel
        AlignWithMargins = True
        Left = 20
        Top = 171
        Width = 351
        Height = 28
        Margins.Left = 15
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 7
        object chkSessionPrefix: TCheckBox
          AlignWithMargins = True
          Left = 0
          Top = 3
          Width = 17
          Height = 22
          Margins.Left = 0
          Margins.Right = 0
          Margins.Bottom = 0
          TabOrder = 0
          OnClick = chkSessionPrefixClick
        end
        object lblSessionPrefix: TLabel
          AlignWithMargins = True
          Left = 18
          Top = 7
          Width = 87
          Height = 15
          Margins.Left = 1
          Margins.Top = 7
          Caption = 'File name prefix:'
        end
        object edtSessionPrefix: TEdit
          AlignWithMargins = True
          Left = 111
          Top = 3
          Width = 130
          Height = 23
          Hint = 'It'#39's allowed to use \ as a path delimiter'
          TabOrder = 1
        end
      end
    end
  end
end
