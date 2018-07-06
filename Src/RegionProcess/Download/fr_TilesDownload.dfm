object frTilesDownload: TfrTilesDownload
  Left = 0
  Top = 0
  Width = 480
  Height = 304
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
  object pnlBottom: TPanel
    Left = 0
    Top = 84
    Width = 373
    Height = 220
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 3
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 373
      Height = 220
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object lblSplitRegionHint: TLabel
        AlignWithMargins = True
        Left = 8
        Top = 194
        Width = 357
        Height = 13
        Margins.Top = 0
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsItalic]
        ParentFont = False
        ExplicitTop = 223
        ExplicitWidth = 3
      end
      object chkReplace: TCheckBox
        Left = 5
        Top = 81
        Width = 363
        Height = 16
        Align = alTop
        Caption = 'Overwrite old tiles'
        TabOrder = 3
        OnClick = chkReplaceClick
      end
      object chkTryLoadIfTNE: TCheckBox
        Left = 5
        Top = 37
        Width = 363
        Height = 16
        Align = alTop
        Caption = 'Try download if tne exists'
        TabOrder = 1
        OnClick = chkTryLoadIfTNEClick
      end
      object pnlTileReplaceCondition: TPanel
        Left = 5
        Top = 97
        Width = 363
        Height = 44
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        Padding.Left = 15
        TabOrder = 4
        object chkReplaceIfDifSize: TCheckBox
          Left = 18
          Top = 3
          Width = 342
          Height = 16
          Align = alTop
          Caption = 'only if different'
          Enabled = False
          TabOrder = 0
        end
        object pnlReplaceOlder: TPanel
          Left = 18
          Top = 19
          Width = 342
          Height = 22
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object lblReplaceOlder: TLabel
            Left = 16
            Top = 0
            Width = 95
            Height = 22
            Align = alLeft
            Caption = 'only created before'
            Layout = tlCenter
            ExplicitHeight = 13
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
            Left = 111
            Top = 0
            Width = 81
            Height = 22
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
        Width = 363
        Height = 16
        Align = alTop
        Caption = 'Start paused'
        TabOrder = 0
      end
      object pnlLoadIfTneParams: TPanel
        Left = 5
        Top = 53
        Width = 363
        Height = 28
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        Padding.Left = 15
        TabOrder = 2
        object pnlLoadIfTneOld: TPanel
          Left = 18
          Top = 3
          Width = 342
          Height = 22
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblLoadIfTneOld: TLabel
            Left = 16
            Top = 0
            Width = 95
            Height = 22
            Align = alLeft
            Caption = 'only created before'
            Layout = tlCenter
            ExplicitHeight = 13
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
            Left = 111
            Top = 0
            Width = 81
            Height = 22
            Align = alLeft
            Date = 39513.436381111110000000
            Time = 39513.436381111110000000
            Enabled = False
            TabOrder = 1
          end
        end
      end
      object flwpnlSplitRegionParams: TFlowPanel
        Left = 5
        Top = 164
        Width = 363
        Height = 30
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        Padding.Top = 2
        TabOrder = 5
        ExplicitTop = 193
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
          Left = 17
          Top = 9
          Width = 120
          Height = 13
          Margins.Left = 0
          Margins.Top = 7
          Caption = 'Split selection to, parts*:'
        end
        object sePartsCount: TSpinEdit
          AlignWithMargins = True
          Left = 143
          Top = 5
          Width = 50
          Height = 22
          MaxValue = 24
          MinValue = 2
          TabOrder = 1
          Value = 2
        end
      end
      object chkCloseAfterFinish: TCheckBox
        Left = 5
        Top = 21
        Width = 363
        Height = 16
        Align = alTop
        Caption = 'Close download window once finish'
        TabOrder = 6
      end
      object pnlAutosaveSession: TPanel
        Left = 5
        Top = 141
        Width = 363
        Height = 23
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 7
        object lblAutoSaveSession: TLabel
          Left = 17
          Top = 4
          Width = 111
          Height = 13
          Caption = 'Autosave session, min:'
        end
        object seAutosaveSession: TSpinEdit
          Left = 143
          Top = 1
          Width = 50
          Height = 22
          MaxValue = 999
          MinValue = 1
          TabOrder = 0
          Value = 15
        end
        object chkAutosaveSession: TCheckBox
          Left = 0
          Top = 0
          Width = 17
          Height = 22
          TabOrder = 1
          OnClick = chkAutosaveSessionClick
        end
      end
    end
  end
  object pnlMapSelect: TPanel
    Left = 0
    Top = 22
    Width = 373
    Height = 62
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 1
    object lblStat: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 44
      Width = 370
      Height = 13
      Margins.Right = 0
      Align = alBottom
      Caption = '_'
      ExplicitWidth = 6
    end
    object pnlFrame: TPanel
      Left = 0
      Top = 0
      Width = 373
      Height = 43
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object lblMapCaption: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 367
        Height = 13
        Align = alTop
        Caption = 'Map/Overlay layer:'
        ExplicitWidth = 93
      end
    end
  end
  object pnlZoom: TPanel
    Left = 376
    Top = 22
    Width = 104
    Height = 282
    Align = alRight
    Alignment = taLeftJustify
    BevelEdges = []
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
  end
end
