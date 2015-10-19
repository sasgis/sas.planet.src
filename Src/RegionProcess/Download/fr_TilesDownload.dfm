object frTilesDownload: TfrTilesDownload
  Left = 0
  Top = 0
  Width = 480
  Height = 200
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
    TabOrder = 1
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 373
      Height = 220
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object chkReplace: TCheckBox
        Left = 5
        Top = 37
        Width = 363
        Height = 16
        Align = alTop
        Caption = 'Overwrite old tiles'
        TabOrder = 0
        OnClick = chkReplaceClick
      end
      object chkTryLoadIfTNE: TCheckBox
        Left = 5
        Top = 21
        Width = 363
        Height = 16
        Align = alTop
        Caption = 'Try to re-download missing tiles'
        TabOrder = 1
      end
      object pnlTileReplaceCondition: TPanel
        Left = 5
        Top = 53
        Width = 363
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
          Width = 342
          Height = 13
          Align = alTop
          Caption = 'only if different'
          Enabled = False
          TabOrder = 0
        end
        object pnlReplaceOlder: TPanel
          Left = 18
          Top = 16
          Width = 342
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
        Top = 5
        Width = 363
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
    Width = 373
    Height = 62
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    object lblStat: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 47
      Width = 370
      Height = 13
      Margins.Right = 0
      Align = alBottom
      Caption = '_'
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
      end
    end
  end
  object pnlZoom: TPanel
    Left = 376
    Top = 22
    Width = 104
    Height = 178
    Align = alRight
    Alignment = taLeftJustify
    BevelEdges = []
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 3
  end
end
