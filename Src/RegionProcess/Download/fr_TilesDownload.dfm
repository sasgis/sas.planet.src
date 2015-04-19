object frTilesDownload: TfrTilesDownload
  Left = 0
  Top = 0
  Width = 447
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  ExplicitWidth = 451
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = 'Download Tiles'
    TabOrder = 0
    ExplicitWidth = 451
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 84
    Width = 340
    Height = 220
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 340
      Height = 220
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      ExplicitWidth = 451
      object chkReplace: TCheckBox
        Left = 5
        Top = 37
        Width = 330
        Height = 16
        Align = alTop
        Caption = 'Overwrite old tiles'
        TabOrder = 0
        OnClick = chkReplaceClick
        ExplicitWidth = 441
      end
      object chkTryLoadIfTNE: TCheckBox
        Left = 5
        Top = 21
        Width = 330
        Height = 16
        Align = alTop
        Caption = 'Try to re-download missing tiles'
        TabOrder = 1
        ExplicitWidth = 441
      end
      object pnlTileReplaceCondition: TPanel
        Left = 5
        Top = 53
        Width = 330
        Height = 40
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        Padding.Left = 15
        TabOrder = 2
        ExplicitWidth = 441
        object chkReplaceIfDifSize: TCheckBox
          Left = 18
          Top = 3
          Width = 309
          Height = 13
          Align = alTop
          Caption = 'only if different'
          Enabled = False
          TabOrder = 0
          ExplicitWidth = 420
        end
        object pnlReplaceOlder: TPanel
          Left = 18
          Top = 16
          Width = 309
          Height = 21
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitWidth = 420
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
        Width = 330
        Height = 16
        Align = alTop
        Caption = 'Start paused'
        TabOrder = 3
        ExplicitWidth = 441
      end
    end
  end
  object pnlMapSelect: TPanel
    Left = 0
    Top = 22
    Width = 340
    Height = 62
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitWidth = 344
    object lblStat: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 44
      Width = 337
      Height = 13
      Margins.Right = 0
      Align = alBottom
      Caption = '_'
      ExplicitLeft = 0
      ExplicitWidth = 6
    end
    object pnlFrame: TPanel
      Left = 0
      Top = 0
      Width = 340
      Height = 43
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 3
      ExplicitTop = 4
      ExplicitWidth = 392
      object lblMapCaption: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 334
        Height = 13
        Align = alTop
        Caption = 'Map:'
        ExplicitLeft = 5
        ExplicitTop = 0
        ExplicitWidth = 340
      end
    end
  end
  object pnlZoom: TPanel
    Left = 343
    Top = 22
    Width = 104
    Height = 282
    Align = alRight
    Alignment = taLeftJustify
    BevelEdges = []
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 3
    ExplicitTop = 28
  end
end
