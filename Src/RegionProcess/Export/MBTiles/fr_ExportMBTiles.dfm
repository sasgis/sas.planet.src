object frExportMBTiles: TfrExportMBTiles
  Left = 0
  Top = 0
  Width = 550
  Height = 401
  Align = alClient
  Constraints.MinHeight = 230
  Constraints.MinWidth = 550
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 550
    Height = 374
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 467
      Height = 374
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 461
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Map:'
      end
      object lblOverlay: TLabel
        Left = 3
        Top = 39
        Width = 461
        Height = 15
        Align = alTop
        Caption = 'Overlay layer:'
      end
      object lblDesc: TLabel
        Left = 3
        Top = 231
        Width = 461
        Height = 15
        Align = alTop
        Caption = 'Description:'
      end
      object lblName: TLabel
        Left = 3
        Top = 193
        Width = 461
        Height = 15
        Align = alTop
        Caption = 'Name:'
      end
      object lblAttr: TLabel
        Left = 3
        Top = 269
        Width = 461
        Height = 15
        Align = alTop
        Caption = 'Attribution:'
      end
      object pnlMap: TPanel
        Left = 3
        Top = 16
        Width = 461
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlOverlay: TPanel
        Left = 3
        Top = 54
        Width = 461
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
      end
      object pnlImageFormat: TPanel
        Left = 3
        Top = 145
        Width = 461
        Height = 48
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 6
      end
      object chkUsePrevZoom: TCheckBox
        Left = 3
        Top = 94
        Width = 461
        Height = 17
        Align = alTop
        Caption = 'Use tiles from lower zooms (on unavalible tile)'
        TabOrder = 3
      end
      object chkUseXYZScheme: TCheckBox
        Left = 3
        Top = 111
        Width = 461
        Height = 17
        Align = alTop
        Caption = 'Use xyz scheme'
        TabOrder = 4
      end
      object edtDesc: TEdit
        Left = 3
        Top = 246
        Width = 461
        Height = 23
        Align = alTop
        TabOrder = 8
      end
      object edtName: TEdit
        Left = 3
        Top = 208
        Width = 461
        Height = 23
        Align = alTop
        TabOrder = 7
      end
      object edtAttr: TEdit
        Left = 3
        Top = 284
        Width = 461
        Height = 23
        Align = alTop
        TabOrder = 9
      end
      object chkAddVisibleLayers: TCheckBox
        Left = 3
        Top = 77
        Width = 461
        Height = 17
        Align = alTop
        Caption = 'Add visible Layers'
        TabOrder = 2
        OnClick = chkAddVisibleLayersClick
      end
      object chkMakeTileMillStruct: TCheckBox
        Left = 3
        Top = 128
        Width = 461
        Height = 17
        Align = alTop
        Caption = 'Make TileMill compatible structure'
        TabOrder = 5
      end
    end
    object PnlZoom: TPanel
      Left = 467
      Top = 0
      Width = 83
      Height = 374
      Align = alRight
      BevelOuter = bvNone
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
      TabOrder = 1
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 550
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object lblTargetFile: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      AlignWithMargins = True
      Left = 47
      Top = 3
      Width = 476
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 526
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object dlgSaveTo: TSaveDialog
    DefaultExt = 'mbtiles'
    Filter = 'mbtiles|*.mbtiles'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 344
    Top = 344
  end
end
