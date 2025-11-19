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
        Top = 42
        Width = 461
        Height = 13
        Align = alTop
        Caption = 'Overlay layer:'
      end
      object pnlMap: TPanel
        Left = 3
        Top = 19
        Width = 461
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlOverlay: TPanel
        Left = 3
        Top = 55
        Width = 461
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
      end
      object pgcMain: TPageControl
        Left = 3
        Top = 78
        Width = 461
        Height = 259
        ActivePage = tsSettings
        Align = alTop
        TabOrder = 2
        StyleElements = [seFont, seClient]
        object tsSettings: TTabSheet
          Caption = 'Settings'
          object chkAddVisibleLayers: TCheckBox
            Left = 0
            Top = 0
            Width = 453
            Height = 17
            Align = alTop
            Caption = 'Add visible Layers'
            TabOrder = 0
            OnClick = chkAddVisibleLayersClick
          end
          object chkAddVisibleOverlays: TCheckBox
            Left = 0
            Top = 17
            Width = 453
            Height = 17
            Align = alTop
            Caption = 'Add visible Placemarks, Grids, Cached Tiles Map'
            TabOrder = 1
          end
          object chkForceDropTarget: TCheckBox
            Left = 0
            Top = 34
            Width = 453
            Height = 17
            Align = alTop
            Caption = 'Recreate target database if exists'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object chkMakeTileMillStruct: TCheckBox
            Left = 0
            Top = 119
            Width = 453
            Height = 17
            Align = alTop
            Caption = 'Make TileMill compatible structure'
            TabOrder = 7
          end
          object chkReplaceExistingTiles: TCheckBox
            Left = 0
            Top = 51
            Width = 453
            Height = 17
            Align = alTop
            Caption = 'Replace existing tiles'
            Checked = True
            State = cbChecked
            TabOrder = 3
          end
          object chkUsePrevZoom: TCheckBox
            Left = 0
            Top = 85
            Width = 453
            Height = 17
            Align = alTop
            Caption = 'Use tiles from lower zooms (on unavalible tile)'
            TabOrder = 5
          end
          object chkUseRecolor: TCheckBox
            Left = 0
            Top = 68
            Width = 453
            Height = 17
            Align = alTop
            Caption = 'Use postprocessing settings'
            TabOrder = 4
          end
          object chkUseXYZScheme: TCheckBox
            Left = 0
            Top = 102
            Width = 453
            Height = 17
            Align = alTop
            Caption = 'Use XYZ scheme'
            TabOrder = 6
          end
          object pnlImageFormat: TPanel
            Left = 0
            Top = 136
            Width = 453
            Height = 48
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 8
          end
        end
        object tsMapParams: TTabSheet
          Caption = 'Map'
          ImageIndex = 1
          object lblName: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 447
            Height = 13
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Name:'
          end
          object lblDesc: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 43
            Width = 447
            Height = 13
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Description:'
          end
          object lblAttr: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 83
            Width = 447
            Height = 13
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Attribution:'
          end
          object edtName: TEdit
            AlignWithMargins = True
            Left = 3
            Top = 19
            Width = 447
            Height = 21
            Margins.Bottom = 0
            Align = alTop
            TabOrder = 0
          end
          object edtDesc: TEdit
            AlignWithMargins = True
            Left = 3
            Top = 59
            Width = 447
            Height = 21
            Margins.Bottom = 0
            Align = alTop
            TabOrder = 1
          end
          object edtAttr: TEdit
            AlignWithMargins = True
            Left = 3
            Top = 99
            Width = 447
            Height = 21
            Margins.Bottom = 0
            Align = alTop
            TabOrder = 2
          end
        end
      end
    end
    object pnlZoom: TPanel
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
      Height = 18
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
    Left = 344
    Top = 344
  end
end
