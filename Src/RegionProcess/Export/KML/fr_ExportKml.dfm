object frExportKml: TfrExportKml
  Left = 0
  Top = 0
  Width = 499
  Height = 337
  Align = alClient
  Constraints.MinHeight = 120
  Constraints.MinWidth = 400
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 499
    Height = 310
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlZoom: TPanel
      Left = 424
      Top = 0
      Width = 75
      Height = 310
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 424
      Height = 310
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 418
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Map/Overlay layer:'
      end
      object lblInfo: TLabel
        AlignWithMargins = True
        Left = 6
        Top = 292
        Width = 412
        Height = 15
        Align = alBottom
        Caption = '_'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        Visible = False
      end
      object chkNotSaveNotExists: TCheckBox
        Left = 3
        Top = 59
        Width = 418
        Height = 17
        Align = alTop
        Caption = 'Don'#39't store references to non-existent tiles'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object chkUseRelativePath: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 42
        Width = 418
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Relative path to tiles'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object pnlMap: TPanel
        Left = 3
        Top = 16
        Width = 418
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
      object chkExtractTiles: TCheckBox
        Left = 3
        Top = 127
        Width = 418
        Height = 17
        Align = alTop
        Caption = 'Extract tiles from cache'
        Checked = True
        State = cbChecked
        TabOrder = 3
        OnClick = chkExtractTilesClick
      end
      object pnlFileNameGenerator: TPanel
        Left = 3
        Top = 144
        Width = 418
        Height = 42
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 4
        object lblFileNameGenerator: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 3
          Width = 415
          Height = 15
          Margins.Left = 0
          Margins.Bottom = 3
          Align = alTop
          Caption = 'Tile name format:'
        end
      end
      object chkAddVisibleOverlays: TCheckBox
        Left = 3
        Top = 76
        Width = 418
        Height = 17
        Align = alTop
        Caption = 'Add visible Layers/Placemarks/Grids etc.'
        TabOrder = 5
      end
      object chkUsePrevZoom: TCheckBox
        Left = 3
        Top = 110
        Width = 418
        Height = 17
        Align = alTop
        Caption = 'Use tiles from lower zooms (on unavalible tile)'
        TabOrder = 6
      end
      object chkPreciseCropping: TCheckBox
        Left = 3
        Top = 93
        Width = 418
        Height = 17
        Hint = 
          'Precise cropping of the image along the boundaries of the polygo' +
          'n'
        Align = alTop
        Caption = 'Precise cropping by the polygon'
        TabOrder = 7
      end
      object pnlImageFormat: TPanel
        Left = 3
        Top = 186
        Width = 418
        Height = 50
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 8
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 499
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
      Width = 425
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 475
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object dlgSaveKML: TSaveDialog
    DefaultExt = 'kml'
    Filter = 'KML |*.kml'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 352
    Top = 128
  end
end
