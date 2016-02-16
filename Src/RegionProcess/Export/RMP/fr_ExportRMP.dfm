object frExportRMP: TfrExportRMP
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  Constraints.MinHeight = 230
  Constraints.MinWidth = 451
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 451
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 368
      Height = 277
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 362
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Map:'
        ExplicitWidth = 461
      end
      object lblOverlay: TLabel
        Left = 3
        Top = 42
        Width = 362
        Height = 13
        Align = alTop
        Caption = 'Overlay layer:'
        ExplicitWidth = 69
      end
      object lblDesc: TLabel
        Left = 3
        Top = 178
        Width = 362
        Height = 13
        Align = alTop
        Caption = 'Product:'
        ExplicitTop = 195
        ExplicitWidth = 41
      end
      object lblAttr: TLabel
        Left = 3
        Top = 212
        Width = 362
        Height = 13
        Align = alTop
        Caption = 'Provider:'
        ExplicitTop = 229
        ExplicitWidth = 44
      end
      object pnlMap: TPanel
        Left = 3
        Top = 19
        Width = 362
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlOverlay: TPanel
        Left = 3
        Top = 55
        Width = 362
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
      end
      object pnlImageFormat: TPanel
        Left = 3
        Top = 129
        Width = 362
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitTop = 146
        object lblJpgQulity: TLabel
          Left = 2
          Top = 6
          Width = 63
          Height = 13
          Caption = 'JPEG quality:'
          Layout = tlCenter
        end
        object seJpgQuality: TSpinEdit
          Left = 2
          Top = 25
          Width = 151
          Height = 22
          MaxValue = 100
          MinValue = 1
          TabOrder = 0
          Value = 75
        end
      end
      object chkUsePrevZoom: TCheckBox
        Left = 3
        Top = 95
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Use tiles from lower zooms (on unavalible tile)'
        TabOrder = 3
      end
      object edtRmpProduct: TEdit
        Left = 3
        Top = 191
        Width = 362
        Height = 21
        Align = alTop
        TabOrder = 4
        ExplicitTop = 208
      end
      object edtRmpProvider: TEdit
        Left = 3
        Top = 225
        Width = 362
        Height = 21
        Align = alTop
        TabOrder = 5
        ExplicitTop = 242
      end
      object chkAddVisibleLayers: TCheckBox
        Left = 3
        Top = 78
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Add visible Layers'
        TabOrder = 6
        OnClick = chkAddVisibleLayersClick
      end
      object chkDontProjectToLatLon: TCheckBox
        Left = 3
        Top = 112
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Don'#39't project tiles to Geographic projection (EPSG:4326)'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
    end
    object PnlZoom: TPanel
      Left = 368
      Top = 0
      Width = 83
      Height = 277
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
    Width = 451
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
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
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 380
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 427
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
    DefaultExt = 'rmp'
    Filter = 'rmp|*.rmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 416
    Top = 272
  end
end
