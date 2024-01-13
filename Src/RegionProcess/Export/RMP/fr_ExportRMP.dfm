object frExportRMP: TfrExportRMP
  Left = 0
  Top = 0
  Width = 451
  Height = 345
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
    Height = 318
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 368
      Height = 318
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
      end
      object lblOverlay: TLabel
        Left = 3
        Top = 39
        Width = 362
        Height = 15
        Align = alTop
        Caption = 'Overlay layer:'
      end
      object lblDesc: TLabel
        Left = 3
        Top = 177
        Width = 362
        Height = 15
        Align = alTop
        Caption = 'Product:'
      end
      object lblAttr: TLabel
        Left = 3
        Top = 215
        Width = 362
        Height = 15
        Align = alTop
        Caption = 'Provider:'
      end
      object pnlMap: TPanel
        Left = 3
        Top = 16
        Width = 362
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlOverlay: TPanel
        Left = 3
        Top = 54
        Width = 362
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
      end
      object pnlImageFormat: TPanel
        Left = 3
        Top = 128
        Width = 362
        Height = 49
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 5
        object lblJpgQulity: TLabel
          Left = 2
          Top = 6
          Width = 67
          Height = 15
          Caption = 'JPEG quality:'
          Layout = tlCenter
        end
        object seJpgQuality: TSpinEdit
          Left = 2
          Top = 25
          Width = 151
          Height = 24
          MaxValue = 100
          MinValue = 1
          TabOrder = 0
          Value = 75
        end
      end
      object chkUsePrevZoom: TCheckBox
        Left = 3
        Top = 94
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Use tiles from lower zooms (on unavalible tile)'
        TabOrder = 3
      end
      object edtRmpProduct: TEdit
        Left = 3
        Top = 192
        Width = 362
        Height = 23
        Align = alTop
        TabOrder = 6
      end
      object edtRmpProvider: TEdit
        Left = 3
        Top = 230
        Width = 362
        Height = 23
        Align = alTop
        TabOrder = 7
      end
      object chkAddVisibleLayers: TCheckBox
        Left = 3
        Top = 77
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Add visible Layers'
        TabOrder = 2
        OnClick = chkAddVisibleLayersClick
      end
      object chkDontProjectToLatLon: TCheckBox
        Left = 3
        Top = 111
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Don'#39't transform tiles to Geographic projection'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
    end
    object pnlZoom: TPanel
      Left = 368
      Top = 0
      Width = 83
      Height = 318
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
      Width = 377
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
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
    Left = 328
    Top = 288
  end
end
