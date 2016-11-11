object frMapCombine: TfrMapCombine
  Left = 0
  Top = 0
  Width = 540
  Height = 304
  Align = alClient
  Constraints.MinHeight = 290
  Constraints.MinWidth = 540
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlTargetFile: TPanel
    Left = 0
    Top = 0
    Width = 540
    Height = 25
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object lblTargetFile: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 13
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 469
      Height = 19
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 21
    end
    object btnSelectTargetFile: TButton
      Left = 516
      Top = 3
      Width = 21
      Height = 19
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 153
    Width = 540
    Height = 151
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlOptions: TPanel
      Left = 0
      Top = 0
      Width = 215
      Height = 151
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      Constraints.MinHeight = 80
      Constraints.MinWidth = 210
      TabOrder = 0
      object chkUseMapMarks: TCheckBox
        Left = 3
        Top = 37
        Width = 209
        Height = 17
        Align = alTop
        Caption = 'Add visible Placemarks'
        TabOrder = 0
        ExplicitTop = 88
      end
      object chkUseRecolor: TCheckBox
        Left = 3
        Top = 71
        Width = 209
        Height = 17
        Align = alTop
        Caption = 'Use postprocessing settings'
        TabOrder = 1
        ExplicitLeft = 0
        ExplicitTop = 77
      end
      object chkUseMapGrids: TCheckBox
        Left = 3
        Top = 20
        Width = 209
        Height = 17
        Align = alTop
        Caption = 'Add visible Grids'
        TabOrder = 2
        ExplicitTop = 71
      end
      object chkAddVisibleLayers: TCheckBox
        Left = 3
        Top = 54
        Width = 209
        Height = 17
        Align = alTop
        Caption = 'Add visible Layers'
        TabOrder = 3
        OnClick = chkAddVisibleLayersClick
        ExplicitTop = 105
      end
      object chkUseFillingMap: TCheckBox
        Left = 3
        Top = 3
        Width = 209
        Height = 17
        Align = alTop
        Caption = 'Add visible Cached Tiles Map'
        TabOrder = 4
        ExplicitTop = 54
      end
      object pnlCustomOpt: TPanel
        Left = 3
        Top = 88
        Width = 209
        Height = 81
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 5
        ExplicitTop = 153
        ExplicitHeight = 62
      end
    end
    object pnlPrTypes: TPanel
      Left = 215
      Top = 0
      Width = 157
      Height = 151
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      Constraints.MinWidth = 157
      TabOrder = 1
      object lblPrTypes: TLabel
        Left = 3
        Top = 3
        Width = 151
        Height = 15
        Align = alTop
        AutoSize = False
        Caption = 'Create georeferencing file:'
        WordWrap = True
      end
      object chklstPrTypes: TCheckListBox
        Left = 3
        Top = 18
        Width = 151
        Height = 130
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          '.map'
          '.tab'
          '.w'
          '.dat'
          '.kml')
        TabOrder = 0
      end
    end
    object pnlSplit: TPanel
      Left = 372
      Top = 0
      Width = 168
      Height = 151
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      Constraints.MinHeight = 71
      Constraints.MinWidth = 168
      TabOrder = 2
      object grpSplit: TGroupBox
        Left = 3
        Top = 3
        Width = 162
        Height = 86
        Align = alTop
        Caption = 'Split image'
        TabOrder = 0
        object lblSplitHor: TLabel
          Left = 7
          Top = 16
          Width = 59
          Height = 13
          Caption = 'horizontally:'
        end
        object lblSplitVert: TLabel
          AlignWithMargins = True
          Left = 7
          Top = 40
          Width = 47
          Height = 13
          Caption = 'vertically:'
        end
        object seSplitHor: TSpinEdit
          Left = 106
          Top = 13
          Width = 47
          Height = 22
          MaxValue = 1000
          MinValue = 1
          TabOrder = 0
          Value = 1
        end
        object seSplitVert: TSpinEdit
          Left = 106
          Top = 37
          Width = 47
          Height = 22
          MaxValue = 1000
          MinValue = 1
          TabOrder = 1
          Value = 1
        end
        object chkSkipExistingFiles: TCheckBox
          Left = 7
          Top = 62
          Width = 154
          Height = 17
          Caption = 'Skip existing result files'
          TabOrder = 2
        end
      end
    end
  end
  object pnlMapSelect: TPanel
    Left = 0
    Top = 25
    Width = 540
    Height = 128
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    object pnlZoom: TPanel
      Left = 469
      Top = 0
      Width = 71
      Height = 126
      Align = alRight
      Alignment = taLeftJustify
      BevelEdges = []
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
      object Labelzoom: TLabel
        AlignWithMargins = True
        Left = 3
        Top = -2
        Width = 30
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Caption = 'Zoom:'
      end
      object cbbZoom: TComboBox
        Left = 1
        Top = 11
        Width = 67
        Height = 21
        Style = csDropDownList
        TabOrder = 0
        OnChange = cbbZoomChange
      end
    end
    object pnlMaps: TPanel
      Left = 0
      Top = 0
      Width = 469
      Height = 126
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object lblStat: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 110
        Width = 6
        Height = 13
        Align = alBottom
        Caption = '_'
        Layout = tlCenter
      end
      object pnlMapFrame: TPanel
        Left = 0
        Top = 0
        Width = 469
        Height = 40
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        TabOrder = 0
        object lblMapCaption: TLabel
          Left = 0
          Top = 0
          Width = 24
          Height = 13
          Margins.Left = 0
          Margins.Top = 0
          Align = alTop
          Caption = 'Map:'
        end
      end
      object pnlLayerFrame: TPanel
        Left = 0
        Top = 40
        Width = 469
        Height = 40
        Align = alTop
        Alignment = taLeftJustify
        BevelOuter = bvNone
        TabOrder = 1
        object lblLayerCaption: TLabel
          Left = 0
          Top = 0
          Width = 69
          Height = 13
          Margins.Left = 0
          Margins.Top = 0
          Align = alTop
          Caption = 'Overlay layer:'
        end
      end
      object pnlProjection: TPanel
        Left = 0
        Top = 80
        Width = 469
        Height = 21
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        object lblProjection: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 0
          Width = 52
          Height = 13
          Margins.Top = 0
          Margins.Right = 5
          Margins.Bottom = 2
          Align = alLeft
          Caption = 'Projection:'
          Layout = tlCenter
        end
        object cbbProjection: TComboBox
          Left = 63
          Top = 0
          Width = 403
          Height = 21
          Align = alCustom
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
      end
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Left = 432
    Top = 224
  end
end
