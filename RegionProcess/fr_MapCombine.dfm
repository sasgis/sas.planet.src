object frMapCombine: TfrMapCombine
  Left = 0
  Top = 0
  Width = 535
  Height = 224
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlTargetFile: TPanel
    Left = 0
    Top = 0
    Width = 535
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
      Height = 19
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 464
      Height = 19
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 511
      Top = 3
      Width = 21
      Height = 19
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 25
    Width = 535
    Height = 119
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object pnlZoom: TPanel
      Left = 479
      Top = 0
      Width = 56
      Height = 119
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      Constraints.MinWidth = 56
      TabOrder = 0
      object lblZoom: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 50
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Zoom:'
      end
      object cbbZoom: TComboBox
        Left = 3
        Top = 16
        Width = 50
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbZoomChange
      end
    end
    object pnlCenterMain: TPanel
      Left = 0
      Top = 0
      Width = 479
      Height = 119
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object pnlMapSource: TPanel
        Left = 0
        Top = 0
        Width = 479
        Height = 120
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object lblMap: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 473
          Height = 13
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = 'Base Map:'
          Layout = tlCenter
        end
        object lblHybr: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 40
          Width = 473
          Height = 13
          Margins.Left = 0
          Margins.Right = 0
          Align = alTop
          Caption = 'Overlay layer:'
          Layout = tlCenter
        end
        object lblStat: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 104
          Width = 467
          Height = 13
          Align = alTop
          Caption = '_'
          Layout = tlCenter
        end
        object cbbMap: TComboBox
          Left = 3
          Top = 16
          Width = 473
          Height = 21
          Align = alTop
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 13
          TabOrder = 0
        end
        object cbbHybr: TComboBox
          Left = 3
          Top = 53
          Width = 473
          Height = 21
          Align = alTop
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = 'No'
          Items.Strings = (
            'No')
        end
        object pnlProjection: TPanel
          Left = 3
          Top = 74
          Width = 473
          Height = 27
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 2
          object lblProjection: TLabel
            Left = 0
            Top = 0
            Width = 52
            Height = 27
            Align = alLeft
            Caption = 'Projection:'
            Layout = tlCenter
          end
          object cbbProjection: TComboBox
            Left = 52
            Top = 0
            Width = 421
            Height = 21
            Align = alClient
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
          end
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 144
    Width = 535
    Height = 80
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object pnlOptions: TPanel
      Left = 0
      Top = 0
      Width = 210
      Height = 80
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      Constraints.MinHeight = 80
      Constraints.MinWidth = 210
      TabOrder = 0
      object chkUseMapMarks: TCheckBox
        Left = 3
        Top = 37
        Width = 204
        Height = 17
        Align = alTop
        Caption = 'Add visible placemarks'
        TabOrder = 0
      end
      object chkUseRecolor: TCheckBox
        Left = 3
        Top = 20
        Width = 204
        Height = 17
        Align = alTop
        Caption = 'Use postprocessing settings'
        TabOrder = 1
      end
      object flwpnlJpegQuality: TFlowPanel
        Left = 3
        Top = 54
        Width = 204
        Height = 24
        Align = alTop
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        Padding.Top = 2
        TabOrder = 2
        object lblJpgQulity: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 5
          Width = 137
          Height = 13
          Alignment = taRightJustify
          Caption = 'Quality (for JPEG and ECW):'
          Layout = tlCenter
        end
        object seJpgQuality: TSpinEdit
          Left = 143
          Top = 2
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 1
          TabOrder = 0
          Value = 95
        end
      end
      object chkPngWithAlpha: TCheckBox
        Left = 3
        Top = 3
        Width = 204
        Height = 17
        Align = alTop
        Caption = 'Save PNG with alpha channel'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object pnlPrTypes: TPanel
      Left = 210
      Top = 0
      Width = 157
      Height = 80
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
        Height = 59
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
      Left = 367
      Top = 0
      Width = 168
      Height = 80
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
        Height = 63
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
      end
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Left = 120
    Top = 128
  end
end
