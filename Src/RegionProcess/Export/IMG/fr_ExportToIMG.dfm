object frExportToIMG: TfrExportToIMG
  Left = 0
  Top = 0
  Width = 552
  Height = 445
  Align = alClient
  Anchors = [akLeft, akRight, akBottom]
  Constraints.MinHeight = 370
  Constraints.MinWidth = 480
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 552
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
      Alignment = taRightJustify
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      AlignWithMargins = True
      Left = 47
      Top = 3
      Width = 478
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 528
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 27
    Width = 552
    Height = 418
    ActivePage = tsSettings
    Align = alClient
    DoubleBuffered = False
    ParentDoubleBuffered = False
    TabOrder = 1
    object tsMap: TTabSheet
      Caption = 'Map'
      object pnlListMaps: TPanel
        Left = 0
        Top = 27
        Width = 391
        Height = 363
        Align = alClient
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 1
        object pnlMaps: TPanel
          Left = 3
          Top = 3
          Width = 385
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblMap: TLabel
            AlignWithMargins = True
            Left = 0
            Top = 3
            Width = 24
            Height = 19
            Margins.Left = 0
            Margins.Right = 10
            Align = alLeft
            Caption = 'Map:'
            Layout = tlCenter
          end
          object pnlMapselect: TPanel
            Left = 34
            Top = 0
            Width = 351
            Height = 25
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
          end
        end
        object pnlButtons: TPanel
          Left = 356
          Top = 28
          Width = 32
          Height = 332
          Align = alRight
          AutoSize = True
          BevelOuter = bvNone
          BorderWidth = 3
          TabOrder = 2
          DesignSize = (
            32
            332)
          object btnRemoveLayer: TButton
            Left = 3
            Top = 117
            Width = 26
            Height = 25
            Hint = 'Remove map layer'
            Anchors = [akTop, akRight]
            Caption = '-'
            Enabled = False
            TabOrder = 1
            OnClick = btnRemoveLayerClick
          end
          object btnAddLayer: TButton
            Left = 3
            Top = 86
            Width = 26
            Height = 25
            Hint = 'Add map layer'
            Anchors = [akTop, akRight]
            Caption = '+'
            Enabled = False
            TabOrder = 0
            OnClick = btnAddLayerClick
          end
        end
        object MapList: TListView
          Left = 3
          Top = 28
          Width = 353
          Height = 332
          Align = alClient
          Columns = <
            item
              AutoSize = True
              Caption = 'Map'
            end
            item
              AutoSize = True
              Caption = 'Zoom'
            end
            item
              AutoSize = True
              Caption = 'Garmin zoom'
            end>
          FlatScrollBars = True
          HideSelection = False
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          SortType = stData
          TabOrder = 1
          ViewStyle = vsReport
          OnCompare = MapListCompare
          OnCustomDrawItem = MapListCustomDrawItem
          OnDblClick = btnRemoveLayerClick
          OnSelectItem = MapListSelectItem
        end
      end
      object pnlZooms: TPanel
        Left = 426
        Top = 27
        Width = 118
        Height = 363
        Align = alRight
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 3
        object lblGarmin: TLabel
          Left = 3
          Top = 3
          Width = 112
          Height = 13
          Align = alTop
          Caption = 'Garmin zoom'
          Layout = tlBottom
        end
        object ZoomGarmin: TCheckListBox
          Left = 3
          Top = 16
          Width = 112
          Height = 215
          OnClickCheck = ZoomGarminClickCheck
          Align = alTop
          ItemHeight = 13
          Items.Strings = (
            '500..800km'
            '300km'
            '120..200km'
            '80km'
            '30..50km'
            '20km'
            '8..12km'
            '5km'
            '2..3km'
            '1.2km'
            '500..800m'
            '300m'
            '5..200m')
          TabOrder = 0
          OnClick = ZoomGarminClick
          OnDblClick = ZoomGarminDblClick
        end
        object TBXSettings: TTBXToolbar
          Left = 6
          Top = 239
          Width = 25
          Height = 24
          Align = alCustom
          Images = frmMain.MenusImageList
          ParentShowHint = False
          ShowHint = True
          ShrinkMode = tbsmWrap
          TabOrder = 1
          object tbSettings: TTBItem
            AutoCheck = True
            Hint = 'Toggle Options'
            ImageIndex = 20
            OnClick = tbSettingsClick
          end
        end
      end
      object pnlSasZoom: TPanel
        Left = 391
        Top = 27
        Width = 35
        Height = 363
        Align = alRight
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 2
        Visible = False
        object lblsas: TLabel
          Left = 3
          Top = 3
          Width = 29
          Height = 13
          Align = alTop
          Caption = 'SAS'
          Layout = tlBottom
        end
        object lstSasZooms: TListBox
          Left = 3
          Top = 16
          Width = 29
          Height = 215
          Align = alTop
          ItemHeight = 13
          TabOrder = 0
          OnClick = lstSasZoomsClick
          OnDblClick = lstSasZoomsDblClick
        end
        object TBXEdit: TTBXToolbar
          Left = 3
          Top = 239
          Width = 25
          Height = 50
          Align = alCustom
          AutoResize = False
          Images = frmMain.MenusImageList
          ParentShowHint = False
          ShowHint = True
          ShrinkMode = tbsmWrap
          TabOrder = 1
          object TBEdit: TTBItem
            Enabled = False
            Hint = 'Edit SAS Zoom'
            ImageIndex = 31
            OnClick = lstSasZoomsDblClick
          end
          object TBReset: TTBItem
            Enabled = False
            Hint = 'Reset SAS Zooms'
            ImageIndex = 49
            OnClick = TBResetClick
          end
        end
      end
      object pnlMapName: TPanel
        Left = 0
        Top = 0
        Width = 544
        Height = 27
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object lblMapName: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 6
          Width = 54
          Height = 15
          Margins.Left = 0
          Margins.Right = 10
          Align = alLeft
          Caption = 'Map Name:'
          Layout = tlCenter
        end
        object edtMapName: TEdit
          Left = 67
          Top = 3
          Width = 474
          Height = 21
          Align = alClient
          TabOrder = 0
        end
      end
    end
    object tsSettings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 1
      object pnlSettings: TPanel
        Left = 0
        Top = 0
        Width = 544
        Height = 161
        Align = alTop
        BevelOuter = bvNone
        Constraints.MinHeight = 90
        TabOrder = 0
        object lblMapSeries: TLabel
          Left = 3
          Top = 64
          Width = 52
          Height = 13
          Align = alCustom
          Caption = 'Map Series'
        end
        object lblMapID: TLabel
          Left = 321
          Top = 65
          Width = 34
          Height = 13
          Caption = 'Map ID'
        end
        object lblMapFormat: TLabel
          Left = 3
          Top = 36
          Width = 56
          Height = 13
          Caption = 'IMG Format'
        end
        object lblDrawOrder: TLabel
          Left = 321
          Top = 35
          Width = 56
          Height = 13
          Caption = 'Draw Order'
        end
        object lblVolumeSize: TLabel
          Left = 3
          Top = 92
          Width = 77
          Height = 13
          Caption = 'Volume Size, MB'
        end
        object lblCodePage: TLabel
          Left = 3
          Top = 8
          Width = 75
          Height = 13
          Caption = 'Map Code Page'
        end
        object lblCompression: TLabel
          Left = 321
          Top = 92
          Width = 61
          Height = 13
          Caption = 'JPEG Quality'
        end
        object edtMapID: TEdit
          Left = 436
          Top = 62
          Width = 74
          Height = 21
          Align = alCustom
          MaxLength = 8
          TabOrder = 4
        end
        object edtDrawOrder: TSpinEdit
          Left = 436
          Top = 32
          Width = 74
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 2
          Value = 24
        end
        object cbbMapFormat: TComboBox
          Left = 113
          Top = 32
          Width = 184
          Height = 21
          Style = csDropDownList
          ItemIndex = 2
          TabOrder = 1
          Text = 'New Format'
          Items.Strings = (
            'Old Format'
            'Old Format in GMP'
            'New Format')
        end
        object chkUseRecolor: TCheckBox
          Left = 3
          Top = 118
          Width = 538
          Height = 17
          Align = alCustom
          Caption = 'Use postprocessing settings'
          TabOrder = 8
        end
        object edtMapSeries: TMaskEdit
          Left = 113
          Top = 61
          Width = 184
          Height = 21
          AutoSize = False
          EditMask = '099;0; '
          MaxLength = 3
          TabOrder = 3
          Text = '36'
        end
        object tbxtlbrGenerateId: TTBXToolbar
          Left = 516
          Top = 62
          Width = 25
          Height = 24
          Align = alCustom
          Images = frmMain.MenusImageList
          ParentShowHint = False
          ShowHint = True
          ShrinkMode = tbsmWrap
          TabOrder = 5
          object tbtmGenerateId: TTBItem
            Hint = 'Generate'
            ImageIndex = 40
            OnClick = tbtmGenerateIdClick
          end
        end
        object cbbCodePage: TComboBox
          Left = 113
          Top = 4
          Width = 184
          Height = 21
          Style = csDropDownList
          DropDownCount = 13
          TabOrder = 0
          Items.Strings = (
            'CP874 (Thai Sort)'
            'CP932 (Japanese Sort)'
            'CP936 (Simplified Chinese Sort)'
            'CP949 (Korean Sort)'
            'CP950 (Traditional Chinese Sort)'
            'CP1250 (Central European Sort)'
            'CP1251 (Cyrillic Sort)'
            'CP1252 (Western European Sort)'
            'CP1253 (Greek Sort)'
            'CP1254 (Turkish Sort)'
            'CP1255 (Hebrew Sort)'
            'CP1256 (Arabic Sort)'
            'CP1257 (Baltic Sort)')
        end
        object chkKeepTempFiles: TCheckBox
          Left = 3
          Top = 141
          Width = 538
          Height = 17
          Align = alCustom
          Caption = 'Don'#39't delete the temporary files'
          TabOrder = 9
        end
        object seVolumeSize: TSpinEdit
          Left = 113
          Top = 88
          Width = 184
          Height = 22
          Hint = 'Restricted to 4096 MB by the IMG file format'
          MaxValue = 4096
          MinValue = 1
          TabOrder = 6
          Value = 1945
        end
        object seJpegQuality: TSpinEdit
          Left = 436
          Top = 88
          Width = 74
          Height = 22
          MaxValue = 100
          MinValue = 1
          TabOrder = 7
          Value = 95
        end
      end
      object pnlLicense: TPanel
        Left = 0
        Top = 204
        Width = 544
        Height = 42
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 2
        object lblMapCompilerLicensePath: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 532
          Height = 13
          Margins.Top = 0
          Align = alTop
          Caption = 'Path to map compiler license:'
        end
        object pnlLicensePath: TPanel
          Left = 3
          Top = 19
          Width = 538
          Height = 21
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object edtMapCompilerLicensePath: TEdit
            AlignWithMargins = True
            Left = 0
            Top = 0
            Width = 514
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            Margins.Bottom = 0
            Align = alClient
            TabOrder = 0
            OnChange = edtMapCompilerLicensePathChange
          end
          object btnSetMapCompilerLicensePath: TButton
            Left = 517
            Top = 0
            Width = 21
            Height = 21
            Align = alRight
            Caption = '...'
            TabOrder = 1
            OnClick = btnSetMapCompilerLicensePathClick
          end
        end
      end
      object pnlGMT: TPanel
        Left = 0
        Top = 246
        Width = 544
        Height = 44
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 3
        object edtGMTPath: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 20
          Width = 514
          Height = 21
          Margins.Left = 0
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alClient
          TabOrder = 1
          OnChange = edtGMTPathChange
        end
        object btnSetGMTPath: TButton
          Left = 520
          Top = 20
          Width = 21
          Height = 21
          Margins.Left = 10
          Margins.Right = 20
          Margins.Bottom = 100
          Align = alRight
          Caption = '...'
          TabOrder = 2
          OnClick = btnSetGMTPathClick
        end
        object pnlGMTTop: TPanel
          Left = 3
          Top = 3
          Width = 538
          Height = 17
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          object lblWebSite: TLabel
            AlignWithMargins = True
            Left = 505
            Top = 0
            Width = 3
            Height = 14
            Cursor = crHandPoint
            Margins.Top = 0
            Margins.Right = 30
            Align = alRight
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsUnderline]
            ParentFont = False
            OnClick = lblWebSiteClick
          end
          object lblGMTPath: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 0
            Width = 85
            Height = 17
            Margins.Top = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alLeft
            Caption = 'Path to GMT.exe:'
          end
        end
      end
      object pnlCompiler: TPanel
        Left = 0
        Top = 161
        Width = 544
        Height = 43
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 1
        object lblMapCompilerPath: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 532
          Height = 13
          Margins.Top = 0
          Align = alTop
          Caption = 'Path to map compiler:'
        end
        object pnlCompilerPath: TPanel
          Left = 3
          Top = 19
          Width = 538
          Height = 21
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object edtMapCompilerPath: TEdit
            AlignWithMargins = True
            Left = 0
            Top = 0
            Width = 514
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            Margins.Bottom = 0
            Align = alClient
            TabOrder = 0
            OnChange = edtMapCompilePathChange
          end
          object btnSetMapCompilerPath: TButton
            Left = 517
            Top = 0
            Width = 21
            Height = 21
            Align = alRight
            Caption = '...'
            TabOrder = 1
            OnClick = btnSetMapCompilerPathClick
          end
        end
      end
      object pnlTemp: TPanel
        Left = 0
        Top = 290
        Width = 544
        Height = 43
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 4
        object lblTempPath: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 532
          Height = 13
          Margins.Top = 0
          Align = alTop
          Caption = 'Path to temporary files:'
        end
        object pnlCompilerPath1: TPanel
          Left = 3
          Top = 19
          Width = 538
          Height = 21
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object edtTempPath: TEdit
            AlignWithMargins = True
            Left = 0
            Top = 0
            Width = 514
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            Margins.Bottom = 0
            Align = alClient
            TabOrder = 0
            OnChange = edtTempPathChange
          end
          object btnSetTempPath: TButton
            Left = 517
            Top = 0
            Width = 21
            Height = 21
            Align = alRight
            Caption = '...'
            TabOrder = 1
            OnClick = btnSetTempPathClick
          end
        end
      end
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 480
    Top = 392
  end
  object dlgSetMapCompilerPath: TOpenDialog
    Filter = 'bld_gmap32.exe|bld_gmap32.exe'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 56
    Top = 392
  end
  object dlgSetMapCompilerLicensePath: TOpenDialog
    Filter = 'License Files (*.mpl)|*.mpl'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 96
    Top = 392
  end
  object dlgSetGMTPath: TOpenDialog
    Filter = 'GMT.exe|gmt.exe'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 16
    Top = 392
  end
end
