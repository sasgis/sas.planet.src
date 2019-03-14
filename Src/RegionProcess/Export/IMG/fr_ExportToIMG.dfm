object frExportToIMG: TfrExportToIMG
  Left = 0
  Top = 0
  Width = 480
  Height = 370
  Align = alClient
  Anchors = [akLeft, akRight, akBottom]
  Constraints.MinHeight = 370
  Constraints.MinWidth = 480
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  ExplicitHeight = 372
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 480
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
      Height = 13
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Alignment = taRightJustify
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 409
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 456
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 27
    Width = 480
    Height = 343
    ActivePage = Map
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 313
    object Map: TTabSheet
      Caption = 'Map'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 285
      object pnlListMaps: TPanel
        Left = 0
        Top = 27
        Width = 327
        Height = 288
        Align = alClient
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 1
        ExplicitHeight = 258
        object pnlMaps: TPanel
          Left = 3
          Top = 3
          Width = 321
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblMap: TLabel
            AlignWithMargins = True
            Left = 0
            Top = 3
            Width = 24
            Height = 13
            Margins.Left = 0
            Margins.Right = 10
            Align = alLeft
            Caption = 'Map:'
            Layout = tlCenter
          end
          object pnlMapselect: TPanel
            Left = 34
            Top = 0
            Width = 287
            Height = 25
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
          end
        end
        object pnlButtons: TPanel
          Left = 292
          Top = 28
          Width = 32
          Height = 259
          Align = alRight
          AutoSize = True
          BevelOuter = bvNone
          BorderWidth = 3
          TabOrder = 2
          ExplicitHeight = 227
          DesignSize = (
            32
            257)
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
          Width = 289
          Height = 257
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
          ExplicitHeight = 227
        end
      end
      object pnlZooms: TPanel
        Left = 362
        Top = 27
        Width = 110
        Height = 288
        Align = alRight
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 3
        ExplicitHeight = 258
        object lblGarmin: TLabel
          Left = 3
          Top = 3
          Width = 104
          Height = 13
          Align = alTop
          Caption = 'Garmin zoom'
          Layout = tlBottom
          ExplicitWidth = 61
        end
        object ZoomGarmin: TCheckListBox
          Left = 3
          Top = 16
          Width = 104
          Height = 183
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
          Top = 205
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
            ImageIndex = 20
            OnClick = tbSettingsClick
            Caption = ''
            Hint = 'Toggle Options'
          end
        end
      end
      object pnlSasZoom: TPanel
        Left = 327
        Top = 27
        Width = 35
        Height = 288
        Align = alRight
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 2
        Visible = False
        ExplicitHeight = 258
        object lblsas: TLabel
          Left = 3
          Top = 3
          Width = 19
          Height = 13
          Align = alTop
          Caption = 'SAS'
          Layout = tlBottom
        end
        object lstSasZooms: TListBox
          Left = 3
          Top = 16
          Width = 29
          Height = 183
          Align = alTop
          ItemHeight = 13
          TabOrder = 0
          OnClick = lstSasZoomsClick
          OnDblClick = lstSasZoomsDblClick
        end
        object TBXEdit: TTBXToolbar
          Left = 4
          Top = 205
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
            ImageIndex = 31
            OnClick = lstSasZoomsDblClick
            Caption = ''
            Hint = 'Edit SAS Zoom'
          end
          object TBReset: TTBItem
            Enabled = False
            ImageIndex = 49
            OnClick = TBResetClick
            Caption = ''
            Hint = 'Reset SAS Zooms'
          end
        end
      end
      object pnlMapName: TPanel
        Left = 0
        Top = 0
        Width = 472
        Height = 27
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object lblMapName: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 6
          Width = 50
          Height = 13
          Margins.Left = 0
          Margins.Right = 10
          Align = alLeft
          Caption = 'Map Name:'
          Layout = tlCenter
        end
        object edtMapName: TEdit
          Left = 63
          Top = 3
          Width = 406
          Height = 21
          Align = alClient
          TabOrder = 0
        end
      end
    end
    object Settings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 285
      object PnlSettings: TPanel
        Left = 0
        Top = 0
        Width = 472
        Height = 170
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
          Left = 273
          Top = 64
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
          Left = 273
          Top = 36
          Width = 56
          Height = 13
          Caption = 'Draw Order'
        end
        object lblVolumeSize: TLabel
          Left = 3
          Top = 92
          Width = 56
          Height = 13
          Caption = 'Volume Size'
        end
        object lblCodePage: TLabel
          Left = 3
          Top = 8
          Width = 75
          Height = 13
          Caption = 'Map Code Page'
        end
        object edtMapID: TEdit
          Left = 379
          Top = 60
          Width = 74
          Height = 21
          Align = alCustom
          MaxLength = 8
          TabOrder = 5
        end
        object edtDrawOrder: TSpinEdit
          Left = 379
          Top = 32
          Width = 45
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 2
          Value = 24
        end
        object cbbMapFormat: TComboBox
          Left = 113
          Top = 32
          Width = 142
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
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
          Width = 345
          Height = 17
          Align = alCustom
          Caption = 'Use postprocessing settings'
          TabOrder = 7
        end
        object edtMapSeries: TMaskEdit
          Left = 113
          Top = 60
          Width = 33
          Height = 21
          AutoSize = False
          EditMask = '099;0; '
          MaxLength = 3
          TabOrder = 4
          Text = '36'
        end
        object TBXGenerateId: TTBXToolbar
          Left = 459
          Top = 58
          Width = 25
          Height = 24
          Align = alCustom
          Images = frmMain.MenusImageList
          ParentShowHint = False
          ShowHint = True
          ShrinkMode = tbsmWrap
          TabOrder = 3
          object TBGenerateId: TTBItem
            ImageIndex = 40
            OnClick = TBGenerateIdClick
            Caption = ''
            Hint = 'Generate'
          end
        end
        object edtVolumeSize: TEdit
          Left = 113
          Top = 88
          Width = 142
          Height = 21
          TabOrder = 6
          Text = '$79999999'
        end
        object cbbCodePage: TComboBox
          Left = 113
          Top = 4
          Width = 204
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
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
          Top = 146
          Width = 345
          Height = 17
          Align = alCustom
          Caption = 'Keep temporarily files'
          TabOrder = 8
        end
      end
      object pnlLicense: TPanel
        Left = 0
        Top = 217
        Width = 472
        Height = 47
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 2
        ExplicitTop = 191
        object lblMapCompilerLicensePath: TLabel
          Left = 3
          Top = 3
          Width = 466
          Height = 19
          Align = alTop
          AutoSize = False
          Caption = 'Path to map compiler license:'
        end
        object LLicenseFile: TLabel
          Left = 381
          Top = 3
          Width = 88
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = '(*.mpl)'
        end
        object btnSetMapCompilerLicensePath: TButton
          Left = 448
          Top = 22
          Width = 21
          Height = 22
          Align = alRight
          Caption = '...'
          TabOrder = 1
          OnClick = btnSetMapCompilerLicensePathClick
        end
        object edtMapCompilerLicensePath: TEdit
          Left = 3
          Top = 22
          Width = 445
          Height = 22
          Align = alClient
          TabOrder = 0
          OnChange = edtMapCompilerLicensePathChange
          ExplicitHeight = 21
        end
      end
      object pnlGMT: TPanel
        Left = 0
        Top = 264
        Width = 472
        Height = 47
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 3
        object edtGMTPath: TEdit
          Left = 3
          Top = 22
          Width = 445
          Height = 22
          Margins.Left = 10
          Align = alClient
          TabOrder = 1
          OnChange = edtGMTPathChange
          ExplicitHeight = 21
        end
        object btnSetGMTPath: TButton
          Left = 448
          Top = 22
          Width = 21
          Height = 22
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
          Width = 466
          Height = 19
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblWebSite: TLabel
            Left = 209
            Top = 0
            Width = 257
            Height = 19
            Cursor = crHandPoint
            Align = alRight
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlue
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = [fsUnderline]
            ParentFont = False
            OnClick = lblWebSiteClick
            ExplicitLeft = 208
            ExplicitTop = 1
            ExplicitHeight = 17
          end
          object lblGMTPath: TLabel
            Left = 0
            Top = 0
            Width = 209
            Height = 19
            Margins.Left = 10
            Margins.Top = 100
            Margins.Right = 10
            Margins.Bottom = 100
            Align = alClient
            AutoSize = False
            Caption = 'Path to GMT.exe:'
            ExplicitLeft = 1
            ExplicitTop = 1
            ExplicitWidth = 194
            ExplicitHeight = 17
          end
        end
      end
      object pnlCompiler: TPanel
        Left = 0
        Top = 170
        Width = 472
        Height = 47
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 1
        ExplicitTop = 144
        object lblMapCompilerPath: TLabel
          Left = 3
          Top = 3
          Width = 466
          Height = 19
          Align = alTop
          AutoSize = False
          Caption = 'Path to map compiler:'
          ExplicitWidth = 345
        end
        object LMapCompilerPath: TLabel
          Left = 381
          Top = 3
          Width = 88
          Height = 13
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = '(bld_gmap32.exe)'
        end
        object btnSetMapCompilerPath: TButton
          Left = 448
          Top = 22
          Width = 21
          Height = 22
          Align = alRight
          Caption = '...'
          TabOrder = 1
          OnClick = btnSetMapCompilerPathClick
        end
        object edtMapCompilerPath: TEdit
          Left = 3
          Top = 22
          Width = 445
          Height = 22
          Align = alClient
          TabOrder = 0
          OnChange = edtMapCompilePathChange
          ExplicitHeight = 21
        end
      end
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 424
    Top = 8
  end
  object dlgSetMapCompilerPath: TOpenDialog
    Filter = 'bld_gmap32.exe|bld_gmap32.exe'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 416
    Top = 168
  end
  object dlgSetMapCompilerLicensePath: TOpenDialog
    Filter = 'License Files (*.mpl)|*.mpl'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 416
    Top = 216
  end
  object dlgSetGMTPath: TOpenDialog
    Filter = 'GMT.exe|gmt.exe'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 416
    Top = 264
  end
end
