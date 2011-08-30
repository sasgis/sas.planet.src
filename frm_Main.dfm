object frmMain: TfrmMain
  Left = 488
  Top = 165
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'SAS.Planet'
  ClientHeight = 535
  ClientWidth = 842
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poDefault
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object map: TImage32
    Left = 207
    Top = 59
    Width = 471
    Height = 467
    Align = alClient
    Bitmap.CombineMode = cmMerge
    Bitmap.ResamplerClassName = 'TLinearResampler'
    BitmapAlign = baTopLeft
    Color = clSilver
    ParentColor = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnDblClick = mapDblClick
    OnMouseDown = mapMouseDown
    OnMouseMove = mapMouseMove
    OnMouseUp = mapMouseUp
    OnMouseLeave = mapMouseLeave
    OnResize = mapResize
  end
  object TBDock: TTBXDock
    Left = 0
    Top = 0
    Width = 842
    Height = 59
    PopupMenu = TBXPopupPanels
    object TBMainToolBar: TTBXToolbar
      Left = 0
      Top = 25
      DockPos = -6
      DockRow = 1
      Images = PanelsImageList
      Stretch = True
      TabOrder = 0
      Caption = 'Main'
      object TBmove: TTBXItem
        Checked = True
        ImageIndex = 8
        Images = PanelsImageList
        Options = [tboDefault]
        OnClick = TBmoveClick
        Caption = ''
        Hint = 'Move'
      end
      object TBRectSave: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 10
        Images = PanelsImageList
        LinkSubitems = NRectSave
        Options = [tboShowHint]
        OnClick = TBRectSaveClick
        Caption = ''
        Hint = 'Selection manager'
      end
      object TBCalcRas: TTBXItem
        AutoCheck = True
        ImageIndex = 9
        Images = PanelsImageList
        OnClick = TBCalcRasClick
        Caption = ''
        Hint = 'Distance calculation'
      end
      object TBXSeparatorItem4: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object TBMapZap: TTBXSubmenuItem
        DisplayMode = nbdmImageAndText
        ImageIndex = 7
        Images = PanelsImageList
        LinkSubitems = NFillMap
        Options = [tboDropdownArrow, tboShowHint]
        Caption = ''
        Hint = 'Cached tiles map'
      end
      object TBGoTo: TTBXSubmenuItem
        DropdownCombo = True
        ImageIndex = 11
        Images = PanelsImageList
        Options = [tboShowHint]
        OnClick = TBSubmenuItem1Click
        Caption = ''
        Hint = 'Go to'
        object tbiEditYandexSrch: TTBEditItem
          EditCaption = 'Yandex'
          Caption = 'Yandex'
          Hint = ''
          EditCaption = 'Yandex'
        end
        object tbiEditGoogleSrch: TTBEditItem
          EditCaption = 'Google'
          EditWidth = 150
          Caption = 'Google'
          Hint = ''
          EditCaption = 'Google'
        end
        object tbiEdit2GISSrch: TTBEditItem
          EditCaption = '2GIS'
          EditWidth = 150
          Caption = '2GIS'
          Hint = ''
          EditCaption = '2GIS'
        end
      end
      object TBXSeparatorItem5: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object TBFullSize: TTBXItem
        AutoCheck = True
        ImageIndex = 4
        Images = PanelsImageList
        OnClick = TBFullSizeClick
        Caption = ''
        Hint = 'Full screen (F11)'
      end
    end
    object SrcToolbar: TTBXToolbar
      Left = 238
      Top = 25
      DockPos = 224
      DockRow = 1
      Stretch = True
      TabOrder = 1
      Caption = 'Sources'
      object TBSrc: TTBXSubmenuItem
        ImageIndex = 0
        Images = PanelsImageList
        LinkSubitems = NSources
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = 'Select data source'
      end
      object TBSMB: TTBXSubmenuItem
        DisplayMode = nbdmImageAndText
        ImageIndex = 3
        Images = PanelsImageList
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = 'Selected basemap'
      end
      object TBLayerSel: TTBXSubmenuItem
        ImageIndex = 3
        Images = PanelsImageList
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = 'Select overlay layers'
      end
    end
    object TBMarksToolbar: TTBXToolbar
      Left = 365
      Top = 25
      DockPos = 352
      DockRow = 1
      Images = PanelsImageList
      LinkSubitems = NMarks
      Stretch = True
      TabOrder = 2
      Caption = 'Placemarks'
    end
    object GPSToolbar: TTBXToolbar
      Left = 536
      Top = 25
      DockPos = 504
      DockRow = 1
      Images = PanelsImageList
      Stretch = True
      TabOrder = 3
      Caption = 'GPS'
      object TBGPSconn: TTBXItem
        AutoCheck = True
        ImageIndex = 14
        Images = PanelsImageList
        OnClick = TBGPSconnClick
        Caption = ''
        Hint = 'Connect to GPS receiver'
      end
      object TBGPSPath: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 6
        Images = PanelsImageList
        OnClick = TBGPSPathClick
        Caption = ''
        Hint = 'Show GPS track'
        object tbitmSaveCurrentPositionToolbar: TTBXItem
          ImageIndex = 15
          Images = MenusImageList
          OnClick = tbitmSaveCurrentPositionClick
          Caption = 'Add Placemark'
          Hint = ''
        end
        object TBXSeparatorItem16: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object TBItem5: TTBXItem
          ImageIndex = 25
          Images = MenusImageList
          OnClick = TBItem5Click
          Caption = 'Add Track to Database'
          Hint = ''
        end
        object TBXSeparatorItem17: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object TBItemDelTrack: TTBXItem
          ImageIndex = 35
          Images = MenusImageList
          OnClick = TBItemDelTrackClick
          Caption = 'Delete Track'
          Hint = ''
        end
      end
      object TBGPSToPoint: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 5
        Images = PanelsImageList
        OnClick = TBGPSToPointClick
        Caption = ''
        Hint = 'Follow GPS Position'
        object TBGPSToPointCenter: TTBXItem
          AutoCheck = True
          OnClick = TBGPSToPointCenterClick
          Caption = 'Centered GPS Position'
          Hint = ''
        end
      end
    end
    object TBExit: TTBXToolbar
      Left = 807
      Top = 25
      DockPos = 807
      DockRow = 1
      TabOrder = 4
      Visible = False
      object TBXExit: TTBXItem
        ImageIndex = 29
        Images = MenusImageList
        OnClick = TrayItemQuitClick
        Caption = ''
        Hint = 'Quit'
      end
    end
    object TBXMainMenu: TTBXToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DockPos = 2
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      Stretch = True
      TabOrder = 5
      Caption = 'Main Menu'
      object NOperations: TTBXSubmenuItem
        Caption = '&Operations'
        Hint = ''
        object N35: TTBXItem
          OnClick = N35Click
          Caption = 'Create Shortcut'
          Hint = ''
        end
        object TBXItem6: TTBXItem
          ImageIndex = 34
          Images = MenusImageList
          OnClick = TBXItem6Click
          Caption = 'Open...'
          Hint = ''
        end
        object TBXSeparatorItem6: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object NZoomIn: TTBXItem
          ImageIndex = 23
          Images = MenusImageList
          ShortCut = 33
          OnClick = TBZoomInClick
          Caption = 'Zoom In'
          Hint = ''
        end
        object NZoomOut: TTBXItem
          ImageIndex = 24
          Images = MenusImageList
          ShortCut = 34
          OnClick = TBZoom_outClick
          Caption = 'Zoom Out'
          Hint = ''
        end
        object TBXSeparatorItem7: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object N14: TTBXItem
          ImageIndex = 11
          Images = MenusImageList
          ShortCut = 16455
          OnClick = TBSubmenuItem1Click
          Caption = 'Go to...'
          Hint = ''
        end
        object NCalcRast: TTBXItem
          ImageIndex = 9
          Images = MenusImageList
          ShortCut = 16460
          OnClick = NCalcRastClick
          Caption = 'Distance Calculation'
          Hint = ''
        end
        object TBXSeparatorItem8: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object NRectSave: TTBXSubmenuItem
          Caption = 'Selection Manager'
          Hint = ''
          object TBRECT: TTBXItem
            ImageIndex = 10
            Images = PanelsImageList
            ShortCut = 32850
            OnClick = TBRECTClick
            Caption = 'Rectangular Selection'
            Hint = ''
          end
          object TBREGION: TTBXItem
            ImageIndex = 13
            Images = PanelsImageList
            ShortCut = 32848
            OnClick = TBREGIONClick
            Caption = 'Polygonal Selection'
            Hint = ''
          end
          object TBCOORD: TTBXItem
            ImageIndex = 12
            Images = PanelsImageList
            OnClick = TBCOORDClick
            Caption = 'By Coordinates'
            Hint = ''
          end
          object TBScreenSelect: TTBXItem
            ImageIndex = 20
            Images = PanelsImageList
            ShortCut = 16449
            OnClick = TBScreenSelectClick
            Caption = 'Visible Area'
            Hint = ''
          end
          object TBXSeparatorItem13: TTBXSeparatorItem
            Caption = ''
            Hint = ''
          end
          object TBPrevious: TTBXItem
            Images = PanelsImageList
            ShortCut = 16450
            OnClick = TBPreviousClick
            Caption = 'Last Selection'
            Hint = ''
          end
          object TBLoadSelFromFile: TTBXItem
            Images = PanelsImageList
            OnClick = TBLoadSelFromFileClick
            Caption = 'Load from File'
            Hint = ''
          end
        end
        object TBXSeparatorItem9: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object N6: TTBXItem
          ImageIndex = 29
          Images = MenusImageList
          OnClick = N6Click
          Caption = 'Quit'
          Hint = ''
        end
      end
      object NView: TTBXSubmenuItem
        Caption = '&View'
        Hint = ''
        object NPanels: TTBXSubmenuItem
          Caption = 'Toolbars'
          Hint = ''
          object NMainToolBarShow: TTBXVisibilityToggleItem
            Control = TBMainToolBar
            Caption = 'Main'
            Hint = ''
          end
          object NZoomToolBarShow: TTBXVisibilityToggleItem
            Control = ZoomToolBar
            Caption = 'Zoom'
            Hint = ''
          end
          object NsrcToolBarShow: TTBXVisibilityToggleItem
            Control = SrcToolbar
            Caption = 'Sources'
            Hint = ''
          end
          object NGPSToolBarShow: TTBXVisibilityToggleItem
            Control = GPSToolbar
            Caption = 'GPS'
            Hint = ''
          end
          object TBXVisibilityToggleItem1: TTBXVisibilityToggleItem
            Control = TBMarksToolbar
            Caption = 'Placemarks'
            Hint = ''
          end
          object TBXVisibilityToggleItem2: TTBXVisibilityToggleItem
            Control = TBXToolBarSearch
            Caption = 'Search'
            Hint = ''
          end
          object NSearchResults: TTBXVisibilityToggleItem
            Control = TBSearchWindow
            Caption = 'Search Results'
            Hint = ''
          end
          object NSensors: TTBXSubmenuItem
            AutoCheck = True
            DropdownCombo = True
            OnClick = NSensorsClick
            Caption = 'Sensors'
            Hint = ''
            object NSignalStrengthBar: TTBXVisibilityToggleItem
              Control = TBXSignalStrengthBar
              Caption = 'Satellite Signal Strength'
              Hint = ''
            end
          end
          object TBXSeparatorItem18: TTBXSeparatorItem
            Caption = ''
            Hint = ''
          end
          object NBlock_toolbars: TTBXItem
            AutoCheck = True
            OnClick = NBlock_toolbarsClick
            Caption = 'Lock Toolbars'
            Hint = ''
          end
        end
        object N31: TTBXSubmenuItem
          Caption = 'Interface'
          Hint = ''
          object Showstatus: TTBXItem
            AutoCheck = True
            ShortCut = 32851
            OnClick = ShowstatusClick
            Caption = 'Status Bar'
            Hint = ''
          end
          object ShowMiniMap: TTBXItem
            AutoCheck = True
            ShortCut = 32845
            OnClick = ShowMiniMapClick
            Caption = 'Minimap'
            Hint = ''
          end
          object ShowLine: TTBXItem
            AutoCheck = True
            ShortCut = 32844
            OnClick = ShowLineClick
            Caption = 'Ruler'
            Hint = ''
          end
        end
        object NFillMap: TTBXSubmenuItem
          ImageIndex = 7
          Images = MenusImageList
          OnClick = NFillMapClick
          Caption = 'Cached Tiles Map'
          Hint = ''
          object TBFillingTypeMap: TTBXSubmenuItem
            Options = [tboDropdownArrow]
            Caption = 'Show for...'
            Hint = ''
          end
          object TBXSeparatorItem11: TTBXSeparatorItem
            Caption = ''
            Hint = ''
          end
          object TBXToolPalette1: TTBXToolPalette
            ColCount = 5
            Images = ScalesImageList
            PaletteOptions = []
            RowCount = 7
            OnCellClick = TBXToolPalette1CellClick
            Caption = ''
            Hint = ''
          end
        end
        object NShowGran: TTBXSubmenuItem
          ImageIndex = 3
          Images = MenusImageList
          OnClick = NShowGranClick
          Caption = 'Tile Boundaries'
          Hint = ''
          object N000: TTBXItem
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = 'No'
            Hint = ''
          end
          object N001: TTBXItem
            Tag = 99
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = 'Current Zoom'
            Hint = ''
          end
          object N002: TTBXItem
            Tag = 2
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = '2'
            Hint = ''
          end
          object N003: TTBXItem
            Tag = 3
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = '3'
            Hint = ''
          end
          object N004: TTBXItem
            Tag = 4
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = '4'
            Hint = ''
          end
          object N005: TTBXItem
            Tag = 5
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = '5'
            Hint = ''
          end
          object N006: TTBXItem
            Tag = 6
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = '6'
            Hint = ''
          end
          object N007: TTBXItem
            Tag = 7
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = '7'
            Hint = ''
          end
        end
        object N40: TTBXSubmenuItem
          Caption = 'GenShtab Maps Boundaries'
          Hint = ''
          object NGShScale0: TTBXItem
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = 'No'
            Hint = ''
          end
          object NGShScale1000000: TTBXItem
            Tag = 1000000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:1 000 000 (10 km)'
            Hint = ''
          end
          object NGShScale500000: TTBXItem
            Tag = 500000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:500 000 (5 km)'
            Hint = ''
          end
          object NGShScale200000: TTBXItem
            Tag = 200000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:200 000 (2 km)'
            Hint = ''
          end
          object NGShScale100000: TTBXItem
            Tag = 100000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:100 000 (1 km)'
            Hint = ''
          end
          object NGShScale50000: TTBXItem
            Tag = 50000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:50 000 (500 m)'
            Hint = ''
          end
          object NGShScale25000: TTBXItem
            Tag = 25000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:25 000 (250 m)'
            Hint = ''
          end
          object NGShScale10000: TTBXItem
            Tag = 10000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:10 000 (100 m)'
            Hint = ''
          end
        end
        object TBXSeparatorItem10: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object NFoolSize: TTBXItem
          AutoCheck = True
          ImageIndex = 4
          Images = MenusImageList
          ShortCut = 122
          OnClick = TBFullSizeClick
          Caption = 'Full Screen'
          Hint = ''
        end
        object NGoToCur: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NGoToCurClick
          Caption = 'Zoom to Cursor'
          Hint = ''
        end
        object Nbackload: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NbackloadClick
          Caption = 'Use Maps from Lower Zooms'
          Hint = ''
        end
        object NbackloadLayer: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NbackloadLayerClick
          Caption = 'Use Layers from Lower Zooms'
          Hint = ''
        end
        object Nanimate: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NanimateClick
          Caption = 'Zoom Animation'
          Hint = ''
        end
        object NAnimateMove: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NAnimateMoveClick
          Caption = 'Inertial Movement'
          Hint = ''
        end
        object N32: TTBXItem
          AutoCheck = True
          OnClick = N32Click
          Caption = 'Gauge'
          Hint = ''
        end
        object Ninvertcolor: TTBXItem
          AutoCheck = True
          ShortCut = 32846
          OnClick = NinvertcolorClick
          Caption = 'Night Mode (Color Inversion)'
          Hint = ''
        end
        object NShowSelection: TTBXItem
          AutoCheck = True
          OnClick = NShowSelectionClick
          Caption = 'Previous Selection'
          Hint = ''
        end
        object tbitmShowDebugInfo: TTBXItem
          Visible = False
          OnClick = tbitmShowDebugInfoClick
          Caption = 'Debug Info'
          Hint = ''
        end
        object tbitmShowMarkCaption: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = tbitmShowMarkCaptionClick
          Caption = 'Placemark Names'
          Hint = ''
        end
      end
      object NSources: TTBXSubmenuItem
        Caption = '&Source'
        Hint = ''
        object NSRCesh: TTBXItem
          Tag = 1
          AutoCheck = True
          GroupIndex = 1
          ImageIndex = 1
          Images = PanelsImageList
          RadioItem = True
          ShortCut = 32835
          OnAdjustFont = AdjustFont
          OnClick = NSRCinetClick
          Caption = 'Cache'
          Hint = ''
        end
        object NSRCinet: TTBXItem
          AutoCheck = True
          GroupIndex = 1
          ImageIndex = 0
          Images = PanelsImageList
          RadioItem = True
          ShortCut = 32841
          OnAdjustFont = AdjustFont
          OnClick = NSRCinetClick
          Caption = 'Internet'
          Hint = ''
        end
        object NSRCic: TTBXItem
          Tag = 2
          AutoCheck = True
          GroupIndex = 1
          ImageIndex = 2
          Images = PanelsImageList
          RadioItem = True
          ShortCut = 32834
          OnAdjustFont = AdjustFont
          OnClick = NSRCinetClick
          Caption = 'Internet && Cache'
          Hint = ''
        end
      end
      object NSMB: TTBXSubmenuItem
        LinkSubitems = TBSMB
        Caption = '&Maps'
        Hint = ''
      end
      object NLayerSel: TTBXSubmenuItem
        LinkSubitems = TBLayerSel
        Caption = 'Layers'
        Hint = ''
      end
      object NMarks: TTBXSubmenuItem
        Images = MenusImageList
        Caption = 'Placemarks'
        Hint = ''
        object TBAdd_Point: TTBXItem
          GroupIndex = 1
          ImageIndex = 15
          Images = PanelsImageList
          Options = [tboShowHint]
          Stretch = True
          OnClick = TBAdd_PointClick
          Caption = 'Add Placemark'
          Hint = 'Add new placemark'
        end
        object TBAdd_Line: TTBXItem
          ImageIndex = 16
          Images = PanelsImageList
          MaskOptions = [tboShowHint]
          OnClick = TBAdd_LineClick
          Caption = 'Add Path'
          Hint = 'Add new path'
        end
        object TBAdd_Poly: TTBXItem
          ImageIndex = 17
          Images = PanelsImageList
          Options = [tboShowHint]
          OnClick = TBAdd_PolyClick
          Caption = 'Add Polygon'
          Hint = 'Add polygon'
        end
        object TBXSeparatorItem12: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object TBItem6: TTBXItem
          ImageIndex = 18
          Images = PanelsImageList
          Options = [tboShowHint]
          OnClick = TBItem6Click
          Caption = 'Placemark Manager'
          Hint = 'Placemark manager'
        end
        object TBHideMarks: TTBXItem
          AutoCheck = True
          ImageIndex = 19
          Images = PanelsImageList
          OnClick = TBHideMarksClick
          Caption = 'Hide All Placemarks'
          Hint = 'Hide all placemarks'
        end
      end
      object tbsbmGPS: TTBXSubmenuItem
        Caption = 'GPS'
        Hint = ''
        object tbitmGPSConnect: TTBXItem
          AutoCheck = True
          ImageIndex = 14
          Images = MenusImageList
          ShortCut = 49223
          OnClick = TBGPSconnClick
          Caption = 'Connect to GPS Receiver'
          Hint = ''
        end
        object tbitmGPSTrackShow: TTBXItem
          AutoCheck = True
          ImageIndex = 6
          Images = MenusImageList
          ShortCut = 49236
          OnClick = TBGPSPathClick
          Caption = 'Show GPS Track'
          Hint = ''
        end
        object tbitmGPSCenterMap: TTBXItem
          AutoCheck = True
          ImageIndex = 5
          Images = MenusImageList
          OnClick = TBGPSToPointClick
          Caption = 'Follow GPS Position'
          Hint = ''
        end
        object tbitmGPSToPointCenter: TTBXItem
          AutoCheck = True
          OnClick = TBGPSToPointCenterClick
          Caption = 'Centered GPS Position'
          Hint = ''
        end
        object tbsprtGPS1: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmSaveCurrentPosition: TTBXItem
          ImageIndex = 15
          Images = MenusImageList
          ShortCut = 49235
          OnClick = tbitmSaveCurrentPositionClick
          Caption = 'Add Placemark'
          Hint = ''
        end
        object tbitmGPSTrackSaveToDb: TTBXItem
          ImageIndex = 25
          Images = MenusImageList
          OnClick = TBItem5Click
          Caption = 'Add Track to Database'
          Hint = ''
        end
        object tbitmGPSTrackClear: TTBXItem
          ImageIndex = 35
          Images = MenusImageList
          OnClick = TBItemDelTrackClick
          Caption = 'Delete Track'
          Hint = ''
        end
        object tbsprtGPS2: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmPositionByGSM: TTBXItem
          OnClick = tbitmPositionByGSMClick
          Caption = 'Get Locaton from GSM (Google Query)'
          Hint = ''
        end
        object TBXSeparatorItem19: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmGPSOptions: TTBXItem
          ImageIndex = 20
          Images = MenusImageList
          OnClick = tbitmGPSOptionsClick
          Caption = 'Options'
          Hint = ''
        end
      end
      object NParams: TTBXSubmenuItem
        OnClick = NParamsClick
        Caption = 'Settings'
        Hint = ''
        object NMapParams: TTBXItem
          ShortCut = 49232
          OnClick = NMapParamsClick
          Caption = 'Map Settings'
          Hint = ''
        end
        object NLayerParams: TTBXSubmenuItem
          Caption = 'Layer Settings'
          Hint = ''
        end
        object TBXSeparatorItem14: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object N8: TTBXItem
          ImageIndex = 20
          Images = MenusImageList
          OnClick = N8Click
          Caption = 'Options'
          Hint = ''
        end
        object TBLang: TTBXSubmenuItem
          Caption = 'Language'
          Hint = ''
        end
      end
      object NHelp: TTBXSubmenuItem
        Caption = '&Help'
        Hint = ''
        object N29: TTBXItem
          ImageIndex = 26
          Images = MenusImageList
          ShortCut = 112
          OnClick = N29Click
          Caption = 'Online Help (http://sasgis.ru/wikisasiya)'
          Hint = ''
        end
        object N16: TTBXItem
          ImageIndex = 27
          Images = MenusImageList
          OnClick = N16Click
          Caption = 'About'
          Hint = ''
        end
        object TBXSeparatorItem15: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object NGoToSite: TTBXItem
          OnClick = NGoToSiteClick
          Caption = 'Web Site (http://www.sasgis.ru)'
          Hint = ''
        end
        object tbtmHelpBugTrack: TTBXItem
          OnClick = tbtmHelpBugTrackClick
          Caption = 'Issue Tracker (http://sasgis.ru/mantis)'
          Hint = ''
        end
        object NGoToForum: TTBXItem
          OnClick = NGoToForumClick
          Caption = 'Community  (http://www.sasgis.ru/forum)'
          Hint = ''
        end
      end
    end
    object TBXToolBarSearch: TTBXToolbar
      Left = 413
      Top = 0
      DockPos = 413
      Options = [tboNoRotation]
      Stretch = True
      TabOrder = 6
      Caption = 'Search'
      object TBXSelectSrchType: TTBXSubmenuItem
        Options = [tboDropdownArrow, tboNoRotation]
        Caption = 'Google'
        Hint = ''
        object TBXSelectYandexSrch: TTBXItem
          GroupIndex = 1
          RadioItem = True
          Caption = 'Yandex'
          Hint = ''
        end
        object TBXSelectGoogleSrch: TTBXItem
          GroupIndex = 1
          RadioItem = True
          Caption = 'Google'
          Hint = ''
        end
        object TBXSelect2GISSrch: TTBXItem
          GroupIndex = 1
          RadioItem = True
          Caption = '2GIS'
          Hint = ''
        end
      end
      object tbiSearch: TTBXComboBoxItem
        EditCaption = 'Search'
        EditWidth = 150
        Options = [tboNoRotation]
        OnAcceptText = TBXSearchEditAcceptText
        AutoComplete = False
        MaxVisibleItems = 20
        MinListWidth = 150
        Caption = ''
        Hint = ''
        EditCaption = 'Search'
      end
    end
  end
  object TBDockBottom: TTBXDock
    Left = 0
    Top = 526
    Width = 842
    Height = 9
    PopupMenu = TBXPopupPanels
    Position = dpBottom
  end
  object TBDockLeft: TTBXDock
    Left = 0
    Top = 59
    Width = 207
    Height = 467
    PopupMenu = TBXPopupPanels
    Position = dpLeft
    object ZoomToolBar: TTBXToolbar
      Left = 0
      Top = 0
      DockPos = 6
      Stretch = True
      TabOrder = 0
      OnDockChanging = ZoomToolBarDockChanging
      Caption = 'Zoom'
      object TBZoomIn: TTBXItem
        ImageIndex = 23
        Images = MenusImageList
        MinHeight = 29
        MinWidth = 29
        OnClick = TBZoomInClick
        Caption = ''
        Hint = 'Zoom In'
      end
      object TBXSeparatorItem1: TTBXSeparatorItem
        Blank = True
        Size = 3
        Caption = ''
        Hint = ''
      end
      object TBControlItem1: TTBControlItem
        Control = ZSlider
        Caption = ''
        Hint = ''
      end
      object TBXSeparatorItem3: TTBXSeparatorItem
        Blank = True
        Size = 3
        Caption = ''
        Hint = ''
      end
      object TBZoom_out: TTBXItem
        ImageIndex = 24
        Images = MenusImageList
        MinHeight = 29
        MinWidth = 29
        OnClick = TBZoom_outClick
        Caption = ''
        Hint = 'Zoom Out'
      end
      object TBXSeparatorItem2: TTBXSeparatorItem
        Blank = True
        Size = 4
        Caption = ''
        Hint = ''
      end
      object TBControlItem2: TTBControlItem
        Control = labZoom
        Caption = ''
        Hint = ''
      end
      object labZoom: TLabel
        Left = 8
        Top = 221
        Width = 14
        Height = 13
        Hint = 'Current Zoom'
        Alignment = taCenter
        Caption = 'z1'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clHotLight
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        Transparent = True
      end
      object ZSlider: TImage32
        Left = 2
        Top = 32
        Width = 25
        Height = 153
        AutoSize = True
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnMouseMove = ZSliderMouseMove
        OnMouseUp = ZSliderMouseUp
      end
    end
    object TBEditPath: TTBXToolbar
      Left = 0
      Top = 257
      DockPos = 257
      TabOrder = 1
      OnClose = TBEditPathClose
      object TBEditPathDel: TTBXItem
        ImageIndex = 36
        Images = MenusImageList
        OnClick = TBEditPathDelClick
        Caption = ''
        Hint = 'Delete Point'
      end
      object TBEditPathLabel: TTBXItem
        ImageIndex = 37
        Images = MenusImageList
        OnClick = TBEditPathLabelClick
        Caption = ''
        Hint = 'Show/Hide Captions'
      end
      object TBEditPathSave: TTBXItem
        ImageIndex = 25
        Images = MenusImageList
        OnClick = TBEditPathSaveClick
        Caption = ''
        Hint = 'Add to Database'
      end
      object TBEditPathOk: TTBXItem
        FontSettings.Bold = tsTrue
        FontSettings.Color = clNavy
        FontSettings.Name = 'Arial'
        ImageIndex = 38
        Images = MenusImageList
        Options = [tboImageAboveCaption, tboNoRotation, tboSameWidth]
        OnClick = TBEditPathOkClick
        Caption = ''
        Hint = 'Manage Selection'
      end
      object TBEditPathMarsh: TTBXSubmenuItem
        ImageIndex = 39
        Images = MenusImageList
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = 'Route Calculation'
      end
    end
    object TBSearchWindow: TTBXDockablePanel
      Left = 33
      Top = 0
      DockedWidth = 170
      DockPos = -6
      DockRow = 2
      TabOrder = 2
      Caption = 'Search Results'
      object PanelSearch: TPanel
        Left = 0
        Top = 0
        Width = 170
        Height = 447
        Align = alClient
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 0
        object TBXDockForSearch: TTBXDock
          Left = 0
          Top = 0
          Width = 170
          Height = 9
        end
        object ScrollBoxSearchWindow: TScrollBox
          Left = 0
          Top = 9
          Width = 170
          Height = 438
          HorzScrollBar.Visible = False
          VertScrollBar.Smooth = True
          VertScrollBar.Tracking = True
          Align = alClient
          Color = clWhite
          ParentColor = False
          TabOrder = 1
        end
      end
    end
  end
  object TBDockRight: TTBXDock
    Left = 678
    Top = 59
    Width = 164
    Height = 467
    PopupMenu = TBXPopupPanels
    Position = dpRight
    object TBXSensorsBar: TTBXToolWindow
      Left = 0
      Top = 0
      ClientAreaHeight = 457
      ClientAreaWidth = 160
      DockPos = -6
      PopupMenu = TBXPopupMenuSensors
      Stretch = True
      TabOrder = 0
      Visible = False
      OnVisibleChanged = TBXSensorsBarVisibleChanged
      Caption = 'Sensors'
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 160
        Height = 457
        Align = alClient
        AutoScroll = False
        AutoSize = True
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
        object TBXDock1: TTBXDock
          Left = 0
          Top = 0
          Width = 160
          Height = 36
          object TBXSignalStrengthBar: TTBXToolWindow
            Left = 0
            Top = 0
            Hint = 'Signal-to-noise ratio for satellites in use'
            Align = alTop
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 6
            DockRow = 10
            Stretch = True
            TabOrder = 0
            ExplicitWidth = 32
            ExplicitHeight = 32
            DesignSize = (
              150
              32)
            Caption = 'Satellite Signal Strength'
            object TBXLabel5: TTBXLabel
              Left = 0
              Top = 0
              Width = 145
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Wrapping = twEndEllipsis
              Caption = 'Satellite Signal Strength:'
            end
          end
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.hlg'
    Filter = 'Selections|*.hlg'
    Left = 161
    Top = 84
  end
  object SaveLink: TSaveDialog
    DefaultExt = 'lnk'
    Filter = 'Shortcut|*.lnk'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 113
    Top = 212
  end
  object TBXPopupMenuSensors: TTBXPopupMenu
    LinkSubitems = NSensors
    Left = 778
    Top = 297
  end
  object OpenSessionDialog: TOpenDialog
    DefaultExt = '*.sls'
    Filter = 
      'All compatible formats (*.kml,*.plt,*.kmz,*.sls,*.hlg)|*.kml;*.p' +
      'lt;*.kmz;*.sls;*.hlg|Google KML files (*.kml)|*.kml|OziExplorer ' +
      'Track Point File Version 2.1 (*.plt)|*.plt|Google KMZ files (*.k' +
      'mz)|*.kmz|Download session (*.sls)|*.sls|Selection (*.hlg)|*.hlg'
    Left = 208
    Top = 208
  end
  object PanelsImageList: TTBXImageList
    Height = 24
    Width = 24
    Left = 48
    Top = 176
    PngDIB = {
      1500000089504E470D0A1A0A0000000D4948445200000018000001F808060000
      0050442DCE00004F5F494441547801EDBD077855451736BAF63EBD9FF4DE1B21
      94D021F42ABD092845295245014110C14F51105051444545444151515110028A
      88A2A0F40E0A01E92D10482FA7EEFBAE9D9C90908400FA3FF7B9F7F73CF3EE29
      7B66AD9935754F3B02DDC5AFC594B56A788B24A22080D5153CCE6E7FBDB71DFA
      1D9570A7B7201C8FF7B381014065EA2B38FE0F8CD2A057AAC44A5DE1D87CD2EA
      57A09D000684FB19695CB79AB46054339ADC2B815AD70D215FB316AF88199F40
      4438126CAF804A1920C0978242392DCC5747B306D7A1A90392A943722069A57C
      F232A8A86962308DEC56975E1A529722FCF44CF4390EC386DB518141CBC96BE6
      C1D380163503E8E9879B5083587F321BF494550457BD1F19BD60D70AE4A77392
      96ECB46874636A5FC71F2F6900985448493906F0102F09C2F4D82033CD1BD194
      EA86EAE9FAB5742A2ACA2783D944268396448582344A8112C32CD42C399E7CBC
      2CF478E7688AF09545C629E17C63863294F2F3D6438EC14B431B13492EFAFBFC
      152A104C64357B93CB094F8240501460D5D1819357C02C83C2FD4D64502B687C
      E7709AB652CE6BA6F1107CCBAA340588BD1A2E0362834C14E6ADA663A72E5226
      59C937C00FCE22491291D32591DDE126BBDD4E91A181649794B4E9C015FAFB4A
      0E05999514EAA5825F59544C8BCD54360591845FED0833ADFCE514592D16F20F
      D0433C6E528802395C6ECA2DB0931141E343992921F6666A971C4A3B0EA6D1D5
      1B05146276D3C54C10218A24223939651904C191A283ADE406DF9B4522B93272
      A8D0A021A316094512984178888EBD214512C42590DBE52017985FC9C8239DC0
      25414E05D3AAC0400EF8F7D502AA8DCCB522CFBC2D4A4ABB9C4DE90A25A9C123
      AFC841BE06223FB346262EB9DD94959D478116155DB9544007FECE225214A74E
      26868700C80A79100FC389C4706FF2F336D2A4EEF1E46735D0D9ABD9B4E9F00D
      2A744824226D4E978B02750E2AB039A94F4A141515E6517E6E0E1D3C96464BB6
      66925DE30D3294E0A9DD65457496F0BB743D9782FCCC3271582932D042FD513C
      7F3E7C990E9DC921975BA2E3E70A292BDF4609C1060A354B949B93455FFD7696
      5CCA100EC2384B25BFD214B01DA9F812FA80C4485F7AAA5B0CB1EE741129954A
      FAE6E783F4C3D13C52A30E14DA1C740D35AF4F7D33350E57D1F58C0C5AB8368D
      B2749184DF57887D6931BD9D812C2683464901DE5AFA5F9F58F2859844C94973
      BE3A4C17725424A07E14DA9D949163A3810DF5D42044A09FF7A4D1E60B3EE450
      6841FF9678D8528E013B20155C519E33E95434B8A9953AD5F6A64FB69CA41F4E
      B849AB51911D49B2DB1D94935B484DC288BA26127DB0D345996E0B079F83D8FF
      8F0D1E5460C02FC0E44BE803024C4AEA91E8A665BF65904D34934A7090DBE924
      B7BD881C8505E48312A5B006529E4B03EF544E34ECC040E163AD3C100B96E19C
      F45C272DDD61A7509D965EEB57839E6816425D02250AB49D46A3E74585A6080F
      718E3987294F08B64A19C09DC084939A8066FB2B85CE4A356A44514242130A09
      A943B6223BD9DD72E2BF825F2E92EC17C68A4A51D18968D4A851D39293935B5B
      F3FFACD93131F9E14E4DE3946E954279F2623E9DBB7A9D9A36A82925FB6A9C57
      F6ACDC4797B605454646363A73E6CC01D0E22611DA2D55690A1A356AF4F2D8B1
      635F1E3F7EFCCB31313AFFA888308DB7DE87EAC705D023BD9AD003AD9B093EDE
      A47AF6D9C9C3E7E0D7A953A75944D4862AF995AD68A5AF376CD8403B77EE94ED
      6AB5850A0AC24981E64276C0431004DABE7D0765659D25FE9D3E7D9A35233FEE
      0ADEDEDE0BCC66F31A93C9B4F6B1C7A65F99397345EECC991FE53EFBECB2DCE9
      D397E63EF3CC92DC279E7821D76AF5DAAC52A9BE063E07E18EC05D2B157C728C
      18B130B7073A5682FA70330328B0A482FE9FBA7709286E0FF2EDB7DFAA070C18
      10FFD0430FB5ECDBB76F9FFEFDFB8F1E3C78F0B38F3CF2C89CAE5DBB76AF57AF
      5EC1952B574E656464B86E0F5B995DF1CB2FBFB406A1C7F01B3766CC98E78D46
      E3ABBEBEBE13FCFCFC1E0A0808686FB55AEBEAF5FA10037E3E3E3E1170EF5BAB
      56ADFEB56BD7962C16CBF1B4B4347B65843D6E8A8913276EAD5BB76E3710AB0D
      C2FE60A052ABD528EA82E0743A051E41B06738C81ADE8960E2E7EFEFDF05185A
      B3664D635858D8F163C78EE5B387DB21BAF043E4141A8D86B45A2DB9D1CF3240
      9C181C80897BC076766746515151218D1B379ED5AC59B3E3A8F56FF5EBD78F8B
      B4DC48B13F8678F5EAD574180409A30626ACD3E908A920D6B927536024278A22
      BC90ECCE6EEC8FFD3323BC13424242ACAD5AB57A028C8E42CC9FF5EAD5AB2102
      C8811490FF20D4C428A442282A2A2298E9DAB56B949393430E8703FE8A1513B4
      D96C72F7C929CDCBCB93DF4300F208831921BF94B1B1B1B5A03F869620E8CF3F
      FFDC2222D065C4D679E2C40979AC93959545C80F99108BA5987CF1939930E11B
      376E1008C829623F0C3090C34022C2C58B17C5FCFCFCD60815A368D3A64D06DA
      9C4791610A143F395076763621C3E5FCF0643213679170AA189C424E31524E37
      6FDEA4BFFFFE9B76EFDE4D172E5CA0DCDC5C0152388FDFB7CA091326FCFEC20B
      2FCC1C3468D02BA1A1A122DA75427124C4849054E2CCBF7EFDBACC8C19701EB0
      88F8FDA54B97885B527667704A1888B984941642BFA9C4C3FDE28B2FBE054291
      28058F07050509E7CE9D23B4A83213CE703046D39C45E9E9E932C1B3673106C2
      008C89B2684A8812C40D72C50AA9F81BA65C66009DECAFBCF2CA749498709480
      EECC04C92354246279B387CD9B37CBC5D643944B12BBB308D90D6165FF903D21
      F612C47703EFDD1E0604B9E77DF6D9672391928DE8A1EA4314029A0342FEC8C3
      F5B231E592C3B165A29C423623C6720A401823F2220911BB020615BA50EAD8B1
      63DCBBEFBE7B263535D5BD7DFB76E9CB2FBF947EFFFD77E9F5D75F975E7AE925
      69EEDCB9D2CB2FBF2CCD9E3D5B9A32658AF4C4134F4823478E94D056B951E4DD
      AD5AB53A839ABD06C43B01E52A1DECC5AA478F1ECD3EF8E0830C940AF7AFBFFE
      2A6DDCB8515AB46891F4D65B6FC94C66CC98214D9E3C5942ED95860D1BC684F3
      1A366CB80F85E36D50180AD400D440954AE8D9B3E7831F7FFC713E33F8F1C71F
      A5D75E7B4D9A356B96347DFA7469D2A449EE112346B8BA75EBF6774242422A2A
      E733A0D416F0032AB4D070AB54291E7CF0C1491F7EF8A183C5C4C4116B269CDB
      A2458BBD68F01621D4102001D001F7A554DDBB777FEEC9279F3C893EE24F34D1
      DF21B65341A90D11F902771D5BF8AD5269F0A62ED00E8805B4C07FEADF934085
      4C42A7AF4586D647A7DF0715671CCC935189660E1D3A74363AFDC128EF1A3472
      8750CB9D77138DCA3AFD57D0548F4131EC8A7E2119ED51245A4F6FF4195A3480
      7EA84C1DD1E9F74389FAAFD3D7C95D20B798DC92B2BCB9E5442B2B773E9E1E0E
      EFFEEBF48B47861E9138309461FCD7E90BFF75FAA8379D0001A8A8FE7FDDE94B
      184D48E8F4F7A1B5FD773B7D0CB21CF3E7CF93304922A13D9A06C1FA0115FA13
      B8955362395B458B0323BCD7349ABACB7AF61CAE8C88684E7DFB4EA63E7D4634
      85D742C005DC5155C78003DB8282626E969D2F2A2C74F0C4EB5D8D2E2A4DA267
      BEA84E9D3A2D1F7B6CFEEAFAF5A37BFBF91995D9D9025DBE9C4E2D5B3608EED4
      E9A1A7140A4774787848ADC87F3A5F141616A2B5D974189EABA953A764C27C92
      68B54AEAFFE68B78AE88C1C3C5F6C8ED8E95E0FF43F3452FCF9DABFCECF32F1A
      6FFC7ED3B0E59F7CDAF8B505AFFB3EF3CC740552754755AD8799CF3D270C1B36
      3C2A29A9D6D309616173FD7373067A0505F7F30B0BEFE11F10D8B07397AE3E0F
      74EE2CA4A4A4D850CBED3B76EC90CA72BC238377DF7DCF0B333143EAD44D7E23
      2434B48F989363711F3E2C9297B75E1B101064329AEA59BDBDBA5B2CD67EDE3E
      3EDDFCFD02921FE8DAD5AB7DFB0E121816356ADCC851694D7E6DC102CDAA2FBF
      ECD82C25E5F3060D1A2EC270B13691A0907CFDC9DDE90112C3C24872BAB0BCE2
      427B2F28D51A8D5F90DDDEA47676D6B884B0888F6AD5A9FB437C8DC4852693B9
      263CDC4A10BE22C5D0B0B0C4E8E8E8F11121A1FDF54EA78F68320976CC99E6E6
      1761D5C3814F5AACD964DE24EFDF7EA22CCCC267A4B4203B3ECA054CDAA86F66
      488ED878BB8DDC1B33AEA5CFBE78E1FC61A587FC3BEF2C0E0C0B0F7B242A2A6A
      34A610A2C573E745C7AF5BA920349C1CCD5A504E4111E515D8C004F3D6D90594
      5DE0A03C1229AF90178A24920C16C9A9D265E69CBBF846417ED67B2F3E3FF326
      D356CC9E3DDB84E9B46E898935DF64063A9DDE571044D1A95451B64A43F93E7E
      944F0215622AC78D4912676111158922650785535E603015091ACA758974AE48
      432794118557C5F0ACEB62BCA146934179BE86A63779B6C50219D7D068B44158
      9FD1A3AD3739DDA4C8C32A87CD64E159282CD0B9C88525C7C00D6B48B17B2765
      46C791134C780056E056D1B15C159DD1450B4260B45E1710545BA3D746393232
      B7E55D483F579A07231F5FA40F8B89E857B34EC83C721504AB4489CC2613E9B4
      1A798AC09E5F48AE03FBA810722FA8994C85583BBB743D8B8ED9BD28CBBF36A9
      2CBE84993E977833F3577BDAA9A945A7CE1CF869E5404966F0F8F43501497543
      C7F807A8C78A4A2950A737080AC14D45B9D9949F9349A2A8280116BA0425E46E
      A3E358FE3A5964A45C959524BF787291BA48BC7E63159D3AF9C2CF0BDB9E67F9
      33944FCCDC9052B77EE0C4C0607D4F35A2AB506984C2823C321A0C24386CA452
      FA52469E8DF2240D5D3CF337A9B406BA74238F2E1608A4C43A67D6F9B3923D3D
      3F4B91E77AD350242DDAFE6E876C26EC81A24EA3017944AA3F25417B56AD3329
      F5668B49673068B332AE1256FA045163205EA729C40AD34D1BD135B71E99EF45
      37AE5FA58CAB57C9AEB0BAF23372BEB1A5677E78F4D89E9B82B58953BAFE8787
      3E293F7CA377266CFB18C326AE793F34CA2FD1E263EAE01BA079C4E4A54B708B
      4A2123B7806E1411E50A6ABA74F52FCA72A04EF89A292FFF028AEE5645A1B54B
      BF42934F0B4D60B3E38AFCA23D42ED4F763B33B38F145EB878A9B41E80012D5F
      D4273761E4C6634171E9F126F39FE295ABDB497B2590A27C5B92C6124837DC5A
      CAF6F2A22209C5F4DA1EAC5FAEA0E090DE429EDDAAC7D463B4A85247A94D86CE
      6281F18A9475F96545F6BE154C5786A1E3E742CDF19B121FFCE8E8B22E3FBD9C
      13F865925BF5B645AABBB4A7343D759434E28B1E5287C563A5BA0BDF9442668C
      91348FF949E2134629E8E57E9265C64649F9D406493FF907B769DC9A8BFAB6CF
      4CD3F8C605E0EB144502E47D077C6B0C8DF77B30262960AA9F9F3951A0874427
      4AC7199F9FA8B1F748526B6C64545EA238F31ECAFD6B3165DFB8464E8D931466
      350A83950A329D24D9DC6E67CE8DBF9CC77F7B4538B17E9D2D375DCE6C85EFC0
      B5F5C29303FF1752C3779A46A30A91DC922862A6CD4F1D44F13E1A94222765E6
      5A292FB306F9A8A32940778648CCA14C17EA858548AFAA4DB957229D8E2B97B7
      0B47BE9F299E5CF7832DFF260A0E620E2578F75812EB13A66B6D090AEC862ADD
      D06436066AD54AA5569523E4EB3EA12BB6CB94793D93E2345D492F35A0D6A17F
      D1CDEC4FE927D4EC4BF6E652EEB9B645578F5C5E271CFFFE1545FAAEA3B6222C
      3A83B047092506511FD7C3648DAB17EB1511D7CA1214D4C9EC654ED61BC8D7A2
      1315D98A54614FE62A8A558EA3D63E35A875D41AFAF182525AFD734AF6D97D7F
      2EA7E3A96F2BB28F9FC584A0BB845ED59AC6E8A73025F5F70EEBFE624ACDD1CB
      67347F3EF597EE1F7C74BDD917635C4D177D2AD59BF5A3347FFDA7EE21EFCEBA
      606E39E969B577AC9C995553BCC31B8DDEA234D51AE817DDEBE53675C77D343B
      68F4AA3F8CC357DFF01DF2F161AFA64307698C3EA63B04BFB7576A73884A9538
      384895F25C0765D290261A9DF9AEC6A6F7C6A5D837E71BA3D856CD53589FBAF6
      1122E11DB4EDE66AFCDED36B4C1EE6607BC913682A8477BA77EBF9AF122F8989
      3975C3BA77949E98B79D7FAB052CF150AA79A99CB46C4C43F232EA65B72C743E
      0FBFB3976EDAAA1EF5EC7E3E855748CC48811C86320B8AF5DB9F7AB540EF8CAA
      5B4A9CDF5B0D3AFAECF1FAD4F3ED23945F74E7A22F72803BE1B391F114EC8336
      E1364F3E26037D39AAC66DAE15ADD532F861575AC550252E3FEC3E4126ED9D0B
      54950C0492A88697832E6716D18F7B4E9490BCA56D3B721A63251B25FB149157
      71D6DC7A59C65429035190A8A68F83E2BCDD14EAADA37C9B834E5DCE280D762D
      2B0FD36A124467A6181F25D5F5B6919F5E2A7D5FD6508E0167E83763E369581D
      8962AD848D4C3A0AF4365290B78944012338EC6BE1C07A6CE008F1B55008F6C0
      04A3EB8CF6C1268F640DFD30A9560591953260E26BC6D7A298605F9AF0600B6A
      9C102013F707F1002F23057A810936323103A34E23C73ED8C74C2160D0302184
      86756E08FF664A9D985C8E492983EF9EAC4D0120C20418DD9A26526244804C28
      1084F45A353B9782EDC148458D707F4AA91555EA6E4111DE30A95EA9BDB41EF8
      598CA58E1E435CC9562B8FFD765DA75611E37677B35E5BEA549A8252977FD950
      9A029BC3491A55A95566935768231D32548181AEEC70DB83F776E515D988C552
      F65581CD4E7A4DB1484B29767D7D177DFF741342772CFBDD977651DE1C1604F9
      33948AF28975E00BE7CA8D1C62F04A60E3C470391C137FE0F57DB46D4633D92E
      7CBB66B5D4A7F78394FCFC1FE483B2FCDD8486B47EC79F94955B4441E8B0185C
      5A8290A11E2676A496095FB9990B0640460E19F51AEAD6B406F55E7C98728B24
      3AF8520AAD59FB0D956320B3C423D0E0A2DA3E4E8AC4B0C5C32021DC8FBC4CC5
      55363BAF90FE3C974E573272E9325271E6869D8EDE50517AC1AD547A18948A08
      744BD5D57C05D69651B1241B74EC274265F310674F16A34EAE781733B2E9F40D
      071DB9A9A61B186DF3BBDB718BE56D6F3836076FA8E54C2E5BCE3DDE9AD48C40
      E66AE9D00D4D95C4D96F950CF825C7AA5FEBDA6CAC14DD9B255226CF7B55FAB6
      D8B1521115BF2A7EF67D673FFD38B519B6FF948F4B41915DCED0625F553FAB65
      C0A9E8FADA1FF4FD33CD65B933292EA2FDDEDA4536A702C59A5DAA063E2A95E8
      FDC9CCB95EB5B7F26F544A056D9CD6B2BC63253610CF1596AFF868BC9797D77C
      2C42576C8C2A0974B74E587AC94F4FBFF27CB5FEFD174A2D80A46A3D56E1A17C
      CE55EE89C74C8BC0A40F70CFCBBA77C3E024F8C601CB89E87D30A90F543D2082
      A7B2EA6E185C468093A0C8291902F3B7407BE0AE54B50CAE3D25E483D21E1706
      9A0A7C3CC11C0C541C28C1112913A09553D53228F1BD13BA0D6065C723032010
      140199A8EF1B523CDC7AC31E0DBD5489A5A63206788A05CA66E89F787D1D60C5
      8D431EDED78065303016E6708C075AC23C0E180D7B20745955CA006F528039F0
      18009D305AB90C3D0D6065C7A339F018300558002C0486021D8087012E712AE8
      98B2E267451C83D320601998D444EC0A605EEF22E25A0F230DC4E37105511D80
      3B896EB0E33565420F0346030940950CFEC6CB934017602A529008DD097C095C
      02224B08CBF2875903B744E014C06EDC04B783B9720620988D972F015B892801
      29E802BD37A006F6002AE076E55FE27001BA1D30039533C8982C48289E3FC1C3
      43C01B802FD002E80170E66AA197530A7C87C1C1080C07FA022B00CCF6F1B31A
      201FDAC3CBD700CBFB02748E61A2A29828ACC5CA457411A6B688DC29E8B252CA
      CFEA1F8721B6B510557D784D0076022741B016F4303052C15C00F316E03A50AA
      8452532506C49C651D8557F50023F01C10019C04C32FA1B703534E8D08F347C0
      5AC43E077AA9E217B2055B6F5E003876B21DC493607812A8016C25A24F802540
      11E007C246C01FE6668009B000568413A197AAB296594474BC0C131BEC17811F
      10AB74C00133C77A1DF43348C15922E2FAA281883885AFC3BE11980126DED065
      A5949FE51FCCA486F0947002CEA780B28A33391C0E2B810F81AD4414EC226A04
      262CCE2498A7C16D2B116D072A2FA678C14C4AC505BB47E5C0E00F4C86786291
      AAC3303F0E1C0461093A2B251E3A40566545243B9479546002B1E4E37D261008
      3402084CF6411F071C2D61C234B5B0CB8A2DB2A18A473926887536FC7D0CE403
      2D216B8E2D33D905FB38E02F8069EAA0CB8A2DB2E10E8F873DEF105B449296C1
      3E0F4800B8864393D51F78B2B84E422F6520C072CF0A3167024F4064BFA159D9
      E5210077A6D786883488CC0FD0EF5F457CF0957FC4FBAB6ADF4E01EE0A802BE5
      EDAF2ADA11A37F3426628AD5E581199E1681511F4007F33DABEA187086C581EA
      72BA8F3111C25459D1F81DE3321EF73D2642D83B334049E0F2BEC7759F63A26A
      19B007602760035871D39CC106E449A56322349672E5633F0C2EB7AC97020163
      61B984D81742E7C115E7C116346661480913EF0AF75CA011600436D4755E1EEA
      10C43129CE738649B66D563F1CED83FB2F406A65999C8217773526C266EDD71B
      3BCFFFAE23C74B7F8901219B5409D68DAA4472164BBE2DE8BC5E198363783108
      5886D4543D2692A4BA9843D21D520487E2A397D4581BBB2058A467B45D0FA518
      C73744F8FAC078018F720A44AD70580734073E4173F01A1AB9B630D705928108
      888B9B6C188B955B92AEE12CE15942BF00B881C910F15BD08BD3C2060F40905B
      CC9760DF4A958C897492832B1F5EDD5220EE5762BB00DD0E94FA116129A7D078
      5539268A735DEF01CF5AA09C52140F5F38C387E3455F6005202B417EDEE10191
      B5C7EBAF9592CB12E6CE16216B3A21FA92027378702F5528611761293726821D
      87FFF879671C6EEE389D9E25EABDFE66C2E4DA8D25AD2B20580BC1C21458E583
      B900E62DC075A09C5296B395B120E62AB5E48C9E51B869A945B2D558A86D4545
      047282D28A7CDA04AFBEC8FCF3AEE2AF9E8F60AF3026825BC54C6647104FD24A
      8EC9AF17AEFFA69FE348CB018E43A4905CAF23E65CF9EE7A4CC4B42ACD834F5E
      5E37D844B68FBB39FE52A9C87D111E1B06585ED0439F07C421052B10FBB630F7
      429AB0128BBA557C0E7315DCDE4511BD095D564AB41D8FC394069C01EA0323C9
      B1BF137456A9783C24CC9A55E0BFF0852498C38195C05D8D89E04FCEE4C56CA8
      047D40786D19F71C98FD011E136D412C0F43941CB9A5C88764A444C03B2550AE
      63E27A30018E4B813DC0C7401B22D2DE469CBFD3F2E19E0904028D001EAEEC83
      3E0EA8744C04F7BBFB3E608F882D224963609E0B70CA4622154E98B9C56D0EFD
      03805BDE2170FF0A6659710A6443750F048224EE6D4CC434EF9A017B06131BF4
      B7806F519222A0CB0AEE120CBF014F00E9C03F5318F7543A2662AA7877F7E322
      0EF04FC11957298D1D3B76F83DF7DC73FD67CE9C390707195E1D3264C8CB0F3F
      FCF08B8F3EFAE8ACC7C78F9FF9F4B46953264C9C38A4D743031253BA74C90A7A
      A0E3B5FDEBF85BBD3C392EB7E55C3EF9E413655252521B3F3FBF67AF5CB992B2
      69D3260D8E6AD0A54B9784226C3F41DB4F2A9D4EE517126C69D0A891B94D4AF3
      C4F890E0BE2A85E2239F7717BF33FFF1F1E5F2402C4B7DC3860DEA8888881198
      EC5EFED9679FB57DFCF1C7B5CB972F17B0BF57888A8AA2FAF5EB5383860D2936
      21811C98915FBB7DBB307DC9FBE2EAEDDB8330A93E35CC627D73FCE2C5116569
      963258B56A159A78E1119C4899FBDE7BEF052F59B28470628BB0719262C68E25
      4BBF7EA4EEDA950C60E4171F4F8960D6AC5953320707D19AA34784AF0E1E5017
      B89C7D8395CA39C33E58C2355EE653CA00C49A1714143CFFCD37DF786FDEBC59
      888B8B13121313B1E5A4D44B71004DF1F16D9B5A4D4E2C9947C5C65234FCEDB1
      DB84DDD9D92AA546DD375610878E98335B16BF1C1A673CAC38B033F5D8B163A1
      202E1FA40A080E26A7524922B6A0C8944B1E2E10968C46D298CDA4C4E50036BD
      9E7C43C3C81812423B2549B8298A3AA3463322C86CE15ACD3D08D1C08103BB60
      3D7ED2BA75EB34383825040606921AC7E80D4C44AB25C5F9F364C0AE285D5E2E
      A9C0D4AD5153914A29FB711B4D94A9C0E61A9C63BBA150E28034D6E0747A2B62
      7EE942B3263B049CB731E238C0BB388D3518B247840DC427B442E2E3717D8199
      441034412C0E942026ECC296206C7A94D3E3B098697F9B76B2991F79FBF6912A
      3F9FC6EAB492DEEEF879EDB9B383451C284CC21470031CB092CFFFE1A812F671
      D949D3B22539EBD6257B5212F614D9C9851377384A4CD8714F8EDB3A7C26CED0
      21B579480D8F7B8C7A5D7430510CEE565034C2347000CA3CC2228908CC3BF639
      80070E974B7E1716124C46884D82283CEFCAEA9CAB2E842F4064F41AADD9A250
      8681B6188614E80A0B0B4B4B0C1CCB862337DE2911D04BA5265F8D8E82008397
      2F6E7FB052FD3D07A8CE81A314B9F710851638290AFBBDF4D817A555AB15A2C3
      A117BEF8E28B05384D37EEEDB7DFD61D3A7448400D969755902F583350901999
      8DCE9E34282D5A949E22AC467193EA8428246474B6DD4E0E1228D78EE5186CCB
      CAC191BF51EDDA49F5A3636EACF9F1C7C73993A7E26CE5B39F7EFAA9F5FB4D9B
      84B0FEFD4B636F387102F75178910A496631892A15B944919C488D1ECCAE63D7
      9A03EF327120DD21B9B1D7A8801468C75F1A3B4EF2359BFFFE70D51703853973
      E6746ADBB6EDE23D7BF6C4BCB36C9910D0B9732903EFD3A78950EE8DEC02A27C
      44C00D5DC090A2108B719CF1F945859491954D4A30BF72FD3AD543A198337DBA
      FBCC99D39B5EFFE0FD47049C7D0D183162C4FB284D3D67CD9F2FDA6BD5627232
      8C4801729770C04F3E4EA94091C53A835CCA24F8409B45372112971BB147F12C
      82B8268D1F2FF5EED1C3F1CD9A35335E7A75FE9B02088BF3E6CD1B8654BCBD2E
      3555F7C9D75F0B0624DF8822978FB3975AC4CC807AC019EF8699C593EDE707F2
      C54A9196466E9CE7C45149B9DDC20146F7F56BD78E7FF0E1D2FE5F7EB1EA4F1E
      6A10DAF950E05364702B9CA613B66DDB267065E343251C63160D175D052A99B3
      A0802488A1983C51D1D1A39485D38DB1313134E9A94952745474E1EAD5AB5F5C
      B972E5C2B367CF3A640638D023A2B9E8D9A54B97C56893823EFFFC7342870309
      08F299403484323D09CD018B4344732D3BE071E38F3F28213C9CD02149681C5D
      88DCDAAFBEFA6A020EB15FC1EB5B2A212141F72CF639AF5FBF3E036D927BDAB4
      69EE264D9A4838DF27E1ECAB141919294545474B61515152705898140ABD01DE
      A3B79340D0FDF3CF3FDB172C58B0A94D9B36B5207639E24CBDB4CBC42944270E
      731E856336F63ED68C8F8FB7A067A3D0D050818FA972138280E4EFEB4B359082
      162929D4A94307DE0BCF3B40F290E235D8C6FE1C62FE27A4C06500A4083544D6
      6E3D70DE49DBBC79F3563854351AC45BE10C94B7DBED16F9EC2574ECE31451B0
      94D808264AE89CF8B4F5B1C3870FAFDCB56BD76A9C4AE5EEB294F82DAA154D22
      88FBE35061371CD77B15274F37BDFAEAAB8770F0F0380E881E85FDB70913262C
      C599C161106D9C4A8536A4228DBB726159A2401922D04734407E340F09096982
      0DAFDC995841A154C430FFDFA870924E423ED443DA55C07D29B1BA502855FB3B
      77EEFC30FCDD17936A194C9D3A95D0847C72BF4CAA6580B37FF44F9854CB00A2
      A17FC2E4AE18FC132677CDE07E99DC1383FB6172CF0CEE95C97D31B89D09EC01
      40B97A82333CDC8E553E1902CF55AAB56BD7123EA364A07595FDA1C65F80A11C
      1334E172B3CDA33DBCBBB33A72E4083131F4D7148EEE917D631CD50F7A3EE000
      EC800150036C8756ACAA159187380EF81FC52755F1675483068458AF06896BC0
      89127067C38C60BDA5AA65C031C787C95C1C2B7E9C839D3C7992EFD661233721
      DE30E401D7802CA05CEC61AF3E0F4E9D3A356DFFFEFD3FC0F319A462260E4410
      F7CD384A4F689F36C3DD0B2897C1B0DFB5B2C26702100A3011154423E1330B43
      25896F179070B3C034BC637FD02AAAEA44C4997811C158BE0EE88ECCCCCCAEF8
      9686110B0AEFBF4FF8EC7D05962A53511D0326CA4C58071D220C4D7E62032E08
      9045C566C01FE05204ADBC922B4379A7EA6DB8E9240EA389348F4F14D90E301F
      0138B3A1FD73C5F9110A324D80F6407DA0CA7CC0BBFB52CC8489B2785867FB7D
      11FA7F37D0945EB1CAA5E3EBC556158BEA4A5155E14ADDDB77A8D7ADCB909EBF
      7F33AB5DEB52C73286FB66D0B97E80F8DDC2077B356C5CF3136FADE0DFAA4B8B
      B53FBFD377E89271C9E5F2E2BE1880B862D2636D46B6EFD4686D76FA35F39E3F
      0E9358906BAD5D3F717942ADF8296599DC338312E28FB568536F89682BC4F782
      199FBA462AC4F75C51FA658A8A8D9A17151733C823A57B668098971277E2AB92
      E1E76DA04C6CAABC793387AE9F3E4181E1316F7E3CA9695366724F0C164D6C55
      BB79EBFA4B045B01B9709CDE891918971D87E08AF8489382B273ED9479339BCE
      FC75C4F6F18F27CBE50533AB12108B9CA1370EBC92B167C5602967C774E9E6AF
      93A5F44DE3A4F3DF0E958E7EFCA0F4CBAB1DA41F67B792BE7EAEC5D59ECD4286
      809816A8F809C58E6501E29CA1256229A0C3074EC8B358E1215E64C7E1B78C8C
      2C3A71F2326E0BD5D1A51CFBF945ABFF7C7AFFE9ECF5A05104DCB9C30171F196
      CC0B88E51D13194041B8EE32372B179715E6535E4E0159F46ABA985D4A7C2308
      CBC4A1DF59A52EEAD72BEFD87CA960FFF352CECE12B1FC3056BAB06698B46B71
      7769FBC207A4EF9E4B913E9FD1E25CFD684B7F50ABB4C9867B45B568424AED1B
      FB676794257EEDC7C765E27FAEE82FED59DC4D4A7DA105CB3CFD4EC42BF40710
      4BA9CCAF9FBF40D8BA8CFB4DB5E4B415C9323F7F3E1D729570CF54363994EAF3
      2F2D3F30F2F0B99C5F1045275041952BA665898B288ADE1603A995A25C893843
      7373F249C4FCD0A5CB3765E2733F3DF834886F03D54A89C35D5EC3619D40BC42
      8672A62A71DD652ECE7CDC4045C28940BA763D1B0B30BAF34C1CA5E59F65E835
      9473CE50963967E806C87CF50BADEF2943955D1A042AC70E69DEAD45EBE44FFE
      3EC2536806EC545642E636B21760360BE5BC10C75673B2F2A9C045E9B35742E6
      6773582C775714211FE3AF9F8CF8311F45316DCD2869DFF287A5F492A2780C35
      74F3DCB6D2F7B35A486562AE4498BB56B2E7D68F7EF4BFDD9F8F088A0932D5D2
      2B253A76EC2C05F99A28037217305D9669A7F30B561DE41ACA32AF3243EFC4D5
      9092E4DFFDAFD523AEB3CC0F2EED237DFD6C53E9E7B96DEEAF1255C1C9BB6DDD
      8081FB3E1E707DCF3BDDD070B5963E9DDAE48E95A80A3A7774F66E5AC3A7FB9A
      17DB1E59F342AB72ADE21D43DDE34B6E4F9A24869ABA229CDCE442FFD7950E14
      19D0FE99126809EABE87C618A142DBE479754F7A199AE51978A87818793CDEAB
      DD43077AB9C68E9810032F64057380455006AC2203A087B918AB48CFF6CAFCCB
      6E72E0E28758EA0062C54E953E8D70AD0BB4045A00710033AE5CA41E5AD0CBA7
      00A12A516EB8A9809A000FD71B418F0228FD611CF962C31D50CC009C3C7EF035
      A9C45A5AE867AB57A62C5FF5D143F3E9A3A7A7AAE63DDB48D9B25BB0145ECBE0
      34059343E0A2CC29D04054AC0B9EF0A57A094D113753D6FAEEBBEF066FDEBCF9
      19AC59BE8379EC559191918B83A2029EB5C4988668C395AD945E42789E325BA5
      D5E8F491BAD888A6D4A65BB3CB9DFA68B30D354030186086D02A2A11B75C4DEA
      75B5E74A5CA4D0029F45F8A68BD0E03C02CEE93BE874D109DA95FB1B1D2ADA23
      87B428ACE8E1D4E4B6384D964863EFC6EA5633EA5D6A3920F454BC9FECA1ECA3
      A404F2CD942C63F972421C58C64136B70C03DFD42DF8905D4253EA38437AD148
      7A05BA501CFC2F70E5D315BA80C37061FA8E61DD5A0F308D988749F2D1B89330
      A82C0F36F3CD9459B661F6547CF8CA8431572D5FBD18640CA144636D8AD6C693
      0A67C335229F5556930ADB67D460A215741429C453A822127764C718F0BDDC1D
      37532EC6CD944FE35EC258944E9119F0CD94AD30F71C80F9E30A37530A4E4C84
      8B4ACA766552BE3B0F370EE02435CEEF1B95260AA7184A90EA90D2A5C22295C0
      4B6402A69C15B89932027A47CCD87BE366CA437C33E5CDC4DF6AF4A8EA664A7F
      65103531B4A220652852A222B51BA7E9DCE1D4D2AB03F9187D65E258C960063C
      2BCF5735CA37537EDDE1ABC54841A088C5D14D30F0C4069DC7A2287BC6AD92F2
      F597BCC4A2260D05E21E6B3F6520052BC2C95F0C266FC98FE8266E7AC8C18501
      58EFC1358B84D8F247BA7C3B25EE7FF4145B49C434FD5F2F5C9ED517EB036E4C
      E9F3D2BA1C2B2C5610673A162B4847068A5626E0E67A1D05A88229D152070332
      817861E8871F7EA0BD7BF79686E30822C252F27BF55E859ECB7DB284F580F558
      E1F04729E81654C9CD9491A19164CA32D2E50B17A8E88A9D0E9E3E82019F4BBE
      489257A74A88CA973680A8AC70CFE355180A9901A1C976BC32CDAAC34A931F4A
      406366C2E2421D29BD99F2A7CD5B70F1BB86944E6C7A13712D3506032040584E
      9119212C56812D84291FF966CABFA79D7A032569713103F8C4CD9445D828F036
      52320B3753C66001F51FDD4C0992AC5CFC28878E1D3B06616EEE43CCAAAC43D3
      B11ECBF1EB7133E57ADC4CB91E778AACC7CD94EB7133E5FAD9B367AF9F3265CA
      7ADC4CB97EE4C891EB318FB4AE6FDFBEEB501F3EC44AC90C10AD0754AE704955
      0DAC947FB67BF7EE75B838723D6EA65C8F9B29D7E3664A99096EA65C8F3B1ED7
      A3F6AE1F366CD8BABE7DFB7E8D45A58528106340B11D100A944A87F340824339
      859B29537033E5D7CC003753AEC7DCDDFA59B366ADC7CD94EB319DB60EEB9FDF
      61F16229DAAFE751511F44E0DA80191081DB6896344CF28B5B0F11AB4C3D31F1
      B496C5C4C4116B26FC15F65A2CC49AC228786D434421004F67422BA34A69B2C1
      8332EF4B8C0ADC4CF91096BA96E0F2C27731CDFF1C62DB17EF6A01B7620B4B39
      E5A1075D4011AD201E14AFE29A080F72C03142147433FC1E824E95BC1764778F
      7FD952FC2893117028E985602A56F76B2FC3A8987331B9BB7A36FBF478B2AF5A
      7CC45FAB6867528B2641124E3A25FAD229096BDFEB1E91753B11C5ED0E55D96B
      2E3B664EE8FFE4E806BE9A175A85E83B36F0D70727786BADE11675B88F56D142
      21509DA6839EBAFCDBA70B2F94A5A1286BA9CA1CF2EE7EFF70BDF8E6C038CBE4
      2ED196E0282FADE06350B9BCF52A7780512D869835C66093AA36CAE7030D1F9A
      98D1EAD12947B77EF2869CB7E5F3A0120E35DEFC5D6BD5BA678FACE93DA47EA0
      D981BA9F5FE470BB3468E5DC908F0D61344A51E16754B94C1A458856212C3C95
      519403E7EF00AA360FFC5EFDB9F7C486619FF5A911A8B43BDD8536A7E4506027
      925214C47CDC2B844D2392A020D1AC55AABC744A7576A14B7BF052DE9EEB7985
      7DA7B48BBA78C714844C5FAB4EF0D53FD23CD4AAC3BEDE1CC4DEA1540A6EB542
      50385C921B9FD0D80382AE01E384EC42A75D8383CC468DC21168D624676416F4
      400ADEBB631EE436EB1FDC29DE7F7A9B085F23F6631421D66E6CA9202596AD09
      7BA2D133E106177CF8430E929BB0882D085A9520E2F620CDCD027BC1B0C933BE
      47BE804F554A23FA786B5556A54270A8705B8F4A4102622F60F789A416454125
      90007149BCEF057E0415A871CA8AFD8AE1FBB71D0DB8630AA879FF8016E1BE8F
      3608B628F05F200E4800C44115311645423AD097A1AC88C813AD5254E2FE235C
      21009149A4BE7CB33023FF66CE863B33A8D753191764792825DCC70BF2B58309
      6F260655ECB6878C38E148880096A20332B439DD2EAD52A1C877B83567AFE5FD
      E5CECE597D4706ADFB8E2CBAE472374E09F54EF6376A9007E4E6982301506082
      D83B3162E3BF5071BA253712226A540AEDF9EC22F1E2E5CC1F562D98FC1DA4C6
      F1A81CBF4EEFEA38713D6FE58F7FF30C88A443260B8004F0495FA444102029E8
      7206F39FBEA872ED2ECD996B7957DDF9F93F1D3DB83DFF8EC594D916E6649F5D
      BEFBF88D70AB36BA5FCD60DE9C5384E28AB843C99F07F8031208C9A01235854E
      49BFEF628E2DF35AE6777AA7FD370E7F4711D19845D1EEDC8CC5B9D937EBEFCD
      C0DD572A9D3ACCA2536B94284E28A3883AEAB3C06347FD953C877EFBD9ACA2F4
      4B3752F54585AF4E1FDEF202339065C9860A78ECB568CABDB104D32E1D486F21
      C1E42D2545C55C681019E4AC11600E09C1FF786850460B900957736C4537B20A
      CEAB72F3D659DC8E8FA60F6B7EC243AF7206FDA7475361EE12B2E575208305ED
      80AF542B3AFE64CF8890D78A5CC2A1CB92D8141BC91255225A088C204D6ED729
      ABE4DC85EB450E3C33AC39AFAB79E857A23FFC5C14751CB699527A4BD461A824
      F67BDA5D7BE6F213333EDEFED8AB2BFE28FD384FFD6C9B76F927BF9996ACF84D
      BF60F9EF8A4A28C94EE553D06B02C4727309655FEB403ADC4F61F1936A25D53B
      D5AD46CD57BC158ACFA60E4DB9EB4928993A1EB74AD1E0E7A3E8DA7910BFDE81
      D43A12F566A9568DDA27BBD7A8F9AA55A1FC7CEAD066F74C1CF44B54BF6971D4
      7DFC4F94D247A216FD24F181E1EE3A53DE499BF1D16F239AB7EFAB2DF1755F9A
      488FCE49C297C3DB64CF6F8FCD8D24587CA956AD86A7BAD5ACF38A55A9FAE2F7
      2DDFFE8398F3D67EB36F777C9C35E3A2A8B0FA4B7593EA1FEF5EB3F6BC3FBE7E
      EFF3698F362BBCAF689709A4F0EA38C48946AC8E4A67F4AF1315F757979858C4
      5CB9EAC3054FFDA3987B78C8A568C8BB5BBAA34C0F0DD7A8B71A44E1E3694353
      0A3C1EFEA95E7E6477FB40EB7EA9971B7895B194D2F330F2BCBB577B2921CEE4
      321679CCE921C6EE3007FC375FC4929045231B88FE9B2F2A1144E9379A12DB3D
      DDEC88EF631A31624477365784809BFCE3CB39E7CB360319C930C04801AF45F5
      AB23BBF003DB41E50ACC6605BE265B2E7F3032137315845D9AF1D8DC9DDAA54B
      97B4366DDAA4D56E929426D5294A3B5E636F9AA29E234DD5D09DE6AE53905650
      EB461AD52E4A6B53AF7D5A939A4DD36A27D54EC3BEE1139819F813F31789D837
      FFD9890503273103115BD26DF8AB0D5C80282744DEA9BC62C58AF8A3478F1AAF
      9FBB61BCB12D373E565383C41DC678FB1597D19D4146D35EFFF8184522054821
      7460CFC1F87DFBF6C56FDEBC3905F3712A26DABA75EB05D8EA3B0E6675B9F922
      38C87FC7D1A14387CBD89CD106DB19DA34ACD3E832CF17F9D6B05E569CD2052B
      52AD6DFC637D2F7BE68BB035251ED7F2A6E11F507C30FDC024781646AC74BE88
      DF622842984362A30C25FECB88E78BAC0A1FA213EA789FEE86AD413EC1799EF9
      22F6C4F345AC63FA8135FEBF104F1E48FC4F267FBDF0C20DCC170D1A46F8E128
      061D38702078F8F0E15B79420ADBA5839B06344D73A5E174614FDFADBE01BE79
      89BA3AF4F95B5F748728F8FF276E60976D77C4FE06470E2458C9F345077115A4
      089B3C5F84C9D88D3013A67208F30E69D8F699878FEE3CCCBCA4F17CD123FD1E
      490BF20BCE735D213AF8D311C214422A32948283837700A9980ADD810DAE4C42
      8667BEA8382968352D982FC24CCB3FEEC1983A76D6AEFBF6816F7AA285D0700A
      D88DFFC9A408FF2F64C53CC49B3C6D336EDCB854CCA4A4621A21151320A9D8A6
      9B8A3D2DA9F8D3A354EC4E4E8D8A8A4AC50C632A8A642AB665710A52B1EF2815
      63C954944C1EB8325D0C63592B03A4E2BFF9A262B1235F6F09A69CA5D4F9BFF9
      225914B706BF6C4527CF5A29EED75E46E4C515AD94621943194FA83095FBBB0B
      3FE55350867E392313E2D4B0CE2FCA9AD97E071417A9B21E96480965AD6CC6D8
      C818C06B6716E1D63A9A455E28E2D7B75049D8F20C8A3D1C9743702C0110E754
      1AE156711DCD82CF64F82923C2E398382C17C15B0C6E11AF016265157775DC53
      D5846313A011100554B68EC6612B3061BFA5B87D5CF4F1AA8FA62EF866DEE2EE
      AB5B7E5BEF8BF02FA3579ADE09F854180C9179031AA0F275B4128A2292247980
      C9F07F671D8D0B43096E89081CD189A0DFFE97D6D1408F553906BCA482CE025F
      546EFAD7D6D1D607A7BEC69CB21FCE4965E2FFCA3ADAC6DE714C934B973C2EFA
      ABD5F1F54C9C8F8CF152094EB4C84B2B0A9B8AA2D5F1248F8BB02FCAEEB063A7
      8B8BAC6A2FF28C8B14F87B5DEEEC114E807C55E8C35B6130B0A0FF4F03C68349
      F97111E696E47111164CF1655B7CF8ADEC3ADA9ACB5BBAAFB9F473F7E0FF53EB
      688891ACFE8FADA34D0F9B983A27F27FA9F7BE8E163CEB5BCD37F3C6FE9BEB68
      07C71D98864C7EBEB8A221E1337C9E7D7FCD9A35BB71A9ADC4832F2C17CAEB93
      9CE13C94FC69D316A2C35847BB848912519EA9432892D7D110461E3463742DE7
      DD67AD56F2AC2F7F23147143267BE4C7BFB58EC6B43C28C7001BE2B371C2F755
      1C0F9B8DC1943F966D051EABF21A2717630CA8E4187285841FB9C4F17091DDA1
      4BBC0A883FA4BAE6215EA5FEEFAFA3312B6EAE8B1B28B93DC7974FCA3DAFA34D
      DDD6A0A4E1946930D9F2F0302976BDF7715199081693A8ECC94CD8BDD8B3DC94
      63005CF93A5A193F1C04B1AF1073417E51D9830397751F23D4058143B2137793
      15DF574AAB5C732D07AEECC104510B50710419ECA7D88D4DFF0CB823E11FDD61
      74372930238A8BC0A80FA083F99ED4DD3038098ADC812C271C390093FA409533
      BDF0534EDD0D83CB0871DF771855CB003739E483C17DDF61542D031067B5130F
      1BC0CA8E4706C01761547A8711BFF3A0520690712C503643FF4480EB00AB423C
      FE6FBDD71177B870998704E86F3CB8787681FE8FEE752CED7040BC1588FD0A1D
      CB12948D35CB97607F16F0DCEBD819E64BC01E2016B85DF9C3E11C7001F003E4
      C88A3010886AA0FF0A8C1166CDCAB9D31D46F05303D002E59482E4A563231C87
      037D811580ECC80CC6C0F23488C741AFA050A2DAC3F16B400F5C00B8A8DED3BD
      8E8F23D07C8099D583BE1FCC04E81E75DFF73A2A4B28D481BE1B60F5221EAF03
      5C9154D0A3807AC893DFA1B705344030D646B741AFF60E23390FE091958B1F40
      53E027882509FA93400D602B117D022C018A80BBBEC348846756A7F16082D068
      271E1D001B7011F857EE75FC108426025F032F027BD3B35F3C0A7D15C0B7EB73
      A6C2487A3CC281950087D94A7779AFE3FBF0D81CC5B53332771FCC5D81F78042
      201BEE66E8AC72F0E0F2CE7718C5A2A53D0CFBE3C09DEF7504D14C787A04F81E
      C49AC3FE3DA0839D895960CE81F9BEEE30F2E4018108277B2C086D0793C5401C
      CCCC381F6639056AF5B13CB87D0CE4032D511094D0F9A2A45DD0C7017F014C53
      075D566C910DFC009325D0138040200D70004E4016D3E5276AD9615E06CC0312
      005FC0A3FE8081C575127AE50CF082539206460FC2AC02C28058A0544C90BB0D
      F6B7807F748711A74C00912A55C40795DFEB5865806A5E706A0C98133533E0D7
      00B01BB43B2B8EE99D7C702A7C356A751B3F5FDFA7A2A3A35F656003D9142C68
      B447403FE08E34EEF452C4F76F143E6927F7ECD5EB4DDCCA3179E9071F0C59BA
      74E99057E6CF7FAA77AF5E0BF16E2AFCC4DC8949950C10F5407CAB3DF1C4F8F1
      C3E6CF9B173F68F0604BF3162DF49862D63F3C70A0190C63273CF9E42321C1C1
      13C024044C1004CFDB54550CB47A83A11362D967E8D0A17E9151510A4C310898
      8D97C1E688C848C5A378C77BF1B0DBB633E86A800AAA52069C91F82EEB84C07E
      4806F6CCA0B12EEEB13C04B0714614202211FF20EC8BEFB94E0863F1BC2CAB57
      C640C0BC83293222222A263616574329CAFA2F67C63E198A898E564545468673
      18BCAC20A64A1940A60A4CD5A3A06860AC1006744A14B69CE0EFE3F88CAC1A1E
      3926153CB36389EF5B1A3C9B710146779C3F0EC6A72C36DD80D2ADD7A5269E3C
      C167AB0BEB66E7CF9C3DFB0D3E75334B5F96182A70647730F081885E7973D1A2
      871E78E0013D92C2FE18FCDA03C96EB3495BB66C297872C2846FB15436050C33
      3C2F3DFAED813CEE6A7C6C77EFD3BBF79C592FBE883B5F62206E6CD5F3BC858E
      D84A674E9F7662BFF0E9AF57AF7E1E1FE36BE1CC8D21B45BAA2A06EC03FB537D
      460F1F366CCCC48913B16614A21044B934F1F543122E5472BFFDD65B973F5CB6
      6C29C4F41E0254883DDCE84E0C202921243828E8A9A79F7E7A28D639BD4C66B3
      EC1F5306D2CA4F5766CF9D37F773AC44BD0AD15C003109A8A0E400155C6F39F0
      1550B5EA2527BFB670E1C2964D9B35D3822B1D3E74C88E54EDD9B173E764AC81
      EE87771750A9AA8E0107D2E1EFE40661D3FC4B3366CE0C446DA677DE79E726EE
      717905CB8BEFC3431EF08F1457BCB8460D1BFEB46FEFDEA2E3C78FDBB1D6B907
      2B7F3C02ACAC1EDD1733BEEBEBC525EFBF9FB5FAEBAF73C3C3C2DE87A8BCEF8B
      52158170C39CAEDBA8C71E3B376DEAD4AB58E31C0E7F95366E70BF2F85FBA954
      B5DAB56DBBB76B972E7F6232AA39A828807F4D713E84C4C7C66E48888BFB050D
      5B0C280BC0BFAAFCB41ACD2A600DA80602FFBAB28222D7D88FA1FB00FFBAD282
      E243C010A0746005F3BFAA98C93D11AFAEA270E9D1A2CFF546BF108AFBD58280
      4036C3CD17B59A19DE9146952F519104144705FA029D4EABF5B55A2C111865C4
      32B0BC1E89E6DC87DF8149951D12CBAE52064C1C01554C0437C5D5EED0B16303
      ECDC8F7F71D6AC70B4FFE153264F8EEB0437BCAB8B11860FFBE5304CF07654CA
      000144C4CE8C5143E290C18323274D9C68ECD5BBB732A5797305FE0F56D1A367
      4F25CE2018706A2222302020097E2D689B14B713677B0506187E70A5C2F575FA
      D00EEDDB876054A7C1E84240EFC3278508F2273647C7C408BD7AF654A34B0DC6
      00210C91527358265A161518A0960A888D06B20E46ABA9C19606F95E35C4529E
      B7C63BF9601632998282838576EDDBABD1DD05C15DCB61CB1267B3921F650159
      72C9D16048A80F8F8810900F3261C4AED41BFCC86EA8D594A10CF3C9A931343A
      37A9468AA4F58AC25EE673F0C8B8087D538514A0FB93F7C222F3142845F21A02
      3C5650872F159A1E5E91DE6DF66FC2E02BBAA4CE92CAD0189E2C402DA01B3006
      E858216390541E7FEA30440FC54057C7F2BE9DD19C8D57939E5F77A5F78D3CA7
      17F6279136EBCFAD8519E73E90F2AF2F2653C88F20BC1B380E6C06CAA8D17B75
      62C3314A93C964C57EA14ED89D33029DFA2874F2A3B1CA311AC4460FFBF8F4A7
      B1330F1F8E9971E8F0B8E57FBDB762E5AA51B56AD5EACC6190D1152452867AB1
      11F265B96B516393060F1E3CE88F3FFE1879FDFAF5D118F78C7EE88353AB64E2
      60F0E9B6F3CFE3E6B791431F7D74103E4E6A41A45AFEA028A652CD133151A0D7
      B2464646A63C3D65CA23870E1E1C3570C989954C9CB1FDAFF429478E1C1935FD
      99671E8D8E8A6A81D87B71986AC8967B2DA058AAD024F8631F4BBB264FAFFB20
      F299FD87A32196AD47AF4D3D7BF6ECA8B716BD351CA2E9E0EDE5C5FF97C9DF6B
      42390AD559502CF9224F8DB2FF17A335C37FDC1BFCD4CE43DFEE383B1DAB53A3
      7FDEB265242A583F341551A80F5AF65B1DBDCADF8FDC358C1EDBF1BD38ECE76D
      3D27BC310DFBBE4661D8320ABB778645464636C3B296B97AD18CDEABA2D17BBB
      0221A55C46EF0D857D21B09E46EDDEA0E934B745AB962DFB6E484D1D89BB0347
      76EDDA75000A41043296BF0BAA114D31F1F532B1D17B6FD797D3D02D61C84433
      86F32DF13138E2BD77DF1D8185ED8E88FD5D666C710AFA82C17B8087C10A98BB
      728AE4628B7F6F46854B1C3C68D02363C78C198A4A580FB1D7DFBBEC51D140B8
      4265C1B5AF2A8C4F439BA7A40CC0D868208A703464AF4604AA110F7CDCA552A2
      B9F08D8E8CEC191315D507E640A44C799761EFCA1B57522B5ACFCE403710E771
      698594DE15A52A3CB1288C78D716E80098817F9D8106446B0035012D2000FFAA
      E218331326CEE6BB227E371EB97F50A249D0A03FD6A082E9000BCC66B8E9519A
      94E054656AAA64808CE471910A45D26A319BA3D124D7C779CB26E8AB1BA2E225
      A1FF0D821F8DA3F767C13472674B14EFD1C002600A3010680BE0B408D8DFAE10
      90632D8F8B50A96AB66CD52A01870E03308AB0B66DDBD652BB562DEF024D50F4
      E9A891FD9DC690EE242A6B61AE24087D2DD70D1E18471211DCB00E0143058564
      DF715C742DA043E2D9C8A143BC0242EBAA9C395E427EFA9EB13D9285C77B3520
      0F4A880EAAD027A3FA73ECB5104D54B7AE5D63070E1AA4C5C5A97CC729DF474B
      FFDB98D9F0E753B66488881A84AA4EB6A81B139814131CAC50EB6C82A8C085DE
      C5D9B1E7C4159947853C40C02AC74513BEBED26CE799C21AA87534A04D12D5AA
      5D3BC16C328AE4B2EBDD2E8712530032D1B28F0A0C207F1122AA302E9AB2FA72
      A303E70B633870BFD689103736CE63FB039A0DC25131FCC1830B77C6B88BA3CF
      9E4A50814165E3A299DF5D49DE713A3F81C3F46B5D83B512081867E1003A6205
      870AC4E1862D37FC2C83DBC745CB0F89493FA515D405292A4F9C30592EF1FDB2
      949B9BE722953E4F54AA9D983091989C270F146C290B448607BF5C79FC0EEBDA
      B6DE774D958C8C07F162B1B05FBE428353EAC2E5CD05B8493A37BFA848A9B7E4
      890A156EA32E4E888781C09501819A03C7E98386E9CC807A7D1421EAAC4F99F5
      9A5A81FEBECA210F24335314F5E2C09C994C1C6325E95AC60DE747DFFCBCC961
      8DCD27A52E1A997319B4186D083F053518DD12FA20A02DCC3D80EEA4F76B2D29
      B47A57517641BFD609268D5A83591711618B19B8713E9C37337DBDEEC7B33F1D
      BB213A0DC126526AFD91213C7CF1543490A4CF99C10C3601570123C02A7B6CAF
      86EA3A71610A5B61BE52905C2A8D86FB7791234F4EFCE3EAB275BBDD17B29D2A
      A7CA9C05C76FC0FD3B12155F23F01FC061E034F03BCB1A3A710D0C940DC50F33
      F6AA4B4AB5CE2EB9CDD93979596A7CA7690C38E272E1DC99E35B8F672564E5E4
      3AA4BCF42F3587177C633FB3AD10A9721707A502E8E980AC38053DD8D4A84630
      6BA5405EC00C910882E4743A44C969D39E3973FEF4FE8B8EF8ECEC1C89FE5CFD
      AE62D7C28DF68CD38518147B88234C79559A82F2CEC5362E725C32941A63C1EA
      D59F9D3547D66F801225455F5CF9C9F113EBF7E28C7E1166BCAA24CE54AA4C01
      BFF460E99AEDDB8B14A6148D82B40383FFFE76EF8F5FFC8D0BFFCF60CB4FD19D
      62CFE12BD464762C8BF7BEDBBB55D298535C82AA28296BE3EA9B27776461C754
      06626F0371B95295F57FBBF98E227AF7BB7D5B5132DA90DBE9569EFB6561BE73
      47589A46A3C07EBC6C88C6898CBD7F06207E0EB16943FCB3E5BC22EC7C23EB4A
      70B01F325F81D8178001CBBE5A065C935B83C620A0329509C785A8E1D741D88C
      A596A6B02B6C76FB0E34155930331368552B66C0626A072FDC5C044267958DC7
      0610FE153A2B010F03D008E0F66B37749ECEAC9601FC9551A3F76AD03631B132
      8EB291DD3430D5006A025A80DDA0FD7B4A042966C2C4D90C6BF5AABA5870FFCC
      1FE4E8E454AAA94F4FCEC6B71BE517E2B67D4120A506BC1CDC3254C10887C759
      FE95BE45A632711E551B31E0F5C320C04FA5D6D2F8279E248C67E463B72E8454
      487854A116BFB5E8D65D6A65FD9410071DADB7AFAF6F3C4E4E84A460629CEF54
      632657AFA10145ECEC6855993E060AF22E5BD6D12312FA0979C2844FB7542A4B
      C8A3D27191CBE9249C0FC7558B56C0444101560A07C270435F983FFE7ADDD748
      7E66ADECE66BD2C971AE20223466DC7B959B2FC24E58019F4C38F40CA120CA2A
      4FB4CAE42033962972218641251697E00A0C90CC4AC7454815A94484C6D53653
      A7CDA071E3C653705828BDF8DC33E86F8A3B7F2E00689F0834087F7B803E48A8
      7454C1AB7EE69A8989319846D3E1639BE72EE440BF6DDFD60317F050E7071E20
      1DFE301E9D28B56BDB9A5AB76E4DB8DE43D6DBB56B47EDDBB727B7424D7F6CDF
      563193D1043067160917CF72F34538684E0AA59A9E7B6E06FF77887C6BF14BB3
      E7E13F1870A7911BD70D009C0294389A397326044595A6808B67A5F345DB77EC
      EAD1AA450BC2520B998C66CC7A29A853DB5694D2B411B56E99421D909AF6B037
      6A908CC53B2D6DFFFD8F8A8B4428662232D48C6F81C6F83781500CD7159814E1
      6F057AE38D3796E05F05E4D835C5D5992C0ADCE1225F7383C281C1805336737E
      CD9F3F9F70C74BC514948888476C5C007CF155AFC25D449C2AC2DE78390F701D
      14611686B89CE32218C254A72CFF962D5BCAF2C7DF1A20057AC27F21544C010B
      0E31E0195F132A59CD7E0F3E1883F9512D17D5C5EFBEBB64C68C19346BD62CAA
      8B7F3961C2B8308683C8401F211706D65F7BEDB5CA53C03ED1537146E3CF1ADC
      855890D3E1BF404C51919122EE28EA81B352D400B745436C722C5954FC473C9C
      1206DB31D727FF9F4B952960269029E705AFDD04D44A4A6A80BBDFBD0F1D3EFC
      21E7C1F46767565B0FE6CE7B454E4199BAC864CB83331CB3BA7AE441C2430306
      2479797BAFF0143F6E6F44DC2F25E08FC5B868F2FC2A8B86F31091938B3367F2
      1D19303B0454E173D52FA966CDA6BDFBF4F9068B73F27F8280B95C6A9818FBAB
      0C6AFC494965EEE5DC4048C467EB3F982F2A47AEA2859B6EF407DA7F67BEA822
      7DD9E5BFF922590CD53CB8B419E1A72DD001F86FBE0842800A331A55732223BD
      DFF2F54D7EC7CBABCF9BBEBEC3E7F9FA0E7BDE64EA3355ABAD3F52A9C43FB310
      7FF8C177E5CAD37D977B9B101121FEF0DA6BDE5F4D98D0A473FBF6A31AF6E8F1
      4C8311239E6C3261C288164F3E39A2C5C891131BF7EC39AD49FDFAA39EF2F36B
      FA3FB5DA275C899B74CA5129B65460901012A25AF6D24B094D9A351B15D7A2C5
      F4D03E7D06C53DFA68A3C4871E0AAFDDB3A75F728F1E7E8DFBF70F6D317C78C3
      B663C70EEC327CF8F42EEDDB8F5E58AF5E52BC9797BA98ECAD673906B166B36A
      D9800175932D96B12ABBBD27464FB15A2F2F93D1CB8B740683036D4B1186F045
      3ABDDE61859B6F68A831B4418398845EBD7AB4EED163DC8F336736DCF7E69B3C
      7E2DE5A0F498C2552AC5DCA8A88458ADF6115CEBD6C8919767413FA91295CA7C
      DC0A7DA9C8EDBEAAD06AF3D05148B83DD76870BB0330AD13201099D09C583526
      533D839F9F02031B7B8DC0C083C7AF5E75326DBC678DE84D3FBF80762D5A8C0A
      6FD3A6A3D2CFCF5C208A46854A251566659DC01FA5EDD2C4C666AAD46A793485
      2F1C5169B7ABF3CF9E0DC4FF33D5142E5D0A35DB6C1216350BDC4AE56F070E1E
      7CAFF5575F5D60CAB28842F07F36F82BA0865EB1B14DF1E9AA77608243B2DB71
      5D90A8C261A9D09C9D3BC3A5AC2C95D1647231BC7D7C1C067FFF7C4574F4D974
      2FAF6DA72E5F3964CF2FC87764676B1C172FD6D7E7E6368BC4843A339045340A
      3D4A7848484BB49A5EF81F2EDCE3644BB76B71EFA1566B36FAF959EC972F37BC
      B46E9D443D7BFEE51D13533A5EC7362D973A2C2C7363FAD53D2166738EB74AD5
      00E1CD0697ABF9448361FB5336DB65A555A954C4040585621234D68D591907D1
      8DB3376F7E7B5DAF4F4E311A53C0D4EAE5EB6B95D2D31BDD484D2555EFDE7F99
      22226426DC93E15B593A929595B12723E372BB909048F442014AA5323AD8C727
      C29A9D7D0D9727090A8BC110AA70B9ACCEBC3CC9919F7F7ECF89133B7E2A2CCC
      0A0E0F57C5180C8D1544162FB3D95A5450D0C8B56BB750A0D5FE892B2D315554
      289DFEFB6FF7C54B97B277D96C7F37351A4F61CF96AFE0729930B71189CC3F40
      48967663A346C34FF4ECF9EBE94183B61FEDDF7FE663313118B1F8263D3E64C8
      A8E3D85877FD9D770EDC5CB4E8F4CDB7DE3A9DBD7CF9FEFCDDBB3FB87AF6ECA4
      B2EB68FE3A9DD7D6962D9F3CD5B9F3B6E36DDB6EDB58BBF6242FA5524F78E836
      D4AD3BEE48BB76DBFEECD66DDB9676ED9E88D2E9F0EF5946791DED858913C7A6
      2D59B2F4EAFCF907AECE9E7D3A7DEEDCD319CB96EDBFF0DD774B663FF5D438CF
      3A9A11C436D5AF3FF468AB56BF1E6ED66CFB86C4C46740DB405E0A85766D7CFC
      C83D8D1AFDB6BF65CBEDBF346A34F5498D067F94A7295D475BF4D24B8F9F0293
      7333661C38FBE493A74F8F1D7BFAC8E8317B97B66C39A78B97573C0F0C02140A
      DDDAC4C4B17B1B36FC6D4F72F2F6B5313193415B4F5651D47C191EDEE7D7C4C4
      2DBF2727EFD85CB7EE9BAF63F10DA305791D0DB20A6FD3BA759FD4CF3E7BF2E4
      5B6F2D3D3E62C481C30F3E78FA8F4E9D4EFEDCA4C9F72BFCFD074F1645EF395A
      ADCFC6B8B8B9DB6BD5FAFDB78484ADA0F928686B998172454040A3EF2322D66E
      8E8BDBB9B966CDB51F4746B688D068501894224AB0119BFA1A601DFFD17D5BB6
      4CDCFFC20B4BB776E8B06F6B9326C7B6376CB8777D42C237EF592C0F2FF1F26A
      931A1DFDD54FB1B13B7F8888D8F84960601B3090AB01BDE6E515F46D40C09BEB
      4242FED8181DBDFD977AF5667D5BAF5E709C97177F7CA830A208F6ACA36D5AB5
      6AC2C2A64D176D484AFA614B52D2EE4D0909BBBE8E8858FF6950D0F275E1E1DB
      D78786EE581318B864898F4FA405C5882B1B85201FDEF7F27AE84B5FDF4DDF04
      05EDFE3139F987B4112346EE7CE185003FB399977FCB8D8B9AC7C5F5F9283272
      E4BAC8C8B5EB232377AD090BDBB51A370EAC0E0CDCF3A59FDF4F9FF9F88C78D9
      62D1CBC43D0F3844ACB05A5FFBD4CB6BFB174141BB7EE9D66DC3C5C58B271D9D
      3FBFE6D38D1BFBFA7A7BD71EF8F0C343478F1C39DCDFC7A7D1241F9F58C87FCA
      170101BF7CEEE7B70744F720ECEFCB2D96B7679B4C71C1253BDA3CF42958A150
      2EF4F24A596A367FB4D464FA038177AF4F49D9BAF7E18797FDD1AAD513FFF3F1
      E93F303676EAC3287E83D5EA87E7EB74E3165B2C2B9659AD7F2CB358F67C6832
      EDF8C06058F99AD1D82E48142BF40B32A317FDFC74EFFBFA76586C342E5BACD3
      6D7FCF68DCBDCCCF6F270AC16F90E9CFF3BDBC76CDB15A772D30997E7ECB68FC
      F55D8361E7BB3ADD1EF6FB8E4EF7E9DB4663F719FC812653ABE2F17C40806E9E
      D5DA7491C130EF4D8DE67BE0F7851ACDEED7359AFD73359A137380056AF5FE37
      D4EA3D70DFB140A3D9FCAA56FBC61B4663AB674C269EF2A9827219E740886B9A
      C9143A4BAFEFF58A563B7FBE4AB57AAE4AF5F3F32AD5A1FF012FA1CD9FA5546E
      7846A55A3A46A51AF398569B38C96C2ED793952157B5D11FB21C6F30F84DD468
      EA8E55A97A3EA8504CE9AD503CDB5F141FED288A9D1345B10E4660019873A95C
      E655932EFF06E956808816E3130BE0036A46408B56962B5269AF583E54B1EDBF
      FB8B6E1DE163898CC1341883CDE570FB01C2722FEFD35242531E55DC2789BB0A
      56A104CC59F2CD5AB75BEAC59FA3B8D4AF22114C4895738454B8D144D3F3DD73
      631EEC5DEE1D2C2869789651ADBAF4FB62DCF0CED424398A9AD68BA1268CE468
      6A9C1C438DEAC614EB25E606D0EBC32DB94E2CED3D70B2C6D68D5FA782D415A0
      5415A780E5559239FF7BEB7369ECD007A8EEF367C86254925EA726354ABC5685
      B97D408D6946DCA5491A4C99694890FFD1E1B9EE065AB16A0BCD9E30A82128EF
      03F86CB2C467382BE4810411D8EC0E32E95564B5A8C96A2E86C5A4C63F4FAAC8
      6CD2E0CF50556434A849AF579246AB249BDD29AFA9C9846F7B143328893DBFC3
      DA0DD98A9C58B1E298ABA1AB304FAABAA583A052832B7BD40052A6805E64C338
      17B35D1CBE1425348B1994BA72B64A546073605657411C58061A09A5064C186A
      E820AAC0728802CBD10A7C7730039C8D2A43E596B19801E741891B4FE5141681
      0188882A251821A64C9489233F348056A7241DDCB4EC867CB1391C98799407DE
      2554A095D02C66007BA942261480814209E260A0020A317778DDA5A4D3850ADA
      9D45F4CB55897E38EFA0B5270B69CDF10272200FC0A19444594331831279F10B
      37EE112DB4B9B08AAEA07C9740A7F304BA5028D0755CFD9DADD01096EC08FFE1
      8AE944FCF5312281456A62FF1C8EC397A2846671314510BC404E113DFDCA8752
      DD86F5E9F3936AB2499858851808998AAF1D12901AC2E5A5921D47711CB87F1C
      79E5B0B969644D1B1DDC7B80163C33F25631054156DC9E739975709965070965
      3B1F81BE181E49921B53C9841FC48627158B0171127844C23AD64FF1D1B4EAA7
      E3988D875DF654F2E03C402A9425D652AD302FE7D7E307F7B7FEDFBEBDA0878C
      43BD909B0DD6653220C40A4D04E282F96C91444C5CDB0BF37F2D2552C6209431
      7B8C31305801590D1A3F7360788D3A03D32EDE08EEDDBE3EADDDB29FE2437D2E
      9F3F7EF88BCF17BFFC85ECE9D6231DC68B40A9AA8C41E94B364C9CFB81D4F381
      96643019E5DA8A2F4BCACFCDA3759BB6D1A219A3AB0D5FECA1445E4CF0763C3A
      79F60A93D56720B201DD31E404B9403A8EDCAC1B5F7CF2C6FF86DEEEBFD4BE04
      212ACB83520F2506109909E322A03832304049C035A05AF57F41A7EFA91FB22C
      20D37BB223D0FF037EEFEAF4E4D76E0E0000000049454E4400000000}
  end
  object TBXPopupPanels: TTBXPopupMenu
    LinkSubitems = NPanels
    Left = 208
    Top = 136
  end
  object MenusImageList: TTBXImageList
    Height = 18
    Width = 18
    Left = 48
    Top = 144
    PngDIB = {
      2900000089504E470D0A1A0A0000000D4948445200000012000002E208060000
      00D4C3866700006C60494441547801ECBD757C1457FB3E7CCFCCBAEFC636EE1E
      088104777768A1B8BB94529C9602C1A5A5A548B1BA60C5A1688B6B7177082440
      DC75B3BBF35E67216952E479BEDFDFEF9FF7FDBCF99C6BCE9923D7DCC7EE3D36
      138EDEF0577FC20E05BCA38108C00D108054E02170E9E492CED9B02B19BED21D
      6EEA7DBCC55514C5DE3C475F9B8DCAEFAB07392F88F4D1CF356AE46B059ED620
      CA283CC81B76255389A8FEF8ED26E2F9CFB44AC9E296D53D6AF56F1E42FD1AFB
      51A75A5ED4BAA61FB58DF30E321B14D3C030076441B0CB4D391148241C47E302
      DDF54306B78E30756F1840A13E4EA4D468C9643252355F1D35AFE242E33A85AB
      A27C743D20F14C90B930A6D3A74F4358E662E0B858BD5A3E6050CB5065EB1837
      B25B4B89974849A95293422E27A35A2057A39AC27D4DD4ADAE87DCCD20FB00C9
      DA002493C9E40E89C0CCC1A3B3AFABDA35CA5B4B89E985444A1D88E464B78B24
      2290E778F817D0E507A9E4A693528859298554DD9096CFCECE16596D904FDD1E
      3AC4ED5B3DC0185D4A025978152915ACE2442A2C2925B594233F370DB91935A4
      554A29392583D2B272B984744B91D5CEFD1EED5C50C22421B0BA83686DB706FE
      EDBDDDF42457A88813242497F06429B552A85941BE2E2A426D524E6E2E5DBEF9
      908E5C7E42A79FD26D8B5D683FA44AF6D332221323AA11ECD2A5511533D50C75
      A3C48C22BA955448A5361B99541C99353C7939AB48CE95D0A3C74FE997238FE8
      76A6E2BAC84B5AA15DBD908080991C5C929FA4E4D96C51EE82AB1E85AB91112F
      DAE9D88D647AF4AC84B2F38AA856A0966AF948E94E42322566DA4491B84CA44B
      07C8211173207B2D2402F773CD1017739F46BE14E8AEA5BB8999F4EBB1A79459
      60A5ECFC128A749750D3409E0E5F49A2934F652536897222A459C1D2572452C2
      63954A26F46D53CDC4B7AA62A45DE75FD0B98412B2586D9457504281463BD5F3
      B5D31FB74AC5348BFA04717C3F103D41BA7F246237902A04F65267B5D028DA5D
      545D7D5244BC20A382FC7CCAC9CB23A58C279D515F9A69919F15899F8EB82740
      64875D99887980CC07B5334846F6EEED225DC26A04B8D399BFAFD3E1DBF7A848
      EFFF9C93C87723DED720B80DBBDC381A64D9DDC89123B5550A0F540FB39C7DD0
      C053911D1EE06E95A84DA431798AB52382EDA1B2F407FAE4C397E457964B91A6
      52DA4A375E5E5E9E515151AB5B358C5D1AE52DA9EEE5E42AA86572AA13134A0D
      6342B9A6D1AEB15D9BC7CCF1F4F41C0D2267E0CDA67EFDFA818D1A35BADABC79
      8B07A3464DCB59B5EA44C9AA55274BBEF9E644C9BC79EB4BBA74E99DD2A04183
      BBFEFEFEDF80C103F8C7ECDEB343BA63E736E5B6ED5B5455AA543185840437F7
      F70FE8D8B163DF8F7AF61CBBA07BF7310BBB751BBDB04B97C10BAB558B9BE0EE
      6E7ECFDBC7BB4EB56AD14E2C0D4BCB38FE61FC3F701D397284AB5446FF5B2E15
      FE1CBD7FEFDEBDDCB061C338E447D9AD5B37FF2E5DBA5487DDAC4F9F3EEF75EE
      DCB956CD9A35F36D365BDAE3C78F994679ED7923468CD009274F9EECAC542A87
      2A148A917ABD7E829393D36093C9D44DA3D1B4855F23AD565B1FFEAD020202C2
      4158885ACDF3F3F32BBE79F36639211EA6E09168727878F858D444273737B76A
      9032401004779EE70D52A9540EEDA7361A8D21201A1E1212B235303070457070
      70D74183067974EDDA95636CB9B9B976BEA8A8A8100978894442B01DE0388ECA
      C0FC40CA741107290D66B3B99B9F9FDFD7205DEEE2E2D2F3BDF7DE33E5E7E75B
      04B0768614A160E6D035280F7DAAB8B89825841741D5DA1DA476BB9DE0CFC1E6
      E472B906D90D85A48D4B4B4B6B9E3A75EAAAC462B1DCCCC9C9E98027F3884028
      17B25AAD545252E22043213BDCCC8F498D6C13B2C2414F0BC9C9C94E2F5EBC88
      7DFAF4698824212161874EA76BEDEAEA5A1DD27088CC9E889F379E0A0A0A1CA4
      4C343C995253531DC8CACAA2CCCC4C42B130520B1E9ACF0D1E3C58121616D6B2
      56AD5AF30C064395C2C24281950F938C65F5F9F3E7F4ECD93307014BC8C891D0
      2125722342A2ABF7EEDD1B2AF9EEBBEFACC8EFFEB163C72A1B376EFCB95AADF6
      43E171884419191974F7EE5D626E944FB9744C4256012CBB7870367292CFFC1C
      78FFFDF7555F7CF1C5845DBB76651F3C78D0BE7FFF7EFBEFBFFF2E2E58B0409C
      3B77AE3873E64C71F2E4C9F68F3EFAC88E5CD8D0602DCD9A354B4073F812E566
      7690945DFAF7EF6F5AB468D1BC3D7BF664FEF9E79FB65F7FFD55FCFCF3CFC599
      3367DA274D9A64470B2EEEDEBD7B1A08FE46DB5BEEECECDC038DD91FE91D3D04
      F63FA67DFBF6AEF3E6CD5BBE75EBD6826FBFFD56FCECB3CFECA3468D2A40B7B9
      0F35B3D5C7C7672CB2551329D84F1807FBEDA64D9B3641C8C6AF4002082EC4C5
      C57DE3EDEDDD03AD3C0CA934C0BB0910C1619090439FF2029AB9BBBBD704810B
      0264C07F478088FFF78DEB57E2FFE8E902AAD9BD57AF5E21C3870F8F40AD351C
      3060406FFC4DE8EC74FF33DCCF422D8D402D79D6A953270759CD3E77EE5CE99B
      C4E6518523F0EBB104EAE43B602DF4D104A89396B043A08B4CB0FD50531FFAFA
      FA2E41810F9C32654A18C8CBC60CE59C3C227746A4861E1E1EBE68E1285719CF
      7A38BA077A0AC7FA1C87D6AE84FAA8079D350F2AE41BA8900903070E0C84E628
      57D53C7A721E681D89C042E8B40EB5010F87CDFCD072999B75662D246C1C1A1A
      FA594C4CCC0E28BBD920ACDEBA756B9904EA20272D2D4D442775142EEB98AC5F
      31223CC0D139211D313234440E60AA46831C44E22111E8979D804142CF9E3DC3
      140A457DA80946C4412FB1F48E0ECA08983E62E4ACE33237B2ED5021503FDC93
      274F9866B0262626FE2540CC17501FBED0760150113C23644F6784AC77330206
      3C959080A03288A916E482981F74532AB4C426C98C19331E7CF6D96733F1241D
      6AAF0102254C1248E950B3D0378E844C91E1210E65C61EE0101B17E8A634A892
      743889476DF1206B833675F7CC9933F643870ED94F9C38216ED9B24584361067
      CF9E2D7EFAE9A74C8D88E3C68D13D189EDF815B17FF0C1073674A7BDEE1EFE81
      8CC801744E397A7DEF0D1B363C662A64DFBE7DF6F5EBD73BF4D19C397344482E
      4E983081A9123B7E388BA12592A1558FA02F7EA837988D0E92B24B870E1D94D3
      A74F1FBE7DFBF6677FFDF597FDA79F7E7248346DDA343B34A81D2D3D1B557DB3
      468D1AEBA1E387205D554003BC6EDAB56B67C0D3E743B2BC55AB56D9A74E9DCA
      34625E8B162DCE60B4B204ADBE139A00CB8A0AA9594DC37A8B69D5AA95FBA851
      6317CE9C39F7D9AC59F3ECCD9BB779E0E313D00D8D952933C9BF9339F7DCF176
      C229537E0A3C70E0DAC103076E8BB366FD92D2B66DEF7E2028EF1270971BA7AE
      1BA442F91D1C1843BAD6AC59B365BD7A4D62222262078487FBD554289CF48585
      36C16C76F2C26FBEC1D3D305DDCE6C4783CC4412875106B6142B3D01ED28B26A
      D5AAABEBD4895D6134F27D89D45EC80EC5C444C82323236AD7AF5F6326D4F04A
      14F46030680187C9DC3DBCF27007CABD0E7EDBAEB76AD5E6D1B8710BF3D9F891
      818D2167CDFAA1A463C72ECFEBD6AD7B1B6A6511182A57393CCA0DBA8A0E6AA5
      9EB39B779BD66DFA8C7EFFFD51333A771E31A353A76133DAB5EB33C3D3D37730
      246C8E5A8B4422364486F50E131F1F5F59D40A71DF1656A98C2AC4A7A3478FBE
      95EC4D616F25AA48FADFB8FFAF11395AE4DBF2FDDF48C2E2203DE720623715F3
      0D37C5D34CE6FD1AE26916356EDC98FEFD2729F3406039299E2096F9BFCDAE18
      9FC5E1D9E5FF06FE3F4CE428E0FFA670DF558E48FFF6EA47ADBC312D6B1A6F0A
      7B67F5238143E28A8C78BAA369FC3BECFF25B506F15FCB5259F6DE155616E7FF
      C8E60E1EDAFF9552A96A8941041BFEB2A9159386C34041C030865506BB773C84
      E739BB449058304812317E72143AFC4A8B8B8BF6488C06E328A34FA4ECD2931C
      47E4B20B472205B92828CADB483C5AC9BDE45CBA9A984FB67F0D76C3DCD5A4CC
      B8EF2661925C799A478BF72650C53F7F27194D6BEF8721DF4B5F09CFD1D13B19
      F4F7E37C48FDD28F5D7BD771A786CE7629139DB0B0872530E6FD122C2F3DE39C
      29CAC74402FFB28504B869695803777A90F298D2F2AD2F23E22A3A3248F43216
      3CFE3122A924764A78968655ACA272EF82620B3D7E9E42465909610DAFDCBFCC
      E190A8EC46863BA560A360831D4F10E8C1B30C52CAA52493482835338F50F814
      855989DD6EA5E44209155418BA974BE46392D2F4B65EF4419492A23DE4E4EFA6
      23B5424A761B96E4F024AD5A41016613857BEAA97DA486E675F6A55AFE9AF232
      741005B92A685A077F6A5BDD8B7A3789A4DAE19E14E061222F17031636A5181A
      F3E4A45591BFBB89AA0599A9631DAC27457AD0A7ED0329D64F8BC7BC2AA3AA5E
      3A8AF57F59B0469D8AAA60392CC0C389742A4579610B024F7A8D92FCCC4E64D2
      A9210947DE4E2AAA1FC2864C442815B0A16A1DB4AF2E3289E0888846F7CAE7A5
      C5DA130AEFE5CDAB6B591C07D1FD6436491629D8AC75ACCD66E616905625279D
      5AE9688C2C8D1DF55C585442997985A457AB1026A7949C627A9A59EC58F17310
      5D7F5640BB2FBDA08F5BF8D0D317A98EDAF1331B50262FB3C762E6171653525A
      0E3D7A9E4525A5A8591F37FAED7C3A569215D4C099883B77EE8CF884F3A3F9BB
      1F92935A42AE0A0B8523200CB5532DC81D65824577749167E93974EDE10BBA9D
      9845D7526C945A2CA78C421BF5A8E54E8D5CD2F21C1211FE5825A717D828BB88
      A7423CD1DD584A4E287829CA0BC16444415BD105AE80E45EA640A5761BF32E87
      A3FACBEFE0C0FA343DCB17C8DFD34C465439BC1C4683320BF53113564541C239
      FC2A5ECA25AAE8596A275A7F3E8D42BD0CE46962C369A25414EC2FE7528949FD
      3A0DAA1FD5570A55201DDCD0B32297A3961351236544E9F916F27356D2C0062F
      89CB224763699FCB4DB7727BF7FDB1D1C9646A84EAFDCF63C2B2D4156C08624B
      4F4FFB93B018A9ABE0EF7062AACE03D58108C0A1391D01EFB8F0369BDDF88670
      560C81F05F08F400992BEC771A36A37EADE652C771AC6E9F2165143007980AB2
      CA4B3BF0AC685E23A9109804F74380D54223D806C0619CBF1405972F452DC8CB
      B35D8908010250562D59487503792C85CD54A51D61814030FC02E1D707A8877B
      296C472D33BB0C4CB9B4436055F4D152E03602D8BA80047613602AF0057ACC68
      6018DC238050C4E72A49044F0B1002CC036A01B745A29BB059B6C622724F8EA8
      35EEDB01CCAF29EC0F002D8F4B4553849BB30093AC1FEC2AC03DE019087C0035
      12C8607BC1AF18108038C0087F58AF0C6A0B02D019DC4E83E8D781A670370014
      08100018A80C2219FC98FB6BD8BF02D9125C2A19901522CF8CEC1ECA88ED29D5
      05214BF802295D10B9AC3298445B717F0F692CAF1121A0CC70487819D9A8098F
      48909EC33DEB055E204E83DFAFC0134602FBA5CE16E3E32370A3052EBBE967B2
      A7C7C06D832ABF087B1F100C48010B003EBA0DFB3C60051C86775C899AC2DE04
      74AA657DC21214C17D1B7800EC064E013780CB904603FB7D60253002C5A08120
      BC0437CCC871F105BEDA55F0E358D8BBB8F878D610091199AD841F23FE0B3693
      7810B21C231295E07E1B80DD385C2B184FB897003C9EB2136416944D21EEB590
      A439EC7DC0324009926EB0A50023B6F170FCDBF8C28391D581CD4C262E3B0067
      C0034820A205C04680D51C232AC06F1EAB1C3A07CFF915F003DCA500A58DE78A
      21CD2F70EF86744ED0FF1C6A2A818816013B013B2417E9B7F53F9B70F31F8D79
      D933B3C7F2EBE17ECB8E4AD68D8EE150769CDBD799CEE6AF5374B4E65FC3B857
      6C9CCBE2A248A7592F6A6B7B7C6B12DC2254F057BE820CF69B8A83FD8622E85F
      064FAB0BAF29C0618089FF14D9B1C3FD56F34676C47E0EB802D380794028F04E
      F3362256534CA939217523C00C380CB423FF4A3B2A1C1EAF2EC22B9B353C4EDD
      3A5E0D58E1C7B2E18946D714EE1CE077F8F380A7B33D5F0BF46A52FAA0F91F0D
      0262BF685C2F2CBE7163BE9C08916448C0129A51CDC9706334452D60DB80448C
      89DB4844DB800031B30586A3EFE3F7AC45983DB5999798530F0F0CAE48847BAA
      8144930026C513B49F60B85959554160138EE3426CC40562D8CF3FE29DB8E392
      80C372D1363D844F65C580A8AF0C6A2B0ACE45908865EF0F104543126F9EC406
      1CC71B10865B910D0D9E6048A7C57D1230E22BD7F5B724705434F770C3B4635D
      D8B148D5D8642F2C2AE4A44A0B49911672711C4BA346F84FC003E039565299FA
      85B38281541C6EB56A7BC900CC47E6AA458BDC467C7106AF46CE38D63839D4C4
      0BC419041C032C5FBAFC66E0E078CDB45C9CE81A614B1EFB42D08D3D2FF8488A
      38C9792B49F2103108D92D84BD0B588D46FA1C362D5BB64C5389286A49AE5221
      96C60E2839DFA3B1F561E7E392C0E7F314CD9C2D9C900A959F8444D540741DF6
      2AE00C900732B6EC2AE1A077987A2886A7FC94E05B054F9F186E4BADEA26E66F
      99236FFEE36A799D9118164A11FE10242C3B6E703F0398544B40848760A0058F
      798005D0D7B33D6900BB00980FFCBC5A5157015B89A9DB23D83F125129309127
      0AB5137583FB77A09C280F3731403EB01438055C838EB1387F399358DE214953
      F86D067E02342019099BB541396C8791E01A4F44ECC908A77C46807B87E1888A
      E0D80DB08461B07702DF0036E07DA09C08EE771B340725D00B1DB53F3AAC82C5
      C6BD11F808A8C7EE5F628D68808633014CBA977EFFBA9A9725E9DD97DDAAEAB9
      E28CB22C08247AED8A0237C59A6417A47DBD1D61859DC70E83110B972E58CF57
      A6A7A70B688964301AEDD8B52D4978919C7E3F2539736CF71EA565A4CCAE2405
      B673D4581DAD85C43D1F3C78D018BB9DEE205310E6E47A93A9D4C3C73BCD6834
      9D71D56A772CFEE5E77D93FBF6CB62240CE5445851D76173A0CF8D1B3786C01D
      7EF7DE3DB9D56E278954CAD9516DA53C2F170C7ACF90B0B0CE514141714A41E2
      3B6EE5CA555F8D1E9DCD887876C1EE9E023B367D21D127EBD6ADAB7AFBF66D39
      36FE39F37BEF716E6DDA90577434F9444592CEC383BB9C9E26D97AF3864F625E
      EE68A3543A68D2DA357AC621DC58B14292A254D6C7FAF44C6C64FA97582C828F
      9F1F27737121CEC3034763049260F397944AE2F4382EE3EACABD9049B92CAC51
      0728953E6AA27BB5716281CF532A3D90A5AE57AE5CF1C74609A731184850AB49
      82DD6379420229D3D258F68853A948301A4874762695A7273DD368F864411220
      972B3AC8950A3D9FAA568763FFA8C1C3870FE5A82912E572B221110A98643939
      C4E5E690C87398200B64C7039E55AF414535E2C8E6EA464F2512992093D52BC1
      E493B74AA5B1D885F1C2760F0705C571EEEE640F0D258B9313B1C256E12013B6
      B108EA954D7EC9AA501076AC482A9773D982C0F172998754B447B04DA8414F9F
      3EAD76E7CE1D549094537B79910224D8C8262976DA3DCC66E2513E2495112793
      93B4B8841499D990368FB4A516AAA252D92DB9B97F4BD0D8402EE7200E713A1D
      153D7F4E126C3D6B90C512547BD293042AC1112B0B669225020E39A1EC0A1940
      E86CD093A64E5D6B9AD59ACF6DDBB66D0176CD477FBF699346A8568DE3B17CA1
      494C241D1249F12B60E77962106117582C98AA1652016A31BFA8889AD7A92376
      6CDC24E1F28D6B0339EC0F75C526D4A2EF7FFFDDEFB95E8F1F0B9E532525110E
      0290140D9249CA4E1A95C26D83243928FC82A26292C864E2D811236CDE1E1EFB
      4E9F3B3B92C346B85F7474F4A2CBD7AF77FEFD8F3FA45848E158D54B912DB6B8
      822361548202B6A00D8990C8FAF02115E2785593264DEC7D7AF77EF1F8D1A339
      C74F9EF891EBD1A38704C7603AA1A32EC6AEBADFF1E3C73946866D41B4009E38
      14B45D83C514B41D1BB25372EB164504068AD8922D42FBDB70FAF4E939CB972F
      7F02DD453478F060436C6CEC789C441879F5EA55230279B6A387A6CF1A10D950
      6E02DA9693D128560349CDD8D85254D2D1F3E7CF7F76E1C285CB67CF9EB53175
      49972F5F2EC6D6E13DA552598ACD1333B676B4388121A0ECC8802CF9A0AB540D
      0EC6A99F186B5060602A34C241102CBA7FFFFE456CE8316D89675630CD9B37D7
      8F1A35AA350E0BAC82B81756AE5C9908A402CF717F73E1C2859BC78C1933A865
      CB96BEFEFEFE8E0E5F21796527F68F0494992BB6A16B0D1C38F0830183870EEE
      3774E4C05EC3C776EA326242D58E13179BFAAE3B5AAE7ECA52739B366F30A060
      55D8C074945759C08913270D5959D91E58372A75717649AA592B8EFD10940557
      B4459C6129ACE851EEC68E96124D6230F6187FC766E5CFB86F521EF816C7DBF2
      E98E828EC3BEA3884D4B2524AE0332F95B381CDE6F2352A3D6B46DDBB675C1BE
      632E76AF8C886D00D96B65037F87791B1141A5F08004246CE3D71BB1FB02AD40
      E60768000EF7E5E66D443616E35505886857129C13A985F300BD90CD495080A3
      101E0532473B84BBF2741D011C030B00D813D167053B0A3C19ED8B6595860E1D
      AAC33E6D2D847F000494C52F97081EEC57B406026B03CE00232248C3D7AB57CF
      8E96AF40BB92F7EBD7CF057BB916C4AF8AD3417D11CF097829113CA5B869AE56
      AB07A1B606A06CBA633D5B0F3F1E44C190800D525538F6E08DACA91A376EACC6
      76B4157DD38C382EC04B223858A2EAD8A3367CF3CD37EA860D1B62735D512495
      4AAD286C35887588C3A4C72DAFC4C39CB0AB8E67594BE09F07941315E3260D3F
      8C12FC646BB09F4D1F7EF86136D8D8500741E5866597A1183DBE105BFAC90849
      031CE32842D658A00FC4EE131111118C822D825A098024ACFD304958DC328818
      1B244E9C38310FBF853BAF5EBDBA8D053002669791614ECF75C1C67704F6F715
      E8BC8108FC778BB660B7E611A486A2CCFDFADAB56B3711E7A544CC510648C76A
      EC3D9C2BAABF74E95223F49313C264007BA808AD980B95F2005BF82FE0B71C44
      19B05F27629E207343360757AB562D047BFD323F3FBF00F8B3EE2142A9658C19
      33E6E1F5EBD7FF86DF4610B1027F331122B0AC46806C3C4E40C9B0358D9F174E
      80BF9892925282834DB9382BB109F78740F4EF0A81770503A9DC811FB1DBBE0D
      65F56B19D0383742C56C475873802F4BC261FCE7F9EA268B8673E50A0A9158B9
      04224C05FCDB58E1F1F476BF7D05A55A0F17B8455AB376252B44B8DF6D7CD76E
      E6BE5B3E48BA6355DFC0533FF68DD91ADF9495D9CB446B44677EF8B0D1E2CBBB
      775F9F0CEB26FAC88A7DA243CC73FC02BC967B077835DBB3A88DF4552A2CE4BF
      72BDCBDAB1E47DFEF0BA3E55C2C27C168A25C59D8B73B26B7BB83B7DEEE2E2DC
      69C7ECD68C2CABBCB0DE46B47F65774E6F50478784F92C36A8659D54325E692F
      2E122C3959915A8D7A86C1D9A5D10FB7EBD8FEC9E75B9870A2D71C12EA3D4BAF
      92B6B6E2279B17310DC42C27333D972F2ACE0CCD299675BA9F66BBF8D682DE8E
      ECE8F4AA605F5F8F099CB5A88F49A3509616634893934719E9D9949D55509A92
      5574E9C2C39C59B5BB7E7AE68D441BE777E0DCDC8C3121A15E73D532BEE9B3C7
      493205C6FF02D9E8C50B9C164FCD2E4DCBB7FC7DE852F2DC1B4F728FACFA650F
      6B2AAFE769DFCAEE81CF4E7CBA3BFFD24C31FBD4643165FF48F1FE865EE2C5D5
      1DC5FD731B97AE1B1B77BA6723EFB62E3A9982A5DEBE7DBB9AD9E560D9D9F1E5
      FB5E094726AFCE39FF5951F6A94962EAA10FC5842DFDC4CBEB3A8B87E637B1FC
      38BED6E95E4D7CDB7839E17CFAAB94BFFDF69BF29593E8B7B9EDB923DFF58B79
      76F2D33DC9C7271625ED1F6D4F39304A7CBCB59F7876793BF1C0DC46961F27D6
      3AD9BBA96FE3003715ABF2F2B4D0AAFFD4FEBE953D221CD9B93843CC3A39517C
      BE77B8780FD961921C9CDBD8F2DDB89AA7FB36F36BE761C428B59CA28263FDBC
      0EDC916FFB063F3D367533B253F2323BA3C5C75BFA39CA8465E7A789B54FF469
      8AEC98E46FEA772FD97E9DDFC9FDFEC1F13F3EDA37A630E18F1162EAC1518E32
      39FF4D07F14F94C9CF136A9D01491348F2E69A79494334E7A3A6FE87BFEFB7F9
      E9BED1457736F5136FFFDA43BCB4A6A3A34C7E40C1F66BE6D73ED0AC7A37C92B
      32D9B0F7AAD63DFD7DCF43F736F62E39B6A4152329FD7142ED533D1BFBB4F1AE
      503BAFE2BFDDD2ABA58A816D429BEC5DDCE6D05F0B9BE5FF34A1E6F95E8DBDDB
      38EB3067787BB27F42D688FF48AC5349E5DD1BF9D6593C3466F1F076811DAA05
      1AE4FFC4FC0FAE35A21B41430A15A2C9E4BD9606A8FB7C1D485F65B25F930A41
      70AE119D10DF13D0E1EE1F03C5C654ED25F83075B910F609A8DB6B88580D6E1B
      DCD75FBA6D365F71CBAD447A2FDACE61A376387713FE3144620BC4FB88887BC2
      5AA43B6EBC803A00238045D5898A63DC3612D44C712D2C89542F36F4E0EC9CBD
      0602AB02CCC482200E603ADFCC8896C0F757602C24F81936C1FEDE4DAFDC0437
      DEFA505E82FB3ADCCEB0BF43D806B8599C75B0C7005B81C53C2E4780838890B1
      75EB567EC396F5EA1F37FEE0F21EF5F5F1E742E254A2A6356F17EA939D5C108F
      C587F5CA0CE792E13A041CE3F7FB1E78BED77BDF2D4C665AE01057373777D741
      6A57D587B186BAE3AA2A62BBBA091ED19E9C4F6460514415B72C6F2FB7F5A445
      96FF211CCEAD811077F833F6A08FF60B35E7F9FAFA0EC6E0A1875AAF6E91ABC8
      ACFE48B8E39DC9A729D552B5DC49E16A366B3DDA052BC387D74C6FD2BCFAA346
      CE18BDBDACED35E260147C28AF94886EAE06B52B064F6A4C6A04B554C369781D
      65D93229DF9E471A890EBBE912BE4092A7F3D2FA443431B6EEDE58D1F6233CB4
      E9800103DC056B911C59E3F87ACAE4E4E101F94998A31146682411A5E42DF5A5
      00790869049060B550C6CBC8C09BC84F08E13D95BE6A7F2FFFAA38803A1C63A9
      31A3EF4E7DD27A7B9B67FCCF092A5A74C9EA89250DC7396CCCC348629751A83C
      8AC2E455482E2A4923EA285A1E47BEF200C2BA0404200EA33919066291E7DCDA
      7D7A3C70703541D2EC43AD59AFAC5E55955B525A5ACA86C30EC9A490CC6EC301
      785B31193827F2B505119F2F751CEF4CC29C17C3448ED90F052F3D363AFF1206
      C4981263B4050538C0E76FB158A438C1EA385DAF5440128986720AB349922327
      3E5D4A2F9292292121C171F814C73B1D0754B9FBC76E592F6CD92F99E73D3BCC
      9099E9B4ECD2BE139877348154729C5AE53036A2BCEC02121F4AA934D742C9F9
      2984306265C9F286D1BFE3A8ECE3AA83AB2736A862609DB63AADB6F71A3D7AB4
      DBCF3FFFFCC9912347B61D387060D7E1C387776FDEBC7937668DBB7166743746
      6EBB715E7B37CE8CEEC639E55D9818EEC4CC69A3FB985FF7D0C2C448465E8EF1
      E3C707AC59B3663E16A2B6E320EAAE5F7FFD75F7E2C58BCB8930EDDA3568D0A0
      1D38EFBE1EA3B82F30981F8C66130D020993A8061A545FDC10CE8CF2185F57C1
      40F32B48B5F3871F7ED88D83A7BB707E7D07DEDEF81D04AB31829BEAEFEFDF09
      E3F150A4D1D38AA251B4DA16CE882682E817C0090184573DF8214386D4F9FAEB
      AF572E58B06033087EC60316610437105388DA2A95CA154D846944A682CC48B7
      8556148C85483401046680051CDC359CFB5933FF76D5FD59E9D6E8F30B779F0E
      1BD13ADBC9A540DC5D7B0B2D2FE84932953FFAD606100C459A96401712A4F559
      E77B819B24E00C7005A00253D8B96786985F0E3DB46DCFF269F8A5E8576B1BFC
      F340721EF63580990BB8FC0D24112779C4A450E109E5835004380CAA9F736DD2
      4D1AD0E5435F1FA3B6AD9B5ADEDCA0903A61E9E956894DDC83F5FAE3F35BFA64
      3A22E3C2246280B3B271597A96F7E939B96A1D7FD759ED429C3E691B6C6CD6CC
      5F57BD818FA6474D0FF5970106D9E4F98793986675246424450E57858BF717C7
      381F852DBA81877256731F5DFB50A352EDAC94966AA542895129A10093DC3DCA
      5539CC5B2799B4E448A2274B2A41B66CCC51110A89A80DD3CBFAD6F7D4367452
      48AC36BB6815B1C266C1A29BD54EA24CC20B0685A0F435C8BBD9ACF6FB4B0E3E
      5A2BA94850E6362B0573B051D9D44323276C3D94B2D33D18DAD9B1F8674716B8
      E252BB5521E12D7AA5C4A49109ED0A65C2F6975AAE8CE195EDDF6160784D4FC3
      8000939A07890D83680E233F0E64100CD4882745A90B3C27C92B2AE53232728F
      BC91C8B75D7FEF2A66DD074138482211A0FAB12F08222C4CA129723CC7484491
      38ECFE09189316A5A5E6EC7F2391679BBE0A4FBDB24DB88BC649291120111306
      F2C040240E6A8A18302D923DCB2C7CF2EC69D2C63712F18D7A94A20C02FD0CCA
      EAEE5A851DD943660807BD0088818318D0E3BC3425CF624F4CCDFD233F3B77E7
      1B0B3B29F189E460666A81A5B890FBA87EB82AD049830D4D8E15B4237BBCC849
      53F32DFCDD67D967F2B2F37FD7CA14B910D6F1B07F2E833F575251EE200CEE3F
      91199D3D3AC6C5E475AAEA6B336B95723976D10A71D405AAB538292DFF5C6166
      EE5265A9E5E82703EA955426EAFCB19E4A2D3DF0C6E9144EA3F77136FBA6D4F4
      0FF8C5C7DD2549A5904728048913F4708EBDA4F4365F5C72586DB7DE0609DB53
      A93015ED38464F053903A9A4E0634EA5F374F30D4EAA1118FA6394B3CBDA2C51
      C8D00A9C0227EA646847562CF216E1854FCBB481F550F6FF6486A8F70C2535EF
      378A6A7548A2FA5DECAEBD3E49EC347FF327D3BE3FC1D44BC598EF707F30D540
      1D3E1C418DBA3FE41A76B7BAF498F8BC6DFCCFD387CCFAC1ED1DA95E0BE249A1
      EE01DF49BC5AEF83EC24C68545AF8930BBAFFB76E6C014F8FFD78697EA5CFC25
      7A27AD9B775052CDB0AAAB235D5CD67C31A2051BAEFCD7242C221FE617B9B96E
      58D40F8D422357557533FFF8F9E8D6954858A7606091DF05A621D35F45F81EF6
      1EA895E3D0C755E1D6C37D026EF673A385FB24DCD5E0AF79E5C61892EAE37E26
      908FDA2427381802618700CC78119584614025C586560010F1CAED8BC0208019
      1F5CC20096D6C4C33113D8098CC693BE85CDC6877BDDF48A1F89C8D34DAF4A06
      98C27783FD07E2FC48EC6F38C7D24C817305B08811FD09C77E44486663C88D5B
      36687EDAF4835B0F1A1610C1C5D4D491A195C42E6D8831A419F1587C58AFCC70
      2E1FAE43C0517E9FCFFE677F78EDBD78F4E8D1561843F6327B9A8768CDDAD131
      A6B8B15594D5BBB808EE553D78EFF0E0E2A86A3EE9C1FE6EBF7206645340E297
      6638B70B8EF3FC117BD8CC5FECF5BEC41AE3700C9CFA969414B74BCE7F56E776
      CEF5E0B4DC1493BA40AB752A76F772B37B76F4B5074D8C79D8B06795E30DC2F0
      AA900FE0860AE806A256BC516A33FA9B143ABC6812BD7BF7EE2A27FF3A1DFAF8
      645268DAD99CD0E2F314AABB620E555D710AB55F9755533E30B6F44B0B9B184C
      515BF0DBBF1FCDE2574969812D509A532CCC1ADEAD41BB603561D9DDBD69D3A6
      8FF1464A56685848729E262B2FC3F379AADDD5922D7A5AD2A4CE42769588AA49
      AE120FAB5AA9B66360968945298F2625E78E37E2EF5E94AC7AA4E5852C3ED01B
      AB0C580F14B1FCEE82C52573749D2A77234CD1963BB7EE98C44C5E17161BFC30
      5016CA9D797CCE0BA3DF428CEEA0C639DD797D838FAEE518E582BCE9288D8B5A
      1E664CB9684061E762454F89619DDCDBDD3B47ABD659521FA7B938051B9EBB70
      66D19641D2C4A789AE18B57178071203BB224DB2E09C995C2CFCCDB74BDFFA47
      838CBD4B21663693C8DFDF3F152FEA5C71377B14061882ECA131C14F9DB52E39
      3909F992C78F1E33299EE32DC33C944F0E0A995CEFEEBCEDB177E26D363E6AAA
      5991390BAFFE2CC7225C02964E9330BC4BC23AA303C33E1C96346068FF24ACD1
      26F5EDDB3709ABA749D81349C21B63CFF0C0A488B1DF9D12163EE92E50877837
      0BAF50BA5F5CBB0CA3B1BCB8B83815960CD3030202525C5C5C52ECA5F6149D5A
      978201568A52A94CC1F83A05D2A4207B29D84A7B9C24F5C8CDD2875C60D29503
      AFC2B8AD58B1E2B383070F6E3F8001E9A64D9B76634B880DFF76E305273610DD
      85CD989D18026EC04BCC5FE275AC61680631D470A4D6913534AAC98C0D433CF6
      A66F00962866A25D6D6783D1B973E7EEC268968D2137A301AEC186C2545F5FDF
      0E58960EC30A30C690C57D90BE1923FA128E83400023C31892C31BBBC18B162D
      5A8831E426368684DF22BC96D70FADBF965C2E67634809E232156442BA9F80CF
      395C447832C33ADF2174DECF259F3FEB61B4A4B6883B35EDE0D9B0112DB395EE
      C5F67971533182ED4C528533E27C85747D90A80DD00B709C197D0007337FE2B2
      1720ABCEE36C5A89EE26CAE9A1BDD697D748EF26C32278612E89A7118EB10EAE
      44CC5D0A574DA08049E483273CC5CD4BB3461C03C74CE02E3008D00250709406
      7B35C0B4E41748B310EE9706C786585E05C71D6E606B000500AD67F78496645A
      106EC779001991CC8B4860F72A648DC52D00A1C8C023D10B78B29FEE48B83F04
      36008D24FCB50FA0111F024F80DBC003817FD09D8501DF01C3811A48CB38B054
      3F9C2B86871268A220EBFE0D4E1BB37FD2FF786FB476557E2857B59696F4AD04
      BB84CDB25D9DB561B7F0F4E3C013C4DF033400585A10AD1127E2E6B3D16E8FF3
      0F459C8D72F7701BA231AB47471B633F8A52567BCF453057F5E47CC3838BA2A2
      3D33FD7DDD7E23BD43430EE7581932ED380D527DCAC41A09A24FCC4E86A1D045
      5D30CB6E9A27CFAA769FBFE995C6252B5512B5D4A4707675D1B9B50ED4848DAE
      97D5AA7D9D47ADCC83070F969228FA11D127C018DE53C84F7657DA0B429C6436
      F423C72CDB80F22CB417509E3DC731CB96F252AE50C853BB69DC031B9A9ABFD7
      40DD623C1EDACEB9E8A99AB7953C04D1457EB2F3D5A3F111D9579BB91667B359
      21869EE4267852A03C149BBED869E7049262966D149CC9970FE2DC65DE721F0F
      DFE0A0A0A07ED3687BA35A594777B9A5FC3D83BF9949C51753AC5ABCE4ED9865
      A3FF60CA2EA1607904452962E86AD6A7619733E7465695C752A02284249C94ED
      6EB1F7B7D93B024124DA6BE7E6E47A088AFAFDA56693BE460D1D36982DECED04
      9E30A1011912E020FCCD02573D3E7C213696156470B91262D37A36BBBE73E70E
      C75EB07C2A3A29D20BED273897AF13439B1AB2264F8B2139DE42D09562CACEB2
      C8C8F2ADB9742EF514E567E49326CB48F959050455EC981443CFB2C9B278535B
      5D7D47193E4B92A6F05AB6BDC8B5269DBFB6755234C75EA4502392E39DD1DCBC
      5C2ABD8721318608A9F9692CA163968D7D37C7DA40A236DC703368506C89443D
      9F7591200B2733ECC9F73BDDFCF20E15346353E86EEC895BB8FCBC7C2A65AF2B
      9472A89897063FA2ECAD7A11FB4694C5F35422D3291112C8F4D16034A8F1B4DA
      1A8C3D104F28B578BCE2E9D090D879DF8D17BC1DB36CA6213FFEF86336CBDEF9
      DE7BEFADC7B6C617DE2DFA4FC0547D1A7D631901329835A21A6413252BF21BE0
      6DF0E0B56BD7CE8386DC81D93653B365B3ECCD50B1AB6263632743A7B7930EDF
      D88396A4CEA7E957F460806124B040E40B4CE05659C22267FF356BD0EA3F0FCD
      9F3F7F13B60A7FC40EDF02ACCCF4910DFC7E1C37F9E468AC0856A355B6B15878
      08614919581771658E571D71B5C84B3BDC746B3AE3577B9DDAEB7223B3B7E8DE
      13F6C5CC96DDEA7F28DF52B3DF476260BD15A432BE8FB9C43A4CB8EE3BD2E2C2
      149B174892E07E69D68843E09802DCA1E43B1F91D2A02095E3304F0E49644B89
      E399625B81345F234EB9614406DC31C9721168859B9DFB631AB136DC85C08DD4
      71F8100B1CAF99974BAB1AF8DBFFA957DC95199C2D92C0DD1BE809FC0EEC0659
      2AECB71A26C99B026DF07C0A04037380E9207787FD56C3BF29044F17E1FF1478
      0CB802D1801A70181CB192E1A89513C8E50E0F5C5819695036F97097958D0CEE
      42400FCC43DE078944C7E0FE089002AC87F0B05B61827A09F6493CB89479943F
      099E2C7177D8B198FC5A801B70E701AC1B74863D1758018289C030B847039190
      8C6344C5B8219CB0E325A2AD086E3F2262096AC3BE0B69AEC376054621721BA0
      3EDC0D0196AD26B0BB001A1ED9CA0189013783EEE62E6681C7E16659E80EBB3A
      F01848E6885C003920053CE05704F040154023ECEFD3471664320DC58DA78C6C
      C7BE50344E20A2CB101DC320C751CF58DCB35A3483400060889D1E7F06FFCF81
      43C03D497D1F1F3F228A0256032E2939B3E438A37D19EE04945120ECEA20654F
      4E1489581635F0632617973F804414B65DA290483C716301528021C003E03EA0
      46C2EB78FC13B883417ACA4EC4B288610D25C1EF3B208591C026FE8ACCB1AB89
      386C359DFC2EF11E4F11D014306351E518A4D90E770920E1B0360F5B013C0718
      9994D518DCC43F15F4057048013590EC26E6556F5F7A2BC7C59E7F1B4F4B84DF
      01E034701638016215ECAEC05A6024E0C8AA2487975FC14D4B2012D8E329E68D
      FCAEF0F750B8AF507CFC3A3738608CC00BE008A003FA43BA1091A819DC1B803C
      DC43DEF8F87AB81906AC079201139001DC70D5CDF025E27E81244F703F1E5000
      1F03FD806BC06048FE8887839933B8FC007403DE07BC8128A08A8932D3606F04
      4C0013F029ECC5C0AF40092003FE598940A3E4E0E10B3426222F4002FC099C42
      736059EB899ABB2E129D4C1FCFB137350210D616D80F891EC07ECDB003A91210
      CB8E0F1ECCCF9D33CB73F682CF83DA2FDE55BBED975B5A7658F24BF0FC797302
      472CF82578D882DFAA0D5DB02162FEBC793ED4F09B6B7CFF55879593BE3D21AF
      4889DF2FB556A309C031A21A387615834FCDF8615CA0A818E73FBA315570C587
      4986F4EDD367FDEC59B34EE27DFFE3987BFCECE7E7D71564DA3711F0FFF69408
      82BE5A7474FF8FC68C99848F04741E317264CD51A346D5FEE4934FBA0C193C78
      4A604040278C0D34FF4E578988C3F93AA3C914D7BE4387EEEF77E9E28FAFC728
      B0622CC13C4E8291BFB25BF7EE512D5BB5EA83F94724E2B2CA29E7AB4404B165
      F8F046749DDAB57D305DC0B2A5C0E23B8032E3BC3C3DA5988B84224E28E2B25A
      7D331152483059D1E90D0619DCE591CA1C8244C2998C46254E926940544908A1
      2C12B391989D64A98A233175F05425A47078B30B20E2188C78F9D2A58C23478F
      1EC4218BEB184FD9E1EF3095F2091FC1C5D9B9D9F8091316E1373F0AD2E1C54C
      E800042091886F67947CB964C9C98D9B36CDC080EB2CBCD13E7185F93711A146
      8CF8DAC9A85123478E68DDBAB559A7D73BA4C69CD7B6FEB7DFEE61D8F3F98387
      0FB7610C958BF4EF36D859F06BDFAEDDCAFDFBF6A563F466C5A0CABA7DDBF634
      CC1AA76348C8BACBBB09CA42515682BBD9DC64C2F8F1C730E02CB97BF76EF187
      A347FF856650AD2CCE7F6D6367CB54B366CD78CCD9D2F18D9F6464770AA47963
      ABFE4FA4020ABBC5ACF8F81BD33EFDF4ACD9CDAD2924AD54E5FF89A03C1C12F8
      77EAD8717FC70E1D36607A11501EF03F75A01D19424342D68504052D60EEFF69
      FA8AF15532A97401301E9E4AE07F6D981AED8FD4EF0195FA16EE2B1B8C5C5F6B
      942CC61766B3305FAF5761A2EF172D93790F512894A3A55247E364E1AF638DC8
      26BA0CE54F4479B0AFE7A89D9D9CBC31A60EC7CA4304D649FC70CE4D8F96CF97
      93AC11790CA959DAD7B38D9A62242AE8A20868C5569F4C9DDA65FA679F75EDD7
      B76FDB986AD56A40BDB08398AFE5E21FF6578FC19285D2DFDF3F10C76423C77C
      F8A1D7C041838CFD070C306047D01DEEB0A8C8C87034050D7BE0AB240EAB1291
      52A9E4D1CF9CB09B17806D415D54952A3CD409876FD471E111117C9BD6AD952D
      5AB6F4414335A3E5572AAFF27261B4281BC1D5C5450FB5AAC19C1587A0958EED
      56160645462627673204D4702E8D16EB17E8E2E248E98C03DC8E8DF107952442
      17605F3893621D04E7B765C412C30FE7B489BB9458AC9DB12FB3C6EA1BA68E59
      C6D8B624D53626516C8D87F4051AFD5368C32E70EAAD9DA5213E2EC1D8A38EC6
      5A113B6DC873BC409BCE677A7C7B32A3764EA145292D4E4D48BA7E626F6AA9FA
      84CDA791155FA3348188FDACC36266D8055EEE1224E0C7D00B3B9F1DFFFEFBEF
      4198BF0E5B7F36754683C5B70ED49E7FFDCFCFD65F9D3F79DAEC5E28B7202C95
      552A16465109F8948A1253AAC8E9F1737B2CDA767D56FD85370F345C7473DF2F
      87EF4E5EB5E6DB7E8D1A36AC8DAC57AEB561172A15BC8310D5CFE95D3C359E1D
      E2BBF98D3D72B0D6DCAB07B79E4E98B26DEBB6011D3B746C0A85E78CEAAF54B6
      34EC02EB4A8EF4952E5CF7EDC15CBF3FBF721EBA77CF8C9F4F4DBF78F9EAD029
      932777C5D67C302466A3BB4AF1418471C3B00B8170340602005FA0230DF97B19
      0D3AF9A54FDB299DBEF87269BF8D1B370EC03A6E437C1F4A8FB2F9A782CAE886
      5D70107540E26F802DC076E05B1A7AFE63EAB3DF07A70CBC060E18D069F2A449
      3DD1E7C2D0D75E978691BD22D2207114D00A680DD400D42C1CC31A43C3060D9A
      E1EB399D51C0EEE81695CB864562701031C75B809EAEF2F1F6AEEDEFE7D7422A
      95EAD1405FCF164BFBB6C26661AF20C330A71A501FF7AFAB0A783A0C1AB3C37E
      C785B58F008447026F2E1F04FCB74681882AE0CDD942003302341C477B6631F7
      4BAC11255C6C37A5CCC94BE59C715D8B9AC35B30262D0A5A65AB3F42616D3E5B
      478D3E75A12A03BDA8C630032007FEB5E3072A24604739341884FA4657ADEA05
      25A7C6061D773FE145C9E1DC2045BAA1A633C9B546CCFF9DB08050822439C011
      1E974A06CD5F8565AFA0DEBD7A457E38668C67FF01038D355AF7F129AC3AA4A3
      D63FAEB7939AEAC605190D5D1A856ABA360A738AF0750E00019B59C27A65A021
      058C0F5D70D0D4AF5DFBF65A2838FE69A9C9B4E186105720750E691813C8776D
      10E212E5A3F772D62A3917AC75B14F402039A6A2B8961987867475D5E3D88706
      DB15DCCD14BBEE8733D9D119459C7B6C880717ECE7A5D769D4325EB4C945BB8D
      2F4BC76C1EADB8BC5A5983436B966AB53AFE5AB24DBFE258464C4ABEE8111BE6
      CE3BE9F1720E472460031B6FDA3112DC318A97A874A3D168A4E8E1E14D7B8D6F
      90A0A8563BB354EE1317EAC119B578FD45C41299CD2AE6E4E6D932F34BB20495
      53962095D92EDC7D41E7EFBEC8E520116BB106F0A6F10FF6A2C6E4F55DBD827B
      BAF90404D58F0E90B818D408621F9C14D9229D3DE9454ADED5C79937AD722701
      EFCE1424A5E74993330B648CA81562B2F99A85EC364ECEDB0CF88287B945753F
      D7201F378C1FD8EC9C23BC6C229EBF7A37FB724276415A91F43149F0F9060EEF
      0D721C536A570434A6FE26ADD2E067D66B9DF52A5D74B087101BE1C3E3BB0BD8
      E6B7C9A41229E5E417149DBA9620FC7D3B3129B7A0E4882851EEC14FCC21E285
      8B10E20E709D90B5B9D51625AE19F587E8C0C83DF635237695AE19BC29F39711
      EB13F78FFAEDE9AEFAF32EED310FDEFEA3A6F59C66B25A23589741DACA86957E
      251FF63BC661A58393288AEE3C4E4E3C72F9A13D373335CB90B477B3F4E6FA0B
      FCD51F596BAE94E6E5CDB00B952462920DDF6D5BD568D9F3595CDF83CBBC876C
      F9B1E798D943F01314F2560DE9607A0351ABB51933A5232ECEA13E076647B5FF
      68203E19D649AFD79BD10F5FCB8183C3711976E1D3B0398FD7F4D858E840BD65
      C95FC9465D5E4843FF1EC7B55B19E2E3EBDFD0DFD7B72934A48E3558479A375E
      865D884581CF065602AB8079400FFC003821BE1CDA3116A803376B6FB0DE6C38
      2462EDC08F5E2EA0C0A20C5C1ED1DAD812D8EC67D9FF95FD007629F0BF364C92
      FFA821B937D1E3671B6F93C914502BDA71E33EDE181C121287510726AA9851DA
      7128E25F89B09D98C0236B956A023583979F945A5F3FBF60BC3E545DADD13937
      6EDA42DDB8594B55BD86CD94751BB752D76DD24A5DAF696B75FD666D1CB6CE68
      523212267AF93398860C0F0F0FC6E0331CE7B53C940A85947DBC442197924A21
      25A6C854CC0D2865124228BAA88D586132320711B2C234A41B5EE0F26BDBAE1D
      D3DBDCBD7B77B1409EEBF88E3666DCF4F0FE3DB2DAAC50D72F4BC5C3C39D9061
      079183845DF0CB2AC12CC8000DA9C2DC0C2A851DCCE07082AE948AF1AA99C552
      8AEFD9E662B9FE25115B902C717626022793887138C01A1C1BB6006CBB021D9C
      2776F0A8081FC24D4F4F252D3EAD9298F48C6C561BCEA740D32195938B19570E
      87096B0C93D3C5B516DCB16D0C5E833D707CD6D205CB1712D41E77F3F69DBA21
      C1214ED89D20BD4E4F1CCEB06BB56A321AF4B8D7929B9B0B3D79FA348B35482D
      1A5F1E2362658491BD37F66163F1C1443DA60E024E834FC05C3684BD86862133
      E11D5264CDC6A263508BAF360505D1C913271E33222988CA5B2CB2A58E8C8C0C
      EBD5B367287E9254274E9C98846968C8FDFBF709AF7E105E374399591D248C0D
      EFDBD0B5AB571FF31549580036BB8B1EE16FF79E3D4FCF9C3E8D6F95DB44484A
      5E5EDEA43718C9D3D3833CF13E9B9FAF2FF9F8C04FCF96DCF0EBC21257047684
      4528372B08ED689C267CB1B4595818FBB60E915E6FA0ACAC74C22F12D60704F6
      D3444E980D24243CC946C555A4F9C78DF9863A243838AA63A74E8B506E4120C7
      6B90820536DEF87C998CD51BEE09AF8826BDF4F9277DB90B834E01DF2FF4E9D2
      A54B1C7E81B93D7FFC7159A9543EC1E744AD2C71794438D0FE18275C6F30F81D
      E240E4D4AC69D3762D5AB4680B0DE90AE556DE0BDE90E4ED5E48A8F1F3F1690C
      34825BFBF698FF398469C85AD0903511F58D3F43F0FFAF0CEB426188190A940F
      36E0FE1F1B56194CCD300DF9CEC43CB6E35F8E125E45DBFFF9E7C2B68103DDF6
      B66BD76867CB96C3B6B46FFFC92F6DDA4C5EDBA8D190251111B5261A0CC671D2
      D7A7EDAC16D8531D34EB172C50864546C6C4B6683138AC53A7F1517DFA0C891D
      38F0FD3A030776A9D3ABD7D0BA5DBA8C6BDDB265EF66356B46C68787B31F0D47
      3A76615F836536FDD2B9B3B68E46D3D248F4A1DAC5A58B292222D42D2A4AE11A
      1252EA1E1262F5AB5A55155CB366D5F066CD7AD768D366CCC0BE7DEBDD5AB346
      EE488C0B938866393949C224923A3ABBBD97BDA0205A2C2E56D98A8A128B39EE
      64B142718001EF1A9F84A67BC426BB72A532C6A4D1F4D6E7E757DDD0AE1DAB10
      921417167241987F1ADDDC5AE0ED5E1F106033C2C6152427E7E3DC7092292020
      1D3B9B642F2DE57378FE49E6FDFB41BA8C8C40BDDD1ECCE7E7B730592CCF20D0
      7389F4C50B99BFA76775854A155E9A9F5F58505A5AA87576F65048244139376E
      5873A5D28B4ED5AB67E1BD6C9BD46CCE782893E5A9718CA58A545AD39A9B5B5D
      53547471B18B4BAAC4989EAED3299591F8A2AD3A2F39F9E299BCBC47713131AD
      DD349A10BD2044165FB9C2156934E7E54141ECA48B3DB1B030E7E1FDFB0F03DD
      DD9D51C061128E8B2495EA2CAD6DD5AACAE5B66D973EEAD3E7D0959E3D87B489
      8AAAFAC39429939EAD5E7D2863E5CABBE9CB975FCFDEB1637DF6A347532E9E3F
      3F04EB931D63BCBD832F346F3EF141EBD6C7CFD6A9B37881BFBF99D6366C58E3
      5CFDFAABAFB76EBDEFAF264D3AFBA077B66ED4A8EED659B3A6257CF5D5DEE479
      F36EA72C5972FDD94F3FFDFEFDB4691FE223AD550D6AB5EE485CDCC0EBF5EA1D
      3B1113B362B6B7B717ADAC5327FA4854D4F2B37171070E56ABD66BA45AADC44F
      9253A7366D9AEDFAFCF3A90FE2E3F7DE1F36ECEEB53E7D6FEE68DC78D91467E7
      B02E12897A5F78F898B3D1D1C70E85862E996D36BBD392FAF583FEF0F38BFF2B
      2CECF0DEF0F04F977A7A3A436F4BA19F033E1E3DBACBB955AB665F1C30E0CF93
      4D9BDE3C5CA3C65F1BBCBD3F5CA1D355DF1310F0E55FC1C14777FBFA4E5DE0EC
      6CA4C55DBB3A6D7475EDB9D3D373DFDEA0A04D076BD468FD478B162A68456DDD
      3A75EAFCBC6EDDE04DE3C6CDD858ADDACF0723228EED0A08D8BFDEC363DD364F
      CF033B3D3C766E7675EDBCC209DFEE8B7176E6BED4EB437E3599BED9E2E979FC
      70FDFAEB6E4C9EDC64F5800126348B60ACB4F7183D6448F70FBDBDDB6EF6F2FA
      1A714E6D72733BBBD1D9F904D2CC5B6B34FAA31DBD3413B55AC51ABDBEDBF77A
      FDAEF5BEBE27FE6AD7EEFBB3DDBA0D5C101CDCA2578D1A433EC0D2FD28A5B2ED
      2A8361F18F46E3C91FF4FA73DF69B51B976934CD3E56ABCBBB89836DA9C9E4BC
      52A71BF08D5ABD69ADC974EC270F8F436B9C9C76CC35180ECCD2E9F67DA152ED
      58AE541E59A1541E07D62F57A9DE8F57AB5F5BFB7790C51B0CFAC56A75F32FE4
      F2855FC8643B17CB6427E6C864D7664BA557164AA527E74BA5BB66C964F3E72A
      954D676B3495D48F83A0E265AC46239BAC50042041FBCFA4D2B11F4A24CB470A
      C2F2D18230ADBF20F4EC2C08E15DE472A6EC2A267BB3BB2E3EE9D0552693B611
      045D1CC779C7709C6F0D8E73092252E317C0A131DE9CF295EF945D22C79C2E53
      8A058FE9E224D327E23876EF40C3A5EF24A814B8A82327B6C5467DB059DE70E9
      FB347D681DFA989AAD0B6544EDFB8CDDD26E8D78563349ACC5EE3B7F2B564A5B
      E9267A91A8DA3B82B35B6CA4C227FEB4BD6B904FB7DE83B6F6FE593C32A436BD
      D7AB06D52A7C7C740623DA31047335E6780561E95151D27E50BCE8DA3C3EBE43
      24ADF46C155FB59E3FF5097426B7F402B2477B716E41CEE48771686906361977
      1D387E71EBEF9B8E6CFEF3962F8574CFA667471D3FD7928F1B73561A5DECBD71
      B07C46CB50E29A8550706E31D1C30C12B5721264025959C1A5E793440D753F7E
      64BF8E871F52E3AED3B69AB7FCBEF96308B4BCFD1A9B2050AFCB92A12DBD2CE7
      12A8367B324E4D97DE4F232EC24C9C468EB78F4A4970D5128FC78A0516E2BD8C
      24F3339136D495B8DD89FE4129C3FFDA3074E8B002F63090C2345AD6EAF31963
      F6804030E0732EA9F9C40FDD4862466E4942A350B9764C037246D9894518DB01
      B6EFCE92E4D2893D8F685787D6487D5F42839E767AAFAEF7A07A01D428D69BF8
      943C12FD9D88E61E247BFA3721DF53F6FDAF6BEFB488BF5F91AEEA5885EAB389
      2E120A97FE3E9D497BBB2D80FB3E173918734E9B65E8FBD1D4B17315D2C3D3B1
      9C5A8831AE971E3B8E12D57EF8DD58D849761359B98FFFE7C28E9D8B28371ADE
      B5AE4E3232A327450E0E156F7E27727CD7A3BD27F66DB4061F1564B30332225B
      F841C6F08FF85FCED3835ABEF403EE03740A1A8033FEBC4242226A93C307084B
      B75F23D9CE6FC62FA3CB5F8D7594916468E2229BA0EEE4ED66F05CD195536715
      A12FA086900D2E2DDF31B0779C5F67B596594874E03651AB702210D1D105F5BF
      A2E7A7C64BA8D132C1BACE7B0A450CD81838F2FB831C476A258E1EB11AC253EC
      CC8D4C9204634F64594CCD236EEFE9FBF97B775EBA4185299920F903D947CD8C
      1C2369B642E434ED7EE839B836E79C857FDF83EAE7593994DA8807247925243C
      487364C9C6D65AF8D2EC44DAD7A33B1D1BDB9124CAC38C88739B6693A7CC134A
      F8C14F874FECE8BDFACF7B44971E6451AFBA46AA1FF0F23323DF9F250EB54953
      9BE3933B52B24DD945F6B425C61654927D0C241C8066E6518F39883E3829A5F7
      FFFA9E3AEC7A2A891E3E9562C6C54FDA29DA27EF14ADD4FD6C2EF9B4F88E1F9E
      7E62C06FC868E7033692E9DE03013395FA2BF360609E21CC414D569A43E78956
      FD14240CEFB75F2C2D52C25F43D5272E2543F03464C9D15CE0F70EE3DB9AA3D8
      A93C355DBD806A7E768C64FAAEEF88FD9620A52B93EACD81023BA481A9196B11
      12FC37A537C77AE9DBE76C350751CF53553E1E71AD415AE7636109717F18BEF6
      DEC8FBBC8CF1E6AB2351C5204FEC197DF2A40D6F767571F3F7F274AE1711E7DE
      36BCE5E09AE62AC7CC43144D58DC98EFD9C90FE6FA2F30EA568339F3D2DEB77F
      99D7B3F8CBB4C1B6667B0245A776F2892CA947BFCA1FA9627EE5187AB58E43BA
      21D76A8EF9E84ED3E3236ED64F98F5A293B828B38B35FE698FD2E8CD669B5333
      C50896C0A50DDED0618E37A1EFB918B6524CAD77F9CF9E91D8515C92DE57FC2C
      B1AD7D415A67FBE4075DAC6E3FA8EC866E92296F4ACBFC1C8999E3DAB7C9689D
      4481439CCEE4E5E675552A0467B420BB28E2A54E5EC1F9985C44FF38635D9701
      AA56C2704EA6182DB994B7C6C2923AC039AEAF2EED8F844BC552B2A63D2F6C5B
      A59AEB76B3DEC4E3ED204E8ACD70834A27EA1446FED0956BB475F1A9B30BA74F
      6F3131764EFEABA4E55625C263E74E7A74DB5FE7CEB8FB0D91ADE6D6F8C4F6F6
      E949ED6C75F6F85BA82EFD34B5F734CF5729CB7354EE4000F7F9A2CF9B4E9EFC
      E9748DA05A2ACF74F6B8C21F235F6717FE595E3A5DBA97CEF511C68BED4DED0B
      03A2FCD37CBD7D9F9CFEFB7431D2398C8308EFAB85E25CF691E62D9A4F4A4878
      14F3F7E5B3AA8C5BB97CBA35877BACBFC7E53D3572F1BE5F53F380667C60F540
      1FB956DE45A955F6C58242FE25FC6DDBB64D9030BA8888082FCCF5A3B0045D8A
      D7ED28E95992902E3EE18293AB53DEB960AAA9A94D8281A72C5D263E5FACB215
      17165B535352BDB0E3DE1BE9D76115DEEE289B59B36635C00ACD31A3D168C5D8
      51C01E08FFE0C17DF1DAD5EB64C9B5707A379D1810148857842474E1E205F64F
      70AC5860C14ACEBDC3F81C542B90393A20E13B3472CC9C87633746860302B6FC
      FC7C8E6D1786848690D1CD88C5032FEED1C3471CDE4824CC1E44994C2660BF5F
      C07F64BA8AC3049B4F9F3ECD0978AB909B3A756A064E189EBA7AF56A38D63FBC
      D9A238F6F96D32A90C2F50F1DC86F51B084B1AF87A114F0216B45353538BB1FF
      B60D044B20CD33FC1B1FB6F203E72BD3B97367369C1B8BB9FDA4060D1A18406A
      07B9B87EFD7AB6E5CAFE851CCBD605BCDEB812E7237E47B202A0B2419E1D7D8D
      F9A2F0AAC4C7C76FC34955F1D6AD5B224ED88BFDFBF77F161212B200E1414099
      719471D9CDBFED72C2BA75EBF6C51B99D7EAD5ABB713919A02EF4C88F0CA66F3
      E6CD3C7C1860911C978AC3E0327F78FF07933333C2D150D37A9BA3D306989F9F
      EDE8FC3D4B223E6DF15689DEC8CE4776B6A7CD8E93CAA3FDBF32F56A66F6F250
      775CDEC12396F339246E68667A23D96B44D93F7414B4DDE68BB2829C65AA20EF
      26BC019F4FAB11E6149E5D38FFE9B2704995A65EFF99A86056146F18B8CB96B7
      A0CA287574E408819797DA1333A49C8FB7B5664DFF162BA7DFEE1F35ED9AFD5E
      0797D7C8CA2512BFA9CDA967DEB05BBF8E6EA3F6F05B21C8B4563BFEAF165760
      E5EC79259C362250ECDFD07BD6BC20A957C8EE34871264E5568672E6C4793578
      EF6917ED4FE2C3E22572FD441793592D91C844C727D159328DD2567A3D49F2FB
      8EBFBFED73BB60E8ED79515CF8B41B2CC4C1552E112341A314B8EA31B3ADA1DD
      639EDEB9B78F8AB14C5864B751898DB8420B2F0BF2B2358F0D1B30C643D59A91
      1CACFF4F16CB881C92A1E3DA7C3A6EB06B922E66288CEE2C6B2496222833930A
      8E9CE5D21F3D15F5323B5F4B523C5CDCF1BD50B77D74597A9238E42262227238
      68D124B26ECB3E6754EA4E5117979A84943B76BB4485C19E88FFC868179F44F5
      1712849B85BFFA2A73AA36EB6BACDA7950FAABF42F899886843EDA8263535157
      2F5DA4AB771FD0E302BBD8C8CEF1A1B66451A62E248949CEB9C435123DDAF750
      55E9F7BCFFD5BD5B9B8E1A3664CE376BBF5DC734A443B48A1A322024B454AF55
      D96F156AE9375943F177B70674A6D497AE460E1139BCAB2D29C8C1A1BDB4D2F4
      8C4CEF948C2CA621B15EEBF6760DF9101AF2EAD56B945150CCB97A7889E121C1
      C45B0AE9E2C58B4C95FCF71AD2C3D393C3AB2EE4E16C225FB30BF7E0CE4DEEC8
      D163FF3B0D8925436C23F1DCFA0D1BE9FE8307FFBF862C6B59B0FF571AF2D0E1
      23E80344F367CFE67FF9E1FB114B977DFDC7E2A55F5D99BB68D1D9E99F4C5D37
      67E68C5870137ED21DF198FB4DE05B346D221EDCBBD7D5DDCBEB70B15AB7CCAB
      7AE31AFE75BBE83C6B7775D7D768DECB22551F1DD2AFF7474B962C11BB74E9FA
      5632FE976FD7495FA4A6FD2AF7F6ADD3A879EBC716D333DB99DC1F74C7B2BE93
      DA5D5C1F6BEA764E730DABFE7593DA357A6FDDBA05FFD86DDA1BC978855CD6B5
      40A16E1259B56642B04EEDF424F79681824F39DDB36F34E7A7E5CAD572B77C5D
      F55669C155E3A6B76B58D76DEEDC79AC83BF963BFE69466667B54B60EEC594E3
      EA2FFE5EAE7898734F70D5388B0A99CA7E3563ABE1AFFB8B5C2D3279814B6875
      DFF3972F36610C182370CCAE08A171CB961F69BC6A684E676E5196061ED3FBF8
      4905ABA5847377D672E69014F5F9C4BD6A776BC7020D27C89FDD3C7333392DF3
      047B49A52289C33D67D1A23F971FB8FFA2F7D6692F1A6D0FB736DD1369FDEC49
      7B7BDB3FC36DB57EF3B7067EE16399FEC7A567F16BB715057838CF74247AC385
      9B3665D297CA6A2D47599C5D1F956614CA2EE7AC37C6B5BD63DA7BFCAECDFDD9
      C26751FEFE82B3D95BF274DFF78A65F3670C01C716E035C37385D9DF173FBE62
      259B42A575092D91081ADB816377EC99CF457BA8AFAFE0EE5BD55A9AFC5873EB
      E481D490F0F022FF5A0D1571D5AB3BF458453647A10DEDD7EB637D50CC57F298
      B62F8A7849717E76BA548B7FC2EA6CC2D72B531E6B6D770E6BB03353929B53C8
      5FD9B571D6B77F1E9FF7D5EA9FF97123FAD9CBC8789C79E0D6FDBC7E29979130
      3C7DEF3716D99D13AEE6926CAD3AED9932F9CF9FE57F2E9FFCDCC9682868DAA1
      ABB48AF689F453E7BB9F1EF6A62E8CE4FC8AE0D7242B230E88F0F3185DBB7A95
      655141BE5F2AF042AC87BB77BD798B97661EF8F65331FF58778BB8B3AB983FC8
      AFF8742CDF8D25BAE6F972F8C8DC0EE037AD123BEE1DD94E0CF3E03EEDD868D2
      C5F6AEB9E2A68EA2F5EAC8127177575BC1B0A09253F80F198EC46FB87078D5C5
      41C0C2305C2E771F08A76619DDDCD3C5F5ED45EBB5511671CFFB62EAA830EB37
      B3FA2EFE6E429CFEFF69EF4BE0A328B6F54F77CF9A6426FBBE90006109040241
      08984812F6800A48101182901094ED8A784114BD2C022A2E4F1031C8268A0A08
      EA7361316A58BC8A0664934D59644B581248802CB3F5FB4E4F66C8BE78FDFFDF
      7DEF39BFFAAA4E9D3AF54D7575F7999EAEAE6AA72157AC0B17FB1AC5E01DC5B6
      ADEE141FDBD373B3EFB06E7E7254B0E9947CAF6AC59623A58B17CDEE5B57DD1A
      FA6BFFE8A26CFAE1411D7B5CB8CFE3E2D1CF97CA393F1D92478E187E0CBFAFA1
      352AD4A75834E739E54AEED9CC71A3663C35BBF8C10787FF1C1919B9183FF5AE
      D5EB89F8D5F5ACAEAC9C778D1F200E1F31C207BA9E403811F1FD00A22A1E72F5
      AA475F5FB2F48B975F5FFAF3C297167FFF873C64003C64B99BE7129F8E7775F1
      EED213F3C792438D9D93479A344DF09097E0215DC222BAF7E8997036A6554449
      731F8305EF10B9A90B083FEDD6EDBEABFE8DF2903ACDB052AD6B52F3D8B8B35E
      7E3EBE9810AF2B76F5B4983D7DA9D4C5A02A317ADF36C40E68D8435EBC5A3858
      086A516CC8CD71552F5FA87159F7A2ABEFDA173C43D6BFE41DBEFE156FFFD58B
      FC645173DBBB0D3CE4FE7D49E85CAACD430A8B17BFBC4B4C7C3062C0D697F56D
      7FDDE64D5AECC9E21B84056A80023A72C24A1F2DF9E9A2509AE7F19F2F3EB168
      FFB1DF1630590DBCF0C28BD9F377FD9A97336766DED53ECD2DE752DA98CF2485
      5B4EC6059A8FB5375A3E0BF7322DFCE4C0A567576E2E6D11588F879C0D0FA9ED
      D477A2E419744AB876C9C526928CC173C104A1107F7DBD5D341AAF66CDC5B3DB
      56E9972CA8C74352E98DD5A653072CA582D6E5864F44F92DCF305B8957A8B5CC
      2BD8E6D72C4C3446B6B794C3B91DDDDD040FA98B199067D2B895AAC826E9356A
      49ABD648E6BC1306DBF16FDD30D2DE780F7975EB72131DD9E12F9F3BE8517AE2
      7BC385ED2BE1219FBAE4F5473D64F7BF3CE4FF2C0F59E314AAAE600F69F0F367
      979B80B27072784808FF5E417034A7FFF35FCA2CBF17F110270D62D4990F149B
      6DB353140E95924364B3E2FD46CD63C8AD054E79E41B0A9EB6765470FA80D34C
      61E35CEF673F92BD5BC4D06B835B70B6414CFBE414159C3A40D9F3ED97CC4EA2
      E459EFCBDE68D11B29F90D92B0C1E42F0394167DB368A4C2A1445C90F8F77764
      AF888EF41F0F76E46C83787CC3412A3C739072168F513894886BDD337DB5EC1D
      D181960FBEC5D906F1D8276E5470E610ED7A659CC2A1445C2B61DA4AB4289A5E
      7BE82ECE3688691FFC84161DA6DDAF65281C4AC4B5E2FF9605A20E2C361A8568
      D19ED727281C4AC435EF9EB21C44D12CD2AD2BE794B47A141412E254159B44A5
      45DF2D7D4CE150222EED31E90D85E8F6B58BF4DE33F7B2AA568C5AF099A267D2
      33C70ED13F974D56389C07246E37631ABAFD6AD76CB3D1A4ACFD545298476B66
      0D542AD616711D87DE498499ED785CCA4E540E228711CB0EE31A29EC1C3AA559
      9C899BF0AAEC11DA96CA6F5DA7D79EEC4BD35EDEC16A4556848AC8A1E74D3B75
      38977EC87A42E15022D8F874CB7C650AD2E7009AF344DD9B33E7D52FD8C489BD
      2BA6FB2273CD49840C079ED7C069535008E36BC09F131C2DAAC2860BD250287A
      03DD813080031F5CDF43C85EB972E579A435032A2A841919E9627A46C6D3C079
      202F237DDC8713C68E5AC860997500973DCDB6CCE4A8CBB2021818D233C66F1D
      9F3EF672E6B8B4D1A8AC1D933999EF66296099755CC6366CCB7594CA880430F2
      A8B00EF2476A6B8977913E6CC069AFC4F2DF7CFA8D5791398D04A92DCA70716F
      3D6621F5BA96D7B6BFDDBC3047EB5E7A6EAB59722940197B369363939E114D96
      8CDF02FA74C93776F0CA3744BF2F1275C12A70546E0107ACB512118F4BDA8872
      036E1E1E19507CA8B065FE57B9368D6A2546C996AA323232C2312293793BC03D
      FD9C67D7E26BAE919FAA65B94B9985CAC33D04F543512478A1BD1B8F91FC539E
      6C0661974BC6E83526499714483F8ED71594ACC248EA7B52E7CE9D5331BADB6A
      DDD2E54F5B072F198787D327959885F2D6DE8226672489092144E1EE18A26C43
      62CE39413A7F934C3A418EB8A9F339F7D3EC948F3AC5C63E241015620B2841B2
      59BE46EB4963B18CC062C0F8591254582242587B98E484F524A47F49822F56E0
      8AF221C162135478C65FD6C296EB54D44D64221F41B69C61A54CD406A34E0296
      B515CEDE20F9F16D24C48712BD9444841175E1F72292258104F49AC0B65C0775
      4F210D5021AA12B0BD84317FD2A24487BE99D19528C00D43CE1B887EB884A169
      3568C052A51232DCA26BB2A08A80CC5F751C7B80CD6490617939929FDD4394B9
      8DE8BB0B58F40EEF5284918C3E9505A2E35C07755B20CD67A2DD5651D50B1932
      A9541F928D5F0222F34B0479385EE815867E0A51DE7A81C7FB712B5696715FDA
      2628B6A854513787F75A218AA744F5893F725D6AF9E92DAD4F328ED0086C2256
      DF13C4899D4970C5E6AC3F4AB25AC4E81B91D62289DFF9DEFE75DA805821595D
      627E18B6CF49188CBA111BDBC9555B543AC7AAD2BD83BEDC7153EB8F93550813
      9179EF17123E384A02C6F9911354D8ACDCC09B87D3828BF6DB02F28F7E8E01A5
      B7D11D3BEB3D452419A78858718AD8ACC7AC42DDA708B6D21ED23332FEA59356
      61C169829D40C4AE0184D5DDC8029CF50B1A72230A81C2562902314E0C4A862A
      018800389C41B41BF866E5CA9517905609AA2AB93B1901A204C88005E0C032EB
      B88CF33581162885756CDAFF4C0F59E6D8A47FC943AE5CB96A8184FE818714DE
      28F533641CF51C72F2B2A1EDC72A92E3E062F10A1641F5631A6E6BC510AD3A44
      8413B05C2D505891CEBFB359D2AFF0531D3AA42AB1CC8B8DEDBC5944D7F7C161
      9FBFFEA525D957DD22C76864DBDD25662A875E092D3C48F6C663324A0651095C
      B0DA66BB9B6DB90ED785BA0F13D5EA2151A8387E8787C47A90BCFBD1AA267A48
      3C72AF61B2B5B89E600FC932A32E0FC92DE2722704743F9C1AB1876425FF82C0
      57B3C8CFF66BD8A5E057079E4751392326AAD7433A2D2168EAF1907C8AB087CC
      809DE221D5165B2F41C08F852C884A452E00D837A119E827784859D656F690B8
      7991D5A0874C698127C8AE1335E02167FF291E1207E4E77F96872CC3D6DB43FA
      FF3A0F69DFAE3F219698A3EF53DF6FEC34E0B159EDFB64CE8CEA3D7E72BBDE13
      A6B6EF357E6ACB9EE3D383BAA56982A3061F3CBFEF4387CBE52A35C0473619DC
      A4569B9E8E352C9DDC89964DEE8C074463A4571EEB28BDF5788CB16F72EBC703
      C3C2DE887FF433971AB52B29142251105C0A4B64E9578C2C9F2AB0D1892B563A
      70C14C6E38E1E2DB78D03DF1A1C9AE3EEE332AD5AB212A4438EEF9CA83F42ABE
      4E14488B535CA711E9CA8D526A835BC063E3DC2934C075548DDA95140A11E7F9
      AC57A3C75438FB554C84B565BF3D713B62E34FD72256EFBA1C11DDCABFC5A435
      076F4F5E75A0246D59EEC9A4A7763F7777E6A77AAECB5088D0225CF660828D28
      E2513C91D478C88F5B8595B9E9315C728EE81E480F01CF3ED0CE65E6D028FD8B
      A33B44A2EFE6A2EF36A2EF701946642702938D69153A19F388B0896891D52AD3
      F51299EAE9BB41E8BB455CB582080BF42287BA4A6BEC2D92E03530C5059B5A5F
      DFB9E2912654B5B708D76358A3D7C2F91AA8ADEF4E150AB4FB74391595CAA4E6
      6F472DA545822C49E5969A370EB0C5CAC662658E2A7DA7C755BB01575E783C46
      693578EC2D329BCA6FAB4503B9EB70EC68CCE4A1B390B72B5A08A6DAFA4EA7C6
      9A6D7800169E94D87F33118E1CFCF059E54B690B727D4D1672C7C56F292EE550
      2693010B7B8CBD27185777D82B78BC947F147064101EDCC2DEC03D31D4C6880E
      6C8920126D7DA17B3F25572D1A322FD76CB559608350AD8CB358321A9348B137
      9051FA0869AD017DA7AAADEF1CC65C86458C15A6DABFAAC2B2046F2241DF7971
      DF715FE09CC46F9BBD906B4BA211FF122C26D6709ED35A91F4C49E9D98F51489
      A329007D77DBD1776CCCD7CAD8B45BEEAE1AFD96795DD1B7ACFD1350578B7CC1
      7D15683060AFB9C2C884F31D49B5802952EDDE7E7BD5DFC78C79A45F5ADAE87E
      781AAA3FD2FE23463CD41F8B2CF41F3C7868FF8103EFEB3B70E0FD03B0A65DA0
      D168F8B95622CC3C6CB770E1E2659191ADBB070787760F0C0A890B0C0C890B08
      08896BD62C42417878F3EE9191ADE2F0E057F1238FA47D54AD2D3823EC9A815F
      7FFD43C9B2652B4D13264C28FFFB93334C4F3C31DD3473E65326CC2B30198DEE
      A6B0B066257BF71E314D9BF6D43B5CA5D6166137B71A3D2A7DE4CD9BD7555151
      ADA5BBE3EF96DAB6692D758CE920AD5DBB46C27C3F9C7E128D1891A63E72E4D0
      A11F7EF8EEE3EA07A4C0EC3611AB7EC34D6A757A21D0DF5F0E0D0BA3D0666198
      4D174A01981A121A1A4A5E5E5E8AADFD90C029C4151D3855212CB3DAFAEA5767
      49653C4F11CF42F2B70938D7308780303F124FAEF8116627C21A7E0C071404FB
      D9CF02A3058E3A4E3BA9D5DD9B5FBD2418D7ADB1A9D02A49AD86C393E8E2C58B
      989B6D221F1F5FC2B4099CF90A115721F1064EDCAF71223703E40F3ED0C98B17
      1B4D9863A48908977BA8F172C0152B64BEC415E1C7F3F3F2695CFA38FC5B1708
      6F895388F8D46126D501A2B93E46E3B42F0203E9D4ECD9848EA6662D5B6A4C67
      CF0AFAE010493A7C58289E378FBC9F5F8087BD6D94BD6307356FDE9C6E5C2FE2
      FACE734FBC4AE4A30D0CD4FBA6A4E80D7DFAE80DFDFAE98DFDFA49FC66413A7E
      5CD0612EA871F72EBA35770EE1A9206A1B1545EDA2DA61BAA7012D622E992312
      B6102D6FEDE595A16FD952904A4A48C2BC50555999A8B25A05C16492F17E4341
      83F6E7050793B87123FD333797B0E403EDF9EE3B4B72F27DAA8F3FDEF0DEABAF
      BE305A851F7417C3CD9B2ADD2FBF901A44B8C14270B258C159D9A502CAE5331E
      1E7462E2448AC6CB01DD0D06F2F3F7273737033705E09B12E868F4D1773F98CD
      7EE8600F0933EF71F3C1763F5174A420B883D4961716261D9E3A954212E2E9CA
      8573E40B9240F4279EE6C40C57352FB50232FBD1E4818DF443AF19A27117F330
      914B2E363796A8FD71BD5EF82835953C9393ADCDD02AA39727050505E3B59A1A
      EBE79F7D21464777516FDEBC61E3C285F3C680A34608DDAFD71FFC5DAB95EF91
      A4ED289D060C0586009CDE8BB40FF028300F9802B4066A04718297D7C4078CC6
      05288904EA0BE84252D76580AE525ED0AE7E4FA7E3F3897F81D8B75706DB30B8
      5C81C0EEA02EC6C6EACF9DFBDD8FBF85B66FDFA65CE714E315965817CA59FFF4
      E9D3CA51EC545413F0242BA5A60EC70F2A792B445CAE9BD48F7410CED29D8F08
      F12CD5F359B4C951687412B1A628732EA603376751C105CC78F6F3E3DF01255B
      252A7E36BD4A5EAC92FB17327F1A51954D735FF10FBA56A955DC67C595F2F589
      4EA28297B6F062ADD4B56B57A73D9E3726F6CF4E4535E1CAB1634E8D93C8A9A9
      26DC5A3E97CAB73BF78EB354DB2F952871B833DF601F3149E8C707296CFB6F4A
      A5B0CF8F52D0AAEC1AE40D12716D012FBB15556A0AD9F813DE55EB4A2AFF2056
      5781B2697C04E3796CFC6605C0BB1E771A609237F15979AE5F4BE256A9BCFDC8
      5270852E0CBF4BB1397BF62C393E4A8BD89963181E3E58262C7BE504AE01143B
      DE1CC9E8810900F9C464615B4FDAF5F8E15404444A8B90D61B64B3194F0094D3
      8511DD88492D972FD5B06F9048DB3795CE0FE9E8AC786E509422B35EC47F1725
      834821E2B33E2F2F8FF04426B18C07E295D506787D63EB00DCF64F19A34CC5C7
      751371BFA19E12AEE6E72B29470A51AADD1570FE8F42B9AE7743ED70226A0744
      3711ED61CFF5C255F0905790F997C2FF1B0FD9F6B5DB689584BB72FB90560ED5
      F377CAF6A63BE5AA1E32AD7F2CA5443A0BE9E065A28EFE77F295A5114BAB7E81
      72645736F8A3F29F46A41C478E56ACDBB68FD66D73E49A963A89723331E711D7
      465D9BE021F1EE2CE7B739899C9A6AC2E46C035A59B563D98477CCA46096EC68
      908837D76E6A8F7363BB2842976DB934E9CEEEA72675B69C55B3650A2B22A545
      F57948D828C141129B9945C2045C8641DB640FE920415D2709CB787A8C13058D
      DA344198A0183B5AA264AA450D12F1DEA1CCDC2A2D610ED637C9434E6F61A527
      276B6AF190FBF060FC55E654A07476EA5F1E52E98C4A51ADD7906D77B5B59B1C
      B0278D89F7C6EC759855F59093329B5307EAED28A443945D25EF2C803061C50A
      C4774283C7D11DD3FAA53F8D4855F97B96AD388D6CD5261355CFC3A496E024CA
      8DCD6DF23564933CE42C430CD95B5AB519BC63D2E803A7B2C13E729064C6C692
      9C2913A75CDBA16799D120111B392AB3CCA89E679DD2470D79C82CF41F1B331C
      F20ABCFEB7B2875488F81A12330D95CEE66B48AEC03879F2242734619FDDE12F
      0BFB9C269D1BA4E8386AB2875C81993D5C51E51BC00939F24AA6226AB08F78EF
      B02D571656084E12D657F690CA5FD14D9B36EA1DD790980D85217F9B720DC9CB
      38F17F7CBE278B099454FD1A12932B69E2C449A5B8D0EAA6F4D1BF9587E4CDAF
      01DCA76EF2FAE2353A1BCB3761159E560F61BAE7F399E3C7BF8E71DCD7070E1C
      380FBB7A103AD750E35B2B145588B060D21F5A5F9CB9AA10F1FAE22903078E68
      EAFAE23588F0B7F30FAD2F5E8308F7848C7F647D712692387200372963131212
      BAA363F5D873ACE61B289CD6BBBE381B380C59C62D419FBE8F4F9BF612EECFB6
      43EB701B5554CA6DD9EB49FCE68E13538CEF447384455FCC550C1D3A9C02B5AE
      2F5E74EAA8CD73CD53555AEFA883F455104DAF4204251626730F4F888FFFFBE4
      C9931FEC9998E88145CCE8AB1D5F5DDFB9E6F5F75F6BEF3A956D2A817F4E8780
      E86C0D229CA0121609BA078BBBCDC166C6E1F560F2D2254BBEDBB869D3F4CBE9
      779582E438E0080740D28933358858897F925E9D3A759A3A7FFEFCC9F8476979
      F5D5575FFBFEFBEF5FE43279D6C0D6481D64DC9A4F90AF33D4585FBCB2259301
      FFA8ACAB53C624EEBFD617AFB377885EC65A114D5B5FBC1EB2FFB6A25A0FC8CA
      AD59BD72DD933807A3A99A250628F271EB6376FAF83166B6577E8E58A80BF0E1
      BD129312FA6B751AA7090F6DECCCD97DF2DAD5023E28ED449F7CFCF931A7451D
      0293C0F13B4BB14A234958F61523594E1DB7088BC2B551D67C746A21C8183845
      D349A7D392C56C81E64EE031158BD58AD13ED9A954E19703E3FD6E54565EE654
      B2C0C68E7EE1DB66AC535A80BA16CC95C1A082F2D3CE7A86B069C316B955AB96
      CA254D914543A7CB8C6836060324511923C1480FF8F8313F526E9DD96C1854C0
      0DA8EC4BAEB399C001E981A1C3E6F0F04D4999898E977B10850493AB3F5E05E6
      6E20C90937C86E241AB17E91077E238D6E1411AC496ED94C93EC6314922F175A
      3A4943870C9BE3EDED4505A544A7057FDC3D36920A8F18A9D0996A8C0E63B96A
      65DC8865EE06FE5190B0AC0E5678202D6EC09FCF2FA3FCABE56384F5EF7E2847
      B68AA45D97D424454490A7BB1B594C68BF6CC326E2C14F1C3FDC5F4830E42303
      843113ACCF83C1F05B58E9F3E0B11B3985459691C2BBEB3E907D9AB5A19D45EE
      E41911823544F134ADC98CFE306384C33EDEE1E8074EF1340006C835A451CB74
      F65C61C9D9DF6F3DEEE9AA5E29BCBD7ABD5CEED7914E6903C9E0E54125D8440B
      A633F0F772C5EAC0FB7F49AB5191D55C4A870F5DFAB4B4CC3A36677A9BEBC2DC
      FF7857BE1E1C47BAB0107CBD0E870128B065D5091C79741BE97544674E5D2A3C
      73BA70CA0FB3A2DFE73261CCA22DB2292296829BF950B949C08BC8585D3B0474
      14F63C9971CCFD72F8E2F7C545657D0F3F7FD72DB616E29FDEF14EA1E04E3CAE
      C60A1C6F9CD40AF0287A0C6EF1C1F8E6B145717B15C5FFEE48D9EC5C9C46CA41
      F347B6151DD605FB9BDD081281627372E88F7CF62526A2191866257CB845B1DF
      7C43D4543290EC4B4EA63B2D0219F627D57B10B14D75B04FA9D0D9378D33F078
      4EA20E1D88468F26FAEC33A25DBBB894E89E7B086F78277AF75DA24387EC3AAE
      639748AC48AB264CC29A7BEFE5D80E87EC28B36B9DB173AFC57EF925D1B66DF6
      82DEBDEDDF6ECF558DB995D9D9765DFFFEB42F2545E9A33B449F7F4EF4C51776
      0347FCE69B0EC99E62ACD62E54C40307D2BE4183142255850A23B4D6863BDBAC
      FC8439AB60715FA77CA7451F7F4CF4C927F682071EA87FD3366FB6DB0D1E4CFB
      860CA9A7458E8E65F3871FE69868FD7A7BCA651F7E68971BDC6BBC8BD9943B96
      5386437694B1AE12EE6C1A46CE69D3A64A458D10535369DFF0E1F56C5A233814
      934A9B7667AFF1E13E6C9852DEE888EB54183B89F6393AB5A2E0FF6072F4FE01
      272A6FB6232F5556362473251B9E1A19DB2E7AE6DB8F1FBDFB2BB7B3EB2E5964
      FDA8C8C82794E3A82102677996DC6C6B4EFA69999F4681B2B0BC1C31D1A83EEF
      50E389B2E428D4FA05A0AFBF492B319A6FDF50DBCCE53103FE3303BA368D23CA
      92B530BE00F8ECCA1E79432ECEBFE2CB9349A0E010F5E9D6D6B57B482EAD8AB7
      90F5C9FE664C09934026AECCE9550CCAECEC9774A261A22C99FF6A3E42F8F44E
      7E271809F5DCFE6D6B4E1D649C6F78D3B2E4E3A8C415C7D004611DE45A43FD44
      59721A6ABD039C00491BA47506B1CE922CD903654C82841EE2A83ED44D44B4B1
      A2E25AB4E6E70AB9CEA476A22CF915D4E8035C031E051A0CFCF2CD89B0E20E3D
      8DB41D30091800706887D61C65A121A860B00CA81E0AA1E80C92DF91362AF0A6
      4D85E56AE000B005E0D60435850475FE0A8DE801A11136554DB26435145D81A1
      4008700C38D134A22C3910953600EE40955099488B1237800F095CE3288BB670
      39CB228D7C33823A0F5B8372BC21FCD65E2AF87D0B69F4BF5044375FE8C2A9E2
      C33F02CF437E0C180D4C00381D81743CA9746369E4F2A334EC950B143F7E3374
      5CC66704D7E1BA50D9830B924C80431447403BC0FE1B3E65EB5B347EC319BC89
      E82DE83874E408E02F7441EA0CAE90C60031C02280372F01693C0577984FD3BE
      FE8D46BF7D8E3C4298B815F42B010EBC057A161CE0CC631599B48A9493147AB5
      E00BB4E420751FF31214A9C03CE06D8009C722E54620B1071724E3013FE00580
      F3CFD0806766D182334769CA97BBA1BB0B1807B0CDCB48BD00AEC3B610ED8159
      99DD1BD9D100299B316AC5399AFFEB31EA3B2305BA54C011C2C9FE9980C40570
      0609D242201DE07E48A7212F6453DAEADFA9DFCC4FA11B050C07B8059948F9CB
      B82B164296802A418F9C3F1040CF1D9E83D7571FA545BFFF93426282A1F30702
      003FC017E094C17590C51F2C74E470486789E802D0021801C4031C86C02F9D66
      A121A860F00C503D1441F12048F290362A3011EFA5D6B06E0B5C00D84BFE0812
      33E4BFC2BF430FF4DAD02B119843151FB1226D5202824422FA16F807A0044189
      9B1025BE9F984820E9D7BF1F6DDFB69D7246E6281C4A84824685F875F1890492
      8183062ACF4830D19EB43D0A8712A190BAADEA3687700B616FFADE1CAAE583F2
      4402C9E02183F1C286528858471B2D82BDC2A144ACEDFC566799532069FFA3FB
      73A8D20765890492D4E1A9E4588B6DC7F61D5091D3D649D47E497BF9E1510FD3
      FAF7D62B0647A61EC9217CA04F249070199EE9878875A5767CC56992C386334E
      A2D6AFB4967BF5EEA53C91B976CD5AC22709E0F0ED23631FA1F3E7CFB34C5F67
      7FCD69D289E92772A8D24770C8118B22E4E45EC94AB665CB96B4226B8522674E
      C8545EBBC3996FBEFE8693A433B3CEE450B58F9328645E48226113B0722512A2
      A876514A7AF497A34AFAEDB7DF729A74E1B90B3954CBC749C465FECFFA275225
      32C84A70905C9E7F3987EAF85421621BEF59DE895489CC4152B0A820879AFAF1
      9CE19908C81548A4BF3E8DEA01AB20A94A4874AD643C057234C0A1C64E6365BD
      F026D266EB5D9E1B1F1C9207C3410007DC8FE7A41128D5E882A6DAE4015354AA
      37233B754EFBCDD3D3EDAB1E7F8FA0A95F8652CA334162233814139DA7D7283F
      0F8F0D3F59CC771F0C0892357860973C7D83513884446966A3897A3C95BDBDF3
      F203178AEF9B52D45E24C1C3CD056EE09505201A8F978BF1C504C486025F1267
      C939B84E38084C9FD13EBAFF7DCD5BC8A8361CE0D0C80665C9F3842CF910BD72
      6D156A69000E4C3412378B256430C70E71BD214B6E83F2D90026BFB90CC76B31
      4B25BC965020E169307D04FD61C02A36E2F87811861C66E3C22201C29B5659CE
      B161110BC88B81F78131803D78D7767C2C2BBF1F7DC2FDF2A9DD8A6FA7D1FD90
      7B029D813820054814F9F8B82C48C34F4AAAAFE23B749C1BD9AC59000AF420C0
      40916A3E640E333902720126DD89743FF003F02590233A8E8FBD564BC2495F9F
      DB3AA3D14AFE7E28A3C52470429F62938E2B523D91F8EDD8B4B7667EB4696CF7
      984E9B22D56ABDD1CD4DA2B889F7A24E77E03AE06815C4BA83981CB1A8FF4BBF
      251DF57C6CFFE343555133B7EC478BDDC3B80FF058EBAD74B4C65C77F53B2592
      70EF9C0FA8B4783054A37E8BECD3E1C4BB7FF3A7A8BEF9E41ED49F9EF4E313D3
      8AB206834A265A447A636B58B62553491E463CDBCB1F3D399F3E98C4D7935037
      2ED8BB938877E120548901B86FCE23DD036C07DE011A1DBAC0F27EA02750E5F8
      40FE4F0B02981848EA0E525D45737716089DEF9B4693EFF1A6594F64D27EB7A9
      14D1358D627A8D174EEE595DA39AF3F41F3AEF2D8A1B3D5931D86D922928DC4B
      0E4809A08727CE12BB78EC54EDBB948085A59228D0532BFB3E7A8B5CBC22A8DF
      08FB01FFCC8C49358733D6EF394DF1783FE2CD93C7923DF65F9FFEE2A7175F9B
      B6EF6F4BC6F5D83F7FE0D40D2355E6BC90F2B7DC4483871F4E5B57E58B47E72F
      9384A12FADC29CB44E74F4E79F848BC70F4A2D7ADE7B97C13364B2246B7BD84C
      AAF01BC584E1531315E65D2195D574D3552B1D17CCC5EFFCF8DE23EF0EBA7FB8
      A945970752CE5F2E168547DF584FCB27B5A5017FDB411A83D1206A74FD0BAEDF
      6EE54EB692A212D943740DEC58A20E88BD59A609B696966175043361518A324F
      9D757944A86FC18D52E9516BD1AFCB84A4B94B877984F7E8EDE2EA51E2E54ABF
      980A2E9DEA186C383F3129E62AF5EC59423B771A837BA427C83E31A3F541AD07
      882ABD5E2EC1DA8DB2C5A456696549548BDEF2898942D08C350B74EED193D42E
      1EEE7A37B15CA5956EE85DC4CB786BCE45C3EDCB7B368EE8F26E60E2B4F311D1
      F1C1D7AE5C4685A8A9180A76914BCA49360BA2A75E73DB57FE6D8AE4DE69680A
      D9BCDB99CB45FDED52B3EAB659742BB688FED7AFE719F66CDBB677D2FCAC5DCD
      7CD426979084DB112D34915755AEF794A3598284057530D956AB55595DE5C2CD
      927BC7E1A9A2CD3B46C0A60B168B0DBE9754E6A23CF9EC3F9F3DB56AFA9BD947
      AE960945F9BE4546F3CC53C666CF164B1A4FD2AA04524B98492CC93A9D582A16
      9FDA24848C5FBF4A4D910FC836B55156C1636855B2DA4D280DF2B22DDE303DE6
      F54041BAD16ECC0B2D442FCF0CB4A69DA433044B7A63802868F07248ADABA74A
      2C0A2A3B9D2A848DDBFCBE2436BBDF5A7655E265E025AD9F8EB46AD9C55D2873
      D10B5BDB07C92BD76674DC1A18102BDECCDF8775C2BB7A8B7EC1C1925770A8E4
      1E10EDEEE9A5F1B1E4BD25341BB7E14B3CAB10527AF9C777CD666B98AB5FAFD1
      2A7DB0BB4D5493C1DB05F3FAF5F961DE74D0D75D7D50AF152E145F3FAFBD70FA
      F8E9BD41895B2E8E0B53976142F2AC337239050D9B34D93769C8BD4FC8B28479
      67EEEE71A3A685DCBFFABBF087B617B718FB8325EEE91372EAD2D3F284B5BF9A
      3257EF3B3BE2C58D6F0F99F1461C0643B5AD93862A47B623D2B1E01A094FA2F8
      7C224370746448DCA3A35BA7BCB8A8FBA837960C9A9CB578E8A445D3EFEA9F1A
      0F5B8FF4259F61185F965AC6F440B65AD0616EA343E51E71378DDD233BB26A87
      D0FDBE34F2F7F17564FF3FA61E91BDC56E33CEC0C5F0C03C7F31523C05963074
      9CE8E5E357AB93FB2F4A82397174528F250000000049454E4400000000}
  end
  object ScalesImageList: TTBXImageList
    Left = 48
    Top = 240
    PngDIB = {
      2300000089504E470D0A1A0A0000000D49484452000000100000023002030000
      00F2AE03ED0000000C504C5445000000000000FF0000808080E6239CB1000000
      0174524E530040E6D8660000026F494441547801AD966B6EE3300C848780F45F
      01E2FB30400EE002D621774FD19B75BF911B24C56E1772525519EB418F872269
      57FAAE45EF52FCFEF5FE09BDBF4B1F1F800EB562EBB314DB9551294033241032
      6C00AB937DC32E42AA2BA0664808424D2A4A36DE10AEE9B6D832137D4A18AA04
      538A096B4A601B269A6C162458623B714731B40BA3EC5029C483001626FB92B2
      226E4BA1AF034DF9BCBE45B4CC2F7C4CF19435908D6182D54C8F6E2B02505740
      8D28288150A2D65057469A6E8B2D132D1BA0E269F3280148BDE1355B4DFD86B1
      EFDDF918A9F94C5350298163F1F543436461250BFB09089A8AA119D2D087896D
      677EA30EAACEEA275D0565818F6C26BB4969EFC2E7E39D21C3663C7CD747C6C1
      0705DDD542EE708AEAB1070EE389FE17DFD9FA28B9803E7FC0DF2B7C51D087AA
      B4D2697F439D9257727494155236A8F48AB47B68A54F3E4EF0E9D0E211AF8382
      3E026A22EB6BB88AE6DC43C1682208BBC9432820A3DB5F2E4FEBFB572A73A6A4
      1EE2F09F54B1C9FEF449ACC921B69363038480A28B831290522AEACB8AC9249D
      6A92FCAD511F6AD4875A1175E17CD9AF9CEEB26232CFC7C3777DC34FE0C6477D
      3044DF91AFC883BF50E1E7D9BAB88EFAF8017FAFF05DEC2FD9944547FCAD29BE
      1FBC919BDFCD86E8B711AF6B36385E4C663B55E68283737CDA9806C5CA9491B3
      1A1E8FB8CCF5C5669952F2474FD5CD04E978FA4B22757635DBEEFE0ADF5AA438
      338FC6D49E1FF31749122C245DE26549F89A47CBCA74ECEEA0B956B94DF66D48
      F1A7E8357D777F1B02763E533BD2CFC4771CF608C00094D6F10F5A9A7E803CE2
      5973FD411FF735A6AFE51F049CBBA96E5E46F799F24AF53A1BFF8BEF1FBADD59
      9D6BF277E30000000049454E4400000000}
  end
  object MainPopupMenu: TTBXPopupMenu
    Images = MenusImageList
    OnPopup = MainPopupMenuPopup
    Left = 208
    Top = 168
    object NMarkEdit: TTBXItem
      ImageIndex = 31
      OnClick = NMarkEditClick
      Caption = 'Edit'
      Hint = ''
    end
    object NMarkDel: TTBXItem
      ImageIndex = 30
      OnClick = NMarkDelClick
      Caption = 'Delete'
      Hint = ''
    end
    object NMarkOper: TTBXItem
      ImageIndex = 10
      OnClick = NMarkOperClick
      Caption = 'Selection Manager'
      Hint = ''
    end
    object NMarkNav: TTBXItem
      ImageIndex = 33
      OnClick = NMarkNavClick
      Caption = 'Navigate to Placemark'
      Hint = ''
    end
    object NMarkExport: TTBXItem
      ImageIndex = 25
      OnClick = NMarkExportClick
      Caption = 'Placemark Export'
      Hint = ''
    end
    object NMarksCalcs: TTBXSubmenuItem
      ImageIndex = 9
      Caption = 'Measurements'
      Hint = ''
      object NMarksCalcsLen: TTBXItem
        OnClick = NMarksCalcsLenClick
        Caption = 'Length'
        Hint = ''
      end
      object NMarksCalcsPer: TTBXItem
        OnClick = NMarksCalcsPerClick
        Caption = 'Perimeter'
        Hint = ''
      end
      object NMarksCalcsSq: TTBXItem
        OnClick = NMarksCalcsSqClick
        Caption = 'Area'
        Hint = ''
      end
    end
    object NMarkSep: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object NaddPoint: TTBXItem
      ImageIndex = 15
      OnClick = NaddPointClick
      Caption = 'Add Placemark'
      Hint = ''
    end
    object N47: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object N28: TTBXSubmenuItem
      Caption = 'Center With Zoom'
      Hint = ''
      object TBXToolPalette2: TTBXToolPalette
        ColCount = 5
        Images = ScalesImageList
        PaletteOptions = []
        RowCount = 5
        OnCellClick = TBXToolPalette2CellClick
        Caption = ''
        Hint = ''
      end
    end
    object N22: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object N43: TTBXSubmenuItem
      ImageIndex = 28
      Caption = 'Copy to Clipboard'
      Hint = ''
      object Google1: TTBXItem
        OnClick = Google1Click
        Caption = 'URL to Google Maps'
        Hint = ''
      end
      object YaLink: TTBXItem
        OnClick = YaLinkClick
        Caption = 'URL to Yandex.Maps'
        Hint = ''
      end
      object kosmosnimkiru1: TTBXItem
        OnClick = kosmosnimkiru1Click
        Caption = 'URL to kosmosnimki.ru'
        Hint = ''
      end
      object livecom1: TTBXItem
        OnClick = livecom1Click
        Caption = 'URL to Bing Maps'
        Hint = ''
      end
      object N51: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object N13: TTBXItem
        OnClick = N13Click
        Caption = 'URL to Primary Map Tile'
        Hint = ''
      end
      object TBCopyLinkLayer: TTBXSubmenuItem
        Caption = 'URL to Layer Tile'
        Hint = ''
      end
      object N30: TTBXItem
        OnClick = N30Click
        Caption = 'Coordinates'
        Hint = ''
      end
      object N20: TTBXItem
        OnClick = N20Click
        Caption = 'Primary Map Tile'
        Hint = ''
      end
      object N15: TTBXItem
        OnClick = N15Click
        Caption = 'Pathname to Tile in Cache'
        Hint = ''
      end
    end
    object Nopendir: TTBXItem
      OnClick = NopendirClick
      Caption = 'Show Primary Map Tile'
      Hint = ''
    end
    object N25: TTBXItem
      ImageIndex = 34
      OnClick = N25Click
      Caption = 'Open Primary Map Tile Folder'
      Hint = ''
    end
    object TBOpenDirLayer: TTBXSubmenuItem
      ImageIndex = 34
      Caption = 'Open Overlay Layer Tile Folder'
      Hint = ''
    end
    object N23: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object N26: TTBXSubmenuItem
      Caption = 'Additional Operations'
      Hint = ''
      object NGTOPO30: TTBXItem
        OnClick = NGTOPO30Click
        Caption = 'Current Altitude by GTOPO30 (~1 km accuracy)'
        Hint = ''
      end
      object NSRTM3: TTBXItem
        OnClick = NSRTM3Click
        Caption = 'Current Altitude by SRTM3 (~90 m accuracy)'
        Hint = ''
      end
      object N49: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object DigitalGlobe1: TTBXItem
        OnClick = DigitalGlobe1Click
        Caption = 'DigitalGlobe Availability'
        Hint = ''
      end
      object N27: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
    end
    object N24: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object N21: TTBXItem
      ImageIndex = 21
      OnClick = N21Click
      Caption = 'Download Primary Map Tile to Cache (Ins+MLeft)'
      Hint = ''
    end
    object NDel: TTBXItem
      ImageIndex = 22
      OnClick = NDelClick
      Caption = 'Delete Primary Map Tile from Cache (Del+MLeft)'
      Hint = ''
    end
    object ldm: TTBXSubmenuItem
      ImageIndex = 21
      Caption = 'Download Overlay Layer Tile to Cache'
      Hint = ''
    end
    object dlm: TTBXSubmenuItem
      ImageIndex = 22
      Caption = 'Delete Overlay Layer Tile from Cache'
      Hint = ''
    end
    object N1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object NMapInfo: TTBXItem
      ImageIndex = 27
      OnClick = NMapInfoClick
      Caption = 'Map Info'
      Hint = ''
    end
    object TBLayerInfo: TTBXSubmenuItem
      ImageIndex = 27
      Caption = 'Layer Info'
      Hint = ''
    end
  end
  object TrayIcon: TTrayIcon
    PopupMenu = TrayPopupMenu
    OnClick = TrayItemRestoreClick
    OnDblClick = TrayItemRestoreClick
    Left = 520
    Top = 112
  end
  object TrayPopupMenu: TTBXPopupMenu
    Left = 520
    Top = 144
    object TrayItemRestore: TTBItem
      OnClick = TrayItemRestoreClick
      Caption = 'Restore'
      Hint = ''
    end
    object TBSeparatorItem1: TTBSeparatorItem
      Caption = ''
      Hint = ''
    end
    object TrayItemQuit: TTBItem
      OnClick = TrayItemQuitClick
      Caption = 'Quit'
      Hint = ''
    end
  end
end
