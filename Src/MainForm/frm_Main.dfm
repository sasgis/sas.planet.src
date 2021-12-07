object frmMain: TfrmMain
  Left = 488
  Top = 165
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'SAS.Planet'
  ClientHeight = 639
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
  OnMouseWheel = FormMouseWheel
  OnShortCut = FormShortCut
  PixelsPerInch = 96
  TextHeight = 13
  object map: TImage32
    Left = 413
    Top = 59
    Width = 265
    Height = 571
    Align = alClient
    Bitmap.CombineMode = cmMerge
    Bitmap.ResamplerClassName = 'TLinearResampler'
    BitmapAlign = baTopLeft
    Color = clSilver
    ParentColor = False
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 2
    OnDblClick = mapDblClick
    OnMouseLeave = mapMouseLeave
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
      Caption = 'Main'
      DockPos = -6
      DockRow = 1
      Images = PanelsImageList
      Stretch = True
      TabOrder = 3
      object TBmove: TTBXItem
        Action = actMoveMap
        Images = PanelsImageList
        Options = [tboDefault]
      end
      object TBRectSave: TTBXSubmenuItem
        Action = actSelectByRect
        DropdownCombo = True
        Images = PanelsImageList
        LinkSubitems = NRectSave
        Options = [tboShowHint]
      end
      object TBCalcRas: TTBXItem
        Action = actDistanceCalculation
        Images = PanelsImageList
      end
      object TBCircleCalc: TTBXItem
        Action = actCircleCalculation
        Images = PanelsImageList
      end
      object TBXSeparatorItem4: TTBXSeparatorItem
      end
      object TBMapZap: TTBXSubmenuItem
        DisplayMode = nbdmImageAndText
        Hint = 'Cached tiles map'
        ImageIndex = 7
        Images = PanelsImageList
        LinkSubitems = NFillMap
        Options = [tboDropdownArrow, tboShowHint]
      end
      object TBGoTo: TTBXSubmenuItem
        Action = actShowGoTo
        DropdownCombo = True
        Images = PanelsImageList
        Options = [tboShowHint]
      end
      object TBXSeparatorItem5: TTBXSeparatorItem
      end
      object TBFullSize: TTBXItem
        Action = actViewFullScreen
        Images = PanelsImageList
      end
    end
    object SrcToolbar: TTBXToolbar
      Left = 261
      Top = 25
      Caption = 'Sources'
      DockPos = 224
      DockRow = 1
      Stretch = True
      TabOrder = 4
      object TBSrc: TTBXSubmenuItem
        Hint = 'Select data source'
        ImageIndex = 0
        Images = PanelsImageList
        LinkSubitems = NSources
        Options = [tboDropdownArrow]
      end
      object TBSMB: TTBXSubmenuItem
        DisplayMode = nbdmImageAndText
        Hint = 'Selected basemap'
        ImageIndex = 3
        Images = PanelsImageList
        Options = [tboDropdownArrow]
      end
      object TBLayerSel: TTBXSubmenuItem
        Hint = 'Select overlay layers'
        ImageIndex = 3
        Images = PanelsImageList
        Options = [tboDropdownArrow]
        object btnHideAll: TTBXItem
          Action = actMapsAllLayersHide
        end
        object HideSeparator: TTBSeparatorItem
        end
        object tbiLayersList: TTBGroupItem
        end
      end
    end
    object TBMarksToolbar: TTBXToolbar
      Left = 437
      Top = 25
      Caption = 'Placemarks'
      DockPos = 363
      DockRow = 1
      Images = PanelsImageList
      LinkSubitems = NMarksGroup
      Stretch = True
      TabOrder = 6
    end
    object GPSToolbar: TTBXToolbar
      Left = 653
      Top = 25
      Caption = 'GPS'
      DockPos = 504
      DockRow = 1
      Images = PanelsImageList
      Stretch = True
      TabOrder = 7
      object TBGPSconn: TTBXItem
        Action = actGpsConnect
        Images = PanelsImageList
      end
      object TBGPSPath: TTBXSubmenuItem
        Action = actConfigGpsShowTrack
        DropdownCombo = True
        Images = PanelsImageList
        object tbitmSaveCurrentPositionToolbar: TTBXItem
          Action = actGpsMarkPointAdd
          Images = MenusImageList
        end
        object TBXSeparatorItem16: TTBXSeparatorItem
        end
        object tbitmGPSTrackSaveToMarks: TTBXItem
          Action = actGpsTrackSaveToDb
          Images = MenusImageList
        end
        object TBXSeparatorItem17: TTBXSeparatorItem
        end
        object TBItemDelTrack: TTBXItem
          Action = actGpsTrackClear
          Images = MenusImageList
        end
      end
      object TBGPSToPoint: TTBXSubmenuItem
        Action = actConfigGpsFollowPosition
        DropdownCombo = True
        Images = PanelsImageList
        object TBGPSToPointCenter: TTBXItem
          Action = actConfigGpsFollowPositionAtCenter
        end
      end
    end
    object TBExit: TTBXToolbar
      Left = 807
      Top = 25
      DockPos = 807
      DockRow = 1
      TabOrder = 8
      Visible = False
      object TBXExit: TTBXItem
        Action = actQuit
        Images = MenusImageList
      end
    end
    object TBXMainMenu: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Main Menu'
      CloseButton = False
      DockPos = -6
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      Stretch = True
      TabOrder = 0
      object NOperations: TTBXSubmenuItem
        Caption = '&Operations'
        object tbitmCreateShortcut: TTBXItem
          Action = actMakeLinkOnDesktop
          Images = MenusImageList
        end
        object tbitmOpenFile: TTBXItem
          Action = actFileOpen
          Images = MenusImageList
        end
        object TBXSeparatorItem6: TTBXSeparatorItem
        end
        object NZoomIn: TTBXItem
          Action = actZoomIn
          Images = MenusImageList
        end
        object NZoomOut: TTBXItem
          Action = actZoomOut
          Images = MenusImageList
        end
        object TBXSeparatorItem7: TTBXSeparatorItem
        end
        object tbitmGoToModal: TTBXItem
          Action = actShowGoTo
          Images = MenusImageList
        end
        object NCalcRast: TTBXItem
          Action = actDistanceCalculation
          Images = MenusImageList
        end
        object NCircleCalc: TTBXItem
          Action = actCircleCalculation
          ImageIndex = 65
          Images = MenusImageList
        end
        object TBXSeparatorItem8: TTBXSeparatorItem
        end
        object NRectSave: TTBXSubmenuItem
          Caption = 'Selection Manager'
          Images = MenusImageList
          object TBRECT: TTBXItem
            Action = actSelectByRect
            Images = PanelsImageList
            Options = [tboShowHint]
          end
          object TBREGION: TTBXItem
            Action = actSelectByPolygon
            Images = PanelsImageList
          end
          object TBPolylineSelect: TTBXItem
            Action = actSelectByLine
            Images = PanelsImageList
          end
          object TBCOORD: TTBXItem
            Action = actSelectByCoordinates
            Images = PanelsImageList
          end
          object TBScreenSelect: TTBXItem
            Action = actSelectByVisibleArea
            Images = PanelsImageList
          end
          object TBXSeparatorItem13: TTBXSeparatorItem
          end
          object TBPrevious: TTBXItem
            Action = actSelectByLastSelection
            Images = MenusImageList
          end
          object tbitmEditLastSelection: TTBXItem
            Action = actSelectByLastSelectionEdit
            Images = MenusImageList
          end
          object TBLoadSelFromFile: TTBXItem
            Action = actSelectBySelectionFromFile
            Images = MenusImageList
          end
        end
        object TBXSeparatorItem9: TTBXSeparatorItem
        end
        object tbitmCacheManager: TTBXItem
          Action = actShowCacheManager
          Images = MenusImageList
        end
        object TBXSeparatorCacheManager: TTBXSeparatorItem
        end
        object tbitmQuit: TTBXItem
          Action = actQuit
          Images = MenusImageList
        end
      end
      object NView: TTBXSubmenuItem
        Caption = '&View'
        object NPanels: TTBXSubmenuItem
          Caption = 'Toolbars'
          Images = MenusImageList
          object NMainToolBarShow: TTBXVisibilityToggleItem
            Caption = 'Main'
            Control = TBMainToolBar
            Images = MenusImageList
          end
          object NZoomToolBarShow: TTBXVisibilityToggleItem
            Caption = 'Zoom'
            Control = ZoomToolBar
            Images = MenusImageList
          end
          object NsrcToolBarShow: TTBXVisibilityToggleItem
            Caption = 'Sources'
            Control = SrcToolbar
            Images = MenusImageList
          end
          object NFavoriteToolbarShow: TTBXVisibilityToggleItem
            Caption = 'Favorites'
            Control = TBXFavoriteToolbar
            Images = MenusImageList
          end
          object NGPSToolBarShow: TTBXVisibilityToggleItem
            Caption = 'GPS'
            Control = GPSToolbar
            Images = MenusImageList
          end
          object TBXVisibilityToggleItem1: TTBXVisibilityToggleItem
            Caption = 'Placemarks'
            Control = TBMarksToolbar
            Images = MenusImageList
          end
          object TBXVisibilityToggleItem2: TTBXVisibilityToggleItem
            Caption = 'Search'
            Control = TBXToolBarSearch
            Images = MenusImageList
          end
          object NSearchResults: TTBXVisibilityToggleItem
            Caption = 'Search Results'
            Control = TBSearchWindow
            Images = MenusImageList
          end
          object tbxMergePolygonsShow: TTBXVisibilityToggleItem
            Caption = 'Merge Polygons'
            Control = tbMergePolygons
            Images = MenusImageList
          end
          object TBXVisibilityToggleItem3: TTBXVisibilityToggleItem
            Caption = 'Time Interval'
            Control = FillDates
            Images = MenusImageList
          end
          object NSensors: TTBXSubmenuItem
            AutoCheck = True
            Caption = 'Sensors'
            DropdownCombo = True
            Images = MenusImageList
            OnClick = NSensorsClick
          end
          object TBXSeparatorItem18: TTBXSeparatorItem
          end
          object NBlock_toolbars: TTBXItem
            Action = actViewToolbarsLock
            Images = MenusImageList
          end
        end
        object tbsbmInterface: TTBXSubmenuItem
          Caption = 'Interface'
          Images = MenusImageList
          object Showstatus: TTBXItem
            Action = actConfigStatusBarVisible
            Images = MenusImageList
          end
          object ShowMiniMap: TTBXItem
            Action = actConfigMiniMapVisible
            Images = MenusImageList
          end
          object ShowLine: TTBXItem
            Action = actConfigScaleLineVisible
            Images = MenusImageList
          end
        end
        object NFillMap: TTBXSubmenuItem
          Caption = 'Cached Tiles Map'
          ImageIndex = 7
          Images = MenusImageList
          object TBFillingTypeMap: TTBXSubmenuItem
            Caption = 'Show for...'
            Images = MenusImageList
            Options = [tboDropdownArrow]
            object tbitmFillingMapAsMain: TTBXItem
              Action = actViewFillingMapMainMapUse
              OnAdjustFont = AdjustFont
            end
            object tbiFillingMapMaps: TTBGroupItem
            end
          end
          object TBXSeparatorItem11: TTBXSeparatorItem
          end
          object tbtpltCachedTilesMap: TTBXToolPalette
            ColCount = 5
            Images = ScalesImageList
            PaletteOptions = []
            RowCount = 7
            OnCellClick = tbtpltCachedTilesMapCellClick
          end
          object TBXSeparatorItem20: TTBXSeparatorItem
          end
          object NFillMode1: TTBXItem
            Action = actViewFillingMapMarkUnexisting
            Images = MenusImageList
          end
          object NFillMode2: TTBXItem
            Action = actViewFillingMapMarkExisting
            Images = MenusImageList
          end
          object NFillMode3: TTBXItem
            Action = actViewFillingMapMarkGradient
            Images = MenusImageList
          end
          object TBXSeparatorItem21: TTBXSeparatorItem
          end
          object NShowFillDates: TTBXItem
            Action = actViewFillingMapFilterMode
            Images = MenusImageList
          end
        end
        object NShowGran: TTBXSubmenuItem
          Caption = 'Tile Boundaries'
          ImageIndex = 3
          Images = MenusImageList
          object tbtpltViewGridTile: TTBXToolPalette
            ColCount = 5
            Images = ScalesImageList
            PaletteOptions = [tpoNoAutoSelect]
            RowCount = 7
            OnCellClick = tbtpltViewGridTileCellClick
            OnGetCellVisible = tbtpltViewGridTileGetCellVisible
          end
        end
        object tbsbmGenShtabScale: TTBXSubmenuItem
          Caption = 'GenShtab Maps Boundaries'
          Images = MenusImageList
          object NGShScale0: TTBXItem
            Action = actViewGridGenShtabNo
            Images = MenusImageList
          end
          object NGShScale1000000: TTBXItem
            Action = actViewGridGenShtab_1_000_000
            Images = MenusImageList
          end
          object NGShScale500000: TTBXItem
            Action = actViewGridGenShtab_500_000
            Images = MenusImageList
          end
          object NGShScale200000: TTBXItem
            Action = actViewGridGenShtab_200_000
            Images = MenusImageList
          end
          object NGShScale100000: TTBXItem
            Action = actViewGridGenShtab_100_000
            Images = MenusImageList
          end
          object NGShScale50000: TTBXItem
            Action = actViewGridGenShtab_50_000
            Images = MenusImageList
          end
          object NGShScale25000: TTBXItem
            Action = actViewGridGenShtab_25_000
            Images = MenusImageList
          end
          object NGShScale10000: TTBXItem
            Action = actViewGridGenShtab_10_000
            Images = MenusImageList
          end
          object NGShScale5000: TTBXItem
            Action = actViewGridGenShtab_5_000
            Images = MenusImageList
          end
          object NGShScale2500: TTBXItem
            Action = actViewGridGenShtab_2_500
            Images = MenusImageList
          end
          object TBSeparatorItem3: TTBSeparatorItem
          end
          object NGShauto: TTBXItem
            Action = actViewGridGenShtabAuto
            Images = MenusImageList
          end
        end
        object DegreedLinesSubMenu: TTBXSubmenuItem
          Caption = 'Lat/Lon Grid'
          Images = MenusImageList
          object NDegScale0: TTBXItem
            Action = actViewGridLonLatNo
            Images = MenusImageList
          end
          object NDegScale1000000: TTBXItem
            Action = actViewGridLonLat_10_000
            Images = MenusImageList
          end
          object NDegScale500000: TTBXItem
            Action = actViewGridLonLat_05_000
            Images = MenusImageList
          end
          object NDegScale200000: TTBXItem
            Action = actViewGridLonLat_02_000
            Images = MenusImageList
          end
          object NDegScale100000: TTBXItem
            Action = actViewGridLonLat_01_000
            Images = MenusImageList
          end
          object NDegScale50000: TTBXItem
            Action = actViewGridLonLat_00_500
            Images = MenusImageList
          end
          object NDegScale25000: TTBXItem
            Action = actViewGridLonLat_00_250
            Images = MenusImageList
          end
          object NDegScale10000: TTBXItem
            Action = actViewGridLonLat_00_125
            Images = MenusImageList
          end
          object TBXSeparatorItem22: TTBXSeparatorItem
          end
          object NDegScaleUser: TTBXItem
            Action = actViewGridLonLat_User
            Images = MenusImageList
          end
          object NDegValue: TTBXEditItem
            OnAcceptText = NDegValueAcceptText
          end
          object TBSeparatorItem2: TTBSeparatorItem
          end
          object NDegScaleAuto: TTBXItem
            Action = actViewGridLonLatAuto
            Images = MenusImageList
          end
        end
        object tbxsbmProjection: TTBXSubmenuItem
          Caption = 'Projection'
          Images = MenusImageList
          object tbiProjections: TTBGroupItem
          end
          object tbiProjectionOfMap: TTBXItem
            Action = actConfigProjectionOfMapUse
          end
        end
        object TBXSubmenuMap: TTBXSubmenuItem
          Caption = 'Maps'
          Images = MenusImageList
          object tbxnxtmap: TTBXItem
            Action = actViewSelectNextMapWithTile
          end
          object tbxprevmap: TTBXItem
            Action = actViewSelectPrevMapWithTile
          end
        end
        object TBXSubmnMapVer: TTBXSubmenuItem
          Caption = 'Versions'
          Images = MenusImageList
          object TBXNextVer: TTBXItem
            Action = actViewSelectNextMapVersion
          end
          object TBXPrevVer: TTBXItem
            Action = actViewSelectPrevMapVersion
          end
        end
        object TBXSeparatorItem10: TTBXSeparatorItem
        end
        object NFoolSize: TTBXItem
          Action = actViewFullScreen
          Images = MenusImageList
        end
        object NGoToCur: TTBXItem
          Action = actConfigZoomToCursor
          Images = MenusImageList
        end
        object Nbackload: TTBXItem
          Action = actConfigUsePrevForMap
          Images = MenusImageList
        end
        object NbackloadLayer: TTBXItem
          Action = actConfigUsePrevForLayers
          Images = MenusImageList
        end
        object tbiConfigUsePrevForVectorLayer: TTBXItem
          Action = actConfigUsePrevForVectorLayers
          Images = MenusImageList
        end
        object Nanimate: TTBXItem
          Action = actConfigUseZoomAnimation
          Images = MenusImageList
        end
        object NAnimateMove: TTBXItem
          Action = actConfigUseInertialMovement
          Images = MenusImageList
        end
        object tbitmGauge: TTBXItem
          Action = actConfigAzimuthCircle
          Images = MenusImageList
        end
        object Ninvertcolor: TTBXItem
          Action = actConfigColorInversion
          Images = MenusImageList
        end
        object NShowSelection: TTBXItem
          Action = actConfigPreviousSelectionVisible
          Images = MenusImageList
        end
        object tbitmNavigationArrow: TTBXItem
          Action = actViewNavigation
          Images = MenusImageList
        end
        object tbxSunCalc: TTBXItem
          Action = actViewSunCalc
          Images = MenusImageList
        end
        object tbxMoonCalc: TTBXItem
          Action = actViewMoonCalc
          Images = MenusImageList
        end
        object tbitmShowDebugInfo: TTBXItem
          Action = actShowDebugInfo
          Images = MenusImageList
        end
      end
      object NSources: TTBXSubmenuItem
        Caption = '&Source'
        object NSRCesh: TTBXItem
          Action = actConfigDownloadModeCache
          Images = PanelsImageList
          OnAdjustFont = AdjustFont
        end
        object NSRCinet: TTBXItem
          Action = actConfigDownloadModeInternet
          Images = PanelsImageList
          OnAdjustFont = AdjustFont
        end
        object NSRCic: TTBXItem
          Action = actConfigDownloadModeCacheInternet
          Images = PanelsImageList
          OnAdjustFont = AdjustFont
        end
      end
      object NSMB: TTBXSubmenuItem
        Caption = '&Maps'
        LinkSubitems = TBSMB
      end
      object NLayerSel: TTBXSubmenuItem
        Caption = 'Layers'
        LinkSubitems = TBLayerSel
      end
      object tbxFavorite: TTBXSubmenuItem
        Caption = 'Favorites'
        LinkSubitems = TBFavorite
      end
      object NMarks: TTBXSubmenuItem
        Caption = 'Placemarks'
        object NMarksGroup: TTBGroupItem
          object TBAdd_Point: TTBXItem
            Action = actMarksAddPoint
            Images = PanelsImageList
            Options = [tboShowHint]
            Stretch = True
          end
          object TBAdd_Line: TTBXItem
            Action = actMarksAddLine
            Images = PanelsImageList
            MaskOptions = [tboShowHint]
          end
          object TBAdd_Poly: TTBXItem
            Action = actMarksAddPolygon
            Images = PanelsImageList
            Options = [tboShowHint]
          end
          object TBXSeparatorItem12: TTBXSeparatorItem
          end
          object tbitmPlacemarkManager: TTBXItem
            Action = actShowPlacemarkManager
            Images = PanelsImageList
            Options = [tboShowHint]
          end
          object TBHideMarks: TTBXItem
            Action = actConfigMarksHide
            Images = PanelsImageList
          end
          object TBXSeparatorItem23: TTBXSeparatorItem
          end
          object tbxMarksDbList: TTBXSubmenuItem
            Caption = 'Marks Database'
            Hint = 'Marks database'
            ImageIndex = 25
            Images = PanelsImageList
            Options = [tboDropdownArrow]
          end
        end
        object tbitmShowMarkCaption: TTBXItem
          Action = actConfigMarksNamesVisible
          Images = PanelsImageList
        end
        object tbitmPointProject: TTBXItem
          Action = actShowPointProject
          Images = PanelsImageList
        end
        object tbxMergePolygonsShow1: TTBXVisibilityToggleItem
          Caption = 'Merge Polygons'
          Control = tbMergePolygons
          ImageIndex = 23
          Images = PanelsImageList
        end
        object TBSeparatorItem4: TTBSeparatorItem
        end
        object tbxIconsSettings: TTBXItem
          Action = actIconsSettings
          Images = PanelsImageList
        end
      end
      object tbsbmGPS: TTBXSubmenuItem
        Caption = 'GPS'
        object tbitmGPSConnect: TTBXItem
          Action = actGpsConnect
          Images = MenusImageList
        end
        object tbitmGPSTrackShow: TTBXItem
          Action = actConfigGpsShowTrack
          Images = MenusImageList
        end
        object tbitmGPSCenterMap: TTBXItem
          Action = actConfigGpsFollowPosition
          Images = MenusImageList
        end
        object tbitmGPSToPointCenter: TTBXItem
          Action = actConfigGpsFollowPositionAtCenter
          Images = MenusImageList
        end
        object tbsprtGPS1: TTBXSeparatorItem
        end
        object tbitmSaveCurrentPosition: TTBXItem
          Action = actGpsMarkPointAdd
          Images = MenusImageList
        end
        object tbitmGPSTrackSaveToDb: TTBXItem
          Action = actGpsTrackSaveToDb
          Images = MenusImageList
        end
        object tbitmGPSTrackClear: TTBXItem
          Action = actGpsTrackClear
          Images = MenusImageList
        end
        object tbsprtGPS2: TTBXSeparatorItem
        end
        object TBXSeparatorItem19: TTBXSeparatorItem
        end
        object tbitmGPSOptions: TTBXItem
          Action = actConfigGpsOptionsShow
          Images = MenusImageList
        end
      end
      object NParams: TTBXSubmenuItem
        Caption = 'Settings'
        OnPopup = NParamsPopup
        object NMapParams: TTBXItem
          Action = actMapsEditMapParams
          Images = MenusImageList
        end
        object NLayerParams: TTBXSubmenuItem
          Caption = 'Layer Settings'
          Images = MenusImageList
        end
        object TBXSeparatorItem14: TTBXSeparatorItem
        end
        object tbitmOptions: TTBXItem
          Action = actConfigOptionsShow
          Images = MenusImageList
        end
        object tbitmInterfaceOptions: TTBXItem
          Action = actConfigInterfaceOptionsShow
          Images = MenusImageList
        end
        object TBLang: TTBXSubmenuItem
          Caption = 'Language'
          Images = MenusImageList
        end
      end
      object tbsbmHelp: TTBXSubmenuItem
        Caption = '&Help'
        object tbitmOnlineHelp: TTBXItem
          Action = actHelpOpenOnline
          Images = MenusImageList
        end
        object tbitmOnlineHome: TTBXItem
          Action = actHelpOpenWebSite
          Images = MenusImageList
        end
        object tbitmOnlineForum: TTBXItem
          Action = actHelpOpenCommunity
          ImageIndex = 71
          Images = MenusImageList
        end
        object tbxSep4: TTBXSeparatorItem
        end
        object tbtmHelpBugTrack: TTBXItem
          Action = actHelpOpenIssueTracker
          ImageIndex = 70
          Images = MenusImageList
        end
        object tbxSep1: TTBXSeparatorItem
        end
        object tbxtmPascalScriptIDE: TTBXItem
          Action = actShowPascalScriptIde
          Images = MenusImageList
        end
        object tbxSep2: TTBXSeparatorItem
        end
        object tbitmCheckUpdate: TTBXItem
          Action = actShowUpddateChecker
          ImageIndex = 73
          Images = MenusImageList
        end
        object tbxSep3: TTBXSeparatorItem
        end
        object tbitmAbout: TTBXItem
          Action = actHelpShowAbout
          ImageIndex = 72
          Images = MenusImageList
        end
      end
    end
    object TBXToolBarSearch: TTBXToolbar
      Left = 486
      Top = 0
      Caption = 'Search'
      DockPos = 413
      Options = [tboNoRotation]
      Stretch = True
      TabOrder = 1
      object TBXSelectSrchType: TTBXSubmenuItem
        Caption = 'Google'
        Options = [tboDropdownArrow, tboNoRotation]
      end
      object tbiSearch: TTBXComboBoxItem
        EditCaption = 'Search'
        EditWidth = 200
        Options = [tboNoRotation]
        AutoComplete = False
        MaxVisibleItems = 20
        MinListWidth = 200
      end
      object tbxDoSearch: TTBXItem
        ImageIndex = 74
        Images = MenusImageList
      end
    end
    object FillDates: TTBXToolbar
      Left = 643
      Top = 0
      Caption = 'FillDates'
      DefaultDock = TBDock
      DockPos = 643
      TabOrder = 2
      Visible = False
      object TBControlItem7: TTBControlItem
        Control = Label1
      end
      object TBControlItem6: TTBControlItem
        Control = DateTimePicker1
      end
      object TBControlItem8: TTBControlItem
        Control = Label2
      end
      object TBControlItem9: TTBControlItem
        Control = DateTimePicker2
      end
      object Label1: TLabel
        Left = 0
        Top = 4
        Width = 42
        Height = 13
        Caption = 'Fill From '
      end
      object Label2: TLabel
        Left = 123
        Top = 4
        Width = 18
        Height = 13
        Caption = ' To '
      end
      object DateTimePicker1: TDateTimePicker
        Left = 42
        Top = 0
        Width = 81
        Height = 21
        Date = 40830.000000000000000000
        Time = 0.496065717590681700
        TabOrder = 0
        OnChange = DateTimePicker1Change
      end
      object DateTimePicker2: TDateTimePicker
        Left = 189
        Top = 21
        Width = 81
        Height = 21
        Date = 40830.000000000000000000
        Time = 0.496065717590681700
        TabOrder = 1
        OnChange = DateTimePicker2Change
      end
    end
    object TBXFavoriteToolbar: TTBXToolbar
      Left = 388
      Top = 25
      Caption = 'Sources'
      DockPos = 224
      DockRow = 1
      Stretch = True
      TabOrder = 5
      object TBFavorite: TTBXSubmenuItem
        Hint = 'Favorite maps/layers'
        ImageIndex = 22
        Images = PanelsImageList
        Options = [tboDropdownArrow]
        object tbxAddToFavorite: TTBXItem
          Action = actFavoriteAdd
        end
        object tbxManageFavorite: TTBXItem
          Action = actFavoriteManage
        end
        object TBXSeparatorItem15: TTBXSeparatorItem
        end
        object tbiFavoriteItems: TTBGroupItem
        end
      end
    end
  end
  object TBDockBottom: TTBXDock
    Left = 0
    Top = 630
    Width = 842
    Height = 9
    PopupMenu = TBXPopupPanels
    Position = dpBottom
  end
  object TBDockLeft: TTBXDock
    Left = 0
    Top = 59
    Width = 413
    Height = 571
    PopupMenu = TBXPopupPanels
    Position = dpLeft
    object ZoomToolBar: TTBXToolbar
      Left = 0
      Top = 0
      Caption = 'Zoom'
      DockPos = -6
      Stretch = True
      TabOrder = 0
      OnDockChanging = ZoomToolBarDockChanging
      object TBZoomIn: TTBXItem
        Action = actZoomIn
        Images = MenusImageList
        MinHeight = 29
        MinWidth = 29
      end
      object TBXSeparatorItem1: TTBXSeparatorItem
        Blank = True
        Size = 3
      end
      object TBControlItem1: TTBControlItem
        Control = ZSlider
      end
      object TBXSeparatorItem3: TTBXSeparatorItem
        Blank = True
        Size = 3
      end
      object TBZoom_out: TTBXItem
        Action = actZoomOut
        Images = MenusImageList
        MinHeight = 29
        MinWidth = 29
      end
      object TBXSeparatorItem2: TTBXSeparatorItem
        Blank = True
        Size = 4
      end
      object TBControlItem2: TTBControlItem
        Control = labZoom
      end
      object labZoom: TLabel
        Left = 61
        Top = 230
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
        Left = 18
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
    object TBSearchWindow: TTBXDockablePanel
      Left = 65
      Top = 0
      Caption = 'Search Results'
      DockedWidth = 170
      DockPos = -6
      DockRow = 2
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 1
      Visible = False
      OnClose = TBSearchWindowClose
      object PanelSearch: TPanel
        Left = 0
        Top = 0
        Width = 170
        Height = 544
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
          Height = 535
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
    object TBEditPath: TTBXToolbar
      Left = 0
      Top = 240
      DockPos = 240
      TabOrder = 3
      OnClose = TBEditPathClose
      object TBEditPathDel: TTBXItem
        Hint = 'Delete Point'
        ImageIndex = 36
        Images = MenusImageList
        ShortCut = 8
        OnClick = TBEditPathDelClick
      end
      object TBEditPathSplit: TTBXItem
        Hint = 'Split Line'
        ImageIndex = 63
        Images = MenusImageList
        OnClick = TBEditPathSplitClick
      end
      object TBEditPathLabelVisible: TTBSubmenuItem
        AutoCheck = True
        Checked = True
        DropdownCombo = True
        Hint = 'Show/Hide Captions'
        ImageIndex = 37
        Images = MenusImageList
        OnClick = TBEditPathLabelClick
        object tbxShowDistIncrement: TTBXItem
          AutoCheck = True
          Caption = 'Show the distance increment'
          Checked = True
          Hint = 'Show the distance increment'
          OnClick = tbxShowDistIncrementClick
        end
        object tbxShowIntermediateDist: TTBXItem
          AutoCheck = True
          Caption = 'Show intermediate distances'
          Checked = True
          Hint = 'Show intermediate distances'
          OnClick = tbxShowIntermediateDistClick
        end
        object tbxShowAzimuth: TTBXItem
          AutoCheck = True
          Caption = 'Show azimuth'
          Checked = True
          Hint = 'Show azimuth'
          OnClick = tbxShowAzimuthClick
        end
      end
      object TBEditMagnetDraw: TTBXItem
        AutoCheck = True
        Hint = 'Snap to Existing Markers'
        ImageIndex = 41
        Images = MenusImageList
        OnClick = TBEditMagnetDrawClick
      end
      object tbitmFitEditToScreen: TTBXItem
        Hint = 'Fit to screen'
        ImageIndex = 43
        Images = MenusImageList
        OnClick = tbitmFitEditToScreenClick
      end
      object TBEditSelectPolylineRadiusCap1: TTBXLabelItem
        Caption = 'Radius'
        Margin = 2
      end
      object TBControlItem4: TTBControlItem
        Control = TBEditSelectPolylineRadius
      end
      object TBEditSelectPolylineRadiusCap2: TTBXLabelItem
        Caption = 'm'
        Margin = 2
      end
      object TBXSeparatorItem25: TTBXSeparatorItem
      end
      object TBEditPathMarsh: TTBXSubmenuItem
        Hint = 'Route Calculation'
        ImageIndex = 39
        Images = MenusImageList
        Options = [tboDropdownArrow]
      end
      object tbxExtendRoute: TTBXSubmenuItem
        Hint = 'Provider for Automatic Route Calculation (with Ctrl key)'
        ImageIndex = 76
        Images = MenusImageList
        Options = [tboDropdownArrow]
        RadioItem = True
      end
      object tbxUndoRouteCalc: TTBXItem
        Hint = 'Undo Route Calculation'
        ImageIndex = 77
        Images = MenusImageList
        ShortCut = 16392
        OnClick = tbxUndoRouteCalcClick
      end
      object TBXSeparatorItem24: TTBXSeparatorItem
      end
      object TBEditPathOk: TTBXItem
        FontSettings.Bold = tsTrue
        FontSettings.Color = clNavy
        FontSettings.Name = 'Arial'
        Hint = 'Manage Selection'
        ImageIndex = 38
        Images = MenusImageList
        Options = [tboImageAboveCaption, tboNoRotation, tboSameWidth]
        OnClick = TBEditPathOkClick
      end
      object tbitmSaveMark: TTBXSubmenuItem
        DropdownCombo = True
        Hint = 'Save'
        ImageIndex = 25
        Images = MenusImageList
        OnClick = TBEditPathSaveClick
        object tbitmSaveMarkAsNew: TTBXItem
          Caption = 'Save as...'
          Hint = 'Save as...'
          ImageIndex = 25
          OnClick = tbitmSaveMarkAsNewClick
        end
      end
      object tbxtmSaveMarkAsSeparateSegment: TTBXItem
        Caption = 'Save as separate placemarks...'
        Hint = 'Save as separate placemarks...'
        ImageIndex = 64
        Images = MenusImageList
        OnClick = tbitmSaveMarkLineAsSeparateSegmentsClick
      end
      object TBEditSelectPolylineRadius: TSpinEdit
        Left = 0
        Top = 162
        Width = 61
        Height = 22
        MaxValue = 100000
        MinValue = 1
        TabOrder = 0
        Value = 100000
        OnChange = TBEditSelectPolylineRadiusChange
      end
    end
    object tbMergePolygons: TTBXDockablePanel
      Left = 239
      Top = 0
      Caption = 'Merge Polygons'
      DockedWidth = 170
      DockPos = 6
      DockRow = 2
      FloatingWidth = 128
      FloatingHeight = 128
      SupportedDocks = [dkStandardDock, dkMultiDock]
      TabOrder = 2
      Visible = False
      OnClose = tbMergePolygonsClose
      object mmoMergePolyHint: TMemo
        Left = 0
        Top = 0
        Width = 170
        Height = 544
        Align = alClient
        BorderStyle = bsNone
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object TBDockRight: TTBXDock
    Left = 678
    Top = 59
    Width = 164
    Height = 571
    PopupMenu = TBXPopupPanels
    Position = dpRight
    object TBXSensorsBar: TTBXToolWindow
      Left = 0
      Top = 0
      Caption = 'Sensors'
      ClientAreaHeight = 561
      ClientAreaWidth = 160
      DockPos = -6
      PopupMenu = TBXPopupMenuSensors
      Stretch = True
      TabOrder = 0
      Visible = False
      OnVisibleChanged = TBXSensorsBarVisibleChanged
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 160
        Height = 561
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
          Height = 9
        end
      end
    end
  end
  object dlgOpenHlgLoad: TOpenDialog
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
  object PanelsImageList: TTBXImageList
    Height = 24
    Width = 24
    Left = 48
    Top = 176
    PngDIB = {
      1A00000089504E470D0A1A0A0000000D49484452000000180000027008060000
      00934FA1E6000054F14944415478DAEC7D077C1545F0F05C79BD2779E9BD9202
      09104A42E8457A9122558A5491220822A8542982521494A25215040421A02020
      0AD23B88107A0D81F4F6FABB6FF6722F242115E2F77DFFFFCFD5E55EAECCEC94
      9D9D9DDD9BA3A0122561E24E311EFCB17A08A792B1DE3DFA59577345CF521500
      0EC5C36CACBDCAB8E547AC1F21A2A4B260D0655D68347EDB023C5C27C07DF54A
      18D52102160D8B83095DC2A069B417B8A8A52020BE8E0D995D2504F8C0168A61
      27FBB8C86046BF5A30A9570CB48A71072997073A85081A867BC2D00ED130AB7F
      34F8E9E5E4910FC9339542D078C28E79A46509116EF05EEF065037D815D40A39
      641AF1AC5C0F4A1DFE2DA5402FB38214CCB074787D6859CB95A7A6344AE8923C
      E7286A4AB0871AE60D6908D1DE7278F634058CC63C50A855A0524881661890B0
      1484FB68202E26149C751A78BB6D20F8B9481D94841685C99640C8B760D6C0FA
      009C0D6EDD4F867C4A055AB513D8AC442528F23FB8696570FE4632224B055F57
      1528C40C8C6EEB0B9337263960BCF10205822AF60AF650818F9318FEBEF91032
      400B2E6E7AFE368E03B0DA38305BEC60369BC1DFDB1DCC1C0BFBCE27C3ADE46C
      F050B3E0AD133958252E8D02A2E750D34F0D1B7FBF095A8D065CDDE4C81E3B30
      3405169B1D72F2CDA0C44743BDF5FC03BEAE6A6811E30DC72F24C193B47CF052
      DBE1614621ACA49208F84E14E8A9053B9E4E37D2604BCD068342024A29128A24
      1004BE5E32FE660EFFA6905F769B056C883C3935176414D10491035652693280
      5B4FF2A1260A578B3273D2B090F4380B521816C48823D768011705805E2DE181
      73763B6466E582BB4604C98FF2E1FCAD4C00465F7A4F16A47F3DDCD709F44E4A
      18DF3114F45A05DC7D9205FB2EA581C1C2A124EC28071BB8CB2C906FB242B7F8
      00301A72212F271B2EFC9D042B0F678059E244C085397A77510AEE927F1E3DCB
      010FBD9A07CE33D35D033D513D0F5D7A0C17EF6483CDCEC1B57B06C8CC334198
      A702BCD5C8BAEC4CF8F1CFBB6063BD8AC17AC11609BDB157B8BF0BBCDB2108C8
      D16AC356B02C6C3F74017EBD920B62EC030693059E62CFEB56470DF57D45F02C
      351516EF4C824C19AF273F62EBDF280B01CF268584053727297CD42D185C9012
      9AB3C29C1F2FC1836C1150D83F0C662BA4669BA04FAC1CEA7A5170E87412FCF6
      C0192C8CB4187B4AB5A64277FF50251341BF865A6853D309D61FBC01BF5EB783
      542202339264365B203BC7000D7C00DA8703AC3A61830CBB863C3E07817F54A1
      B976B0CA4DC542A7703B7CF3672A98683588280BD8AD56B09B8D6031E483338A
      89D1BA43AE4DF2026BCAB5A6C28D735272ACB0FAB819BC655258D8A306BC13E7
      05EDDC397037DD46A3A70383CACF017C4E69C0CB1D0F0452C3D06CFFC8C8B450
      A34600848535002FAF5A60329AC16CA71C034E5849B6142D4C6927870D1B3639
      2626A6A936EF6A44EBF098DE6D1A86B07611C3DE789807F79E3C83867523B818
      178935F9F4C6B3F0E88887BFBF7FBD3B77EE9CC747AD95A2A05EBD7A9F8C1C39
      F293D1A3477F121424730DF0F39138C99DA14E881B0CE8D2005E6B1A47393B81
      E8830F260C9E83A54D9B3633F0B166A5C1624B3BB967CF1E3871E204FF5B2CD6
      407EBE2F30CCF35B8999387AF438646616F4A7DBB76F9383122A5B9C9C9C16A9
      D5EA1D2A956AE75B6F4D499E366D5DCEB469DFE67CF0C1373953A6ACCE79FFFD
      9539EFBC333D47ABD5FD261289B662FD1E1F6B0D552822A145A406636D290028
      59EB6055635538CCE87FA5CAE5857EF0D34F3F897BF5EA15FAC61B6F347EFDF5
      D7BBF5ECD97378BF7EFD3E183060C09CF6EDDB77AC5DBB767E7272F2CDD4D454
      5BA510FCFEFBEF4D11D05B58468D1831E263A552F9A98B8BCB58BD5EFF869B9B
      5B4BAD561B2D97CBBD14589C9D9DFDF0FCEB5151513D6BD6ACC969349A6B4949
      49E5FAA7CCB871E30E4747477740603511B02B221089C5628A14ABD54A110FC2
      A1FBE480D76844A27775756D877560444484D2C7C7E7DADF7FFF9D57AA2DB261
      C1C631128904A45229D8719C251581F3D501DC514921E709A2808000AFFAF5EB
      CF888B8BBB86BD7E598F1E3D824B5A68FAC9932729E424F1120860994C064805
      7F242319839E1C4D175814729E9C23F791FB0922BC46797979699B3469F20E22
      BA826CDED4A54B975887196290FF7DB12706201594D16804FC0D4F9F3E85ECEC
      6CB0582C852D21004D26138F80509A9B9BCB5F4706F0941144282F363838380A
      8F6FA125F0B87AF5EA411A1F7A8CADB55EBF7E9D0792999909280F1E90832545
      9110C06969698000788A1CAC235492679023D4C3870FE9BCBCBCA6F84810D3AC
      59B354B4396FA2C018543FFEA1ACAC2C4081F3AC7008D9C112D26A5209858462
      A41CD2D3D3E1D6AD5B70EAD42978F0E001E4E4E450C885FB587E62C78E1DFBD7
      F4E9D3A7F5EDDB7781B7B7378D761D501D494B00490522FC67CF9E150ADEC122
      72FDD1A347BC257528441145E09052031ED3890DB6CF9C39731902F2472D78DB
      C3C383BA77EF1EB1A83C10421122E659979292C203BC7BF72ECF7B41C885AC24
      327214A4E21639388CBC79C182055350637C51033A1224481E6047E2F94DCA6F
      BFFD56D852029450C43F882C24E788B691FB91F7444E1CB28F3C682F1C4590EF
      B99B366D1A8A94ECC511AA0EB282427300281F1E48D19692D693D612A08442F2
      1B5BCC1F895CB072D8B0E4D2865068DDBA75C88A152BEE242626DA8F1E3DCA6D
      D9B285FBEBAFBFB8CF3EFB8C9B356B163777EE5CEE934F3EE166CF9ECD4D9C38
      917BE79D77B8A143877268ABECA8F276EC0F77B067EF40506DCA9CC576EAD429
      6ED5AA55A9A815F63FFEF883DBBB772FB774E9526ED9B2653C92A953A7721326
      4CE0B0F77283060D228073636363CFA2727C818F0FC45A838CB6E5CE9F3B77EE
      DCFDBBEFBECB2308F6EFDFCF2D5CB8909B3163063765CA146EFCF8F1F6214386
      D83A74E8702B2C2C2C113BE7FBF84C73ACFAB23C95520D61F7EEDDC7AF59B3C6
      42D8448063AB09E09C8484843368EF96E23DFD895F8455F6B2E385A863C78E1F
      8E1933E6068E1157D144FF8CAD9D24B8282E55696D7985F886D1585B080E80F4
      BF71FA5F1FF4A528D03A38E87743FD1E85BF2760279A3670E0C0D938E8F7437D
      97A091BB88BDDCFAB283FE0234D523500DDBE3B81083F6C51FADA7138E195234
      807AEC4CAD71D0EFF1DFA0FFDFA0FFDFA0FFDFA0FFDFA0FF0A833E8780391CF4
      CF56FBA08FFCB6CC9F3F8F8B8989E150012657B6B57405D72D28EC851249F437
      9D3B0F66FDFC1AC1EBAF4F806EDD8634C46BA423D95E1501DF7F3C3C82D28BC6
      8B0C068B4765BD8B72E345B56AD56AFCD65BF3B7D5A913D855AF57B25959143C
      7E9C028D1BD7F56CD3E68D7719C612E8EBEB15F5CAF1221F1F2FA9C924C39E2A
      86366D62C8755AABE5C4FFC58BFE97C58B3E993B97DDF4FD0FF5F7FEB26FD0DA
      F51BEA2F5CF499CBFBEF4F615E4A4D8B96691F7E480D1A3438203232EABD301F
      9FB9AE39D97D741E9E3DF43EBE9D5CDDDC63DBB66BEFFC5ADBB6547C7CBC0955
      DB7CFCF871AED20856ACF84A874E59FF5AD1319F7B797B77A3B3B335F64B9768
      D039C9A56E6E1E2AA5AAB6D649D751A3D1F6707276EEE0AA778B79AD7D7B5DCB
      96AD384468AC57BF9EA5D47EB070D122C9E62D5B5AC7C5C77F5FB76EEC5234C3
      35513919CEC515EC6D5E03DAC70738AB0DEC361BDA7B8A154B247A0FB3B941CD
      ACCC51613E7EDF46D58AFE35B446F862954A1D516C40405B4F7BFBF884070606
      8EF6F3F2EE29B75A9D69958A32631FC8C93342BEC9C247DE6D19E9E0F4E701C8
      C4DFA9F1096026BE11FA6FE2F454CE121C6A36817D6FEAD394D90F1FDCBF54D8
      7BBEFC72B9BB8FAFCF00740787A3371148DFBB4F5BFE380CF9DEBE60894B80EC
      7C23E4E69B108919CC59F990956F815C3404B906B250C401A7D07056912C23FB
      DEC3CFF3F332BF9AF9F1B4745E0638F4A9D0B3EE101E1EB1842090C9E42E1445
      D3565604592209E439EB210FC8AA07B60BFD25AB01C75C1C9FB33C7C21D7DD13
      8C9404726C34DC334AE03AEB677842FB663EA34315351AF4CD7551344C278E97
      06795C4322917AD8EC9C1C6DBDCA6A0726D76C05934A431C527E61C8F63405DC
      F7EC00E6D409C8080C012B22210E58BE5D047FE788E08E2C90A2DC03E532378F
      9A12B934C0929A7124F741CABD42190C7D7BA9DC27C8AF47442DAF7960CBF714
      D11CA8D1A3904925BCB760CE3380EDFC593020DFF32362C060B3C3A36799F0B7
      590799AE3541A4710174FA6D747AC61FE6A49B938C37EF9C3FB0B10FC723787B
      CA0EB7C868EF11AE6EE29134CBB9CBE40A8AA1EC60CCC982BCEC0C745918A1A2
      3F44B1C877135C7B9205378C4AC8116981D387E2C82336D2CFD236C3CD1BD30F
      2D6E7EBFD09ABE336D4F7C741DF771EE9EF2CE626C2E23925086FC5C50A2CF49
      594C20625D2035D704B99C041EDEB90522A9021EA5E5C2C37C0A58373D64DEBF
      CB9953F232995CDB1285915B7A7445ABAC621DAD56BD5EB968A7AE7294F4AE58
      A662E56A8D4AA6504833539F90953E8A9628F8751A03238574F40C9FDAE5287C
      1DA43D7B02A9E85A9A19AD2D2F357BBB292563CD95BF4FA753DA0656EED9B1E7
      E3C19ACFBB92E5CDB3A40E1AB7E36BEF007DB8C659D5CAC54D3240A59385D969
      F4F072F221CD888E262586474FFE814C0BF6091735E4E63D40D53DCC18B4ED7A
      1854CE0912F7B86B4C9EF1345573FD296B46D665C383878F8A0D386B9776CB09
      1BBAF76F8F90945095FA2A9DFCE4284893DD21C0A5314834EE90669742964E07
      460ED5F4E969487EB40E3CBDBA52B966AD1C47A1405A240E10AB146DE97C6532
      97F9F81326EBECBA42E08AD6DF5311A3F78577FFF6CA37ED0E7C92EDBE25D22E
      FA42C345AFEECC4D491CC60DF9A113D76AF9482E7AF112CE6BEA084EF2969EA3
      DF51721E9FF4E03453F772ECBB7B38F9845FEDAA513B1ECA9BBF3F59E212E286
      6E3EC553E0D2EB27A577A8BE7B50A4DB24BD5E1D4EC11BB415B5E38EF301A8EF
      3414C4121328D94710A23E0D39FF2C87ACB4A76095A047AD16A33268213FC30A
      9CC96EB766A7FD63BDF6E702EAFAEE5DA69C145ED88C4B9F9DB57D63DC3FF2AA
      E1325922117971768EA6D1E9D68B3D20D459825A64858C1C2DE466D400677120
      B8C9EEA0AB900D1936EC17D80FE5A29A9093EC6FB5243F3E4A5DFE651A7D63D7
      AFA6BCF4DCC2F1DBA9D3CA60671F59538D877B07ECD2B12AB5D25D2A6659A928
      9BCA93AD8764D363C87896012192F620E7EA4253EF7F203D6B031CC09EFDC8DC
      88CBB9D7DCF8E4F2E35DD4B55F16302927AF988C464B694B8DB43CA4934A1B52
      3B58E717D244E3E1D146AD53C7C815E0A291D14C1693489DCED80CC1EC2868EA
      5C039A06EC80FD0F586EDBA1F8ACBB67AFAE856B895F3059D7EE1AC9FA7C85C1
      21A59E5145F674F2E938333E62F8DAA98D3E4EFCBDE3AA6F9FC5FD30C2D670E9
      06AEF68CFDDCFCDD1BECFD57CC78A06E3CFE3DB153302FCC978B44C935AC2AAA
      8F3EB0CB27CDA2477D3BDB63F8E663CAC1DBD25CFA7F7749D770605F89D25955
      6D83BE58ED251285F7F310C57FD88A8DECDF402253FF6B912FAAA25D3DC56EDE
      9DB873001EBE44DBAEAECE56A0B7978DD3E5775802BC6387CEEA7F815275E29E
      5D5FB28E96379F7FACCC3B75222B7C332216744A7EAB0F64E2E0D3FBCB33685D
      CBF67A4E7D1C4F463C75A1B1CBC82FFD46B998822F874517022745AB90C1A6B7
      EB40E72F2E435E05AA5FE10C67D3D050F074D6BC70DE59A5802DC36A54C8A70A
      11FC7A32A9EC6BA7AE83AA823E4697AD8B1CD4D059E0718611F69FBEFEC2F523
      976FA3AF6482186723E8E4554440531C44385B20C4C90EDE4E32C8438FEEE6E3
      D4C2EB4F3373C16EE790756A08726621DAC9047A3957310222D0ED234361502D
      0E82B5003E08DCDD49091E4E2A444AF140F9FB2422F072D180975E0D9E387406
      3A8BA05F8C047E1D1FF502CBE8A2C0778C8E82204F1718DB3D01EA87B9F1C05D
      11B89B4E09EE3A4442173CAC9449F8D693EA850862C3BC6050DB58BC5F0D89E3
      628A212944F0F3989A08E8B9EDEAD0301CC2FDDC7820EE58E5D2E2B10DF2B727
      5251C3D715E2A3020ACF6B5085F78CAFFDE22C53AF79719218E2AD2F57436462
      115F5FE8C272699566FAAF540A293059AC2011159F36131751860265E8D2DB41
      F676E51A4D3C5B8A16E2E2CB25E2E208DA7F76127E79AF01E070CCFF7D36E921
      0FC003F94F2ACB144762C1194E725A365FC9C4BC7EB86F21F0D73E3B0B47A6C6
      15F4A79F766CE3BA75ED0E311F1F0367D4E59FC7C6C2EEE3572133C78880557C
      2582F640813A9098915A1E787A0E1EB1A66683522E41C5A8015D975F821C2307
      1766C5C38E9DDB8B23701477850D6A3A5BC11FDD160782305F3DE854055D362B
      D70057EFA520E01C788C88EEA499E14A9A0852F29F53E940506AACE2491E031C
      7AF676CEC4477C497F7000E7555129E33BDEC3D42CB89D6681CBE96248CBA7AA
      668B486B2EA489792117D573476910E187C295C2C5344999C02B5453F2608FA6
      35CBBCDE312E1C320C9554D3B2CAEB5F9E83FD93E28029A145F946332FD04AF7
      83F2A868BFF018FCF27E239EEF0E15EDB1EC2498AC0CAA750508B06493DE4DA4
      5E698F816560EFE4C615B79E6573A8B5EBBE1DADD3E9E6DB6C3665759A089C30
      E6A5A4247F5CE18DAE8BB904AC912F8DA832FE0DD6A588A41B56D9BF81E006B1
      DC640A87F56B4452072B539D081E13244C01252456FD9310C3AB1E044FDFA5C8
      0ADF691B3A9A4CC1FD9EC45A94212FEA6528208504511D0B3464592A5500483B
      80BA7CCE91ADA45DF1EFC00A11E04DC125047A15EB33E137310EB9789DB875FD
      B08EC4DFBEE80F908E310AEB70FCDBBD220A48AF9B8337BA913FD05B2172482A
      424123AC6F619D887511D6C5C2CA472BACBDB1128D139587E06FAC7DB17E8337
      4660EB886BBC1BE5902D5CEF43823428935A58891DEF2084FA4958C28750212C
      5C9489E096A09EEDB04E420AC285C83AD9E8FA08ABBF0098124297646301B9E7
      A670AEA6B0C9A074040890CCD267613D4C5A821410445D85859FD36584915D85
      E303818DEA3211A44EA03854CF0350B049FE73610F450259A0125698A4A54478
      2921D63D985879ACEBA0B293399403E9585BA1802D8E168633259E47213C2493
      256CDCCD4A8F0742B9846CDB89ACAA23088FF48B1B08308A08151189F0375184
      8345D4192AA44050353220D716C8FF10AB1F018E0889C05B2052B3C0EA6FB1EE
      C4D66797DAD1B81933A6630D2B029C98E83102CF89B0D7635D89956CD1D72360
      25562258E261A904F3A125BDBBAC9E4CD661AE1541424C03E1E9AFD8AA14AC16
      414D7761BD8314DC15FA8B8429A0F033AC7BB14E45244EE5C98020A941BD4B5D
      17F4BA98D78E95F8881BB1AE1128F344FED7630A5437127F4F16CE1F2DAFA315
      A5A468C916F47D02B22718A9226EC5DBC49123D6B648A36595B1A62F2041B6E4
      09E68018B37A82393F2B18B92B0212BA683FA9C85C174382AD263DFC3BAC0451
      63E4352B20392920F9478029ABCA78D0BBC8E0430CDA3758E709FDC1A5C87DC7
      0476DD288AE0A52255C258F10EB2EC4F342B274B8C68644550828DF9F595FC1E
      BF553FBAFA7DBDB96629E719ACCACAB6F4957CA2CAC8E0957CA2CA2078259FA8
      32085EC927AA10C1ABFA4495ED0755F289B063B2E58E07C42722033BB6DE20FC
      4D64701029F0B115006F8F35473015441DF7445B1F0FB450F48878EB3DC578D3
      11AD9EE3B77BFD8E35917E159F88E2B8CFEA5BEFFF2503CBAC7F6837AF7DA230
      ED5E5138BA1F3C58B291E333FAA57D228E8BC62995EC22E3E98D935E74376CF9
      0F280DF7BEB4FDC578E5E85861317B74692CD20A830A69E97AA460212221AD21
      1B2A634867659EBB288295E59E72147557601B09434E40162F2B55C815F94432
      EEC5083102D797E613958AA03C9F28C4F6AC53557CA24A5953874FC472368D8F
      3D8B465EC375DA0598123BD84AF3892AEB175D6A64B99D9249CB75B70860B09D
      028A4AAE8C4F542E02E21389396BE054C3BED51ACE5463B1B409FA2B8419AC16
      E5B48FB00EE573DF568E4F54DE042452CA59267C66D8BDBD87E572E35E968BC0
      70B6CFB0E586AAF84465CA60FD27BBFAA9C0F45D07CB3F2211D8096F63DD34D3
      E5C250198214AC1354B70B53C07FABD0193793BD0648497A218BD076BC2D5CBC
      23748EA16039D746B89E48B4899A3123DF75F1F4C8AAFA440E192C2F430CDD10
      F0CE327CA283C427429690C6AD46C03182AA16F3891C32184B6E122616DF0983
      B6B404F097F289AAE4550823D908AC7389C610562202AB708D989555C2E8D71F
      CFFF58E5C0ECCBF844558EFC221232F01023F613B2CCAFC879C29E3F89AF44C2
      7DAF1C0B2ACB27AAB25F541DA54C17E4F8F1E3FA0F3FFCB0E7B469D3E68C1E3D
      FAD3FEFDFB7FD2BB77EF996FBEF9E68CB7478F9EF6DEE4C913C78E1BD7BFCB1B
      BDC2E3DBB5CBF478ADF5D373BB76DB2AB445EBD7AF672323239BE9F5FA0F9293
      93E3F7EDDB27397AF428D91D4B194D2662FB41249389F45E9E9ABAF5EAA99BC5
      370A0FF5F27C5DC430DF3AAF58FEE5FCB747A79429E43D7BF688FDFCFC865014
      B576D3A64DCDDF7EFB6DE9DAB56BA9ECEC6C2A202000EAD4A90375636321382C
      0C2C340D3B8F1EA5A6ACFC9ADE76F4A887986527F968B44B462F5FEE572A82CD
      9B37A389A706984CA6B95F7DF595E7CA952BC9E66D48484880A0912341D3A307
      88DBB7070522D287864238228B8B6B086A4F0FD871E532F5E385F3E27C9BF575
      4F969D3368D54AD7171020B046F9F9F91F6FDFBEDDE9B7DF7EA3424242A8F0F0
      F0C24DDD850F480A5EDF362172AB5C0101C1C11088F79D369BA8535959225622
      7E3D98A2070E99339B2D44B065CB16ADD96C9EF4F7DF7F7B93BDBD644FB59BA7
      275859166885A2F8C8858039A51224780FAB469B2B97838BB70F28BDBCE004C7
      51E9342D534A24433CD49A90422DEAD3A74F3BA3D1387ED7AE5D92F4F474CADD
      DD1DC41A0D280810A91498FBF74191910EB2DC1C1091ADE9123118452C7F8F5D
      A9820CB27D5D26833486257BDC214226D762CB1F3D886B709C5AB56A95D2C5C5
      65C5AD5BB7FA21EF69B2519B542FE4334140234015B2C542363021609B0AC717
      4D817B6AD1A8E15CB316CF9764CE9E05515E1E8C944939B9D97268E7BDBBFD68
      B95C1EC9B26CDD7BF7EEF1AF02908DDCFCB17163B04647833932120CF8B78DBC
      974FF63B22420B55BA8D94913DF1480DF17B947259207ACA4134C330F550906E
      645BBA63BF3BD9EE5C6CBDC666E3AFF978798212A9E218B6CC48BB0D9FCFC7C6
      C82552B586617D1036ED830FCB0C0643A1C694D41C3B5E63F1419D480C2E1219
      786055E85C40A5D0429DD3E7A1D6F92BE07FE62278E75B21C046831C9D6EA958
      CCD0168B9CFAE1871F16B9BABA8EFAE28B2F64172F5EA4B007F3CB2A64733DA1
      444DF88DC025A82D52D41E2343F3BEBC15AF7128E82C649F0587951C33F6729B
      1DB2333361588B165C9DC0A0B41DFBF7BF4D843C292222E2830D1B36687FD9B7
      8FF2E9D9B3B0E58AEBD7C159A7031122246CA2D105B021755644284764CFB2B2
      C082D7320CF978B4436E7E3E3068C7678D1CC5B9A8D5B7D66CFEA10F4536BC36
      6FDE7CF9E9D3A783BEFCE61BCAAD6DDB42044E64A32AEA3D6F7B496E04F29605
      D9A14F936D710582CF331A2035330B58449EFCEC19D446A59833658AFDCE9DDB
      FB3E5BF5F500AA67CF9E6E43860CF91AB5A9F38CF9F3697354542102255240B4
      86CBCDE5DFAC60F037D9F74EB48C83829DB3E9C8129B1D5B8FEA69C4F3E3478F
      E6BA76EA64D9BE63C7D4599FCE5F4221607ADEBC7983908A2F762526CAD66FDD
      4A29907C25AA5C1E0296226005AA2EBF430D7F13F664E99F2FA232494960371A
      E1E1C387BCDD9A3E7DBAFDD9D3A7D756AD59DD73CB0F9BAFF20A8D76DE1BEB06
      147093152B5650478E1CA14867233BF3498B096B88C019EC6456E43317F97C6E
      6EBC7205321F3D82E0A02018FFEE782E3020D0B06DDBB6991B376E5C7CF7EE5D
      0B8FC0CDCD8D4673D1B95DBB76CB917C8FEFBFFF9E0C38E4551CFEF5006255F9
      79009A03C20E3AEC799427EDD83108F3F5051C9038348E366CDCCE1F7FFC71EC
      891327928BE97A585898EC830F3E98B07BF7EE54B449F6C99327DB1B3468C0F9
      F8F8705E5E5E9CBFBF3F171018C8F90404709E78CE1B8F75F13AD9C18F00ED87
      0E1D322F5AB4685FB366CDA290EDD40B43665A5A9AF5C9932757C852A5B3B373
      446868A8064736F27A0645AC2B3121F820B862FFA8811424C4C7439B56ADC85E
      78B203241729DE919898F821B6FC2A72812BD3F172727292366AD4A8496C6CEC
      7004DE44269339D9ED769ABC8661271BFB0A5E87E1F0C8E1E044DEB6FEFBD2A5
      4B1B4F9E3CB9EDCE9D3B64B8E42A150140E0AE1DB08C1933E6D3993367EEFBF4
      D34F2F2E5CB8F0DA82050BAEE0DF7F8E1D3B7675F7EEDD07216B435085C52FEB
      75100AC9DBDB7E3846D4457934427934D06AB56430D14235BD13F83FB80C1830
      804331D48657D8765EA1F38B5A75AE6DDBB6BD5F16498508264D9A046842D6BF
      2C920A11D4AC59F39590546A7EF02A482A3D01795924559AE1BC0C922A6F9EA9
      2A9297DA9D5315242FBDFDA728123266954452AB562DEAA510ECDCB913701AC5
      57B4AE8E1EFFA0241234E15CA5D7D12E5FBECC03C3F11A7C7D0BB6F9A01FD543
      58A8B0086134851076B35489450EE079797957704A55308DAA5B97B47A1B9922
      4341BEB5EBC2FCD85C651910E0383199FBD34F3FBDCD47CB6FDC20EF4AF1D790
      FF64392B57409459B2F5954270F3E6CDC9E7CE9D23AB1977908A69B367CFE6C7
      E6F1E3C7036AD06F785EF72AD6562BC425BC05202262C2719AC591427E3769D2
      64B270DF4BA9699EB01A9822906FC9C8C8688F7369FEE2D75F7F0D38ED5D501E
      1515BE515744530A4260898924A60A6BD6ACE1595564B15A51D6A05EE552BF7E
      FD10F4260AF789A2CA92CD02970561574B1109726920AC88D4294F0EAF82442B
      B0470BFF63D30B4DEC12CCAE1E5D3BB8DAADA9A3B46C55BB43BBFE9DFFDA3EA3
      45D36A45D0B68E1BFDF3E2EE5D62EB47AC779252AE4DDA25EC3CF4E5EB03578E
      8A11BD320204CE8C7FABD9D0966DEAEDCC4A79AA3E7DEC12D0F939DA9A75C2D7
      8645854E2C8A847E49E06F2534ABBD92361970BEA0C6A9AE120C389F33A63C86
      80E080790121417D5F1A4151E056F2FA3656BD930232D2B2213D3D1B9EDDBE0E
      EEBE414BBE1BDFB06195112C1DD7A466A3A6755652A67CB0592C603599C066B6
      80D968E2F7DE65E59821233D0BEEFC73D9F4DDFE1BA22A0B34EDFC82D4D3EBFA
      71D9C7A770E97F4CE052F68DE2EEFF3490BBF25D77EEF74F5B71FB6737E1B67E
      98F0A4739C577F470C9BAA1ACFF3E1D2F9EBFCACC3D74B07668311525333E1FA
      8DC7E0A296C1A36CF3FDA5DBAEBE77EE76D66E28D8E0513E8B48CB8B0227FC0E
      F277030F5715E464E6404E761EE466E783462E86875985C0F73A8057581297F6
      E892FBF77C2EFFDCC75CF609812DBF8EE41EEC18C49D5CDE913BBAF835EEE70F
      E3B9EFA726DCAB13A8E95996C92E5DA063E36BA69D9B9D5A14F8D3FD6FF3C0AF
      AEEBC99D5EDE814B9C9E40789E521E70AA3C9E3FBBFF80EC7D06B5428A1A63E4
      797EFF7E0A49E5094F9F66818515DF9FB5F6FCD04BF7B2C9CAABB542535152A0
      4E1A0588599AEF440438E139CD71F0E8713A0F7CEE860BEF21F02365012FE678
      9526505259B0434E9E01D2B02391D4974F9F65A102CA78E0AF2CD0A7A8E70E9E
      1381EE419E6F9BDEB44A0265DBD5756747F66FD421A169CCFA5B9749084D0132
      09CBF75273BE81D773439E11B233F320DF0629B33722CFEFF26C31565661947F
      AC1FB23F0F5B9EB4631877766DEF4255FC1B7BE86F739B73BFCC28D672B62AE6
      85BFB9E99BDF7E74EAFB211E411EAA2839CBA1AB78173C5C54904AF6B5DBED90
      6186FB8B3617F2DCFA32265E111FE9DAF19F6D439E91965F58DD8DDBFA4143EE
      D0DC662FD789CA9A73378F76EB73F6BB5ECF4E7FD9010D57536EC3A40629D505
      BC1049C31ACE1D77CC6C7E79C7F426C5AC627516D2DA06E1DEAAF6F02F665B92
      C12BE4A0286E8B5672CF43602328AA5A9A5704667104251139AE55F5EF32DD16
      7263512AF0B79B8662DD368302AB1C7F1754F21B6B69F797E4025D78A27CF690
      F838D91942360E2708CBBA0471E9CF14815919AFC22E78CE1182BB4E16ABF957
      2A527A571CC2A44BB61E6793ECF6EDDBBD376DDB18BF76F3B76FCC876FDF9B24
      9AF7413DB671074FCE374A615579828552081448B0964E890093FEFDF7DFA37E
      FEF9E77E38B17BFFE8D1A35FBAB9B96DF6F7F75FEE11E0F6812648D55FEACB36
      6175946F2E9B25924A64727F59B05F4368D621EE719B6ED22C05D92AEA595E4F
      A7351ACDF82E4F3A6F8C898949C06911CEE9FC243A9D8EB6B216B86DBC0E2773
      FE848BC6D3FCCD1A468B239C18EC1AAB4AE3AFEC5A5FDC646AED478D7B79DF0C
      D597A5AA244915FFD660C924550A92A99B72063387A6D47207E4B412E40C0EA1
      9404C7853C488607E0EDE4236FEDD3A1692FD59079A3478F1EDEA3470F8F1728
      78F2E449A6699039B164922A0FA517842B6B42A0341444941824347957590C22
      5ACC23915232F0A742C19BF18740CF2005CE973BC6C5C52D1F3162C47B5DBA74
      0946191426A96A221289DC4A4B52455969606916B26C199067CF05ABCD0A1C65
      0725AB025F088230AE16B03651D124554C7070B01F1E5BABD56AA7AB57AF5E24
      49AAD2C3FFACD1A9AC2455AEAC07345034010FD61B291181D82E014FBB2F34D6
      B50267A54B9949AAB6B6FA916C0871A7D3D3D3F709810D205997C8CD84028284
      B00A9903EEAC17E85977F0647CC195F604270E659A4EA3DB98CBDF433263616B
      C9249D4F54959292E2505B8E1E3B76EC3FD31FCF78DD6AB5DA49AE2892788A20
      21F9A388D049C22A196A61201B06524E066E224F08D7D4226B8AFCC2D0AFBFFE
      0A67CE9C297CCE91A42AE6ABDA9F8290438A9B3973E66ED42257D4820EA525A9
      F2F7F60755A6121E3F7800C664335CB87DB93249AA9EE0C1C00A3A6B5930592B
      6318468F1A50BFB42455077E3B88C3BD04582B83745B2A4C52756BF2CDCF5193
      96174D5265DCB469D31748C98C366DDA04BD6A922AC702FA0B1DB075EBD61E2B
      56AC588302DB85A663F7962D5B76FFF5D75FBB3FFBECB3DDB366CDDA3D77EEDC
      DD9F7CF2C9EED9B367EF9E3871E2EE77DE7967F7D0A143770F18306017AAFC2E
      EC0F6B7C7C7CA642C166EFD24BA74E9D6AAC5AB56A136AC4AE3FFEF863F7DEBD
      7B772F5DBA74F7B265CB782453A74EDD3D61C284DDD87B770F1A348800DE1A1B
      1BBB181582ECDE6921044AD852873847E9DCB973FC77DF7DB79520D8BF7FFFEE
      850B17EE9E3163C6EE2953A6EC1E3F7EFCAE214386FCDCA14387D568BF3EC6CE
      D91D0ADE9A50175AE862304B1B36F1C6EEDDBB775EB366CD4EC226021C5B4D00
      FF989090B018FB0D890C36C3EA05A5A5D42A84497E386A297B5F3A76ECF8C698
      316356F6EAD56B45CD9A353FC4D6929D8051C55A5B1A70A15676D00FE001AEE4
      2E5675D0674B05FCAA7F17735BAA58E2365C8B7111D3035CA54C0B959856511C
      75C3CAC1162B47EDFCAAA35FE60B3CAE2CE0886FFE5687F51C33BCAE8B647A13
      2F79EBBAAE72CF3027A9D65723F6759632090C05B51AF67DF7F19F1B163FA832
      02AF15E75C7DE5F4923E219A09ED02359E013A29E5AC10D99CE422BB9B524C7B
      A9254A4F95A8264AFCB5D837C6A5367973E295C3EB3FAF5CF4BDC692BFA45AA9
      7DF6D008A7FE75DCD516ECFB79468BDD2641DB6147FE10F326616946AF14D954
      12C64BCA508B6FA61AC9A6BF9F2B2503FDA787BA8E8BF5D9D4AD863B6BB6DA0D
      262B6761188A62698ACE33DBF815798A015A2D65453A192BCE32D8A4171EE59E
      7E966B787D628B8087E552E03565A738CC453EA091B75666E7B86C6CBD056DA0
      5DCC508CC5C6D9710ACD6FBC46B7C19E65B09A250C4D2B258CC55D2D8949CDC8
      27BB6BBF2A570639713D3DDB84BA4E69E6E7A2B47360C456DBC97BE82C454C2B
      FF950BB2FF93267CE0EC68C5698A928A28DA6A07497ABE397FD084A9BF94EF3A
      4A686727A948CB329445C45076110314B69E62B0E5628426C2010CD9C5917D2F
      780F25A2C9461BCE5E702FED7BEEC815B7F2B5A8514FB7045F9737EB7A6A1886
      A62CC8018A7C1384742742036931D92348237029BA1F363BC76F19B471207E9C
      6E48CD4BCFDE533E82DA9DD9100FCD1BF1BECE3AE4AF990067F8C4A464E19E2A
      F0DC687E24A62DC84393D56E93B20C9367B14BEE3ECDFDC79E95BDAD5C044D5F
      1F6A7C64B3D78FF7768A71554A500660E7073641FBF8CFA7A0AF433EA162B573
      76444E4B448CF47E96917EF838E3D7CD8B26FC5CAE0CFE98D2DE72FD59EEC6FD
      B748048493A190897A7258C99BBE7CE107528A1730F9E88B28C76C93DC799AFB
      C49E9777E0CA85A3791576344376D6DDB5A7AEA5F96AA5813D223CC9E61C63C1
      E655C2A1824A5E2B56886889C1CAC9CF3ECC36653CCDF8596E35FF59B1A918B1
      34D09E93BA3C272BBDCE9954234D8B64621F8D4C2C61519DA08000A24CF8539E
      9C6B911FBD9B694C79949628371A3E9D32B8F183F27BF25B0B0321276D25584D
      AD40AE014AE5C44506043DA8EBEF61ADE1A6F6F252C94412D4D17C14C2936C93
      312D33FFBE28277797C66EF976CAA046D7CB37D73DA7048221672598725B8142
      8376C0858B0A0CBDD1D9CF6BA1D1465D7CCCD10D39860D17D16821D08354D96D
      37B59CF5A49882F3EF0F6A945B3ED37B7F1800AD07FD06F15D39683590A37BBC
      67AF396DEDF5A9DF1D7DEBD375C70A27E7899B8E48D7AEFF53B572DD9FF2456B
      FF62CA9E88172D5DC6225BD25742D6D3562053E1F447CF4545D6BED9A146C402
      2786D9346960BCB1AA03D4732DEAF771003CBD8FC09FB502B10C68B99A8BAA51
      F346C71A119F6A19F6FB4903E3AA0CFC79E93139043A8E3E00F1DD3848E8C1D1
      AF0DB6D79AF865D2D46FFF1CD2A8E5EBAF1410A1E1CD39913873F802CC792D41
      A1064AE3025151B1373B44D45AA065453FFC75F027E3AB2150BB744457398EA8
      22A375E5A223EB5CEB185173DEB1AD5F7D3FF9CD38C3ABC645185DEBFE563462
      B54432A56BAD80907FDA050563CBD9CD6B16BD6B846A28BC16F55F71B023EAF4
      405F89F8B082A6BE9B3C303EBFBA024FFFC58BFE8B17FD172F42328BC68B70FE
      D5B1ACEEE202A12F6C57200D5782A29712DC1606F4A855786DDBB66DCF3753E2
      6CB2F1DAEEFE1964B6BE6BD7AED0AFBEFA2AB15DBB7649CD9A354BAAD9203289
      AB654CBA56E34C1253DB92248AB527D96BE527E547A525414D6352B3DA2D931A
      44344CAA195933292222E23A72E3EAC3870FC3434242365D5FD4673CCF229C95
      9B6EDFBECD39420364F6BE6EDDBAD02B57AE289FDD4B53A61DC9090D96D400FA
      B832D49C6C53DA5341A93AE31A1AC484831BE705E74F5F083D7BF66C28CA301E
      D9CBAF5F366DDA7451CF9E3DC9EB33E262F1225248BCA855AB568FD7AF5FDF6C
      C3860DCD626BD57B4CE2452E35B48F999B324F2651DBCC35D8E5B1235E74F3E6
      CDD07AF5EA25E173CEAC90D8A7CC7811CF6DB251383DBDC89027E2E3455AC619
      E0BA38D4B9A3E2B087B367AE235E440A8917099B3AF863D17811CBC78BA6A7BD
      6EB5F61D44CE90B8CFF9F3E73D070F1E7C98043A4E9F3EEDD9D0AD61922D89F1
      F4EEEC72D8C5CD25375C560BBE5FF64347640549459DB67BF7EE8ED8FAB42269
      DCF978D1059C01D08E7811762EB23E031E1E1E101B1B9BE4E5E5958B0FE7B669
      D32689C48B06F41890E4A1F7CCB525035C3870996CE14D2401294F4FCFE35813
      51B58F97162F2AB4829AC95A59EBD6AD0DD561AD516176FDF4DAF6CED89B2574
      D17851525292362121610989A88C1A352A71D0A0418963C68C499C32654A62A3
      468D12EBD7AF9F58BB76EDC4F0F0F0C4808080C4E0E0E04454C9445F5F5F4241
      A25C2E4F445F32F1BF78D17FF1A2FF35F1A2CAF84B95B887ADB4234500146549
      E932ABC4FEA295DC0B792AD01752BEE017692845659EA54BB9E15A51278A385E
      65FA451A8A2EE16C5D2B89842E05788D57F08B6A9486A45829E9177DB7F9DB49
      8BB6CF5BDE715BE39F6AFFE0BB2570A3EA4BB70D543F649353B97E512105453A
      45B5F9454560169341B5FA45A509F95F5947DBED99C8EF0CCEEA9D9D586DEB68
      7BBB86147AD7C42FFAA7C9B5DD043879658CF84564598554C62482407128F07E
      113060B69851A56CA015EBC0E117315CE13B6C14F18B700C6FC2FB45077A8D7E
      C12FAA681D6DC7E3831D773C3AD4F15F5B472BDC1BF16FADA34DF1199738C7FF
      A3C4AAAFA379CEF849B27DDEC8EA5C47BB30EAFC6414F2C7B4C32A4E75FEE0EB
      1D3B769CB2582C1C71BEC89763C8C344D8C4953CB0EF20C02509B08F64C58092
      2530929EDDB18E46E4B0A9C9C64E4287331633D7D5B58E56E67870E9D2A5AC3D
      7BF67C2A168B67A333E5AAD56A29C25B221BA2C6E461C70777C82B6444E31C40
      F1C89155406C54C51B8BABDF2F7298EB020315F6D27ED1A423758BC2805247A4
      E74361D5FDA27281971CF68A98DC32FDA2923E5529C02BE7551418AEE8627ED1
      8BD75F3AF38CE3E14BC5C6DFEA0AFDFC97D7F1BFBC8EFFE575ACE6BC8EDC8C19
      8EB440D596D7912D02BC0959D9C2A346CF4116B69AE430FA009EE7306A2B0027
      7E64696F4C907741EE41410E233D14CDEB8840490BFEE02DCC8C19D9D59ED711
      119091E83D041E528656BD725E47F2A6D67C011989319C4364451F7EE9BC8E0E
      042464784AF83D130A321C16CBEB88C0FF8282D459849D9E8890EC5CAE308711
      5D9C42BE90371F0E54775E47F2EDABC82266A11554735E47928F689C2048C2A2
      33295933C90BE924E111C9E2EE7815EFA5F33A7E4DBA3F0AB82D0A97A40C22FB
      7DBF12EC4E56910EF872791D1128E9E203B0FE82C01AE1DFBF609509C034A46F
      0836E9E5F33A221042F648421A2259CE15F409022CCF418158FC370990BE7C5E
      4744B252D07377C17A5A041BC4B3E9F13B51441655CA61F4625AAC193392B076
      1766F73E82DD29645375E530A22BDA96525E0EA39729841A05CE11D4A442153E
      B856D1984CA8709188C5CDF42E2EEF0606067E4A2A3ABE1371A2D25230CBF4CB
      22203B1B0270B639A173972E4BE6CD9B3761F5AA55FD57AF5EDD7FC1FCF9EF76
      EDD265315E9B84F7049507A79C8F5C803BCED5DE7967F4E841F3E7CD0BEDDBAF
      9FA6514282BC51A346F2DE7DFAA81161F0D831630678797A8E45245E65C9AC2C
      0452B942D1065BD96DE0C0817AFF800046269351E8BAF395FCF6F3F767DEC46B
      640EA15028DA0A56B672088820715ED6061FD623190C5DF0F189A22DE4375490
      5420DD5E7FDD05E7736DF06F4D651150387B54F9FBF9050405078B4AE6D12936
      98E0B5A0C0405180BFBF2F79A63436958A000B2397CB5151241455DE3400AF89
      0B527D90AF0933A521604A7F8E52BBBBBB776CDBB6AD27D9895C1616123CC169
      AB6DDBB66DF7EFDCBDBB1DA7BA19959A42213C6764D182254B97BEF1DA6BAF11
      524AFB8A0F67C6B9F1C18307F3C78C1DFBD3EDDBB72722C2D4CACED1C438D9EE
      D8AD6BD7393366CE0C0E0A0A62493CA8C4320A77E7F66DEBCC99336F6FDDB6ED
      639C8CEF8452DE112FCFDEE0BCDA79F8E04183468C1B37CE0B0B43099F32216F
      57272727DBBF58B6ECF19A6FBE598D6CFACAE17157050161BD97A787C7BBEFBD
      F7DE409C1FEB546A357F7F5E5E1EB771C3C6ACB9F3E67EFFF0E1C34F11DF0328
      239D4A45334592022AAA764CCCC2C58B17376E18172725F2BE74F1A219A93A7D
      FCC4890956ABF51C94F3EDD2CA4C45654A85A2EF881123664D9D36CD9DEC8DFF
      F2CB2FD33FFDF4D305999999642CCF7DE5AD11484548BDD8D80367CF9C315EBB
      76CDDCAA55ABD32CCBD6866AFCD40AC9F53573E5D75F676EDBBA35C7D7C7E76B
      649513546311A181EB30ECADB7EE4D9E34E98952A91C5C96717BE9093BF23EAA
      45F3E667DAB76B77952476836A4E6B43E4E0151A1CBC272C24E477FC1D042FF9
      0A7E79452F95483663DD21B835D55EB4823B491C2FE77F0381549852F5876A7A
      95AC2C2455024E57427BA438E63AE1B8E08D7DC103AB3BF98DE75CC8B597765B
      88A54375245FA997C9A45217AD46E3875E4630A93A9DCE1FCDB933B9864898F2
      863DBA2CE022D23C04E2EAEA5AB355EBD675274C98103A73C60C5FB4FFBE1327
      4C086983E7F05A34764067726F59484A4580F7D3D83A357A0DE1FDFBF5F31F3F
      6E9CB24BD7AE6C7CA3464C7C7C3CD3A9736776FCF8F18A010306F8B9BBB945E2
      BD1AB44D4CA5101077043B92040772EF562D5B7AA1572741EF82C2C187BC29C4
      7FDF9DFC0E0C0AA2BA74EE2CC621D513EFF52199B068C7B7B5CA434086466C8D
      0479ED895653E2E9E9C9E755239B6B48DC9A54F29B9CF3C06B2D5AB614E368E7
      81E7A52587D552174BC926556C8D045D42B9AF9F1F4502E10468D1348084DD7C
      901C11A5B23ECED9350606E644D688E7A4BA0074C9EE09310B3243DD4797E68A
      1081A1F0181261A7CBF8AAD5A5470655EF75291D66FF49F54B9645B6E5448AFA
      C25C394A08EF90F044EB170443D60D900219BAE8DEE8E8CA08BF4B229AB3F749
      E4C7BB92BBA6E55A75642D419A79F5B021F5DE2A2EEFD9725079ED17A206645D
      F4B7E2D0879F91D1B12358954AA58D888868B361C3862138A80FC3417EB8C562
      198EC0860FFAEEF686E069972E054DBD7869D4DA7FBE5AB771F3B0A8A8A8B6E4
      19A27D15DB652CC87729F6D8C87EFDFAF53D76ECD8D067CF9E0D47BF67F81BAB
      6E6EE68163DD70E4FEC7274E9C183AF0CD37FBE2E4240A592AA5A94A869B49EF
      C4514BEBEFEF1FFFDEC489032E5EB830ACCFCAEB1B0970528FFE9332F1F2E5CB
      C3A6BCFFFE9B81010109D87A1D79A64A030CAAA2084D826B7878788B06EFED5A
      E5FFFEB94B81C896C3579E4EBA7BF7EEB0654B970D46D6B472D2E9C8F7324555
      1E8048CA4AD47509DBF387E192C1FBCF78BE7BE2E24FC7EF4E494949197EE8E0
      C1A1D8C17AA0A908C07BA47459AA5661197A7210BC75FC177AD0A1239DC77E3E
      F9CA952BC3D06D19366BD6AC41C8BE388D46A3AE9835C3CF88B0B6C7EA55E49C
      37D6C55877C3B0537B246DE6263469DCF8F53D8989430F1D3A34B47DFBF6BD50
      09FC50B062AA42C11600DF5D465D0B030FFAA010D5E8CE37C6C9E090AF56AC18
      121616D61A5B5F49C11650F03AD6AF8A005EC72376A8AD4422C50E17DEAF6FDF
      0123478C18889DB036B65E5E75DE6347C3FAC243229615A17FEADD283EBE17FA
      467D508503853C82D5E6BAB0682E5C02FDFD3B07050474C3DFEE48185B9D833D
      E9A45A64555BAC1D04BFB45ABF31E888EA36170286EA7F03814408714608EE4B
      B5BB8EB480445A95D657E646323EB0C46CE0782CC10E26C3AAC1DF6A3C2727D7
      CAA3A622BF4844ACAA46AD0E44935CC7C7C7A7018ED5B1D8F12271FCF5C05B24
      96AE9B3C61E889C6A8DEC3B12EC23A116B1FACCDB1B24C457E1176AA88C64D9A
      84F5EAD5CB0DBD086DF3E6CD3535A3A29CF2251E81B70386F6B42ABD3A02CDE2
      304979E08362C131F61786CEAC97F28B9EBAB50ABFEB3FB0BFCECD3B5A64CDD6
      517929A747768AA1DEEE52171C55287D99D2FC22D278644D4087F6ED83FBF4ED
      2B0D080C24394E493E5AF8686F46ECA19BA6181285A9EB2DBA91101DE41E19E4
      E9C98865268A663887ED3B7D3DB9EA7ED1D8ADC97127EE186A90177B7A358B84
      A89A35C3D42A250D36B3DC6EB3B050CAB62FBAB27ED1C46D8FEB9DBF6F205327
      E8D1345C78A38BE6370FB0D826CE66A339CE5EB167579A5F34EDE7E498E3B7F3
      C20A80D728D6FF28E1BDA8B254B542BF68ED453AF240527E34822A019CFF3C07
      BF69232727D70622792ECD8AAD144D734565C0941AEE62F87CD3FA4BB2E64DCF
      3E15C5102A1C6C29A0B28052B2273CDF60809C3CA391956B726946642B29648A
      740661E9F01AAC8A4DE187C02EDFFAD132EDBB6AB924CADDD585EDFF5A0CC5C7
      EE8A6020C0C9CED8A7A969D66FB71FDA67D106E7012B0BC47B1E0B4BC4CD0A28
      A83BBCB1B0ACD81C7F77C2DA11E4FAA61C2395DB8C59F93D9A86A92462094D3E
      67E9681DD9254236336DDDB5FFEE81BFD368ABC25305ACD41505222AD2D148F9
      9E20982AFCF14430C9A4648DEC122BAE15E2C3980C792CC5D9441289987FBD8D
      7F8BCE6A836F769DB23FC8B28AAC2275269EDC8ED87F069AD92A84F92F09EB42
      7F158E4AD8FB8A4EAED59CDDCEB1629999B3ABB3B27333C5384F93281434F5E0
      DE9D6B87AF65866566E758B8DC942D924B8BB69BEF1C31D81D7BDB0BD6D3529E
      6B11610B592FA9E15952D8059A47519CD56AA139AB497AE7CEFDDBE71E5A42B3
      B2B239B8BA6D057372F15E73EA6D83E30D8052C7DA72ED34AA1CD10C56A2CCDF
      B66DD35DB57F9DBA244371E0C38DEBAF5DDF7DC66C361BC9CEAAF260944941D1
      B27AC7D1A34646152F6140DAC7F3D64F67F6FF702B3B3BFB8EC9643296D7FA4A
      0D385FFD7CE6302751C7DB2891313273EFB6F41BC733D3D3D353B1F526045EE1
      9ED37259B4E2E7B38751339A81DD6A67EFFDBE38CF7ADC274922610C06439695
      F0C6F149E8974180C0EF393A0B98B21750273ECF4CF6F4D4938E8EADCF1778CF
      55EC2D0C3FD354E868A5950C7E43C0AAD867249E2D118BC9422A63329B8FA3A9
      20EF83DB2B838015D6DF1B15093491F4F97B10F01F45DC1685B0FAC70893BCDC
      CA2028E99B4AB052FFA3FC22AA123E1119788893219AF4DE842C3274E6192C05
      B37D09E2B2E497174A2C5B8B04D78578D54A7478F5E804E84562298C7E670CA0
      3FC3AB0F095833E5E8D1F2654B4B4750C42F72422F2EB47EFDFA5EF171715292
      538D2079F234937FB5DD8C56952B701478134E8E64FC261BFB08A564176795FC
      221BC90D824FB8BA68B1AAC0C34D0BBE587D5C555835E0E5A204BD5ACA9F7351
      C94AEF68A5C58BFCD0BB205B7569AA6049BA3060504482859F57160661116D2F
      1D41597E1109EB8B68326CDA61D2E4A9306AD468F0F4F186991FBECF8FCF64F0
      276C21C111C2AA3973E6146CD52DC5AB20AB7EEA88F0F020648B0C27DBBC6F44
      1EFAF3E8914E717171D0F6B5D7402693F02FE8B768DE94BCFB041D3A74E08F2D
      5AB480962D5B829D11C3B1A3475EA4A0BC7891156D1BC38AE1C30FA7926F87F0
      598B67CD9EC73B5F8EEDD48402B24574DAB46995F38B8AC68B8E1E3FD9A94942
      0234442A544A359038609BE64D20BE613D68DA381E5A21352DF1EF7A7563402E
      97C2D1BF8EBDD8D110014D58847381FA1F7CF08137BAEB8C4EA7E3917CFEF9E7
      2B274F9ECCB7AE61C3863C2B3EF9E413BED5A401648B2EF94DE4357FFE7C983B
      77EE8B14082C224223EC73C159BD8878D6E4A1A3478FF232205F0C201F82217A
      DEA40952101FCFF3BF71E3C63CD2060D1AF0297A8F1C3952BAA920E601354285
      9D2CA247F7EE4103060C9012555DBE62C5CAA953A7C28C1933203A3A9A07BC74
      E9D2E7321272E89023C9D15C2A050EC70AA9B0E1D1909C9C2C536B34AA007F7F
      FAEAD5AB9DC8772648B668641BDF4AC22A92419A50422AF91B65C7873DCBA4C0
      112F425990B51BB7A8C8C8BA93264D72BA78E9D21A2283291F4CABB01FCC9DB7
      80A7A05C6B4A048EAD94A30CC2DEE8D52B52E7E4B4CEA17EC4DED0348B000A04
      4BFA0A610D4146044ED4B94204FC8281542A4272F59111110DBB76EBB69DEC54
      26022746CDB1B1BBACE2F838065444C5ABC58B2A13E6ACB678515961CEFFE245
      FFC58B5EDA7DF7512A4573FCFD9D96B9B8C47CA9D3755BE2E232789E8BCBA08F
      55AA6E93A4D23A4359D6C5E5653E4F10E6E747FFBA70A1D38F63C73668DBB2E5
      B0D84E9DDEAF3B64C8980663C70E4918336648C2D0A1E3EA77EE3CB9419D3AC3
      DED5EB1B7E24163BFB6287AC1482302F2FD137B3668535888B1B16929030C5BB
      5BB7BE216FBE592FFC8D377C6B76EEAC8FE9D4495FBF674FEF84C183639B8F1C
      D9A7DDE0C153DAB56C397C71EDDA91A13A9DB85C04C16AB5E89B5EBDA263349A
      9122B3B9339AC760A94EA752A26996291416D47F23BAF046995C6ED1E239176F
      6FA577DDBA41615DBA746ADAA9D3A8FDD3A6C59E5DB24452EA04C417EDCADC80
      80B060A97400984CF52CB9B91A1C274534CBE6594DA64746BBFD092395E6F2B3
      4EB359A9B0DBDDB073B8A12AA9481F91A854B5157A3D8907986BB8BB5FB8F6E4
      89B598EBB444AF776B919030CCB759B3D6AC5EAFCEA7692523127186CCCCEB54
      78F84949707086482CE6BD299CE1D0ACD92CCEBB7BD7DD7EE74E04F5E891B7DA
      64E2188ACAB7B3EC9FE72F5CF8AAE98F3F3E28649117DA159F8080585D707043
      9CBACA2D060370663325A269116D347A679F38E1CB65668A942A958D54276767
      8BC2D5358F090CBC9BA2D31DB9F938F9A2392F3FCF929525B13C7C58479E9313
      E72F91480B59340C47145F2FAFC66806747683C16E309952CC522985B37BB552
      AFD7981F3F8E7DB46B17079D3BFFE3141454E8AFCB150A9BD8C727636FCA93D3
      5E6A75B6934854179F572B6CB646E3148AA3EF9A4C8F592D3A37411E1EDE6A85
      22D86E34B21680B4BBE9E93F3D93CB63E295CA7844AAD5B9B868B994947A6989
      8920EADAF51F959F1F8F44781F87BB9C99997A3A35F5710B2F2F7F1C85DC700C
      09F47476F6D366653D65C9AC5183A698B1D9B4D6DC5CCE929777FFF4F5EBC70F
      180C999EBEBEA22085A23E0A4EA353ABB5C6FCFC7AB693A7A87CA9F42AADD1E4
      E37496BB7DEB96FDE1A34759274DA65B0D95CA9B228671A16C361552EF8FB0CF
      039225DD5BAFDEE0EB9D3BFF71BB6FDFA3577AF69CF65650900B59477BBB7FFF
      61D756AF5EFDECCB2FCFA72F5D7A3B7DD9B2DB596BD79ECB3B756AD593BB77C7
      175D477395C974871B371E73B36DDB23D79A373FB2B766CDF13A969503FE23DB
      131D3DEA728B1647AE76E870E4608B16EF04C8640AC73ADAF471E34626AD5CB9
      FAC9FCF9E79FCC9E7D3B65EEDCDBA9DF7C73EEC1CF3FAF9CFDEEBBA31CEB684A
      04B6AF4E9D81579A34F9E3525CDCD13DE1E1EF236C05E81846BA333474E8E97A
      F5FE3CD7B8F1D1DFEBD59B34462251165D475B3A6BD6DB3711C9BDA953CFDF1D
      33E6F6ED91236F5F1E3EE2CCEAC68DE7B4D3E9428963E0C630B29DE1E123CFC4
      C6FE793A26E6E8CEA0A009085B0E5A9A966CF1F5EDF64778F8C1BF62628EFF16
      1DBDE4339DCECDB18E86ACF26DD6B469B7C44D9BC6DC58B66CF5B52143CE5FEA
      DEFDF6B1366D6E1C6AD0E09775AEAEFD26D0B4D31CA9D4796F48C8DCA351517F
      FD1916761861BE89B0A50401BBCECDADDE2F7E7E3B7F0B0939F15B44C4CEEFFC
      FD13FC2412967814A8C14A6F6FEFBA13274C78F3ECC183E3CE4D9FBEFA70AB56
      670F3768F0F7D1D8D833BBC3C2B67FA5D1F45EA9D3354B0C0CFCF14070F0895F
      FDFCF6AE77776F4660F3FABC50A7F3F8C9CD6DC92E2FAF637B03038FFE5EBBF6
      8C9F6AD7F60C41AF9A908F1E85A7631D6DDFE6CD6317376CB8744F64E4AF0723
      234FED0B0B3BB9D5CF6FF7060F8FB5BB7C7D8FEEF6F63EBEC3DD7DE54A67677F
      8D23C8E78572F85AA77B638B8BCBBEED1E1EA7F6C7C4FC9A3464C8D013D3A7BB
      E9D56AB6A45FD42824A4DBB7FEFE4377F9FBEFDCEDEF7F72878FCFC96D9E9EC7
      B7B9BB9FDEA2D71FD8E4EC3CE4138D465ECCACE209BF755AEDC20D3ADDD11F3C
      3C4EFEDEA1C39E87CB978FBF327F7EC47BF5EBBBB83839D5ECD3BBF7C0E14387
      0E767576AE37DED93918F93FF10737B7DFBFD7EB4F23D0D3F8EC5F6B359A2F66
      AB54219E25371078320CBB58A78B5FAD567FBB5AA53A860F9FDA1D1F7FF84CEF
      DEDF1C6BD2E49D8F9C9D7BF6090E9ED41BD5AF9F58DC7BBE4C366AB946B3EE1B
      ADF6D8371ACDE9352AD5F1550AC5C6854A650B0F9A2EDD679CA9D7CBBE767169
      B55CA9FC66B94C76F42BA5F2D4377AFD0954823F91A787E6EB7427E768B52717
      A954879629957FAC50284EAC90C94E937BBF94C9367CA154769C4A2668E5958F
      DDDC64F3B4DA864B158A794B24925FB0FEB5582239F59944726EAE44727D0ED6
      4562F1B9CFC5E2D378FEF82289E4B74FA5D2CF3F572A9BBCAF52552E2FB63BB2
      6BB24AE53D432EEFB2402A9D3F5F24DA3657243AF4B14874F123ACB3D0E6CF60
      D93DEF8B44AB47884423DE924AC3C7ABD555DFF3E58ABC1C8DAEFB3889247AA4
      48D4B93BC34CECCA301FF4A4E9375BD374DB709AAE851E989BB4B44C03552948
      378340C8BABA06AB33425362953205E30955BEC7F65FFEA20AF217415998AB8B
      4D34FCCBE50572E6ACDCBED36EE7BA701C577A60977B611227BC614DFDFCE188
      EE5D2B5C856AD2AEC70FA306B785063101D0B076103420352610EAC70441BDE8
      A082A3F0BB2E1EEBE031A656309C397FA3C6E1BD5B49EA99E41729706473C0F2
      D1B2EFB991035F83E88FEF8046C9825C2606316ABC545450C50C4772698284B6
      E36481E2BFE8F0614705ACDB7C10668FED4B3E737FB628CC5216EA004C660BA8
      E422D06AC4A05517548D4A0C2A9508D42A09A81422502AC42097B32091B278BF
      955F532BDB7D2FA25A9CDD0E26A31558BEE5623C8A40847DB8F0880059090B22
      3156A48CC1A3D16425119452FBD28B14E07FF9260B02297898AF04B044A86252
      F11CB9CE924AF308CA5A497821C70A9F10C068E181D022476B05E0489504AB54
      C6820CCF49C939948BC96241D6DA2BD90FF07C3E22201F4065110159B237A04D
      7B6663E1B68181539900BF3FE1E0D7FB16D879C3003BAEE5830565009595811D
      5B6230D98046F2F36C14DCCEA5E08181420422C862246013A1D967D1A6322CFF
      E1608A66F8FBED5CE932A08ACC74F819C97B0BD670D1B175E0FB1B62307114F2
      5A4472BC02CE7680221B10AC76B2AB1EC062050E656531D9616884092E9C390F
      8BDE1FFA5C4D8B4DA156721607460E71E6E1433F0CF6478DB23DD7DDC22359BC
      135E68A4F8CDC0B0F9C035FEB9176430A29480862137FB8F6B17CE35FDE8EC99
      02C1094BBB05D934091801B0902394C6AE443334980D797F547681822CAB177E
      67ABEFE8697D7C6BD4EA93F430CDB36BCB3AB0F3E03908F5767E7CFFDAA51FBE
      5FFEC90F259E4D11B620567A0504C6CD5DC5757EAD3128544ABEB7920D037939
      B9B06BDF11583A753855396B5AC416952C6F4E98BD4EA575EE8377880A4C299F
      02D6929399F6C3FACF3F1A58EE7830A21241250442C28C4B4B504B303DADDC78
      F0BF7ED07FA5BFA18A9F9EA9742982E45F1FF42BA400FD1F4608A315F502F816
      A6F4860A5364D245C84928E7BEAAE5752C45C82443C651D482529713E095F33A
      16BC05D48BCCA4AA2DAFA303019FD7D17D578F39EE17E2DE76BBFBADDED56D4B
      B5E7758C8F8FEF3D3CCE3B74458AFF6BC9AA107DB5E62F72EC8B904A25DCF5AE
      79FB7B9DD0373EF054EA240315556DDF01211FC126838ABBD466BEDD29ED975C
      4A2E3E9113E6AA62EBC95EFA3B20822A535BB76E9D1D151515EDEEEE4E91242F
      6459E4C1E364EAD4335A352CA90679771F7A7ACC3A6C82734686CAB1923CDA52
      5A06EE561F68C03503D6222AB9A0C1DDBB77CF7EFCF8F1FD087B0D9FBF088152
      45F31779B9BB71B15A63F6B198337B57875E38ACA39A2BB24CDD5D1DF98B3496
      287182B615555EFE22C784B5DCFC455A95C21EADB0E4F6D41B5212343752FABB
      CFFCBDADD357271E191ABB74FFA74787336922152BD730A5E52FF2F1F121EF6A
      16E42F5ABF7EFDB715E52FAA210EB4798BD2F312D48F53DFF430252FD11D38F4
      4146D3A64B4F65BAEDBBFC485E327FD1A14387C8CBD02FF71D90CBB72F71A87D
      F9EF5B0F243EA19DE5EB423E68D1FED6F2BF5C9E9C2A7CABD191D7D1C12BEB82
      050BD6BD4CFE2275EE83FC219727FEBA37687423AB6B0D39910389469ACD66B2
      A8CF55CB7740442693B5FBDF337FBF29F5D77AE7FF6D321A8D8E57F8AA37AF23
      193263BA8DDCFAAFE575E4C7E45E4B2640A9F98B4A9497CA5F346C4B187C657D
      A3D2EB3B2F91BF482AA41BAA7468A7EA791D09F062791D2B57FC058097AAEA78
      55D66DB9FBD2A104AE0A71903D8B75E3CC666B66B7F773D655CB425DD1B2EA5D
      AA669BFE8B17B5E8F1D1D2E5EF50DED58E20B26EFD0F45CEB5588D4F534D4878
      E8BBD58A60E5783AA6567C9FF660B887562B0D6A37EE3D60F9E8CA5151292147
      C4C44E56B9FB2B213F95ECFA02BD7F5D7D7058104975FCDE2B53F0F57826A656
      FDD73A8201E71B56034E5F73D184E540ED865DFA7FF136E5F9CA14848747BCAB
      717555417E8AE0E0A1AB6A3183AB6F985B70A02FA16272A528C0D6C8568DA7FC
      D67F206FBC7DB67EE0EF5F867C767663C35FEAC5B7E80579C905DBDD48EBCD79
      58C93107E29B741B71FA9BD85D0716072ED83A5DD777DD2471A39563296F8455
      18A8A576CE761E1519153D54A9D4B829354E6AA95C2163A524664332462181C6
      4C640DCEECF967188168A620A3941867BB62353FFB079CFD9B0D3956534E467E
      6ED693AC9C8CE42717FF39B384958BC50D82FDFCEA800C015872F0666C5D8EBD
      C0E7259D90CC1329C21AA38040A824C10C416E17B6EEA2D511DB2916BD1EB5CA
      DD4FED26627DAE255DA8432D1E09B522BD744B1A37886B2E75D2F0027C3E0540
      24249CC09180931074E267FD5401050489E3480246A45DAC0E729F3E83DF2F1D
      FFF96656FE346AD93BA0B1582122D24333AB496C83563282C48CE335BA873C50
      CEE6D82A5E00841300F147AAA0F209B84888DC1372D273B983974FECB89D6D98
      2B66E0266F8BBE1C4369CC8824DC4D35BD7974C86B5235B2CB9C515C8B392801
      5CA864572E8ED14049213797B6FF76E3D6F63BD9461EF83B5F70B985C64E4012
      5E432FFFB865AD80B61229F2C86E7D1101070225F01C0922CDCB07DBBE1B8FB6
      DECD312E70007FC19A1224462B8445BBABE6B5AEE9D9A2704AC615FD704909E0
      E460B1436252CACEA42CC3C75206EE3880976AAE3FC7FE10E2245BDA29DA7B58
      41249182E25F46E14A50003C9BB65D4FFEF441BEF9C377977396727BF284159C
      E1F81CAD2FCF5B2B0785BB6E4BA5E0F9DFCE12916F8F85264B85A662F1DB9453
      477F976020DF4FB29560910351496D422AB50CE3F7F9684A3C6139672E178198
      060FAD887101B3FDB9DED342A61BD263EDFC5E1D41831C598B6882C013EFD241
      89BC392F2090D0B4BF92A215404295E475025606368315EE6764665DC8CAFA93
      8407A3D5DA465E1AAD92C1D6F076C96A061D45BB4A292AB042044A860E9192F3
      2CDA189B14EE27A7E65C487BF6D75DB371AB8D0292F689B96748A9139891DDB3
      96565FCF5BEB2407261F67EA1952054D919473C7CB45E04C33B509014FD34CF9
      E79EDD3B7DD398BFD542C1319686C7A2828DDE345A8ADBD72C8613779FDE6F1A
      96F9AC676D8DBE8E132B15AB800A2BD7AB58368612EB6866849B48D2E1B621FF
      403E057F882878881C4F43DD2E263CEC3332A23C160E7C541CB4F61749E21FDA
      2C1B73C0BE65EC17CF55B5644713A10C9D503B952C05F9C8E10C045CEE0747F0
      19053EA3C16764F84C2E3E938ACF14BAED542664FEABE19C7F3D5EF4FF2E20B5
      66FBED177CCAA1DD03A9B2AE17BD562E02C783BD9A869582F47A21D092D71DD7
      4A22A24B02270F922A29FD6D3E18D032A2A0C717B94E7E3B9E2B49193305A614
      034E96B0C8A7B188F6928F7DD949C41DDBF4E31FD7A17B4228DA403B44F83AC3
      E63FAE41ED2057FE1E9E150CC53F5703AF6D687073469D1F75330B59E4005EB2
      588495A52D870B80172DE4EF8D38997FA35918884A6CDA2CA0E43A47D8552803
      91B079DDF1DA0E494A401E34989F4F75738CCF875095B4E0512BF630B26FD3F1
      9C88298E8C5AB4FD1CD739AE208BA2C142767F73852493B2EFF46DE8D6A82097
      A109A1588531C221C97D676EC36BF5020BCF93ED143261F7FDAEE337FF2FF503
      A3409F09473147CB18FAB9969055A6020A9FB34B26668A2D4F5A85379688152A
      1ABDE659D4B2B63F142212944C229079E8FC5D68165390F4CD627DAE8122B600
      CAE10BF7A0053E6FB2D80B1D42A9B02BF1203EFBEF2F58136B4AD4343ECAB720
      3CE21032E9039C9D5F65FAEBCA7D888BF279E1E1E3571E405CA4377F8F5578CE
      D12F8EE133C5D4942EF8681EF9A85DA1C68819AA708DD25EC65B61147FAF8D5F
      3C02C11F78C154104C472F3F2893CC786CE5C97F1E153B47FE26E74B2B0496C3
      26D145AD21B940E4432A693D7981C1E16FC51541428E4DA2FDF8EBA4F2F70ACF
      15055EEA88E6305671A5B4EEF8DFCFD7E04A5E775C2B694DD9B26CFEBF361E54
      F44065AFFFEF1993FF7504FF7F95C09A75A693FAAFB128A8669D19A4FECF13B2
      4F600D71DF7726CDA91553074825BFC9B96A43D067CCF869BEC1615359B1F87B
      562CDA447E9373D516F1A229962CAD6F5B307E28C96F0D5396AC110BE7A657A7
      0CECCFC7E0B2B64FBD2402B54AF13BF1A7DE5FB266D3FB4B566FA428AAA75625
      3F522D08A2A26BAB6AF8FB34F77275211FA57D0D87C736F81B42FD7D5B47C5D4
      D6BE328237DE1CDC1387433F1F77FDB9DDFF67F3264BB6E7278BC94A881C018A
      4944C42645516481AEBE21BBAEA1711E887DF9C2B9FECB172E803B7717CE5DE8
      01FBCED0285B5BDF90936C0BC21393DDA013D5B7572D9CB7C1C6C3AF1E84D72E
      99B7133A12AF159590E283777E0099F3422E7955A6B051F61619467941714987
      9088A84256161691DB77EEDD3D76F284A867584C03300F385C387BFA8FA2AABA
      A4B090903C9FA0A0FEF9CB578405C5241D4EF178D5D7F01AF9F77C3CBF12AB0F
      98E0438A0C0CA6E6969A9C1C1C1AC014F96AFEAC69DBD05D366FE6D47D7FFFFD
      7BC6C1CEAE666A6EA10DD5CF8CEE689CD5DEFA7D4736323230FAFD61FCDF16E2
      6803CEB5A0E001374B766C02772E5AFA2657E91818B50299BB029CACDD898E83
      F0B8241DA0E1200D3FD62F983B15DEDE011A0C331C04562C9A0F3A921A34BAE5
      14119F6442B405C0461A68580634D1B378E9C2F9CF70F9F2CA8573AF80F9792E
      A4C8611424BA4C088E8E33D9B0EFE8DFB0F8443582F9243E5101A8F67770549C
      25D116183B7B6747E7576C24567D5C61D50A6317EF4AA22D708D4A5902AC1A4D
      89AEE9748C0C807AD6338C029ACFA35165B4E5D6AD5BE25BB66C21EB246E1F1F
      9FCB6A6A6AF8471DABAAAAFC95804058581825E3804E8FF80DECC6423AEA9065
      0EE03DB0E0E35698183E7C78F71EA85779CD9A3533514A53D05E6364B07CF972
      E35020707272F255515131969393335654543496929235161292341611913596
      9656365656D634D6D0D035D6D3333136303035E6E7E7E73E75EAE8F988888833
      44B52A4073F6DFBE7D032FE400CDE17FFFFE8BE1DDFBEF0CBF7EFE6360656307
      F640413DEEFF0C2CACACE0B58EFFA9D3B2FB0F9E5D0119FC9FF11F7871E57FC8
      00361852A9E908330C6209039225945B00753DD8070C501F807DF20F6A0911C9
      B4A8A8684F454505437A7AFA4C503C408E11FA0B5A7701DED50E62C3AE6E825D
      7506DA2BBE64C992F9353535BB09FA0094078283835D4C4C4CC01BCA41DBE641
      9BCA6DED1C18AC6D9C18CC2D1C184CCCED188C4CAD190C8C2D19740D4C193475
      8D183CBC7D1D376FDEAC47544673747404ED3F06A7249454F4F12BC3F75FBFC1
      6BAF7978B91878801D7076262E0666205F595555E1C0DEDDE2544845D04805C7
      C53F867F0CFFE0F1415924C38FA1FA078E58186460FC0BB1E0FF3F6AE583FFF0
      F4FF9F01E1035CC914A3342D2F2FF71717178FB6B2B20A055DE605C2A0E5EA5F
      40B9FAD317861FBF7F3130B3323370F37031707373015BDE7C0CDCC05474E1CC
      D9F52F9F3E5FD2D9D9B90E6F24834A446032DD232222128A2F997E0326D32FC0
      64FA159A4C776DDBB11F984C311676601476F9F9F9A1C0BC9066636323A5ACAC
      CCA0A0A0C000A615551864E4951824E5E51964951519E4D49418E4541519E481
      5856591E28AF203175E2940F91919167F00691A7A7674A4E4E4E06B05C3706B9
      189E4C7FFC6478F7E933C3E73FDF8041C4C4C0C50BCC64C02012E0E367E0E1E2
      667874F7E1B9B95366CDDAB163C74CBC91FCE9D32726D01105D852D17FC6FF28
      49F51FB4D0FBC708BAAFEE17F8963782A908680133D002463C952C24053142A6
      216105DFEF3FBF19417AB15A70FCF871E6D5AB57730231FFFBF7EF397FFFFE0D
      3A10031C79A0737D41675380690120CD27C8C0CFCBCFC0CB058C5C0E2E067636
      36F0E10CBF7EFF6202EAE5009901320B6426ACC7C800ACC040BD14D018992ED0
      A06A3B3BBB99C0F2684550501008AF04615F3FBF551E9E5EAB5CDDDD57BB7AB8
      AF72F3F658E5E9EBB5CA27D077A57F88FF4A6B7B9B5920BD0C90AB6F64406682
      CC06275360A1061A33FECE00395160273333B30A48123901808E53861D440D5B
      AD867CF2A8BC34F77F6971A93B403DA0C395BE43CDA47DB30500A97E675684CC
      EBFE0000000049454E44AE426082}
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
      4E00000089504E470D0A1A0A0000000D49484452000000120000057C08060000
      003608CFDA0000AA0B4944415478DAECBD05781547D43F3C2BD7DD726FECC6DD
      08490812086EC15A28EEEE142D45833BA540F152C7AD40D1E25EDC1D421220EE
      766DF73BB359680281D2BEEFF73CDFFFF9FEDB1EEEBDBB3B67CF9C39F39B7366
      CE6C0854C51137768F183E228082818C40145006D053A06B6797B4CB7BB70CF9
      EE893A5FEE706259B61B49A06F4D1AC9C6EABEFA792166D56C8D5CB48E22D15A
      B865283CC8FDA38CE2C6ECD622929CA290D00B9B567789EDD5D81FF5ACEF89DA
      C6BAA1E6353C51CB18775F935A3C196E9D05CC7CAB64044C688240A37D9C55FD
      FB350FD676AAE78D02CC3A24912B9056AB41D53C94A87198018D6E1B240D352B
      3B83C4D3819901973D7FFE3CF5B7440411AD92897AF76D1A20691E69448CDD86
      485A80245219128B444823A390934686823CB4A8636D1791512DFC024AB5C045
      8542A188E4954BC0473B0F279953A8BB02A56495202451022311621816B15874
      8284F3C5E8FA930C64540A90BF492200A93A4259322F2F8FC5AD81CCB53B2BE1
      A347756F4D840D1AC84A4A91448C1B8E4525161B920908E4699423A3468E1412
      014A4BCF4699B905445296B5D4CE10DB23F4C5168297C8193ED675ACEBD5CADD
      A84222B11411148D443489AC363B0A30899187418AA035517E4101BA7EF7293A
      71FD053A9F8CEE5B19AA55FFB0BCE4378CB49851949FA17D7C9809D50830A294
      EC52742FB504D91C0EA49512C82427919B5E8A4484053D7B9E8C7E39F10CDDCF
      11DF6649BA19D8D56B9A57753E50DA8BF4428723D49972528172E54244B20C3A
      75270D3D7B69417985A528D6478162CD02F420290DA5E438403E2207CA65716D
      55C19A9BD014F1730D7F83A97BBC07F27156A0872939E8D753C928A7D88EF28A
      2C28C499460D7D4874FC462A3A9B2CB43868C9389066E5BB8C24F0B15A2AA47A
      B4A8A6259B8569D0DECBAFD1A5240BB2DA1DA8B0D8827C340CAAE3C1A03FEED9
      D84CABEC0C22C89EC0E84525463C337FF858A69751F111CEACF4E68B52445242
      545C5484F20B0B91444822A54665CBB18A2EB2889C0AF79E0146CC7B8C786666
      689DBE42C4744A08310446793BA30B7FDD46C7EF3F42A52AAF57042DDA07B77D
      0B0CEE7FB0AF0D193244115672B87AA0F5E293BAAEE2BC206F673B2DD322B9D6
      95AD19ECC70408B39EA8D28E5F13DD582178B76CA51F6E6E6EAEA1A1A16B9AD5
      8B5E16EA4E5777D3395132A108D58A0C40F522038886114ED11D1A47CE727575
      1D06B7EBD1878EB8B8389FF8F8F89B8D1B37793274E8E4FCD5ABCF5856AF3E6B
      59B5EA8C65CE9C4D96F6EDBBA5D7AD5BF7A19797D72AB8DDA552E17DFBF708F6
      FCBE4BB26BF70E69585898D6DFDFAFB19797779B366D7A8CECD265D4BC4E9D46
      CCEFD871D8FCF6EDFBCDAF562D66ACB3B3E93377B37BAD6AD52274B80C2E8B79
      A0FF8DE3C4891304F9BFC1480A07D7FB0F1C38400C1C389080FA483A76ECE8D5
      BE7DFBEAF0D9A87BF7EE9FB56BD72EB6468D1A450E8723F3F9F3E76C558C060F
      1EACA4CE9E3DDB4E22910C108BC543542AD5589D4ED74FABD57694CBE52DE15C
      BC42A18883F3CDBCBDBD83806109B46AA1A7A767D9DDBB77DF32828789492834
      2128286814B4445BA3D1580DA4F4A628CA992449B540201001FAC9341A8D3F30
      1AE4EFEFBFD3C7C767A59F9F5F87BE7DFBBA74E8D08133E8828202862C2D2D2D
      8102244DD3083E392200BCDF10FE0D4C31161120A5DA6432750489BE05A62B0C
      064397CF3EFB4C5B545464A5806B3B90220077170C5C85D0A7CACACA3810C307
      C3301C43FC09E709F8244422911CAA1B0092D6B7D96C35CE9D3B7793B65AAD77
      F3F3F35B63A9E006047A4176BB1D592C168E192899FB8ECF61A9A1DAB82A04E0
      34959696A67BFDFA75747272B23F9D9494B447A95436777272AA0ED21070337E
      22579DE2E2628E013EE0C928232383A3DCDC5C94939383402D98A9151E5444F4
      EBD78F0E0C0C6C1A1B1B3B47AD568795949450B82A58322CD1AB57AFD0CB972F
      3906B820668E25C404B56141A29B8F1E3D1A407FFFFDF776A8EFA151A34649EA
      D7AFBF4826937982F208B809656767A3870F1FE2029CDEDE48C7F5769018FF86
      07E7414D8ADE5EF8FCF3CFA58B172F1EBB77EFDEBC23478E30870E1D62B66FDF
      CECE9B378F9D3D7B363B7DFA7476C28409CCC8912319A885030CD6DAA851A324
      3087A5A03753250BEDD5AB9776C1820573F6EFDF9FF3E79F7F3A7EFDF55776D1
      A2459809337EFC78062CB8AC53A74E99C0E02FB0BD157ABDBE3318B317EFAD54
      3E5AB56AE53467CE9C153B77EE2CDEB061033B65CA1466E8D0A1C5D06D1E03CC
      EC349BCDA3A05A35E0566D55085BE968D1A2852F54E357A02460702526266695
      BBBB7B67B0F240B82CFF47066F0E2848409F72036AE4ECEC5C031860AF43F8C9
      0CFE5F399CBE61FFD5D3296866E7AE5DBBFA0F1A3428185AAD5EEFDEBDBBC131
      B69DEEF114F83D035A6930B4926BAD5AB5F2A1AA79972E5DB255C58884261C0C
      A3C7128093EF81D6011E8D0538690A9FFE80455AF8F484961AEEE1E1B10414DE
      E7ABAFBE0A04E6F47B8CE0E67670533D1717170FB070D0AB90C43D1CC306EE2A
      D0D40458BB04E0A30E60D61C809055002163FBF4E9E303C8F116AA49E874859C
      E307858009D7C32BE2113E877B3CBE013AB30224AC1F101030253232720F80DD
      4C6058BD79F3E6421AE0203F333393854ECA2917774CDCAF30137CBC8113CC0C
      4B87FB18DC2B871A84C04382A15FB605EA4B75E9D225102EC4014CE092044644
      7CE00EF9068F3073DC71F1770C701842007E88172F5E6064B0A7A4A41CA340CC
      D7001F1E8076DE00112466889F8E196266980126782A82020820838316A80577
      0EB0290350622B3D6DDAB427D09FA6C39394D07A75E1228D25C178849F0E78C3
      15C4526070C3D2558413A87A264009E7B591D05A24306B0136F5F0C2850BCCD1
      A347993367CEB03B76EC60010DD8993367B293264DC230C28E1E3D9A854ECCC0
      28C27CF1C5170EE84E079C5DBC7CDE7286CE29825EDF6DF3E6CDCF31841C3C78
      90D9B469138747B366CD62417276ECD8B1184A181838CB0025D200554F405F1C
      AE529B34958CAA75EBD692A953A70EDABD7BF7CB63C78E313FFDF41327D1E4C9
      93194050062C3D0F9AFA6E5454D426C0F8FE50249C4783F78F848404353C7D2E
      4856B87AF56A66E2C48918110B9B34697201BC952560F56DC1047055A4FF8806
      CD9A35731E3A74D4FCE9D367BF9C31630ED3B8718B2766B3774730560C66EF75
      0F7D973D1F66F8D5573FF91C3E7CEBC8E1C3F7D919337E496FD9B25BCFAAE23B
      7CE83A6C1650EFF8904EE02834AD53A74164707074EFA020CF1A62B14E5552E2
      A04C269D1B8CF96A575703743B13030699F3A69CC4A729FBAE0F19121E1EBEA6
      56ADE8951A0DD90321991BEE7B9191C1A29090E09A717151D30186BF0345F783
      DB156FCAE5EC1BC4BEEB43D682B1ED76B3662D9E8D1E3DBF08FB8F6F7CC81933
      7EB0B469D3FE55EDDAB5EF03AC2C80DB351FD40B741525C04A1DBDD1BD45F316
      DD877DFEF9D069EDDA0D9ED6B6EDC0690909DDA7B9BA7AF403091B43AB85C0ED
      FFEC37262626B2FFF6DA077DC893274FB2FFE61AF9BF3558FCAF3122FE49279F
      72407982A8AADEF01D25A2E9551742331098C87BE7DFF61BB8487C4AAB5575FF
      FF3795FDFFC7E6AFAA893F76EDA3CDFF6E137FECDAFF21AD8695F731C5FEBFEA
      7312478E1EFA46229136058701BBBF5C8087CF83A340811B43571C04C13D6268
      8AB6829384DD39963F672B2B2BDD4F6BD49AA11A7388F0DA8BFC770C8C45BE06
      310A75D72012ACE4515A01BA9952841CEF38BB81CE3224C97E6CA4B12437920B
      D1C20349956EF0D209D1E4569EE88DA5D124814E3EC8467F3D2F426C05F3ED56
      CB19D5D33302CE8E18B860735436F72E317A146AD6228A2C6F0F6FA3020DACEB
      8C9EA43F4799457FFB476F9856D16A2C92D20C4A7A9989F28A4ADF9E2D2EB3A2
      E7AFD291466801E9D0872D9B9B98845F12CA81FCD40C3C81424F5E6623894800
      E769949153C8F990A1061C30DB515A098D8A6D55D891592B40535BBAA12F4225
      28C24584BC8C4A24130B10E360B9AA2A6462E46DD2A22057156A15224773DA79
      A0582FF95B1D728C7C9DC468726B2FD4B2BA1BEAD62004D50C7245DE2E5AE466
      50230930A32812E91452E4E5AC45D57C4DA84DAD00542FC4054D6AE583A23D15
      7F330A7753A268AF72C56A945214E6ED0C8C74482915BF553666A6924B90A749
      87B44A19E787BBEBA428CE5FFBB78EC0A850655D516F3DFF4AFD09FF7E47D16F
      EEE1183D4E2BE25ACBCFA4E0E666730A8A91422A424A99A4BC3067222C2A29B5
      A09CC212A49249E19A08A5E797A1E49C324E871CA3DB2F8BD1BE6BAFD1974DCC
      28F97506D73A9E2635E8A4BC7AF8CEA29232949A998F9EBDCA4516303A3FB311
      FD76390B19956254570FB75CBA74817D4178A2B9FB9E229D8C464E622B0A820B
      81D03AD57C9D81A19693EA65563EBAF5F435BA9F928B6EA53B50469908659738
      50E75867146FC82CA4FF36430265153B505E29894AE089CE1A1BD281E20574B9
      77A80145DBA10BDC00268F722864631C1FC7233B034F2FA29097AB096914D2B7
      E7E5A0B300B309E55845C084F8B865BF396C0C429B2E67A200373572D59633CB
      00C5FE72298393BA2A64A3A1F96C0005827EF55CDF1335055AE40DA3AC222BF2
      D44B509FBAD24AF745B82B10519065270E1CFC638B4EAB8D87E6FD4F73892088
      232B2BF34F3CA1A9AC22542781AA030503093F09FC1D0E46F3811118C71BF381
      3A0333A77F640408F95ECB658C2670DBBE040AC5AB3240138199E9BF8E6BA9FC
      A2136E8578EC86BF0D6296B2946129ABA8586DF21DDD50406F9A2517E80ED411
      C317864A06AEF900F911E5D5EE8ED7ACE0B7A02A8930B824C0C570E8A336203C
      8B5EC8DB5B035C45A0C5D06386010DC4B3AB4001781EE55D4656203CEF3F0728
      16E83E60FB5DBE5AA3E0E62E204D73FC30FE5C4320BC86A4789711AEC2455E32
      1C9F85013DC28A07066620191410C2A71B9C2BE3A7C462705724DF692D3CB85C
      009A0CA2DF06C24FAC0B24860B148BB8F5286C1B4254FEFD5BA05F81F2E82A9A
      BE04EA8C993D021DE135A5DA788A043E5F4349031FCB225EA29DF83E2863A53F
      66FD50F03A3C1D4FCC8500D34BF01BF70237609CC94BF2023379DBFBD9C4C460
      5E2FD78DAAE9F8E991400E80F2ABF07910C88F8FCFAC7C95706B5EC6A8F3AE1D
      615D6C056A1B6B7F21E0958E6F7E82D71780CE619BC20F0269709CFF39D077B8
      F9410D7210847C5335119007D0377B8B7F1C059F7B89C4441B6FA4F853C2333E
      C6CFFEF5852A47826816F8BE0BB7DEBB3AC2DD610996149EF23B30B3E2C10357
      1B2469CC577339660C4C3AF2D515726AA842C91E3CB35A6F8268A03DFCCA0C5E
      54C1FECF3CA02D7CCB6146C524EF795D029A5B817E40E57D0C658E21CA409A5F
      B0AE403A1DE03F012D8599E148FB77DC0709EC32FFB6E967EDA7009769F94B93
      CB8ADB419ECB4FD2EB874512B87F19BFCDD19BBE4D57A2B555CF59128685A521
      BA19AF6B2A3A6FD052C66029AF6C095F0DF283B14815505B1B4F8F001DE7C54F
      86EA30FF05D85E617EB8CFF14810F05F3DFF1C1ED4743C3A9A2AA023C9A3A3B8
      D25C6DC5495E59F3441910367B5C0D57A2DCE2B1DFBC1DCE9340AE7AA64801D4
      B581ED49E33FEA7A472FAE5F2730B17E7DF22D23B849C877151334731A3E051A
      6C828D0D8F95E0BEB6A059476F6F36A709B8A39F83D9340964321AB9B1F975E0
      817E151961C547018DE7A57801F6E3C7EB2A0C2E3680C2FE3060FB80DB4F3E23
      75C469DAFBB888754CF52733EE10EFB4161E7E168044B87A7F00A30890C49D44
      6C5D28ABE6430CEC1ABC802156C18F3483BF71DA74EFDDBEF6884747DCFCD150
      AABE9629292D2104122B2A5F9C827F68AEDA08FDC4A3C32B8AA284C40766D615
      32C6D21BE291D932D62A7220B22C9B94E109646EA60F247A8D1100E814C6A8A5
      86DFD4551A64D385294EC18EB451AF29E5A8CB94992E25E8CB7644E361C91784
      C268B017680D1829B637B47CF972792546A14B0A2462D616DDDB72B9737DFBD3
      76A7699F5773C48DF45682CA0041B03EAAE14101AF79F38344211E30468F1E4D
      13803B7A1E0E44E7288F3078FAB8204746B8912DDA314BD4F8C735A25A43A0C3
      63DC790A4CFAF279242F79A99600A38C37983D87C762551DC70B3CF414F350F2
      F31A716D31D75959F40C3E7FE4A1651C748700A61CD8B6F389291CA3421EECB1
      B3BD8CC7E75B181DF54BA773BD9A1FDFB6F12D25072643F85E21AAE84326E201
      90EF16459841056828E5C11F170CE49160156FED9F5764F4290B2D12A0AED051
      7B418715F3E734402381EAFC7DE75A560DA405A23F8C8EA92AE7E5F7C25D575E
      905478804AB1B2D8285E9B66C03CDEB3A30B172E902E2E2E1A9AA60DB9B9B992
      ACAC2C6E2951ADD1300291D092F43A2DEB717A5ACEA84E9D6D1FF4B32F5DBA24
      53ABD5B150B8CB93274FEA2727273B0333310E9F545AADCDC5EC9EA9D1682F38
      29147B16FEF2F3C1093D7AE6BEC7E8D8B1634A9BCDD6FDCE9D3BFDE17BD0C347
      8F44768641B440403020918D2445945AE5EA1F18D82ED4D7374642D11EA3BFFB
      6EF537C386E5BD45C89D3B778A0B0A0A7A80445FAF5FBF3EFCFEFDFB22ADBF3F
      61FAEC33C2D8A205728B8840E6D010A4747121AE6765D23BEFDE31A714160CD3
      08047DC7AF5BABE210F2CECA9574BA4412979494347DEFDEBD5E16AB95327B7A
      12428301112E2E88A028445B6064964810A15221919313F15A2820A04E326F89
      C40C30F0A866E3264FC84289C405AAD4E1C68D1B5ED9D9D9845CAD46944C8668
      BB1D89929290243313570F115229A2346AC4EAF548EAEA8A5ECAE5641A457B8B
      44E2D62289584566C86441797979759F3E7D2AC293E0AC48841C5008148C84F9
      F91067E42316FC1BBC7AC3C0035E568F42A55131C8E16444C9342DA484C23A16
      083E49BB40109D9393E3565C5C8C977A08C2D919310101C8AAD321AC6CA914A0
      1B98B35C3C4B223B5E5106128844441EDC4F8A842E029609C68B507DA199AB3D
      78F0006A2020646E6E480C4C5071311214162217930991A01F24102242284282
      320B12E7E481B4854861B3A230A994B11614FC85C32C602E22B05209A51295BE
      7A85E8EC6C2407292C2045EA8B2464A105C80A91A405EEB1E265674CC050AF56
      2179ADDAF64CBBBD88D8B56BD7BC8C8C8C611BB76E9553D5AA1124E8499E9282
      9478450B460106AA8389C5ABED562B84AA25A8185AB1A8B41435AE558B6D53BF
      41D2F53BB7FA109B366DEAA052A9166CDCBEDDF3954A05830549485353110D85
      0438690018E24C231B7C778024F9A0FCE2D232440B85ECA8C1831DEE2E2E07CF
      5FBA388458BC78B1674444C482EBB76FB7DBFEC71F0208D509DCF402EC138374
      4828441650AE156C8805E6F6A74F514941016AD0A001D3BD5BB7D7CF9F3D9B75
      FAEC991F89CE9D3BD3F1F1F16DA1A32E3C72E488E7E9D3A709CC0CAF3DE26542
      0214CDC8E58800DB7140752CF7EEA1601F1FB66BD7AEA5607F9BCF9F3F3F6BC5
      8A152FB8DEDFAF5F3F757474F418A55239E4E6CD9B1AB848E2153D0EDB70D570
      0A03D8964EA361AB01931AD1D13668A493972F5F9E72E5CA95EB172F5E747043
      F6F5EBD7CB341ACD23894462339BCD263F3F3F85C160A04077480D6486AE12EE
      E7876A4546DA7D7D7C3200118E0083058F1F3FBE7AE6CC19C77B00D6B87163D5
      D0A1439BCF9B376F35887BE5BBEFBE4B01CA007A05BFEFCE9F3F7FDB881123FA
      366DDAD4C3CBCBEBE393A16E6E6E14E8CCA95BB76EB17DFAF4F9A277BF01FD7A
      0E18D2A7EBA0516DDB0F1E1BDE66DC426D8FF527DF435362EBB6CD6A50AC949F
      7F7C7B9C3973569D9B9BE74251A4CDA037A4D6888D29FDC0B3D9D2D292922AAF
      8487874BC024FAC5C6C66E8F8989F9197E37F8AFAE9F33283A66D4A8516CAB56
      AD2420712D6026FA2F8C64D06A8A962D5B1AC2C2C20AC09EF0DC801A98D1FF3A
      5C07482181686C940CC3E0D4CE1E787912987902C9813E6962DC5161029805BB
      A23B76EC18ABD56ABB4235C703000EC5CE0B30A3AA64849F52E149DCB00F5231
      A0F034B02F5C553460C000655454542C1F5D7BBFB99FACD852BC335A938F84B8
      1B401AB24E9D3A0C58BE18EC4AD4B3674F43424282351C6B5F24C2D5D5FD3D0F
      191E8EFD9FC63299AC2FB4566F90A293DD6EC7C30C098CFC4002ECA44AF57ABD
      3BB6B9FAF5EBCBC68E1D6B87BE891D7943458970A1EADDBB7757AF5AB54A56AF
      5E3D311CA500BD7650B60C182BF97BE127298187E92C160B3CCB6EE1DDA2B78C
      B0C7960903230D43B67CEAD4A968F8F0E179C08CA92208C254063DBEA4A8A808
      3BF6996FA3235E616610BB7B7070B01F28B6146005277D69AA6859167C839471
      E3C615C258F83BC0CEAE4A6116CF0C627AA2BDD1680C9E346992183AAF4F15CE
      94F5E4C993CF406A00CA826F6FDDBA75B7CA780D18E216FBCCC7C7276ED9B265
      1AC0275D85BC1A1650B10020E5C94F3FFD847DED15C028FB83811F30334235FB
      55AB56CD7FF2E4C9424F4F4F6FDE736101D4B201939EDEBE7DFB2F1C2003230B
      FAD87A3D300B066663468E1C298C8B8B83E185C056CCA6A7A75BA64F9F5EF0FA
      F56B3CE17014187D34B2C48C9C817EAC51A3C62ED0D5AF6F088C730B40CC6EB8
      D61888FCBB39D7B2AE6FA7C2061125151809F9993F6915CFC1D153F2FD9E078B
      6D0A1703375FB276DD779FB416E4B16E1BF1FD8ABE823DAB7BF89CFBB147E4CE
      C486740587564F0E1A38EC9356FB5E0CECC89A8565E6087FD32C4F6FB715EEDE
      6E8DF62F68F1663695FAA465B13D4B3E278FAFEF1E1618689ECF5ACADA95E5E7
      D57471D62D3218F46DF7CC6C8E99E5FE23A343DF7522546A59847FA079A15A26
      6C2B159212A6AC94B2E6E78628E4B2696ABD21FE87FBB51CF43F31621169F20F
      709FA1920A9ADB61C826590803E16C4E5601595A9613905F266CFB38D371F583
      8ADE0DD551AAA47E1E1E2E63097B6977AD5C2CB195814B030E5676561ECACB2D
      B6A5E7965EBBF2347F46CD0E932E54C968CBDCD684D1A889F40F709B2D13920D
      5F3E4F158A296C910EF4FA7536CACCC8B3651659FF3A7A2D6DF69D17052756FF
      B2BFEA19E683DF75F2797966D2BEA26BD3D9BC7313D8F44343D8C79BBBB257D7
      B4610FCDAE6F5B3F2AE67C9778F79606A5900B7276EFDE2D7BAF3A7B967EEE96
      7462C29AFCCB534AF3CE8D67338E0E679376F464AFAF6FC71E9DDBC0FAE398D8
      F35D1B78B470D389DF1AEA6FBFFDF636D841BFCD6E459CF8BE67E4CBB393F6A7
      9D1E579A7A6818937E7828FB7C674FF6E28A04F6F0EC78EB8FE362CF766BE851
      DFDB28AD341B0FA84A56A84EE760AE3A57A7B1B967C7B1AF0E0C621F4175B024
      4766D7B77E3FBAC6F91E8D3C135C34A2AA47DC4D735A132736F4F04B3E35711B
      54C7525E9D61EC73A80ED609AECE4FE36A9EE9DE10AAA315493F6827BFCE6DEB
      FCF8C8981F9F1D1C5192F4C76036E3C8504E279757B566FF04263F8F8DBD004C
      1A80241F9FFB9F35B2A1D7F18D3DB7251F1C56FA606B4FF6FEAF9DD96B6BDB70
      3AF90114DBB391672B1F93F493161084033F0BAF7D7E6397A38FB674B39C5AD2
      0C33B1FD38B6E6B92EF5CD2DDC2BB4CE3F1E2A9940DCA7454083030B5B1C3D36
      BF51D14F636B5CEE5ADFBD855E29FCB4287A6D85250FA55420EA14EF516BE180
      C88583127C5A57F3517F7A28BE9635E27F2A263309455D9779CBBA7FEB83BEC9
      D1575140C721EA5A56F9CE793D86DA6BFCF88D1753CE00DCDE8273D538D76610
      71BBFCBBC3E1C1EEB897823E8B6008A105CEDF85F391800D78EA6C2420F60B6C
      917816DD8D9F9BADC63FA33A8CCA91C62D78082A0317C656BD4CDD198264268A
      4F3943DC441522628030E69B487E8217CF968F8227FDCCDD3288D8685449F070
      E30E9FD780F0548F1E3EBF876B9BF97BD6C3BF23F8E9FA8598D109A02370211B
      A26D72F38E4DB21FB7FC60F80CF5307B11FE315256DE9C64A838C470D5AF8CA8
      8308EC441CC5335BE4218FC3AF0EB81FBC07C14C132727A78E4667A7BE3227E9
      F06875EDD1E1E2E80E46CA25C2953087F89406871973DDDD8C9B9002AA4C5660
      B616E8017981F11D7988AA31C7C3C3A31F380F9D652A599302714EF567D403F7
      1C32532213C8443AB193C9A47049F093040DAA91D5A071F567F17AF0DE28BEC5
      FA010590129A353AA9654EE03CC920A8A164023921279528D791838A984224A7
      95882669B2982E54BA29CCC10D34CD3BD517B71C090F6DD8BB776F67CA5E8AED
      8D20EB48D2D2067917A5E2B5599C4C49B302E42EF040DE227F2407478D266824
      2485484D6A9127E54FBA4A3C645E6E5EE1FEFEFE83C0971A31ECE1C417CD77B7
      7849FE9C24450BAED95DC1D7E13231F1CC0CCD0851802814058AC29088952039
      D85F8428067988BC11553E9D4280372704472CE4923161D2699F7ED528BAD170
      854925A91E2E2DB0544CF314B078899E41658E32A42674C8C3E18BC8220197DE
      990A312FB88904FE7C4AB9A9B28AACC7A8DE91DA94484571B1C964F202890438
      731EA77A4AC420092D47F9257988CE1721324B805EA7A6E1A4532EEDB3040222
      9CF2493C3E75CF7E65C7217A8EFBCC40754E8E6EF9B5836720EE680052894A4B
      4BB998B630AF18B14F05C85660456945E95CCE28D625375F04E1294E957D1EDE
      AF7A4ADD30356EBEEA680DD375D8B061C69F7FFEF9EB13274EEC3A7CF8F0DEE3
      C78FEFDBB66DDB3E881AF7CD9C39731F786EFB264C98B06FF4E8D1FB060D1AB4
      1702C3DF2172DAE23CE2D7FD687E4A4825431D33668CF7DAB56BE71E3B766CF7
      A14387F6FEFAEBAFFB162E5CF89611845D7BFBF6EDBBE7F3CF3FDF045EDC62F0
      E5FB81D94494BB856BD928A01E7CCE2809FE7518389ADF8054BFFFF0C30FFB66
      CD9AB577FCF8F17B060E1CB81D18AC010F6E22DE3701FE7800E7E8AF2C1D8AD6
      388230A37140BF705803479B366DC8FEFDFBD7FAF6DB6FBF8320791B30F8191E
      B0003CB83E1042D4944AA54E602242DEDB3301ED402B8B4761A318CB2F0EE00B
      47F60202C8E7DE0F3F949B658FB83C7FDFF9C0C1CDF3748662765FCD1D684571
      1724947A7108B0961D80279A71262DA20471B8F3BDE667CAF16CF00D2ED9441B
      78E9A53AF297A34F1DBB73CDF596B29EB1BBB8984328C56B6AB778955E01FA8B
      2B4BD0CFB014D28A4EE85BCC150A09A7061D05DEED877B98358A964699A8B15A
      2CD0510479CFE260F7DB18747A6E53734EC5C0AF4AAFCDB0EC2269EE3221BC96
      97D38C047FDDD72DFD348D1A7929ABD735CB3BD770912DF5560B27CC3D9EEA56
      91D17BF1BCFBE2538459EC88A8EB2299D1D8AC6C15A091C8F412814D21A02C1A
      098DBCB522E75027C94077253D7EC98914D7F209CD41C47B731B629A5504AA84
      3DE25C15F57462DAEE60583BB8BEA4D5CE307606B1429AA4D4624AE2A1167574
      D899C74B8E3C5B57A50F699250263F8DA4A18B5C8427B36D38BB075C3B862410
      5E8A27CA6C8C5D4C93569584D6CA8554428990DA4D55C5C8AB759FA01AAEEADE
      DE5A19401A7280134D505C963BB722CAA5A40840EB1449D085A536223BBBE044
      958C3C127AB98799945FF8EA64344D01F493046684E77EE12009CC04EFCB6358
      44814F5A9A99917FA84A46AE2D7A885D5592164106B94E42530E8ACB8B29FF1F
      442200A61026F85FF832A7E4C5CBE4D42D553222E33BDB40073E9E6A49756785
      98799301E560310396CBB3A14952905E686552320AFE28CA2BF8BD4A65A7A6BC
      A08FE464145BCB4A88917141521F9DBC0CEAC49487D950399610641459C9872F
      F32E14E6156D5708C505EFFBD9FD16495069415FA8C3D7428DDEA54D4C6461DB
      700F8749211189480A95D81902A0B52C35B3E852494EC13289CD7AF2EBDE752C
      9519B5FB52856CD6CE88B17F45C85566BDC923BD8697F72F666743AA542C0A16
      53B40E70389FB1D8EE936596E332C67E1F98582B87A26D46A8203EE8832CC55F
      1252A5ABD1C32F35CA27E0C750BD615D2E4B652B28424C90A410AA6717B04CA9
      14B1D6C97DEABC13A2759B26418D7B0E45B1AD53515C7BC6A9EBD7296DE76EFB
      7AF2C633A64FCF69F962A21AB51E3E18C5777A4AD4EB6437741EF7AA65E2CF53
      FBCFF8C1F8EFD218C5B2CE78B99094A9CC509D9498C088B5C126E7F51BA6F749
      FF578C044A8317ADD2298CEEBEA93502C3D784180C6B170F6E92F6AF132B033D
      43B6D50E0CFD213E206475B8D1F4E3A261CDD3DEC9E6218877D3C7AA5C825ECB
      66F1DF3702ED0758390DE7C2B91162107106BE4770690E8388B3BC6F29E7BFE3
      15B038209CED5C44F233523A3EC6F77F33418A9025101C2A88824ABD8182F9EF
      3817E0CD366A33BFC285CB6A499E235EEE1A064FDAC07B61078C2A315E987335
      AAA4694018F08DF0F9075CFB91BFE7777EE11CEFA75D8019FD898369EC0F621F
      72CB8ECDF29FB6FE60EC8C067A071391359448DD8C6604F5A0AB9BAAF0218B78
      1FF22479D07CE8E51F6E07AE9E3C79B219F8905D4DAEA6FE0A936258A4366654
      98A47A7B03E51CEE42BA07F99585563367F979197F25D4504DAA0233BC167999
      3CC1044EFF85A9B354ABD50E02DFA887C552969056F4B2D6FDFCDB7E9905E95A
      59B142A12B73763332AE6D3C18DF71914FEB75093B5D37B043870E662023281D
      AF453623350287C64B2B561E3C783062DFBE7D61678F9D0F787E363520F3627E
      40D96514A0BC610A90DED00530B785D5244F344D3D3303C7F9A1D01D30F61F02
      ABF895B6153B7C04F965D48C411DEB26F8C9D0E5CB979D1B366CF8DCD9D93937
      20D03FAD509E5B98EDFA2A8371B2E6B1AED64C819ECA0B0B0E4F75A25DEC3289
      8C01C72C273737D7A581E5D2E978F2E1557AF5330549E5923EEEE59B6ED8C78F
      1F1B6EDDBA658AA815F630581B617D70EF8196CD219581D17E4F7D8401C485E7
      97DCC0FB2D01EF0E8F07CACBAABA236FE56B4494A8E150B941260AD4A45F5583
      B20BB2B2B224E0D689DC9DDDF31532A535E379A641E7A77E65204CAC231B0952
      92539CF02A455A5A1A3876A5F2344A9F935646FD452664EDFCA36EF681652066
      1E96087C9F8CF6EDDBDF7036B99478AB7D998048BF64BDC2909F9F54443F7FF6
      1C4BF1AA106F9824082EA3D7E9E1EFF75D0E8CBB8FFDA386F2953933BEF8E28B
      1523468C481A3B766C2AB877A9E3C68DE368E0F081A9BD07F44AEDD9B3676A8F
      1E3D52BB77EF9EDAB56BD7D44E9D3ABD8407A6068FFAFE1C35FF45270AB54E34
      5A49B1C4F9EABAE5E08D15C6C4C448CD667396B7B777BAC16048676C4CBA52A6
      4C07072B1D74930EFE753A48930ED54B2F2E2E7E9E2A7029C855F95FA964A843
      860C31AE5CB972CA91234776638774EBD6ADFB162F5E8CDDBF7D53A64CC18EE8
      DE7EFDFAFD0E2EE0E6BA75EB2E0D08081808661089EA0D517055039AC0FB9078
      A7AFF7AA55ABA6835DEDC6CEE8ECD9B3F782378B7DC86D60806B636363274200
      D45AA3D1049224093E64597728DF08335A8A5D3E206FDE8724DAB56BE7B760C1
      82F9E0436EC53E249C5B101A1ADA13AC3F562412611F92E62108AF38FF04B408
      FF7833121CE56810B1885EF4B2B3C69AD124E6DCE42317030737CD9338973173
      62268207DB0E09C47AB8E71B288733EBF03B10BA62B790E61324108F0207B849
      46A5CBC54C8BF22EE8E92913BBF4165219854AA5B2A400B1E72B4C029FE7D7FF
      7132583196C80C4F48AE107A8FE031EA219F40A1E0339FF0BCF51A3E805E0C65
      E65728C3D5F54D2448A0F20D5BE272D463C0A5B39879041497CF200B0139291D
      375BBA969573D90A83081653B97B5CCE04C713C38170141D4F93B7BE00447C0A
      F402E83ED0138A7CD2894FDAF91E2311B7D8B0B67C131409DCCAF824A5066264
      3FB459B725EF27D58F8F862956170510E1B10AA46A4631348EB29DF48AC07BDC
      E0308878C10D14E5E97B92724638844068CA30E3F3A2A3C117439D5D8CFDE526
      D9B0084DF4C85049B5CF0C9429DC95F008F22B0D8D70CDF1F230FE06AAC70839
      8878C8676A4C061E93483EDFE16B934E3D00B0A83D44D90D0B45B9D51E9377DD
      32893489949609B462BD9341696CEE230F1C5627B759AB5ACF9A99C0C2058865
      3D71593C9140BA524569CE12A6D85F2774403FE2A26C35E8B3842946854C3E17
      650B4801514215CA8C72679F7ADAC69FD5953519030F4DD09726CB488705A783
      5E2527E86F9E4C0CCEBBD9C8A92C0F4785E07A2223E58A7C44014849A9114540
      8C0B51B686D2230FD2977016BA8BCC2E1E7EBEBEBE3D27A3DDF1B1B927F71AD3
      FF9A46DECD416557D3ED8AFCFC7C2ECAC6AB57344B233F51300A1547A29BB993
      02AFE7CC0E091745231FB13F84EF02BCBA85F76FE33D02BE88656A16E417B850
      E2B85E0293561515A52C063E566E0B27B767942D4F84BF5BECA49242D05E5F58
      9C4D14D078433717653F78F080C01B2C93599D38AB84394318BE4D0968A8CE9D
      30391289F0A64A1CB2E32A626645F6027429E31C2ACA2E42F25C0D2ACA2D4600
      C55C508C3756C2BDEC5D4575D90349D00C3A53ECB67C77A9530D74F9D6CEF111
      04DE4821C351368EFD0B0A0B90ED11B8C4E022641465BE8DB271388FAFA72882
      D4777DFB465B68D95CDC457CAD8450BDBFC8F37CE3EB7BA4808C0D01BBF1E678
      A2A8B008D9F076051B51712510EF6E65B97DA2A0068B5029297740CA675BC6A0
      3576BF912347BA02A8251E3D7A9443C84D9B36ED5BB4681117656384FCF2CB2F
      7194FDFB679F7DB6A94E9D3A8BDD9BF41A0BA1FA64B4CA3AF84DEF95610BA757
      16D51D3C78B0DFBA75EBE60042EE8168BB6294BD0D2076757474F404C0F404C1
      A02D9DD1928CB968EA0DD5DF4CCA3F3D80C612ABAD8121338FCDE8BBE6CFA373
      E7CEDD3A60C0801F131212E60507077717F6D9389A98707618FA26A71A5AED18
      85D630FE15234827DEABC01D710D4B0A5ADF35369CF62B53ABE6FA8290BC1DCA
      CFA883913385F77A1D2DB2D6E83992F5A9B31249359F83C1AD07BFF07145D7CF
      0D98A45600A9FEBC03F500A53D1889246A319272C93CF988162E4304598D73AE
      0611DFBEEB43AA79C90AE0A29D4F0912F08B9A38FABE93319AB07C646A55CE65
      697E2079099B4537BC9185CFC3DAF726A9EBDFAE1CE38039994FCCC519E35381
      B9F3BF66C427336346CFF9C688E0D3F2DEE4440A0D4B591D301755D4919C772A
      DFE846C8EB06DBC71CA87B5FB63C156F243F9AE01E820568066E3C9EE73D0B0F
      B691159FC417C6001F0D37E3E4DC3BFC3A2CEE06ED8066E3160306E3F88C71FC
      2694903719E3657C223349B30E3C2BE1C917C0ADF610A4B9CD576F28DCDC822C
      F7F6EBF10365036EB6065A0E8F22F9C0049B40DF87050BF1C5D37C153A95CF22
      737A4A832A1A80444002A23CE3B794D731CE2A975387BA7717FA6AB5782EC855
      881CA7168BEB27F159CF389469523EDDCCB5A2091850E5413B973D8EF3FD16F1
      3EC3233ACE6CF6E43706E0E1D8909E3F4364544DBF8E738C4147383EA98EE70E
      70DE285B5EC537DBA70B7016273E8F736E69314DBBF2498338D0EBCF3B15B80F
      C9B07E089CD609F6044CCF31E55574C2D944FC689BFE267197BC21E45635197E
      41D7F31AE992FC269B9524D02928B41B95E761D3FCDC88984FE0C5CC046FDE95
      402653AA625EB9D80CD28C6C61F556B67BF906A6E83E3C2D05CE1DE65D189CB6
      7F0618E375920E40EBF8C195AB2A9D4F8A6EF0936ED889D8EFCA160EF9BE647B
      0037DF9698B89E8F9035FC5C1C9E8DC72B10BD400C7F10AF11EF741412BC0DE1
      2C426C609BB054A8FCF51678B1FB8E93721A047BC42F2009D6D518BE6A5FF21B
      0A701CD70F247FF6A6AF5DE093BB3BF2798EEE7C4B8669514E269F1DAEE5D33C
      B10E17F20B0E16BE4BFD3D13C196EF26C4A1667D7E7983E6DDC173600EB86A5D
      A0E56E4375CE668D21F04E0DECBCB6C4412348F4A4EA8D0289893490F074BF7E
      E4EC59335C67CE5BE4DB6AE1DE9A2D97EE68DA7AC92F7E73E7CCF2193CEF17BF
      81F37EAB3660DEE6E0B973E69851BD55B7C85EAB8F4BC66F38237A279345A690
      CBBDDDDCDCA25C5C5C22E572B9270CE7E27F351900A18253505050FF1EDDBB6F
      9A3963C6D969D3A69D86D8E3674F4FCF0EC04CF149C0465394AA5A4444AF9123
      468C9F387162BBC14386D4183A7468CDAFBFFEBA7DFF7EFDBEF2F1F66E0BBE81
      FCA38C70968146AB8D69D5BA75A7CFDBB7F70A0C0C14EBF57A1AE2381A3C7F49
      C74E9D429B366BD61DE28F907767272ABFE7892485EEEEEE11B56AD63443B840
      9314F56646834B2874737515402C1200F704C0BDF4C724A2215851AAD46A6155
      D321144D135A8D46A256A9E438A5A5CA1CF6378C40ECF07AF5EAD582A74AB0E7
      51C1D658F08BD8EBD7AE659F3879F2487A7AFA6D06BF91E20379FE9441AF6F34
      66ECD80530E687827414C96F008432EC8B172F2C4B972C39BB65EBD669E0705D
      E4F78E54BD61005A4453AB56ADA143870C19DCBC79739352A5E2C48298D7B1E9
      B7DF1E81DBB3E8C9D3A7BBC0872AF8E7555295CAB35542C277870E1ECC02EFCD
      0E4E957DF7AEDD9910354E059750F36FB69251CE265383B163C69C0287D3F2F0
      E1C3B2E1C3861D0333A8F6AFF7A5818FA8856039113CB7ACEDDBB7A74175BF02
      6914FF658F1B05CA6E322331F1CEE449932E9A8CC68678EAF83FED230609BCDA
      B66973A84DEBD69B21BCF046FFF5003B5207F8FBAFF7F7F59D87BFA3FFC12115
      0A04F380C6BC89CBFEEB8161B4174E6CFAD056C7B70778AE557A6D8B4D266AAE
      4A258540DF33422874EF2F164B860904D4C756C6A53CD1157483DF9E23D3EB74
      EEE05307797878041B0C064F38A502CB272B9425F9B292AA5A0A339102160503
      2A36FB7AE2C4F653A74CE9D0B3478F9691D5AA4501BCE0444CE21F1152241249
      BCBCBC7C3A77EE1C3262F870B73E7DFB6A7AF5EEAD1E366C98337C0F0C0D0909
      C26FD2799719F90E56E357CFE8E2E2E2BC5BB56AA50C0D0B23014E0857575722
      2838986CD1BCB9A449D3A666305493E01D7DD1EFD80DE56430A80056E510B392
      C098E031898B2CB53A3D527B47E96D116C5CB132260649F4F9D031F1C8FCE45D
      84C46F381328954AEA6DA62F20A59D41C4B59432C5B48339516BEE68DBE46AA2
      5B2281A23EB8A5CDF984CBF8BFEB39F00A21DBD94EE06F36F8F5EEDD3BA24387
      0E38DB9024480A6DBD9CE3B2E16C76CDFC12AB44509691947AFBCC810C9BEC8C
      C31C6F4724F7E6984C5481112932F8523018BA414CD6E6AFBFFEEA0BF1EBC04D
      1733A6D55D78EF70CDB9B7FF9CB2E9E6DC0993677605BDF94AA5D28F1BA842A1
      9040481532357176E705BB6ECF889B7FF770BD05770FFE72FCE184D56B37F48C
      AF57AF2654BD72AB0DBCF2BEA1E2EC7195C155EEDA3AB1A3E7A813476267DF3C
      B2F37CD257BB76EEEADDA6759B8600787A68FECA6633F04AD5992E44A7DD7E44
      CF3FBFD10F38B07FDACFE7A65EBD7E73C057132674F0F7F7F70389DFDFC03BF0
      8A08FFE303541FC81BC803A80DEAFFD772D4F7EC5273CBAFDA2E5EBAACE7962D
      5B7A376CD8B09E4EA753816E880F316A0DB40A6807D06EA00D68C0E52F51F743
      66A3D1E8D6A777EFB613C68FEF027D2E10BA4ED5DB89794672A050A06640CD81
      A280B8F804DC1A75BDBA751BB568D1A21D28D819BF1AEBC38C3E72404F979ADD
      DD6B7A797A36812EA122DFDD2DFF4FCAAE0806E0E654038AFB28428231FF236C
      E36C5EDE75FE1FBFBA518C3EE1ED4B1437E3B77F4645C4A489E88E12A1CE4DAA
      CFBEAD8096530198297086AF236EB0D8DE78A612C54F32A0B03E6E286AA01A48
      046421AA42480C5CE0847A448487BB01C8C9F06E84C749AF2DC70B7CC559EA1A
      7A24526820FED7013458F84D7327DE6B4E6022F585A35BD7AE21C3478C70EDD5
      BB8F26AA7977734978FF360AAF986E3A19AA1DE3AB51B78F0F9077880FD4057B
      E8B10E5BBC8B9014385A86F8F878CF8456AD14007064B24DABDD7C878A2916E8
      FDEB45FA901DEAFA1B42CD2A37BD4242185452EE1510E5A1E8BB08E9E4A40A0B
      0B939B4C26E26E3AA3FCE1425E447629E11CEDEF42F879BAA994729990641D22
      9671BCF39ED1815704159C5102AC59A05028C95B690ED5CA53D991E945AC4B74
      A033A95349B8456C8A5B46E6A655890F6EF1C4300B3D3CA861D7317593C4D56A
      E6D844E698001742A310732FDF7038EC6C7E41A123A7C8924B4975B99440E8B8
      F2F035BAFCF0750144EF57B0C5620721937C72005A4C14E7E4E6D7C568F6F68D
      8BF0A60DEAF26901FC6E5F704699D4D7E985379FE7DCB58B74142D1014A76615
      0AD2728A8504D75911AAC3C5B58C8310910EB546683135A9EEE9E46B360AF13E
      1B2C787169197BF9E6C3BCEB4979C599A582E78816E3D420FC3A4BDCCF6E5060
      4CBDB40A89DAD3A452E8555265849F0B151D6C06C8B75124720805B400E51715
      979EBB9544FD753F25B5A0D87282A525FB41A14711495DE5E699B84986815766
      575B90B276E81F2C4743F6336B07EFB5ADEDB735E797C19B520E0DFD2D796FDC
      9C6BFB4DFD76FF286F3EAB913076B0F893829AF27768C078468B4B1F3C4F4B39
      71FD2953909391AB4E3DB04D7077D315F2E68F960F41402589300DDAE7581DBF
      FCD50CA2C791E5EEFD77FCD865C4CCFE3004F97F10213FC4A8D9BAECE982C157
      67A1EE876786B61AD9A759B3166DC127307D102179469302673D5FDB794B0947
      7596A77D231C7A7D3E1AF0D76822E13B7FB387573D2F0F8F86D07ACA0F2224CF
      281A6826D07740AB81E60075860100AF3688001DA3816AFD930F49F078EBF976
      3EB23CDE7F86D6455B786FC58BFF7CC22FA8FCE743F2290859E54518B6499150
      280658518C1EFDE5163F7FFF18F03A701809B66C7F6FCE3125392509F77EF25D
      84C40C3C3C3DFD1A376E5C5D2657EAEB376C22ABDFA8A9B44EBD4692DAF59BC9
      6A376826ABD3B0B92CAE510BEE53A9D14AC8779588111242753F703E83860F1F
      EE22118B05F8E5256291004901C430904945E52411D208AE421775704A24DF41
      486383060D3C5B262460DC261E3D7A4814151670EFD106C70B3D7DFC08D91DF6
      B76F8771717146385D867E6764A5210A5203424A0129F12080711FD9ED365456
      5A86AC561B2A04A60E9E119E90B4E8F59CA6E977C27502BB2D40DC0202F7E256
      00B4D2B232949595014E9814A5A4BE440EBB03B17C38AB3394E78951DCB87475
      9D95573429572874919191064F4F4F1A3B5D77EF3FA8EDEFE7AFC30B072AA50A
      11AC0318CA9046AD82DF0A64341AD08BE4E45C6C900A30BEC2373A82C1D0BD6B
      D7AED1BD7AF55241E840EDDEBD7B2CC4B2FE781B1AB8CCE8C993276FF3FD70D5
      60E44267CF9C794E70E0BF2EDA56C187948584840476EDD225008624E9993367
      C64318EAFFF8F163E4E3E383AE5CB9F2F6B598F8888A8A42B76EDE7C4E5664C2
      ED2AB15A4B9FC1B16FFFFEE40BE7CFDBE0E92C488ADCDCDC914AAD41AEAE2EC8
      D5C505797A7820B319CEA994EFCF447013D950105AC40E0C19D09956ABD3350A
      0C0CD29787F16A88FFB3108C48D0C254F9EB8C201A484A7A91F7C1FE03F186CC
      DFCF2FB44DDBB60B406FBEC0DC02E3A715BF7CE1CD8B83581E51F3F2F2523FC8
      08BF375AA95098DBB76F1F83E3B7FD7FFC711DAAF8A24B972EF6772760C0543E
      BC3103C6210218E91A356C98D0A449939680904EF8DDC8FF093FA0A0DCD36CAE
      0F140FDF15FF132CC208190B5483F7DCFEF341F3692F01FF531F92A880901F4F
      1B5AB06041A50D1187162DA276F5E9633C909010FF7BD3A60377B46AF5F52F2D
      5A4C58171FDF7F497070EC38B55A33BA8AB0BD929FB369DE3C49604848647493
      26FD02DBB61D13DABD7BFFE83E7D3EAFD5A74FFB5A5DBB0EA8DDBEFDE8E64D9B
      766B54A3464862505025271DBF0D96FBF24BBB768A5A7279530D42C36506437B
      6D707080313454ECE4EF6F73F6F7B77B86874BFD6AD4080F6AD4A85B548B1623
      FAF4E851E7DEDAB5A24A63FF0C9D8E0EA4E95A4A86E9CA141747B06565524769
      694A19419C2D138B0F63B28BC56701E99EE160572491446AE5F26EAAA2A2F0CD
      09091CA6D1652525842FDEAC6E34362169DA0C0C84D0E188E2B4B42242284CD5
      7A7B671118976D36329F245FE43C7EECABCCCEF651318C1F5954D4446BB5E2D5
      8857B4E0F56BA197AB6B75B1541A642B2A2A29B6D94A147ABD8B98A67DF3EFDC
      B11708045775D5ABE71252A9436032653F150A0B656969696102410D7B414175
      7969E9D585064306ADC9CA522A259210545A2A2B4C4BBB7AA1B0F0594C646473
      A35CEEAFA2A890B21B378852B9FCB2C8D71767BA30292525F94F1F3F7EEAE3EC
      AC070507C26817021DF3225AD7AC59D8F5962D973DEBDEFDE88D2E5DFAB7080D
      0DFFE1ABAFC6BF5CB3E668F677DF3DCC5AB1E276DE9E3D9BF29E3DFBEAEAE5CB
      FD070C18D026D2DDDDEF4AE3C6E39E346F7EFA62AD5A0BE7797999D0BA7AF5A2
      2EC5C5ADB9DDBCF9C1630D1AB43343EF6C1E1F5F7BE78C199393BEF9E640DA9C
      39F7D3972CB9FDF2A79FB66F9C3C7978647070B85A26539E8889E973BB4E9D53
      67222357CE74777743DFD5AA1571223474C5C59898C347AA55EB3A04BF2CD968
      D4B56DD1A2D1DE458B263E494C3CF078E0C087B7BAF7B8BBA77EFDE55FE9F581
      ED695A76302868C4C588885347030296CC34999CD192B838DF3F3C3D138F0506
      1E3F1014346999ABAB1E0F4980CFDE5F0E1BD6FED2EAD533AFF6EEFDE7D9860D
      EF1E8F8A3AB6D9DD7DF84AA5B2FA7E6FEFA5C7FCFC4EEEF3F098384FAFD7A085
      1D3AE8B6383975F9DDD5F5E0015FDFAD47A2A29AFFD1A489148756B501F57F5E
      BFBEDFD6D1A3A76DA956EDE723C1C1A7F67A7B1FDAE4E2B27E97ABEBE1DF5D5C
      7EDFE6E4D46EA54E2744917A3DB154A5F2FF55AB5DB5C3D5F5F4F1B8B8F57726
      4C68B0A6776F2D9885DFC811233A0FEBDFBFD37077F796DBDCDCBE857BCE6D35
      1A2F6ED1EBCF409939EB341AAFB7FD649C42215EAB5275DCA852EDDDE4E171E6
      5842C2C68B1D3BF699E7E7D7A46B5454FF2F22227A0D95485AAE56AB17FEA8D1
      9CFD41A5BAF4BD42B165B95CDEE84B99AC7284BD4CABD57FA754F65E25936D5D
      A7D59EFAC9C5E5E85A9D6ECF6CB5FAF00CA5F2E062A974CF0A89E4C44A89E434
      D0A61552E9E7893259D56F184E54AB550B65B2C68B45A2F98B85C2DF170A8567
      660985B7660A0437E60B0467E70A047B67088573674B240D67CAE5B28F02D428
      B95C38412CF68602ADA60804A386D3F48A2114B56218454DEE45515DDA515450
      7B91E8D326376BC308D2412814B4A028650C41B8471284471441187C1192293E
      E52D975FED2D5F74337C5546B94C65C76BBF6647BFBD586F19F9C92BC70BDA10
      6CCB352CE96712D55BF6399A3AA016FA12355ACFBD8EA755F7513B12D6B217E5
      E3D9582E057703FBE13F7911B180951E184C305607929A1448D12D0A993B76EB
      BBB3DBCFEC89FE35D1675DA3506CC9F393D3B89D88FD2BBF34885A7692A55BF5
      4D649D1A2726B60E41DFB9364B0CAFE385BAFBE89131AB1831116E84D1578F3C
      C107B5651723B4F7F0E9AB3BB76F3DB1EDCF7B1EC8BF531E7A59FEA253FACBFA
      841D0D2B73DFD24F34AD6900221AF923BF8232849E662356214294904276ACB8
      AC2244CB00EEC70CE9D9E6F85354BFC3E49DA61DDBB7E195BF15ADD63A280A75
      BD4E0F68EA66BD94846AE22743E86A7B9C898860132270E679990D514E0A44E2
      D783155B11E9A641424F2D52043821625F8A976FFAA0639B070C1858FCB75B11
      BFBCD9A26923F603034A2D416C461122076C416C768125293E40A4185117E941
      776C290C3A408EEF2F22FADA99FDCFD0DED67852F3318DFA26B7FDACB67BDF3A
      DE283EDA1D91E98588F582B868F611C464ADF2DF88F21E7F5BF3772BBBFD8660
      759B301487035DACDB6B7F9DCF41073AE2B7573D2642FA41CCE9B00EF83C02B5
      6917C62DD173D3A925E0E3BAA9705AB8F4105E199DDF567817AAF2B8D8C2A59D
      B3A03734A8436D253D24BB0B0AE917C0DEFD9E25C80E27BB8DEB11BFD64D5D3E
      CDA1816AC1808C5F9D41FE72193D89F5403FC06F6FA518F5868790621AB1D09A
      84568A6CBB6F21E1EFABC62C47D7BF19C5E9881E90B2C041C9DABA1BD5AE2B3B
      10B2DC52E80B426E6F2B9159543EA6E3FC757C2EA704A1C3F7116A1684103042
      27E7C57D835E9D1B43839229FB7AF7AF5070EF2D3E43361E01496412180F710B
      E14D02F83BF684B1EF095566330A1171E0FCE3A203BF5FBB834AD27380C91F9C
      650F1832826EB49225E4093F74E95793D0E79670CD4F623DD81CE01B40E05368
      41D4934CAE4A0E3CD742DAF252D0C1CE9DD0A9516D102D39CEF93FC6C90E51FA
      1CCA42F64B1E34AE8DFB9A3F1F2174ED492EEA5A5B83E2BCCB5F33B2F12222A0
      35D1C4C6E0B60990E3ABBD88C95CA269822C79A7786F066476A9536E4B5F9C15
      A0CF8F6D44ADF726D3118326A2C8D189E37F679909BFB376D4E962013237F99E
      1C9475A6F76F50D176871D48A8FCEC632923E4DB8CE806DF9902E6B076D55750
      30A8E721D6562AE196E4AB8F5B86D47E93A14AAA4FD8F2DC9C40D11349D470CD
      3C5463CA29245475F80F31B5D387018C12117CD8013184F4E3B3A1DD2F56E318
      753917F6E5E05B7533DB9D0A4C8AF943FDADFB16D2FCAF726B5C4D26EAEB172D
      489393C1E8E5E6AAAF131CE3DC32A869BF1AA6B053A6FE62EE0D16911B4D9FFE
      82E0A1F7EACE9A93F939B3B4B04BD9D2CC7E8E46FB7D585D82681C1710F7947F
      98D1809BB538E9FADFAA3162E48386A707DF8D4B9AF1BA2DBB20A7BD3D31B9B3
      2D629BC9A16B24E6B2C30C2D241F5E81E8712992F39D9BEFF59A392DA50DBB24
      AB073B25A525332FB31D33E1497BBBF10729A3EE487FF5B16966EEB8B5218DC3
      5E9FFEBA0B8505851D24624ACF727B4C1842488A09B3D6C07AC5686A1B7A4B9B
      518308A178187DAD70ADB5EAB99156278204AC0DD9335F95B40CABE6B4DBA4D2
      92003F8480A609B554C92AC51AF2E88D5B68E7C27317E74F9DDA645CF4ACA28F
      4EB69CBA74D6A5E3A15A0F463FAE07D56A6C4F4C69C54C4D4D70D4DAEF6545B5
      D14F13BB4D767DB74615630A62D182450D274C9834554E49978972F42E37C853
      C8436F205F1666A16B8FB288EED418B695B6558977A857A687BBC78BF37F9D2F
      ABC4A87FFFFE0163C78E3DD1B849E3F14949CF22FFBA7E519A7DAF80CCB2E713
      CF558F88C2640D91E8F12D6AECDD88F4A9EE63162944ED250A490F955255740D
      8E5DBB7695BFFB303838D80D62FD50B55A6DF3F3F343A92F53A92CF605E19756
      1D155EF24335E43561682151AE3207494452475949993D233DC32D373717A7A8
      AD371A8DE5734A3366CCA8DBA04183531059DBC177A4944A25F9E4C963F6D6CD
      DBC85A6025544625EBEDEB83B77AA22B57AFE03F8263C7EF2279F4E8D1F1BD7B
      F7367B5BB5A64D9B8A20721E64369B850CC338F05FCEC1CB85FE01FE4863D4E0
      57D010CF9E3E234E9C3881207A6021AEA1F0DBAC9F3F7F7EF3C58B17DBCE9F3F
      4F500B172E24264E9C989D909070EEE6CD9B41CECECEEE7852BCB4B4D4211408
      0912FEDBBC69337AFCF83183676FF02B56323232CAFEFAEBAF5DC0006FEA7DF9
      FDF7DF576EF676EDDA61776E14C4F6E3EBD6ADAB06A60C3067376DDA84975CF1
      9F90C3D5BA72ECD8B1EF2C16CB7654FECAB5CA07D4F92D1280F2C2121313773D
      78F080BD77EF1E3B6EDC38B657AF5E2FFDFDFDE755D82F82FE6926F02DC3DAB5
      6BF7983C79F2AD3A75EAFCCEA75BFDBBF78B6FDBB6ADE2DE3F11AAFC87363E7D
      36227F7A30D79A99DD4C1199BD4DAF2EB6D16FE4AA9FDC84F857D98764483B26
      73668C4014E1F58DB66B23939B8BACCD8AD62ED184F928BBB99196F82446793F
      B4A1141DE7B2C2E2FCE5525FF706A45A6B73890AD405E595CC4D5E1E44873574
      FB6746C5334249759FBD8EC2796143651121832952646352B20584D9DD5EA386
      5793EFA6DEEF153AF916F3A8B5E1C3ABEBECAA9A846CFA1DC6FE6D440B998BE7
      4A4AA8B03345568A28B6134CA1855004FBB0BDEAB9CF98E32B70F3DF97C97E10
      8752E64491EE93AF322F12031369916A9C416B92D1B490E55E898E8BC9250EDB
      ED547AFB9EBF3674BF5F3CE0FE9C502268F21DF63D893013304A8AA81E39D31E
      D02932F9C1A383A80CBCBC52C6812C0EECC691425F3747E3E8C0DE235CA4CD31
      9323717F5791AC28197EABB2B9CD66469E7A355BAC71C655432C4E60CEC941C5
      272E1259CF925995902163E9B241EC9E8D54ED5611E4BBEBFDDCBCDBDCB9731B
      84D46EDAFD8254D636F4EA322D95FE806168298D178EAC24C3BE08ED45255177
      4B7EF590E48737EAA1096FD737AB12238C9080473B626363436F5EBB8A6E3E7C
      829E17336C3C4390010E800D5909A2B522C21013CFBAB4EA2C0DEBF9AAD7CD03
      3B1B0E1DD87FD6AA751BD6638424DF45486FFF009B4A2165EE9528D06FC27AEC
      76635D74C1E6816E86F46709B104D1C5F9282733D396959DE39E9ECD2124FA28
      423E0584BC79F316CA2E2E239C5CDCD8207F3F445A4BD0D5AB57FF1D42BA0042
      0604042017BD1679980CC49307778913274FFD3784C44B6278667CD3E62DE8F1
      9327FF1721FFA70879F4F809EE297367CE247FF961E3E065CBBFFD63E1B26F6E
      CC5EB0E0E2D4AF27AE9F357D1ACEF44530A47FBC1A4D1A36608F1C38E0E4ECE6
      76BC4CA65CEE56BD7E9457EDF64AD79A1D9C55518DBB5A05B293FD7B761BB964
      C912B67DFB0E1F86DA5F36AC17BCCEC8FC55E4EE512BBE71F3E756ED4BC78582
      1F94A772BF173006A7E7F2DAED329D02AB7FDBA06654B79D3B77B053A64CAE1A
      21C522618762B1AC4148788D243FA54CF7A2E09E1AF99DD33D62B6988A320B44
      3291B14859BD59A65F78CCD4847AB58DB367CFA9FA4FF02467E7B493197C0AAE
      A69F962DFE6B85F869FE23CA49AE67C5422973337BA7FAD8E3054E56A1A8D810
      50DDE3F2F5AB9C7B0C3DE03DA9A8FA4D9B8E94BB45C9CFE7EC90D87C4EA9CC9E
      02CA6EB510CE7A0561F24F975D4E392073B6B729961394E8E5DD0B77D33273CE
      E04D2AEF1DB3162CF873C5E1C7AFBBED9CFC3A7E7790BDE1FE10FB9417AD9896
      7F0639627FF3B2FB2C365BA7FE71ED65E2BA5DA5DE2EFAE91F9CAF9EFCD5F8A5
      926A4D875AF54ECF6CD925C2EBF99B34312D1F680F9C7EE8707E39FF65A89717
      A537B9D3C907378A97CF9D86D3AE7754A923A2246F63D9F31B76E4104B158600
      0B4DC91D874F3D60725EB14C808707E5EC116EB7A53D97DF3B7B38C33F28A8D4
      2BB69E38A67A75B2CA516440CFAE5FAA7C23BF1145B67C5D4AD2654579590285
      5448E9B5CE88497FAE703C382EF7F4F0B014E4979037F66E99B1E1CFD373BE59
      F333397A70CFB7132D64AFDE7D88F53F6F5A4664270DCA3AB0CA2A7C70C6C964
      C953C8325F4AD2FEFC59F4E78A09AF741A7571C3D61D04618A178249FA87938E
      BBA3F698C9E5957E1F7428BC833D5D86D5AC1EB63CD4D763A918A1412ECEEE75
      E62C5C967378C324B6E854272BFB7B07B6A8AF67D9F968B2231731B8BEB36C54
      1146F8DF5CB553025D88496DE2C75F6DE554C06E6DC3DA6F0EB1B0FB3A388A07
      FA5ACEB9711B2FAA6EC5E8E8E8B7C60683C1DBEF878350A3EC8ECE59ECA656AC
      FDD6502BBBFF73366368A07DD58C1E0BBF1F1BA3FA247C79D95449BA1E29600E
      AA505C54BC66A7A143AC131BEC6A7DCAB6A6D7EDBA53BA68DE94A69F8C5159D3
      A3B9AADF6E15513BB58DFAE5BDFD2BD893976FB15D3B77BC0F8A70FF5780372F
      711AA7D8A903FB769F30714A41A74E1DAF43A4B00846A0F72637F17B343E9A37
      2B8B6B4176ECDC192FDCC5F38BE992F71172E3F783BF5DBEE28FC5DFAEB83E77
      E1A20BFF09214D809016B966B93E22265A171DAF5545357457566FD8D52AFC17
      08F90A10526AF6AA553BBE6E52357FAF126FBDC2AE92490BC526CF67F2D83699
      C64F4248B1B043A948D6C03BAA6692D6496FA0553A71814C63B7690CA854AAA0
      4B94BA6245548B7F46C8979939ED08179F02C5959332C1EAB942E9CF0B64861F
      E76BDC7E5BA8F3FC6D89CEB8719E134B0A8B75818090D73E8C90C4A2458B4F93
      F53B79B538B85812F4F8900E89A0250BF2102A82B8B7281BDD79E8403B965F7E
      4994BE56EF5D3066DEB5FB4FE654A9A4F9F317FC39EBF4E3D72713BF7A9DD9C4
      DB9EDC32D0F6BC81A7FD514D67DBFD50A57D9FA7D63A77CF8D575337EC2CF571
      FE08424E01841445361D4A695C9E1259AFA40C89583BF89E56F89203A1AF4E2A
      146A3DBCC9A443DF4B96CFF90842A2D2BC8DD6A737ECA584489AA7F7B21469CC
      4C89D6DD51A675659C3CCCA4D22FD46E0170BB77E65F20A4B85A8BD756A1BC94
      460C25110A28914048D95E3F54300F4EC83DFE0D42661E5C6D45778E18D9E49B
      EAD2871714A9873700424E7CA5FDAF0859EBFF22E4FF5908F98F074648859311
      33AC5B0921FF3F77BC6DB5E6B30F7080F5AB57974F2AD8FD79F94BD00E4D6949
      544AF4661C36A4F3AE86E43E659FC448C384A0EC6737DE97A8F1D41DACCEA71A
      FAA69DCF27311ABDE729CA7E7A03FD39AB7C4078CBA8E1D79B582CD1CA969FF6
      CE92E1074C9C44C7E775ADCCA8FEF89F58AD57045AD629E293187DB9F526CA79
      7E139D5CD4AB32A37A6337B23AAF70B4BA5DD127311AB2478EB29FDF42A797F4
      ADCCA8EEE80D205118FAA64BCCA7E968F36590E8363AF34DFFCA8CE246AD0546
      E1FFCA767240A2B3DF0EAACCA8CE88D59C44F828CA48AEB2A08BDBDB774BA102
      2BC949746EC590CA8C6A0F5BC9312ACE7A897E9DDCFAC3863867DF5BA6CFEFDF
      42E7BF1B5ED9205996412CBFB109BF987CD8DA6BA824E735FAE1EB840F32C565
      DEDFC20085DF5CB0BCF3FD8347856B6FAB5673D05256ED1E842C45B9E89B714D
      D1E8C547B8F3F87BA5D6E2CFE3AA3DBD7D055D5C3BA6928EF4B10397E0D73A70
      4BA789633E5C9DC4A57F54FA7D69DD58BCB333EB2D23FE53FB1F1004BF6F2CEB
      7F1F8F2A1EE0906230C77FBB07A778BE59A1C1C685375EFEB961C38614F48182
      44F9673FB25FFFFE938052805EF7EFD777CBA03EDDE762C2DFF139FEDA247C6F
      C5B26F0FB8A8E8D77FC0C101FDFAA40FECDBB3071416F51A381CCF667184BFE3
      73F81ABE07DF8BCBBCAD1A70C4ABC238FF6A87C051A2CB97985B3CD3D6B73CD1
      371B40235B4F445041E5D6E7B86F47829F7DB30EFF3FEC7D075C5447D7F7B965
      2BB0B0F48E4811A55856C5868246C15E31568CBD1B8D35D158628D1A53AC5863
      8F35CDAE31D84BC02E2A2A2A2A82F4B66CBFDFCCDDBBCB2E2CCD27EFF7E57DBE
      5C7EC3BD77CA7FCF9C3B73E64C3BB3A56E6E82C0B634EDA49A12E385AA58B2A9
      0C599A4BAA34A39EB9766C9A2109B3CFB009DD87E86E8A4F6D516AF4037002D4
      00E17949E495E85A747F906BE1BD5CFF8CB3893A3EBD15D1B3964614D5611818
      53E26A3B324DDABC30DB2AE0571EC33455684059C78EE00D6C00843DA2F7E023
      60FE7AC7A81160D37449E80E15258C72839BA38539F26D04017BA8264D9AC432
      0411B86BEDC62FB4BD7E1821607413E56A4259CF81E0270C02320255F83AB600
      B1414026A411D4EB22500909C6B748E898F6D7BC2E871BCB64F874AF5CCCF908
      4AA7F9835D4DA7D10C60D8897482C66725FE781F9888BD408C3C018493189806
      8E406874040D04C908505C76E0409F36120339128CE605371819A4437A919807
      C4CB7C60A69E02A20D2A512BA3D81975E255013014C11A53C3BBE483D8AFC568
      B02D0DD78A56F009FD8995021A2F8D0598D51CC5B206883E00703D1DB5FD3C30
      3BEAD27450299B21685FAE983FD61FE085441383CDCB01F3E5658031A700AEBC
      6167D5313DF84476461F17D346E3863003035DD29274077675264DFF84640C5E
      E88A0F11641376F0467CF2644FBD20D8A5AB0CA321501C362E9E2CD7A74DC05F
      2D17054F6ED0B1CD833CCAFFD76281637BC46E5F84A6428C252734010267676F
      32303C923D8C4BA0A1C82B4E254FA7759611ED7972F56014773E75EBD6AD7C99
      ACB195A0A074A19616EE443F7CA648E0822A2BE18DA9D8F31088FDC94008699C
      2D8246D94A742BBA1FE751704BE79A917C0C68720B8A7BA1CA2A4231A88A905C
      15D1691F6989CAABC8DF56696B22469662579D18A94CB07972039811DCD2737C
      E1427B09B9F348B0BDB1B402B332C9497123EF1A935178AACAD1D1FF4A09A9F8
      5B24E4D6ADDB96527A0949AC2B75B619952CED9D926953FF67949716084089B7
      57DF8C439DB64600DBEEE1E5328C9247807781D0A5899A126D76A6EFDDA3E59A
      AF64B2264730C33AA2629FB177E50FE7B2AC0386F1195D6BB91A8C5B39FCEC80
      7110956D9B96A31FE0E974AD715C9C86D1EF83EC58A984643511942D8384C48B
      73F48B786A292129FD5E19F8B1AB5E421AC7642B919064551292D52AD0F76CE0
      68DCDDC2374848A6B612D26CA975151292E624E4288384E469741DF0D2731D43
      906C42E3582C272191F4241846602A214960E2AB95905DD0EF3DCFAB5642CEFB
      5B24242A90C7FE2E09A9F82F96907FDBA59FA89B73ED60E3CEE33F0FE9386676
      838F464F0AFE68EC94900EA3A7F8B71B3DD23D3C8EEFD1A0D7DDD7493F69AA9D
      B9B2B1A6020F7D21B3593BA931AC9FD4845A3BB111F5CDF886D4A6A98D249DDA
      D79BEAE6EDBDAECDB8DFC5D502910421CE9533D4D32C069EE7E8E0C97B2DDC79
      A3066B54E1DA04D941DB365EEDAD1C6D67550B84E5063648831703F1D1830039
      219F84F7F9A510240518DEC216BC5CAD86D468520ED77ABCE81E9B82A131104D
      C29F4F4A7C0FFE95EDBBFD62A66F68A08BDFC41D774B266DBB238F5B9F981235
      E7D2FCD6637E1595A788ADD6F8246A9A221120C952A554AB613C523907B47483
      81C87DD937583CBB4F03D1D743C30210EF1621DE1D44BCB32E0362F4866BF470
      0C6B77878F8DF92245324FCE4015BCEB8678B7DC04487F4A37DEB9CF335244E9
      9BD66A7867C5174E2CE311837E5D67B99858E2DDF35C022EA52AA1A094011E67
      3780D4CB5D8A526A2A0E1C54C63B116AE46C905CA138AA8D406A95B28447DA80
      AD10E59FAF063BA1061CACF42DA325DE097924CA2AC56DAB32D146345A263D6E
      69A2934A83D78D32A586958936B6140C6FEBC1F28E2448B651C0602446E7EB79
      67D87FC4029D5CD132DA127F7A7F95A846BCA32BD37E748CC6688AABCAE52D88
      77B425DE192E1C46E848A22A454BDF3C972A7211EFEC31EF700E4876D7A68926
      464A3055AA6A27B5A33EBB7C8122890094C415AF1836AEEAD4B39F4020C5B656
      7CD1D1AF9ADBFECFF6B2417F2A43564D00D057C3A3A22A8BB631689A0EDEB265
      DBCC61C33E898E8B1B1A3D6CD8B018748F193060604CBF7EFD627AF5EA13D3B5
      6B8F4E5DBBF6ECBC6AD52A3789C4E6B64520A15018BC6CD9AAF50101F55A7A78
      78B57473F76CE1E6E6D9C2D5D5B3858F8F2FEBEAD4A9DB322020B0C5FBF7EF0B
      3FF924EE706559EDFAC71FD7E5EBD76F558D1D3B563973C62CD5679F4D57CD9E
      3D47656FEFA892486C55DEDE3EF21B371EA8A64D9BB3B3FC5A3F93C95B2270E8
      9091838A8AF2E8060DEA51ADDBB4A6EA07D5A31A360AA37EFC7107A556AB51F5
      A360C08038DE8307F7EE5DBF7EE567D21245A88CE91854D5054211E1E6E2C278
      797B83978F37A06C02DE1AE2E5E505F6F6F6846926CC809E73F7F55A5D27D1F6
      784A81F729D2346B1A066F1FE6F379E0ECEC0C2E2ECE606D63C396298200A202
      901F57DA1AF3782DEB66A513925D3B7434A28AC26720A0DAFEF6ED5B502A55E0
      E8E8047676B67A81C8D145E6A3AF8DB449910F3E4D79FF7E21B36A9544A55693
      7CDF3A4C2B1E4988376F66B08A4B225994F12E03468C1C817451029C10180632
      8A913B008B1C259269C7DDDCE0F9BC796C7DF2F1F7E7AB5EBE24441E9E1475FF
      3E51F8D557E0B0642968191D9C3B7306EAD6AD0BF979054609CA52848AAFA3C0
      CD4DE4D4A58BC8A66347914D74B448121D4DE19305E1F16342E8E808924B17A1
      78D142BC2A08EA2317DC2018EF97E4A8E1EADE51808DF5ECED4789FCFD094A2E
      074AA1005AA12069AD9620542A46ABD1107C94E29D870790070FC2D5C444F046
      5FEDF2952B9AF6ED7BD03FFF7C60CF9A352B86D2A84117DB1415D1C2870F8187
      8078DCB9BB3AFD97C027DB322FECECE0C98409106A6B0BB68812671717B0B6B6
      61B8469A31F0E8CA75B5DA1931D80E954E2DD201753D01420308C21681EADE79
      7B53F7A74C01CF8836F0FE4D1A382110BC67542A951234CDC3A6560C0796B35B
      F29DF1E1BEA128E17D446122CAAE0C20E4B148441C8E8D0569FBF65A1F4495C4
      5E0AEEEE1E8C40C0D71EFBFD38191ADA9477E4C88183CB967D35CC520DF1BA25
      12DD7D2510306D290A1B75C39B4FB045B4DEDC1D0FE462A378781DF257A03F3E
      C5E211CFE4587BFB097D25123C6919508D38C23A13AF2A2D0E7768787B844282
      6BB5693077944947597F162E1607FFA9984D4B7BE5CCB622A74F9F62F51C6CD5
      B479F3E6C608A9A9A96C29AEECBA79F326C4C6F6C716071D8CCD91706234ABFE
      BF2CA785BDAC727AEB90E14962D6AE158C59047E7E6514BC79F316890D278B18
      855F8EAC7EA9E7875C7F1B9059D66C372F301B98C63C2BAC2D50CECAA3D858AB
      D957C35BA8B17CAEEC7AFFE851B5A335C6AB78E322509E3E54C15F101D8B3AE9
      FD6BCE230CE2F5F35DF03EADB701E87D2C19DCB79DAB005E236613622B20691E
      781EFC0B489115D02EEE9679844B3036ADEBEAEA8A6DF51A03B1B1605C2BD3A2
      FD59AA680767D0E4BC8737FDF51355F8F01B338A7035F0F1F1615B85A0A020A3
      130A85C6EC50123BD06465B060DE2753805B1B517366B3E21D7525749412DE0C
      0867413599E950D331B6B2AFD329165EF72E9B054CEBD6C0E86F6A918F36D4FA
      77EFDEB19683F133B6B2872361FBC6DACE71407419C66EC5C7560830DF0C5756
      46863910270AFE938BD5EBADB939696C0828B4962E844B57874612F2FD3F5342
      D6FFB6442FCFB725958B9E5429D08D32D9662E21E36264D0C5A401BA9B09D0B0
      92A33806AC4DFADF2421779D4A42EE3F044A1C03B596908F6A2321279DB36129
      2D7FE10F33D1A31695B63C48A28C5D8D064D4F25C2C4911FC86C263EA96A1E55
      2521CB83C8C6C4033156F66112D29412034879095933E14F8CAD0052EB0289BF
      0E8C49AC0082FD6B2521A7FB6961C624BE05099904191959FF4AC80F929017F5
      83C470A7E694DC6874C3B2849C38A62E84B1B3F3FAEB1E9C337B37BDC66EDEFC
      BF4942AEDF9C0A7A23D4A6D7E65A4A4859E2FFAC84FCDCA61147A9F9853F4C1C
      ECAF398F0C2063643260C630ECDDD4BF56CC3624AEECBDC612325E56767A8CE1
      797312612621698384C4A72A606663C968B85252F4BAE2D824BDC05FEF7D0C26
      A675FB7009B939492F6A692757B3F75AF1087F1D436262336104C1FEA61292ED
      8A1E3A7450649090E1E1E1460989CD38E13E3E6BFDD4820E998174C809132696
      223112FECF939096FBEB1F605FBC02B3B17DF1C0C0C081BD7AF55A3266F4E8EF
      478D1AF57DD7AE5DBF429FBA5B65F6C52B007DA87DF10A40D8BE7897AE5D07D4
      D6BE7805A00FB52F5E91A20FB42F5E61641451228B88886889185B2BFBE21586
      A19D1C1D3B4D9D366DE5D8B16383CDEC8B9FDB0BE4F9FD957DB085C4F2E38B88
      72C3CF16ED8B173C4FD64977CCA9CC9CF31A0454715B02B62F1ED1A6CDCC4993
      267DDC2E32D20EDB5E3B7BE66CDE851DDFEFFB36C46A4AB9E8B839ED8D805E56
      5CAF8F3E92AB8B4BDB4183062D44D96C2197CB99B53FFC70E5E0A143D3334736
      C375F2B1291002695CE9503DB62FDEB871E3298B172F9E84F401CD9A356BBEBD
      76EDDAD72CD73FEF5ACF040C53F34B95939CE5ED8B9B69B7080CB90535920BFF
      DA17AFFAAABD7DF17FE255ED3AFFED5B77CD407530B47C4CAD469B8104C0BC91
      A387A96BA4D62019DE21322A224620E49B5423122E245C4AC9CECAC185520FF4
      CBCFC71E553B6223E49BB5AA028100289232CE3F1A280A0A0E0E626D3E9A4FAD
      326C8B2B140A40A3D694E3076A799154D09902A196031B930685525121B2812F
      58E5E1E6D2D8517E0DFA512D6AC24D852471E8C0512630D09F55690A347C4855
      48B0E11B7682979D73240DB3B47A209C1667F35CBAD53CB32AD5B74F3F6CF50A
      E40A153C56A22AE5E901562E7640D9DA98386BD691121B20ED501B29B1065F0F
      7E7B7F1F7E7B4709D13E3357D398EAD3BBDF4207077BC841D4A7122EE0E7A73F
      B51BB547EC7C119FD6DFB1C36CC08D0285FCF87C0A7D04025E672820234B398C
      D8BBFB27262030002EA6F380F2F50529FA658D4A87D7FBA02C32AC5D1E42CF31
      D0A2AC69D969690C464271A902EE3ECA4FC82DD00C2276EFDACF38FA04C18502
      5B90FA7AA2AF41835A85E765D54010154B2C830F03A7F8C0E731F0322D57FEF2
      55F154A9156F2BB165FB5E46E9DC109E0BDCC0C6DE0EE4A5786E5B6D50792CCC
      06D220E0D3A05597C2FD7BE9BF962AB4C313A607E5118BBEDBCDE479B400A1B7
      273E721C1503B385E016041E2AED48C179F13C3DF7456AEEE4EB9F87EE633FFF
      B0E5471995AF0C3C7C1C41A92240ADAEAADB8E3F3DAA13A8CC3DBCFFF65A6181
      A2D3FD25CDD8C96EA2CD176776E612B6606A17B3BA1ACEAEA1D0E9363C5ADEE2
      06FCF75F6CB613B979D20FBA10C39A1A96E261105942C207E1244546B260468A
      64E7CF03D4160C8124B56F6F4211AB4DA152A8AEA559639DA5BD10D806AD0128
      2C0C60E85080DF7F07B87851EFD7B62D3E9D1360F76E807BF7CAD254D93B1A3A
      547FEF6EB2DDC3F06C08ABECABC94E9C0038C58D667EF4913988E985A93C774E
      FF1C1303495DBAB03C2A033A760CE0B8F98609D8B0C1FC7DC204F3F7AE5D21A9
      5BB772CC36E5516557F970131E9551F4F3CF00BF705A5CDFBE5567EDC811FD73
      AF5E90D4BB77151499820CE6F603EEDD5B16F6D34F35FC6AF8131B7EDD9412D3
      B04ABFDAC18300870ED5AE40C6C64252FFFE1FC0ECF29749D6CCAB48BF7EFF79
      15491A3C18FE3FBF927B767E62E99DAA2D880E293AC38343676F999ADCFAACF5
      CB5DE91A46342420E0B3DA09EA78C6E764C2C85486D41B85C955EA575F0FE9B8
      B316F643E2193CD5F7103FFE713E4E2E5197E4F3746A65A3CEBFE1E5C641440D
      41B0CD11BC72D5F1E2B941F94C61C67B2761D90847835F4FD6ABE980263E2CD0
      F1DCF961720C62488CEF594865BC101DF584AC0135B8ABF9092B38DBEF64A754
      DA9DFEB39E29187E276A0084BB9DF5D89ED25862D7877521E2197CC41E5E83F5
      0481047DD8A0AFFE74CF9DDC5BB55B5AABE2D141EEFE23A2E6F68701C533D8D4
      0E5EF68217248CAB993612CF4CE0FAF1A9DC78195E4CDA990B0F46D424D77418
      7ABD057FBC6FAF09771E6B8D47D8F178C7766E9CE328478D7B6D40FEBDFEA65E
      B68532865740E11906BCCC0A2F9AC77DE227B59590F868D903A03FA3B5528A0C
      06937091D042D9D2292DEB3768832F34E9B7838DA92CBE0139AF8E025FF4107C
      C39D4C077D71227C702B3EDE13EB7663B93BDEA5321A68E17018B43119FA7DF3
      06DA8C3EC2854DE0D2983520789DC91883E434560FFDF9A100934F6E82D1075E
      A0AC6DE2C20CCB36C64239F3F356DC100F3EF06F39973DBCEBA00D78842D8669
      7F3C83A15BD2C0CE130363CBA85BB974E3CB8F2D89384F7CC599F877813539C7
      112577A1E5307C04682CE817C16DE1008783F941B92C79F868597C58C90AEE7D
      2E749EFB392C7D910C934FE03D107841CD082ECE6AD06FE21D6D296B18DD8163
      24B0D918B2390D163F7D049D6675E1A8315C752AE311E6FC32E446720C1E09BD
      579C83B8EDAF207A3636C685370BF4E72818C3FDD8782E4D85661FF3092FA570
      85F9F717C28AD7C9B0FCD555F06CE461F4D767CB89BB3B9B321A4B48FC4B2FB9
      96D48F2B3B6D0C239F482EA5D65442CEB5E08F57967E8C40DED5B4F6D0DC57C2
      0D607D8E2A2C256F2210F5BF32ED1F727538D02112B9853569FBAB0401FD09C3
      0B3E586647EE8B6441A263A2E1F4A9D390302881A835509B5D6D5890AEDDBAB2
      338018E872DC6573A0F06DE138BF093746DEB0388A80C259905EBD7B19C72531
      108A6F0ED4645313C3D051D4AD71B7CCC050180B12DB3F160CB6D8CE9C3E6316
      D70814F243083378C860D8BB87ED72463D98F22081F3674170D8FBF7FAE51367
      CF9C358B630654EF9B7A4C878F3AB0B3E93FEEF8918DC805FDF9C9F04FE0F56B
      BD6D803FCEB15B26A39E4C7F9260B15DF35DEECBB4EFA09FFEC0C7EA6C8ED7CF
      F38F193B863D76075FE7FF608D7747BDF8FC4542A50DA4E7579E6C16A2A2F484
      3408D6B74AC90FF50A1B367A8B41DECC7F93506DDBEFF2A58B1998E13280642E
      CE4CA8B112E1F0B9831998012467794EED47AAA4B3A491C8319C8BFC57B2D470
      D081A0683990A6ED3C5E041FFAC19A1F6AD305E744E2F9A33D3C71536558D740
      D6B8975DCA17BA4FD1319D27D3F48680C64DE29E49A5D6675BCDF4852927BCA0
      CB5CF71A4B48A1D47E88B39DDD81BF34EAD6775DDD19BE04F5C2A44E1E6C6B4C
      52B36B0CD46ACEB9D34D36DE7953D86372410809849D355242CE7EB394552C74
      DA65355789E3990456E18A67A6CF0A098DE951D78FE134945AC8FE78E62B229E
      B907DF646F03EEC0766E0C7D101004E6B380AA0108EECBEA6765F8E2FE706C51
      29DE6A4D00F105A3B784761F970EB206E5E36BEE791E522CB082BA41CB30093A
      603011AB407F9EFDB0AACBC77A654F8E2FBF72D1F0DA989EA0B7E9D704B916A0
      3F563E92C4E52393A0FAA750F4D936610D1705F8F8B8B29A583C630324BD9803
      305884C72B7430283EA2E01672F838F013B8F5210DE5E386561391E2E4582294
      48B4E0C29EC4B78A2BF8BFA22C3DAEB62BFAE7F0B84DB30F1F1ADEB251E34301
      3C9E48626D4D418B097844135B88C9436E718DFAB4ED7D97C7AC7C16952C1D7F
      6B6A1FBAC1ECA3B710C5B6DEEDD85045F1C89A6A6E14D17DE17E282DEC8555E0
      67011DC39EECFED4051A74CA005BF71898E1FC8EEB1D55AF4332B8EF2192E875
      4895FC1D419021CCE1198B61FFC45AED74268C7D0EFD276FC4F106B7869771F3
      6E323E52A3ABD2F2F177F77B890F4EBDE8428EC5C4DDE75CFEE0291D92499C50
      ED543565392D23B66DF37144DFF18BFBB4B995FBF1E1A74DBA35683BBC857BFD
      0E6E1DBBC7E53D39BFBF58693215F7388EDD9BC4F55E16C7136F1FDFA5FCDA75
      6F6623F59C443182563A155D27BF1020234B05B9EFDE03AD55155909A8C784BA
      70E7CD3D9FECEED6B3BFCAAF69DF2EAF330BCD0FD3EC3177830DC917C6E4E495
      04DA824E5E2067EC482BB786729EABAC48C1F7D0962A0842A3063EA3534885DA
      8DBE5E4E39F9A5D4386DC1D3F544D4A2B5FDECEAB4FA486C6527B7B78287AA9C
      F4E70D3D6C5E4F886A9405EDDAC9E1C2058947AB91118C63A3A122F77A9D495A
      2462E4A58480D1A878B480A1481EE9C03C9940B8CFDAB154681B3A9127B6B315
      59934A5A40E58BC464A6B500DEDA94645E3E38A0299E2D78DD6AF2118FECF799
      284183293AA0C48C5C89ED8F935211BFC489793699F09CB47335250E1ACAF0AC
      1C75022481AD840469C5035A9391F1FCE2E91F260E18B06E7D9FC0A28F376590
      42F2F998DB79C2C5C572C69E2C2D6540C990B63C5AE1AC4A1941D936EC1F4BEA
      1C1AA13ACE27341A1D92BD40AB0BDE312FAF7EF97CDBF40DFD1A382946AFFAC9
      A940A29EFD5CE2F36521C5978280268047E15DD88C5048969285CF0F119EA3F7
      6EE341405F46C79330B41ADBBE6078D644A9BBBD6ED581E98DBE7723A8FCE061
      2BFC487BE9A82CDA2A9812DA785022892B49F06D8110584969B2C05D911A4B78
      8F38B28F227D7A6A15591436034F099C8520E0A1824428C422E264883BB3F5C7
      510D4FBAB9CAC8A28C247BF069EE403A7B7850F61E5E94AD6BA8ADD49EEFA879
      B789F01971E00468D49EA5993777ABD55A6F2BE70E43699187AD8EE4818D8318
      BC5C4519DE0E70D7C99677572420DE14E6BD16BC497D9CBA6F5EDC5121004F81
      0AF5E72F1825E1DE6FE224754EFAABA1E78F9E582322AC6D1B0D1961E3D2BE1F
      2DF608A584B662273729E5E5C6037B1BAD9AD115A617663D3FABCC79BFEDE8D7
      936E1304989D555B610D9F8D476880678B7143EB75F97A79CB21EB7EE836297E
      559F89CBA7378B89C5DD783BAE1AD57A890C0FFE9F5F76011F91E1B35E90657B
      53D92DE244449F11A4BDA333F17F979A1D00EE3729DE38E4BEC10EBFD774D0D7
      7861007B809574FBA8EF052D9A7F4A0A799F3672B07FBB87A67EED04E0532320
      0C92AB5577F08B8A1AECDFAB27E1E1E9C978D7A9035E1A35D3D4DABAC7008258
      32002B2D487BA337A8DBB083E7F1CC01E41622379032640795B6713E1111B1BE
      132732994B1653A9972E93FCFAF5499BA64DB592D7AF09572B51C362821F78BE
      F588C96AA14D2F5490FCD00770E40667DAB0423D98E2F5209A37EDE53D7EBC2A
      63CD1ADEDDE7A94FFD487247C19B375D1D274F6ECDB7B5D5381E3C40F4B7E6F7
      4A3EF8191C8F18BBFFAD4EBB8FC783C7EABA9178ACADA9216B0182E0608A128B
      C9DC870F89272479B0914EB7FC939292B89BB1B137152D5AD2BC4E31949D8D2D
      8C6D2D45ADE74FF0CBB751D7D55F473D434A061E22DAC0666D344945D30DEAB7
      B40D0C84E263C748AAB454DA8D201E6D2A2EBEBDEEE9D313A7177C3BCCABDB47
      E2173DA2194597185DA8B373D8D1507F3F8193F59537F79F161999FD50ABFE46
      939F4F6AB55A4A642FD5D4232054C4E3AD8B4779B73DEFEF32A9DB46C75DA712
      E1AD9B1321D2EAB4D6D6D6E0595818787DDF09B159BB8674B9A2D894142FBE9B
      5B13695414A14C48D0BA09F8AE72953AFCAC7FC427D95E0D6DEFB9F87ED620F7
      A20DD268EB5CBB713551A522475EBA78E5A1C5B2704A285AF36AD122267BE17C
      F52B92D43C777662BEEBD09B112DBCC11A8C5CBC7C8EFF17F3A7C677EDDE915D
      B0DDBB7F54C5EA7200E94ADF200A7FB6B15E93B6E26BE6FD8CE9EABFBE5FC524
      245D6496FEB0FAE9F03103A36A5420A508C41F89194151F1AC8CB9F3D6A47AB9
      D2C59DA3C14E6CCB8487D6F717D0BCC9CB56CE15F51FD893EED3AF5BD515F722
      B6AB373E8EE26D28FDAEEBDC9DCC2FC70E307F5E3ACDAC58BDF0E6E4A923C2AB
      EA73D0A6533E6DD9316C6612C28B39E93D18C21EAD3964ADBBEFBF71C396316F
      5E65DEA9E5940F123D8C2E4F47508D97CF9CF58AFBB2DAFE037B5107F7FFA2AD
      6A126A32A73B36E126A2B6B0C35E6309E53F635EC4DBDBC7A04055BBAE3D2DED
      95D89447E67D57A11076EFDE85776988CAEFA9C17B682412FDB9C6D1D131A5E5
      996D7629140A1C41448C356C0A482AA72996C0A3695616BF9AA50172F6FA69B2
      F9E689134FCDF6B3892C0299F22636B63F6B38739685FD7EECEE8769A7449552
      F401BCB1CCA30FE08DA8321ED5983778C75485AC7D086F860CA9B80689FE10DE
      3C7D9A52C1CC13FD9FF2C6944735E24DE4772F45F5BF45CF31470CC319FA19AB
      1819ECEA43106605B2FCD656D3ABAA8D95461EE1AB7075203B1E8B0F2933E591
      5058717707DECE98546EA715656565BDB07FFF587C82393161C284850E0E0EC6
      40BC59106F7FE1F3F9B0FEBADA08822F775977F0D8CC0DF9276D5E4404040432
      DF7EBB861D35C6FB6A9A366D6ADC5373E3C60DC0473CE37D35E1DBA84A3756C2
      E6A6AC9D08633912080422D30D1518D0F0FE346A1F4E809490F88A1B2B31504D
      2560DC5186A96C3B2CFBD50C2B9AFB0F60ED608F05FD9AF5269CF4C4297FC6C3
      513B7B03B1B377C53D59068A490E048FDE9DF7F2F61E39F1D3CF0B96AEDA7465
      E9EA4D17277CFA7991974FDDF128760217A7F29696A3646F744CB7CC319F2E7A
      9DC5B83CF82529FBD75F93B20FC905EE7F4C9BB530B1634C8F2204B69F8B5B69
      933D127D31EBB69DFA509BCF3EEF7D2D257B51236F61E30E0D6CBA2CFFE9F186
      815FDF18D1A57BDF775E5E75B06636AA2AA0FEBDFB7CFCE07A4A6EE6CDE43CE8
      D2D06E8F884749556AD07E3732903DE5ECF19BE22BDD7AF64B06821C50554BDB
      D8DDAB6E6EC2B59C77233A79AC2B5468693E45A82952A711F008CDDCFEBEB393
      DFE4BFEB1AE6AB45DFBE61554078C5B3960186CA2D569768D8B355B52A9A2255
      34098AA7198AA7122B41B5E32318E8D6DBB454A5B793AB64EB891763A39A384D
      A740A7A268A244AE640A2EDDCDF975FEE006235EBE4CE5218657AA4810B11FF7
      1DE1E5E93565CAEC257787ACBAC9CE63376F603F8260407EE3512EBB3AF0A7D9
      E1DBD67C3DBFD5EBB497AB0FFE7478BBC57284035EBF799D73F6F77DA25DD39B
      ED9FD4D36F99B31D5FE022E58B66F7AF3775DFACE6BB8EFD7AD0F375DAAB8CF2
      209634B6A167CE9C7658B7EACB06AE905EA7AFCCBE43CFC6D21E62455AC4775F
      2F68FAC79963BC16DE0A66C7943A59EBE2849F56CA6CF44BD8F0430754E046AC
      5FBBFA63AE8A003768F90DA664DB24AFACD6DDC63826DF4A58B176A8909CBC5B
      F1AD59161986A951A55D3B94FFA947608BE5CDDBF516255E3EAE78FDF8F29CC9
      BB14DF9BD5B59A5C9377ABBE7F9B727DE18D0B474B9BB6E92A74F0085AF1C310
      FEE80F9ACC44602BD3536E2CBE78629BBCC3A085428AA2D754A58D5407B67CED
      5081F6E0CA010B488AD8FACF9D80C15A2DFBD9D6B5D67B4C0A2188750FF47EF8
      D9638F8EF1B126E06A2F737FE3F3951AF2E86D09519399012056DD651811A5FF
      950FC9928132F29FC76C5306FE9765CDB49C7C0880213D5DD372F29F96B37FBF
      DA3FB1AE119E7BF488731AFD8DF2A854FB1FC8A39AB66B9502D4B65DABD538E4
      7F1710DD6BD6467CC72B0EF14A39D395977891EE46AEF75759D83C538A96366B
      E03B67EDCC41B65BE77D0206F7FDF481F64DEA79CFF57496561A86BBBFA66DFF
      F84E2D8361C3E10478F422DD727BBF6A9FD9BB9F9733746C110AB79EA4614A57
      EA155A8290CA955A78FEE63D94941437CACFCF634789574C8E85B923BAC36783
      3BB30E5F380CB9BACF5FBF2FE68A9FF13C12A2E7CC0DD943BAB471387BE30164
      E6E4D788B1BE1E4ED03CC41F0E9CBE96F6EBAA09EC0F93A864EF3C8740FCBCDC
      A07FA75630B84B04EB3AB6080327A904447C1EB46A54DF2CCC1FC53D7FF321EE
      CFFD68A4A8DDE8A55636369299A8C7381465B3AEC9AC5F9256ABD9CFE8187B9A
      C71B503E0C81EC2C2A2ADC7E61CBDC120310C9CD8DE035B5024EC3C73B4A94FA
      8114D6B2AFD8421806502020DD7F79A5FD07EA907F17B359999D4E56CB2A3C1D
      F62912E93FE4015CC276E15E2302469A105153667FEABE7F7F3F5478A633FAA9
      FC0FFB6A789B31BC7D0BBE3FFCD01395D0CF1158879A0C688261A4896B7ADBA8
      1190AAA808326EDF06A7BE7DA3D30E1FB666F4694FD7B42FC2F2C677CB965E39
      172EB006606C4522E8D4BF7FEBFD070F2EAD0A6811702BC6B105E7523C0FB065
      4B4F0CA2D06AE1CE1F7F5CC37E2AE41813316B044A29E37ED7C89F7E92417A3A
      68150A90E7E78301E4F8BE7D891A80AF5082534C655D515D195FE69D1E306049
      A72D5B6485F7EE416E66267BB8D483D3A7AF68F40BDF4F553980B0AB0CE8142A
      2B6ECAD1A327B6FAEC335961723288ADACA008804770204C559576172A9DAC03
      B0D90BC077B87C59F6C79A3549122727B0934880E3090BA2AD41EDC7964A621E
      3D7AF45DBB76EDE61D22C96D978F1F4FC203754C4DC508A19F3D6DBB6FDFBE59
      A1A1A19FA2E70D28F146F4EBF30E1D3A94045519F72B0714DAA54B972143870E
      C59F1F2FE8CA33F08CD1AF72195B6D93CD010D3975EAD43AF478FBFFB918F96F
      9790C18B6F015F20001A391E2A3778A097A269BCDB602A72DF69359A3D78F5B3
      5AA5028D52092AE41E7ED9A446F268084A3C75485777D99EE3E95339BF3DD57E
      7E7389C60C41BF3E35AEBBB72C2D4D090363DC65BB8FA54DE52C4AEFA9191002
      51ABD55387F7F693252717E907CC0B553020DA53B6FBF7D42AC14C0EBB4394A8
      545347C4D697DDBD6BAE273D7C5880D41A1FD9BEDF53A6329580D10610C4C8A9
      A3073694656428C0C14180285140767621623C03422189C0F220BA958BECF73F
      D32C82B1405A952A02076ED871C35839DBC85C65393979E0E6660717FF7AC7F9
      67183E798445200432D660170303A22F9658AA234045F34081DA12FCB9517168
      6A54612C34A866CCD621D9AC90EB2D3C1792287B7C1D080901A8147A831D42B1
      18488AAAFAAB69D46A283139743C871641AE08154C1E0D6A6E7329BE5BD9DAB2
      85D72250EEA5ED50525408FCD0BE65AA3D02CA170A5853172A65D94C6BFEDEC9
      A0C97888D47CA62250CEA56DFA5F4C7F0082A8CFD9E7024A00C57C6C900451CB
      990553FEB91CB4EF1F555F207124C5F96540B599C18E4D8BF1B93D04B0754C7B
      7935E8B21E575DFBB9AF81338E0F6EB1225B4D9F0E0E0181650C7B9AA2BBFA0D
      DE1B8949C3B3CB2506A96156FB0D04817E0365214A34A3921FC671FE97ECB5F9
      7B2564E4F6CCF281F1E8663ABF9384AB91699C84112E16B4116D85165DF67127
      77995ACDE00306E0E0D9F4EAFBB46CF5D0543CEDA4A4448B4489066C6C688BE1
      16810AB2B341CC4DA31A2E8542C34A472C8FD42606EFF02567277CBD2B32BBCE
      8C3F818FEA9595C4D8914EECDAD6579695A5044747019CB8F402CB2376F6B2A4
      B000490325BC5C1D65B940E25F2DCACF0381483F97AB5269D86D2F04C133565A
      65A99CB5E9537D5D43911425C540513412B3D98025249F6F076A248F506F1274
      55988F61B3E633FD7C3C22D16C3AAF658B3AB2F4CC227071B2869B375F259937
      364CD2AB6FDA8FADD0F14362E2129682DDFBC864A111F565216D82646F951492
      905690AEA220B045A02C203C50D6AC7DB00CC7C3F12B6B45F66099F3D38EF353
      DBC77D24BBFE4AAE1FC91009D9B10B7C065580231F128E2424210ABEA3F9FC3D
      55B56B7B30C34F6D393EB5D1C8DEB23B59654CF5B3A5E0AFDDBFB0203C0B2046
      A0E2BFF6B2A69D08C7C03D601F00891B0F4CF51E374496828A4B1D2B80275BF6
      B02090FB748FFCFE5150BF7B00B08E29DFD49B7B90CE414047CC1AC2EBBD35D1
      FD2CC3E03B7EC7FEE5D399A53579C1CB15B12D50ACAFC8C826236790DD3725E2
      3B5781B1ABCB8D1154003215B51437882086CAA709F12203AC5D68CA8BDAFF62
      09F9F75A87DDCF1DA482B81D03FA7ED93C24334F694C22950FC3F9185849773D
      064558123B7BB64CAB4F10533EACC7D4A996C2CA80B0C4C63DC48F67CD923D7C
      F810FAA24E9FA62C410C7EEE3062840CEF638B1832840DD356D1159DB76BE5CA
      253DC78E955DBE7C193AC4C5C9CEECDA85C120BC674FD983070FB05D4438B707
      5597721D63F34ACBF1E4707CFC92C8BE7D6577EFDC81C0880819EEA25FBF7A15
      1B7883E7376FB220B8834C542521B93E2D9C3B726489574080EC19B7AF0F9F48
      959796C6825095749059A0B75C9DE0731EF8598DBAEAC69538A8DD5343D9019A
      2AB07C44A129452C6303A5529908C96F6BAEF26187FD701863E18B95CF5A0CFA
      D525112881C1E3525E1E2BAB0D7EE1E88EFC9670C1A72AB6B4FAE7253D4C407E
      432086AF731E2536847546F7DFF460A72A0828DC667E8C28FA0C358C8FA55206
      DFF17B1DD06F09B714E65D5E28722FD80441206A4A470D43119BE9B79FFB7382
      0EAF17F16D0EF0090E6BAA5FA98177BC785A02C20D3F5EE6E3C639676E64D896
      0B93827E87B8211CAFF095549090FFF6B2FF9DA9F9C74CF9702D483C61AEA057
      7B3158893719A0A2BAEA6BFFC2AE63C6C85C8382DCB1730A087077F0F373B7AB
      53C75DEAEBCBDEB1B3F5F1611DF67BF7F2254EB7D9B0F98F3D62E95B5419DF0D
      1BC6A4F4E9C33CE8D68DB9D5A91373232A8AB9DAB62D73252282B9DCA60D73A9
      756BE662AB56CC85962DD93B4E637A3C132E4D9E430196799419E3A8D185C473
      F26E802F406F37010C351FD774DC4B71AFA173E1D248FEAD22FFCED4FC8F7D7E
      DCDB8EDAF13E1EB921115BD2A1E58657D0F4DB1468B8E23ED45FF857F53A6499
      D2C1EA613293AEFA9E5A29A3A68351F81ADECB47B6E39757D58E3D560A6418B9
      CACBD3C0B01E3EB2AD87526A0C6611A8B4540369697218DAC35FB6FDD0C34AC7
      1E2B05320CCAA9D53A04A6806BD772A14F471FD94FBF3FA916CCECF3FBCD61BB
      AA891F45F8C95253338D6A4B48B03BFC7EF669D2F315114DAB1C643152C4D9C8
      78AF62205FA05F931D52570A477EBBADEF41D6366B993C2BC8B4E243A89B104E
      1CF853DF9715086AC1238ED9193C6B7041B2F7EA8F3F1B3BC4CA37D80067A7DA
      7D355A40C3A3F8B20E71F1BD23FA0E31CCA81990F2DE6120837AC0ABB5DB9398
      EC27DFE91EFFBEA7B271C7AA4B76F2AF42D2CE3F1914F9F774B7B6611282F4C3
      07ACC3B337CA9A0ED56BB8A1439AEBEB684C9CF6FFD3991AC3DC9AA9EB6FC1AF
      5257552B32E0439B236CB36F2994D9EEC3431BBF9ABC0F2CF75E65D60623778C
      7B36BD8799BC97B910D4453C887A143F39999523BCA465AF4999AAEC5EA6AD7D
      696B316B7898A751B984D8202D3EE4E01E77C7EF7A130E0D7835FE6A95391EFB
      95F6A3EC1C70AA90354CD115AEEB54DD3D81A3063755F1481B3DCA75BDAAA0A8
      A7B8A25F7DC4E0DDA89BB6D7317178E6674CCCA38145E8798C65A0EE2220A6DB
      006C455ADD16E4364981E82264EFB0DD5E063B1D1207BE9AC07CFC621C839F91
      6B6B0E548FA707889762E784408EC266FB78042083B5B6C0DEB7D827767F18C7
      74B93798C1CFE8C76260877DB901CD29DCE61B0D2B097A47B588EA28B5915A1F
      3D7B04F363336899318DFD1ACB525FA6C2C3970F92902E3C0F78A4792F9B9880
      40D6484C5D5BE4125BDCECCC34BED29EC1CF81E7C219DFD34DD867F8561203EB
      ED007A54777A412701C00A9B18586E9358F7A68CF1BA16C6B85D69C0E077D63F
      5A5063D16BC5EAD0C384FD609E38D1E1813F83EFE8BD2FD713C7BAB5C86418C5
      E225E0CA863F5BDABB51B130924E64EF7A632601DC18813D37B0F90FD64129B6
      DEC45A61E73179ED678EDEE3EBD9CB66B791DE0B4F1523BF221C461C29AD5642
      02EC7334B856E35FCF89E9913CAC332ABD9DD17B6BEC4FF4B332246A83DC21EE
      EECD3D9BECD9DEE960706346A54E5FD031A9FFFC7ECF46CF47EFA3B13FD1C758
      EF0E5BB8BB977D6EB5B15D0B51A9D5010A858211E29372D5CC53E38C5A59DFCE
      D29DBB36D819DCA981F7C7BD6B7E3EFA5DCC9D81EFD0FB69EC4F741319577120
      F73377F7E19E1B97017D6B6B7089FDEF8E6164673F623E4A8C65F0BB21AC26CD
      11C0D7360697187B7B140BD4FE665F06BF637FA2A3C090A81572BF71CFEDB867
      93D551F3C4005F596197D8F3661CE37FB8191376A21D83DFB13F31D808E481DC
      EFDCB3F15E06340395F4D9A8EA7C214E6C75AC33238DAFCB82E177D61F8513DE
      144EE489DC710EC0782F039A2E8C47911361347DBDC5C96846FA635DC6F76724
      32D03BEB8FC28956B4A11C9D44AE0E7291DC73A451F8136AA669F4801EB28E63
      7A8717F28A0110BB8456026837BC4B7844EF8F64A0D2C918195B37B1D518BC54
      F325DB10E89F13CA281AC71B02637989C17FB561A4E7EA1A5DC0B5E60CF6C7E1
      4433366BED913BC3DD7DB967CF32A0B1A88919410D81E154A2FFFD668CF42F94
      B57B286BE89DF5EF4E1B98FD87857BB933D55C5179E8480E812164A24F4A2306
      DFD97757B3C27BC9C2DD6CFE90C70D80CAA0253103061089EC1D208C9B9971E2
      A4620FE46E70F750EE39D25C94E8CD5A581AB69072A2575085FB6FEF65EB37C2
      9D2EF920345DB41561AEF96B757FCF940F68B44664A6A7B41623764C3920B5D6
      E8431CCEB69CA69F23FB63732ED9E36307F0726ABCD07B8239904A5373BEE898
      F1335A1F84D557FA8FB700A42ECBDA275ED501C10BE506F65EE1F3935B5E56F8
      6ACC185F8B0AC267E7244C9F666DE0E85F97E19B0E0584D9E727946A4B239FEC
      F5E9296B234FBE8F299EA0D5E840AD5100BE57A088FAA6ACBBA99B51361C39E9
      B89805991DB51FBEFE7320ACEB2A27901FD3BD6963F83DF136ACED5262A4480F
      B4FCBE59D6745F8411E37F15B120733AEC817BF94BE04452D972982EB220F67D
      430FB979D610B32B644DABD58D9FD3713724667FC9BECF6C5F6697EB76CE421C
      6EA140AA3446C6EA9685EB7BDC25BAED4B4F0E1ED1B1B1FE2BA267B384AA1273
      8B5FFAACCDBA669EB555AD30B0F0E38DD476B11D0C8C6CEC0A09B73360C740AD
      2F1705F7BAF3515A8579D634163BD08A03E3B523FAAF27E17CD2BB812A394BC1
      CBAAEB9A565796B51FCCD6282B0E4ED48DE04AB0E2DF71C87FCAA06F39CDCBD0
      10E2A5B1D8F0DF75283302B88E0B93584867A408FBF4140804FBBA77EFBEF2C0
      810383E6CF9FDFA17DFBF6E1D8E167EC87C3701CD09BA523CA9723DCA718DFAC
      59B3D8F5EBD73768DCB8B1EDA14387D8FDFE2525FA757F67CE9CE1070505F18F
      1E3D2ABD7DFBB6EBC48913ED131313EB72E2962D5FF8FCC3C88E1D3B763F76EC
      982C2424C4161B067071718151A346213706783C1148248E70E2C409D668008E
      83E3E23438AD2945035AB468618F48E61B4EE85CB66C19BB226AEDDA8DFAF149
      BF1078F326D97882278E8BD3FCF1C71F781CE5940168E7CD9B37A5858585F584
      9CE5045F5F5FF8E38F6BD0B2654736213EA88CC7F3350221A55E81D260C32F3B
      4D29BA7AEEDC39B701030614AF5EBD3A0A81B8B56DDB16CE9EBD08F5EAB56223
      3D7972153A766C0BD31F2F801BB949EAFC7B396F72CFBDC216E2AF9A750BF071
      8DE86A211289A263626242FAF7EF1F929696E676EFDE3DF6B0CDB0B0B0226F6F
      EF77A3F267BAF4EFD7DF6EEBD1ED4FC8638AFABAEE020646E61A810CFA910302
      B347A0589FC69A982DF745413B4614CDF8538E4013AAD881B1750F1D3EF41028
      42830ACF4D18953BC60064D85383FBA97C8BFDD41F6C4F46C576082BEFFDE7D1
      F3B76062BE0CA026A60871AFBA93008F85B00942FA37943D38783789CB4F124C
      2F1C5BB33A8481564B70171E60B175A2FB8B6006DFD9F7D5925A544603D03474
      9FCC8F87F148F7C6F769825A02FD5F9790A396652F449F7B0192948B905BC836
      8005B7AB1C3A6D6CDBD89866EB178E6C1A226E613A33B8B3F4D5DE93EC7633E2
      BDFA3D608722E263A99D2D01058B82F71AD2EC5AE8CEFE22B6C9B268EBD1B70B
      08825C84A9D0325A4EAB635CCF7ED96EA646CB54187AFEFD42613A4AE383D318
      B3163BFB193BD5834FC353922A4853A719C2E24ECE8D1CAF506B2B18A911F228
      49E7A5091BED09FB5DE757352F6B200BC802249D94A0D029D895CE6AFDD40FA5
      D6EA342A357B601D563355450A75895CA92D968879F8E0522A0B4C8E4B7FCEA4
      9A31163F73A707532A8D4E53A4D0CA738A947985A56ABC1D8F6DBB688AB4D2E9
      7494A5117633C662AA50565B1CB9FE5AF23657A1B016D244B097A4986D07D916
      5EC7DA2533FBFC4D66B1B6C28794676C4E91C2D946CCB741B4EA961E7D94141E
      606FD4B4DCA52297899BAEE0FEDA86A71BF52BA8696EB124A9D2307253C6E616
      2B25344D89F93449DA219EB8D989DC0CEA25A2D05AA3D1988DB512A1D3D81EF7
      F0E373A346942A350506C63E7E93EF9891271738D808845FEEBBF5B4A8A8E856
      B9F6F015727BDFED1E66465145C60299EA2CB506173BA13312F617E572F956D3
      E39B717131FB484193D863C0C6EE9ED6B6DBB38CE2674811D799AAC8EE52A1CB
      844DD7303FE2F162545C34EC519F58C0D88098B0831BFBF55A30D170C445900B
      B226AC1FDF3A223DB734B37CE13365ECDBDF1680FCED03081F988C347F2DFABA
      94118814299C17DAE7D75F7FFA7C6928666839E78A186B8319FBE6D7F92C889E
      3FE44204C2E0BBB11CA19674C1BC09CEAF966C781FBCE7AF93FB2C5552F5FB27
      F2D2F4877805396692CA240D36C3AF076B3F2A7361FB51EF99769FBC586CA1A7
      8D3FB9E1600907AEB5B1891AF176114E83D3FE832524DF4B0675DA4D8588FA31
      A685CD280191B45C582309D9ECE3B3606363039E9E661D3E2435ED9104CCF579
      589A3CB8A2846CB0D710BE6BA1076104C2B51D9FA7EAEAEA66908E0B1946B720
      A6B56D7AF776B66F2B4AC8028F53570ADCB1843CF4B5FF4233205C5504BE2210
      E8F8ECFC6C2E9357030929451232BCE22C4C29530A72F4A7D6D4544296F5C889
      A6FDCF943547BEC4427D07588725E4A85F3F8F1ABCE3F00BD7339732829A3492
      5EEBD0CE85D58770459E107F1D1F82B8F5E1F7FA13F688901EBF32DFCCEDF46A
      FAD2333E82FA62236351E56C3FB8AD6FE3DDDB539AAC9EFB51E18CA5E7249FCF
      6CB4A67C457EBCAE971E2830E608622CB320AE7740FA9801F5DE969790DB0FA5
      08F6FCFCCCAA5933971B9D3EF2BC6E5A91995CD8F0EC0027216927DE42BE0B3F
      75D4C7F5C6CB557AC6E617AAEC3372E456CEF63A7258DF0085C2867C1019ECFC
      8AAB32CC91632F43B44F19A48812CE86BA665142A6BC2CF293DA597B5DBEFB3A
      DFC7539C535E42DA64392C5DBFB8E3AB895F9EF57976A60751A9847C93A790DF
      7D5E982B950A5FBB3BDAA8CA4B489A16F1A77E756501BA2FFA8F24A453511DD6
      8E3B839AF747C73FAA9D84142B9D363C3D3D81156E560E8D408C9C3CE70E94E4
      E86D2B112D063D66AB439B70D1C34E91A2F23631993309A521976F9406E3EA70
      7D5F50A5F2A75A091948B7FFF8ABA9BE0669F82327250D13E52A30AC14AD4E42
      627F1C1E393CED2B8384E45469DE3F7BA6E61FB862BCD18C53AC6043C8B6C8D5
      E7067BF1B263B9B3C09925F7BDF23D61F0C3AB41502E1E215760BAEDC34091B7
      B344D06356CFA0B8A3335A8F3F35B7EDCCCF3ACA667A88DC7663879FB11F0EC3
      7170DCF263D95842DA3ADAF0A3BE19D6A8BB8D88B6475544939256AA3A9E901B
      D6BAB13DBB1D163FD77125933D9C7964CB40FBA0102F89F3D41DB7C8F7050A5C
      BF0A0C95362C2EB24EB85840DB22654B499304EFE0A9372E4E5281DC4E42B3F6
      0D9CA48C00FB4D8BF32DC671705C9C66E5CFC9A9861177A2C194DF061C991539
      842913EC5603A65E6CD2AF6BC37481889F2357EA4AE5C5A576A7CE27072C98D5
      70777E89BA50C4A7046E48DC8E5A7FF544F20F3D587B6444BD893F8F39FE65C7
      C1C50A4DAE1E88B4EA36E274FBF12323AFE416A959B2A5363CC9A66D096DBE5B
      D67A6B5A8E3C1D9FF85BC7D9CA63DAD6EB894FD6F7666D0E127E630FC51D9CDD
      A18F06F10625CC57A25663CEA2BF0675EFDCF00925E4B3D240AB50B9FC7EF26E
      BDC5F364BBDFE69666E3EEA297BDC869D68EEB379EC7C7B2BBB1099F91FB3B4D
      EFDDB09F8F8B2DF9F45D713AD28784A74EBEEFC5E75B0B03EA39B0D2E0E9939C
      6095AA5811D3D9F997C2524DA995801264E517F1779C7D7CEAD5B681AC5A4C78
      0CDBED20B5160C9BD94FE69B5DAC292E516A945620A87FF897922E6D5A3AB047
      5D5DBE9653B75F2FAB1325A07C6425A0058ED6B4F5D78712530B4A54BBDEEE1C
      9AC302D934EC0DB453609053DDB0EEBD5BF97BD5F376E449C47CD18D24A5D7CD
      5B7CB6A3D1BC89EA66B84CF0BA48AE2A7D9C96ADFEF9EAB3D759A9F77ED764A5
      3C2EBAFBB3D9748623DF2DB4A96D8759710EFDD67FE6F0F196790EB11BA7B945
      2D18871D7E66FD50188E83E3827E8D36557E304ACC0518B434274EF648A06CA1
      B73317E6CEC5159BD6D57F5EEDFF5742D64042F69C960C5656D6783F48109294
      D80E0DDE73873709BC453DCF8B69F2B443B9EA5CAA9C844C46AEB08284442D6D
      1FA5B2F44F37475DE7F0105ADBB99530C3C55E43979696F4B511927D570C0AEB
      5D4E42F62C2F2129DF26A383B45AF5C1968DA4F2BA5E42398F476872F295E4B5
      5B19F6C37BDA3FEBD5C601BE3FFEF8768B7A8E4E146AB7BD1D45CE4857F2B998
      FC5E85EAE5CBCC6B7BD9EE2B59585830CBDB8D0FAE4E42A556CBE0E697B89AF8
      46D238887ED7C05F9C6D2DE2D11DC35CFC8F5C7999C275598D12124B5723D343
      7AFCF662F880A66A077B313B3089FAB0826FE2AF782FF834F4AA952D2FB540AE
      2E44C2CEFAEBC3B7DFCEECD7D8A95209E9DFE990EACBCF3E7AA5D642B10168F1
      9AB341AB16B5DCF2AEB094DDDBE9E364E53279C385D7DF8F6BEB53A984ACFBD1
      9117A387862BA552714989425BAAD58176C79E6BCDC70E0FFC1984648A5E4712
      B8AC39722B6B7ABFA6F6954948A4D658FDF9302537262C949F955DA82E10F048
      9EB7A773E1BD0745414DC2EDD881923BCF32B561BE0E929C22951CF188C012F2
      FECB6C25EA596618CB9140205999FCA4B48BBD43A9B5D8862E962B35AAD010B7
      D70997DED5F3F3850C370FE2DDD95BAFDE4FECD1C43E2D57918F25A4B580B05A
      7F31251501251A3F3FEA9B66F368ABD4BC12D7013449DA5B5B51606FC7D3300C
      9FBC91A40EB9F8F0C59B7EEDBD0129A502214D085E65E4529B4FDE7B9DF9ECCE
      B1D2D4CB6F54998FCD25A48B7FBF4EE1FDAE1E8A1CF1F655D4C877AAA811E92F
      5B0EBC75C8B9DD1763CD24647B2C214364FF4AC87F25644D25A4C9B2850813BF
      3F91F858595A5AFAB8A4A4B88CB21E531F82582CEE4351E4FABA5E22F076E597
      D84B687ECA9BD29C872F4A588A837DAD74819E2287DC428D2A2D436595FABA14
      CF414E445DAFA3BF7D17AC078A99703B88C7A3FE8C0C7795BBBB08D588653AAD
      8E61FEBC979DD236D881DD1272F161CED3A830C7408A9D1A00323D53C14BB891
      2156ABB551A736347E5CA9847CF8AA30B3AEABC899872A1F76F819FBE1301C07
      C7C56970DA2A25E4B11B6F53DA3774F52C516A4BB00EC9A34170F349767EB340
      27EB52A54EC1A3095AA3543A1FF8F9B6ED83DF7AF8562A210F5C78F9A47D2377
      7B531DF2CF3BE979510DDDEDF38AF57E123165BF65C7C5E6CFCEC4F22B959037
      1E6714CBD0AF17CAF5BD6E944892949255DC34D0458C6438FB835AA5CAF9C8CF
      B79C53CFF5F5AD54423AD90A918E58AA130B05EC90077EC67E72A55685DB449C
      B5E72F0B5C50DAF3554A4877072BDBDBCFB24A1AFB3BB1B303C9198585E8D9AA
      40AE2940CCA78BF2D5AECF5F280538AD11282BE5C7C752B73613926EDB6DA8EB
      2BB47771A533698AD4BADB499C126EE4E12D9050DF4F22407E6A52C7B8BC4DD3
      B8A4BE500814794F26E4BDBBFCB8FC92188B12B2D5C0DB07B18B44CFD80F87E1
      3838EEBF12F25F0959631D92614E71BF1AF34112D2D18EE6272617132FDE2A1D
      D9E9560F4176D306D64C767E2D25647EA14A78FA52BAE394C11EECE2CA1FF6BE
      0D888E70CFB693F015B59290061DB2AEB730173BFC8CFD6A2D21CBEB9039D90A
      8F8D3F3EED31B05FD3EB554AC845B3A25F9622B16AAA43FEB0BC75FCCBEC920C
      830EF9E99C2BE3C68E6877C92021ED6D78761BB726B4A8524296D72141A10B8C
      DF91D23B6E508BEBB59290E575C85BE819FBD55A421A74C8E0FA043BE47A2F59
      1B1019E1F60487D54A423A39F2554181CED9877F53B1BB71D073BA93A350959D
      ABF93009D930E6C08FD8FD2B21FF9590FFC93824182C70E9A7C63E6C1C12F7AC
      5DED842DB0B3D0CBAED938646E89326BDB1F4F53560C0A65A7B8961E797069D1
      C030370192C7958D4392E5C721B11FEE51E39E35EE61D7B8976D691C72FC864B
      17560C0B6F945BA2CAAA712FDBE238E4A2137FAE1DD7CEDB544256DBCBB6340E
      B9E2C0CDACCFFA36714ACF5366D6B4976D711CF256CA5BBEA34448BB3B4B59B1
      9AFE3ECF3ABB50A16912E8A1AA6C1C92463DE5A4CD271F84E07148544879EF0B
      15F2560D3C6CD7FF762B777C777B760D4015BD6CA31522BAF0CED11CB953E089
      A5FBD58671483B6B315FD0AB95BFC3D27DD7F091A930223AC419F9F1BD5061C5
      E390F1BF3F4BCB4ABD7752939592534142F2DD4365B61D669B8E434E95B49D12
      8B9DA1976DCF8E43CE8EC371FF9590FF2F25A425CF6B97CEED5BF5ED86817366
      4C86E6ADCA4E7D33F8CF9C36617FCB888F0655BB8C1145AE72A993A5708B40E8
      17713FEDF88AD56B592A4CA9C1FE5C78F5401F725904C2F9B7F4AB066ACBF3A7
      C2A4B885EB38CE0ECA1618B2552B8A2AA3AA326A6ACAA3E3D5515323A0E5478F
      76EDDEBD3B7BFF60208FEB77FC588DCDDF6BBFE97BAD80D256ADBE513A734673
      D1AAD537315FF01DBF637F8B4DB62580EAB26B88E33D7346B845201CC12B7E53
      F38267CFF46B242C80481055F86EEBEF8FE333088C30033280BC1E3BAEECA497
      99332A9CB854B86A351B8ECB058A3F0AA7C39459E4110A185D55D62C859395F0
      604B353CDA52A53C2ACF23948D9BA60CC5E1A63C426C308613557D3553909A86
      FFE7A23666C909BD9504DF81D5461EF2822DE0706A5E970A39A1755A3538D46D
      04D67ED5AFBE97EA822127D5F2F952C4475F1E661CFC1AC1B7BDF4D568C4BA93
      F0211412ED3FDFC7608AD675D12BB043B716B114EE3031335AD935FC690A4BE1
      99053D092272E64EC6DEB7217CF7B1FE90AE11DF1D01530AABBAA6FDF21C729E
      DF81738BFB1144DBE9DB1907DF30D8D84B3FAA30687D3A985258D535E9842B4B
      D1F9E583082262DA564451287C3BB0999EA295BBC194C2AAAEA907EE42EE8BBB
      90B06A1841B4F9341E01952D74CE7FFD084C29ACEA1AFF8B35E4BCB80717BF19
      4110AD276F6429C257F1FB34D028E5604A61953CDAFF17A2E83E5CFA761441B4
      9AB88E052AC97E0B7BE676878FE7ED03530AABBB72114597BF1F4B102D27FCC0
      D8D7090579EE3BD8F1795718CC02955158FE72F72CDB6858A822598AAEAC1D8F
      80C67DC74851C2D2BC4CD834BB330C9FB7174C29B4581097FE6E047DF1E81E5C
      5D3F89205A8C5DC3D879D50765711E7C3BA3138C9F8F2832A17062FC2DE3B3B1
      202E3F6E044A4DBE03D7364CD12B80E163BE612BEEC2CFBAC257DF9D02530AA7
      6DBE657C365CE3BE3E5906F4E0165CDB34B5E2DAE9F2144E5BCD2AF6ECB3F16B
      717E18E8F9FD44B81EFF99E545D8A6145ABA16AE316FBD6F6C9EFE8FB58351C5
      15DABAFD7EECAAED655717412014B97EB0EAC702884444978123864675E9D108
      3BFC8CFD6A0DE4E2E1ED1D16DE6A4972D2F589C84DC0CFD8EF43B2C65A4AB87F
      F32A3BCC1ADABCF50ACEEF55AD80047C5EBAB558241C3469265BFAD0B34828E0
      65D43A6B9DBBF5EC12E4EB2551E4665E48BD7FEB2C7AB689E9D6B347AD3EB99B
      8727FDCBF92B493F1D3F97E8E1E52376F7F211EC3F7EEECA2F7F5CB9E7EAEEC1
      AB92A2609EBD6B08DF9EFDD47D060DC143198D6F5CB9B8D62BB0C114EFC006D3
      FEBA72E93BA42904F71B14D7BD7C7C30ED936E768A5A1AE5DD70C9AB2675FB7F
      D4297A8C56A3D19E4DB8E8DDA049F35EAE5E75DA3C7AFCD8CFDFCFCFCEDED9B9
      5B8E5CD5ED4B7EC884F65A07AF03254F4F989186D0DDB08BEEDEAB15CA96E2AB
      6FD74E67A54187CE73B0C3CF1367CC9988C254284E5B43FC4AF913BFEFF04E14
      3947D63CDCD602EFAC51D8FBCDFB0FFF542DA3E3F71E3EF0EDE61F9757168EC2
      16C6EF3D74B44A10BE50C41B3279D62F614DC32B6DF8C39A85D71D3265D631BE
      48C4AF14C846EAE0806AFAD81A4884B128AEF3FF4261B6A38327F164443D41EE
      CC509BDCD9A192CC710DC4973BFBD2359647877A781379CB1A4AFB0C720D76EB
      64DB8AD754DC8ED7441C298EB48E081DE8D8246F4523CFB421F5F85502258E08
      A03A7676F4A5FD042D286BBA354D4807F38401D3F8D6C13378E23A636991A823
      ED2B6865DB41129A3B25C4DA22D0E1581FC23F5CE24339F01A936A41671ED178
      394FD27224250E684F897C2268DBD0DE02C7A8457CB1FF784A4C35A69A894272
      C6078B2BC8A30E2DA5F69494AA4FCA897694161F6CABA375C5E940F0B8182492
      B00C4991947D244FA0B50178F10313262CFC33BACEE3A8D32FD9E151FAC73EDE
      24E9C4F322B025D52CAB413A7E2E4D28B5408850B8906157CFE2A11D8641C4EB
      F84090621945584730D2A2AC8661366FE034DB5102BA553D6B2129209C204FD7
      8451681D40F51E1571253E51011B6807428C4FAC4164E944AC63186B20686104
      41145F245C6829D7E302DAC9892F4072C61A4A983A4C491EF2C1E74921101E02
      C19B99181D4A4CE1B58CC06895C8A130ADB52708414C8849EB321E5104E60005
      1A2D0D0A39CA0E9E87D295F59DF0BE1E4419A32D0546AD0146A30682D2518490
      A2100F8DF28C9A11E5C2A3A5B417A16402205FDD0045424C665846B3474EE3EF
      8A8F19D0181CCBFCF73A21FF0F5D913675C58977ECC81FD5B3BE2DE3E22D7444
      7C1232991A9477F46B3402A23810ACB06002B524A20A7BA25F1089AE307CEA86
      365BF3F4EBB319ACFA4B6DBF99A39DDDC58D4F8A481B92202550ACF16301CC80
      F017431E3A44224F90ADB316EED6E9E049F183D2E76B6EEA8D91B3799CD4CCA1
      946FCF1393122A8F60082928184FFD2E5704801CC146C320C26CC646BC454791
      0FB499EA872E8BEEE79A09FF35E7DF6B66B5732E066B8A2225741A61C57B8712
      0B11083EFE4443F0059920165D4194EC45200FB5859A074F6E14BEDE7A3797B1
      D8A7CDFDB6912DE5C2F327798437FA58F6041EB444ECC2C76730F8500506DE69
      7334CF9EDC2A4A6FB1F56995D6D0E0C2C4003A6F7D63C7FCED4DEB15EC6D262B
      D8D7BC59C1CEA6A1791B9B78BE9D1F22FEDF230CA97EF3E74370646477E42291
      93597005411111B9F55AB786D00E1D705CF077A2C18FC864399C5BA2AF05148A
      8477ACB4DFB560C1A6CEAD5B778F46AE63CB96AC73168BBBDF7EF60C9B232840
      717229EE6C11C2C103C840FDDEBED7CF5F197AD95AFD28322A613A8D86752AB5
      1A94E88E6D671F59BC7863EF2FBE309871786A9A1DA7C62D014E5F00C3503D37
      9A4CE02398804FD3F0303515B20B0BE1E18B173077C30668E0E9B9E1DAFDFBBF
      3ABAB8608501CF843EAB206A311072788FAF7E6F1FBA179594408F366D60C6C8
      913065F870181F17073FAE5CD97348870EF15AAD16EFA208A8A0FA71562AD9E3
      593008FB84B717D632AB462082A30830006708DF905563C9CFCD85757BF74287
      E0E00D3FECDD8BAD2C2C29CB1A02424E9F351330D3AC6230118F075DA2A3A103
      2A023D7AF400BFA0A07CE4CAB2A6365064C81AE8B70B996515BD9B52867F00FF
      B8198FD4265963B3036547C91BA82B0F6619486FCA91A508670797535CB60C59
      050B60B86056007A9690008E616102C3D4044E8C77B39A66B53C18F65797072A
      CDCBF3C97DF3A6501216B6C2D80F71770F1FDCA54B9421ABE5C1304205205C8F
      4A9E3FFFC954C8797A79E17623CA90D5F260D81FB1842CC9C9319B37C24359B8
      2528E21C2572776F3AED934F224D8D4A7133A7EC1D1F0CB472D3A6CBAF2E5EBC
      C7A5B7389DE163E5E7D787148BABD4117572F97B94136CCA30F51F3A53B3AE9D
      6156AACCF385A83EFFA55570372833009855A7E4E131DFD247C6EDAB8649B049
      172A76FC8C67D3D8673CA2C3A23B3AB7183185ED6B5CDFFEC3BB37A70FCF075B
      83597BF66037337D9C66CA7E41D6A9FF08D9E3C78FA1C9A04D2014E2B651A857
      D03F9EE016D07394DBFB7DE320303010CE1DFEB162D6BE8B303E2776E8334476
      E7CE1D6C2C01F0D974D819B657E32D67788B63484808FCF9CB3EE3293153B9A5
      97B4E9A139B86A608B0C4545FA53BC5E9152282585C6706CB42151C187629193
      30DBA93EBBA7566F631195556CECC5E8F09104E8D77102ECD47C6B18BB6C0F8C
      5CBC13862EDA013D66AE05C788C1902DF5B779552722E08557EBB2AC2D2F1B93
      4B8CEAD95B5658583639731F7DB4714B7783121FE584AA8602FDC8C5EBD76150
      E7CED063F6EC8928CAB9DF57AE4CD18B5A13F333EC196B9CA561FC5CAC2D61EB
      15C51DB8254452D2206E3B8585AD3715B7B4A981180C82192D97CB4189E454B1
      40680684AF013D7BB2949188F1BF5EBB56F8D428FC4D4C1F95969602CE9A4142
      62CB446622D73895CF43AA21C5EE6F2F6B454C28C254B032886B9A716B429617
      B98684247B301759266A4D8070E1343D3B4CAB296B96A01C18FE8F80CA44D5AB
      62BD211D96326C9E0737419CC30D23C1097B4C018D7E848764361F650D3F9714
      15194725E823E9003D5C50579CEB85512614F1D4A530B04B937253CEA8E17F91
      95F154E8B41D519F692A21454F4AC0BE2E09831B36F4763795863E560434B025
      A1BE8480FAB6040449003902D26F3C7972EB65C694D2F474BCAA5E639090784C
      C8BD390FBE9012105813D993C740CA4D352CC38B13407FE6F1BF12F24324E4FD
      FBF7A1C9B06D1625E4DB9D2321343414CE1CDC5E71B0CED4F854E0846D7063F7
      1EB878F1347B020CDE41CE4385AF18A982850505E0DCF633081C38004EFE6409
      A85C1519346430BC48B5DCE6F9D6ADAB3F5BD4422F8458D9A24CB04DBBAA9355
      7536964137FAB6156994D9B3AE5BA008D7F68B77F4B35552A994152B4D02CDCB
      28CDAA3D16285A5C768843E2F4EB6AD95B934301F1E56C6565F68E2BED372D78
      468ABEE416C5100B1A9501CDB8A9925D4A4A62A9C9CBCB339F5B437E587A366D
      D0005637E71B811671D36DC4DC9032A099494AD9DB9C9C2A79E4E1E000AB6402
      23D0D2071624246E299E3D7B664605A60CDF0DA2D8153D5B3204467C56C6CBC4
      3977E4B2F4AC2CB30876E57824168B614523B191A235291C505F7744AEDE186E
      E2E7F74A6457AE5F37FB6A4101E6DD0E2B04BC3CCC8A057A5B0A7024DDA473CC
      49C8C4F98F8AD96389AABAACADADE1ABFAD649EF54D0F4B74C736554843C6C62
      EDD895A8202A97950A550137924835F82D9F5D4654649884FF6F96907FD78A28
      829A45E9E587CE38C8F1097271155230B00BB91F8DCD2C1E4AFA5A63C19E7F99
      45BE61BFCC791DB9EC48DF82FAFEF5AD1F3D7B54FC45DF23B6BD5678E1147A20
      81A5014DFD49C8B331454C01E32606B170F5D181F220BF205B4747470ADFF13B
      F6C7E12CE5383E531EA894ED4D60D1D70154D0B3AE435DB720DF20B19D951DA8
      E56AC077FC8EFD71381B0FC72F2D3FC1822BA01616324AE67B50828F2DDFD6FE
      6DEA5BC8CFCD0745A902044201D8DA21F94DDBD8A3F0E14C31F38A10109F42B9
      034F2976B91BD2F210C987987CA65F8B362D025D3D5C49CFBA9EF0E6C51B681E
      D51C6C6C512340F3A887371F261334D11395E762269D8105C316C0A24506C30C
      A80E13AD5057AA35722D896BD15F473383B60D62461F1DCD84CF0D67EFF81DFB
      E370361E3644D8B4FC897B4DC19F0827BE46EE263486AB918B2335FDE2FB31C3
      0E0C635ACD6FC5DEF13BF6C7E1381E8E8FD39903358495D008BA23670521F063
      F3CF9B17F7D8D2831978602013B12482BDE377EC8FC3D978383E4A670E84679B
      1B71F71098567778DDB731DB63987E87FB31EDBE69C7DEF13BF6C7E1A6F11933
      F3A098FB42F6CB5921A6C7BC7BF12EC3BB95B733EA87D08435012ABE0A145A85
      06FB030F62508ACD284D49454B886D8C6E25B48231E83ECB7382E7CBE8A3D1BA
      A8FD510CBEE377ECCF85AF34A431CF9A1FE73AC3EFEC0A485FB08168F8DEED53
      B7E7910723B5F88EDF597F3B2E1E97C65285279126D486D3877CC11AA9374D51
      35E805D7D8BB352BE87172671423028C1B7A2B0211FAD15CD65AAE6135A6A543
      8070B8D1F8DB7F2A82FE9748C8BF6DF521F1DB44D2387E82B06DD10FE03E435D
      8EB90609AAE19A9D5494815B280F05581FEBBE4E6B91A2FA7C89CB84C0EEF367
      467EF9D7C298952F97745EFD6659E7D5AF97C5AC7CB12472EE8D8581DDE6CDC4
      7170DC0A0D27C7210FA17D9D812DC6EFFFC4CAD6D98B9D13D1C84DCB065F6425
      F1F48B18E6E91ADC31E47AFC5051494E1A56A7DF9629A37AA4F6217D9774B3B2
      B1F30275156DBF4E0556B68E5E213DBFEC7675DB68DC0BDD5D5E3DF675F46954
      0F1B582DD3958A51DF351D94EA4CA40F15A13E9F82E3A510784E4EF5503A5FF3
      91516E1499D1A9093C2BA3D5C9A144F914E48949A0DE9800CCF016200CAF8300
      0CD36A3AD489CE21503A73336A7B46B1FC1EDD6EF4A6292EFE7543E4A58F2123
      AB105EFEF114A28B7DE0C2A9DD1076689259B9C97C92FC2071FFD61F066FD16E
      3136905A7D3B75E6C6C1AF422346CE75D4C23BD707C90C749DB801B6CE1B021D
      A6F6023E55B68CB6F07D7AC6BD633FFF8954E4336614EDF884E4746C6862EB63
      33DBCAC7AA5364E761761B7ECCD675681742D60F7063DB42FCBBF7CE1EB9FAE4
      EA999BCAE282DD2401B73ED951568E68B18F0EECFDF1FC19D8941417DA47769E
      60B77DED8A97C43BE2EEB564C8BCC6FDE298EF8F8D493C79E8014AB6E47916E4
      FC7E97414A82C9756001C0995D44BB5F3612C74BD33F67D6CF27CE8FED0F1F75
      694840D73002BA8492E48448AAE5AA586A1F76FECE651D3BB37A8A80FC0D206B
      E71117FAC7B0C648B13CC286B5FCB9521C1615440EFAB23B85D7C28471616233
      A04DD3E173A6F809B3EE4BE2CF79E39050D537055880D972F5CD89136C6E2642
      0E87F1CC5A9171DFC0B2BBCF820E49EC602D2D804BF0DF77AD5EBD1AFAF6ED0B
      B1B1B1C6CF3277EEDC8023478E38376BD68CB5CD3A74E850C0C64CABBC121313
      A17FFFFEC4C8FFD3DE7140457565DFFF7F7A630ADD01E9E850A52945690A038C
      9408D80858A22086A851C112E331C458584BD68226279A62622C1B65C9D1B545
      D4B8C4C468348A5150B0203108080C0C6566FEBEF7678081011D37BB7BB267F7
      9D730FC37FEFDDFFCA7DF7DD7BDFFDEFCE9E0D62636385898989076262622E28
      148AF2A4A4A40DE8F9AC59B3B0D0D0500025B9C1912C58B080A28979F3E661E8
      EDB0E2B1B9D9D9BB16BDB96458446494FBF4E9D3CF40C445284F2E97632E2E2E
      83232A282840ADC051B361ABA64646465EB5B1140227A9104C4B538098E87120
      4E1E5B999A9A366ADAB46960326CF9A2458BFA23494848A0AE958D1E3F1EB3B2
      B6A14786873D8A8A8C382E714FF4B5F4C9CAF58D2DC876094A739E9195913651
      213F3B62A40C4C4A4DD5B18C397DEEB5842B54A36EDEBC897575AA486FCF118B
      99025B31C1123A4864B1CBEC645E0A3A1B286CDC22726AE9531EB21B8E4BA552
      EBEE96E6E6EB6EEE6EB8482426D1D8520C0FEA650497C305E662816D5363D3F4
      A7E20504EE90E8226A2AC5780F0FA9277AD2D48BE51C06DFAC7E69356731AE7C
      56B750D5D925B4B1959298C1E587B4DAC78F8986FA5F35523BDB02DF8009CD51
      537C7D9B5B9D40A0B50BF9A0EA3271E67C39D8F3FD375A1E470A08628A9B961F
      F6A3A3C59DD77FB9F1DDBB5C810DBD47D4C76DACADBBFCFC47073536B60526CF
      C8BD201F0504119E84FA611B1DDF72A49EC4F91CB29A1E8D6BD8C38194FCD292
      A5AE75AB7ED8B0D2CEDED5D1DEDEBED7F88805070743BAB02C1375DFDD4ECCF8
      799818BFB2D997B8A9FEE6F443C6DE3D27B4B63E23B1D64E734C36295DCDB674
      26C4A0E2FDEE0379761AAE15AFE65E95BCA2A24287293D3579E9C48949A72D42
      B7005A42535C4A5125B9BAE82F1DD2514BB57CEF255AB6E3742DCF364D6315F6
      7967416987E6074DBBFBC6AD85202E2EEE727272724AAF804562F409743A7182
      FFEC087670ADA06CAC8FFD39C791E14C0BCF24926F1946F2CCC348B65D2AB072
      0A618C19AE3E5C76975D7DECF0D788C59641FD36A39F4F447B7B7B385F5D53D4
      8555A9BEBD6F291FE92CF994EB1E9AC0259EB1E81E42926303DAD85C7060DDC5
      EEB923DA2E80E696569CC6E247D7D4D4ACE825243F3F3F065C4F475E99945A92
      1C159838BF60953C6EE5A7C9E3DFFCB858BEFCF84F09AB4AAEC62FD95E9C985F
      3C2D76CE9ACCB8A8D0F9A9696997E00AD8317AB4813374767636282C2CC4274F
      4E5F268F57FC35363AA254111D7A24212A788F22D2776742B8DF8EF8C8D00F13
      A2C7EE9F1813BE5F9198B4372D35752AA2EAE5CB97F7092139393960EDDAB520
      3E3E1EF8FBFB032F990708F3720261011E609C9F2308F5760061FE9E60EC683F
      10111C00C6858753EC26232383806CE68F2C4386EF7932549E7FD6044B7E5D23
      DCCF7FAC6FD59B0C8D8FF56759E91079AFBBDE8715EEC35C41AF8F576EE6045B
      DBCA879D58F92F0D48A04297E083B6966628B4F46D43D797EB9C793187A567FB
      C47F8240FE8F8023A084864399B10EB28A7B6DE072653DBA062BBDBDA5057476
      A880D6C0C856531439B8D3208A06AF6C6A023406C3B1A5AD43F6F0D7DF40475B
      BB0A859CD76886F65EE8D7220CC7B7E8BF8544C95C1EEFE97FFECE33D05EF508
      8DCF53FD7E7F8BD46A1719B5486D70973F2CF4238BCF8E494C1F23537693E0D2
      5335A8E34B80E7046B7F0B16067E28BD7453ADEADC37D855C6FDC35C6BB5FB9A
      EAEAC1171F9F5D16346D82C77D920E9AA024D00085BC8A83276F6A5ADBD6B338
      9C7D184118F72D286B2D05A367BC0746CCDD0344330F03EE94CF3204D9476F84
      5C2449873324C99F7BF4067A86F2501954B6A75E6F827B1805292929689705E3
      A7E402BBF4CD809EFC610633EBC0CF0C08E8377A86F2501954B6A7DE90AE9E22
      4D3D1803EAC15342B4EFB766334A3AB1249AEF9B934DCF0DC06788880F071B39
      4D52F78A4BB04620A13582811675BD5DAD54FFB76FC6D1A740FAF46A7E7EFE2A
      8140E0DA01894EA56AA7824BAA29EB658F82C9404CB072DFBE5D853D527F69A9
      CEE99B40C66F74A8D2DDDD1D01B7EEE91289040885661044402C160389C41C58
      584055D6C20AD0A024367CB893E4E8D10365B0FCA5FAFA7A505757D76787F4F0
      F040D6F4C52525257F2A2F2F473B2FE8EAEAA4CE1D75E66D06657D6A5A7F1FCC
      BAB4082C5C3873496B6BEB2654AEDF490DC42C865D0A92C964CE1084EEEE6EC0
      C5C519822B405BBABBBB0B056E777CC0EDDBB76ACE9D3B7D0D6E43B7415F3C44
      2A2181D313B6680AECCA5A530095A50C207A6FE0FF6BD9A625DC70CDC16D8609
      D7522084C9105ED3FF0D44CF513E225E4318CA4393A9D56AE34685C5C6CD2AD8
      98B27EDBDE196F6FD83635705C5C3C7A0E5E10498866708EED1B101EEF131435
      31DC42C064B118040433FBB8A449E61AAD06BF78AA0491F0A5170E3694A95FCD
      7B6767029BCB37727EC7351D8DEBF3E71C31D4AA513A79F2A4B1ADB6B3B353C4
      E3F12D90FA6E69C6624A780CAA2BED9D1A75A3924647F9264D3F942C66BFBB63
      7F02C6E48A470E13F4FB16F4EE83C7CFF273A66F876FEFE7738CD665BFA83094
      15D1C7676CCA94ACC49849592176120E0BC7740B9A46608C0F8AB77F7BE4E017
      9F402E51DE53111D55236F05233A52A954978BB76EB875E493ED573B94CDCD2C
      3ACE6C6DAAD76CDBB4FE8ACCD5199DEE79424466E88E7674E6860ECE87E2902A
      A552F9E5FB456B6B21DCD79BC890D9A2061DFD97969EF860C58A250C26937908
      FEFF9BB1E11CF963E9BB87DE84DE680888E9C1BF6298B7AABCFC3A191010B028
      242464188AAD411AC53B3238EF1F2235E238FE51787800EBDAB57B9B33335368
      708C8A866A1106FAA27BA059EBF94EDB41EF6E2683031B0DBBB6C1CACA0A458A
      71346C51EFACE9F58A9E3811C420801B002AA782759B8CA6FFF7B2913F20878C
      5DA8F85D084E6CFDFADFC421A97F6E6BFA31AFD6EFEB9C898AC78A97463436C4
      FE0BC70E5684F04163E06886F2BC62BCD9459190213105513FCA2E3D767D8ED4
      5AE39B122FF92A2642245DBDF1C186A7B6E69FBC748B04A3258D7EDEFCC2607F
      9EF46E8D4A75E6C2D35DD4F35AADF54B21C27EAE0DF6F3E204DA583180AA5DD3
      D1DD4DA3442B6F4F9B8DCE5DECC92623B2B0E08C19374640399578C938A29000
      2E7581F8B74D4F32EF3254070CA61C7B2EA2CE6E74B78F6EC970390448518873
      18B79E78189669B9D4208E991070F0B988AA1F688E7E7759D97B981D1F25765F
      9863FB3799BA65B7795DFD326F75EBE6D7670ACF077A731B9F3B6BE250AB5F4F
      9D6F2E1D13C09F4183EBFD66150E981CB174DD725E6F1CDA13E79E3DBEFA936A
      13F0E20F8D08A5CF0F77E5B2598F6CF35EB38DB11693A0AB578604E0ECC596BA
      92930D2B355E96778C461BADB58100222268393343F27714C59CFA6C775CC5DE
      9DB1975715447D264FF4F3362AFBDFB168FF4793062368ED00377406C8D39F13
      F60AFE2F952073629E6673DE9E334C8A042F45AF5E6D2A02158365FB86968CCB
      A3D176BA8EF2CBAC128978A742963A82378ED981F895B626CF1A4B24CEB0140A
      0FFCA0EE0EBD666D4B320442286658A0ABD552607B0A4C4614B2ECF409BFE29F
      1EB524E6357BE20013F238009CDA8474D13940AB79CF342CBBA196BC9B2C8370
      0DC2E27C4F2F79A29333E237E92F478FBBC977B0DDE475B0E9E9477A9D4BE74A
      00C0342A42009C00C20424C82DFC2DEA3783930EBE5EA322308C0125C31510D3
      61248421EAC04DA08F0DFADF6F816C0C9D88EED490649916A02F76009291D07D
      2459CFA78F1D9D49FA7129E93B8BA74E2990D31A3AD043FE54F1102270441F4F
      3022FD0E413B15E6EDB3C675F870B4F5B061653EC069853D96EA1EFB39D0C560
      47DE6B28BC434F90B7321AA20F5657E786E34D0D406A61DEC652AB59C08A7284
      2ED2137E09EC52EF2D67DDA1783F818A7E511703103F3B337357C1E14333837D
      471D72A5D3D9021E8F00637291EA138CBC5F20140EACD8038688F128C775F28D
      559115A2795716BE4293157C7505B6D8CC5EE7B8D7A19C0D5BD36DD2DE0FD1BE
      45AA5A70406392C7C7176AC1A177A13EA15281A6DA04B04C5A6F64A81AD035C3
      C3CC75802D70A7CE88BADAEB300CF7240F2F2904FBE73F1A0AC9C06EE9B6A36C
      6CBF7E0AD10807511EB0DD2AF44125FA28FB0400C024690433A00FB4929FE98D
      270CBD4E8F148EB217750DB5F03FA365BBEC527A9A8A70D06F1B5DB7350AD434
      E656F8AE99A6F26323440E3B9591388DF1310648FB97E9622F22BB2D804D672A
      D743CD31EF9FD915A83172DAD512446729AFC2EA6F0C44E254ACAC84F06787E2
      F638F4B22107DDB1B87515E475AB01002F6472707EEF12A4565E952BA8329A35
      F225F67F58C5598BE17F473D30EA5A750E6F0DC0B42816E16D13F15940E6F88D
      DD2ED530A3C1BE9723F81EF67F141A6C3060B03540E38C032206D385B59EA8CB
      23B974A041C3317748FB1135FD18E51D464DFFBD79BC5EA44E3B94E3E040A0AB
      66DCA87700D2A33A57707B50CAAEC9E59D25D45D5E24C0F60ECCBB379F779E64
      F3D0B51F87749383653E57D0AACC13B754CFE3CE822DF31A98573D0374549BF3
      A6C2667E095BE46792C45695C3BB312819A4014D07C19B0DA7DEF2B96BCD9454
      3B17B4BB6E2392FE253264651EFBD11F5734FC076327962E526D98CC00000000
      49454E44AE426082}
  end
  object ScalesImageList: TTBXImageList
    Left = 48
    Top = 240
    PngDIB = {
      2300000089504E470D0A1A0A0000000D49484452000000100000023002030000
      00F2AE03ED0000000C504C5445000000000000FF0000808080E6239CB1000000
      0174524E530040E6D866000001CA4944415478DAAD56516AC3300C95C0FE5720
      B98F0A394006D121B753F4669D9E62A7DBDA416DD7859718272F4F7AB255A2FF
      069B397C7D5E0B985D896EB72B358E04989D6A5F31C35400EAC058E53DB5F0ED
      D0C644796350F14925F896FAC287B5F02D0055978197530629EED47F7C2C2C2D
      7C104419614D881720177CC2C0C758E0267D1A8AFC35DC241BD5778FF7CEA7E0
      D32E3EB66272DEC25A013BE44636C38F4D3BF5816119D4F7970F20CB40FE0E8F
      BD0AB357A14D0EEC831240000AB078E4753E24D077894DB4A2BA93F339084001
      C694BAF4A1E65076954FC0A7CEC736C037830FF1D27BE25D913FE873556DF1FA
      A741A5B00207836FE34169776B4FBE116B39F425849A10AA409F409F562B48FB
      4B8F46F53D2B65CF696FFE6264F5376402CF049E096E5EAAC804D265DB5BF812
      8E9619BA56989BB02FEAFEC0C4F952973E29C552F90457376569E9220F7CF3B1
      6F8F24BE21DE15F9ABFBA32DDEACE558960A6CBFA6E591D6FEE6F196FECB7676
      E200E29EF3390EE480BCD7A61E9D84C8B4A9F66ABCE89AC28A16754EDBE38DCD
      C9E81AA980E72F9AC8563A315193BEADFC290A29684563FA7EF87BF28DF81BC9
      0E03AA0B793FAD38FC10EAD307C8A3FA9ED573DE5ECDDF37BADD599D96ACDE3E
      0000000049454E44AE426082}
  end
  object MainPopupMenu: TTBXPopupMenu
    Images = MenusImageList
    OnPopup = MainPopupMenuPopup
    Left = 208
    Top = 168
    object tbitmProperties: TTBXItem
      Caption = 'Properties'
      ImageIndex = 31
      OnClick = tbitmPropertiesClick
    end
    object NMarkEdit: TTBXItem
      Caption = 'Edit'
      ImageIndex = 31
      OnClick = NMarkEditClick
    end
    object NMarkEditPlacemark: TTBXItem
      Caption = 'Edit Placemark'
      ImageIndex = 31
      OnClick = NMarkEditPlacemarkClick
    end
    object NMarkDel: TTBXItem
      Caption = 'Delete'
      ImageIndex = 30
      OnClick = NMarkDelClick
    end
    object NMarkOper: TTBXItem
      Caption = 'Selection Manager'
      ImageIndex = 10
      OnClick = NMarkOperClick
    end
    object NMarkNav: TTBXItem
      Caption = 'Navigate to Placemark'
      ImageIndex = 33
      OnClick = NMarkNavClick
    end
    object NMarkExport: TTBXItem
      Caption = 'Placemark Export'
      ImageIndex = 25
      OnClick = NMarkExportClick
    end
    object NMarkPlay: TTBXItem
      Caption = 'Play'
      OnClick = NMarkPlayClick
    end
    object tbitmMarkInfo: TTBXItem
      Caption = 'Placemark Info'
      ImageIndex = 27
      OnClick = tbitmMarkInfoClick
    end
    object tbitmFitMarkToScreen: TTBXItem
      Caption = 'Fit to Screen'
      ImageIndex = 43
      OnClick = tbitmFitMarkToScreenClick
    end
    object tbxtmAddToMergePolygons: TTBXItem
      Caption = 'Add to Merge Polygons (Ctrl+MLeft)'
      ImageIndex = 62
      OnClick = tbxtmAddToMergePolygonsClick
    end
    object tbxFillingMap: TTBXSubmenuItem
      Caption = 'Cached Tiles Map'
      ImageIndex = 7
      Images = MenusImageList
      LinkSubitems = NFillMap
      OnClick = tbxFillingMapClick
    end
    object tbitmHideThisMark: TTBXItem
      Caption = 'Hide'
      ImageIndex = 19
      OnClick = tbitmHideThisMarkClick
    end
    object tbsprtMainPopUp0: TTBXSeparatorItem
    end
    object NaddPoint: TTBXItem
      Caption = 'Add Placemark'
      ImageIndex = 15
      OnClick = NaddPointClick
    end
    object tbsprtMainPopUp1: TTBXSeparatorItem
    end
    object tbitmCenterWithZoom: TTBXSubmenuItem
      Caption = 'Center With &Zoom'
      object tbtpltCenterWithZoom: TTBXToolPalette
        ColCount = 5
        Images = ScalesImageList
        PaletteOptions = []
        RowCount = 5
        OnCellClick = tbtpltCenterWithZoomCellClick
      end
    end
    object tbsprtMainPopUp2: TTBXSeparatorItem
    end
    object tbitmCopyToClipboard: TTBXSubmenuItem
      Caption = '&Copy to Clipboard'
      ImageIndex = 28
      Images = MenusImageList
      object Google1: TTBXItem
        Caption = 'URL to Google Maps'
        OnClick = Google1Click
      end
      object YaLink: TTBXItem
        Caption = 'URL to Yandex.Maps'
        OnClick = YaLinkClick
      end
      object kosmosnimkiru1: TTBXItem
        Caption = 'URL to kosmosnimki.ru'
        OnClick = kosmosnimkiru1Click
      end
      object livecom1: TTBXItem
        Caption = 'URL to Bing Maps'
        OnClick = livecom1Click
      end
      object osmorg1: TTBXItem
        Caption = 'URL to osm.org'
        OnClick = osmorg1Click
      end
      object nokiamapcreator1: TTBXItem
        Caption = 'URL to Nokia Map Creator'
        OnClick = nokiamapcreator1Click
      end
      object terraserver1: TTBXItem
        Caption = 'URL to Terraserver'
        OnClick = terraserver1Click
      end
      object Rosreestr: TTBXItem
        Caption = 'URL to Rosreestr'
        OnClick = RosreestrClick
      end
      object tbsprtCopyToClipboard0: TTBXSeparatorItem
      end
      object tbitmCopyToClipboardMainMapUrl: TTBXItem
        Caption = 'URL to Primary Map Tile'
        ImageIndex = 45
        OnClick = tbitmCopyToClipboardMainMapUrlClick
      end
      object TBCopyLinkLayer: TTBXSubmenuItem
        Caption = 'URL to Layer Tile'
        ImageIndex = 45
        Images = MenusImageList
      end
      object tbitmCopyToClipboardCoordinates: TTBXItem
        Caption = '&Coordinates'
        OnClick = tbitmCopyToClipboardCoordinatesClick
      end
      object tbitmCopyToClipboardMainMapTile: TTBXItem
        Caption = 'Primary Map Tile'
        OnClick = tbitmCopyToClipboardMainMapTileClick
      end
      object tbitmCopyToClipboardMainMapTileFileName: TTBXItem
        Caption = 'Pathname to Tile in Cache'
        OnClick = tbitmCopyToClipboardMainMapTileFileNameClick
      end
      object tbitmCopyToClipboardGenshtabName: TTBXItem
        Caption = 'Genshtab boundary name'
        OnClick = tbitmCopyToClipboardGenshtabNameClick
      end
    end
    object Nopendir: TTBXItem
      Caption = 'Show Primary Map Tile'
      OnClick = NopendirClick
    end
    object tbitmOpenFolderMainMapTile: TTBXItem
      Caption = 'Open Primary Map Tile Folder'
      ImageIndex = 34
      OnClick = tbitmOpenFolderMainMapTileClick
    end
    object TBOpenDirLayer: TTBXSubmenuItem
      Caption = 'Open Overlay Layer Tile Folder'
      ImageIndex = 34
    end
    object tbsprtMainPopUp3: TTBXSeparatorItem
    end
    object tbitmAdditionalOperations: TTBXSubmenuItem
      Caption = 'Additional Operations'
      object NGTOPO30: TTBXItem
        Caption = 'Current Altitude by GTOPO30 (~1 km accuracy)'
        OnClick = NGTOPO30Click
      end
      object NSRTM3: TTBXItem
        Caption = 'Current Altitude by SRTM3 (~90 m accuracy)'
        OnClick = NSRTM3Click
      end
      object tbsprtAdditionalOperations1: TTBXSeparatorItem
      end
      object DigitalGlobe1: TTBXItem
        Caption = 'Images available (F6+MLeft)'
        ImageIndex = 11
        Images = MenusImageList
        OnClick = DigitalGlobe1Click
      end
      object TBXMakeRosreestrPolygon: TTBXItem
        Caption = 'Make Polygon by RosReestr (F8+MLeft)'
        OnClick = TBXMakeRosreestrPolygonClick
      end
      object tbsprtAdditionalOperations0: TTBXSeparatorItem
      end
      object tbxWeatherUnderground: TTBXItem
        Caption = 'Current and Forecast Meteorology (Weather Underground)'
        OnClick = tbxWeatherUndergroundClick
      end
      object tbxYandexWeather: TTBXItem
        Caption = 'Current and Forecast Meteorology (Yandex Weather)'
        OnClick = tbxYandexWeatherClick
      end
    end
    object tbpmiVersions: TTBXSubmenuItem
      Caption = 'Version'
      OnPopup = tbpmiVersionsPopup
      object tbitmSelectVersionByMark: TTBXItem
        Caption = 'Select by Placemark'
        OnClick = tbitmSelectVersionByMarkClick
      end
      object tbitmMakeVersionByMark: TTBXItem
        Caption = 'Make by Placemark'
        OnClick = tbitmMakeVersionByMarkClick
      end
      object tbpmiShowOtherVersions: TTBXItem
        AutoCheck = True
        Caption = 'Show other Versions'
        OnClick = tbpmiShowOtherVersionsClick
      end
      object tbpmiClearVersion: TTBXItem
        Caption = 'Reset'
        OnClick = tbpmiClearVersionClick
      end
    end
    object tbsprtMainPopUp4: TTBXSeparatorItem
    end
    object tbitmDownloadMainMapTile: TTBXItem
      Caption = 'Download Primary Map Tile to Cache (Ins+MLeft)'
      ImageIndex = 21
      OnClick = tbitmDownloadMainMapTileClick
    end
    object NDel: TTBXItem
      Caption = 'Delete Primary Map Tile from Cache (Del+MLeft)'
      ImageIndex = 22
      OnClick = NDelClick
    end
    object ldm: TTBXSubmenuItem
      Caption = 'Download Overlay Layer Tile to Cache'
      ImageIndex = 21
    end
    object dlm: TTBXSubmenuItem
      Caption = 'Delete Overlay Layer Tile from Cache'
      ImageIndex = 22
    end
    object tbsprtMainPopUp5: TTBXSeparatorItem
    end
    object NMapInfo: TTBXItem
      Caption = 'Map Info'
      ImageIndex = 27
      OnClick = NMapInfoClick
    end
    object NMapStorageInfo: TTBXItem
      Caption = 'Map Storage Info'
      ImageIndex = 27
      OnClick = NMapStorageInfoClick
    end
    object TBLayerInfo: TTBXSubmenuItem
      Caption = 'Layer Info'
      ImageIndex = 27
    end
  end
  object TrayIcon: TTrayIcon
    PopupMenu = TrayPopupMenu
    OnClick = actViewNotMinimizedExecute
    OnDblClick = actViewNotMinimizedExecute
    Left = 520
    Top = 112
  end
  object TrayPopupMenu: TTBXPopupMenu
    Left = 520
    Top = 144
    object TrayItemRestore: TTBItem
      Action = actViewNotMinimized
    end
    object TBSeparatorItem1: TTBSeparatorItem
    end
    object TrayItemQuit: TTBItem
      Action = actQuit
    end
  end
  object tbxpmnSearchResult: TTBXPopupMenu
    Left = 160
    Top = 264
    object tbitmCopySearchResultCoordinates: TTBXItem
      Caption = 'Copy coordinates'
      OnClick = tbitmCopySearchResultCoordinatesClick
    end
    object tbitmCopySearchResultDescription: TTBXItem
      Caption = 'Copy description'
      OnClick = tbitmCopySearchResultDescriptionClick
    end
    object tbitmCreatePlaceMarkBySearchResult: TTBXItem
      Caption = 'Create placemark'
      OnClick = tbitmCreatePlaceMarkBySearchResultClick
    end
  end
  object actlstMain: TActionList
    Left = 328
    Top = 136
    object actSelectByRect: TAction
      Category = 'Operations\Selection'
      Caption = 'Rectangular Selection'
      Hint = 'Shift - snap to active grids (if enabled)'
      ImageIndex = 10
      ShortCut = 32850
      OnExecute = actSelectByRectExecute
    end
    object actSelectByPolygon: TAction
      Category = 'Operations\Selection'
      Caption = 'Polygonal Selection'
      ImageIndex = 13
      ShortCut = 32848
      OnExecute = actSelectByPolygonExecute
    end
    object actSelectByLine: TAction
      Category = 'Operations\Selection'
      Caption = 'Polyline Selection'
      ImageIndex = 21
      OnExecute = actSelectByLineExecute
    end
    object actSelectByCoordinates: TAction
      Category = 'Operations\Selection'
      Caption = 'By Coordinates'
      ImageIndex = 12
      OnExecute = actSelectByCoordinatesExecute
    end
    object actSelectByVisibleArea: TAction
      Category = 'Operations\Selection'
      Caption = 'Visible Area'
      ImageIndex = 20
      ShortCut = 16449
      OnExecute = actSelectByVisibleAreaExecute
    end
    object actMakeLinkOnDesktop: TAction
      Category = 'Operations'
      Caption = 'Create Shortcut'
      OnExecute = actMakeLinkOnDesktopExecute
    end
    object actFileOpen: TAction
      Category = 'Operations'
      Caption = 'Open...'
      ImageIndex = 34
      OnExecute = actFileOpenExecute
    end
    object actZoomIn: TAction
      Category = 'Operations'
      Caption = 'Zoom In'
      Hint = 'Zoom In'
      ImageIndex = 23
      ShortCut = 33
      OnExecute = actZoomInExecute
    end
    object actZoomOut: TAction
      Category = 'Operations'
      Caption = 'Zoom Out'
      Hint = 'Zoom Out'
      ImageIndex = 24
      ShortCut = 34
      OnExecute = actZoomOutExecute
    end
    object actShowGoTo: TAction
      Category = 'Operations'
      Caption = 'Go to...'
      Hint = 'Go to'
      ImageIndex = 11
      ShortCut = 16455
      OnExecute = actShowGoToExecute
    end
    object actMoveMap: TAction
      Category = 'Operations'
      Caption = 'Move'
      Checked = True
      ImageIndex = 8
      OnExecute = actMoveMapExecute
    end
    object actDistanceCalculation: TAction
      Category = 'Operations'
      Caption = 'Distance Calculation'
      Hint = 'Distance calculation'
      ImageIndex = 9
      ShortCut = 16460
      OnExecute = actDistanceCalculationExecute
    end
    object actSelectByLastSelection: TAction
      Category = 'Operations\Selection'
      Caption = 'Last Selection'
      ImageIndex = 44
      ShortCut = 16450
      OnExecute = actSelectByLastSelectionExecute
    end
    object actSelectByLastSelectionEdit: TAction
      Category = 'Operations\Selection'
      Caption = 'Edit Last Selection'
      ImageIndex = 31
      OnExecute = actSelectByLastSelectionEditExecute
    end
    object actSelectBySelectionFromFile: TAction
      Category = 'Operations\Selection'
      Caption = 'Load from File'
      ImageIndex = 34
      OnExecute = actSelectBySelectionFromFileExecute
    end
    object actCircleCalculation: TAction
      Category = 'Operations'
      Caption = 'Circle Calculation'
      Hint = 'Circle Calculation'
      ImageIndex = 24
      OnExecute = actCircleCalculationExecute
    end
    object actShowCacheManager: TAction
      Category = 'Operations'
      Caption = 'Cache Manager'
      OnExecute = actShowCacheManagerExecute
    end
    object actQuit: TAction
      Category = 'Operations'
      Caption = 'Quit'
      Hint = 'Quit'
      ImageIndex = 29
      OnExecute = actQuitExecute
    end
    object actViewFullScreen: TAction
      Category = 'View'
      Caption = 'Full screen'
      Hint = 'Full screen'
      ImageIndex = 4
      ShortCut = 122
      OnExecute = actViewFullScreenExecute
    end
    object actConfigZoomToCursor: TAction
      Category = 'Config'
      Caption = 'Zoom to Cursor'
      OnExecute = actConfigZoomToCursorExecute
    end
    object actConfigUsePrevForMap: TAction
      Category = 'Config'
      Caption = 'Use Maps from Lower Zooms'
      OnExecute = actConfigUsePrevForMapExecute
    end
    object actConfigUsePrevForLayers: TAction
      Category = 'Config'
      Caption = 'Use Layers from Lower Zooms'
      OnExecute = actConfigUsePrevForLayersExecute
    end
    object actConfigUsePrevForVectorLayers: TAction
      Category = 'Config'
      Caption = 'Use Vector Layers from Lower Zooms'
      OnExecute = actConfigUsePrevForVectorLayersExecute
    end
    object actConfigUseZoomAnimation: TAction
      Category = 'Config'
      Caption = 'Zoom Animation'
      OnExecute = actConfigUseZoomAnimationExecute
    end
    object actConfigUseInertialMovement: TAction
      Category = 'Config'
      Caption = 'Inertial Movement'
      OnExecute = actConfigUseInertialMovementExecute
    end
    object actConfigAzimuthCircle: TAction
      Category = 'Config'
      Caption = 'Azimuth Circle'
      OnExecute = actConfigAzimuthCircleExecute
    end
    object actConfigColorInversion: TAction
      Category = 'Config'
      Caption = 'Night Mode (Color Inversion)'
      ShortCut = 32846
      OnExecute = actConfigColorInversionExecute
    end
    object actConfigPreviousSelectionVisible: TAction
      Category = 'Config'
      Caption = 'Previous Selection'
      OnExecute = actConfigPreviousSelectionVisibleExecute
    end
    object actViewNavigation: TAction
      Category = 'View'
      Caption = 'Navigation Arrow'
      OnExecute = actViewNavigationExecute
    end
    object actShowDebugInfo: TAction
      Category = 'View'
      Caption = 'Debug Info'
      OnExecute = actShowDebugInfoExecute
    end
    object actConfigStatusBarVisible: TAction
      Category = 'Config'
      Caption = 'Status Bar'
      ShortCut = 32851
      OnExecute = actConfigStatusBarVisibleExecute
    end
    object actConfigMiniMapVisible: TAction
      Category = 'Config'
      Caption = 'Overview Map'
      ShortCut = 32845
      OnExecute = actConfigMiniMapVisibleExecute
    end
    object actConfigScaleLineVisible: TAction
      Category = 'Config'
      Caption = 'Scale Legend'
      ShortCut = 32844
      OnExecute = actConfigScaleLineVisibleExecute
    end
    object actConfigScaleLineExtended: TAction
      Category = 'Config'
      Caption = 'Show Vertical Scale Legend'
      OnExecute = actConfigScaleLineExtendedExecute
    end
    object actViewToolbarsLock: TAction
      Category = 'View'
      Caption = 'Lock Toolbars'
      OnExecute = actViewToolbarsLockExecute
    end
    object actViewGridGenShtabNo: TAction
      Category = 'View\GridGenShtab'
      Caption = 'No'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_1_000_000: TAction
      Tag = 1000000
      Category = 'View\GridGenShtab'
      Caption = '1:1 000 000 (10 km)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_500_000: TAction
      Tag = 500000
      Category = 'View\GridGenShtab'
      Caption = '1:500 000 (5 km)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_200_000: TAction
      Tag = 200000
      Category = 'View\GridGenShtab'
      Caption = '1:200 000 (2 km)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_100_000: TAction
      Tag = 100000
      Category = 'View\GridGenShtab'
      Caption = '1:100 000 (1 km)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_50_000: TAction
      Tag = 50000
      Category = 'View\GridGenShtab'
      Caption = '1:50 000 (500 m)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_25_000: TAction
      Tag = 25000
      Category = 'View\GridGenShtab'
      Caption = '1:25 000 (250 m)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_10_000: TAction
      Tag = 10000
      Category = 'View\GridGenShtab'
      Caption = '1:10 000 (100 m)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_5_000: TAction
      Tag = 5000
      Category = 'View\GridGenShtab'
      Caption = '1:5 000 (50 m)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtab_2_500: TAction
      Tag = 2500
      Category = 'View\GridGenShtab'
      Caption = '1:2 500 (25 m)'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridGenShtabAuto: TAction
      Tag = -1
      Category = 'View\GridGenShtab'
      Caption = 'Auto'
      GroupIndex = 1
      OnExecute = actViewGridGenShtabExecute
    end
    object actViewGridLonLatNo: TAction
      Category = 'View\GridLonLat'
      Caption = 'No'
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLat_10_000: TAction
      Tag = 1000000000
      Category = 'View\GridLonLat'
      Caption = '10'#176
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLat_05_000: TAction
      Tag = 500000000
      Category = 'View\GridLonLat'
      Caption = '5'#176
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLat_02_000: TAction
      Tag = 200000000
      Category = 'View\GridLonLat'
      Caption = '2'#176
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLat_01_000: TAction
      Tag = 100000000
      Category = 'View\GridLonLat'
      Caption = '1'#176
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLat_00_500: TAction
      Tag = 50000000
      Category = 'View\GridLonLat'
      Caption = '0.5'#176
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLat_00_250: TAction
      Tag = 25000000
      Category = 'View\GridLonLat'
      Caption = '0.25'#176
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLat_00_125: TAction
      Tag = 12500000
      Category = 'View\GridLonLat'
      Caption = '0.125'#176
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLat_User: TAction
      Tag = 1000000
      Category = 'View\GridLonLat'
      Caption = 'User defined'
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actViewGridLonLatAuto: TAction
      Tag = -1
      Category = 'View\GridLonLat'
      Caption = 'Auto'
      GroupIndex = 2
      OnExecute = actViewGridLonLatExecute
    end
    object actHelpOpenOnline: TBrowseURL
      Category = 'Help'
      Caption = 'Open User Manual'
      ImageIndex = 26
      ShortCut = 112
      URL = 'http://sasgis.org/wikisasiya/'
    end
    object actHelpShowAbout: TAction
      Category = 'Help'
      Caption = 'About'
      ImageIndex = 27
      OnExecute = actHelpShowAboutExecute
    end
    object actHelpOpenWebSite: TBrowseURL
      Category = 'Help'
      Caption = 'Open Home Page'
      URL = 'http://sasgis.org/'
    end
    object actHelpOpenIssueTracker: TBrowseURL
      Category = 'Help'
      Caption = 'Report a Bug'
      URL = 'http://sasgis.org/mantis/'
    end
    object actHelpOpenCommunity: TBrowseURL
      Category = 'Help'
      Caption = 'Open Forum Page'
      URL = 'http://sasgis.org/forum/'
    end
    object actShowPascalScriptIde: TAction
      Category = 'Help'
      Caption = 'Pascal Script IDE'
      OnExecute = actShowPascalScriptIdeExecute
    end
    object actShowUpddateChecker: TAction
      Category = 'Help'
      Caption = 'Check for Updates'
      OnExecute = actShowUpddateCheckerExecute
    end
    object actViewFillingMapMarkUnexisting: TAction
      Category = 'View\FillingMap'
      Caption = 'Mark Nonexistent Tiles'
      GroupIndex = 4
      OnExecute = actViewFillingMapMarkUnexistingExecute
    end
    object actViewFillingMapMarkExisting: TAction
      Category = 'View\FillingMap'
      Caption = 'Mark Existing Tiles'
      GroupIndex = 4
      OnExecute = actViewFillingMapMarkExistingExecute
    end
    object actViewFillingMapMarkGradient: TAction
      Category = 'View\FillingMap'
      Caption = 'Use Age Gradient'
      GroupIndex = 4
      OnExecute = actViewFillingMapMarkGradientExecute
    end
    object actViewFillingMapFilterMode: TAction
      Category = 'View\FillingMap'
      Caption = 'Within Time Interval'
      OnExecute = actViewFillingMapFilterModeExecute
    end
    object actViewSelectNextMapWithTile: TAction
      Category = 'View'
      Caption = 'Next map with tile'
      OnExecute = actViewSelectNextMapWithTileExecute
    end
    object actViewSelectPrevMapWithTile: TAction
      Category = 'View'
      Caption = 'Previous map with tile'
      OnExecute = actViewSelectPrevMapWithTileExecute
    end
    object actViewSelectNextMapVersion: TAction
      Category = 'View'
      Caption = 'Next version'
      OnExecute = actViewSelectNextMapVersionExecute
    end
    object actViewSelectPrevMapVersion: TAction
      Category = 'View'
      Caption = 'Previous version'
      OnExecute = actViewSelectPrevMapVersionExecute
    end
    object actConfigScaleLineOptionsShow: TAction
      Category = 'Config'
      Caption = 'Options'
      ImageIndex = 20
      OnExecute = actConfigScaleLineOptionsShowExecute
    end
    object actConfigScaleLineNumberFormatNice: TAction
      Category = 'Config\ScaleLineNumberFormat'
      Caption = 'Nice'
      GroupIndex = 5
      OnExecute = actConfigScaleLineNumberFormatExecute
    end
    object actConfigScaleLineNumberFormatRound: TAction
      Tag = 1
      Category = 'Config\ScaleLineNumberFormat'
      Caption = 'Round'
      GroupIndex = 5
      OnExecute = actConfigScaleLineNumberFormatExecute
    end
    object actConfigScaleLineNumberFormatScience: TAction
      Tag = 2
      Category = 'Config\ScaleLineNumberFormat'
      Caption = 'Science'
      GroupIndex = 5
      OnExecute = actConfigScaleLineNumberFormatExecute
    end
    object actConfigMarksNamesVisible: TAction
      Category = 'Config'
      Caption = 'Placemark Names'
      OnExecute = actConfigMarksNamesVisibleExecute
    end
    object actShowPointProject: TAction
      Category = 'Marks'
      Caption = 'Project New Placemark'
      OnExecute = actShowPointProjectExecute
    end
    object actMarksAddPoint: TAction
      Category = 'Marks'
      Caption = 'Add Placemark'
      Hint = 'Add new placemark'
      ImageIndex = 15
      OnExecute = actMarksAddPointExecute
    end
    object actMarksAddLine: TAction
      Category = 'Marks'
      Caption = 'Add Path'
      Hint = 'Add new path'
      ImageIndex = 16
      OnExecute = actMarksAddLineExecute
    end
    object actMarksAddPolygon: TAction
      Category = 'Marks'
      Caption = 'Add Polygon'
      Hint = 'Add polygon'
      ImageIndex = 17
      OnExecute = actMarksAddPolygonExecute
    end
    object actShowPlacemarkManager: TAction
      Category = 'Marks'
      Caption = 'Placemark Manager'
      Hint = 'Placemark manager'
      ImageIndex = 18
      OnExecute = actShowPlacemarkManagerExecute
    end
    object actConfigMarksHide: TAction
      Category = 'Marks'
      Caption = 'Hide All Placemarks'
      Hint = 'Hide all placemarks'
      ImageIndex = 19
      OnExecute = actConfigMarksHideExecute
    end
    object actConfigDownloadModeCache: TAction
      Tag = 1
      Category = 'Download Mode'
      Caption = 'Cache'
      ImageIndex = 1
      ShortCut = 32835
      OnExecute = actConfigDownloadModeExecute
    end
    object actConfigDownloadModeInternet: TAction
      Category = 'Download Mode'
      Caption = 'Internet'
      ImageIndex = 0
      ShortCut = 32841
      OnExecute = actConfigDownloadModeExecute
    end
    object actConfigDownloadModeCacheInternet: TAction
      Tag = 2
      Category = 'Download Mode'
      Caption = 'Internet && Cache'
      ImageIndex = 2
      ShortCut = 32834
      OnExecute = actConfigDownloadModeExecute
    end
    object actMapsEditMapParams: TAction
      Category = 'Maps'
      Caption = 'Map Settings'
      ShortCut = 49232
      OnExecute = actMapsEditMapParamsExecute
    end
    object actConfigOptionsShow: TAction
      Category = 'Config'
      Caption = 'Options'
      ImageIndex = 20
      OnExecute = actConfigOptionsShowExecute
    end
    object actGpsConnect: TAction
      Category = 'GPS'
      Caption = 'Connect to GPS Receiver'
      Hint = 'Connect to GPS receiver'
      ImageIndex = 14
      ShortCut = 49223
      OnExecute = actGpsConnectExecute
    end
    object actConfigGpsShowTrack: TAction
      Category = 'GPS'
      Caption = 'Show GPS Track'
      Hint = 'Show GPS track'
      ImageIndex = 6
      ShortCut = 49236
      OnExecute = actConfigGpsShowTrackExecute
    end
    object actConfigGpsFollowPosition: TAction
      Category = 'GPS'
      Caption = 'Follow GPS Position'
      Hint = 'Follow GPS Position'
      ImageIndex = 5
      OnExecute = actConfigGpsFollowPositionExecute
    end
    object actConfigGpsFollowPositionAtCenter: TAction
      Category = 'GPS'
      Caption = 'Centered GPS Position'
      OnExecute = actConfigGpsFollowPositionAtCenterExecute
    end
    object actGpsMarkPointAdd: TAction
      Category = 'GPS'
      Caption = 'Add Placemark'
      ImageIndex = 15
      ShortCut = 49235
      OnExecute = actGpsMarkPointAddExecute
    end
    object actGpsTrackSaveToDb: TAction
      Category = 'GPS'
      Caption = 'Add Track to Database'
      ImageIndex = 25
      OnExecute = actGpsTrackSaveToDbExecute
    end
    object actGpsTrackClear: TAction
      Category = 'GPS'
      Caption = 'Delete Track'
      ImageIndex = 35
      OnExecute = actGpsTrackClearExecute
    end
    object actConfigGpsOptionsShow: TAction
      Category = 'GPS'
      Caption = 'Options'
      ImageIndex = 20
      OnExecute = actConfigGpsOptionsShowExecute
    end
    object actConfigProjectionOfMapUse: TAction
      Tag = -1
      Category = 'View'
      Caption = 'Map Original Projection (from zmp)'
      OnExecute = actConfigProjectionUseExecute
    end
    object actViewFillingMapMainMapUse: TAction
      Category = 'View\FillingMap'
      Caption = 'Displayed Main Map'
      OnExecute = actViewFillingMapMapUseExecute
    end
    object actConfigInterfaceOptionsShow: TAction
      Category = 'Config'
      Caption = 'Interface Options'
      OnExecute = actConfigInterfaceOptionsShowExecute
    end
    object actMapsAllLayersHide: TAction
      Category = 'Maps'
      Caption = 'Hide All'
      OnExecute = actMapsAllLayersHideExecute
    end
    object actFavoriteAdd: TAction
      Category = 'Favorite'
      Caption = 'Add'
      OnExecute = actFavoriteAddExecute
    end
    object actFavoriteManage: TAction
      Category = 'Favorite'
      Caption = 'Manage'
      OnExecute = actFavoriteManageExecute
    end
    object actViewNotMinimized: TAction
      Category = 'View'
      Caption = 'Restore'
      OnExecute = actViewNotMinimizedExecute
    end
    object actIconsSettings: TAction
      Category = 'Marks'
      Caption = 'Icons Settings'
      OnExecute = actIconsSettingsExecute
    end
    object actViewSunCalc: TAction
      Category = 'View'
      Caption = 'Sun Calculator'
      ImageIndex = 66
      OnExecute = actViewSunCalcExecute
    end
    object actViewMoonCalc: TAction
      Category = 'View'
      Caption = 'Moon Calculator'
      ImageIndex = 75
      OnExecute = actViewMoonCalcExecute
    end
  end
  object tbxpmnScaleLine: TTBXPopupMenu
    Left = 160
    Top = 312
    object tbiConfigScaleLineExtended: TTBXItem
      Action = actConfigScaleLineExtended
    end
    object tbxsbmScaleLineNumberFormat: TTBXSubmenuItem
      Caption = 'Numbers Format'
      object tbiConfigScaleLineNumberFormatNice: TTBXItem
        Action = actConfigScaleLineNumberFormatNice
      end
      object tbiConfigScaleLineNumberFormatRound: TTBXItem
        Action = actConfigScaleLineNumberFormatRound
      end
      object tbiConfigScaleLineNumberFormatScience: TTBXItem
        Action = actConfigScaleLineNumberFormatScience
      end
    end
    object tbiConfigScaleLineVisible: TTBXItem
      Action = actConfigScaleLineVisible
    end
    object tbiConfigScaleLineOptionsShow: TTBXItem
      Action = actConfigScaleLineOptionsShow
    end
  end
end
