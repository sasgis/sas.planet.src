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
    TabOrder = 0
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
      DockPos = -6
      DockRow = 1
      Images = PanelsImageList
      Stretch = True
      TabOrder = 0
      Caption = 'Main'
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
        Action = actShowGoTo
        DropdownCombo = True
        Images = PanelsImageList
        Options = [tboShowHint]
      end
      object TBXSeparatorItem5: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object TBFullSize: TTBXItem
        Action = actViewFullScreen
        Images = PanelsImageList
      end
    end
    object SrcToolbar: TTBXToolbar
      Left = 230
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
        object btnHideAll: TTBXItem
          OnClick = btnHideAllClick
          Caption = 'Hide All'
          Hint = ''
        end
        object HideSeparator: TTBSeparatorItem
          Caption = ''
          Hint = ''
        end
      end
    end
    object TBMarksToolbar: TTBXToolbar
      Left = 406
      Top = 25
      DockPos = 363
      DockRow = 1
      Images = PanelsImageList
      LinkSubitems = NMarksGroup
      Stretch = True
      TabOrder = 2
      Caption = 'Placemarks'
    end
    object GPSToolbar: TTBXToolbar
      Left = 577
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
        object tbitmGPSTrackSaveToMarks: TTBXItem
          ImageIndex = 25
          Images = MenusImageList
          OnClick = tbitmGPSTrackSaveToMarksClick
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
      DockPos = -6
      MenuBar = True
      ProcessShortCuts = True
      ShrinkMode = tbsmWrap
      Stretch = True
      TabOrder = 5
      Caption = 'Main Menu'
      object NOperations: TTBXSubmenuItem
        Caption = '&Operations'
        Hint = ''
        object tbitmCreateShortcut: TTBXItem
          Action = actMakeLinkOnDesktop
          Images = MenusImageList
        end
        object tbitmOpenFile: TTBXItem
          Action = actFileOpen
          Images = MenusImageList
        end
        object TBXSeparatorItem6: TTBXSeparatorItem
          Caption = ''
          Hint = ''
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
          Caption = ''
          Hint = ''
        end
        object tbitmGoToModal: TTBXItem
          Action = actShowGoTo
          Images = MenusImageList
        end
        object NCalcRast: TTBXItem
          Action = actDistanceCalculation
          Images = MenusImageList
        end
        object TBXSeparatorItem8: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object NRectSave: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Selection Manager'
          Hint = ''
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
            Caption = ''
            Hint = ''
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
          Caption = ''
          Hint = ''
        end
        object tbitmCacheManager: TTBXItem
          Action = actShowCacheManager
          Images = MenusImageList
        end
        object TBXSeparatorCacheManager: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmQuit: TTBXItem
          Action = actQuit
          Images = MenusImageList
        end
      end
      object NView: TTBXSubmenuItem
        Caption = '&View'
        Hint = ''
        object NPanels: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Toolbars'
          Hint = ''
          object NMainToolBarShow: TTBXVisibilityToggleItem
            Control = TBMainToolBar
            Images = MenusImageList
            Caption = 'Main'
            Hint = ''
          end
          object NZoomToolBarShow: TTBXVisibilityToggleItem
            Control = ZoomToolBar
            Images = MenusImageList
            Caption = 'Zoom'
            Hint = ''
          end
          object NsrcToolBarShow: TTBXVisibilityToggleItem
            Control = SrcToolbar
            Images = MenusImageList
            Caption = 'Sources'
            Hint = ''
          end
          object NFavoriteToolbarShow: TTBXVisibilityToggleItem
            Control = TBXFavoriteToolbar
            Images = MenusImageList
            Caption = 'Favorites'
            Hint = ''
          end
          object NGPSToolBarShow: TTBXVisibilityToggleItem
            Control = GPSToolbar
            Images = MenusImageList
            Caption = 'GPS'
            Hint = ''
          end
          object TBXVisibilityToggleItem1: TTBXVisibilityToggleItem
            Control = TBMarksToolbar
            Images = MenusImageList
            Caption = 'Placemarks'
            Hint = ''
          end
          object TBXVisibilityToggleItem2: TTBXVisibilityToggleItem
            Control = TBXToolBarSearch
            Images = MenusImageList
            Caption = 'Search'
            Hint = ''
          end
          object NSearchResults: TTBXVisibilityToggleItem
            Control = TBSearchWindow
            Images = MenusImageList
            Caption = 'Search Results'
            Hint = ''
          end
          object tbxMergePolygonsShow: TTBXVisibilityToggleItem
            Control = tbMergePolygons
            Images = MenusImageList
            Caption = 'Merge Polygons'
            Hint = ''
          end
          object TBXVisibilityToggleItem3: TTBXVisibilityToggleItem
            Control = FillDates
            Images = MenusImageList
            Caption = 'Time Interval'
            Hint = ''
          end
          object NSensors: TTBXSubmenuItem
            AutoCheck = True
            DropdownCombo = True
            Images = MenusImageList
            OnClick = NSensorsClick
            Caption = 'Sensors'
            Hint = ''
          end
          object TBXSeparatorItem18: TTBXSeparatorItem
            Caption = ''
            Hint = ''
          end
          object NBlock_toolbars: TTBXItem
            Action = actViewToolbarsLock
            Images = MenusImageList
          end
        end
        object tbsbmInterface: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Interface'
          Hint = ''
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
          ImageIndex = 7
          Images = MenusImageList
          OnClick = NFillMapClick
          Caption = 'Cached Tiles Map'
          Hint = ''
          object TBFillingTypeMap: TTBXSubmenuItem
            Images = MenusImageList
            Options = [tboDropdownArrow]
            Caption = 'Show for...'
            Hint = ''
            object tbitmFillingMapAsMain: TTBXItem
              OnAdjustFont = AdjustFont
              OnClick = TBfillMapAsMainClick
              Caption = 'Displayed Main Map'
              Hint = ''
            end
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
          object TBXSeparatorItem20: TTBXSeparatorItem
            Caption = ''
            Hint = ''
          end
          object NFillMode1: TTBXItem
            Tag = 99
            Checked = True
            GroupIndex = 2
            Images = MenusImageList
            RadioItem = True
            OnClick = NFillMode1Click
            Caption = 'Mark Nonexistent Tiles'
            Hint = ''
          end
          object NFillMode2: TTBXItem
            Tag = 99
            GroupIndex = 2
            Images = MenusImageList
            RadioItem = True
            OnClick = NFillMode2Click
            Caption = 'Mark Existing Tiles'
            Hint = ''
          end
          object NFillMode3: TTBXItem
            Tag = 99
            GroupIndex = 2
            Images = MenusImageList
            RadioItem = True
            OnClick = NFillMode3Click
            Caption = 'Use Age Gradient'
            Hint = ''
          end
          object TBXSeparatorItem21: TTBXSeparatorItem
            Caption = ''
            Hint = ''
          end
          object NShowFillDates: TTBXItem
            Tag = 99
            GroupIndex = 1
            Images = MenusImageList
            OnClick = NShowFillDatesClick
            Caption = 'Within Time Interval'
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
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = 'No'
            Hint = ''
          end
          object N001: TTBXItem
            Tag = 100
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = 'Current Zoom'
            Hint = ''
          end
          object N002: TTBXItem
            Tag = 2
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = '2'
            Hint = ''
          end
          object N003: TTBXItem
            Tag = 3
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = '3'
            Hint = ''
          end
          object N004: TTBXItem
            Tag = 4
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = '4'
            Hint = ''
          end
          object N005: TTBXItem
            Tag = 5
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = '5'
            Hint = ''
          end
          object N006: TTBXItem
            Tag = 6
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = '6'
            Hint = ''
          end
          object N007: TTBXItem
            Tag = 7
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = '7'
            Hint = ''
          end
          object tbitmTileGrid1p: TTBXItem
            Tag = 101
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = 'Current Zoom + 1'
            Hint = ''
          end
          object tbitmTileGrid2p: TTBXItem
            Tag = 102
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = 'Current Zoom + 2'
            Hint = ''
          end
          object tbitmTileGrid3p: TTBXItem
            Tag = 103
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = 'Current Zoom + 3'
            Hint = ''
          end
          object tbitmTileGrid4p: TTBXItem
            Tag = 104
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = 'Current Zoom + 4'
            Hint = ''
          end
          object tbitmTileGrid5p: TTBXItem
            Tag = 105
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = N000Click
            Caption = 'Current Zoom + 5'
            Hint = ''
          end
        end
        object tbsbmGenShtabScale: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'GenShtab Maps Boundaries'
          Hint = ''
          object NGShScale0: TTBXItem
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = 'No'
            Hint = ''
          end
          object NGShScale1000000: TTBXItem
            Tag = 1000000
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:1 000 000 (10 km)'
            Hint = ''
          end
          object NGShScale500000: TTBXItem
            Tag = 500000
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:500 000 (5 km)'
            Hint = ''
          end
          object NGShScale200000: TTBXItem
            Tag = 200000
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:200 000 (2 km)'
            Hint = ''
          end
          object NGShScale100000: TTBXItem
            Tag = 100000
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:100 000 (1 km)'
            Hint = ''
          end
          object NGShScale50000: TTBXItem
            Tag = 50000
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:50 000 (500 m)'
            Hint = ''
          end
          object NGShScale25000: TTBXItem
            Tag = 25000
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:25 000 (250 m)'
            Hint = ''
          end
          object NGShScale10000: TTBXItem
            Tag = 10000
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:10 000 (100 m)'
            Hint = ''
          end
          object NGShScale5000: TTBXItem
            Tag = 5000
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:5 000 (50 m)'
            Hint = ''
          end
          object NGShScale2500: TTBXItem
            Tag = 2500
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:2 500 (25 m)'
            Hint = ''
          end
          object TBSeparatorItem3: TTBSeparatorItem
            Caption = ''
            Hint = ''
          end
          object NGShauto: TTBXItem
            Tag = -1
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = 'Auto'
            Hint = ''
          end
        end
        object DegreedLinesSubMenu: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Lat/Lon Grid'
          Hint = ''
          object NDegScale0: TTBXItem
            AutoCheck = True
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = 'No'
            Hint = ''
          end
          object NDegScale1000000: TTBXItem
            Tag = 1000000000
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = '10'#176
            Hint = ''
          end
          object NDegScale500000: TTBXItem
            Tag = 500000000
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = '5'#176
            Hint = ''
          end
          object NDegScale200000: TTBXItem
            Tag = 200000000
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = '2'#176
            Hint = ''
          end
          object NDegScale100000: TTBXItem
            Tag = 100000000
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = '1'#176
            Hint = ''
          end
          object NDegScale50000: TTBXItem
            Tag = 50000000
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = '0.5'#176
            Hint = ''
          end
          object NDegScale25000: TTBXItem
            Tag = 25000000
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = '0.25'#176
            Hint = ''
          end
          object NDegScale10000: TTBXItem
            Tag = 12500000
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = '0.125'#176
            Hint = ''
          end
          object TBXSeparatorItem22: TTBXSeparatorItem
            Caption = ''
            Hint = ''
          end
          object NDegScaleUser: TTBXItem
            Tag = 1
            GroupIndex = 1
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = 'User defined'
            Hint = ''
          end
          object NDegValue: TTBXEditItem
            OnAcceptText = NDegValueAcceptText
            Caption = ''
            Hint = ''
          end
          object TBSeparatorItem2: TTBSeparatorItem
            Caption = ''
            Hint = ''
          end
          object NDegScaleAuto: TTBXItem
            Tag = -100000000
            GroupIndex = 1
            Images = MenusImageList
            RadioItem = True
            OnClick = NDegScale0Click
            Caption = 'Auto'
            Hint = ''
          end
        end
        object tbxsbmProjection: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Projection'
          Hint = ''
        end
        object TBXSubmenuMap: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Maps'
          Hint = ''
          object tbxnxtmap: TTBXItem
            OnClick = tbxnxtmapClick
            Caption = 'Next map with tile'
            Hint = ''
          end
          object tbxprevmap: TTBXItem
            OnClick = tbxprevmapClick
            Caption = 'Previous map with tile'
            Hint = ''
          end
        end
        object TBXSubmnMapVer: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Versions'
          Hint = ''
          object TBXNextVer: TTBXItem
            OnClick = TBXNextVerClick
            Caption = 'Next version'
            Hint = ''
          end
          object TBXPrevVer: TTBXItem
            OnClick = TBXPrevVerClick
            Caption = 'Previous version'
            Hint = ''
          end
        end
        object TBXSeparatorItem10: TTBXSeparatorItem
          Caption = ''
          Hint = ''
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
        object tbitmShowDebugInfo: TTBXItem
          Action = actShowDebugInfo
          Images = MenusImageList
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
      object tbxFavorite: TTBXSubmenuItem
        LinkSubitems = TBFavorite
        Caption = 'Favorites'
        Hint = ''
      end
      object NMarks: TTBXSubmenuItem
        Caption = 'Placemarks'
        Hint = ''
        object NMarksGroup: TTBGroupItem
          Caption = ''
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
          object tbitmPlacemarkManager: TTBXItem
            ImageIndex = 18
            Images = PanelsImageList
            Options = [tboShowHint]
            OnClick = tbitmPlacemarkManagerClick
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
        object tbitmShowMarkCaption: TTBXItem
          AutoCheck = True
          Checked = True
          Images = PanelsImageList
          OnClick = tbitmShowMarkCaptionClick
          Caption = 'Placemark Names'
          Hint = ''
        end
        object tbitmPointProject: TTBXItem
          Images = PanelsImageList
          OnClick = tbitmPointProjectClick
          Caption = 'Project New Placemark'
          Hint = ''
        end
        object tbxMergePolygonsShow1: TTBXVisibilityToggleItem
          Control = tbMergePolygons
          ImageIndex = 23
          Images = PanelsImageList
          Caption = 'Merge Polygons'
          Hint = ''
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
          Images = MenusImageList
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
          OnClick = tbitmGPSTrackSaveToMarksClick
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
        OnPopup = NParamsPopup
        Caption = 'Settings'
        Hint = ''
        object NMapParams: TTBXItem
          Images = MenusImageList
          ShortCut = 49232
          OnClick = NMapParamsClick
          Caption = 'Map Settings'
          Hint = ''
        end
        object NLayerParams: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Layer Settings'
          Hint = ''
        end
        object TBXSeparatorItem14: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmOptions: TTBXItem
          ImageIndex = 20
          Images = MenusImageList
          OnClick = tbitmOptionsClick
          Caption = 'Options'
          Hint = ''
        end
        object tbitmInterfaceOptions: TTBXItem
          Images = MenusImageList
          OnClick = tbitmOnInterfaceOptionsClick
          Caption = 'Interface Options'
          Hint = ''
        end
        object TBLang: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Language'
          Hint = ''
        end
      end
      object tbsbmHelp: TTBXSubmenuItem
        Caption = '&Help'
        Hint = ''
        object tbitmOnlineHelp: TTBXItem
          ImageIndex = 26
          Images = MenusImageList
          ShortCut = 112
          OnClick = tbitmOnlineHelpClick
          Caption = 'Online Help (http://sasgis.org/wikisasiya)'
          Hint = ''
        end
        object tbitmAbout: TTBXItem
          ImageIndex = 27
          Images = MenusImageList
          OnClick = tbitmAboutClick
          Caption = 'About'
          Hint = ''
        end
        object tbsprtHelp01: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmOnlineHome: TTBXItem
          Images = MenusImageList
          OnClick = tbitmOnlineHomeClick
          Caption = 'Web Site (http://www.sasgis.org)'
          Hint = ''
        end
        object tbtmHelpBugTrack: TTBXItem
          Images = MenusImageList
          OnClick = tbtmHelpBugTrackClick
          Caption = 'Issue Tracker (http://sasgis.org/mantis)'
          Hint = ''
        end
        object tbitmOnlineForum: TTBXItem
          Images = MenusImageList
          OnClick = tbitmOnlineForumClick
          Caption = 'Community  (http://www.sasgis.org/forum)'
          Hint = ''
        end
        object tbxSep1: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbxtmPascalScriptIDE: TTBXItem
          Images = MenusImageList
          OnClick = tbxtmPascalScriptIDEClick
          Caption = 'PascalScript IDE'
          Hint = ''
        end
        object tbxSep2: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmCheckUpdate: TTBXItem
          Images = MenusImageList
          OnClick = tbitmCheckUpdateClick
          Caption = 'Check for updates...'
          Hint = ''
        end
      end
    end
    object TBXToolBarSearch: TTBXToolbar
      Left = 486
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
    object FillDates: TTBXToolbar
      Left = 629
      Top = 0
      DefaultDock = TBDock
      DockPos = 643
      TabOrder = 7
      Visible = False
      Caption = 'FillDates'
      object TBControlItem7: TTBControlItem
        Control = Label1
        Caption = ''
        Hint = ''
      end
      object TBControlItem6: TTBControlItem
        Control = DateTimePicker1
        Caption = ''
        Hint = ''
      end
      object TBControlItem8: TTBControlItem
        Control = Label2
        Caption = ''
        Hint = ''
      end
      object TBControlItem9: TTBControlItem
        Control = DateTimePicker2
        Caption = ''
        Hint = ''
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
        Date = 40830.496065717590000000
        Time = 40830.496065717590000000
        TabOrder = 0
        OnChange = DateTimePicker1Change
      end
      object DateTimePicker2: TDateTimePicker
        Left = 203
        Top = 21
        Width = 81
        Height = 21
        Date = 40830.496065717590000000
        Time = 40830.496065717590000000
        TabOrder = 1
        OnChange = DateTimePicker2Change
      end
    end
    object TBXFavoriteToolbar: TTBXToolbar
      Left = 357
      Top = 25
      DockPos = 224
      DockRow = 1
      Stretch = True
      TabOrder = 8
      Caption = 'Sources'
      object TBFavorite: TTBXSubmenuItem
        ImageIndex = 22
        Images = PanelsImageList
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = 'Favorite maps/layers'
        object tbxAddToFavorite: TTBXItem
          OnClick = tbxAddToFavoriteClick
          Caption = 'Add'
          Hint = ''
        end
        object tbxManageFavorite: TTBXItem
          OnClick = tbxManageFavoriteClick
          Caption = 'Manage'
          Hint = ''
        end
        object TBXSeparatorItem15: TTBXSeparatorItem
          Caption = ''
          Hint = ''
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
      DockPos = -6
      Stretch = True
      TabOrder = 0
      OnDockChanging = ZoomToolBarDockChanging
      Caption = 'Zoom'
      object TBZoomIn: TTBXItem
        Action = actZoomIn
        Images = MenusImageList
        MinHeight = 29
        MinWidth = 29
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
        Action = actZoomOut
        Images = MenusImageList
        MinHeight = 29
        MinWidth = 29
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
        Left = 24
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
      DockedWidth = 170
      DockPos = -6
      DockRow = 2
      TabOrder = 1
      Visible = False
      OnClose = TBSearchWindowClose
      Caption = 'Search Results'
      object PanelSearch: TPanel
        Left = 0
        Top = 0
        Width = 170
        Height = 549
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
          Height = 540
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
      Top = 244
      DockPos = 240
      TabOrder = 2
      OnClose = TBEditPathClose
      object TBEditPathDel: TTBXItem
        ImageIndex = 36
        Images = MenusImageList
        OnClick = TBEditPathDelClick
        Caption = ''
        Hint = 'Delete Point'
      end
      object TBEditPathSplit: TTBXItem
        ImageIndex = 63
        Images = MenusImageList
        OnClick = TBEditPathSplitClick
        Caption = ''
        Hint = 'Split Line'
      end
      object TBEditPathLabelVisible: TTBSubmenuItem
        AutoCheck = True
        Checked = True
        DropdownCombo = True
        ImageIndex = 37
        Images = MenusImageList
        OnClick = TBEditPathLabelClick
        Caption = ''
        Hint = 'Show/Hide Captions'
        object TBEditPathLabelLastOnly: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = TBEditPathLabelLastOnlyClick
          Caption = 'Show only final distance'
          Hint = 'Show only final distance'
        end
        object TBEditPathLabelShowAzimuth: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = TBEditPathLabelShowAzimuthClick
          Caption = 'Show azimuth'
          Hint = 'Show azimuth'
        end
      end
      object TBEditMagnetDraw: TTBXItem
        AutoCheck = True
        ImageIndex = 41
        Images = MenusImageList
        OnClick = TBEditMagnetDrawClick
        Caption = ''
        Hint = 'Snap to Existing Markers'
      end
      object tbitmFitEditToScreen: TTBXItem
        ImageIndex = 43
        Images = MenusImageList
        OnClick = tbitmFitEditToScreenClick
        Caption = ''
        Hint = 'Fit to screen'
      end
      object TBEditSelectPolylineRadiusCap1: TTBXLabelItem
        Margin = 2
        Caption = 'Radius'
        Hint = ''
      end
      object TBControlItem4: TTBControlItem
        Control = TBEditSelectPolylineRadius
        Caption = ''
        Hint = ''
      end
      object TBEditSelectPolylineRadiusCap2: TTBXLabelItem
        Margin = 2
        Caption = 'm'
        Hint = ''
      end
      object TBEditPathMarsh: TTBXSubmenuItem
        ImageIndex = 39
        Images = MenusImageList
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = 'Route Calculation'
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
      object tbitmSaveMark: TTBXSubmenuItem
        DropdownCombo = True
        ImageIndex = 25
        Images = MenusImageList
        OnClick = TBEditPathSaveClick
        Caption = ''
        Hint = 'Save'
        object tbitmSaveMarkAsNew: TTBXItem
          ImageIndex = 25
          OnClick = tbitmSaveMarkAsNewClick
          Caption = 'Save as...'
          Hint = 'Save as...'
        end
      end
      object tbxtmSaveMarkAsSeparateSegment: TTBXItem
        ImageIndex = 64
        Images = MenusImageList
        OnClick = tbitmSaveMarkLineAsSeparateSegmentsClick
        Caption = 'Save as separate placemarks...'
        Hint = 'Save as separate placemarks...'
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
      DockedWidth = 170
      DockPos = 6
      DockRow = 2
      FloatingWidth = 128
      FloatingHeight = 128
      TabOrder = 3
      Visible = False
      OnClose = tbMergePolygonsClose
      Caption = 'Merge Polygons'
      object mmoMergePolyHint: TMemo
        Left = 0
        Top = 0
        Width = 170
        Height = 549
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
      ClientAreaHeight = 561
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
      1800000089504E470D0A1A0A0000000D49484452000000180000024008060000
      009735A64B000059AB494441547801EC7D07745545D4EE3EE7F69BDB526E7AEF
      2194D021F42ABD0948538A5451401044B0A020A0A2888A8A8882A2A2A0200414
      114541E91D9426BD0502E9E5D6F3BE7D5248484200F9D75BEFFDDE35DF997266
      F69ED953CFB42BD05DFC9A4E5AA386B770220A00585DC1E3ECB6377BD8A1DF51
      09777A0BC2B1783F13E80B54A4BE81E30B6074027A854AACD0158E4D26AC7A0D
      DA71A06FA8D540633A57A379231AD3C4EE71D4A25610F998B47845CCF83822C2
      91607B3954C80001BE1614CA29213E3A9A31B0264DEE9B446D93FC492BE592A7
      878A1A2504D2F0CEB5E89541B528CCAA67A2CF731836DC8E720C9A4D5C3D079E
      FA36ADE647CFF46B4875A37DC9E4A1A78C02B8EAAD64F0845D2B9055E7242DD9
      69C1C806D4A6A62F5E525F30299792320CE021561284A9D101269A33AC11D50A
      D6D3F56BA95450904B1E2623193DB4242A14A4510A941062A6C649B1E4ED69A6
      273A4452988F2C324E09E71B3394A1949FB71E720C5E19DC804872D13FE7AF50
      9E60248BC98B5C4E78120482223F8B8EF69FBC02666914EA6B240FB582C67608
      A529CBE5BC661A8FC0B7AC4A5280D8ABE1D2373AC048215E6A3A7AEA22A59385
      7CFCAC70164992889C2E89EC0E37D9ED760A0FF627BBA4A48DFBAFD03F57B228
      C0A4A4604F15FCCAA2625A6CA6D2290827FC6A849968F9AFA7C8623693AF9F1E
      E271934214C8E17253769E9D0C081A1BCC4C09B13751EBA460DA7EE0045DBD91
      474126375D4C0711A270229293539A41001C2932D0426EF0BD5920922B2D8BF2
      3D3464D022A1480233080DD2B137A44882B80472BB1CE402F32B6939A413B824
      C8A9605AE518C801FFB99A473590B916E499975949272E6752AA42496AF0C829
      70908F0791D5A491894B6E376564E690BF5945572EE5D1FE7F32881485A99389
      E12100B2421EC4C2703C21D48BAC5E069AD02596AC160F3A7B3593361EBA41F9
      0E8944A4CDE97291BFCE41793627F54C8EA082FC1CCACDCEA203474FD0A22DE9
      64D778810CC515D7EED2223A4BF85DBA9E4D0156934C1C560AF737531F14CF5F
      0E5DA68367B2C8E596E8D8B97CCAC8B5515CA007059B24CACECAA06F7E3F4B2E
      651007619CA5A25F490AD88E547C0DBD6F42B80F3DDD398A5877BA88944A257D
      FBCB01FAF1480EA95107F26D0EBA869AD7B38E891A84AAE87A5A1ACD5F738232
      74E184DF37887D4931BD9D812C260F8D92FCBCB4F442CF68F2819844C949B3BE
      394417B25424A07EE4DB9D949665A3FEF5F4543748A05F769FA04D17BCC9A1D0
      82FE2DF1B0A50C0376402AB8A23C6FD4A96860230BB5AFE1459F6D3E493F1E77
      9356A3223B9264B73B282B3B9F1A8610754A20FA68878BD2DD660E3E0BB17F81
      0DC528C7805F80C9D7D0FBFA1995D435C14D4B7E4F239B682295E020B7D3496E
      7B0139F2F3C81B254A61F1A71C9706DEA98C68D88181C2C75A5920162CC359A9
      D94E5ABCDD4EC13A2DBDD13B9E9E6C1C441DFD25F2B79D46A3E749F9C6B062E2
      1C730E5396106C1532803B810927350ECDF6370A9D85E2E323282EAE210505D5
      245B819DEC6E39F1DFC02F1749F60B6379A528EF443462C488294949492D2CB9
      7F556B9790D4AF7DA318A55BA5509EBC984BE7AE5EA74675AB49493E1AE795DD
      CBF7D2A5AD01E1E1E1F5CF9C39B31FB4B84984764B559882FAF5EBBF3A7AF4E8
      57C78E1DFB6A5494CE37222C44E3A5F7A63A317EF468F786F4508BC682B717A9
      9E7B6EE2D059F8B56FDF7E0611B5A40A7EA52B5AC9EBF5EBD7D38E1D3B64BB5A
      6DA6BCBC5052A0B9901DF0100481B66DDB4E191967897FA74F9F66CDC08FBB82
      9797D73C93C9B4DA6834AE79FCF1A957A64F5F963D7DFA27D9CF3DB7247BEAD4
      C5D9CF3EBB28FBC9275FCAB6583C37A954AA95C09720DC0EB86BA5824F8E1123
      1AE63640BB0A50076E2600059654D0FF53F72E01C5ED41BEFBEE3B75DFBE7D63
      1F79E49166BD7AF5EAD9A74F9F9103070E7CEED1471F9DD5A953A72EB56BD7CE
      BB72E5CAA9B4B434D7ED612BB22B7EFDF5D71620F4387E63468D1AF5A2C16078
      DDC7C7679CD56A7DC4CFCFAF8DC562A9A5D7EB833CF0F3F6F60E837BAFEAD5AB
      F7A951A38664369B8F9D3871C25E11E16237C5F8F1E3B7D4AA55AB3388D50061
      5F3050A9D56A147541703A9D028F20D8331C640DEF4430B1FAFAFA76040657AB
      56CD10121272ECE8D1A3B9ECE176882EFC10398546A321AD564B6EF4B30C1027
      060760E2C5603BBB33A3888888A0060D1ACC68DCB8F131D4FA777AF7EECD455A
      6EA4D81F43BC7AF56A2A0C8284510313D6E974845410EBDC932930921345115E
      48766737F6C7FE9911DE0941414196E6CD9B3F09464720E62FBA77EF5E0F01E4
      400AC87F006A6204522114141410CC74EDDA35CACACA2287C3017F858A09DA6C
      36B9FBE494E6E4E4C8EF21007984C18C905FCAE8E8E8EAD01F474B10F0D75F7F
      6D1611E83262EB3C7EFCB83CD6C9C8C820E4874C88C55248BEF0C94C98F08D1B
      370804E414B11F0618C8612011E1E2C58B626E6E6E0B848A52B46CD9320D6DCE
      63C830058A9F1C2833339390E1727E14673213679170AA189C424E31524E376F
      DEA47FFEF98776EDDA45172E5CA0ECEC6C0152388FDF77CA71E3C6FDF1D24B2F
      4D1F3060C06BC1C1C122DA75427124C4849054E2CCBF7EFDBACC8C19701EB088
      F8FDA54B97885B527667704A1888B98494E643BFA9C4C3FDF2CB2FBF0342E128
      054F04040408E7CE9D23B4A83213CE703046D39C41A9A9A932C1B3673106C200
      8C89B2688A8812C40D72850AA9F807A66C66009DECAFBDF6DA5494985094802E
      CC04C92354246279B3874D9B36C9C5B698289724766711B21BC2CAFE217B42EC
      2588EF06DEBB8B1910E49EF3C5175F0C474A36A087AA035108680E08F9230FD7
      4BC7944B0EC79689720AD98C18CB2900618CC80B2444EC0A1894EB42A95DBB76
      31EFBFFFFE99949414F7B66DDBA4AFBFFE5AFAE38F3FA437DF7C537AE59557A4
      D9B3674BAFBEFAAA3473E64C69D2A449D2934F3E290D1F3E5C425BE546917737
      6FDEFC0C6AF66A106F0F94A974B017AAAE5DBB36FEE8A38FD2502ADCBFFDF69B
      B461C30669C18205D23BEFBC233399366D9A3471E24409B5571A32640813CEA9
      57AFDE5E148E77416130100FA8814A95D0AD5BB7873FFDF4D35C66F0D34F3F49
      6FBCF1863463C60C69EAD4A9D2840913DCC3860D7375EEDCF99FB8B8B81454CE
      6741A9156005CAB5D070AB50291E7EF8E1091F7FFCB183C5C4C4116B269CDDB4
      69D33D68F01620D420200ED001F7A5545DBA7479FEA9A79E3A893EE22F34D1DF
      23B69341A92511F900771D5BF8AD5469F0A616D01A8806B4C07FEAC149A05C26
      A1D3D72243EBA0D3EF898A3306E689A844D3070F1E3C139DFE4094770D1AB983
      A8E5CEBB8946459DFE6B68AA47A1187642BF9084F6281CADA717FA0C2D1A402B
      2A533B74FABD51A2FEEBF4757217C82D26B7A42C6F6E39D1CACA9D4F710F8777
      FF75FA8523C3629138309461FCD7E90BFF75FAA837ED010128AFFEBFEEF4258C
      262474FA7BD1DA3ED84E1F832CC7DCB973244C9248688FA640B056A05C7F02B7
      324A2C632B6F716084F78646536B49B76E439561614DA857AF89D4B3E7B046F0
      9A0FB8803BAAAA1870605B4040D4CDD2F345F9F90E9E78BDABD14585492C9E2F
      AA59B366B3C71F9FBBAA4E9DC81E56AB41999929D0E5CBA9D4AC59DDC0F6ED1F
      795AA17044868606550FFFB7F3452121415A9B4D87E1B99ADAB74F22CC278916
      8BA4FE6FBE88E78A183C5C6C83DC6E5701FE1F9A2F7A75F66CE5175F7ED560C3
      0F1B872CFDECF3066FCC7BD3E7D967A72A90AA3BAA2A3D4C7FFE7961C890A111
      8989D59F890B0999ED9B9DD5DF3320B0B73524B4ABAF9F7FBD0E1D3B793FD4A1
      83909C9C6C432DB76FDFBE5D2ACDF18E0CDE7FFF034FCCC40CAA592BE9ADA0E0
      E09E625696D97DE890489E5E7AAD9F5F80D160AC6DF1F2EC62365B7A7B797B77
      F6B5FA253DD4A993679B366D25302CA8DFA0BEA3C29AFCC6BC799A155F7FDDAE
      7172F29775EBD65B80E1620D224121F9F892BBFD4324868490E4746179C585F6
      5E50AA351A6B80DDDEB04666C698B890B04FAAD7ACF5636C7CC27CA3D1540D1E
      6E25085F9162704848426464E4D8B0A0E03E7AA7D35B341A053BE64CB3730BB0
      EAE1C0272DD66CD26F92D7EF3F530666E1D3929B921D1FE502266DD437D32447
      74ACDD46EE0D69D752675EBC70FE90B298FC7BEF2DF40F090D793422226224A6
      1022C573E745C76F5B282F38941C8D9B52565E01E5E4D9C004F3D699799499E7
      A01C1229279F178A24923CCC9253A54BCF3A77F1ADBCDC8C0F5E7E71FA4DA6AD
      983973A611D3699D1312AABDCD0C743ABD8F2088A253A9A24C958672BDAD944B
      02E5632AC78D4912677E01158822650684528E7F2015081ACA768974AE4043C7
      9561F957C5D08CEB62AC477CC301393E1E8D6EF26C8B19328ED768B401589FD1
      A3AD373ADDA4C8C12A87CD68E659282CD0B9C8852547FFF5AB49B16B07A547C6
      90134C780096E756D1D16C159DD1450A827FA45EE7175043A3D74638D2D2B7E6
      5C483D579207C39F58A00F890AEB5DAD66D01C72E505AA44894C4623E9B41A79
      8AC09E9B4FAEFD7B291F72CFAB9644F9583BBB743D838EDA3D29C3B706A9CC3E
      84993E977833FD37FB8953930B4E9DD9FFF3F2FE92CCE089A9ABFD126B058FF2
      F5538F169592BF4EEF21280437156467526E563A89A2A20858E8129490BB8D8E
      61F9EB648181B2551692ACB1E422758178FDC60A3A75F2A55FE6B73ACFF26728
      9F9CBE3EB9561DFFF1FE81FA6E6A4457A1D208F9793964F0F020C1612395D287
      D2726C942369E8E2997F48A5F5A04B3772E8629E404AAC73669C3F2BD9537333
      1439AEB73D0AA405DBDE6F9BC9848BA1A859BF6F0E91EA2F49D09E55EB8C4ABD
      C96CD479786833D2AE1256FA0451E341BC4E938F15A69B36A26B6E3D32DF936E
      5CBF4A6957AF925D6171E5A6657D6B4B4DFFF8C8D1DD37054B43A774FDCF62FA
      A4FCF8AD1EE9B0ED650C19BFFAC3E0086B82D9DBD8D6C74FF3A8D15317E71695
      425A761EDD2820CA16D474E9EADF94E1409DF031514EEE0514DD2D8A7C4BC7DE
      F946EFA61AFFC6C714B905BB851A9FED72A6671ECEBF70F152493D00035ABAA0
      6776DCF00D47036252638DA6BFC42B57B791F68A3F45F834238DD99F6EB8B594
      E9E94905128AE9B5DD58BF5C4681413D841CBB458FA9C74851A58E501B3D3A88
      79862B52C6E55715997B97315D191EEDBE14AA8DDD98F0F027479674FCF9D52C
      FFAF13DDAA77CD52ADC5DDA4A92923A4615F7595DA2E1C2DD59AFFB614346D94
      A479DC2A894F1AA480577B4BE6691B24E5D3EB25FDC41FDDC631AB2FEA5B3D3B
      45E313E387AF53140990F7E9FB9D2138D6FA7054A2DF64ABD59420D023A213A5
      E38CF7CFD4C06B38A9353632282F518C693765FFBD90326F5C23A7C6490A931A
      85C14279E94E926C6EB733EBC6DFCE63BFBF261C5FB7D6969D2A67B6C2A7FF9A
      DAA149FE2F04C5FB4CD1685441925B1245CCB459D50114EBAD412972527AB685
      72D2E3C95B1D497EBA33446216A5BB502FCC447A550DCABE12EE745CB9BC4D38
      FCC374F1E4DA1F6DB9375170107328C1ABEBA268EF105D0B73807F6754E97A46
      93C15FAB562AB5AA2C2157F7195DB15DA6F4EBE914A3E9447AA92EB508FE9B6E
      667E4E3FA3665FB23791B2CFB52AB87AF8F25AE1D80FAF2952771EB11560D119
      848B95506410F5315D8D9698DAD19E6131CDCD0101ED4D9EA624BD07F99875A2
      22539122EC4E5F41D1CA31D4C23B9E5A44ACA69F2E28A555BF24679EDDFBD752
      3A96F2AE22F3D8594C08BA8BE855AE690C568531B18F5748979793AB8D5C3AAD
      C98B29BF76F9E893EB8DBF1AE56AB4E073A9F68C9FA4B9EB3E770F7A7FC60553
      B309CFA8BDA2E5CCAC9CE21DDE68F466A5B17A7F6B64F7575BD61AF3C9CC8091
      2BFE340C5D75C367D0A7873C1B0D1EA031781BEF10FCDE5EA94D412A55C2C000
      55F2F36D9589831A6A74A6BB1A9BDE1B9742DF9C6F8C425B154F615DCA9A4789
      84F7D0B69BAAF07B4FAF31799885ED254FA2A910DEEBD2B9DB03255E141353CA
      FAB5EF298B63DE6AEEAD16B0C84389E6A972D29251F5C8D3A097DD32D0F9F47B
      6F0FDDB4553EEAD9F56232AF90989002390CA5E715EAB73FF56A81DE1B51AB84
      38BFB778E8E88B27EA50B7770F536EC19D8BBEC801EE842F86C752A037DA84DB
      3C791B3DE8EB11F1B7B996B756C9E0C79D27CA872A72F971D771326AEF5CA02A
      65209044F19E0EBA9C5E403FED3E5E44F296B6F5F0698C956C94E45D409E8559
      73EB652953850C4441A26ADE0E8AF17253B0978E726D0E3A7539AD24D8B58C1C
      4CAB49109D89A2BC9554CBCB4656BD54F2BEB4A10C03CED06F47C7D2909A1245
      5B081B9974E4EF65A0002F2389024670D8D7C281F5D8C011E463A620EC810944
      D719E98D4D1E491AFA7142F572222B61C0C4578FAD4E51813E34EEE1A6D420CE
      4F26EE0BE27E9E06F2F704136C646206069D468E7DA0B78982C0A05E5C100DE9
      500FFE4D94323EA90C931206DF3F5583FC408409303A374AA084303F99903F08
      E9B56A762E01DB03918AF8505F4AAE1E51E26E46115E3FA17689BDA41E58CD86
      12C762434CD156AB62FBEDBA4EAD22C6EDEE26BDB6C4A92405252E0FD8509202
      9BC3491A5589556693936F231D32548181AEEC70DB83F776E514D888C552FA55
      9ECD4E7A4DA1484B28767A7327FDF04C4342772CFBDD7BE2A2BC392C00F26728
      156513EBC017CE951B59C4E095C00609A1723826FED09B7B69EBB4C6B25DF86E
      F52AA9678F8729E9C53FC91B65F9FB71F568DDF6BF2823BB8002D06131B8B404
      20438B99D8915A267CE566361800695964D06BA873A378EAB1F01065174874E0
      95645ABDE65B2AC340668987BF878B6A783B291CC396620671A156F2341656D9
      CC9C7CFAEB5C2A5D49CBA6CB48C5991B763A724345A979B75259CCA04444A05B
      A2AEE62AB0B68C8A25D9A0633F112A5B3171F66436E8E48A77312D934EDF70D0
      E19B6ABA81D136BFBB1DB758DEF6866373E0865ACEE4D2E5BCD85BC36A61C85C
      2D1DBCA1A99438FBAD9401BFE458F56E51838D15A24BE3044AE779AF0ADF163A
      5628A2C25785CF5EEFEDA39F2637C6F69FB271C92BB0CB195AE8ABF267950C38
      159DDEF8937E78B6892C7726C545B4F73B3BC9E654A058B34BE5C047A512BD3F
      9938D72BF756F68D4AA9A00D539A9575ACC006E2D9C2D2659F8CF5F4F49C8B45
      E8F28D510581EED6094B2FB9A9A9575EACD2BFEF7CA9299058A5C74A3C94CDB9
      8A3DF198690198F404EE7959F76E189C04DF186029117D08267580CA0744F054
      5ADD0D83CB0870121439258360FE0E6803DC95AA92C1B5A7855C50DAEDC24053
      818F27980381F2032538226502B432AA4A0645BE7740B701ACEC78A401048222
      2013F5794B8A855B0FD823A19728B1C454CA004FD140E90CFD0BAFAF03ACB871
      C8C1FB78580602A3610EC578A019CC638091B0FB439755850CF0261998058F7E
      D009A395CBD04F00ACEC7834011E072601F380F9C060A02DD00FE012A7828E29
      2B7E96C751380D0096804935C42E0FE6752E22AEF530527F3C9E5010D504B893
      E80C3B5E533AF4106024100754CAE01FBC3C09740426230509D09DC0D7C02520
      BC88B02C7F9835704B004E01ECC64D706B982B6600829978F90AB08588E29082
      8ED07B006A6037A0026E57BE450E17A0DB0113503183B4898284E2F9333C3C02
      BC05F8004D81AE0067AE167A19A5C077181C0CC050A017B00CC06C1F3FAB00F2
      A10DBCAC0458DE17A0730C13148544612D542EA28B30B542E44E419795527E56
      FD3804B1AD81A8EAC06B1CB003380982D5A18780910AE63C983703D781122594
      982A3020E62CEB08BCAA0D1880E78130E024187E0DBD3598726A44983F01D620
      F659D04B14BF902DD87AF312C0B193ED209E08C353403CB085883E0316010580
      15840D802FCC8D012360062C0827422F51A52D3388E858292636D82F023F2256
      A98003668EF55AE8679082B344C4F5450311710ADF847D03300D4CBCA0CB4A29
      3FCB3E9849BCF0B4701CCEA780D28A3339140ECB818F812D4414E822AA0F262C
      CE4498A7C06D0B116D032A2EA678C14C4AC4057BB1CA82C1179808F144235587
      607E023800C21274564A3C7480AC4A8B487628F528C70462C9C5FB74C01FA80F
      1098EC853E063852C484696A6197155B6443258F324C10EB4CF8FB14C8059A41
      D61C5B66B213F631C0DF00D3D44197155B64C31D1EFD8ADF21B688242D817D0E
      1007700D8726AB3FF164719D845EC24080E59E1562CE049E84C87E47B3B2B398
      00DC995E4B22D220323F42BF7F15F6D137BE611FAEA8713B05B82B00AE94B7BF
      2A6F478CFED598882956950726785A00463D011DCCF7ACAA62C0191603AA4BE9
      3EC64408536945E3778CCB78DCF7980861EFCC002581CBFB6ED77D8E89AA64C0
      1E801D800D60C54D731A1B9027158E89D058CA958FFD30B8DCB25E02048C86E5
      12629F0F9D07579C079BD1988520254CBC13DCB381FA8001585FCB7979B04310
      47253BCF794CB06DB55871B40FEEBF0229156572325EDCD598089BB5DF6CE03C
      FF878E1CAFFC2DFA056D54C55936A812C85928F956A0F366450C8EE2C5006009
      5253F99848926A610E49775011188C8F5E52636DEC8260969ED5763A986C185B
      0FE1EB0063053CCA2810B5C0612DD004F80CCDC11B68E45AC15C0B4802C2202E
      6EB2612C546E49BA86B3846709FD02E0062642C4EF402F4C0B1B8A0182DC62BE
      02FB16AA604CA4931C5CF9F0EA9602716B91ED02743B50E24784A58C42E355E9
      9828C675BD2B3C6B81324A51387CE10C1F8A17BD806580AC04F979870744D606
      AF572A259739C49D2942D6745CF42105E6F0E05EA250C22EC252664C043B0EFF
      F1F3CE38D4C4713A3543D47BFEC384C9B50B4B5A5740B03A828528B0CA07731E
      CC9B81EB4019A52C632B6541CC556AC919392D7FE362B3648B9FAF6D4E050472
      82D2827CDA08AF3EC8FCF3AEC2AF9E4F602F3726825BF94C6647104FD44A8E89
      6FE6AFFBB6B7E370B3BE8E83A4905C6F22E65CF9EE7A4CC4B42ACC83CF5E5D3B
      D048B64F3B3BFE56A9C87D111EEBF9995FD2439F03C42005CB10FB563077479A
      B0128BBA55780E7305DCDE4711BD095D564AB41D4FC074023803D4018693635F
      7BE8AC52F07844983123CF77FE4B89308702CB81BB1A13C19F9CC90BD950017A
      82F09A52EE5930FB023C26DA8C581E822839728B910F49488980774AA04CC7C4
      F5601C1C1703BB814F819644A4BD8D387FA7E5C23D1DF007EA033C5CD90B7D0C
      50E19808EE77F77DC01E115B449246C13C1BE0940D472A9C30738BDB04FA4700
      B7BC83E0FE0DCCB2E214C886AA1E080449DCDB988869DE3503F60C2636E8EF00
      DFA12485419715DC25187E079E0452817FA730EEA9704CC454F1EEEEC7451CE0
      DF8233AE421ADBB76FB73EFFFCF37DA64F9F3E0B07195E1F3468D0ABFDFAF57B
      F9B1C71E9BF1C4D8B1D39F993265D2B8F1E307757FA46F4272C78E19010FB5BB
      B66F2D7FAB9725C7E5B68CCB679F7DA64C4C4C6C69B55A9FBB72E54AF2C68D1B
      3538AA41972E5D120AB0FD046D3FA9743A953528D05CB77E7D53CBE42609B141
      81BD540AC527DEEF2F7C6FEE1363CBE481589AFAFAF5EBD5616161C330D9BDF4
      8B2FBE68F5C4134F68972E5D2A607FAF1011114175EAD4A1BAF5EA51745C1C39
      3023BF66DB3661EAA20FC555DBB60560527D7288D9F2F6D8850BC34AD32C61B0
      62C50A34F1C2A3389132FB830F3E085CB46811E1C41661E324458D1E4DE6DEBD
      49DDA91379809135369612C0AC71E346640A0CA0D5470E0BDF1CD8AFCE73397B
      052A95B3867CB4886BBCCCA784018835C9CBCB7BF1DB6FBFF5DAB46993101313
      2324242460CB498997C2009AC2E3DB36B59A9C58328F888EA648F8DB6DB709BB
      3233554A8DBA57B4200E1E366BA62C7E3934CE7858706067F2D1A34783415C3E
      48E51718484EA592446C419129173D5C202C190CA431994889CB016C7A3DF904
      879021288876489270531475068D665880C9CCB59A7B10A2FEFDFB77C47AFC84
      B56BD76A70704AF0F7F727358ED17B3011AD9614E7CF93077645E972B24905A6
      6E8D9A0A544AD98FDB60A4740536D7E01CDB0D851207A4B106A7D35B10F34B17
      1A37DC2EE0BC8D01C701DEC769AC81903D22EC417C422B283616D71798480441
      23C4E2400962C22E6C09C2A647393D0EB389F6B56C2D9BF991B3772FA9727369
      B44E2BE9ED8E5FD69C3B3B50C481C2444C01D7C5012BF9FC1F8E2A611F979D34
      CD9A91B3562DB22726624F919D5C387187A3C4841DF7E4B8ADC367E20C1D529B
      83D4F0B8C7A0D745061245E16E05457D4C03FBA1CC232C9288C0BC639F0314C3
      E172C9EF428202C900B1491045F1BBD23AE7AA0BE1F31019BD466B322B9421A0
      2D862005BAFCFCFC921203C7D2E1C88D774A04F454A9C947A3A300C0C3D307B7
      3F58A8CEEEFD5473FF110ADF739082F39C1481FD5E7AEC8BD2AAD50AD1E1D00B
      5F7DF5D53C9CA61BF3EEBBEFEA0E1E3C28A006CBCB2AC817AC1928C884CC4667
      4F1A94162D4A4F0156A3B84975421412323AD36E270709946DC7720CB66565E1
      C8DF88D6ADA53A91513756FFF4D3139CC99371B6F2B9CF3FFFDCF2C3C68D4248
      9F3E25B1F7387E1CF75178920A496631892A15B944919C488D1ECCAE63D79A03
      EFD27120DD21B9B1D7288F1468C75F193D46F23199FEF978C557FD8559B366B5
      6FD5AAD5C2DDBB7747BDB76489E0D7A1430903AFD3A78950EE0DEC02A27C44C0
      0D5DC090221F8B719CF1B905F9949691494A30BF72FD3AD546A1983575AAFBCC
      99D31BDFFCE8C347059C7DF51B366CD887284DDD66CC9D2BDAAB576772320C48
      01729770C04F3E4EA94091C53A835CCA24F8409B45372112971BB147F12C80B8
      268C1D2BF5E8DAD5F1EDEAD5D35E797DEEDB02088B73E6CC198254BCBB362545
      F7D9CA958207926F4091CBC5D94B2D62E6817AC019EF8699C59369B5827CA152
      9C38416E9CE7C45149B9DDC20146F7F56BD78E7DF4F1E23E5F7FB5E22F1E6A10
      DAF960E0736470739CA613B66EDD2A7065E343251C63160D175D052A99332F8F
      2488A1903C51C191239481D38DD1515134E1E90952644464FEAA55AB5E5EBE7C
      F9FCB367CF3A640638D023A2B9E8D6B163C785689302BEFCF24B4287030908F2
      99403484323D09CD018B4344732D3BE071E3CF3F292E3494D02149681C5D88DC
      9A6FBEF9661C0EB15FC1EB5B2A2E2E4EF71CF639AF5BB72E0D6D927BCA9429EE
      860D1B4A38DF27E1ECAB141E1E2E4544464A21111152604888140CBD2EDEA3B7
      9340D0FDCB2FBFD8E7CD9BB7B165CB96D5217639E24CBDA4CBC42944270E731E
      816326F63E568B8D8D35A367A3E0E060818FA972138280E4EBE343F14841D3E4
      646ADFB62DEF85E71D203948F16A6C637F1E31FF0B52E0320052841A226BB71E
      38EFA46DD2A449731CAA1A09E2CD7106CACBED768B7CF6123AF6718A28584A6C
      041325744E7CDAFAE8A1438796EFDCB973154EA572775942FC16D5F22611C47D
      71A8B0338EEBBD8E93A71B5F7FFDF5833878780C07448FC0FEFBB871E316E3CC
      E010883646A5421B529EC65DB9B02C51A03CC2D047D4457E34090A0A6A880DAF
      DC995840A144C430FF6F54384927211F6A23ED2AE0BE9458552894AA7D1D3A74
      E8077FF7C5A44A0693274F2634219FDD2F932A19E0EC1FFD1B2655328068E8DF
      30B92B06FF86C95D33B85F26F7C4E07E98DC33837B65725F0C6E6702BB1F50A6
      9EE00C0FB763154F86C073A56ACD9A3584CF2819685D657FA8F1176028C3044D
      B8DC6CF3680FEFEEAC0E1F3E4C4C0CFD3585A27B64DF1847F5869E0B38003BE0
      01A801B6432B54558AA898380EF81FC12755E16754DDBA8458AF02896BC0F122
      7067C38C60BDA5AA64C031C787C96C1C2B7E82839D3C7992EFD6612337215E30
      E400D7800CA04CEC61AF3A0F4E9D3A3565DFBE7D3FC2F319A4623A0E4410F7CD
      384A4F689F36C1DD132893C1B0DFB5B2C0671C100C3011154423E1330B432589
      6F179070B3C014BC637FD0CAABAA44C4997811C158BE0EE88EF4F4F44EF89686
      110B0A1F7E48F8EC7D0D964A5351150326CA4C58071D220C4D7E66032E089045
      C566C017E05204ADAC922B4359A7AA6DB8E92406A38913C53E5164DBC27C18E0
      CC86F6EF15E74730C83404DA0075804AF301EFEE4B311326CAE2619DEDF745E8
      FF6EA049DDA3958BC7D68EAE2C165595A2CAC295B8B7695BBB73C741DDFEF876
      46EB16258EA50CF7CDA0431D3FF1FBF90F77AFD7A0DA675E5AC1B779C7A66B7E
      79AFD7E0456392CAE4C57D310071C584C75B0E6FD3BEFE9ACCD46BA6DD7F1E22
      312FDB52A34EC2D2B8EAB1934A33B9670645C41F6FDAB2F622D1968FEF05133E
      750D948FEFB982D4CB14111D312722266A40B194EE9901625E42DC89AF4A86D5
      CB83D2B1A9F2E6CD2CBA7EFA38F98746BDFDE984468D98C93D315830BE798D26
      2DEA2C126C79E4C2717A2766605C761C822BE0234D0ACACCB653FACD4C3AF3F7
      61DBA73F9D2C9317CCAC52402C7286DED8FF5ADAEE6503A5ACED53A59BBF4D94
      52378E91CE7F37583AF2E9C3D2AFAFB7957E9AD95C5AF97CD3ABDD1A070D0231
      2D50FE138A1D4B03C439438BC4924787F61F9767B142833CC98EC36F69691974
      FCE465DC16AAA34B59F6F30B56FDF5CCBED399EB40A300B8738703E2E22D99E7
      11CB3B2ADC8F0270DD657646362E2BCCA59CAC3C32EBD57431B384F806109689
      43BFB34A59D0BB7BCED1B952DEBE17A5AC1D4562F971B47461F51069E7C22ED2
      B6F90F49DF3F9F2C7D39ADE9B93A91E63EA05661930DF7F26AC1B8E41A37F6CD
      4C2B4DFCDA4F4FC8C4FF5AD647DABDB0B394F252539679EA9D8897EB0F209612
      995F3F7F81B07519F79B6AC9692B90657EFE7C2AE42AE19EA94C7228D5E75F59
      BA7FF8A17359BF228A4EA09C2A534C4B13175114BDCC1EA4568A7225E20CCDCE
      CA2511F343972EDF9489CFFEFCC03320BE15542B240E77790D877502F17219CA
      99AAC47597D938F3710315092702E9DAF54C2CC0E8CE337194967F97A1D750CE
      394359E69CA1EB21F3552FB5B8A70C5576ACEBAF1C3DA849E7A62D923EFBE730
      4FA17960A7B21232B7913D0FB35928E7F938B69A95914B792E4A9DB91C323F9B
      C562B9BBA208F9187EFB6CD84FB9288A27568F90F62EED27A51615C5A3A8A19B
      66B7927E98D1542A157325C2DCB5923DB778EC9317767D392C202AC0585DAF94
      E8E8D1B314E063A434C85DC07459BA9DCECF5B71806B28CBBCD20CBD13578FE4
      44DF2E7FAF1A769D657E60714F69E5738DA45F66B7BCBF4A540927AF56B5FCFA
      EFFDB4EFF5DDEF7546C3D542FA7C72C33B56A24AE8DCD1D9AB51BC7797D52FB7
      3ABCFAA5E6655AC53B86BAC797DC9E344C0836764238B9C985FEC0950E1419D0
      FE9D126811EA7E318D5142B9B6A9F8D53DE9A5689665504CA59851B1C77BB517
      D3815EA6B12326C4C00B59C1EC6716947E2BC803D0C35C8815A4677B45FE6537
      3970E1432C7100B142A70A9F06B8D6029A014D81188019572CD2625AD0CBA600
      A12A506EB8A9806A000FD7EB438F0028B51F8E7CB1E10E2864004EC57EF035A9
      C45A5AF017AB96272F5DF1C92373E9936726ABE63C575FD9AC73A0145ADDC369
      0C2487C0459953A081A858178AC397E8453445DC4C59FDFBEFBF1FB869D3A667
      B166F91EE6B1578487872F0C88F07BCE1C651CA40D5536577A0AA139CA4C9556
      A3D387EBA2C31A51CBCE8D2FB7EFA9CDF48807C1408019422BAF44DC7235A1FB
      D56ECB719142537C16E19B2E4C83F30838A7EFA0D305C76967F6EF74B060B71C
      D2ACB0A0875393DBEC349AC30D3D1AA89B4FAB7DA959DFE053B156D943E94751
      09E49B2959C6F2E58438B08C836C6E191E7C53B7E04D76094DA9E30CE94503E9
      15E84271F03FCF954B57E8020EC385E8DB85746ED1D7386C0E26C947E24EC280
      D23CD8CC375366D886D853F0E12B13C65CB57CF562802188120C3528521B4B2A
      9C0DD7887C56594D2A6C9F51838956D051B8104BC18A70DC911DE581EFE52EB8
      9972216EA67C06F71246A3748ACC806FA66C8EB9673FCC1F97BB99527062225C
      5452A62B9D72DD39B8710027A9717EDFA034522845519C5493942E1516A9045E
      221330E5ACC0CD9461D0DB61C6DE0B37531EE49B296F26FC1EDFB5B29B297D95
      01D4D0A339052883911215A9DD384DE70EA5669E6DC9DBE02313C74A0633E059
      79BEAA51BE997265DB6F162205FE22164737C2C0131B741E8BA2EC19B74ACAD7
      5FF2128B9A34E48F7BACAD4A7F0A548492AF18485E9295E8266E7AC8C2850158
      EFC1358B84D8F247BA7C3B25EE7F2C2EB6928869FABF5FBA3CA317D607DC98D2
      E7A575395658AC20CE742C56908E3C285219879BEB75E4A70AA404734D0CC804
      E285A11F7FFC91F6ECD953128E2388084B491FD47E1D7A36F7C912D603D66185
      C317A5A073400537538607879331C340972F5CA0822B763A70FA30067C2EF922
      495E9D2A222A5FDA00A2B2C23D8F5761C867068426DBF1DA148B0E2B4D569480
      06CC84C5853A527233E5CF9B36E3E2770D299DD8F426E25A6A0C064080B09C22
      334258AC029B09533EF2CD94FF4C39F5164AD2C24206F0899B290BB051E05DA4
      64066EA68CC202EABFBA99122459B9F85106EDDAB50BC0DCDCC79855598BA663
      1D96E3D7E166CA75B899721DEE1459879B29D7E166CA753367CE5C3769D2A475
      B89972DDF0E1C3D7611E696DAF5EBDD6A23E7C8C959269205A1BA858E192AA78
      AC947FB16BD7AEB5B838721D6EA65C879B29D7E1664A99096EA65C873B1ED7A1
      F6AE1B3264C8DA5EBD7AADC4A2D27C148851A0D81A08064AA4C37920C1A18CC2
      CD94C9B899722533C0CD94EB3077B76EC68C19EB7033E53A4CA7ADC5FAE7F758
      BC588CF6EB4554D48711B806600244E0369A450D93FCE2D643C42A53374C3CAD
      61313171C49A097F83BD16F3B1A630025E5B125110C0D399D04AA9129A6C2846
      A9F74546056EA67C044B5D8B7079E1FB98E67F1EB1ED8577D5815BB185A58C2A
      A6075D40112D271E14AFC29A080F72C05142047413FC1E844E15BC1764F762FF
      B2A5F0512A23E050D40BC154A8EED75E8A5121E7427277F56CFCF9B1241FB5F8
      A8AF56D1DAA8168D82249C744AF4B55312D67CD0252CE376228ADB1D2AB3575B
      72D414D7E7A991757D342F350FD2B7ABEBAB0F8CF3D25A42CDEA506FADA2A942
      A09A8D063C7DF9F7CFE75F284D4351DA529939E8FD7DBEA17AF1EDFE31E6891D
      23CD81119E5AC1DB43E5F2D2ABDC7E06B51864D218028DAA1A289F0FD57B647C
      5AF3C7261DD9F2D95B72DE96CD830A38C4BFFD87D6A275CF1C5ECD6B501D7F93
      03753FB7C0E17669D0CAB9211F1BC26894A2C26A50B98C1A45905621CC3F9556
      9005E7EF01AA320FACAFFFD2637CBD902F7AC6FB2BED4E77BECD293914D889A4
      14053117F70A61D3882428483469952A4F9D529D99EFD21EB894B3FB7A4E7EAF
      49AD232EDE31054153D7A8E37CF48F3609B6E8B0AF370BB1772895825BAD1014
      0E97E4C62734F680A06BC0382133DF69D7E020B341A370F89B344969E9795D91
      820FEE9807D98DFB04B68FF59DDA32CCC780FD180588B51B5B2A4889656BC29E
      68F44CB8C1051FFE9083E4262C620B82562588B83D487333CF9E3764E2B41F90
      2FE05399D288DE5E5A9545A9101C2ADCD6A3529080D80BD87D22A94551500924
      405C12EF7B811F41056A9CB242BF62E8BEAD47FCEE98026AD2C7AF69A8CF6375
      03CD0AFC17880312007150458C4591900EF465282B22F244AB1495B8FF085708
      406412A92FDFCC4FCBBD99B5FECE0C6A7753C604981F490EF5F6847CED60C29B
      894115BBED21234E38122280A5E8800C6D4EB74BAB5428721D6ECDD96B397FBB
      33B356DD91418B5EC30B2EB9DC0D9283BD927C0D1AE401B939E64800149820F6
      4E8CD8F82F549C6EC98D84881A95427B3EB340BC7839FDC715F3267E0FA9713C
      2AC66F533B398E5FCF59FED33F3C0322E990C90220017CD217291104480ABA9C
      C1FCA72FAA6CBB4B73E65ACE55776EEECF470E6CCBBD633165B6F959996797EE
      3A7623D4A28DEC5D2D9037E714A0B822EE50F2E701FE800442F250899A7CA7A4
      DF7B31CB967E2DFD7BBDD3FE3B87BFA38868D482487776DAC2ECCC9B75F6A4E1
      EE2B954E1D62D6A9354A14279451441DF559E0B1A3FE4A8E43BFED6C4641EAA5
      1B29FA82FCD7A70E6D768119C8B26443393CFE462465DF58846997B6A4379360
      F4921223A22ED40D0F70C6FB998282F03F1E1A94D13C64C2D52C5BC18D8CBCF3
      AAEC9CB566B7E393A9439A1C2FA65731833E5323293F7B11D972DA928719ED80
      8F543D32F664B7B0A0370A5CC2C1CB92D8081BC91254225A088C208D6ED7298B
      E4DC89EB45F63F3BA409AFAB15D3AF40EFF77C04B51BB289927B48D476B024F6
      7EC65D63FAD2E3D33EDDF6F8EBCBFE2CF9384FF962AB76E967BF1B172DFB5D3F
      6FE91F8A0A28C94E6553D07D1CC4727311655E6B4B3ADC4F61B64AD5136B9FEA
      1C5FED352F85E28BC98393EF7A124AA68EC7AD5234F0C508BA761EC4AFB725B5
      8E44BD49AA1E5FE36497F86AAF5B14CA2F270F6E7CCFC441BF48F59E12435DC6
      FE4CC93D256ADA5B121F1AEAAE39E9BD13D33EF97D589336BDB445BEEE4B13E9
      B15989F8727897ECB96DB0B99104B30F55AF5EEF54E76A355FB328555FFDB1F9
      BB7F1173DEDA6FF2E9828FB3C65C1415165FA956629D635DAAD598F3E7CA0FBE
      9CF258E3FCFB8A76A9400ACF76839C68C46AAA7406DF9A11317F778C8A46CC95
      2B3E9EF7F4BF8A79310FB9140D7A7F731794E9C1A11AF5160F51F874CAE0E4BC
      620FFF562F3BB2BB7DA075BFD4CB0CBC4A594AE815332A7E77AFF612429CC9A5
      2CF298B39818BBC3ECF7DF7C114B42168D6C20FA6FBEA8481025DF684A6CF774
      B323BE8F69D8B0615DD85C1E026EF28F2DE39C2BDB3CC8401E7D0DE4F74644EF
      9AB20B3FB01D54AEC06C56E06BB2D9D287C3D3315741D8A5198BCDDD291D3B76
      3CD1B265CB13351A269E906A169C3816BFE784A2B6E384AA9EFB84BB66DE89BC
      EA374E508D82132D6BB739D1B05AA31335126B9CC0BEE1E39819F80BF31709D8
      37FFC5F179FD273003115BD26DF8AB0D5C80282744DEA9BC6CD9B2D823478E18
      AE9FBB61B8B1353B365A134FE27643ACFD8ACBE04E2383718F6F6C942281FCA4
      20DABFFB40ECDEBD7B63376DDA948CF93815136DD1A2C53C6CF51D03B3BACC7C
      111CE4BFE368DBB6ED656CCE6889ED0C2DEBD5AC7F99E78B7CE22D9715A77481
      8A144B4BDF689FCBC5F345D89A128B6B794FE01F50BC31FDC024781646AC70BE
      88DF622842984362A30C25FECB88E78B2C0A6FA2E3EA58EF2E1E5B02BC03738A
      E78BD813CF17B18EE907D6F8FF428AF340E27F32F9FBA5976E60BE68C010C20F
      473168FFFEFD8143870EDDC21352D82E1DD8C8AFD109D7099C2EECE6B3C5C7CF
      2727415793BE7CE7AB2E1005FFFFC40DECB2ED82D8DFE0C881042B79BEE800AE
      82146193E78B3019BB0166C2540E61DEE104B67DE6E0A33B07332F2778BEE8D1
      DE8F9E08B006E6B8AE101DF8F930610A2105194A818181DB81144C856EC70657
      2621A378BEA830296835CD982FC24CCBBFEEC1983A76D6AEFDEEA16FBBA185D0
      700AD88DFFC9A400FF2F64C13CC4DB3C6D3366CC9814CCA4A4601A2105132029
      D8A69B823D2D29F8D3A314EC4E4E89888848C10C630A8A640AB665710A52B0EF
      280563C914944C1EB8325D0C63592B05A4E2BFF9A242B1235F6F09A68CA5C4F9
      BFF9225914B706BF6C4527CF5A09EED75E4AE48515AD84622943294FA83015FB
      BB0B3F6553508A7E192313E2D4B0CE2F4A9BD97E071416A9D21E164971A5AD6C
      C6D8C8E0C76B6766E1D63A9A595E28E2D7B75041D8B20C0A3D1C9343702C0110
      E7541AE0567E1DCD8CCF64F82925C26398382C13C15B0C6E118F07B1D28ABB3A
      EEA9AAC1B121501F88002A5A47E3B0E598B0DF12DC3E2EFA74C52793E77D3B67
      619755CDBEABFD55E8D791CB8DEFF97D2E0C84C8BC000D50F13A5A1145114992
      8A81C9F007B38EC685A108B744048EE844D06F3FA07534D0635586012FA9A0B3
      C017959B1ED83ADABAC09437985366BFAC1426FE40D6D136F488619A5CBAE471
      D1DFCD8FAD63E27C648C974A70A2455E5A51D85414A98E25795C847D5176871D
      3B5D5C64517B52F1B84881BFD7E5CE1EE104C857853EBC390603F3FAFCDC772C
      98941D17616E491E1761C1145FB68587DF4AAFA3ADBEBCB9CBEA4BBF7409FC9F
      5A47438C64F53FB68E3635647CCAACF01752EE7D1D2D70C6779A6FE78C7E90EB
      6807C6EC9F824C7EB1B0A221E1D3BC9FFB70F5EAD5BB70A9ADC4832F2C17CAEB
      939CE13C94FC79E366A2435847BB848912519EA9432892D7D110461E3463742D
      E7DD17CD97F3AC2F7F23147043267BE4C7835A47635AC528C3001BE23371C2F7
      751C0F9B89C1942F966D051EABF21A2717630CA8E4187285841FB9C4F17091DD
      A14BBC0A883FA4BA564CBC52FDC1AFA3312B6EAE0B1B28B93DC7974FF23DAFA3
      4DDE5AB7A8E1946930D9B2286652E87AEFE3A252112C2451D19399B07BA167B9
      29C700B8E275B4527E3808625F2EE682FCA2A207072EED3E4AA80502076527EE
      26CBBFAF905699E65A0E5CD18309A216A0E20832D84FA11B9BFE1D7047C2BFBA
      C3E86E526042141780514F4007F33DA9BB61701214B903594A3872002675804A
      677AE1A78CBA1B069711E2BEEF30AA92016E72C80583FBBEC3A84A0620CE6A07
      1E3680951D8F34802FC2A8F00E237E578C0A1940C6D140E90CFD0B01AE03ACF2
      F1F8DF7AAF23EE70E1320F09D03F7870F1EC08FD5FDDEB58D2E180787310FB0D
      3A962528136B96AFC0FE1C507CAF6307982F01BB8168E076E50B8773C005C00A
      C89115612010D540FF0D1825CC989175A73B8CE0271ED002659482E4A563031C
      8702BD806580ECC80C46C1F20C88C7402FA750A2DAC07125A0072E005C54EFE9
      5EC72710682EC0CC6A43DF076602F46275DFF73A2A8B28D484BE0B60F5321E6F
      025C9154D02380DAC8933FA0B702344020D646B742AFF20E23390FE091958B1F
      4023E067882511FA53403CB085883E03160105C05DDF6124C233ABD378304168
      B4038FB6800DB8083C907B1D3F06A1F1C04AE065604F6AE6CB47A0AF00F8767D
      CE5418498F4728B01CE0305BE82EEF75FC101E9BA0B87640E6EE85B913F00190
      0F64C2DD049D55161E5CDEF90EA368B4B487607F02B8F3BD8E209A0E4F8F023F
      805813D87F0074B0333133CC5930DFD71D46C5794020C2C91E0D42DBC0642110
      033333CE85594E815A7D34076E9F02B9403314042574BE286927F431C0DF00D3
      D44197155B64033FC06411F438C01F3801380027208BE9F293D5ED302F01E600
      71800F50ACFE8481C575127AC50CF0825372028C1E8659058400D14089982077
      1BECEF00FFEA0E234E99002295AAB08F2ABED7B1D20055BCE0D478604ED4C480
      5F0F80DDA0DD59714CEFE48353E1A351AB5B5A7D7C9E8E8C8C7C9D810D6493B0
      A0D10601ADC01D69DCE9A588EFDF087CD24EECD6BDFBDBB89563E2E28F3E1AB4
      78F1E241AFCD9DFB748FEEDDE7E3DD64F889BA13934A1920EAFEF8567BF2C9B1
      6387CC9D332776C0C081E6264D9BEA31C5ACEFD7BFBF090CA3C73DF5D4A34181
      81E3C024084C1004CFDB54650CB47A0F8FF68865CFC183075BC3232214986210
      301B2F83CD61E1E18AC7F08EF7E261B76D07D0D500E554850C3823F15DD61E81
      AD4806F6CCA0B12EECB18A0960E38C28404422FE41D807DF73ED11C65CFCB2B4
      5E110301F30EC6F0B0B088A8E8685C0DA528EDBF8C19FB64282A325215111E1E
      CA61F0B29C982A6400992A30558F82A281B15C18D02952D87282BF8FE333B26A
      78E49894F3CC8E45BE6F69F06CC205185D70FE38109FB2D874034AB75E979878
      F2049FAD2EAC9B9D3F73F6ECB7F8D44D2F79596428C791DDC1C01B227AEDED05
      0B1E79E8A187F4480AFB63F0EB6248769B4DDABC7973DE53E3C67D87A5B24960
      9856FCB258BF3D50B1BB1A1FDB5D7AF6E8316BC6CB2FE3CE9728881B5BF58ADF
      42476CA533A74F3BB15FF8F4CA55AB5EC4C7F81A38736308ED96AA8C01FBC0FE
      54EF9143870C19357EFC78AC19052904512E4D7CFD90840B95DCEFBEF3CEE58F
      972C590C317D8000E5620F37BA1303484A080A0C0878FA99679E198C754E4FA3
      C924FBC79481B4FCF3E599B3E7CCFE122B51AF433417404C02CA29394039D75B
      0E7C0554F5DA49496FCC9F3FBF59A3C68DB5E04A870E1EB42355BBB7EFD83111
      6BA0FBE0DD0554A8AA62C08174F83BB901D834FFCAB4E9D3FD519BE9BDF7DEBB
      897B5C5EC3F2E287F09003FC2BC5152FA67EBD7A3FEFDDB3A7E0D8B16376AC75
      EEC6CA1F8F002BAA47F7C58CEFFA7A79D1871F66AC5AB9323B3424E44388CAEB
      BE2855120837CCE93A8F78FCF17353264FBE8A35CEA1F05761E306F7FB52B89F
      4A55BD75AB567B3A75ECF81726A39A808A0278608AF32128363A7A7D5C4CCCAF
      68D8A24059001EA8B26A359A15C06A50F5071EB8B28022D7D84FA17B030F5C69
      41F11160105032B082F9812A66724FC4ABAA285C7AB4E873BDD02F04E37EB500
      C09FCD70F341AD668677A451E94B542401C55181BE40A7D36A7D2C6673184619
      D10C2CAF87A339F7E67760526987C4B2AB90011347401513C14D7135DAB66B57
      173BF7635F9E312314ED7FE8A4891363DAC30DEF6A6184E1CD7E390C13BC1D15
      32400011B13361D4903068E0C0F009E3C71BBAF7E8A14C6ED24481FF835574ED
      D64D8933081E383511E6EFE79708BF66B44D8ADB89B3BD1C030C3FB852E1FA3A
      7D70DB366D8230AAD3607421A0F7E1934204F9139B23A3A284EEDDBAA9D1A506
      6280108248A9392C132D8D720C504B05C446035907A2D5D4604B837CAF1A6229
      CF5BE39D7C300B994C01818142EB366DD4E8EE02E0AEE5B0A589B359C98FD280
      2CB9E4683024D487868509C80799306257E20D7E6437D46A4A53867867C50F8E
      CC4E8C4F96B49E11D8CB7C0E1E1917A16F2C9702747FF25E58649E02A5485E43
      80C772EAD0A57C63BF65A99D67FE2E0CBCA24BEC20A93C1AC09319A80E740646
      01EDCA650C92CAE34F1D86E8C118E8EA58DEB7339AB5E16AE28B6BAFF4B891E3
      F4C4FE24D266FCB5253FEDDC4752EEF585640CFA09847701C7804D402935728F
      4EAC374A69341A2DD82FD41EBB7386A1531F814E7E2456394682D8C8219F9EFE
      3C7AFAA14351D30E1E1AB3F4EF0F962D5F31A27AF5EA1D380C32BA9C444A512F
      3442BE2C772D6A6CE2C0810307FCF9E79FC3AF5FBF3E12E39E918F7C746A854C
      1C0C3EDF7AFE45DCFC367CF0638F0DC0C749758854CB1F148554AA7822260AF4
      5A96F0F0F0E467264D7AF4E0810323FA2F3ABE9C8933B6FD9D3AE9F0E1C323A6
      3EFBEC639111114D117B4F0E5305D932AF05144B159A045FEC6369DDF099B51F
      853FBBEF5024C4B2E5C8B5C967CF9E1DF1CE82778642346DBD3C3DF9FF32F97B
      4D2843A12A0B8A255FE4A951F6F96AA466E84F7B029FDE71F0BBED67A762756A
      E42F9B370F4705EB8DA62202F541CB7EABA257F1FBE13B87D0E3DB7F1087FCB2
      B5DBB8B7A660DFD7080C5B4660F7CE90F0F0F0C658D632552D9A917B5434724F
      2720A884CBC83DC1B0CF07D6D1885DEB35ED67376DDEAC59AFF52929C37177E0
      F04E9D3AF545210843C6F2774115A22924BE4E263672CFEDFA521ABC39049968
      C270BE193E06877DF0FEFBC3B0B0DD0EB1BFCB8C2D4C412F30F8002866B00CE6
      4E9C22B9D8E2DF9B51E112060E18F0E8E851A306A312D646ECF5F72E7B543410
      2E575970EDAB0AE3D3E026C9C97D3136EA8F221C09D9AB11812AC4031F77A994
      682E7C22C3C3BB454544F484D91F2953DE65D8BBF2C695D482D6B303D019C479
      5C5A2EA57745A9124F2C0A03DEB502DA0226E08133D080683C500DD00202F040
      15C79899307136DF15F1BBF1C8FD83124D8206FDB106154C07986136C14D8FD2
      A404A74A5353290364248F8B54289216B3C9148926B90ECE5B36445F5D0F152F
      11FD6F00FC681C3DBE08A4E13B9AA1788F04E6019380FE402B00A745C0FE7685
      801C6B795C844A55AD59F3E6713874E8875184A555AB56E61AD5AB7BE5690222
      4F470CEFE33404752151591D732501E86BB96EF0C0389C88E086750818CA2924
      FB8EE3A26B7E6D13CE860F1EE4E9175C4BE5CCF2147253778FEE9A243CD1BD2E
      15A388E880727D32AA3FC75E0BD14474EED429BAFF80015A5C9CCA779CF27DB4
      F4C286F47ABF9CB225414454375875B269AD28FFC4A8C040855A671344052EF4
      2ECC8EDDC7AFC83CCAE50102563A2E1AB7F24AE31D67F2E351EBA86FCB44AA5E
      A3469CC96810C965D7BB5D0E25A60064A2A51FE51840FE2244546E5C3469D5E5
      FAFBCFE74771E0DE2D12206E6C9CC7F607341B84A362F8830717EE8C7117469F
      3D15A11C838AC645D3BFBF92B4FD746E1C87E9DD229EB522081867E1003A6205
      8772C4E1862D37FC2C85DBC7454B0F8A893F9FC8AB055254963861B25CE2FB65
      293B3BC7452A7D8EA8543B31612231B9E23C50B0A53410191EFC72E5B11ED2B5
      6AB1F79A2A09190FE2856261BF7C8506A7D485CB9BF3709374766E4181526FCE
      11152ADC465D98906206025706046A021CA38FEAA53203EAFE4998A8B33C6DD2
      6BAAFBFBFA28073D94C44C51D40B03736632718C95A46B69379C9F7CFBCB4687
      253A9794BA4864CE65D062B424FC1454776433E8038056307705BA90DEDA4252
      68F5AE82CCBCDE2DE28C1AB506B32E22C2163270E37C386F665AB9F6A7B33F1F
      BD213A3D028DA4D4FA224378F8525CD14092BE6406D3D8045C050C00ABCCD1DD
      EBA96BC684286CF9B94A4172A9341AEEDF458E3C39F18FAB4BD6EE725FC874AA
      9C2A53061CBF05F7EF4954AC44E03F8143C069E00F963574E21AE82F1B0A1F26
      EC5597946A9D5D729B32B37232D4F84ED378E088CB8573678E6D399611979195
      ED907252BFD61C9AF7ADFDCCD67CA4CA5D1894F2A0A702B2E214746553FDF840
      D64A80BC8019221104C9E9748892D3A63D73E6FCE97D171DB199995912FDB5EA
      7DC5CEF91BEC69A7F331282E268E306555490ACA3A17DAB8C871C9506A0C79AB
      567D71D6145EA72E4A94147971F967C78EAFDB8333FA0598F1AA943853A93405
      FCB2188B576FDB56A030266B14A4ED1FF8CF777B7EFAEA1F5CF87F065B7E0AEE
      147B0E5FAE26B363697CF0FD9E2D92C694EC12540589191B56DD3CB93D033BA6
      D2107B1B88CB95AAB4FFDBCD7714D1FBDFEFDD8292D192DC4EB7F2DCAFF3739D
      DB434E68340AECC7CB84689CC8D8FB6700E2E7109B96C43F5BD66BC28EB732AE
      04065A91F90AC43E0F0C58F65532E09ADC0234060015A97438CE470DBF0EC226
      2CB534825D61B3DBB7A3A9C880999940AB5C310316536B78E1E6C21F3AAB4C3C
      D683F06FD05909787800F5016EBF7641E7E9CC2A19C05F2935728F066D13132B
      E5281BD94D03533C500DD002EC06EDC12911A49809136733AC55ABAA62C1FD33
      7F90A39353A9263F333113DF6E949B8FDBF60581941AF07270CB5009231C1E67
      F957F81699CAC479546DC080D78A418055A5D6D2D8279F228C67E463B72E8454
      487854A216BEB3E0D65D6AA5FD1411071DAD978F8F4F2C4E4E042563629CEF54
      632657AFA10145ECEC6855993E060AF22E5BD6D12312FA0979C2844FB754284B
      C8A3C27191CBE9249C0FC7558B16C048017E160A054270435F882FFE7ADDC740
      56935676F331EAE4389713111A33EEBDCACC176127AC804F261C7A8650106555
      71B44AE52033962972218641251696E0720C90CC0AC7454815A94484C6D53693
      A74CA33163C652604830BDFCFCB3E86F0A3B7F2E00689F0834087F7B803E48A8
      7054C1AB7EA66A0909519846D3E1639BE72EE440BF6FDBDA1517F05087871E22
      1DFE301E9D28B56ED5825AB46841B8DE43D65BB76E4D6DDAB421B7424D7F6EDB
      5A3E93D1043067160917CF32F34538684E0AA59A9E7F7E1AFF77887C6BF12B33
      E7E03F1870A7911BD70D009C0294389A3E7D3A044515A6808B6785F345DBB6EF
      ECDABC6953C2520B190D26CC7A29A87DABE694DCA83EB568964C6D919A36B0D7
      AF9B84C53B2D6DFBE3CFF28B4428662232D4846F8106F83781600CD7159814E1
      6F057AEBADB716E15F05E4D835C2D5992C0ADCE1225F7383C281C1805336737E
      CD9D3B9770C74BF914148988476C5C007CF055AFC25D449C2AC2DE78390F701D
      14611686B89CE32218C254A72CFF66CD9AC9F2C7DF1A20057AC27F21944F010B
      0E31E0195F232A59B5DE0F3F1C85F9512D17D585EFBFBF68DAB4693463C60CAA
      857F3961C2B8308683C8401F211706D6DF78E38D8A53C03ED1537146E3CF1ADC
      F95890D3E1BF408C11E1E122EE28EA8AB3525417B745436C722C5954FC473C9C
      1206DB31D727FF9F4BA52960269029E705AFDDF8554F4CAC8BBBDFBD0E1E3AF4
      31E7C1D4E7A657590F66CF794D4E41A9BAC864CB82331CB3BA7AE441DC237DFB
      267A7A792D2B2E7EDCDE88B85F4AC01F8B71D1E4F955160DE72122271767CEE4
      3B32607608A8C2E7AA35B15AB5463D7AF6FC168B73F27F8280B95C6A9818FBAB
      086AFC494945EE65DC4048C467EBBF982F2A43AEBC859B6EF407DA07335F549E
      BEECF2DF7C912C862A1E5CDA0CF0D30A680BFC375F04214085180CAA59E1E15E
      EFF8F824BDE7E9D9F36D1F9FA1737C7C86BC6834F69CACD5D619AE54E29F5988
      3FFCE0BB6255DC7D97791B171626FEF8C61B5EDF8C1BD7B0439B3623EA75EDFA
      6CDD61C39E6A386EDCB0A64F3D35ACE9F0E1E31B74EB36A5619D3A239EB65A1B
      BDA0567B872A71934E192A8596720CE28282544B5E7925AE61E3C623629A369D
      1ADCB3E78098C71EAB9FF0C823A135BA75B32675ED6A6DD0A74F70D3A143EBB5
      1A3DBA7FC7A143A7766CD366E4FCDAB513633D3DD585646F3DCB30883699544B
      FAF6AD9564368F56D9EDDD307A8AD67A7A1A0D9E9EA4F3F070A06D29C010BE40
      A7D73B2C70F3090E3604D7AD1B15D7BD7BD7165DBB8EF969FAF47A7BDF7E9BC7
      AF251C94C5A650954A313B22222E5AAB7D14D7BAD577E4E498D14FAA44A53217
      B7425F2A70BBAF2AB4DA1C7414126ECF3578B8DD7E98D6F113888C684E2C1AA3
      B1B687D5AAC0C0C61EEFEF7FE0D8D5AB4EA68DF7AC11BD6DB5FAB56EDA744468
      CB96ED9456AB294F140D0A954ACACFC8388E3F4ADBA9898E4E57A9D5F2680A5F
      38A2D26E57E79E3DEB8FFF67AA265CBA146CB2D9242C6AE6B995CADFF71F38F0
      418B6FBEB9C094651105E1FF6CF05740F53CA3A31BE1D355EFC0048764B7E3BA
      205185C352C1593B76844A19192A83D1E86278797B3B3C7C7D7315919167533D
      3DB79EBA7CE5A03D372FD79199A9715CBC58479F9DDD381C13EACC4016D108F4
      28A14141CDD06A7AE27FB8708F932DD5AEC5BD875AADC960B59AED972FD7BBB4
      76AD44DDBAFDED151555325EC7362D973A24247D43EAD5DD41265396974A5517
      E14D1E2E5793F11E1EDB9EB6D92E2B2D4AA5222A20201893A0D16ECCCA38886E
      9CBD79F3BBEB7A7D52B2C1900CA6164F1F1F8B949A5AFF464A0AA97AF4F8DB18
      162633E19E0CDFCAD2E18C8CB4DD6969975B070585A317F2532A959181DEDE61
      96CCCC6BB83C4950983D3C82152E97C5999323397273CFEF3E7E7CFBCFF9F919
      81A1A1AA280F8F060A22B3A7C96429C8CBABEFDAB94BC8D36AFFC29596982ACA
      974EFFF38FFBE2A54B993B6DB67F1A190CA7B067CB4770B98C98DB0847E6EF27
      244BBBA17EFDA1C7BB75FBEDF48001DB8EF4E933FDF1A8288C587C129F183468
      C4316CACBBFEDE7BFB6F2E5870FAE63BEF9CCE5CBA745FEEAE5D1F5D3D7B7642
      E975345F9DCE734BB3664F9DEAD061EBB156ADB66EA8516382A752A9273C74EB
      6BD51A73B875EBAD7F75EEBC7573EBD64F46E874F8F72C83BC8EF6D2F8F1A34F
      2C5AB4F8EADCB9FBAFCE9C793A75F6ECD3694B96ECBBF0FDF78B663EFDF498E2
      753403886DAC5367F091E6CD7F3BD4B8F1B6F50909CF82B607792A14DA35B1B1
      C377D7AFFFFBBE66CDB6FD5ABFFEE4A7341AFC519EA6641D6DC12BAF3C710A4C
      CE4D9BB6FFEC534F9D3E3D7AF4E9C32347ED59DCACD9AC8E9E9EB13C30F05328
      746B121246EFA957EFF7DD4949DBD644454D046D3D594451F3756868CFDF1212
      36FF9194B47D53AD5A6FBF89C5378C16E47534C82AB4658B163D53BEF8E2A993
      EFBCB3F8D8B061FB0F3DFCF0E93FDBB73FF94BC3863F2CF3F51D385114BD6669
      B5DE1B6262666FAB5EFD8FDFE3E2B680E663A0AD6506CA657E7EF57F080B5BB3
      292666C7A66AD5D67C1A1EDE344CA34161508A28C1066CEAAB8B75FCC7F66EDE
      3C7EDF4B2F2DDED2B6EDDE2D0D1B1EDD56AFDE9E757171DF7E6036F75BE4E9D9
      322532F29B9FA3A377FC1816B6E1337FFF96602057037AC3D333E03B3FBFB7D7
      0605FDB9213272DBAFB56BCFF8AE76EDC0184F4FFEF85061441158BC8EB671C5
      8A71F31B355AB03E31F1C7CD8989BB36C6C5ED5C1916B6EEF38080A56B4343B7
      AD0B0EDEBEDADF7FD1226FEF70338A1157360A423E7CE8E9F9C8D73E3E1BBF0D
      08D8F55352D28F27860D1BBEE3A597FCAC26132FFF961917358989E9F94978F8
      F0B5E1E16BD68587EF5C1D12B273156E1C58E5EFBFFB6BABF5E72FBCBD87BD6A
      36EB65E2C50F38842DB358DEF8DCD373DB5701013B7FEDDC79FDC5850B271C99
      3BB7DA330D1AF8F87879D5E8DFAFDFE091C3870FF5F5F6AE3FC1DB3B1AF29FF4
      959FDFAF5F5AADBB417437C2FEB1D46C7E77A6D1181358B4A3AD983E052A14CA
      F99E9EC98B4DA64F161B8D7F22F0AE75C9C95BF6F4EBB7E4CFE6CD9F7CC1DBBB
      4FFFE8E8C9FD50FC06AAD5FDE6EA7463169ACDCB96582C7F2E319B777F6C346E
      FFC8C363F91B0643EB00512CD72FC88C5EB65A751FFAF8B45D68302C59A8D36D
      FBC060D8B5C46ADD8142F03B64FACB5C4FCF9DB32C969DF38CC65FDE31187E7B
      DFC363C7FB3ADD6EF6FB9E4EF7F9BB06439769FC812653ABE4F1A29F9F6E8EC5
      D2688187C79CB7359A1F803FE66B34BBDED468F6CDD6688ECF02E6A9D5FBDE52
      AB77C37DFB3C8D66D3EB5AED5B6F190CCD9F351A79CAA712CAA59CFD21AE2946
      63F00CBDBEFB6B5AEDDCB92AD5AAD92AD52F2FAA54075F005E419B3F43A95CFF
      AC4AB578944A35EA71AD366182C954A6272B45AE72A32F6439D6C3C33A5EA3A9
      355AA5EAF6B04231A98742F15C1F517CAC9D28764810C59A1881F961CEA56299
      574EBAEC1BA45B01225A8C4FCC8037A819002D5A59AE4825BD62D95085B6FFEE
      2FBA75848F25320AD3600C3697C1ED0708CBBCBC4F4B114D7954719F24EE2A58
      B912306BD1B76BDC6EA93B7F8EE252BFF244302155C61152E146134DCFF7CF8F
      7AB8479977B0A0A4E1594A35EFD8FBAB31433B50C3A4086A543B8A1A329222A9
      415214D5AF1555A81799EB42AF03B7A49AD1B467FFC9F82D1B56A680D415A044
      15A680E55594392FBCF3A5347AF04354EBC53364362849AF53931A255EABC2DC
      3EA0C63423EED2240DA6CC3424C8FFE8F07C170F5AB66233CD1C37A01E28EF05
      F86CB2C46738CBE5810411D8EC0E32EA556431ABC9622A84D9A8C63F4FAAC864
      D4E0CF505564F050935EAF248D564936BB535E539309DFF6286450147B7E87B5
      1BB21538B162C531574357619E54754B0741A50657F6A801A44C01BDC086712E
      66BB387C098A6816322871E56C9528CFE6C0ACAE8238B00C34124A0D9830D4D0
      415481E5100596A315F8EE6006381B558ACA2D632103CE8322379ECAC92F0003
      1011554A30424C992813477E6800AD4E493AB869D90DF962733830F3280FBC8B
      A8402BA259C800F612854CC803038512C4C14005E463EEF0BA4B49A7F315B42B
      83E8D7AB12FD78DE416B4EE6D3EA6379E4401E80430989D286420645F2E2176E
      DC239A6F7361155D41B92E814EE70874215FA0EBB8FA3B53A1212CD911FEC315
      D389F8EB6344028BD4C4FE391C872F4111CDC2628A2078819C227AE6B58FA55A
      F5EAD09727D5649330B10A311032155F3B24203584CB4B253B8EE23870FF38F2
      CA6173D3F06A363AB0673FCD7B76F8AD620A82ACB83DE732EBE032CB0E12CA76
      2E027D35349C2437A692093F880D4F2A1403E224F0888475AC9FE2A369C5CFC7
      301B0FBBECA9E8C1798054288BAC255A7E4ED66FC70EEC6BF1C2DE3DA0878C43
      BD909B0DD6653220C40A4D04E282F96C91444C5CDBF3737F2B2152CA20943217
      1BA360B000B21A30767AFFD0F89AFD4F5CBC11D8A34D1D5AB3791FC5067B5F3E
      7FECD0575F2E7CF52BD9D3AD472A8C17811255118392976C183FFB23A9DB43CD
      C8C368906B2BBE2C29373B87D66EDC4A0BA68DAC327CA187227931C1DBF1D8C4
      99CB8C16EFFEC80674C79013E402E938B2336E7CF5D95B2F0CBEDD7F897D1142
      549407251E8A0C20321DC605406164608092806B4095EA7F41A75F5C3F645940
      A6F764472025F0E01547A4886A615B5464F99FD0AA4C012EDF518031468EA8C6
      682C6066259728FC8B86FCC5CF0E95E1D6FF801035ADCC13DC79C6B116F46600
      FB8B817EC75B7FF05E56C5220A866D1B6E7380564EA14192E7E5AAE14D438097
      7C23A057747F113B9741318334B8F605026FBFBFE8BEFF0704C45815FE0F88FF
      DADEB3FC0F347EC2EFEC27565FBFAFC31FF4FF80602DA6DFC8C6C1B1EFA7863F
      74C51863C5F2C9BFFF1F108E3E50F23F205AAD463ADE23F7A7BE3BACCD7EBEA6
      F5D2612ACE22783F98FF01C174013A1789FCB52EFBE9AE377EC811F4EA1DD971
      BE46657DDD7DFF0F083A3B248084952B57CEC4EEE35A38412760F64A5E36BC70
      F98AB0EBBA681C7122BE397BEA13F0CA161BED2B5008D94EBE475B2BEAC8DF19
      420DA596A474FC9FEAAE3D38CAAB8A9F6F5FD90D497637210F20243124842434
      AF4229E155422B4C5B7598005587EA8CD34E2BD00AA86DD1DA8A7574AAB54069
      AC547406E50F69CC583BB58A521E8E102C5891C084479A26D094844793B06137
      D9DDECFAFBDDFDBEED2E69C39A963FDCB9E7DEFBDD73CF39F7FDDDEFDEBBF758
      19250A9808873B3B3B43D0CAFD57F0DEAEEE75C47AA816AB0764524E7678866B
      F0EAA1AAA36FFC72EAB1FD6E6DE1B8FEA1FA2CE3FE226760BA6DAEEB4E6D343D
      20BAC4D1F580B852C7852AC705069667FA7AE63ACFF6ACCCD9B86F49FA4B87BB
      7CF3C6D7B72EBBE7E8156B2A8E5E99B923883DB6383D20D8AA5C0221113D20B8
      85F2D7D8A21A550FC8345BE170AEF5836B73D3DEBFFC9509431736BBF7ECDDD0
      BB60C196B7FAB277B77425777575098A470119E374732BDCB1E90169693F1EC6
      8AA3F7F1E09ED7BB4D19C93B8A37D4DDFD4EC3C1F1DD6FF582A932C6BD8E464F
      0E4201F98EB1DC5F943670DEFBB5966FFEE58D29ABE704B3A62563154B70B806
      EBBA7E0724E10A2BD8349F440F88756828587F72E3BE367B812BD77B7208BB84
      C3E409305C7875F349EE75C46019AE5AFA70232AF83B60570DF86833D6FB8B28
      40566C5E0FAE1C9D477FD78CE9FEA2077795C84BC1FB2020213396FB8BEC2A17
      DBC249094940A4FF5D0F08997356A783062689980211C1D2901C877B6343E67A
      ACD12B428F04A743C6F8D3D4DC3F41E23F6D727FC3EF0FF62D7DDCB323411231
      7AF20DE3BFBC4EBBE5B32B373D57B7EC7B5B1AD6686C8637A46184840594DF7A
      DB93D68C0A8B73F2026771E9D475244E041212B06DADA9AAA2F64B778BAF5324
      7845AAE77DF1FE86D589E522A14A2EAB9AF1586A4E418A7831BBD1AC9259706B
      6651C994B5C8C1B700A39A1BE6E0176BCD5515B72DBE577C17917A9F4860005A
      2C3C527DFB17566E5DA54D1C953B9037CC416969D93A6716CE58797B109DEF5F
      4C55037EC9CA2BC92E2ACC632E1E03E2634D3407488DE3E5B55AFE6F3624CF6B
      7A26F3ABFB5E2CFED9BF76DEFEE799B5752BE4DA05A4DC0B40EAFDD770891D5D
      8FD4CE5FFAD0915FCD786DCFA6C2671B9F767F79C7B76D73B63DAAE582179655
      2332B5579FC9F87AF9F4CA0752529CD929CEF4345C92E0B0D8B9668394E2E487
      0CF6A168F0650F1566583F0015330D5C18607389D8D0C1F1F52FF8FAF7FB3CC1
      214FAF77A0BFBBDFD37BA1FB3FAD47375B926DB65945F9F935583E410A3D6086
      D479426004C087A26860888A1519441898AA134370A92485C2431AC2015876B0
      85349CCBC39A554E7E5AB6D532F9D4996335DAA687A5A27C927BF3BC59B317DA
      D39DAA0241010322DCE2285C4E08074542045D28980973402186CB0523A0C5E2
      96818B9764DFF1E63FB6F57BBFABBDB0469C58D7282B9FE0FCC1FC19B3EE7450
      88BF1B8250D6641AC65B8F3921908972215FB94804734021263B8A6BA2783E18
      08BFD972F80FED577D3FB299A54D8D452F3EA239B1E453569A9DFAF4C2CAE2C5
      76ACD389BF175CA26D20F26DA3982258B9F854A6C061B8508A239A1D27A64CA1
      BF9D7DA7E9DDAB838AF99AADE101250024A20B299D9699FCD4A28ACF2CC13949
      4D150B9104F0C1043622888CF94C5781C835683CDE7DB6ABB1C333F82C534EE6
      248B0AE00385603DB0A43227F5C777DD32B12EC20D18A6980C15C082C1C205EA
      051E98502024AF9FE979F54CBFEF29AC13BE6B300725A6B224A64F87E757698E
      E274C796CF55E63E881392084539F3FB0F8CE8A85C30D57CA6CB8A4531FDFEF4
      859F9CF7FA9F5CD7100E80286AD006A37EE559FFF3B0AFF987AE3C019140E113
      92A9C223CCE165826299EBCF1949D6BC653FC51218A2C49A110236ADD2D2EF2D
      185F2401B41E0A616C9D09BD4A1053CE30A69E2E56605C6673FEF3AB35DBFA86
      B05FC5D3AD11026C2699E0B29AC78B1FD45C6C2503FCD7437D4FB0C72218CB5C
      C818B2C1D60347F0DE828089261137F872D0821331230440DF53418A661A2768
      B762C2E06671C8B02F28E77AFBFA8FF5F7FF1DB79EDA2AD35C7326395D2966A4
      46FCE8F941BFB8355316AE822804DBD105A4984DC5E8325834C518336C977317
      2E7B8E5DB974B0C33FD88855CE136060EEF4F5D414F65E5D5EE1CA9C99EB4A4F
      16B35752A4D78E7F0F4C05BE191035237290613257330317AF0C79DFBED479A4
      6DD0DB18D0E4105690DF477E54EFC348D17E2AE03BDC71F1DC8292BE4BCBAB9D
      9935E916BB2D55B4922867DD13D70F5E7844B3B94DE687B2AD49F7B4FBBC7BBC
      9A1CB042DF201AEA15B4EDB8CA439F71A0F8330261999C1A96BB0AAC49B5EF0D
      07767A24B4EBD1AD1F36D5380120B2A20ED3D13A532C9A784D22BD60CC61544F
      CF480734E340E3048D033403A0B90C1A34C1485CAD4FFA22BE9B6443E04DE2AC
      B3BDE90246B4225DB06C6F6A471D1A4F11F781FA42D477C47F3D3E16178911B1
      47083008572C18D1E220F47454E8F5F8ED4D11DCF582E28A88CC4948A0EEA748
      1AE2EDFB1795A980583CFDA42190878AA05BE627E409E5258211B88545D5581C
      82A027076F652CF8A3605E39705AEAE74EC5181892B2BC0CF9DD8153523D250B
      47D5810407FC274D48370DB8DFCE6AFB7ECD2BEE8D08C6DE096C8339BC7126C0
      C10E21BBF64798C31B3514B6736FABDC7747094E94C7158430A12C321657B40E
      ACD8032035074CBA664D14A1CF1FED33E2C1EB8E38422AA64E7483E861561B66
      3BE86D7C36F8D04FD09E6B7A3BFCF9D945F48B0FAF3EEC632A3FB34CCFEE23ED
      B2744E31BD02FD6750ED1AA967C85761BB8FB6CBE29985D170EC698A433F7DFF
      5A731B067215EDE65916B21ED4CB65086F3123656613666F4402B8CB040739FC
      B0B81C983A308CC00611C4BFEB941F513483090254112DAA2E10FE94A0480948
      929ECDBDFFEE903BAAF2899600CA5B79605931B2C191FDC73AA50EF443285E3E
      E3ED29D057A7BC6F8236BEFA55F0A76BA9D194CDB4767A9EE20C4D79CA659BE6
      E61B141ACAC113E764F6F4C92A3CD66A3E715E6697E7E2156DC2F65A58A1D877
      E839049AB8666A520AF8D0DC58A088C116039D80980685F08477BC5EC6EA21C6
      62710F05B173A817BC89013178554494F48F96F331C1F1DE5AA4F29FAD5D7181
      7C66785CA0FE405EE4C94725801E061081772FBA3C26CAE86938E3A7A641CC3C
      8B824C1997EEFCCA7C219EC09C1A74E4415E8C475075408F01AC0FFAC9906E2C
      349F7C2FFA783DDEC0C53267640BAD58302218823E0AC7B0EBF1061D71B13042
      8081FC388244F146BC681D18019FB6FBFF2FE0BF236CF43CC87636A600000000
      49454E4400000000}
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
      4100000089504E470D0A1A0A0000000D49484452000000120000049208060000
      00BC31BE9C00009A25494441547801ECBD757C1457FB3E7CCFCCBAEFC636EE1E
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
      B3FA2EFE6E429CFEFF69EF2D00A33ABAF6F17357B371571208EEB6B8266870A7
      D022C58BB55028546809054A5BA44281066DD1A2351C0A2952A4C1DD2140B024
      84105DBBFFE7CC66978D1168FBFEBE7EDFFF4DE6193973E6D999B973CF9D6B73
      ED8A5CB030DC6DE5AE08D9996AD9E6418DF44DBD36FA75AFEB2F570C315C933B
      A8166E3A9B3973C6A4568595CD274F9C5C4B34FD4CFB6A0DEE74F4BC7B7EF35C
      39F6CFD3F2ABBD7A5EC0F135345F81E70966447F2466721F0E1DD867C2BB9352
      5F79A5E7893265CACCC4A1DE256F39058EBA5E79858E6997466D143D7BF5F285
      AC29508288F87A00512E0BB974C91B5F7D3D77CBACAFE69EF8E4F39987FE9285
      0C8485CC76F5FADAB75AED5A3EB59AE2FDB166A1EE359BBD6AD0BC84854C8085
      740E0BAFDFA069E39BD5CB866794F47533E11B224F9D024B5C77ADDBF151C00B
      5948274DF74CAD4B64497DBD9BDEFEBE7E7821DE29D5C5CB64F4F2A34C673755
      86BB4FBA9BBE4DD116F2EEA3E4CE5270A954B7B85817F5824F34CECB3F73F1FB
      EE53AF62AB3EF729B16AB64FC0D219FEB24293EE531E16F2F8B148742E156421
      A5993367ED5344BC12DE66DB2C5D852BDB7D488B2D999A4258A00648A2B397CC
      B4E1EB3FEF4A99F73C7FF9ECED19C72F5C9DCE64F9F0E9A79FED9EBAEFCABDD8
      E889F71EB52C698A6F5BDE7823B284E972BD20E385CAEEA65F4B781B3EF9E964
      C2878B3766960A7A8E859C040BA9ADD16A84D22BF89A9498E06C51908C9BE792
      0191649CFAFA386B34DEC54B2A6E6E5FA2FB7AFA732C2465A62C355C3B69CA94
      B4CE29BEE1D9695E61960CEF5073967788C5BF7898C2BD4C6553368CDBF9FD2F
      61219DAAB7B967D0B866AAC8A2D469D44AAD5AA334DEBBE466B9B8D71577DA5F
      DC423EDAB6C040677706C8F1A73C332F1D72BBB363312CE4BB09DE7FD542D6FF
      AF85FCDF6521F3ED4279056C21DDFC03D8E436465E09B2594844FE5D4EB25527
      6ADA5699E32BC37B735024FADC582374B64F6A2B38542205CF62C6F78D4A5627
      D752D8E5912ECA79592A51D2F5937635C1C6A9161F6E907D4A55A72F3A97E264
      9118FBD3354ABA7692764FB54E99ED44CDDE5B2DFBA046DFB4BD5F24092B8CDA
      1A286AB467C6AB8243789C11F1CEF7B2777835FAF2956A9C2C1263D69EA2E41B
      A72876667FC1213C2ED564DC52D927BC2A2DE89CC6C92231FC27574ABA719AF6
      CD1E283884C7A51A8F5D8C1A55A12F7AD7E6649118BBE64FD4E80CEDFF62B0E0
      101E976AF4560C88AA72F485918C1A1DF86A98E0101E976C387A0188AA7094D2
      1EC68B30AF175CAC985D946A50881A1D9C3B5C70088F731B8CFC4610A527DEA5
      951F74605181E833FD572167D21B174ED31FF346090EFB80C4E566BC866E9DED
      1A2D161A19739C3292EFD1B2F7DA898205795CC626B713E1CD763C2E6525CA06
      914D89E336E57C21F46C32512D4ED41B3647F60CAD40D9698FE98BF1AD68ECAC
      9D2C167111C9F16C726EDAB533717438E66DC1213CE8F8D61D3A7B34C28F008A
      7EBBF0E644CFD9C22A761C5938CE0F89443B1112ECF8BD060E5F06C9504E04FE
      1967AB512E364C4843216801D407C200763CB80E21B27BF1E2C5B711E6772828
      08070F1EA4183478F0FBC06DE0DEE041037F1836A0CF270C8EB30CE0BCF75997
      996C65392E0005B74183876C1B3268C083A103FBF545616DFFA1A3F86A9600C7
      59C679ACC3BA5C461486278191EF0A3B21BE416DCEF079A20B6B73DD3B22FBAA
      6FEB212A32F6234959017998DC9B2F9848BDBC74E28E45259363B51E99F1DB8C
      4AE724E4B16533D89AF481C2601A7C35B065ADFBEE55BDEFBB5559AD20AA8555
      E028DB040E686B95447C5FD2421417F8F4CCAB81A9A7934BDFDF1567D1A816E3
      2ED95CD5E0C1834BE08ECCD0F4408F41F15E7552135DCAFCAC96E55A5926CA2E
      E129A97B5724C91BF55D7781E43FEFC94610D64A70AFB2CCA0748A0CA2A3439C
      923296E04EEA4A65CD9A357BE0EE6ED9E57317BC6FEEFCF5403C9C3E32C32865
      97F39134B1AF92A27131A2121EB845599E14B1F192F2F65332384972F85327DF
      F83F27B5DD5043AFEF2D1125A305D4586931FD86DA93C664EA85C58071589254
      582242FAEE0CC98D579134682B497E5881ABA22F49268BA4C233FEB216BA5C26
      A76C0413F94AB2E9060B65A2F2B8EB2461595BE9660AC963B693D42894E8F348
      22DC51976E3D21592991845E935897CBA0EC3584812A78B91CDA4BB8E74F5AE4
      38A16F26D4210A74C52DE7B5448713706B5A0D1AB0E42A8404D728519654E188
      F34F5DC41660351964585E8EE40F0F100DDD4E74F00E16BDC3B714A124A34F65
      89E8229741D95208EF33D17EB342D51C0932A8543F90853F0222F34704F976BC
      D43C0CFD544C7CF5028FF7E352AC2CE3BAB44512BA2894533696B75A32B24757
      6CD9E8EC6365E99FD3B4BECD3042C3D144ACBE272946D424C905CD59759E64B5
      0277DF88B426A5E2A05FFA95B16DF452337586F135E87EA4C4CDA814BDBE868B
      F64966B459E5F43DFA72E7536D007656294C81C4CA7324AD394F12EEF32325A9
      D0ACB8A0A767FA853C396E09BC7F7E336E282D4277FCFEDC5D4429631751E4EC
      2216F305B354F82E82565ADDA0C183FFD64E2B58B09B602310B16900615E3332
      1D7BFDF4A2CC8820106C0E1E88B1635033881A03E100BB1BF0F6037B162F5E7C
      07612EA7CA957A9690105502326002D8719C659CC7E9FC400D4466214DFBDF69
      21B36C4DFA5B1672F1E225D395E81F5848E99B4C7FB7C1E7BDBA5C7EE056E147
      15C9F56062F109164975B41F2E6B55275A729A083B60B65AA2B0274E01358D4A
      DD427FD5E9D3AA0CD3C77A7DCD8D0A747D4B0CFBFBAB3EFF7AF723D732FD35B2
      A5618691B22117AE9427C93E784C4624E065C004AB2D9686ACCB65B82CC42D99
      A8400B894C61F86D1612EB41F2E647AD5ED242E2917B0D937D87F9045B488E33
      0AB3905C23CEB74342F7C3A8115B4816F21104B69AA3FC6CBF864D0A8E3AB03C
      4264F798E8B916D2AE8988E63916927711B69083A1272CA4DA64692E493858C8
      924214E40C806D13AA817E82859465ADA385C4C58B98222D64DB527882EC3151
      111672D23F6221312037FF5316320BADB7BA41FFE72CA4B55DFF80AF648E56EF
      1E5A57A3CDF0F72AB71C3AB1628B21A32AB518F666E5E643DE2CDD74C8A0E0BA
      FD3421153B9FBA7DEC079BC9E522F9C0239BDC5C9565D7BFAF779B3BAA06CD1B
      55130F885657CE1E5E4DF9ED98EAEEAD9A951B131416F64DA3377E75CE57DA41
      20881492E49C9C212BAFE0CEF2B5240B5D7A68A693778CE48A1DAE51794F6AD2
      28B4998BAFC7048772F9A28208E39E671EA453F13C51222D7671278D821EA664
      52795C021E50CF8342035DFAE42BED2010449CE6BD5E8D1E5361EF573111D696
      DD7B293D7CDD9F89E14BF73D08AF5236A0D4C865A7D2472D3999D16F5EDCE5C8
      77F77FD470E8CF3A2ECB1044A811A63D78C146A1C0A3780A52E3213FAE1556E6
      A6E19872F6AA1F44BD810FBB55729ED8B5A2EEB3BE55CBA0EFA6A0EFD6A1EF30
      0D23B21281C9C2B4824EC67B4468226A6436CBF43843A6E7F45D7BF4DD0C2E9A
      4384057A914259511B6B8D94B01A78C5054D7D5EDFB9E0912614B5D608F331AC
      D16BE2743E14D477D79225DA7F3D9B9E64CAA4E65F4729512349562AB34DF92F
      1CA0C5A2B158992357DFE9306B77C3CC0B8FC7885A83C75A23A3213B5DAD7023
      0F278C1D8D913C9D4CE4E3821A82A9A0BE735263CD363C000B4B4A6CBF990823
      07073EB39CD06F7A9C9FC1441E98FC66622A873C99DCB0B0C780262198DD61AB
      E0F1523E286064101EDCC2D6C0353194C61D1DE812214AB4EDD3FAAD452A8FD7
      E5E338A3D962820E5C9E3C4E62C968BC448AAD8184E82384053AF49DAAA0BEB3
      29731E1631164C05FF548E6606BE4482BEF3E6BEE3BEC03E89639B35934B2B15
      EE384B301958C2690E0B44E4DB077EC75B4F65309A02D177E9B6BE63659E2BA3
      69691E2E1ADDA68FEBA06F59FA0FA0B01AF981FB1150A4C35673819201FB3B82
      3C0EAF48555AB468C93BFDFBBFDEBA5FBFBEADF1345414C2A85EBD7A47619185
      A8CE9DBB46B56BD7B155BB769DDA604DBB207777B7130512E1CDC34A9F7C3273
      5E9932E5EA878484D60F0A2E562F28A858BDC0C062F58A170F172851A264FD32
      65CAD6C3835FA9AFBFDE6F439EBA608FB04ADAFDF6DBE18C79F3161B860D1B96
      FDCEF80986B7DF1E679838F15D03DE2B30B8BB7B18C2C28A671C3972D63076EC
      BBDF7391026B84CD5CB66F9F41AF3E7DFA5855B1623965C3460D9515CA975356
      AB5E55F9DD77CB9478DF0FBB9F927AF5EAA73E7BF6F4E9C3870FFE9877404ACC
      6E5160D56F9849AD934E0A0A089043C3C228B47818DEA60BA540BC1A121A1A4A
      DEDEDE42D73A24B00B71411BAEE544E6992DAD744B639459FC9E229E85E45F93
      B0AFE11D02C2FB917872C59FF07622B461C730A010B1EEFD1C6194C2A8E3B086
      5A5DBFE4A304C97DF9328B0AB552AAD530784ABA7BF72EDECD3690AFAF1FE1B5
      09ECF982888B9022053BEE6FD8918B03F29A354EF2CC99EE06BC63A4092F2137
      50E3E3800B17CA3CC555C08EDFBF779F060E1A88B37589F0953841C4BB0E33A9
      4E124DF175771FBB252888AE4D9A44E8682A5EBAB4C670F3A6A40B29A6549E39
      23A57EFC31F94C9B8E87BD2DB47BE74E2A59B224A53C7EC2E5EDFB9EE21191AF
      362848E7D7B6ADCEAD654B9D5BEBD63AF7D6AD95FC6541BA785172C2BBA0EEFB
      F751DA9468C2534154A16245AA54B1125EF774438D984B668FA44D440BCA797B
      0FD6952E2D2933324889F7425559590A95D92C4906838CEF1B4A1AD4FF5E4808
      29D6ADA33FE2E2084B3ED08183074DCD9A7554FDF8E3DA9573E67CDA578503BA
      B3DBD3A72AA773E7480D225C60211859ACE02C36A9847CF986A7275D1A3182AA
      E0E3801E6E6EE41F1040AEAE6E5C15802F4AA0A3D147070F1B8DFEE8604F25DE
      BCC7C5074B27A22A6524C903A4967B6161CA336FBE49C51A37A28777E2C90F24
      41E84F3CCD89375CD5BCD40AC8ACA3C9138DF447AFB955C155CC3344CE7168AE
      9EA8F2459D4EDAD0A30779356B662E8E5AB97B7B517070083EABA9316FFE758B
      A24A955AEA8D1BD7AEFBE4938FFB83239F0B3DAED39DBAA5D5CA4D94CA1DC81D
      0B7405BA001C7640D8127803F818180D9403F239C5306FEF11DDDCDDA723A70C
      F03C872E2475610AE82AF18176F54A2727DE9FF808C4B6DD11ACC3E07C0189CD
      41618C2F2A8F8FBFE5CFBF423B766C17F39C547CC212EB42D9CB5FBF7E5D8C62
      BB204F044FB2528F1E3D7140251F41C4F94E235B93132237E9D99F02D19BF49C
      BF19EB6D99EE7622963C193A05AF0397E4A8C01DBCF1ECEFCFC70191CCE5A57E
      3828575A912BF53712FF1851AEA6792C9C4C890EB5E23E4B75483F2F6A274AFA
      7C132FD64A75EAD4B1EBE3796362FB6C17E4893CBC70C12EB113D9257922690B
      A650F60EFBD6B1E76A5BF7208AE8694F17D9474C12FAE3290ADB7155140ADB7C
      9E8297ECCE475E24119796F0B15B854A4DC5D6FD896FD5BA902A2098C5B9209A
      C62318CF63E3981508EB7AD1AE8097BC89F7CAF8D6A5896BA5F2F12753D243BA
      D3B3B6D0B979F326D9FE448DD898E3363C6CB04C58F6CA0ECC01841E3747E9EE
      891700EE1393856DBB6C95E3C02922F0448D103ED7C946239E00C8A63BBDEA12
      939A1E24E4D32F9248DBAA07DDEE52CD5E30BE7D451167B902E72E22014F10F1
      5E7FEFDE3DC21399C4713C102F561BE0F58DCD6D70D9BF6D7FF12A3EE64DC4FD
      8672C23DBA7F5F84EC09A21E5653C0E9BF0A31AF7745E912445409A8F292A80C
      7D2E5742050BF91089BFE5FE3316B2C217E9A8951257E58E21747479D3CFF28E
      0CB2C7735BC87E517A6A5BC69E49A71E10550B7896768CF59A9BFB07C4C87654
      F8ABF17F8C488C235B2D966F3F46CBB7DB522F17DA89E286E29D47CC8DEABC84
      85C4B7B3ECBF6627B24BF24446ED76432D73772CABF0861919C2312B8A24E2E6
      5A55AD7E9CBE9688D4DA1E47239F6D7E7AA9CE9663F2D74CB0C213357A9E8584
      8E703612FDD0189286611A06E94B5B481B09CADA49388EA7C7381078A1A649D2
      30A16CAB8948E4F18A24E2AD4343E372D5843958FE5216725C29338D1FA529C0
      421EC383F18F98534074768FFF5A48D1190E5E8173C80AFB2A58554E5A8317F1
      8F543F6253CB6D21470E2D4955A9852D934ED3EE5C697B0622C3162E84FFCC15
      398E9EA93E3FF68F11A91C7F67DEC2EB48E6AE3251DE34540A7076A2387DDC4B
      CF215FCA42BEE7569DAC35CD5D0DDE30FD688D5D58641FD94886EAF5240F9589
      432E6D93739C5124112BD90A739C9137CD32D1474559C818F41F2B336CF185F8
      FCAFA38514443C87C49B86A2B3790EC90518972F5FE680861DB31AFC79619B69
      647C7B2163EFA52DE442BCD9C305557E811C902D2D12395E917DC45B8775B9B0
      B450B293B0DCD1428A53D1F5EBD7E96C7348BC0D855BFE163187E4659CF81C9F
      AFC9E2054ACA3B87C4CB953462C4C84C4CB4EA8A3EFA5759486E7E3EE03AF54B
      AF2F9EAFB3B17C1356E129DB1BAF7B4E1B3A64C857B88FFB55BB76ED3EC6A66E
      8FCE75CBF7AB39825C445830E92FAD2FCE5CB988787DF1B6EDDAF57AD9F5C5F3
      11E1B4F32FAD2F9E8F08D784DCFFCAFAE24CA464CF065CA4D4376EDCB83E3A56
      872DC762BE80C2E173D71767059B22C77149D0B7D598B1633FC7F5D94AA81D2E
      A32A44BE65F72A52EC7966C484F2332F5A9AB1658A50B4C9B00B14B8BEF8936B
      E72D5ECBDECD557B5B19847340342E1711845898CCA344E3468DDE19356AD42B
      4D23223CB18819EDDAB9EBF1EFCBBE5AFD4565973759C7017C38ED02A29BF988
      B0832AB14850132CEE168D66D6C3E7C1E4B95F7F7D70DDFAF5E31E0CAA9D0992
      8B80CD9D04490D4EE4236221CE24BD6BD4A8F1E6D4A95347E18CD23467CE9C2F
      0E1D3AF419E7C9EFB52B87D046C6B5F909E9425DBEF5C51D35990C98EC282B34
      8E97B8FFBBBE78A1BD43340B6B45BCDCFAE2CF21FB1FCB2A70403AD666E9E2E5
      E3B10F56A13C9AB841711F973E260D1AD2DFC8FAE270C491C2001BDE3C22B271
      94D6496357E15B1BBFC7EEBF9CF8288907A595E8A71F375FB06B14126112187E
      7B2E56692425967DC59D2CBB8C6B8445E1CA8B351FED524464DC3845D5C9C949
      4B26A30992678EEFA998CC66DCED93ED42158E1CB8DFEF4A59D95976214758D9
      D62F7CD98C65A206286BC2BB32B8A9200EED2C6748EBD76E92CB962D2DA6344F
      4C1ABA9EE58E6AE366805221EE91E04E0FF8F8313F1297CE2C16DC54C005A8DD
      092E9398C00665B7AEDDA3F9F64D4696812E667B12150B2197007C0ACCC38D94
      76B822EE4A0A77AC5FE48963A4BB2B8587689A952EAE69E6EB2E357B906CAAA1
      ECDAA57BB48F8F372565125D970270F5D89D5478C44885CE54E3EE3096AB16F7
      8D38CEDDC007052596D5C10A0FA4C505F8DBF7B3E8FEA3ECFED2AA153FC865CA
      96A17D096A5286879397872B990CA8BF6C4113F1E027C60FF71702DCF29101C2
      3D13ACCF839BE16958E9F3D48594D8E427A657A515CBD7C8BEC5CBD3EF4F3CC8
      2BBC18D610C5D3B40623FAC3883B1CD6FB1DB67EE0104F03E006B986346A996E
      C62767DCBC9536C6CB45BD585AB474959CED5F8DAE6983C8CDDB9332D044135E
      67E0DFE5827981EFFF9256A322B33193CE9C4EF83933CB3C20765CF9C7D2942F
      57C88F43EA91535831FCBC13860128D0B2BC04B634BA8D744E4437AE2524DFB8
      9E3CFAF07B5556739ED47FC626D910AEA790E2BE946D90F0213216170C091D85
      2D4F468CB97367EE1E4A7D92D5EACCB4DA69AC2D357A7FE7F7C99207F17D3516
      60BC715020C023E4B8B9C58371FE8519F58E08C1FF6D4F343B0EBB9118347FA5
      ADE8B05AD8DE6C461048A48F8DA5BFF2772C2202D5C06D56C21FD748BF670FD1
      CB9281E458B366F4AC4620C3F6A4E70E22D6C90BB62939326BD338018B6727AA
      5A95A86F5FA25F7F25DAB78F73899A34217CE19D68C50AA2D3A7AD322E638D91
      2227CC1D30094B3A7460DF0A5BDC966795DA7DFB56D36FDD4AB47DBB35A3450B
      EBAF5B53B97DAEE5EEDD565954141D6BDB56F4D133A2CD9B89B66CB12AD8FCF9
      F36D316B887BB5D6488EDFAE1D1D6BDF5E10A97244B8436B2EBAB38DE210662F
      82C57DEDF16735FAF147A29F7EB26674EBF6FCA66DDC68D5EBDC998E75E9F29C
      1AD93A96D55F7B8D7DA255ABAC21E7FDF083355EE456E34DCCAADCB11C326C71
      5B1ECB1CF0AC69B8734EEBD73B64BD40B4470F3AD6B3E7739AF6021C42C5A169
      CFB61A0FF7EEDD45FE0B7B5C2647D94E74CCD6A93919FF3F0CCE776A73C9B1D9
      B6B4D25158549C0B59F0D4C8804A55262E1A73BEE12ED79BCB134CB2AE4F9932
      6F8B715414813D3F462EBE2D76D075999F468130393B1B3E519F96DFD38B13C5
      C81551EA1C40BFEDE997E16E4C4F515B8CD9D5DBFC3218B2F22F4614236BA17C
      07F0DDB7FBD51439F5FE433F7E99040276157FDE56AE600BC9B9B9F12D92BEBB
      F7F4CF6012C4890B73F80837657E6F1D79A968A218994F355F27FCB568F67D08
      026ABA636F390E6D649C2EBA6931F24514E282FD6998B41CF102DDF38962E47E
      28F53D700924E51116EA1485E6C4C89EC8631204D49BBDE7A17022A2753905BF
      436D4EE4C40B0D0A268A9167A3444B2011780328D2F1C73747408B3BF43AC24A
      C048A00DC0AE126A739E234541058579405E970C414D90DC42F8428E9BF62634
      970227814D00D726F8654850E6BFEE057A407A019DDC2A31B21A823A4057A018
      7001B8F472443172100AAD053C805CCE91488B1C57808704E63862D116CEE7B8
      825E9D1F4E35BB2F433EBE109E7684926E6D228DEE1C85D7F583AC04E5FCF141
      601AE2C381BEC03080C35E088790CA6900BDBAE03C759F7D871A0DD90819E7F1
      1EC165B82C4456E78C6028C0AE227B4025C07A0C1FBDED5B1AB2F606BE44F42D
      64ECAAB107F00F3A23B43B17C4FA03D581190037AF31C2461452752A8DFDED2A
      F55D144F9EC598B82CE48B0176DC021D476CE0C4F09C44BF9C9083B63427690B
      6A728AEAF7FF1C821EC0C7C02280090720E44A20B03A670443007FE05380D31F
      509B0FDEA3E937CED3E8ADFB21AB0D0C04586716426F80CBB02EA256C7ACCCEE
      83645F804433FA2C8CA7A9572E50AB096D21EB01D85C09B2FE0D43E00CD89D12
      B14F804100F7C320EAF2E96EEAB7F416B59EF833647D809E00D7602842FE31EE
      8A4F105702B99C0EA90020903E3A138DCF579FA719B7FEA062D543200B000201
      7FC00FE090C16590C409163AB227623789E80E500AE8053402D875815DBACE91
      A2A082C207405EF704825740720FE10B3926E2AD540EDA15803B005BC9A32031
      22FE5FF76FE881E66B9B4700D194F3A7C8095F2A00410411ED052603C249C27F
      092F6275440481A475546BDAB17D07C5BE1A2B3884878C17728D96378A2090B4
      6BDF4E3C23C14407FA1D101CC24326D55D52379A7009E1C8A023B154C01FF223
      08249DBB74C6071B3211C53ADAA811F40587F0585AF3DB9A328740E4F1378EC7
      92C31FF22208243D7AF620DB5A6C3B77EC8488ECBA76A2CA5F57965FEBF31AAD
      5AB94A289C7DF36C2CE10FF2080209E7E1997E44B1AED4CE5D1C46DA74386127
      2A37BB9CDCBC4573F144E677CBBE23FC4502ECF6BE3EE075BA7DFB36C7E9B7DD
      BF71187969DCA55872F8936CF1F019E172B3E6CD44B274E9D2B43066A1880F1D
      36547C7687137B7EDBC341E48DF76EC4529E3F3B51B18F8B45109A80952B1110
      55AC545184E7CF9D17E1DEBD7B398CBCF3D19D582AE0CF4EC479011F06449003
      19E2C2D9481E4C7D104B85FCE522621D9FF77C22C881CC4692342329965EF6CF
      6B82570420E72082FEFBF7423D609694AA0C52B838288F46BC0AC02EDF4663E1
      73E143A4DDAD73FE684848B17B506C0FB0C3F5780E5E00991AA7E0372D729BD1
      2AD5FC32356AF6BBEAE5E5BAABC13BE1F4E6D6506AFB41B0E20538848A939777
      1F7F4FCFB57F9A8C0D4F0506CB1A3CB04B5E7E21C8EC420AE5C417266AF0EEEE
      1D35179CBC93DA71F493CA0A923C5D9D6106664F07D1107C5C8C27138816E578
      4A1C23C7629E700A1837A17295A88E254BC928D61360F782158A913F9662E4D3
      343B71094A6900764CF42A2E162B91C03B76F09FEB62E4F2C89F04E0E537E79E
      F82C66A6129F2594487A1F4C1B203F0398152F303E3E8322BB499858344664BE
      5996632D58C402F199C06AA03F60753E058D8F79D99DD027DC2F3F5BB5F8721A
      7542BC295013A807B40522143C3E1E48CA9E9795AA5D8DAA569B52A678F14064
      E840801B45AAA988B39BC81E100730E9EF088F038781AD40ACC2363E8E984D8D
      2FFBF9A63BB9BB9B29C01F793493240EE86734E9A2883DC753EC1DD0EFDB891B
      D60FA85FBDC6FA326AB5CEDDD55549F546744099FAC063C0562B440B778A66E1
      33A23EBF1A79DE6BF8F1315D5515276E3A8E1A7B84711FE0B1D6B441A88DB1F0
      E2CF72945287E8359499DA19A23E57CBB4AC7A69C55B0154B1D57DF2088EA2F1
      FEBC639A9157A453C9443348E75E0E9A15C890710F773C2BCB1BC64FA5352379
      3E09F18B396B7712F1266C8F22D501EE9BDB080F003B80EF811776B5A0D90968
      0AE41A1F48FF634E021303C15F70537E4F2AB0708777ADF3A197A79465851C37
      4255544165410AB8B1EBECD1E895C6DD864FEDDAE878F22B1BAED46C5FB1C980
      7AC1159A07B5ECD0EFF1A53D6BD2B271A3C756F6623FE27793ACC92E5363A4BB
      174F294B35ED50DBCDABD828A5AC6D6031A84AA4A4126E9F1A28F9DE4352990D
      4F5DB4CA8B9231F5FBA32B5F5FD1BE534F43A95ADDDADE7E906A7D76C64A45D4
      F183F96E0A8D5354D2E3F4B21E64C97892217B2A5C82AA65A803F54FB33421E6
      CC2CAC8E60242C4A91E5E5645E101EEA979492A97CC3FCE4CA3C2972CADCEE9E
      251AB47076F1CCF076A17386A4846BD542DC6E8F88ACFE889A36CDA0DF7F770F
      6930A8B1EC5BBDAF2EB85C1B854AA79333B076A36C32A8555A59A9502B7CE44B
      23A4E009CBA63B795419A976F6F4D0B92AB2555A658ACE59F1005FCDB9EB96FE
      E0C0BA5EB556A0C6B71B8CDE1892F8F0010A547CD3424A6739239B64A3A4F0D2
      69D2FDE4ABA3A562A3BE9FA5742EDF5756BBF85AB4B0C02EF806A48B9A54A6FB
      F7AFEDDBF1F5C85EBDBE99D7B5ECD357BEBDAF70525C1B7AE2B1D3D4B40CD95B
      919989EFCCCA0A0FB52ACBDF7079A0D2A35ACF1E0A8B4F75094D974C260B6C2F
      A98C4FEEC937FFF8F0DA9271F3BB57F4CB1A32F307BF27EEC689D7DC8B7F98AA
      D478915625915A89378995B293932253917A6DBD546CC8AA256A2AD34DB6A8DD
      65152C865625AB5DA5CC606FCBCCB5E3AA7F152429532AF5FFB494C2DB6BF023
      954B25A5935B8852E71EA89034F838A4D6C54BA578129C75BD87143670E36AA5
      A2782773D623252F03AFD4FA3B91562D3B7B4859CE3A695BE56079F17783AB6D
      0B0AD42B9EDE3F8675C2EBF828FC434294DE21A14A8FC02A1E5EDE1A5FD3BD6F
      A5E203D76EC5B30AC5321F1C5D61349AC35CFC9BF755E9423C2C0A35B9F938E3
      BD7EDDFD301F3AE5E7A13EA5D34A77521FDFD6DEB97EF1FAEA49FD363911A9B3
      F042F27B37E46C29B8FBC851C6A4845B7DF76CDA3A4727B97A54EF33D02DA059
      7795734815A59387B35F90973234484DDE6E66A36C494D487D746D5776D2C325
      9B3E1B7502B7A431C0B14D731C88736239815B489532C5EABDD1B75CDBCF66D4
      EFF3CDD7ED47C5CCEC3A72C6B8DA513D1A41C513C06D7C59C9E1CB40FD32CAFF
      195DCF322D147527DCC00C03BD207E02219E026BDC75A0C2DBD7BF403B25D4FE
      23DE32A2E0A34AF51BC06C06A75FE48750FD676A28F8863796CD52358BFC4A5B
      AFCE5B0A27F55BD57DBCEFAE54297F6E4554FC9966FE989D884992CDC6E6A522
      235F2BDDB9931452AC981C56A204859A8C722D57D78EBD24695A2F221F4C2ED4
      AAF9C6460867036B8168A0B7180BCBD01C8CB6378A376EDC237CE448F9C1B4A9
      CAEBFB0F2834152A28DC6AD532BBDFBE2D05BAE8AAE1E38765F7341C38DAE8E4
      D61903A9146C852FEA560168248C7A25A5BAA354A756E7B0E1C30DF7E7CC519F
      BA76FD4A298562D9933B77DAF98E1EDD50E3E161F25DB756EAE9AAE97C7EDDDB
      B4A5F1B035772DE6D57839FAA2B164047A83ACCB3483B18CB65225A5D2D95991
      7CEE9C7449A15857DD6299F17A7A7ABFA33D7A1CCDAA575FA56E15A5F474F3A0
      610DBDA81EFD403F7D1179D8F859E4554C32F812D17CD1B4210A656B55C50AF5
      3DCA96A5B4CD9B15CACC4CAFF69274E1DBB4B413DF5CB9B275C7E42FFA87B66F
      E17CA3636B39AB6D94A58ABF7FD54D554A97D2FAB91EBC73E6CA5354C47A77FD
      9CD938DB9492A2C0238B4A9DB797A99C4455746AF53731448D3CF6940E18D57E
      81EF72BCFA7237C84FD2992D665757572A969A5AF6F0EAADCE4CC21035C25CEE
      698FCB974335414135BD2223A5ECD85873905613986130D6DD55BAF1EB89A1D5
      3C4E0784BF5D31799F9B52924A1C3AF2479CC1A018B47FDFC1734C920FDB9D74
      736E4D992227467F64BCA55098AEF9FBC95F36EF22EBA28FC4B3F2D419EF967E
      FFA33131ED3AB4ACC5E92E3D23250E73612D1E829A0D23F5A39BEB9CF84F3F93
      1F8E1F67FCF3AB9972ECB17DF2F4AF675D1930B47764AE0279120A5BDA0B24A5
      7118D63E4D9B70FF834973AE8706AAD2DAB4264F670FB96E950AA5B52AF5E84F
      3EFF40D7B3772755D7EEED255BB902C37DB82E3977783FA57A7EE697ED3EF85E
      FE69F35A79EFFE1DF2A7B3A28F8E1E33B02E0A2980029D0AC37B04722E02D79B
      88D7F3E451E08BDA16F61A55BD3067BDABE54CE905F3170DBD73EBC149E814EA
      54C8990738389C65C896C716495963C63B136E2183B7ACB9273E4EB86ECD4F85
      4E4CF926D4682857076A02D78145C05E8CD86C84FFEF9D94F727F116A29C23CB
      CC090B0DE2E36F39DB32B98F6C7111F28BA62B562CE7377D75FC4602BF4C2032
      E0F13B34F816126244AD5B47E5FAA17C44595959ACA093861D1305886C2127B9
      DFD3E9C258174EE4423E22E4EA00E17E18AD17A1CDDB7A851C5F8AB3EB71BE9D
      C8B16FF0DC3F852177C27678791CCB69ACF51575C72C3BD15FE81BEE023B979D
      E82FF44DC14D03B53DA3A8BEC113D250CFEDF8ED55FBB879D1BEE9D3A76F6E16
      A4547FA56FAE5CB96C1F88E0100E6B4BBCF4B8B1778160C8F1B8B3ED19CFEB9B
      882F6FEA2A7C81C119B5918BDABA83F815BDE55D253CD3C9E21CE47DB535472C
      82E7BD58C90AA28F38923AABACB81E8B4F0F705280F72DEE439170F0F875C663
      0B1D04882A5D5C5CA37BE2F232BE902B8D1831020FA2FA406C75FC3A359E3BE6
      EFB1D3BCC3462164128E04EB3B50C8C268127FC7164E91B0648CFCC51773C455
      E37B78371BDF85207EC2971F113E72E4081E6D335210D6CEA8BB044B3DC51C13
      E5D8CBF54EE4C25A12BF9C63EBB84C3C53ACC30C8FF50498D096BE12B91A2F8E
      D5C224E4D92B9E42893D26E2F045D06F932CE7ED702E67DF6A38EDE434F5ECD5
      9DADE53024BA006CBF39CD6DF911E998EFBB90F47D173DA2B99DADC6E2380592
      1064EF090D0B1B34F2ADF79E4C9FF9EDC1E9B3BEDD37E2ADF79E86162F391C13
      AAD81C1DA815ECF0B108519355ADA3DA3F18FAD694DB8FE480B33F1D4BFCF9E7
      6389EB33B4C1BF8D9D101DD732AA233EB929AD0119D7B24026AED120BC02E3DA
      A45557E5C25DD7BA1CBA9C38A57A98538DE615DDDACEF8E1E2FCDE9F1D19D8B6
      43B77BA1A125BCC1301828D03151CF2E5D5F397BF872F283A3E71F53DB6A9E2B
      756AA5179E1A367F39A8EC522E75F14EDAC1F69DBA9F2749D18BD3054105618D
      E0D092C9B18792EE0D6C15F24D6A9659A5514A46A5C262D2AA25D3073DC3279E
      BF9372AF5DD57033B67D35E817E898080FE7F243CEB23239CD986EC2E53C8DD2
      6C502915069582B2AEDCCFBAE2EEA2E5BE611448C242263A7E37FE7A76985FA0
      FBE2AD378645D6F41BA7248B41A992D233B2E527FB4F25FDFCD16B1507E28B83
      6A6CBD935CA820483D5EE93630B458E89B6F4E9C76AACF4C5EE892A84E45EF81
      924C19472E24FFC0857E985877C99CCF3E6A703BFEE6AC753F6C10FDC672867D
      1C71C6ED3BB79376FDBA5AB77C5CED35A33A95FAC4DF53A30DF0D2E826F62C37
      66F5843ACB37FFBCAED8EDF85BF759970B1704DE6A2CEFBB73E70E9F6F667E88
      0FE62694E8A6F76EDEA9865747E7ACF8C65F7E36B9D66F3B37ABEB8565C9CBDE
      2CF1E89B7E4E6F7181BCE03E22FC5202329A63C00D9C3777D62B88D704D8E112
      20CD46FED225A3421F356C3FD4F7FCF1D84FE7F675528C5E91F5052BD820D9F6
      359BA0B0706E5FCD5B2165EBCDA8D3B48B2EEEC096ACDB170FBC3B7A79D657F6
      3E2AAC605EF9E81586AFEE5E3E1C7DE4F74D99B51AB573F20929FFE9D77D3443
      6C7AB63EB2A59F1B82ECF384CB47A6EEDBBA24A3F9ABD14E4AA56A8EAD80E823
      5BE2454290CD98DB576B5EF779AFC90AA5B4F845CAFCCFE8F0AC56D8EC6F1A5A
      2B30AAB2247D73164F6F23C9F1909516B9B8AB447F74CE2DB7EB1C84225C917D
      74379D17558466114E9A794A9675981AF2AF17A15B60B6AD662FB5F90B64FAA7
      85B93AF6AF90FF8B9BE6384EFE4AD36CE5552F3A4E0AFB91BF5B3E1FEFFFE5AD
      F6AFDBD7A4622BADB6E7DDEAD60DC156C036EC396E1B272F658F32CD563247FF
      45C7C90B1FD71CC91DE32F7D5C732C5C50FC1F336CFF3E2255E7090BB8C933E0
      0D073C009B4B4684337168A0C2F2264147386EDAF4DA15C3DF9DFBCEAB1E8B27
      BD4E367C35AEB777CD72611F14F3F72A340F0C1300E1F8B836BC55FD4A347F43
      2C5DB8C1D32421CFE58D9EB93A57BA54A83FB5AC57858E5F8AE79A7ECE990A8C
      03AF8C6C335DBBF390D2D3D3AAA7A43C2ECE199F8EEE411F0CEC406FBFD64680
      659C0794BC76FB619A757F206F9633A44EEFCC4FECD3B691CFAE2367E941520A
      CB8A4478881FD5A95C9AD6EE3814FFF3CC11E2871598B17DBF1B24A54283A867
      AB06F45ADBC6022DEB55253F2F77C2A7C1A841F50AB9F24A4377CFD1737C82F8
      1DE5FC494D874C777173737F07678C7DD1CC9239724CF2E56366B3690DBEEBE0
      AD52AB71395BCA9567B158BE7FFA3475E9EF8B3E48E7324CA440C4097005B400
      CFF02D08F91A5B262E4D222A39C3CB9BC7045920625DFA3FBCD3A2E9FF32F78F
      7536EF6B94C0EF6D3EBF858D90FD164CFAD78F89F6DF47E236F6914100A2C2F1
      18129122BCB782D7ACE98E81354E266A5A90EE0B1161C429B0342C857FFD7527
      8CD0F764A2E679C944D3F20A390DE59CA73DA89111EF2B1B9E3EA5FB274E905F
      B76EADE3376C70453E97DDC1BA0C4E70581844DF842F5AD439E9F7DFF1F9C334
      F2D0E9A855CF9E0DD7AC5B371D850A259A82CC8A005EEF26059AA12CBD685127
      26C9C29B61277FFBED10CB0CB83F801A4D623D1B448D2E3FEBFD76113FFCA0A7
      840432676551464A0AD948B6AC5E1D6722FA1805B683C456DE1E428E57837392
      5098B4A357AF69AD162DD2A7E2A5C9E4070F883F2E7576C78E83209906B5ED40
      814EC5D2E5EC0120DA8EB112943D64C8C8066FBFAD4F3D7F9E9C5D5CE8296EEC
      4A784B122ADC640EF241C192E5924402446EABF0B892CF8103FADFE6CC39E6EE
      874580DDDD29A74F04097E888BE48320CA91E2B6901485358DBE6CDAB4E9A4F5
      0AC592035BB61CC37A11822047A7D04010C1C4E26A85D464F5EAD513AA54A9F2
      16D2F3D1CC05F8F549EBD7AFE7AB5A8C424938C34654A56DDBB67DFAF6ED3B05
      C2250076296164790BF10DA761903DD789CE460DFA6CDFBEFD1B689E00FE92FB
      C7CCC85FFAF5FF68A17FAC69A2B32B4D3D4E1AAD9654805AA321BED0AB54A9FA
      A00963802FB170C24A5EFFC8683090293B9B0CC0B90F6B22EB991344CF92F658
      1F141ED3A75DB07EE5960426E38C95EC150655BE0C59EE633299C6F4EB10A68F
      8FCFA6DE51C1FA159BE3C76032C0AA2BD92B08AA5C429060FDF03103BA94D29F
      3F8F5D1599A9A906EAD5BA987EC5AFD79F4B6627C2F4A68FC9601833B04705FD
      A95329A078E6CE9D7B82694D71FDEA5F2F8F811E67AC64CF112A4E20B30F3A72
      CC90DED5F4F7B15C838F8F160B97665162622A3A5EC632180A3A77EE31B56E10
      A0FF756F7C816482C86C30340619CD5F76C4BE7336D207EA93921EE3EABA27ED
      FBF35E8E9C8F68B8D529498D51819580DDA938069261983871147D8AB5204CA6
      B84C8B848FBBA8290BC712DEDC180EB81E2F091D4CCA44E8E809229BC002039F
      95912192A90A344F632127494B06D86F163A393B93829765E0441ED8894C4623
      A53F7962CF4E52E928191F8450AAB100050620671811BA60556F1EBC9C768420
      4ADEBF94D29FA692A64A377B5E328852B0D68812577EB969B68C9455A3C974FF
      1CD174983E9B10A1204ADABF0451BC719E7096B491EF89F813A596D234B8530E
      0D0C0B21CBDE3B83CC0F2F88785E0F6ACF44AC94B5E71352361A8F83B444CE6A
      5849F42F8606990FCC22CBA38BCF94F3C4C4DE0F0BC9622DBC70C045D160DC38
      F229531671AB4BBA72D9F2C7ECD948E08042D711A6634B23104341848E353243
      9208A4A210AA84587EC73A9853E4CFF8F749441FFD9D6AE5F4AFF5DB19114B1F
      E4E242660C047AC0E68EA17387D9121CC60E0CE0C00ED1D9BC6BD825D688FE95
      56C17AA351C6DE2FD1BA5D0956E9737C41048B984F253DDD0C5362C272FB2AF1
      B64A3E853C0241F42431919C31EB70CCCBCA3281C8801AC9C403D2312F03CBAC
      93B8DBFD4C2A3ABBC4F8BDA4C17EE5E2EE61CB896BD7245CFFE85136F9FA6A69
      EBFE1BC790510BA0F4D427B006D9747356242779F2214251238EF1AF3E4D794C
      5A9D3327B174B389F8B51749528BC30F0BB33333B0989089A3F96027E21C1C82
      282B3D8D70091E663691D8426A349E64843DC2D9249F7AB25A81104D2B3E6E4F
      0C36B9E3E6A6FAF54AE8131E3CA5003F573A7AF41637CD4E80A170ECD6EC6662
      38A09C908BF911CCC47EB6821DBAEAF5551A57D0576E545E7F375B49A91A174A
      C0073FCAD62BAB2F53B7ACBE76B34A7AD6637D51DAC1134DC32FAC4426FDB06C
      CF9866FD5AE80FDF6273ABC497229C88AF6BC07053195F0DC56E8C3D861A7CA9
      D268563A7088A84AF8F0988C3B7CFBA22D63AA0FEAA23FF9E859A796F250D29F
      2B7E1224981BAC847A3EA76249DA9FABB035CC24F9965D49DE65286EC1DA3161
      6FF4D15F4E252A8107322E2D5A294828F9CACA8C339BC878EF2CD137B94DAD38
      FC30990D0AFFF2A46A3CA18FBACBE2B8E05DB2CC21A7596ED3E1102DC85D9605
      9C01C0B05228C29A805E5173D07845876FE338E4740E4A22740204894359EB79
      3F3A90F3D0BBE48A088F480961412E13C2A780C946925316A27F9B1323FBEF54
      CAD634C5DF21712C2BC6D11A9C1D71EFA2B7A390390D9864C669956D48AA20C8
      9BC7A3A8F7B3134672AC51140A4FEB3171A29E4394655204C289BC8E63C61494
      271404110A127E7DDA2B1326E8CFE13B23DD70D2C76968305914C79B0F1CA8BF
      7AF52A35EED347CF692E837CBBE35A93C59A9CB4FCF3CFA7751A364C7FE0C001
      6ADEAF9F7EE7F2E5D338AB6EA74EFAB367CFF2BA88B47B257617A2492C7784CA
      96409BB79B90D81013332DA25B37FDA99327A96CE3C67A5EA3F5F01F7F101678
      A36B478F1E435F4E42A1ED08A1FDCC41F62CC1645CE5DD1B374E0B2D53467F35
      E7BD3E0D66698FE3E305093A7DFBB312CF6282E82ED2FC0B1A842CE0B811A7EA
      BCAF402496EBE199037E88FB92CF71F39D9E8ACE16CAF0A0283AB6AC97975E67
      32891D8F773E06CBD0F469AC03D57C8E2B601346E157A73506894DB0FFF16361
      AB6DB2BAC8836C5A4E7EAE26AA58885FE2605A47287284F10B48D0C4491CDFF3
      F8B13DAF0D7490370DF25C44C2AE8441FA0A51D4DBF862F0452F2F99434E9720
      584880E32C73CCE33236530215CC11ADC3BC1812656BE1ABA9FD41569B6800D2
      A58150200408AF43F43AE7B10ED2E580620511B923C30708CA813F426F808FE1
      9CE785B81F60CBF745DCDD91E81F332320FE97B97FAC69B946F6DF69E47F898A
      EE3D55CE11244622D217ADFE4C43C6C3A4C0309B44D90E31135174BBA143F581
      E5CB0733FCCA9409F629552AD8B3448960AFF0701172DCA378F16006CBEEDDBC
      C9466EE1CF282F5C19F85F6047BDD7BFBF7CB96B57F96CFBF6F2F156ADE42391
      91F21F4D9AC8071B37960F346A24EF6FD850DED7A081FC7BFDFA22E4325C16C5
      85438BA8585FA24FB08B57149217F4EE129D5F41F43ED4EF00C2F1DECD7B7A00
      52C12F08D6E5325C16457026E9680A84E425BD7F7C12F17F781779C9AEFD7FA0
      FE9FB1907CB61DB9EC610CD0A7F1A204AA3FFF16D5FAE23255FBF40C5588FEF3
      B9ED5239E6E2ED0C4EB21560707C257B2F0295A3125F8CE2F480CEC5F5CB7EBA
      F5DCCB85ACE7885C447CC58A331F3F3651FF8EC5F58BD75F7E61B2028932334D
      141F9F417D3B96D62F5D7FAEC0CB85FC838EC845C4A7999C69345AB03A45161D
      3A944C5D5B16D7FFF0EBA522C9726DFE52EFEE679EB8168D4BE9AF5F7F20A63C
      2CA85C29987EDD75E5D8B54F1BD7E2B4236C7B7FEE1A65660A9D87069952B42E
      225EB9A4176DFCE584F50C52480AF67213E1FC9ED51EA85DE8818B86AA0439D1
      D6B57B05895AAB5DC9798541E59861BB4C785FED4A01B0BD7F7CF7A3950467D5
      D9774E43B51550B0CB4564DBFC2AAD8A2EC460862F495FF20971DAE98DD61362
      1A5F300BA4B988B24F6F2045F98E746BEED26372E2A52F2D177F5DC99714A157
      A4CB45643AFFB393C2B3F479CA4A396D39BEE42C4A9707B272F018613650A0CB
      45040D53CEA543965B38ED0033E285BA5CE3A850ADE764D8C6D17354FEA7B2B8
      6A79D0334F5A5CFE2A4C66AB7741C7B55EB6CC970999A8370A4C073844404A78
      3C5BB1A539744C23BB009753E5D7106E06B8198E6155C86C69CEB3A2B206ABFE
      E18CE207208793C7CBE788AF02388E800A0B394F40FED043848E1E37AD2604D5
      014782DD48AF014E031C72BA3FE2583E462D827C1EAA6EAD2E5F8D783ED4AC4B
      6BD09CB5C03AC0A1695CA383600F028A0A63A9A21A6A62D21A83B3E34D48800D
      3E3BFE957CE8E49CBF9615D0C12B7CB1DCAC6FDC80076FCB51177A3FA555BE43
      9943201749071D49E3DC88167B132D02BEF522A9AD1311425AEAADA7EF7DE27A
      DF1A21BF72E30D99E3401341C29E202A87E7DC9920C68B28C6CB0F249B68A177
      0C08F434175BE85B2F3D64711DCEF593DB9E7E4DE6387E2C8A96E1C79804105B
      4A7ED36AE8C92443445D22EB45B6F472F372DDB46B231FBA17E2E1FDA1354AD5
      D05FBF799DCEDD3C7B8C245C1B512BB6B3B21DD20890CC71277A862688C7D53B
      DA46AE71B099CCF1B2BBEBCAE13B6A8A387DE11E45F33CF10EBFCECE5170A495
      96E853B7289AE11657F2A85E0E3D54550E3A5851E6B490B7467EC1257349B9A9
      A8229EBDEAEFD49D2639C7F99C2D2D7348FD9DBA41D30740F589ABC3BA3CE187
      28BFE39FE3B1511A59D5A9BDB2070D52C58910DFAC85AC0C500CE05EC6E6CC75
      450CE27F935312EF373DD02D3D5C4246CF7DDB376C78396FFDC4465EA7EB5E77
      A61E2E4F01923666E61FE939FBE5B3C6ACC6B0B7A2C1F0DBEF46753CDFBF0D46
      6F1B5AEDDB1020A9BB8B8DA41106EF7A80C330841CE793A01CAEEFB121AC183A
      F8FAB8C92D8FF5FCA8FBD5211F61F80F0148EA6ADFEF36A030933A867622DCA9
      14A399592BE3CBC265F08A1ED63570C25AF3F21516D2B3C3A26D53E70D851AD1
      7C8C522BB6F73EF3C6BD3A7B5ADF8B3AD9FB1EE43B0092DA6347B6F64713D4E8
      4780C3E208395E238705C117D829AD88EB796AA8ACDFD5426E11D743A62F3CE2
      0062A01037A9408021C77D06B361455C8F13830551B3A3DD64FACC2D0E20A925
      2F1E81576325A901087F0198B029428E37CD614130C919CFF060F37FEC12D7E9
      683FB9F486DA72D5AD4D65421A20E9353B51080AFF0A30913D04438E1B8F913E
      11BBCEFBCE710D36B791BD624A0A32429A588E7C294CC9858B81640BC0717B98
      C382609C530C8D778AA321AAC3F5B6B596BDBE2B2987FF089381B490235F6AA0
      E2C28D40B20D280144001C8FA09C3F8584B7EE5BF7EAA86F39B44BDD54751A9E
      F7217272D152D3016DEB36EED2424F068B5ED6F331820EA04C1BE02611C51211
      C763C9FEF786BA0F0D53C755FAB391ECB5BBA41D650ED591594EC8976A8BA635
      432D76021C864B92C471B6043954C3D44403957D688032AEF499DAB2D79F68DA
      69340D6921EF209A4528F81B90370CCD61C9090231585B2AFA501F455CF1CBD5
      650E89D32CCF5141B01F60E71886B1C00654894A105FADA92F8DA75E521C7148
      F8CE1D5149C8D9D0B155EC88F81180C32A08391E410E7F4AC4B19F10EF808E08
      80DC0BC02023B69C85012AFF46F78F4D8F55DC3AC58E7499C39785A5B58B642B
      2388C86CB1A5FF7268253299EDCC7227DE502FC8273F6B8895C868B64BA40D89
      05B2C8DD7DC58FBDBBDF7B3E1486030B8011807056228349245EC4B358E4E1E3
      1BAEA359077B32595E22A3F83526925FCFB3FBB0D00120A21BD9F3713BDADE08
      912B36BF62D1CDDC5264C943C3EDE448DADDDBBBDDE5AEB51BD1A63F0FD0ECE6
      4FB01F5BD544D3A46CA35DD116B131BFB5DDD5DE275F45A58D30E34549A3290B
      F775736F695123E5EC0BB6F264195FD11E1FB5C559904C8C5C439FEDED4DDFB4
      CB9020933BD4AA41BFC69DA0B96DD3ED35B212CD3863AB8020B1BC5F551AFEB3
      4E90BCDB7C259D4E99465B8F5D1479ECB5D59717E9F91D33EC44A26984374C59
      C11166B365F8BB2D57505CE28742FC4EB3EF45C8DE89A4687C122277D372884C
      122B302C9FD4E580B2D32D4BA76F7B6D60CB1AA1228DB8086D9E219DBEB7C539
      B4366DC2A1DC4D9BD94042A6D32B0B944B9D3DA977448D408A3D719F96F53687
      43CE2E0B5E0AAEAA66A16D88E2F122E19BCC22C8E365AD1D6E1ED8739E82F61C
      BBD7DB90216A70930AF9B336CD8C27BB72142C5F37CD8989206BDD48CB40C446
      005C0B04FF5B9CE8ECBF53595B672BFE0E8963D97F8C880FC10239ECEE083B02
      DF005B81C339E038CB38CF1D3251C6D62C4EDB6A2421D1096FD3AFEED0A1C3E7
      6BD7AE7DF5A38F3E6ADEAC59B3BA0C8EB38CF3588775012E83C0EA781C39213A
      BC76EDDA3DE6CD9B57115FCBF3C063C2E27DFFF4F40C6411EDDCB95383CF846A
      366DDAE475E2C489C09123477AC7C5C59544E602408C2F3C48A88868D9B26587
      CD9B37EB2B57AEEC81273429202080060F1E0C0C252C3E48EEEEBEB475EB56B1
      6800EBB02E9741E108CAF9E31AF5AA57AF9E37AAAC31E02955967FF2C927C44F
      44CD9DCB3F4854AA5465BA73E73C1E25327036B12E97F9EDB7DF7A41B01D1057
      1DBEC7A2095E58D6A21C164EE06652787838E17175AA5FBF25EBE0094D2D6A16
      6E27C2A43E0B656E21F37B4038AED11FBB77EF0EEAD5AB57DAAC59B322C3C3C3
      839A346942BB76EDA372E51A08A54B97FEA0962D9BD0B88B93E948F23163CAE9
      A43BC9BB6FAD45E61F807012FB682BB6A4540FF7F45B47454555EED9B367E5F8
      F8F8A0D3A74FBB717ED5AA559FE21B91F706A7BC13D0B37B4FCFC59B965E526C
      CEAA60E9A095695032AB1013D9E6473E60F30629CFA7C3908B5302124D350FD5
      B5964B2B7D4925197AF4EE5172FD86F5E748299970FE789406270F85AE205220
      C20574083500932370705F7B6C8BECD1BCAA834444F76EDA739C46A6E83991BF
      104B1DC167D5ADB47C2D4414A8DCB39AFEECBA53C7848A44C7685CEA30112FD2
      63A259EE7C0A4F34D5352EF8462599439CBAE3CB819017496053B0118DD5128D
      D6C4D070759C0839CD3F60D3FBD785D2E04F12A3B1D927CBB23C058826FC9D78
      72822043AC6057C3A34634F227CB28B3F87DDF68C29FD42F3A417EAD8DD7AD55
      DB1E17E7F443E3436240B105D2FE403E5749576995ADCCF2E860B1E5794D9629
      8B37DD9D8C4538A74892842B3CD6631C7E2D70D7874DDF3199E5ECBC4CBFFE9E
      9A8032C5B98C2D4FEA31F12A8EE3663CDFA7A46C8581E28DF1B6BC7EDB3E8818
      9E6534A7DA04B6D049AD746F333D7681B7E4BD7CCFCC3A42AC62FF89E2096551
      366559B2C44780D92641AE349A2D2683517CB08E30CD343CCD32A6E3EDBB3477
      67357FB854F9881E41CDEA54D7E4EBC44DB22649C4F9A97AD826A5C164313DCD
      3267243DCD7E9C9A69E4D7F164D6C362142E780C9BF7514E0A881A21D602F007
      84C38ECB4DADB7F1F06DF7BBC95959AE4E2AA952A87B1A32B17C04615A8EB516
      CDE65C4452CD093B914F7DF2766CD2D32C7F37678D1B368965FAA60BC7EA96F1
      BEC88A8C602F5DC0C86F0FEE477CFE9505DD106036822A72446130C9198E1D9B
      9C96EDAE52299D352A85C2137D12E4A90B624540460D5DF1C0AA1A71BB93AA8C
      DDC289015B3E881C98996D7AC209EED88B77527CEF3FCED0FAB8699D3E5C7DFC
      CAD3A74F8F739E0DE8D75B9224ADBAB7A2BF10A9726A94BF634971DDDF0B776C
      3C9DFC61ECF7E1ABB08BB1014421F6F8E38920E2A880547ED44F1C19B6626C93
      F657EFA75DC584DCC202860C2FD8CB2960C4B787B83F62F861541E1ADE522869
      653772963CE9C89A8AD0C2D6AE36701F65681F8D9837BC61E384E4CC0742EAE0
      3976ECDD5F2653C6DDB354B7F779CCFCCD18734A3B914297E51FED9D5261DE8E
      3D9955B843F320101DEBC61D7BE7E78F0409FF06768D6885422973C86906DE95
      D44D9E34C2FFD6B4F90F2BADFC73DB6A16E685F1E1A58CCC84736521373070F4
      B595998C7434F15FB3C10FA29B0D7E28377DFDC654A483F380377920643C587D
      107A026E9103EF4EE1325C16E97FA99334A17A2AD1740C35AE1065AF22C64734
      3059860584B58CA63C7F055AC8DAAFECC233D86E54AC58A8A33AACA637AC6672
      F17399E75F73CCE078255D4558486BFEF2E810EC8E18474CC47B3B462F050672
      DF8A07C0A265D93239AAA1474287A61E77B9B0237EFDFD49C8F6834F82B1F9A7
      ACFFAC7434E14FB211F1AEA20DD791D6A221348992E5C72F6021BD6021EB8206
      7BBFF073BC4C399332F06F341959F20216F2D919B954ABE74E7BC7CAE15234E1
      8F6B871D74F0CFEF45BEB66CC38DC09DFBEF97AF59DDEB50F3A6017F209B02B0
      238F8839BC0FF1C5E7BEEA80007D54B9E3CFF2EC0F5ADD1A377D67716D05E7D7
      84141E76CE66AF3509AFB162E9E59AB33E68913A7EFA6EF7F7DEA93E47469EE3
      8E7CF19BCE9080A86CD4C668F4C9E47E5DCA240CED55EEAE90C2B359C8A5EB2F
      6B57FE78D5A576ED8023AD5A143B8C2CB2EDC87232CDBFBAB61B8BF0D0BC9F3A
      5A13A0B93EF89572C3330CD6434F4AAAC1FB7E52868BBFB745D1BF1BAE03BB29
      CE4654F2BF8512BC59E58D9B6F56365F917BA11EBCEB4413FE0AB490976F3E2D
      E5E5E91A7AE0D4ED94E2C59C93F25A48B7473ED3E74D6D796BE487BB8A5FDDD9
      51020F156821EF3CCECA38752D35D9CBCBE976B0AF9B01632C978554A9749A31
      1F1F9C8C700A9330FE9285F47B5A8224092B7DC966BAB0858F6444D28B5A48E7
      6CBFF957768C10C6CDC5A73A3903194927291D1035AAF7EA456C35CBE4467575
      E75A45E8CEB1D001F2CED8CCCA078E6456E2DDE1F0EAF2D154C85F9116B2ACAA
      D92B1F8F09670B3A191CDF11898758B310320C08ADD78CD8CAB1B52BCC42B29C
      F32306C47F8C429E801BA003D48002F8973A09BBC7DFAA1A2CA928FF8FB55155
      7DFC76620B09660FA002E803006720C35FEB2F23A487D90F25844286F0015A71
      0178C2E60669E16C350AF377D7769CD0A97CBF4DE31B0EDFFE419377DE6EA97F
      274417B482C17196711EEBB02E4A870176A742CCC3D74D1339BB7FF50E6E3A95
      372611A6CBF199862DB1C9551BD6F07E8C7CE2788940C5F9107FB5A27E59EFF2
      9543DDFDC72C3BAE78F824EB27E48BA910EFB455FB4594A8EBAC557960B295AD
      5248EA75DBEF04F87969333CDD55E950C4A20AB2966563FB85A7B10EEB7299CF
      7F3C7F1DF9FB01922ABEF94BAF8D1322FA88CE8004535F975E63F6D5ECDEAE5A
      8256A749CAC8B66466A4657A6EDF73BECCE409D556A4A41B53751AA53608E676
      F0BC3FB69EFFBAE30F284652B9913F0EDDF261CBD7D2B24CC92C70522B5CDA0F
      DCD16CF8A08883C94F8DA2DA5E6E6AF76F97C436FAF293868BE3933212085FDA
      2DE1EF123276F1E1B84BF3BA2CE47252A961EBFBAD9BD8BCABC9229B5030251B
      F3EA77A7FCF96A8736D52E299D340F58C99C6508F875DBA9725327E957DC4DCE
      4CE4F3CE506F9DDF8465878F5C8BE9B19C75A4E283D6B41AD7A55AF7E2011E8A
      2BF7D212301F72DABEED61678DC6D5A94C399F73AC74E552522583212D2BAA8D
      FF4FA999A64C2CEAAE7D94F254B36CD7C5EDB796F4DEC93A5248FF153E5EAEDA
      FEEF74D78727A699D2D2B34DD92EA4ADB0E1A7F4B68DEAFB7067D281434925BB
      7776D99A4ED9175CB42AADAFABCAF5B3F571D79FA41B96DFFDBE6F922072ABD6
      85547E65CBFB95ACDAA14B83D2A1E5C27CD5EECE1ADD9163D9A1478F6BEAB052
      9D9A86A375F5DADB4F330C9917E3138D3FFE71F5F6A3EBA77F353DBA7CF1E9A9
      1F59454009DF571354A59647F309FD7CBACF7BDBE79545937C7A2C181B1439F9
      0D06C7850C79ACC3BA5C06E0B208AC4E818087BF2FC260800F397E083D01F71C
      7821F407388F755897CB70598861B3B1CF88C85FF5B07F8AA2764691FA3FE5FD
      0B2D64A7B1E7092B7BF2FB20E5612927A0BF238110E0AE4CF2BEF88CF8F5C9C6
      641E2FBCB93320670B791E5B3B359F85C4C4B36B7676E6DE205F4B9BBA9555E6
      360D9CEE07789B549999E9DDDC9C14DD3E7DB56A973C16B21308C300BB5386D7
      1C52DE6C36AEAB5FDD2BA364A853861A4B3926A5642B0E1DBFEF3DA093F7D5CE
      8D7CE8AB2D174FD42BE7EB87B50D1461BE3A7FCC958AEF3BFFD080FDF2E68343
      ABB2994D919AFA644258908602FD9CB2CD66990FBFD21F7177DC6B9457DDAB58
      DA39D155A756B5AC1A507AE3C19B9751804F59B36D16124DAB0A997052E58EBF
      DC18D0AB96D1C7DB998FE5847358EDEC98836193DFAAF2878B87FAFA930C632A
      8C9DEB671B4EDC7DA77B0DBF422D64E956EB0D1FBEDDE296D14C694CCD4453E7
      EC2A3F734AFD45F75233135856DCCF2560F4FCDF6F7FF54693E2855AC8922D36
      DE18D2B76EB69797737A7A963913B7DACCCB561EAA336C40D91FC949C1CDC1E4
      531B3067E3F147E3BAD7F22ECC42625AE3B2F7DCE5E4A8AA55348F12538D4FB4
      6A853AAC987FEAE9B34FCBD7ACEB19CF353A79F581B96AB88F7BD25343068E22
      125BC8333713B37166799FF319581BC0FDF3F39732DB7AFB64BA3ABBA9D232B2
      4D862A95836EC7EEBF57AE5438DD0F0A91EEED3A7EEBE1C88E35BDE393B352D8
      42BA6A259779FB2E5F07511C93309438374D54AB5CAE3F4E0FEC856B05DEAE2E
      4AF2F6549B6459A33872CC5879DFB91B77BA370B234C4AB5B8B2A1BD753F59B9
      70DBE9DB0FAE9EDC9C79FDC01DC3838BCC23C0A3D637A074F75675BBFFB13E62
      E0DD5B9183EE19220726DCACDFFBF87AFFA6EF0FCB65219B4DE8A709AAAC4749
      5F80CB22B03A05021EFE9C118C7810F05F0B894EF8A7DDFF320B89E6EF07D835
      8627AC26C2BD301F9F6766665E4C4F4F43D2EAA48E63CE91B3B37357A55231AF
      64A88EC20235E9DEEE2ACDE53B9949E76EA4F318A34AE12E96B2C5743EC9A926
      43FC7D83CBF5DB99B8766919898B539B7EF9B2926092A2469C28AF562BF746D4
      0DCC080E7032E23EB7C56C91E5BDA7132F37A9E45386B5F69D4BBA1259D5B7AC
      5281E3AA448A840759EAD823F79D8D4673E4F6F935C43E52A0853C772BF541C9
      409DBF1A3B1F83E32C03A9C45694AD295B55B6AE900957A085DC7CE4EEE566D5
      028BA5679B7125D492898519B4472F25A6D42EEBE79A996DC952AB2415960BF1
      5FFBE3098FB3BF740C6726A9200BB9F6F79B979A550FF6C654F0092BF11C72EF
      C984C791D582BD1FA759E795EECE4AEF45CBF6D5B9BAB3878675A4822CE4918B
      F7D3F4F8F5D40CEB59370AB91FBBFC28AD56D90067D870B1A9CCD906FF8D3F1E
      F7BFBEBB9BA8518116D2CFC30973C44C8BB3939627F4F42825D3C2325CF03558
      2C58E8074DBB76F349805AEDB2876BC328D04206FBB8789CB8FA28BD46693F77
      563A7F3F351571972719A627E87CD5D31463E0B51BD938F976FF9CF319AA4797
      BFBBE815D468C4B1139EF34B863B790704AA1EE0EAB039D8D3DD2FF6C8635756
      AA50CA5D0B9951619103EEC69B02AEDFC8D2663DBE34E2F1BD0362D3B30E4309
      AF400BD9A0F789758C08584BB69A6C3DEBC28AB235E53200974560753C7A9D11
      F505FE6B21D109FF29F7BFCC42629EB89DBB0256280AE1CB5B485F4F9526EE7C
      9A74E36E368F2D0A0FD126D6AAE82A27A6BCA4854C493538EDD89FE0FBE66B21
      575013FA7AD5DD32AD1B07277ABA6BF030C54B5848DB1CB26498533283E7932C
      03E9CB59C8BC73C8A4C4AC9005DF5DE9D8BB7BADC3CFB5905326B4BE9909B38A
      5F1493519E437E3DA361CCCDC4F4FB2CE339E45BEF1E7C63D8C0A6FB6D16D2DB
      4DEDB960716CBDE75AC8BC7348DCE62A1BB3EC72977EAFD63BFC521632EF1CF2
      38E6932C7B690B699B4356AA20DDE5A69D3E6F2E13D138E812CF2F5FCA42FAF9
      6A0CE5CBFA276EF8C5D08A89104FF0F3753224269BFE9A85AC16B5F63BC67F2D
      2477E7BF08FF420BF9BCEB90E839B1D3220C04F8206A3BCB7EF1EB907C661DE8
      E9548FC1F13C67D91D411C06D81D4F5BF25D874C4ECF7EB4E4B72B973F7DB54A
      146B4EDF7876FF94DE5583B498B116761D5281A96EAEEB9028A8E0336A3EB3E6
      336C06C759C67938832CF82CBBA0EB90C3E7EFFFFDD3FE75AB27A71B1EC1FEBC
      D8597681D721A76CDD3BF78DA6618E16B2C8B3EC82AE437EBAF6E8A3B7BBD5F4
      4B789C2DAE43067B157D965DE075C8E397EF6A7CDD9D54C1FE5E69E8174A78F8
      D8353135CB54B36C88A1B0EB902A9C291F5BB8ED6C65BE0E894722D40F53B332
      1A540CF198F7CBF1E4E11DBCFD99E83967D9C7389FA14A3DB92929C3AFECD6E9
      6B8CB6EB90F8CEAF46DBB941699FE9AB0FDD63A581AD2BFBBB3A6B34A144017C
      1D32E6D7ABF1B80EB90DD7219338DF069E07FA6A82ABE83D9A4F74BC0E39C6BD
      C99B3D18B6B36C6F711D72623FD645615F80CB22B0BAFFCE21ADFDF01FF7A582
      7EE1D0FEDDAB677E31BFF7BBE347539D0691761D9BFC9DB123D6D46FDCE255C7
      B2BCC51CD3220E12771129C42B28BF4022FC622A38B67C3A6B2E712D10172108
      7A23BE25271FD167AE40A267D92F1E2B9088DB5FD0AF322DCB399FE38E6053EB
      98CE1BDFC2CD41F38843646E010A7405D68835F957F9D7396E03A7596E4B3B86
      85123928895A215D686D9087C7FED87F0E666CDAD4AE43870EC4E173D49E4F14
      72F864292E5CBE74E81A0E6D698EE745A14D8B9F39EB48E63BE3EBE866CE3ACA
      FDC221A7599E9784D3F9B65A618AAC6C834D27EC9DF1756DB25C44AC101AF36D
      9D2757AF8AFC4CE1E7F6DC514B9678942E4DD0974126F6453B11844798E4F6B0
      3716B3A2C03BE3078BD0C14B9D394BE4F33E04FDC15C0E64750BEC23640C7128
      9B2F5A507E8144F89545F94A3B080ACA17EDB3E9404134CFD64768C651FC7A5D
      C77CC73E4237D8F37311710126E390E148C2694651F9ACF3B720454DDB2AEEAC
      AE0C679BF57CAE3E37C400A7ED93DAE66B89CA82870B7D4A5627D75259CF6741
      AE97A512255D3F89587E27B5F87083EC53AA3A7DD159EC5634F09B6D42EB656B
      28357B6FB5CC35FAA6ED7D41D077F153E2F432876546454601DE802B97450D77
      4EEE244911EF7C2F7B8757A32F5FA92654077EB9911C6B28848578637FBA4649
      D74ED2EEA9DD25A9C9B8A5B24F78555AD0394DA8BF3A2F41D4C85643212CC41B
      B53550D468CF8C57B114EED8C5A85115FAA2776DA13EF0F315E45843212CC41B
      B3F61425DF3845B133FB4B52A3B7624054D5AE9A72FB0239D6D09E514064F84F
      AE9474E334ED9B3D50921A8E5E00A22A422DED613C565ACE408D9ED5506414E2
      8D5DF3276A7486F67F3118AF5B8EFC4610A527DEA5951F74A05726AD06D1B31A
      16C2611727A34607BE1A2649F5477C2D7B97A84219C9F768D97BEDE83541F4AC
      86F6123991E062C5726244A90685A8D1C1B9C341F4C697B2577815CA7CFC80BE
      9DD886064C5A851A55215B0DEDA51C227DA6FF2A524C7AE3C269FA63DE2849AA
      376C8EEC195A81B2D31ED317E35BD1F08FD034871A8E8C396EAFAD280D6FC08C
      2DF0F14D63D4EEFAF9937468FE9B92D8F9EA0E9D2D76DCE8B7DBD1C75F6E27C7
      1A8E5D78DC5E5B511ADE1B9F597723AED1F5B3C7E9D0B763AC44C8B3BBBC351C
      3B6BA7C8E3DA8A083C9B8C89AE9D89A3C3316FE727821E39D690D379113DC7DA
      349BFCC8C271A265B6F4DF0AFF3F380C25497B599D8F0000000049454E440000
      0000}
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
    object tbitmProperties: TTBXItem
      ImageIndex = 31
      OnClick = tbitmPropertiesClick
      Caption = 'Properties'
      Hint = ''
    end
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
    object NMarkPlay: TTBXItem
      OnClick = NMarkPlayClick
      Caption = 'Play'
      Hint = ''
    end
    object tbitmMarkInfo: TTBXItem
      ImageIndex = 27
      OnClick = tbitmMarkInfoClick
      Caption = 'Placemark Info'
      Hint = ''
    end
    object tbitmFitMarkToScreen: TTBXItem
      ImageIndex = 43
      OnClick = tbitmFitMarkToScreenClick
      Caption = 'Fit to Screen'
      Hint = ''
    end
    object tbxtmAddToMergePolygons: TTBXItem
      ImageIndex = 62
      OnClick = tbxtmAddToMergePolygonsClick
      Caption = 'Add to Merge Polygons (Ctrl+MLeft)'
      Hint = ''
    end
    object tbxFillingMap: TTBXSubmenuItem
      ImageIndex = 7
      Images = MenusImageList
      LinkSubitems = NFillMap
      OnClick = tbxFillingMapClick
      Caption = 'Cached Tiles Map'
      Hint = ''
    end
    object tbitmHideThisMark: TTBXItem
      ImageIndex = 19
      OnClick = tbitmHideThisMarkClick
      Caption = 'Hide'
      Hint = ''
    end
    object tbsprtMainPopUp0: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object NaddPoint: TTBXItem
      ImageIndex = 15
      OnClick = NaddPointClick
      Caption = 'Add Placemark'
      Hint = ''
    end
    object tbsprtMainPopUp1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbitmCenterWithZoom: TTBXSubmenuItem
      Caption = 'Center With &Zoom'
      Hint = ''
      object tbtpltCenterWithZoom: TTBXToolPalette
        ColCount = 5
        Images = ScalesImageList
        PaletteOptions = []
        RowCount = 5
        OnCellClick = tbtpltCenterWithZoomCellClick
        Caption = ''
        Hint = ''
      end
    end
    object tbsprtMainPopUp2: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbitmCopyToClipboard: TTBXSubmenuItem
      ImageIndex = 28
      Images = MenusImageList
      Caption = '&Copy to Clipboard'
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
      object osmorg1: TTBXItem
        OnClick = osmorg1Click
        Caption = 'URL to osm.org'
        Hint = ''
      end
      object nokiamapcreator1: TTBXItem
        OnClick = nokiamapcreator1Click
        Caption = 'URL to Nokia Map Creator'
        Hint = ''
      end
      object terraserver1: TTBXItem
        OnClick = terraserver1Click
        Caption = 'URL to Terraserver'
        Hint = ''
      end
      object Rosreestr: TTBXItem
        OnClick = RosreestrClick
        Caption = 'URL to Rosreestr'
        Hint = ''
      end
      object tbsprtCopyToClipboard0: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object tbitmCopyToClipboardMainMapUrl: TTBXItem
        ImageIndex = 45
        OnClick = tbitmCopyToClipboardMainMapUrlClick
        Caption = 'URL to Primary Map Tile'
        Hint = ''
      end
      object TBCopyLinkLayer: TTBXSubmenuItem
        ImageIndex = 45
        Images = MenusImageList
        Caption = 'URL to Layer Tile'
        Hint = ''
      end
      object tbitmCopyToClipboardCoordinates: TTBXItem
        OnClick = tbitmCopyToClipboardCoordinatesClick
        Caption = '&Coordinates'
        Hint = ''
      end
      object tbitmCopyToClipboardMainMapTile: TTBXItem
        OnClick = tbitmCopyToClipboardMainMapTileClick
        Caption = 'Primary Map Tile'
        Hint = ''
      end
      object tbitmCopyToClipboardMainMapTileFileName: TTBXItem
        OnClick = tbitmCopyToClipboardMainMapTileFileNameClick
        Caption = 'Pathname to Tile in Cache'
        Hint = ''
      end
      object tbitmCopyToClipboardGenshtabName: TTBXItem
        OnClick = tbitmCopyToClipboardGenshtabNameClick
        Caption = 'Genshtab boundary name'
        Hint = ''
      end
    end
    object Nopendir: TTBXItem
      OnClick = NopendirClick
      Caption = 'Show Primary Map Tile'
      Hint = ''
    end
    object tbitmOpenFolderMainMapTile: TTBXItem
      ImageIndex = 34
      OnClick = tbitmOpenFolderMainMapTileClick
      Caption = 'Open Primary Map Tile Folder'
      Hint = ''
    end
    object TBOpenDirLayer: TTBXSubmenuItem
      ImageIndex = 34
      Caption = 'Open Overlay Layer Tile Folder'
      Hint = ''
    end
    object tbsprtMainPopUp3: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbitmAdditionalOperations: TTBXSubmenuItem
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
      object tbsprtAdditionalOperations1: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object DigitalGlobe1: TTBXItem
        ImageIndex = 11
        Images = MenusImageList
        OnClick = DigitalGlobe1Click
        Caption = 'Images available (F6+MLeft)'
        Hint = ''
      end
      object TBXMakeRosreestrPolygon: TTBXItem
        OnClick = TBXMakeRosreestrPolygonClick
        Caption = 'Make Polygon by RosReestr (F8+MLeft)'
        Hint = ''
      end
      object tbsprtAdditionalOperations0: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object NoaaForecastMeteorology1: TTBXItem
        OnClick = NoaaForecastMeteorology1Click
        Caption = 'Current and Forecast Meteorology'
        Hint = ''
      end
    end
    object tbpmiVersions: TTBXSubmenuItem
      OnPopup = tbpmiVersionsPopup
      Caption = 'Version'
      Hint = ''
      object tbitmSelectVersionByMark: TTBXItem
        OnClick = tbitmSelectVersionByMarkClick
        Caption = 'Select by Placemark'
        Hint = ''
      end
      object tbitmMakeVersionByMark: TTBXItem
        OnClick = tbitmMakeVersionByMarkClick
        Caption = 'Make by Placemark'
        Hint = ''
      end
      object tbpmiShowOtherVersions: TTBXItem
        AutoCheck = True
        OnClick = tbpmiShowOtherVersionsClick
        Caption = 'Show other Versions'
        Hint = ''
      end
      object tbpmiClearVersion: TTBXItem
        OnClick = tbpmiClearVersionClick
        Caption = 'Reset'
        Hint = ''
      end
    end
    object tbsprtMainPopUp4: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbitmDownloadMainMapTile: TTBXItem
      ImageIndex = 21
      OnClick = tbitmDownloadMainMapTileClick
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
    object tbsprtMainPopUp5: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object NMapInfo: TTBXItem
      ImageIndex = 27
      OnClick = NMapInfoClick
      Caption = 'Map Info'
      Hint = ''
    end
    object NMapStorageInfo: TTBXItem
      ImageIndex = 27
      OnClick = NMapStorageInfoClick
      Caption = 'Map Storage Info'
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
  object tbxpmnSearchResult: TTBXPopupMenu
    Left = 160
    Top = 264
    object tbitmCopySearchResultCoordinates: TTBXItem
      OnClick = tbitmCopySearchResultCoordinatesClick
      Caption = 'Copy coordinates'
      Hint = ''
    end
    object tbitmCopySearchResultDescription: TTBXItem
      OnClick = tbitmCopySearchResultDescriptionClick
      Caption = 'Copy description'
      Hint = ''
    end
    object tbitmCreatePlaceMarkBySearchResult: TTBXItem
      OnClick = tbitmCreatePlaceMarkBySearchResultClick
      Caption = 'Create placemark'
      Hint = ''
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
    object actShowCacheManager: TAction
      Category = 'Operations'
      Caption = 'Cache Manager'
      OnExecute = actShowCacheManagerExecute
    end
    object actQuit: TAction
      Category = 'Operations'
      Caption = 'Quit'
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
    object actViewToolbarsLock: TAction
      Category = 'View'
      Caption = 'Lock Toolbars'
      OnExecute = actViewToolbarsLockExecute
    end
  end
end
