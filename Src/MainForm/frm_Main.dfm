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
        Checked = True
        ImageIndex = 8
        Images = PanelsImageList
        Options = [tboDefault]
        OnClick = TBmoveClick
        Caption = ''
        Hint = 'Move'
      end
      object TBRectSave: TTBXSubmenuItem
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
          Images = MenusImageList
          OnClick = tbitmCreateShortcutClick
          Caption = 'Create Shortcut'
          Hint = ''
        end
        object tbitmOpenFile: TTBXItem
          ImageIndex = 34
          Images = MenusImageList
          OnClick = tbitmOpenFileClick
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
          OnClick = NzoomInClick
          Caption = 'Zoom In'
          Hint = ''
        end
        object NZoomOut: TTBXItem
          ImageIndex = 24
          Images = MenusImageList
          ShortCut = 34
          OnClick = NZoomOutClick
          Caption = 'Zoom Out'
          Hint = ''
        end
        object TBXSeparatorItem7: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmGoToModal: TTBXItem
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
          Images = MenusImageList
          Caption = 'Selection Manager'
          Hint = ''
          object TBRECT: TTBXItem
            ImageIndex = 10
            Images = PanelsImageList
            Options = [tboShowHint]
            ShortCut = 32850
            OnClick = TBRECTClick
            Caption = 'Rectangular Selection'
            Hint = 'Shift - snap to active grids (if enabled)'
          end
          object TBREGION: TTBXItem
            ImageIndex = 13
            Images = PanelsImageList
            ShortCut = 32848
            OnClick = TBREGIONClick
            Caption = 'Polygonal Selection'
            Hint = ''
          end
          object TBPolylineSelect: TTBXItem
            ImageIndex = 21
            Images = PanelsImageList
            OnClick = TBPolylineSelectClick
            Caption = 'Polyline Selection'
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
            ImageIndex = 44
            Images = MenusImageList
            ShortCut = 16450
            OnClick = TBPreviousClick
            Caption = 'Last Selection'
            Hint = ''
          end
          object tbitmEditLastSelection: TTBXItem
            ImageIndex = 31
            Images = MenusImageList
            OnClick = tbitmEditLastSelectionClick
            Caption = 'Edit Last Selection'
            Hint = ''
          end
          object TBLoadSelFromFile: TTBXItem
            ImageIndex = 34
            Images = MenusImageList
            OnClick = TBLoadSelFromFileClick
            Caption = 'Load from File'
            Hint = ''
          end
        end
        object TBXSeparatorItem9: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmCacheManager: TTBXItem
          Images = MenusImageList
          OnClick = tbitmCacheManagerClick
          Caption = 'Cache Manager'
          Hint = ''
        end
        object TBXSeparatorCacheManager: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmQuit: TTBXItem
          ImageIndex = 29
          Images = MenusImageList
          OnClick = tbitmQuitClick
          Caption = 'Quit'
          Hint = ''
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
            AutoCheck = True
            Images = MenusImageList
            OnClick = NBlock_toolbarsClick
            Caption = 'Lock Toolbars'
            Hint = ''
          end
        end
        object tbsbmInterface: TTBXSubmenuItem
          Images = MenusImageList
          Caption = 'Interface'
          Hint = ''
          object Showstatus: TTBXItem
            AutoCheck = True
            Images = MenusImageList
            ShortCut = 32851
            OnClick = ShowstatusClick
            Caption = 'Status Bar'
            Hint = ''
          end
          object ShowMiniMap: TTBXItem
            AutoCheck = True
            Images = MenusImageList
            ShortCut = 32845
            OnClick = ShowMiniMapClick
            Caption = 'Overview Map'
            Hint = ''
          end
          object ShowLine: TTBXItem
            AutoCheck = True
            Images = MenusImageList
            ShortCut = 32844
            OnClick = ShowLineClick
            Caption = 'Scale Legend'
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
          Images = MenusImageList
          OnClick = NGoToCurClick
          Caption = 'Zoom to Cursor'
          Hint = ''
        end
        object Nbackload: TTBXItem
          AutoCheck = True
          Checked = True
          Images = MenusImageList
          OnClick = NbackloadClick
          Caption = 'Use Maps from Lower Zooms'
          Hint = ''
        end
        object NbackloadLayer: TTBXItem
          AutoCheck = True
          Checked = True
          Images = MenusImageList
          OnClick = NbackloadLayerClick
          Caption = 'Use Layers from Lower Zooms'
          Hint = ''
        end
        object Nanimate: TTBXItem
          AutoCheck = True
          Checked = True
          Images = MenusImageList
          OnClick = NanimateClick
          Caption = 'Zoom Animation'
          Hint = ''
        end
        object NAnimateMove: TTBXItem
          AutoCheck = True
          Checked = True
          Images = MenusImageList
          OnClick = NAnimateMoveClick
          Caption = 'Inertial Movement'
          Hint = ''
        end
        object tbitmGauge: TTBXItem
          AutoCheck = True
          Images = MenusImageList
          OnClick = tbitmGaugeClick
          Caption = 'Azimuth Circle'
          Hint = ''
        end
        object Ninvertcolor: TTBXItem
          AutoCheck = True
          Images = MenusImageList
          ShortCut = 32846
          OnClick = NinvertcolorClick
          Caption = 'Night Mode (Color Inversion)'
          Hint = ''
        end
        object NShowSelection: TTBXItem
          AutoCheck = True
          Images = MenusImageList
          OnClick = NShowSelectionClick
          Caption = 'Previous Selection'
          Hint = ''
        end
        object tbitmNavigationArrow: TTBXItem
          Images = MenusImageList
          OnClick = tbitmNavigationArrowClick
          Caption = 'Navigation Arrow'
          Hint = ''
        end
        object tbitmShowDebugInfo: TTBXItem
          Images = MenusImageList
          Visible = False
          OnClick = tbitmShowDebugInfoClick
          Caption = 'Debug Info'
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
        ImageIndex = 23
        Images = MenusImageList
        MinHeight = 29
        MinWidth = 29
        OnClick = NzoomInClick
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
        OnClick = NZoomOutClick
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
  object PanelsImageList: TTBXImageList
    Height = 24
    Width = 24
    Left = 48
    Top = 176
    PngDIB = {
      1700000089504E470D0A1A0A0000000D49484452000000180000022808060000
      0073922B7C0000570D494441547801EC7D07745545D4EE3EE7F69BDB526E7AEF
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
      420DA596A474FC9FD2AE3D28AA328A9FBB2F7661611F3C4504421001E5253E42
      8991341DB5691C1F3DC66AA6C9D17C05563A96E9984D8D3D041F64DA6386C67F
      1C72B2C62C8BA29AF1316A0D498EA688A0212F715976B90BBBCBDE7EE7EEB2C9
      50CB86CC77F6DCBDF73BBF73CEFDBEEFEEBDE77E7C47CD55FC841B61A9B9B9D9
      83ACDCDF01FB63795D47C443857BF3808C8D8B950A8C7D3DA7732F9CF86842DD
      4F26617698B57F49CCE0FA4506D724CD2CE31C21501E109FC6C079408CE1619E
      9C30977D59B4A37D96E15AFB8AB81DB5F3CD07CEB6388AA2965C5EBAF042973A
      1C53AF94FC4610EFD886E401C1ABCAF950E2CD038255283FC52BAA807940266A
      520612D4777B6745DCBEF3CC98FED60A53CD8F5B2CC5C57BCE75C79EAC6F096D
      6969219C1E991818B39B2F838F2E0F487DE34509114771B3BBE6789B2232B42A
      6D4BC982EB95A7A2DACE59002A97C1751D0747B21B09C8AB46B37E5184FD96F8
      5CFD4BDF9E18BF76A63B666228A25884C93588EB3A75D08425ACF0C9E57EF280
      A8FBFBDD4B2EEDA86DD0261B13C44BFD784B38C098A0418E4D5FB99F751D71B1
      947217AFAE4603BF0AB83CD0BF97D1AE5FC40A6879C546A0F2D539F06FCDA8D6
      2F5A79249D0EB81F8782A0CA68D62FD2CA5E1C944282D2804AFF3F0F0883F35D
      9D8F04800453928908A121BA083E7261705FADC00DE1AB04D644A3FC13E47BFF
      2085BF2E37BDE874BABB176FB655052942832379C4FA87CA84C98FAC287FAF64
      E9EB7B2AD709DC0D4794E10A412BC89A326DAB3A325B6518576C48CB9850C6C2
      C150500A0E962A72B30B9F5C408E66227717E5153DF174E5DAE0BC08AA913373
      0B3685C725EB49C4DD8DA0A6E8E429D1A9E9E34BE1C1CBA08065440F3E2C55E6
      664F9BB7881C1DB0DE41E4B2238B858DF2663CB662DF1A213E203A0E8EE84146
      466699210673ACC47654E7DF5FDCAABA9C1493981E9B9A92C85E6CC281FF2C7E
      0F608DEE50A990F4D996D0A2A33BA39FADDD9FF6FEAF87677C33B5B06439F5B6
      C2721104EB9DBD58C48EB98D0A1F5ABCEAFC27055FD594A7ECAADE6E7AAAEA15
      CDCC831B84046021ACEAD5291CDB19F942D6A49CE7F57A43ACDE608EC022093A
      95966336B014333FA8AF1BA7064FF6486186F801A4D8691C93401A239106031C
      4FFF84A77FA7C3E6EEB75944BBB5CD6AB3B4B6FD7EF942852A54A3999E9A9494
      8FF0092CB4010CD6D93C0002E141910400A26189FAB00FA0F28C21704E92C2CA
      3D02F6831076D07804CCCB43CC2A2E292256AD1A77E56A5DBE50BE9AB2B3C69A
      2A8AA63F385B6B36C80D08091408611547E27082E426F230F994028CD8035632
      C8396084C3A43291BDA3936A2F9EF9B2C12ABE26EC5D4706C43532B3C618DE78
      A860FA1C1D2B71B64111CE35834AF8D5634F981844E6D02F7318C11EB0128516
      A72B9E6C77EDD20FF567BF68EC71BCA55152837C2DDABF5E3020E49399111BBE
      7D764EDA3C2DE274E4B400C5DF07BCCF36322876CB1C8FCAAC70001C497148D0
      62C694C2F3FDB5EB476FF4F4C9E0EBF64976590144C8A724636274E8B687B31F
      988F7992827C5AF82013707003EB55C4C0FC9DB94C44BDC8787CF25A4B7593AD
      6F175BCEE02CE657C05F5809E281E93971E16FCF9D1C5FE245C311B6980165C2
      070A021768176CA0785C1E3A7EB5FDD855AB631BE2843706C121895B5916E62D
      1FED5E23E8D2CCBA3D8FE624ACC40C49ECC579E6E73F003193BD60ABF93B736E
      589CA6CFFF6C7DE796E8DC5A5629B920E42FE883FE6D7963E30792E3CC9BC644
      821021E113CC94F77BC1B1C906DD0BEEFB1E19A24E5CFA2E4260A8726F19A6A0
      7C8D605E941C954A2EF41E56C2B57D20BC292B62CB791F5BCF1C1118A35299B4
      7BADA0D9582939E57ABE8F610A340A1A63542BA3C809690EB63200FED7437E9E
      E0118BDD0873C131B8C1BD078CF0BB0505F10A221370F9A205E62DC31420DF53
      B25E508411FA2D29707153E968C0E1A69B966E6B9DD5FA0B563DD5E44418678E
      3518F54A58434E8C7CB7934C8222064B41A40036B002BD5291862183A029AE31
      035ABAD97AC756D7D579AAC9D9578D28E71F0050363BDAF3532C3DCBB28DD153
      138CE650528AA4278B16FF3D3001C7CF80FC659807910A651E3BD0D1D52FFED6
      D97CBEA14FAC7609741A11E4DBF0471E7DB852345E7139CE3675DC2C4EEFEE5C
      966788CE37ABB49A7012D2FDC8BE8D21E360EF7A4163522857C5AA4316363AC4
      1A51A09FD5C837888EDA85BE3DA4F130667438FD912E89C6854B3437591D52F8
      D780EBB08D3C4736ECFBA7AB0E51002135DAD08CDEA95709242A882C00E6CBA8
      CF9EE10C32619031404607193B64EE40065DD05BF76FC2C60139B35C36A70000
      000049454E4400000000}
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
      4000000089504E470D0A1A0A0000000D49484452000000120000048008060000
      00F22F1D0C00009A58494441547801ECBD757C1457FB3E7CCFCCBAEFC636EE1E
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
      B3FA2EFE6E429CFEFF61EF3D00A32ABAF6F173B76FCAA6F7020142EF0BD22109
      2DF48EA014E9D21444C18212144445B0206028A254A936A40B9122C5D021F416
      2009908424A46EBBFFE7DCCD2E9B46407D7F9FDFF77F9379A69C39F3ECCCDCB9
      E7CE6D73ED8A5CB02CDCEDA09305EDCAB26C77A396FA361E9B7DFA36F1156B06
      19AE89DD144BB69CCB9B3B677A87B2CA9690A7CE682435FD6CD77ACDEF7477BF
      9BB0758118F7E719F18501FD2FE0F81A52A2C093047362DE936672EF8E1E3E68
      EA9BD3B39E7FBEFFC9F0F0F0B938D43B172F27C351D7A3B8D031EDDCB293ACFF
      8001DE90B5012A12115F0F202A6221BF59FEF2175F2EF8F5D32F169CFCF093B9
      87FF9285F487852C70F1F8D2BB5EE3465E8DDAE0FDB1A8105DC3A8170CAA67B0
      9049B0904EA161CD9AB76975B37ED5B0DC4ADEAE267C43E491C6BFE2759726DD
      1FF83D9585D4A8FAE6A99D232BE99BDEF4F4F5F6C10BF19A2C670F93D1C387F2
      9C5C15B93AAF1C577DA7F22DE4DD07E93D85C0CA59AEF171CECAC51FAA9C567E
      ECECF3ED471EC16B3EF1AAB8669E97DF37737C45992AC7AB3A2CE489E391E85C
      2ACD420A73E77EBA5F16F17C58A7ED9F6A6B5CD9E1456A6CC9AC0CC20235401A
      9DBB64A64D5FFE7957C84B76FFF9E3D7E69CB87075369395C0471F7DBCE783FD
      5792E362A6253F685FC994D8B9BAF1466445D3E5A601C60BB575A65F2A7A1A3E
      FCF154D2BBCB36E7550E7882859C0E0BA96ED0619CDC23F09A909AE464919188
      9BE7820191749CFA7A39A9549E152AC96EEE58AEFD72F6132C24E5657C63B876
      CA9427A89D32BCC30AB23D422DB99E21E67CCF208B6F8550992EBCB6A900C62D
      E1C03358484DFD4EC906954B9E822C72AD4A29572B557263F22557CBC57D2EB8
      D3FEF416F2C1F6C5063AB7CB4F4C3CED9E77E9B0EB9D9DCB6021DF4CF2FCAB16
      B2D97F2DE4FF2E0B5962172A2E600BE9EAEBC726B715F22A92CD4222F2EF7282
      AD3AD1B3B6891C5F1D3690837231E8C63A4967C7F4CE1287424AC1B398F17DA3
      4AF5C9A5327679A4CB731E965A9476FD945D4D62E354BB7737895E95EBD3673D
      2B73B25C4CFEF11AA55D3B457B3EB04E99ED44516FAD15BD50A3AF3AA7944BC2
      0A13B6F94B35DA3BE7058943F23823E28DEF44CFB07AF4F9F3F538592E26AD3F
      4DE9374E53DCDCA11287E471A9D653BE11BDC2EAD2E29ED99C2C17637F74A1B4
      1B6768FFBCE11287E471A9569397A14675E8B3818D39592E26AFFB13353A4B07
      3E1B2971481E976AF96A2C88EA72F4A9918E1A1DFC628CC421795CB2C5C4C520
      AAC351CABE9F2885C5BDC0E060BB28CB20936A7468C1588943F238B7F9F8AF24
      A29CD4BBB4FA9D6E2C2A158366FF22C999F4C68533F4C7C20912877D40E27233
      5E43B7CE768D160B8D8F3D41B9E9C9B4E2AD2E52C1D23C2E6393DB89F0663B1E
      97B2121580C8A6C4719B7289107A3699542D4E341D335F740FA94105D90FE9B3
      D73BD0E44F77B1588A4B9142CF26E7A65D3B1B4F47625F9338240F3ADE4D46CF
      9B88F03D80625E2BBB3931F37F65153B8E2E99E28344AA9D080976FC5E0387CF
      827428A702FF8CB3D5A8081B26A42110B4039A01A1003B1E5C8711D9B36CD9B2
      DB084B3A149408478E1C211B3172E4DBC06D2079E488E1DF8F1936E84306C759
      0670DEDBACCB4CB6B21C970005D71123476D1F3562D8BDD1C3870C4661F5D0D1
      13F86A96048EB38CF3588775B98C54189E0046BE2BAC417C93D29CEB95A90DED
      74DD33A2E0AA77C7510A320E21415E037998DC9B2F9848B9B24AEACEA595D2E3
      D46E7989DB8D72A734E4B16533D89AF48ECC601A79D5BF7DA3145D5DCF14D73A
      6B65448DB00A1C1598C0016DB59C88EF4B5A88E2FD1F9D7DC13FEB4C7A9594DD
      F116956219EE922D508C1C39B222EEC88CCEF1771B91E8F15C56AA73F84F4A51
      6C946FA2828AEE8272604D123C51DF0D1748FC33593482B05192AECE0A835C13
      1940C74669D27297E34EEA6A79C3860DFBE1EE6ED5950B16BF6DEEF9E5703C9C
      3E3ED7281454F31254712F90AC5530514537DCA2AC4EB2B844417EFB11193482
      18F648E39DF8E7F4CE9B1AE8F50305A274B4805AC92DA6DF507B52994C03B018
      300E4B82024B4408DF9E25B1D51A12466C23C1072B70D5F426C164111478C65F
      544397CB14968D60226F4134DD60A148541D779D042C6B2BDCCC2071D20E125A
      86107D1249843BEAC2AD4C12E50209E8358175B90CCA5E43E8AF8057C4A1BD84
      7BFEA4468E067D33F539227F17DC725E4F742409B7A695A0014B914248708D52
      4541118638FFD4456C0156134186E5E5487CF720D1E81D4487EE60D13B7C4B11
      4A22FA5414882E721994AD8C3085890E98658AB6489041A1F89E2CFC1110913F
      22C8B7E385B6A1E8A760E9AB1778BC1F97624511D7A52D82A48B428565E378AB
      A5237B62CDF62DCF3D9457F9295BED1D85111A862662F53D4136AE2109CE68CE
      9A04129532DC7D23529BE4B2433E39572677D20B51CA5CE38BD07D4F8E9B5119
      7A7D036775665E8C59A1F90E7DB9EB91DA0F3BAB102A4362F57912D6259080FB
      FC48090A342B3EE0D1D9214199272CFE29095B71436929BAE3F727EE227211BB
      88AC7017B1982F9885B27711B4D2EA468C1CF9B7765A8905BB093602119B0610
      163723B3B1D7CF2ECF8C4804129B830762EC181405512B200C607703DE0160EF
      B265CBEE202CE21445528F1302A27240044C003B8EB38CF3385D12A881945946
      D3FE775AC87C5B93FE96855CB66CF96C39FA071652F82ACFD775648247AFCBF7
      5C6BFCA020B1294C2C3EC122288E0DC165ADFA44CBCF1061072C500A149AA9F1
      6B68946B97F82ACE9C51E49ADED7EB1B6E96A1EBDB63D8A7ACF9E4CB3D0F5CC2
      87AA444B8B5C2315402EB9CAEE247AE1311929012F17265869B1B4605D2EC365
      216ECF44A55A48644A86DF6621B11E246F7ED4EA192D241EB95731D9B7984FB0
      85E438A32C0BC935E27C3B04743F8C1AB18564211F4160AB39CACFF6ABD8A4E0
      A803CB2389EC1E133DD142DA3511513DC142F22EC2167224F4240BA93459DA0A
      020E16A220930A7206C0B609D5403FC1428AA2DAD142E2E2456CB916B273653C
      41F690A81C0B39FD1FB19018905BFF290B998FD65BDD88FF7316D2DAAE7FC097
      334787370F6F68D069EC5BB5DB8F9E56B3DDA809B5DA8D79A576DB51AF546933
      6A44609321AAA09A3D4FDF3EFEBDCDE4729112E0914DAE2EF2AA1BDFD6BB2E98
      D080164E68880744EBCBE78DAD27FF7A527D5D87A86A93024243BF6AF9F22F4E
      254A3B082422992038A5E78AF22BB8B37C2DCD4297EE9BE9D41D23B960876B59
      DD9D5AB70C8972F6769BEA50AE445422C2B8E7990769153C4F14488D5D5CA392
      D1FD8C3CAA8E4BC0C39ABA5188BFF3A012A51D041211A779AF57A2C714D8FB15
      4C84B565F75DCA09DBF0676AD837FBEF85D5A9EA5779FC8AD33913969FCA1DB2
      30FE72E49B07DE6B31FA272D97654844A811A63D78C14626C3A3783252E2213F
      AE1556E6A6B198720E681640038177FBD4729AD6BBA6F6E3C175C3D17733D177
      1BD0779886115989C064615A894EC47B4468226A64368BF43057A427F45D57F4
      DD1C2E5A4884057A914259A936D61AC96135F08A0B9AFAA4BE73C6234D286AAD
      11E66358A3D7C4E91228ADEFAEA50B74E07A0165E689A4E45F4729A946822897
      17984A5E38408BA5C662658E227DA7C5ACDD15332F3C1E23D51A3CD61A190D05
      394A992BB9693076544672D798C8CB19350453697DA75162CD363C000B4B4A6C
      BF99082307073EB398346476BC8FC1446E98FCE6612A873C915CB1B0C7B0D641
      98DD61ABE0F1523E286064101EDCC2D6C0353194C61D1DE812214AB4FDA3661D
      A55431AFD7FBF146B3C5041DB862799CC492D17889145B0309A98F1096EAD077
      8AD2FACEA6CC7958C458622AFDA70A3573F12512F49D27F71DF705F6491CDBAC
      995C5A2ED3E12CC1646009A7392C1591AF1DFC1D6F3D856334F9A3EF726C7DC7
      CA3C5746D3B2DD9C55DA2DEF3F87BE65E93F80B26AE403EE0740B90E5BCD194A
      06ECEF088A39BC22556BE9D2E56F0C1DFA52C721430677C4D350D108A3070C18
      188D4516A27BF6EC1DDDA54BF70E5DBAF4E88435ED02743AD793A512E1CDC35A
      1F7E3877617878B566414121CD0202839B06040437F5F70F6E5AA14298848A15
      2B350B0FAFDA140F7E65BDF4D2904DC5EA823DC22AE9F2DB6F4772172E5C6618
      33664CC11BAF4F35BCF6DA14C3B4696F1AF05E8141A77333848656C83D7AF49C
      61F2E437BFE322A5D6089BB9EAE041235E78F4E8A1A266CD6AF2162D5BC86B54
      AF26AF57BFAEFCDB6F57C8F1BE1F763F390D18304479EEDC9933478E1CFAA1F8
      801498DD22C3AADF30936A8D5608F0F31343424329A44228DEA60B217FBC1A12
      1212429E9E9E92AE75486017E282365C2B8C2C345B3A68BF8995E7F37B8A7816
      927F4DC0BE86770808EF47E2C9155FC2DB89D0861DC38042C4BAF773845119A3
      8EC3064A65B34A0F9204DDCA1516056A25572A61F0E474F7EE5DBC9B6D206F6F
      1FC26B13D8F325222E42B20CECB8BF6147AE0088EBD669C4B9737506BC63A40A
      AB283657E2E3804B96883CC595C18EA724A7D0F011C371B62E10BE122711F1AE
      C34C8A534433BD75BAC9BF0604D0B5E9D3091D4D15AA5451196EDE14B441C172
      F9D9B342D6FBEF93D7ACD978D8DB427B76EDA24A952A51C6C34C2E6FDFF7640F
      88BCD501015A9FCE9DB5AEEDDB6B5D3B76D4EA3A7694F39705E9E24541837741
      7507F653F6CC18C2534154A3664DAA55B3165EF774458D984B648F842D448BAB
      797A8ED456A922C87373498EF74215F9F93285D92C08068388EF1B0A2AD43F39
      2888641B36D01FF1F184251FE8E0A143A6A8A8EE8A1F7E58BF7AFEFC8F062B70
      4077727DF448A1397F9E9420C205168291C50ACED22615902FDE7077A74BE3C6
      511D7C1CD0CDD5957CFDFCC8C5C595AB02F045097434FAE8D011A3D1171DEC2E
      C79BF7B8F860E94154275C10DC406A490E0D959F7DE5150A6ED592EEDF49241F
      9004A03FF13427DE7055F2522B20B38E267734D217BDE65A075731CF1239C5A3
      B97AA2DA17B55A6153BF7EE4111565AE805AE93C3D283030089FD55499B7FEF2
      ABAC4E9D46CACD9BD76FF8F0C3F78782A3840B39A1D59EBEA5568BADE5F29DC8
      9D0CF4067A011C7643D81E7819781F980854034A38D9184FCF717D74BAD9C809
      079EE4D085A42C4B015D257DA05DB95AA3E1FD898F406CDB1DC13A0CCE9720B0
      39288BF169E58989B77CF95768E7CE1DD23C270B9FB0C4BA50F6F2D7AF5F9746
      B15D502C822759A95FBFFE38A0929744C4F99AF11D4983C84D7AFC2743F4263D
      E16FCE465BA6CE4EC492CCD133F13A70258E4AB883379E7D7DF93820258B7859
      EF8E2892961549FD8DC43F4654A4696E4B6650AA43ADB8CFB21CD24F8ADA89D2
      3ED9C28BB5D273CF3D67D7C7F3C6C4F6D92E2816B97FE1825D6227B24B8A45B2
      17CFA4829DF6AD63CF5577EC4714D1DF9E2EB78F9824E487D314BAF3AA542874
      6B02052EDF5382BC5C222E2DE063B73285928237FC896FD53A93C22F90C54520
      358D47309EC7C631CB1FD6F5A25D012F7913EF95891DAB10D74AE1E54BA6B4FB
      74A77F6349E7E6CD9B64FB936AC4C61CB7E1618345C2B25776600E20E97173E4
      3A77BC0090424C16BAFDB2558E03A7148127D508E1139D6834E2098002BA33A0
      0931A9E95E5209FD7289D41DFAD1ED5EF5EC0513BBD694E22C97E1DC454AC093
      8878AF4F4E4E263C91491CC703F1D26A03BCBEB1B9132EFB771E2ABD8A8F7913
      71BFA19CE41EA4A448217B12513FAB29E0F45F8534AF7741E98A44540BA8F38C
      A80D7D2E5751010B791F89BFE5FE3316B2C66739A8951C57E58E237474C5D38F
      F38E8EB0C78B5AC821D17AEA1C6ECFA4D3F788EAF93D4E3BC6062C28FA03D2C8
      7654F8ABF17F8C481A47B65AACDC719C56EEB0A59E2DB413C58FC63B8F981B3D
      F70C1612DFCEB2FF9A9DC82E291699B0C715B52CDAB1ACC21B667C10C7AC2897
      889B6B55B5FAF1FA4652A4D18E781AFF78F3D33375B6185BB266122B3CA9464F
      B290D0919C8D443F3A968431988641FACC16D24682B276128EE3E9310E243C55
      D304618CA46CAB899428E6954BC45B8746C717A90973B0FC992CE494CA667A7D
      82AA140B791C0FC63F604E095267F7FBAF85943AC3C12B750E59637F0DABCA29
      6BF034FED1FA476D6A452DE4F8D195A82EB5B365D219DA53246DCF4064CC9225
      F01FBB72C7D163D527C7FE312285E3EF2C5C721DC9A255262A9E864A29CE4E14
      AF8F7FE639E43359C8B75CEB93B5A645ABC11B6608ADB30BCBED231BC968BD9E
      C4D12271C8A56D728E33CA2562255B618E338AA75926F55179163216FDC7CA0C
      5B7C093EFFEB682125229E43E24D43A9B3790EC90518972F5FE680C61CB71AFC
      85A15B697C625749C6DE335BC82578B3870B2A7CFC39205B5A4A147AE5F6116F
      1DD6E5C2C212C14EC272470B299D8A6EDCB8416B9B43E26D28DCF2B74873485E
      C689CFF1F99A2C5EA0A4E27348BC5C49E3C68DCFC344AB89D447FF2A0BC9CD2F
      015CA77EE6F5C54B7436966FC22A3C5507E275CF59A3478DFA02F771BFE8D2A5
      CBFBD8D45DD1B9AE257EB5505084080B26FDA5F5C599AB0811AF2FDEB94B9701
      CFBABE7809229C76FEA5F5C54B10E19A90EEAFAC2FCE4472F66CC0454A7DAB56
      AD9AA163B5D8722CE60B281C3E717D7156B029721C9704BD3B4C9A3CF9135C9F
      AD85DAE132AA4CCAB7EC5943B2BD8F8D98A4FCD88B11E6FC3A5352B4C9B00B94
      BABE78E6B5048BC78A378BD4DE5606E17C104D29420421162673ABD8AA65CB37
      264C98F07C9B8808772C6246BB77ED7EF8FB8A2FD67E56DBF915D671001F4E7B
      81E8660922ECA0722C12D41A8BBBC5A0994DF1793071C1975F1EDAB071E3947B
      231AE781E4226073A740D28013258858883349CF060D1ABCF2C1071F4CC019A5
      69FEFCF99F1D3E7CF863CE13DFEA520DA18D8C6BF323D265BA12EB8B3B6A3219
      30C35156661C2F71FF777DF1327B87E853AC15F16CEB8B3F81EC7F2CABD401E9
      589B6F96AD7C1DFB601D2AA6891B1429B8F4317DC4A8A146D6970E471C290BB0
      E16D23225B45AB352ABB0ADFDAF83DEEC0E5D407693C28AD443FFEB0F5825DA3
      8C0893C0F0DB73B14A23C9B1EC2BEE64D9655C232C0A575D5AF3D12E4544C48D
      53549D341A35998C26481E3BBEA762329B71B74FB40B153872E07EBF0BE517E4
      DB851C61655BBFF06533964935405913DE95C14D05E9D0CE7286B071FD16B16A
      D52AD29426D3A4A2EBF93A541B3703E432E91E09EEF4808F1FF323E9D299C582
      9B0AB800B527C9793A13D820EFD3BB6F0CDFBEC9CD37D0C50277A2E02072F6C3
      A7C0DC5C496E870BE22E24D361FD22771C23752E1416A48AAA524115E5AD13A2
      EEA59B1AC87BF7EA1BE3E5E549697944D7053F5C3DD691028F1829D0994ADC1D
      C672D5D27D238E7337F041418E6575B0C203A97101FE764A3EA53C28182AAC59
      F5BD185E359CF62729491E16461E6E2E6432A0FEA2054DC4839F183FDC5F0870
      CB470408F74CB03E0F6E866763A5CFD31732E2D2334D2F08AB56AE13BD2B54A7
      DF33DDC8232C186B88E2695A8311FD61C41D0EEBFD0E5B3F7088A70170835C45
      2AA5483713D3736FDECA9EE4E1AC5C262CFD668D58E05B8FAEA903C8D5D39D72
      D144135E67E0DFE582C581EFFF925AA520B3318FCE9E49FA292FDF3C2C6E4AF5
      87C2CCCF57890F839A922634183FAFC13000055A569CC09646B791564374E35A
      52FA8DEBE9138FBC55672DE70943E76C110D617A0AAAE04D0506011F226371E9
      10D051D8F264C4983B7FF6EEE1ACCCFC0E676735CE666DA1E5DBBBBE4B17DC88
      EFABB100E38D8352011E498E9B5B3C18175D98D3F4A824F8BFED49CD8EC76E24
      0D9ABFD2567458236C6F36230804D2C7C5D15FF93B1E11816AE0362BE18F6BA4
      DFBB97E859C940723C2A8A1ED70864D89EF4C441C43AC5C136A550666D1A2760
      F1EC4475EB120D1E4CF4CB2F44FBF7732E51EBD6842FBC13AD5A4574E68C55C6
      65AC319215864503266149B76EEC5B618BDBF2AC52BB6FDF6AFA6DDB8876ECB0
      66B46B67FD756BAAA8CFB5DCB3C72A8B8EA6E39D3B4B7DF49868EB56A25F7FB5
      2AD8FC458B6C316B887BB5D648A1DFA50B1DEFDA552252148A7087D65C7E671B
      A54398BD0816F7B5C71FD7E8871F887EFCD19AD1A7CF939BB679B355AF674F3A
      DEABD7136A64EB58567FF145F689D6ACB1869CF7FDF7D678B95B8D3731AB72C7
      72C8B0C56D792C73C0E3A6E1CE396DDCE890F514D17EFDE878FFFE4F68DA5370
      482A0E4D7BBCD578B8F7ED2BE53FB5C7650A95ED44C76D9D5A98F1FFC320A147
      A74B8ECDB6A5E58EC2F2E25CC882A74686D5AA336DE9A48416BB5D6EAE4C3289
      DA41E1E1AF49E3A83C027B7EAC58617BDC88EB223F8D02617A41017CA241EDBF
      A3A7278A156BA2D479807EDB3B245767CCC9505A8C05F53BFD3C12B2EA4F4714
      2BAAA17C07F0DEBFE7850C312BE5BE0FBF4C0201BB9A3F6DAF56BA85E4DCA2F8
      1A49EF3D7B87E63209E2C485397C809B32BF778CBC543E51ACC8A79A2F11FEDA
      457D178480DAECDC578D431B19A7CB6F5AAC781185B8E0501A23AC44BC54F764
      A25871084A7D075C02497584653A599939B1A23BF29804010D64EF49289B8868
      4361C16F519B9385F13283D28962C57928D11E48055E06CA75FCF1CD71D0E20E
      BD8EB016301EE804B0AB85DA2470A43C28A0B01028EED2216808925B089FCA71
      D35E81E637C029600BC0B5097C161294F9AF7B8A1E109E42A7A84AACA884E039
      A037100C5C002E3D1B51AC188042EB0137A08873245223C705E02181398EB468
      0BE7735C462F2C0AA3867D57201F5F08CF3E4A69B7B6904A7B9EC29AF8405691
      0AFFF820300BF1B1C060600CC0E10084A348A119462F2C4EA0BEF3EE50CB519B
      21E33CDE23B80C9785C8EA9C108C06D8D5640FA805588FE113B77F4DA3D6DFC0
      9788BE868C5D3DF600FE41278476E78CD850A03E3007E0E6B542D89282EA7E40
      937FBB4A839726927B301357857C19C08E5BA0E5880D9C185B9818521872D099
      E6A7FD8A9A9CA666433F81A01FF03EB01460C26108B91208ACCE09C128C017F8
      08E0F43BD4E99DB768F68D049AB8ED00648D81E100EB7C8AD013E032AC8BA8D5
      312BB37B21391820A919839624D207572E5087A99D21EB07D85C45B2FE8D41E0
      04D89D1CB10F811100F7C308EAF5D11E1AF2CD2DEA38ED27C80601FD01AEC168
      84FC63DC151F222E078A382D527E803FBD7736069FAF4EA039B7FEA0E0FA4190
      F901FE802FE00370C8E03248E2040B1DD91FB19B447407A80C0C005A02EC7AC1
      2E5DE748795040E11DA0B8CB84E0799024237C2AC744BC95AA41BB067007602B
      790C2446C4FFEBFE0D3DD0767DDB0820860AFF6485E13305208820A27DC00C40
      7282E43F8317B136228240D231BA23EDDCB193E25E889338240F194FE55AAE6C
      194120E9D2B58BF48C04131D1C7250E2903C645293E54D620897108E8E381A47
      A5FC213F8240D2B3574F7CB0210F51ACA38D1A415FE2903C9636FCBAA1C82110
      79E2E51371E4F087BC080249BFFEFDC8B616DBAE9DBB2022BBAE9DA8F697B5C5
      1707BD486B56AF9114CEBD722E8EF00779048184F3F04C3FA258576AD76E0E23
      6D3A9CB013559B574D6CDBAEADF444E6B72BBE25FC4502ECF6BD34EC25BA7DFB
      36C7E9B73DBF71187969CAA53872F8136CF1B039616254DB282959A54A155A12
      BB448A8F1E335AFAEC0E27F6FEB69783C81B6FDD88A3627F76A2E0F78323084D
      C0CA9508886AD6AA298509E713A470DFBE7D1C46DE79EF4E1C95F26727E23CBF
      77FD22C8810C71C9D948EE7D702F8ECAF82B42C43A5E6F79459003998D246D4E
      5A1C3DEB9FC7548F08402C4404FDF7EFA97AC02CC815B9247376509E88781D80
      5D898DC6C227C28B48BD47EBF4DEA8A0E064287605D8E17A3C074F813C9526F0
      158BD869A242B128BC41C321573D3C5C76377F238C5ED916429DDF09943D0587
      A4A2F1F01CE4EBEEBEFE4F93B1C569FF4051850776C9C3270899BD48269FF6D4
      44CDDFDCB3B3E1E25377B2BA4FCCAC2D23C1DDC5096660DE6C108DC2C7C57832
      8168798EA7C4B1621CE609A78129536BD789EE5EA9B28862FD01764F59A158F1
      7D21563C43F35297A3940A60C7442FE062B11C09BC6307FF892E56AC8EFCE900
      5E7E73EA8FCF62E6C9F159428184B7C1B409F2B38059F614E3E36328B29B8E89
      452B4416994531CE82452C109F0BAC05860256E755DAF85858D0037DC2FDF293
      558B2FA7510FC4DB000D81A640672042C6E3E39E20EF7F59AED8DDB26EBD99E1
      152AF823430B02DC28527C8038BB69EC01F10093FE8EF0047004D806C4C96CE3
      E3A8D9D4EAB28F778E46A733939F2FF2682E091CD04F68D24529F6044FB66FD8
      90AFA76DDA38AC59FD061BC3954AADCEC5454E4DC775439966C043C0562B44CB
      76B2A8B039D19F5C8D4CF0187B62526F45CD695B4EA0C66EA1DC0778AC357B04
      6A632CBBF8E31CB9D02D661DE565F58468D0D5F0F6752FAD7AD58F6A764821B7
      C0687ADD97774C33F2CA750A91680E6975D5A059830CB9C9B8E3595BDCF4FA07
      B46E3CCF27217E3A67ED4E22DE845D51A43EC07D731BE1416027F01DF0D4AE11
      347B006D8022E303E97FCC09606220F80B6EE6EF69A516EEF6A6753EF4EC94A2
      2813E3C729CA2B282F4D0137769DDC5A3EDFAACFD80F7AB73C91FEFCA62B0DBB
      D66C3DAC69608DB601EDBB0D797869EFBAEC02DCE8B195BD3884F8DD246BB2D7
      07B1C2DD8BA7E595DB746BECEA113C412EAA9B5B0C8A8A195984DBA7064A4FBE
      4F0AB3E191B35A7E5130667D776CF54BABBAF6E86FA8DCA84FE7DBF7B2ACCFCE
      58A988BABFB3C855A6D244A73DCCA9EA4696DCCC5CD15DE61C502F57E9AF7F94
      AF0A32E7E56375042361518A7C0F8D797158884F5A469EFC6573E6958542E4CC
      057DDD2B366FE7E4EC9EEBE94CE70D6949D7EA05B9DE1E1759FF01B569934BBF
      FFAE0B6A3EA295E85D7FB036B05A279942AB1573B176A3683228156A512E53CA
      BCC44BE384C0A92B666BDCEA8C573AB9BB695D64050AB53C43EB24BB87AFE6DC
      75CDB97770C38046AB50E3DBCD276E0E4ABD7F0F056ABE6221B993985B40A251
      90796855393EE2D58942F084EF3E953B551F2C2A9DBD2D6A5860677C03D25949
      0A534ACAB5FD3BBF1C3F60C0570B7B577DF4FCD729328DECDAE8930F351F64E7
      8A9EB2BC3C7C675694B92915F9BE86CBC3E56EF5FAF79359BCEA0B68BA603259
      607B4961CC4C166FFEF1EEB5E55316F5ADE9933F6AEEF73E993AE3B46BBA0AEF
      66C9551EA45608A494E34D62B9A8D1C8F26459D7360AC1A3D62C5752781FD1A2
      D4890A580CB54254BA0879819E96B9EBA7D4FF22409067D41AFA516599A7C7C8
      070AE75A728D6B905CABF397092A7C1C52EDECA1906506E65FEF27840EDFBC56
      2EABD0C39CFF40CECBC0CBD5BE1A522B45273721DF492B6CAF1D282EFB7664BD
      ED01FE7AD9A394E35827FC392F996F5090DC332844EEE65FC7CDC353E56D4AFE
      5AA8307CFD363CAB109C77EFD82AA3D11CEAECDB76B0421BE4669129C9D5CB09
      EFF56B5342BDE8B48F9BF2B4562DDCC97A785B7DE7FAC5EB6BA70FD9A22152E6
      E385E4B76E88054260DFF1138C6949B706EFDDB26DBE567071AB3F68B8AB5F54
      5F8553501DB9C6CDC927C0431E12A0244F57B351B46425653DB8B6BB20EDFEF2
      2D1F4F38895BD218E0D8A6850EC485B1C2C035A84E7870D3970757EBFCF19C66
      83BEFAB2EB84D8B9BDC7CF99D238BA5F4BA8B803B88D2FCA397C16289F45F93F
      A3EB1EDE4ED664EA0DCC30D00BD24F20C45360AD7A0F97797AFB966AA724B5FF
      88B78228F0985CF932308FC1E9A7F92154FFB11A0ABEEC8965B31451915FA89B
      3EF7AA4CA37CB5BE97E7DDD50AF94F1D882A3CD62C19B3133149BAD9D8B67264
      E48B557AF610828283C5D08A1529C464141BB9B8741F2008B30610796172A154
      2C32B644380F580FC40003A5B1B002CDC1687BB942AB56FDC2C68F17EFCDFA40
      7EFDC04199AA460D996BA34666DDEDDB82BFB3B61E3E7E58756F8BE1138D1AD7
      9E184895612BBC51B71A404BC9A8D7922BBB0BCF35EA193A76AC2165FE7CE5E9
      6BD7AF5496C95664DEB9D3C57BE2C4162A373793F786F5427F1755CF840DAFD1
      AFADC6ACBB6B31AFC5CBD1178D9522D01B645DA6198CE1EA5AB5E472272759FA
      F9F3C225996C437D8B65CE4B3939438EF5EB772CBF693385B243B4DCDDD58DC6
      B4F0A0A6F43DFDF859E411E3C7915731C9E04B448BA4A68D92C93B2A6AD668E6
      56B52A656FDD2A93E7E5797415840B5F67679FFCEACA956D3B677C3634A46B3B
      A71BDD3B8AF99DA32D757C7DEB6EA953A5B2DAC7E5D09DB3571EA122D6BBEBE7
      CDC679A68C0C191E59946B3D3D4CD504AAA3552ABF8A256AE9B6B78ADF84AE8B
      BD57E2D597BB013E82D66C31BBB8B850705656D5236BB739310943AA11E6728F
      FA5DBE1CA20A0868E811192914C4C59903D42AFF5C83B1C9EE2AAD5E4A0DA9E7
      76C62FECB59AE9FB5DE58250F1F0D13FE20D06D98803FB0F9D679212D8A1D1CE
      BF3573A6981AF39EF1964C66BAE6EB237EDEB697A88D399AC8CA1FCC79B3CADB
      EF4D8AEDD2AD7D234EF7EA1F29705804EBF110D43C18A91F5C5DE6277EF4B178
      FFF529C63FBF982BC61DDF2FCEFEF2D32BC3460F8C2C52A05842664B7B80A40A
      0EC3EA47D95353DE993EFF7A88BF22BB5347727772139BD4A95145AD504EFCF0
      9377B4FD07F650F4EEDB55B0952B35DC8FEB920BC60E912B17E57DDEE59DEFC4
      1FB7AE17F71DD8297EF469CCB18993863741211950AA5360788F43CE45E07A6B
      E9F53C7102F8A2B787BE48752FCCDFE862395B65F1A2A5A3EFDCBA770A3A653A
      057216020E0E6719A2E5A145903798F3C6D45BC8E02D6BEE8F8F136E58F76399
      1353BE093511CAF58186C0756029B00F23B600E1FF7B2714FF49BC852816CAF2
      0AC33283C4C45B4EB64CEE235B5C0AF945D355AB56F29BBE5A7E23815F269032
      E0F13B34F8161262441D3B4617F9A11244F9F9F9ACA015C61C970A10D9424E72
      BFE7D085C9CE9C28821244C8D50292FB7EA25E0A6DDEB62BE4F8529C5D8FF3ED
      448E7D83E7FE2914B95377C02BE6584E93ADAFA83B66D989FE42DF7017D8B9EC
      447FA16F4A6F1AA8ED19E5F50D9E90867A51C76FAFDAC7CDD3F6CDA041838BB2
      20A5F82B7D73E5CA65FB400487E4B0B6C4338F1B7B17480C851E77B63DE3497D
      13F1F94D6D8DCF3038A33773515B7710BFA2B7B2B780673A595C88E2AFB6168A
      A5E0492F56B282D4471CC9FAB4AA743D169F1EE0A404DEB7B80FA58483C7AF33
      1E5FE2204054EEECEC12D31F9797F1855C61DCB8717810D50B62ABE3D7A9F1DC
      317F8F9D161E314A4226E148A0BE1B052D8921E9EFF8929902968C113FFB6CBE
      74D53819EF66E3BB10C44FF8F223C2478F1EC5A36D460AC0DA194D9663A9A7D8
      E35239F68ABC13B9A491C02FE7D83A2E0FCF146B31C3633D094C684B5F895C8B
      17C71A6112F2F8154F49893D26E2F06930648B2816EF702E67DF6A38EDE434F5
      1FD097ADE518247A016CBF39CD6DF901E9D8EF7A91F05D2F3DA2459DADC6D271
      0A2441C8DE1B121A3A62FCAB6F65CE9EFBF5A1D99F7EBD7FDCAB6F3D0AA95069
      2C265471853A502BDDE16311524DD6748CEE7A6FF4AB336F3F10FDCEFD783CF5
      A79F8EA76ECC5507FE36796A4C7CFBE8EEF8E4A6B00E645CCB5299B84623F00A
      8C4BEB0EBDE54B765FEB75F872EACCFAA19A066D6BBA769EF3FDC545033F3E3A
      BC73B73EC92121153DC1301228D53151FF5EBD9F3F77E472FABD63090FA9733D
      F7D55AA5DC034F0D9B3F1F51F51B2E75F14EF6A1AE3DFA2690201BC0E9D2A080
      B0416048A5F4B8C369C9C33B047D95956F56A8E482512EB398D44AC1F44EFFB0
      6909773292BBD40D3363DBD7837EA98E89F0702E3FE42CCAD3B38D39265CCE53
      C9CD06855C6650C828FF4A4AFE159DB39AFB86512A090B99E8C4DDC4EB05A13E
      FEBA65DB6E8C896CE833454E16835C21E4E4168899074EA7FDF4DE8B3587E38B
      834A6CBD535CA83408FD9EEF333C2438E49557A6CD3A3D682E2F7449F45C4DCF
      E18248B9472FA47FCF85BE9FD664F9FC8FDF6B7E3BF1E6A71BBEDF24F51BCB19
      F671C419B7EFDC4EDBFDCB5AEDCA298DD74DE851F9435F7795DACF43A59DD6BF
      DAA4B5539F5BB9F5A70DC1B7136FA5B02E172E0DBCD5583E78D7AE9D5E5FCD7D
      171FCC4DAAD847EFD9B647038FEE4EF989AD3EFF7846A3DF766D55360DCD1757
      BC52F1C1574334AF7281E2E03E22FC521232DA62C00D5FB8E0D3E7116F08B0C3
      25409A87FC6F964F0879D0A2EB68EF8413711F2D18AC914D5C95FF192BD820D8
      F6359BA0AC70C160D5AB41559BCE79AE4D2F6DFCC15FF36F5F3CF8E6C495F95F
      D8FBA8AC82C5E5135719BEB87BF948CCD1DFB7E4356AD945E31554FDA32F07A9
      46D9F46C7D644B3F3104D92749978F7EB07FDBF2DCB62FC468E472C57C5B01A9
      8F6C89A709413667C160B579C3270366C8E4C2B2A729F33FA3C3B35AC9667FD5
      C25A8109B505E1AB73787A1B498E07ADB688155C04FAA36751B95DE71014E1CA
      EDA3BB39BCA82234CB71C2DCD3A2A8C5D4907FBD1CDD52B36D357BA6CD5F2AD3
      3F2D2CD2B17F85FC5FDC34C771F2579A662BAF78DA7152D68FFCDDF22578FF2F
      6FB57FDDBE2604AFB6DA9E37EB5B37045B01DBB0E7B86D9C3C933DCA335BC91C
      FDA71D274F7D5C7324778C3FF371CDB17069F17FCCB0FDFB88143DA72EE626CF
      81371670036C2E1D11CEC4A181CACA9B0E1DC971D36637AE19F6E682375E705B
      36FD25B2E18B29033D1B560B7D27D8D7A3CC3C304C0524C7C7B5B11D9AD5A245
      9BE2E8C20D9E2649F222DEC4B96B8BA42B87F852FBA675E8C4A544AEE9279C29
      C338F0C82D30D3B53BF7292727BB7E46C6C30A9CF1D1C47EF4CEF06EF4DA8B9D
      24B08CF3804AD76EDFCFB6EE0FE4C97286D0E38D45A9833AB7F4DA7DF41CDD4B
      CB6059B9080BF2A1E76A57A1F53B0F27FE34779CF4C332CCD8BEDB0392CA2101
      D4BF43737AB1732B09ED9BD6251F0F1DE1D360D4BC7E8D227955A0BBF7D8793E
      41FC960AFF8436A3663BBBBAEADEC019E36034B352A11C937CF1B8D96C5A87EF
      3A782A944A5CCE168AE4592C96EF1E3DCAFAE6F7A5EFE47019269221A2015C00
      35C0337C0B42BEC696874B93880A4EF08AE731413E885897FE0FEFB468FABFCC
      FD639DCDFB1A25F17B9B4F6E614B64BF0A93FEE543A2032948DCC63E32024054
      723C86A44839DEAB81EBD6F5C5C09A2212B5294DF7A98830E264581A96C2BEFC
      B20746E85B2251DBE26452D38A0B390DE5C2A73DA8A511EF2B1B1E3DA2949327
      C9A74F9F8E899B36B9209FCBEE645D0627382C0B52DF842D5DDA33EDF7DFF1F9
      C36C72D36AA943FFFE2DD66DD8301B85CA249A89CC9A005EEF26199A21AFB274
      690F26C9C79B61A77EFBED30CB0CB83F801A4D673D1BA41A5D7EDCFB5D22BEFF
      5E4F494964CECFA7DC8C0CB291FCBA766DBC89E87D14D801125B797B08395E0D
      2E4C4261FACE0103667558BA549F859726D3EFDD23FEB8D4B99D3B0F816416D4
      7600A53A054B57B207806807C64A40C1A851E39BBFF69A3E2B21819C9C9DE911
      6EEC0A784B122ADC640E4A40C69295824012885CD7E07125AF8307F5BFCD9F7F
      5CE7834580753A2AEC1389043FC4454A40222A94E2B690108D358D3E6FD3A6CD
      F48D32D9F283BFFE7A1CEB454804853A650612114C2CAE5608ADD7AE5D3BB54E
      9D3AAF22BD08CD5C8C5F9FBE71E346BEAAC5289384336C44753A77EE3C68F0E0
      C133215C0E6097928C2C6F21BEE13406B2273AA9B35183413B76ECF80A9A2781
      BFE4FE3133F2977EFD3F5AE81F6B9AD4D9B53E38412AB59A148052A522BED02B
      572806A1099380CFB170C26A5EFFC8683090A9A0800CC0F9771B22EBB193881E
      27EDB141283C69509740FDEA5F93988C3356B35716142532447190C9649A34A4
      5BA83E31B180064607EA576D4D9C84C900ABAE66AF34288A084182F5C3270DEB
      55599F90805D15995959061AD03158BFEA97EB4F24B313617A33C864304C1ADE
      AF86FEF4E90C503C76E7CF67625A5341BFF697CB93A0C719ABD973848213C81C
      848E9C346A603D7D0A966BF0F25263E1D27C4A4DCD42C78B58064346E7CF3FA4
      8ECDFDF4BFEC4B2C954C22321B0CAD40468B561CB5EF9C2DF5FEFAB4B487B8BA
      EE4EFBFF4C2E94F3110DB73A05A1152AB01AB03B05C740320613278EA24FB116
      84C9149F6711F0711725E5E358C29B1BC301D7E305490793322974F424229BC0
      02039F9F9B2B25B364689ECA421A414D06D86F166A9C9C48C6CB3270A218EC44
      26A391723233EDD9690A2DA5E38310722516A0C000E40C234267ACEACD8397D3
      8E9088D20F7C43398FB24855A78F3D2F1D4419586B448E2BBFDC345B46C69A89
      644A394F341BA6CF26442811A51D588E28DE384F3A47EAC8B7A478A65C4DD92A
      DC2987068685242BD83787CCF72F48F1E21ED41E8B58297FEF87246FF93A0ED2
      0239296125D1BF181A643EF829591E5C7CAC5C2C26EDFDB0902C56C30B039C65
      CDA74C21AFF0AA885B5DDA95CB963FE6CD43020714BA8E30075B1A813414A4D0
      B146664852812C144295102BE95807738A9219FF3E89D4477FA75A85FD6BFD76
      46C437F78A70213316023D6073C7D1B9636C090EE386FB716087D4D9BC6BD825
      D688FEF90E817AA351C4DE2FD086DD4956E9137C890816B1844A4E8E19A6C484
      E5F615D2DB2A25148A0924A2CCD45472C2ACC3312F3FDF0422036A24120F48C7
      BC5C2CB34ED2DDEEC752A9B32BBEBE8F54D8AF9C756EB69CF82EADC3F40F1E14
      90B7B79AB61DB8711C198D00CAC9CA843528A09B9F467292271F5228D58863FC
      AB8F321E925AEBC4492CDD6C227EED451094D2E187850579B9584CC8C4D112B0
      13710E0E41949F934DB8040F339B4A6C21552A7732C21EE16C924F3D59AD5448
      4DAB30656F2C36B9E3E6A6664D2BEA93EE3D223F1F173A76EC1637CD4E80A170
      FCD6BC286938A09C2497E647301307D80A76EBADD7D76955435FBB6575FDDD02
      3965A99C29091FFCA8DAB4AA3EBC49557DE3A85A7AD6637DA9B48327350DBFB0
      1A99F4FD8ABD93A286B4D31FB9C5E6568E2F456888AF6BC07053B8B78AE236C7
      1D470D3E57A854AB1D38A4A842F2E1311977F88EA5BF4EAA3FA297FED483C79D
      5AD94D4E7FAEFA5122C1DC6035D44B38054BB2FF5C83AD6126C1BBEA6AF20CA7
      F8C5EB2785BE3C487F398BA8221EC8B8B474B54442E95756E79EDD42C6E47344
      5F1535B5D2E187C96C90F9562745ABA98394BD96C507EE16450E39CD729B0E87
      6841D1B22CE00C00869542103604F4B286235E9775FB3A9E434E17A212420D20
      913894B59EF7A303390FBD4B2E88F088141096E6F2207C04986C24856521FAB7
      396964FF9D4AD99A26FB3B248E65A571B40E6747DCBBE8ED6864CE02A69B715A
      651B920A088AE7F1281AF8F884911C6B148DC2B3FA4D9BA6E71065991481E4A4
      BCEE93269596272948442848F8F559CF4F9DAA3F8FEF8CF4C1491FA7A1C164D1
      1C6F3B7CB8FEEAD5ABD46AD0203DA7B90CF2ED8E6B4D166B72FACA4F3E99D563
      CC18FDC18307A9ED9021FA5D2B57CEE2AC263D7AE8CF9D3BC7EB22D29ED5D85D
      88A6B3DC110A5B026DDE614262536CECAC883E7DF4A74F9DA2AAAD5AE9798DD6
      237FFC4158E08DAE1D3B761C7D391D85762084F66307D9E304937195F76CDE3C
      2B243C5C7FB5F0BD3E1566690F1313251274FA8EC7251EC724A2BB48F32FA810
      B280E3469CAAF3BE0291B45C0FCF1CF043DC977C8E5BE2F454EA6C49191E14A5
      8EADEAE1A1D79A4CD28EC73B1F836568FA2CD6816A09C715B009A3F1ABB35A81
      C42638F0F0A164AB6DB226C8836C56617E91262A58885FE2605677287284F133
      48D0C4E91CDFFBF0A13DAF137490370BF22244925D0985F479A2E8D7F0C5E08B
      1E1E22879CAE48B09000C759E698C7656CA6042A98235A87793012551BE1ABA9
      4341D6986818D25580102008087B8EE825CE631DA4AB01C1A511E990E1050414
      C217A127C0C770CEF340DC07B0E57B23AE7324FAC7CC0888FF65EE1F6B5A9191
      FD771AF95FA2F27B4F517804891588F4E5AB3FD610F1302930C626917741CC44
      14D365F468BD7FF5EA810C9FF0F040AFCA9503DD2B560CF4080B93428EBB55A8
      10C86059F2CD9B6CE496FC84F2920B87FF1976D4E4A143C5CBBD7B8BE7BA7615
      4F74E8201E8D8C14FF68DD5A3CD4AA9578B0654BF1408B16E2FEE6CDC5DF9B35
      93422EC365515C726811050F26FA10BB784D49F294DE5DA28455446F43FD0E20
      39DEBB794FF7432AF029C1BA5C86CBA208CE241D4D81247946EF1F9F44FC1FDE
      459EB16BFF1FA8FF672C249F6D47AEB81F0B0C6AB534899A2DBA458D3EBB4CF5
      3E3A4B3562FE7C62BB148EB9783B83936C05181C5FCDDED340E1A8C417A3383D
      AC6705FD8A1F6F3DF17221EB39A208115FB1E2CC870F4D34B47B05FDB28D979F
      9AAC54A2BC3C132526E6D2E0EE55F4DF6C3C5FEAE542FE41471421E2D34CCE34
      1A2D589D229F0E1F4EA7DEED2BE8BFFFE552B96445367FE5370F304F7CBB5695
      F5D7AFDF93A63C2CA85D2B907ED97DE5F8B58F5A35E2B4236C7B7FD11AE5E549
      3AF70D2265A89DA578ED4A1EB4F9E793D633484952BA579408E7F7AC764FE94C
      F79C5554274043DBD6EF9348946AF56ACE2B0B0AC70CDB65C214A50BF9C1F6FE
      F1ED0F56129C5517DC3903D50E40E9AE08916DF32BD40ABA108B19BE207CCE27
      C4D967365B4F88E9F5D259202D4254706613C9AA77A75B0BBE392EA65EFADC72
      F197D57C49117AE5BA2244A6849F3432F72A09949F71C67262F93994AE0EE417
      E221C202A0545784081AA6C24B872CB770DA0166C4CB7445C651995A4FC8B08D
      A327A8FC4F6571D58AA17FB1B474F9AB2C99ADDEA51DD706D8329F2564A28128
      301BE01001C9E1F16CC596E6D0318DEC525C61955F44B815E06638867521B3A5
      39CF8ADA2AACFA87338AEF81424E1E2F9F20BE06E038022A2BE43C09E2BB6E52
      E8E871D31A42501F7024D883F43AE00CC021A787228EE563945250C243D5ADD5
      E5AB114F869275691D9AB31ED80038348D6B7408EC014079611CD554424D9AB4
      C6E2EC780B126083CF8E7FA5047A3895AC650D74F02A6F2C37EB1D3FECDE6B62
      F485818F688DF768E6905084A49B968429AE44CB3C8996025F7B90D059438490
      BEF1D4D3775EF1036F8D139FBFF1B2C871A0B544C29E44540DCFB93341AC0751
      AC870F48B6D012CF5810E86901B6D0D71E7AC8E2BB9D1F22763EF3A2C871FC58
      34ADC08F3109206D29F115ABA127930811F58A6C1AD9DEC3D5C365CBEECD7CE8
      5E8287F74737A8DC407FFDE6753A7FF3DC7112706D4429DBC1CA7608E340325F
      47F418AD118F6F7AAC93D8E05094C8F1AA7B9A88613B1B4A71FA4C174D0BDDF1
      0EBFD6CE517AA4839AE823D7689AE31A5FE9985E0C395C570C385453E4B424EF
      88FCD24B1691725351453C7B3554D397A63BC57B9DAB22724843357DA0E905A0
      FAC4D5615D9EF04354D2F1CFF1D8A882ACFAD455DE8F4628E2A510DFAC852C1C
      0806B897B1398B5C1183F8DFE4E4C4FB4D3F744B3FE7A0890B5EF30E1D5BCD53
      3FADA5C79926D79DA89FF3238084CD7925477AE17EF9B8316B31ECAD683EF6F6
      9BD1DD138676C2E8ED446BBD5B0024F475B691B4C4E0DD0870188A90E37C1254
      C8F51D368415A3475E9F32A3FDF1FEEFF5BD3AEA3D0CFF510009BDEDFBDD2614
      6652C7D04E843B95D26866D6DAF8B270385ED1C3BA061AAC352F5E61213D3E2C
      DA3675F15052235A84516AC58E81675F4E7E6E6FC7E4E853039321DF0990D015
      3BB2B53F5AA3463F001C5640C8F106852C083EC34E69457CFFD3A345FDEE7662
      BBF87E227DE6160F100385B849A5020C85EE63980D2BE2FB9D1C2911451DEB23
      D2C7AEF10009ED79F108BC1A2B08CD41F833C0846D1072BC4D210B82E94E7886
      079BFF7DE7F81EC7868855363516EB6E6B2312D200092FDA898250F8178089EC
      21180ADDEB18E9D3B0EBBCED14DF7C6B27D123B69244464813CB912F84CAB970
      30487E05386E0F0B59104CD1C4D2EB9A781AA538D2747B47D1E3DB4A62D80F30
      19484B72E40BCD155CB82548B60315810880E31154F82713F0D67DC701DDF5ED
      47F76A92A5CCC6F33E441A6735B519D6B949AB5EEDF464B0E8453D1F23E820CA
      74026E12511C11713C8EEC7F2F2B07D118657CAD3F5B8A1E7B2AD9117EF83991
      E5847CA1B1D4B428D46217C0619820081C674B5048354649345C3E8886C9E3AB
      9C6D2C7AFC89A69D41D39096E4DDA466110AFE06140F430A590A037F0CD6F6B2
      413448165FE1727D9143E234CB0B55101C00D83986A12CB00155A28AC4576B9A
      09AFD300219E38247CE78EA812E46CE8D82A7647FC28C0611D841C8F20873F39
      E2D84F88774047F841EE016090115BCEB200957FA3FBC7A6C70A6E9D6C678EC8
      E1B3C2D2D159B0959188C86CB1A5FF7268253299EDCC620FDE504FC9273E6E88
      95C868B64B844DA9A5B2887DBDA51F7BF380E722288C051603E300C959890C26
      29F1349EC5228E7DBDC506FAF4507F262B4E64947E8D89C4978AED3E2C740088
      E846C122DC8EB63742CA9536BF6CE9CDA2526489A3C3ECE448DADD6B7B7462EF
      C62D69CB9F07695EDB4CECC75635A9694281D1AE688BD8985FDDE162EF932FA2
      B3C799F1A2A4D1948FFBBA45B7B45423F9BC0BB6F26479BDA63D3EE157278964
      5AE43AFA78DF40FAAA4BAE0099D8AD5103FA25FE242DE89C63AF919568CE595B
      052412CBDB7585B13F69259237DBAEA63319B368DBF18B521E7B9DF5D5A5F4A2
      EEB97622A96984374C59C11166B365EC9BED57517CEABB92F88DA8EFA490BD93
      6931F82444D1A6151299045660583E6CC20115E458BE99BDFDC5E1ED1B844869
      C4A5D0E61972E83B5B9C436BD3A61E2EDAB4B9CD05646A9E5F2CFFC6C99D0646
      34F0A7B89329B462A0390C7276F9F0327055351F6D43148F1749BEC92C05C5BC
      FCF563CDC3FB2F94D1DEE3C9030DB9520D6E52197FD6A699F16457A182E5CB36
      853129C8DF30DE321CB17100D702C1FF162775F6DFA9ACADB3657F87C4B1EC3F
      46C487600985EC3A84DD81AF806DC09142709C659CA7834C2A636B16A76D3512
      90E881B7E9D776EBD6ED93F5EBD7BFF0DE7BEFB58D8A8A6AC2E038CB388F7558
      17E03208AC8EC79106D1B18D1B37EEB770E1C29AF85A9E1B1E1396DEF7CFC9C9
      4516D1AE5DBB54F84CA86ACB962D1E274F9EF41F3F7EBC677C7C7C25642E06A4
      F18507096511EDDBB7EFB675EB567DEDDAB5DDF08426F9F9F9D1C8912381D184
      C50749A7F3A66DDBB6498B06B00EEB7219148EA0C23FAED180A64D9B7AA2CA2A
      039E5265F9871F7E48FC44D48205FC8344952BD7A63B7712F0289181B38975B9
      CC6FBFFD3600821D8074D5E13B2C9AE081652DAA61E1046E26858585111E57A7
      66CDDAB30E9ED054A3666176224CEAF351E61632BF0324C735FA63CF9E3D0103
      060CC8FEF4D34F23C3C2C2025AB76E4DBB77EFA76AD59A4B4A972EFD41EDDBB7
      A6291767D0D1F4E3C68C336977D2F7DC5A8FCC3F00C909ECA3ADD8924253DCD3
      EF181D1D5DBB7FFFFEB513131303CE9C39E3CAF975EBD67D846F44268FCC78C3
      AF7FDFFEEECBB67C7349B635BF86A59B5AA411E9AC424C649B1F7981CD13A43C
      9F0E452E4E09486AAA79B4B6A35845EE4D0AC1D06F60BF4A1B376D3C4F72C184
      F3C76334327D3474252219225C408B500530390207F7A5DBF6C87E6DEB3A48A4
      E8BE2D7B4FD0F80C3D274A1662A923F8ACBA839AAF8548056AF7AFA73FB7E1F4
      714945A0E334256B8C142FD763A24F757C0A4FF4814B7CE08D5A22873875C797
      03212F97C0A660239AAC269AA88AA5B1CA7829E434FF804DEF5F170A233F4C8D
      C1669F218AE24C2086F07732F324418658E9AE815B8318E4CF105166D9DBDE31
      843F61484C92F862278F5B6BB63FACC0E9FBC6FBC480623BA47D8112AE96B6D6
      1A5B99953181D296E73559662EDB72770616E19C290802AEF0588F71F835FFDD
      EFB679C364160B8A33FDF27B5612CA54E032B63CA1DFB4AB388E9BF17C9F9C0A
      64064A3426DAF2866C7F27626CBED19C6513D8428D52AEEB343B6EB1A7E0B972
      EFDCE724B182FD4C5926E55301E55BF2A58F00B34D825C6E345B4C06A3F4C13A
      C234D3F028DF9883B7EFB2754E4AFE70A9FC013D809AD529AE89D7899B644D92
      14E7A7EA619BE40693C5F428DF9C9BF6A8E061569E915FC713590F8B5138E331
      6CDE47392941AA1162ED005F4072D871B9A94D371FB9ADBB9B9E9FEFA25108B5
      4274D9C8C4F211846939D65A349B8B10090DA7EE423E0D2ADEB1698FF27D5D9D
      54AED82496D95B2E1C6F12EE79911519811E5ABFF15F1F3A80F8A22B8BFB20C0
      6C0455E488CC6012731D3B363DBB40A750C89D540A99CC1D7D12E0AE0D604540
      440D5DF0C0AA1271BB13EA4CFE9513C37E7D2772785E81299313DCB117EF6478
      A73CCC557BB9AA35EFAE3D71E5D1A3472738CF06F4EB2D4110D624AF1A2A8914
      85352AD9B124BBEEEB813B36EE1A5F18FBFDF82AEC326C00A9107BFCF1441071
      5482507DC28F1C19B36A72EBAE5753B2AF62426E6101438417E8A1F11BF7F561
      EE8F587E18958786A710426AD1959C04773ABAAE26B4B0B5EB0DDF4FB9EA07E3
      168E6DD12A293DEF9E2475F01C3BF6EECF3328F7EE396A323001337F33C69CDC
      4E24D3E6FBC67866D458B8736F5E1DEED062F047C7BA72C7DEF9E93D89847F03
      BB468C4C261739E43403EF4A6A674C1FE77B6BD6A2FBB556FFB97D2D0B8BC378
      FF526E5ED2F9AA901B1838FADACACC403A86F82F6AE4BD98A891F7C5362FDDF8
      00E9C062E04DEE0F190F562F84EE806BE4F0BB33B90C9745FA5FEA0455889E2A
      B69944AD6A44DBAB88F11103CC106101612D63A8D85FA916B2F1F3BBF10CB62B
      05078738AAC36A7AC26AA657389F97F0A26306C76B696BC2425AF357C6046177
      C4386222DEDB317AC9DF9FFB567A002C46142D33A25BB825756BE376970B3BE2
      97DF3383761CCA0CC4E69FB9F1E32A31843FC146C4BB8A3A4C4B6A8B8AD0244A
      171F3E8585F480856C021AECFD925FE8E58979948B7FA3C9C892A7B0908FCFC8
      8546FD77D93B560C1362087F5C3BECA0237F7A2BF2C5159B6EF8EF3A9052BD61
      7D8FC36DDBF8FD816CF2C38E3C2EF6C87EC4979DFFA21B02F451EDEE3F89F3DE
      E9706BCAEC5D15D4359C5E94A4F0B07346BDD83AACC1AA6F2E37FCF49D7659AF
      CFDEA37BEB8DFAF345E439EEC817BFEA090988AA466F8E419FCC18D22B3C69F4
      806A7725293C9B85FC66E365F5EA1FAE3A376EEC77B443BBE023C822DB8E2CA6
      D3A2ABEBFBB0080FCDFB2863547EAAEB239FAF3636D7603DF46464193C53D272
      9D7D3D2DB2A17D701DD855762EA296EF2D94E0CD2A6EDE7AB3B6F98A3800F5E0
      5D2786F057AA85BC7CF351650F77979083A76F675408764A2B6E215D1F78CD5E
      F841FB5BE3DFDD5DE1EAAEEE0278A8540B79E7617EEEE96B59E91E1E9ADB81DE
      AE068CB1221652A1D0AA26BD7F6806C2994CC2F84B16D2E751451204ACF4259A
      E9C2AF7C2423129ED6423A15F82CBAB2739C64DC9CBDEA9313909B768A7200A9
      464D5FB888AD6699D1B289F67C8708ED79163A40DC159757FBE0D1BC5ABC3B1C
      595B3D86CAF82BD7425655443DFFFEA430B6A033C0F12D91F4106B3E428601A1
      F59A115B39B67665594896737EC4B0C4F751C81D7005B480129001FF522760F7
      F85B55832595CAFF636D54D47F7D07B18504B31B5003F47E801390EBABF61511
      D2FD82FB02424986F01E5A7101C8647383B4E46C350AF5D5A9BB4FED517DC896
      D75B8CDDF14EEB375E6BAF7F23481BB08AC17196711EEBB02E4A870276A740CC
      CDDB5515396F68FD6EAE5A85272611A6CB8979865FE3D2EBB668E0F910F9C4F1
      8AFEB284205FA5AC5955CFEAB54374BE93569C90DDCFCCFF11F9D2548877DABA
      43222A3671522BDC30D92A50C804E5861D77FC7C3CD4B9EE3A450E14B1A882A8
      66D9E42161D9ACC3BA5CE6931F12AE23FF004042CD577E1EB0796AC420A93320
      C1D4D779C0A4FD0DFB76A997A4D6AAD2720B2C79B9D979EE3BF62684CF985A6F
      55468E314BAB92AB03606E472EFC635BC297DDBF473112AA8DFF61F4AFEFB67F
      313BDF94CE028D52E6DC75F8CEA8B123220EA53F324AD5F67055EABE5E1ED7F2
      F30F5B2C4B4CCB4D227C69B7A2AF73D0E46547E22F2DECB584CB0995C76C1CB2
      615ADBDE268B6842C18C02CCABDF9CF9E70BDD3AD5BB24D7A8EEB19239DFE0F7
      CBF6D3D53E98AE5F75373D2F95CF3B433CB53E53571C397A2DB6DF4AD6112A8C
      58D7614AAF7A7D2BF8B9C9AE246727613EA4D9B1FD7E4F95CA45135ECDEB3C2B
      5DB99456CB60C8CE8FEEE4FB63569E290F8BBAAB1F643C52ADD87D71C7ADE503
      77B18E1034749597878B7AE81B7DF561A9D9A6EC9C02538133A96B6CFA31A773
      CB665EDC9974F0705AA5BE3D9DB7E550C10567B542EDEDA270F97863FCF5CC1C
      C3CABBDF0D4E93885CEBF522854FD5EA3E95EA76EBD5BC4A48B5506FA5CE49A5
      3D7ABC20E4D809D573ACF45C43C3B1267AF5ED47B986BC8B89A9C61FFEB87AFB
      C1F533BF981E5CBEF8E8F40FAC22410EDF5B1550A7915BDBA943BCFA2E7CCDEB
      F9A5D3BDFA2D9E1C1039E36506C72519F2588775B90CC06511589D0C010F7F6F
      8481001F727C10BA03BA427820F405388F755897CB70598861B3B1CF4891BFEA
      61FF948ADA19A5D4FF29EF5F68217B4C4E20ACECC9EF835487A59C8AFE8E0482
      80BB2289FB13731337A61BD379BCF0E6CE859C2D6402B67656090B898967EF82
      82BC7D01DE964E4D6A2BCC9D9A6B52FC3C4D8ABCBC9C3EAE1A599F8F5EA8DBAB
      9885EC01C250C0EEE4610D4755379B8D1B9AD5F7C8AD14A2C9556229C7B48C02
      D9E113299EC37A785EEDD9D28BBEF8F5E2C9A6D5BC7DB0B6812CD45BEB8BB952
      85FD09F70DD82F6FDE3BBCA680D96459599953430354E4EFA329309B453EFC0A
      7FC4DFD135A8AE48AE59C529D545AB54B4AFEB5765F3A19B9751804F590B6C16
      124DAB0B99E484DADD7FBE316C4023A397A7131FCB09E7B0EA79B1874267BC5A
      E70F6737E5F5CC5C63168C9DCBC79B4EDE7DA36F039F322D64950E1B0DEFBED6
      EE96D14CD94CCD441FCCDF5D7DEECC664B93B3F2925856C1C7D96FE2A2DF6F7F
      F172EB0A655AC84AED36DF1835B849818787534E4EBE390FB7DACC2B561F7E6E
      CCB0AA3F9046C6CDC1E453ED377FF3890753FA36F22CCB42625AE3BCEFFCE5F4
      E8BA75540F52B38C996AA54C191AEC9B75E6DCA3EA0D9BB827728D4E5DBD67AE
      1BE6A54B7B64C8C55144600B79F6666A01CE2C53389F81B501749F245CCAEBEC
      E995E7E2E4AAC8CE2D3019EAD40EB81D7720B95AE5304A09081292779FB8757F
      7CF7869E89E9F9196C215DD482F3C2FD97AF83289E4918729C9BA62A15CED71F
      E6F80FC0B5024F17673979BA2B4DA2A8921D3D6EACBDFFFC8D3B7DA342099352
      35AE6CA86FA5A4CB976C3F73FBDED5535BF3AE1FBC63B877917924F0A8F5F6AB
      D2B74393BE7F6C8C187EF756E4886443E4F0A49BCD069ED8E8DBE6ED31452C64
      D4D421AA80DA7A94F406B82C02AB9321E0E1CF1981880700FFB590E8847FDAFD
      2FB39068FE01805D2B7892D544B80FE6E393BCBCBC8B3939D9485A9DD07DD279
      727272EA2D97CB16560AD152A8BF2AC753A7505DBE939776FE460E8F31AA15E6
      6CA91AACF54ACF321912530CCED76FE7E1DAA5653C2E4E6DF9F9F35A1293103D
      EE6475A552BE2FA2897F6EA09FC688FBDC16B34514F79D49BDDCBA9657386BED
      3F9F7625B2AE7755B90CC755816449F7F2957147539C8C4673E48E450DA47DA4
      540B79FE56D6BD4AFE5A5F25763E06C759065281AD285B53B6AA6C5D21935CA9
      1672EBD1BB97A3EAF907E714987125D492878519D4C72EA56634AEEAE3925760
      C9572A0405960BF15DFFC349B7733F770F6326A1340BB9FEF79B97A2EA077A62
      2A98C94A3C87DC772AE96164BD40CF87D9D679A5CE49EEB974C5FEE7AEEEEAA7
      621DA1340B79F4624AB61EBF9E956B3DEB4621DDF1CB0FB21B55F573820D9736
      95B9C0E0BBF98713BED7F7F4916A54AA85F471D3608E986771D2A879424F0F32
      F22C2CC3055F83C582857ED0B46B3733FD944AE7BD5C1B46A91632D0CBD9EDE4
      D507390DAAF8E8582921252B0B71E7CC5C53263A5FF128C3E87FED46014EBE75
      9F703E43F1E0F2B7173D025A8E3B7ED27D51A5308DA79FBFE21EAE0E9B03DD75
      3E71471FBAB0528DCA3A3564469945F4BB9B68F2BB7E235F9DFFF0D2B887C907
      A54DCF3A0C39BC522D64F38127373022602DD96AB2F56C022BCAD694CB005C16
      81D5F1E87542D41BF8AF854427FCA7DCFF320B8979E20EEE0A58A16884CF6E21
      BDDD15AAF8846CE1C6DD021E5B1416A44E6D54D3454CCD78460B999165D0EC3C
      90E4FDCA8B41575013FA72CDDDF08EAD0253DD752A3C4CF10C16D23687AC14AA
      4967F07C9265207D360B597C0E99969A1FB4F8DB2BDD07F66D74E4891672E6D4
      8E37F36056F18BD26494E7905FCE69117B33352785653C877CF5CD432F8F19DE
      E680CD427ABA2ADD172F8B6BFA440B597C0E89DB5C5563575CEE35E485A6479E
      C942169F439EC07C9265CF6C216D73C85A3584BBDCB43309E6F088560197787E
      F94C16D2C75B65A85ED53775D3CF860E4C8478928FB7C6909A6EFA6B16B25EF4
      FA6F19FFB590DC9DFF22FC0B2DE493AE43A2E7A49D16A13FC00751DB59F6D35F
      87E4336B7F774D5306C78B9D657707712860773C6D29711D323DA7E0C1F2DFAE
      5CFEE8853AD1AC397BF3B9033307D60D5063C65AD6754819A6BA45AE43A2A08C
      CFA8F9CC9ACFB0191C6719E7E10CB2F4B3ECD2AE438E5D74E0F78F8636A99F9E
      637800FBF37467D9A55E879CB96DDF8297DB843A5AC872CFB24BBB0EF9D1FA63
      0F5EEBD3D027E96181741D32D0A3FCB3EC52AF439EB87C57E5ADD328027D3DB2
      D12F9474FFA14B6A56BEA961D5204359D7211538533EBE64FBB9DA7C1D128F44
      28EF67E5E736AF19E4B6F0E713E963BB79FA32D113CEB28F733E4391756A4B5A
      AE4FD56DB3D7196DD721F19D5F95BA67F32A5EB3D71E4E66A5E11D6BFBBA38A9
      5421447E7C1D32F697AB89B80EB91DD721D338DF069E077AAB02EBE8DDDA4E73
      BC0E3949D7FA957E0CDB59B6A7741D72DA10D645616F80CB22B0BAFFCE21ADFD
      F01FF78B58C8870FD3ABEC3FF0FBFAAE5DBA37C3330F86D27EFDD8B123F3355A
      EDF5BA75EA7DE598CF5BCC9E3E77FEECD70D1BE81B9E3973EA75BBD021929292
      DC50A556BD9297973B07678F18568F33ED44B8C1EB8C5A44E0823299CCA67732
      32332A92C31FE693F22B572F7FEDE5E92DF7F70B70C1736E11E4F06727C20AA7
      39B896B414134F0AF00F723A7FFEECE70E7A84DABE1C1418DC183760F0DCDFED
      9BD5AA55DBE298CF16D29EAE57B7FE3BA74E9FEC131810E4E3E5E5DDE3DAF5AB
      5D2B57AAB2353B3B3B00A7E6B35D9C5D29372F87EFC2BFA254AA72EC05112942
      8447F4D2753ADD9B26B371B946ADA5A4E43B5F5408ADF0DBD973A7E7050606BB
      0928909E9EFE638BE62D7F41B48893C7C4C490E39F8FB7CFA90B17133AB8E9DC
      435C5D5C3D8E1C3DDCC0CFCFBF374EF7B188596A4E8DEA35BBE129C54CC7321C
      2F41C47DA452AA4E3E7C9836127D26C3B348E1324146D8E4FC60E57B152B54DC
      C6058BC3DED98E197812E1445E7E5EAC5AAD92962A4A4D7B40C9C94909756AD7
      9DEFA8E7185738261CE341781BF4D6858471BE15C3488B357E71976B8DBC590B
      A3A38E63BC0451EE9D3B4179C9C901B7DE7D679E93A0A41329370D5532F35505
      4E9AA9E935EB6C150D4695AE468D0B4A1797D2B75A5E5252C0B9575F8D35FDF1
      4757A54221F0AA7079013E69AD162EED7175FCD88D9E390501377BF73EADC352
      747744315B3B68D027E153A7CE16E4720BD74CEAA3FC070FBC8F77E972C072F4
      6827E701031628FAF4F9C2D3D595743DFB2E0A68D1EA50CDEF5675CA9689B90A
      8D36571C3F7EAC253CFC4FC59A35EF9F9F3C3996491812D18577DFFD48959A5A
      D9FFA38F86D49D37EFD5A03E7D36982D163C80499229F5AAD7E0B4CADBEFAE51
      AB7D183E76ECD7F5376F6E97D5A0C166DD8103239376ECE8C84402164A561DA8
      5C39A3C0DFFF6CC18C194DAF5EBD2AABBC67CF928A972E0D9FE7EEDE292B24E4
      77566C74EAD4177DD4EA517151519DF3C2C377052814A10D172DBA2E4645ADAB
      B670E10BB4FCF3CFBD0F07068A735C5D77AB89825F542A3F3D171E2ECE727189
      23A21A406D861751E4DE90900C20AD81200C7421AA74B64A15F3865AB57EFBFA
      EBAF05EAD6AD9B6CA14E77EC3C0A1F0D0E362754AD2A7EA2D3EDD71235014103
      A071211A84130DD8191C7CEF66BD7AE23990A46235943E2AD534E6209807BA93
      90E073E1FDF7671E1B3BF6CB37A2A27A55AF56CDD5DBDBDB0DBB82076E71F093
      AD9E1C6759836AD502E6B56FFFEAB94993169F5BB060F88584045EA69EFEBF01
      00853609D12F92E3730000000049454E4400000000}
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
      Caption = 'Center With Zoom'
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
        Caption = 'Coordinates'
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
      object tbpmiShowPrevVersion: TTBXItem
        AutoCheck = True
        OnClick = tbpmiShowPrevVersionClick
        Caption = 'Show previous Version'
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
end
