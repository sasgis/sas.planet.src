object frmMain: TfrmMain
  Left = 488
  Top = 165
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'SAS.'#1055#1083#1072#1085#1077#1090#1072
  ClientHeight = 535
  ClientWidth = 842
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poDesigned
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object map: TImage32
    Left = 33
    Top = 59
    Width = 645
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
      Caption = #1043#1083#1072#1074#1085#1072#1103' '#1087#1072#1085#1077#1083#1100
      object TBmove: TTBXItem
        Checked = True
        ImageIndex = 8
        Images = PanelsImageList
        Options = [tboDefault]
        OnClick = TBmoveClick
        Caption = ''
        Hint = #1055#1077#1088#1077#1084#1077#1097#1072#1090#1100
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
        Hint = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
      end
      object TBCalcRas: TTBXItem
        AutoCheck = True
        ImageIndex = 9
        Images = PanelsImageList
        OnClick = TBCalcRasClick
        Caption = ''
        Hint = #1048#1079#1084#1077#1088#1080#1090#1100' '#1088#1072#1089#1089#1090#1086#1103#1085#1080#1077
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
        Hint = #1050#1072#1088#1090#1072' '#1079#1072#1087#1086#1083#1085#1077#1085#1080#1103' '#1089#1083#1086#1103
      end
      object TBGoTo: TTBXSubmenuItem
        DropdownCombo = True
        ImageIndex = 11
        Images = PanelsImageList
        Options = [tboShowHint]
        OnClick = TBSubmenuItem1Click
        Caption = ''
        Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1085#1077#1082#1086#1090#1086#1088#1086#1084#1091' '#1084#1077#1089#1090#1091
        object tbiEditYandexSrch: TTBEditItem
          EditCaption = #1071#1085#1076#1077#1082#1089
          Caption = #1071#1085#1076#1077#1082#1089
          Hint = ''
          EditCaption = #1071#1085#1076#1077#1082#1089
        end
        object tbiEditGoogleSrch: TTBEditItem
          EditCaption = 'Google!'
          EditWidth = 150
          Caption = 'Google!'
          Hint = ''
          EditCaption = 'Google!'
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
        Hint = #1042#1086' '#1074#1077#1089#1100' '#1101#1082#1088#1072#1085
      end
    end
    object SrcToolbar: TTBXToolbar
      Left = 242
      Top = 25
      DockPos = 224
      DockRow = 1
      Stretch = True
      TabOrder = 1
      Caption = #1055#1072#1085#1077#1083#1100' '#1080#1089#1090#1086#1095#1085#1080#1082#1086#1074
      object TBSrc: TTBXSubmenuItem
        ImageIndex = 0
        Images = PanelsImageList
        LinkSubitems = NSources
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = #1042#1099#1073#1077#1088#1080#1090#1077' '#1080#1089#1090#1086#1095#1085#1080#1082' '#1080#1079' '#1082#1086#1090#1086#1088#1086#1075#1086' '#1087#1088#1086#1075#1088#1072#1084#1084#1072' '#1073#1091#1076#1077#1090' '#1073#1088#1072#1090#1100' '#1082#1072#1088#1090#1099
      end
      object TBSMB: TTBXSubmenuItem
        DisplayMode = nbdmImageAndText
        ImageIndex = 3
        Images = PanelsImageList
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = #1042#1099#1073#1086#1088' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
      end
      object TBLayerSel: TTBXSubmenuItem
        ImageIndex = 3
        Images = PanelsImageList
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = #1042#1099#1073#1086#1088' '#1089#1083#1086#1077#1074' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1093' '#1087#1086#1074#1077#1088#1093' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
      end
    end
    object TBMarksToolbar: TTBXToolbar
      Left = 369
      Top = 25
      DockPos = 352
      DockRow = 1
      Images = PanelsImageList
      LinkSubitems = NMarks
      Stretch = True
      TabOrder = 2
      Caption = #1052#1077#1090#1082#1080
    end
    object GPSToolbar: TTBXToolbar
      Left = 540
      Top = 25
      DockPos = 504
      DockRow = 1
      Images = PanelsImageList
      Stretch = True
      TabOrder = 3
      Caption = #1055#1072#1085#1077#1083#1100' GPS'
      object TBGPSconn: TTBXItem
        AutoCheck = True
        ImageIndex = 14
        Images = PanelsImageList
        OnClick = TBGPSconnClick
        Caption = ''
        Hint = #1055#1086#1076#1082#1083#1102#1095#1080#1090#1100#1089#1103' '#1082' GPS '#1087#1088#1080#1077#1084#1085#1080#1082#1091
      end
      object TBGPSPath: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 6
        Images = PanelsImageList
        OnClick = TBGPSPathClick
        Caption = ''
        Hint = #1056#1080#1089#1086#1074#1072#1090#1100' '#1087#1088#1086#1081#1076#1077#1085#1085#1099#1081' '#1087#1091#1090#1100
        object TBXItem5: TTBXItem
          ImageIndex = 15
          Images = MenusImageList
          OnClick = TBXItem5Click
          Caption = #1055#1086#1089#1090#1072#1074#1080#1090#1100' '#1084#1077#1090#1082#1091
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
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1073#1072#1079#1077
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
          Caption = #1059#1076#1072#1083#1080#1090#1100' '#1090#1088#1077#1082
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
        Hint = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1087#1077#1088#1077#1084#1077#1097#1072#1090#1100' '#1082#1072#1088#1090#1091' '#1079#1072' '#1087#1086#1079#1080#1094#1080#1077#1081' GPS'
        object TBGPSToPointCenter: TTBXItem
          AutoCheck = True
          OnClick = TBGPSToPointCenterClick
          Caption = #1058#1077#1082#1091#1097#1072#1103' '#1087#1086#1079#1080#1094#1080#1103' '#1074#1089#1077#1075#1076#1072' '#1074' '#1094#1077#1085#1090#1088#1077
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
        OnClick = TBItem2Click
        Caption = ''
        Hint = #1042#1099#1081#1090#1080' '#1080#1079' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
      end
    end
    object TBXMainMenu: TTBXToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DockPos = 2
      Stretch = True
      TabOrder = 5
      Caption = #1043#1083#1072#1074#1085#1086#1077' '#1084#1077#1085#1102
      object NOperations: TTBXSubmenuItem
        Caption = '&'#1054#1087#1077#1088#1072#1094#1080#1080
        Hint = ''
        object N35: TTBXItem
          OnClick = N35Click
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1103#1088#1083#1099#1082
          Hint = ''
        end
        object TBXItem6: TTBXItem
          ImageIndex = 34
          Images = MenusImageList
          OnClick = TBXItem6Click
          Caption = #1054#1090#1082#1088#1099#1090#1100'...'
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
          Caption = #1059#1074#1077#1083#1080#1095#1080#1090#1100
          Hint = ''
        end
        object NZoomOut: TTBXItem
          ImageIndex = 24
          Images = MenusImageList
          ShortCut = 34
          OnClick = TBZoom_outClick
          Caption = #1059#1084#1077#1085#1100#1096#1080#1090#1100
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
          Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082'...'
          Hint = ''
        end
        object NCalcRast: TTBXItem
          ImageIndex = 9
          Images = MenusImageList
          ShortCut = 16460
          OnClick = NCalcRastClick
          Caption = #1048#1079#1084#1077#1088#1080#1090#1100' '#1088#1072#1089#1089#1090#1086#1103#1085#1080#1077
          Hint = ''
        end
        object TBXSeparatorItem8: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object NRectSave: TTBXSubmenuItem
          Caption = #1042#1099#1076#1077#1083#1080#1090#1100
          Hint = ''
          object TBRECT: TTBXItem
            ImageIndex = 10
            Images = PanelsImageList
            ShortCut = 32850
            OnClick = TBRECTClick
            Caption = #1055#1088#1103#1084#1086#1091#1075#1086#1083#1100#1085#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
            Hint = ''
          end
          object TBREGION: TTBXItem
            ImageIndex = 13
            Images = PanelsImageList
            ShortCut = 32848
            OnClick = TBREGIONClick
            Caption = #1055#1086#1083#1080#1075#1086#1085#1072#1083#1100#1085#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
            Hint = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1087#1086#1083#1080#1075#1086#1085#1072#1083#1100#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
          end
          object TBCOORD: TTBXItem
            ImageIndex = 12
            Images = PanelsImageList
            OnClick = TBCOORDClick
            Caption = #1055#1086' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1072#1084
            Hint = ''
          end
          object TBScreenSelect: TTBXItem
            ImageIndex = 20
            Images = PanelsImageList
            ShortCut = 16449
            OnClick = TBScreenSelectClick
            Caption = #1055#1086' '#1088#1072#1079#1084#1077#1088#1091' '#1101#1082#1088#1072#1085#1072
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
            Caption = #1055#1088#1077#1076#1099#1076#1091#1097#1077#1077' '#1074#1099#1076#1077#1083#1077#1085#1080#1077
            Hint = ''
          end
          object TBLoadSelFromFile: TTBXItem
            Images = PanelsImageList
            OnClick = TBLoadSelFromFileClick
            Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1080#1079' '#1092#1072#1081#1083#1072
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
          Caption = #1042#1099#1093#1086#1076
          Hint = ''
        end
      end
      object NView: TTBXSubmenuItem
        Caption = '&'#1042#1080#1076
        Hint = ''
        object NPanels: TTBXSubmenuItem
          Caption = #1055#1072#1085#1077#1083#1080
          Hint = ''
          object NMainToolBarShow: TTBXVisibilityToggleItem
            Control = TBMainToolBar
            Caption = #1043#1083#1072#1074#1085#1072#1103
            Hint = ''
          end
          object NZoomToolBarShow: TTBXVisibilityToggleItem
            Control = ZoomToolBar
            Caption = #1052#1072#1089#1096#1090#1072#1073
            Hint = ''
          end
          object NsrcToolBarShow: TTBXVisibilityToggleItem
            Control = SrcToolbar
            Caption = #1048#1089#1090#1086#1095#1085#1080#1082#1080
            Hint = ''
          end
          object NGPSToolBarShow: TTBXVisibilityToggleItem
            Control = GPSToolbar
            Caption = 'GPS'
            Hint = ''
          end
          object TBXVisibilityToggleItem1: TTBXVisibilityToggleItem
            Control = TBMarksToolbar
            Caption = #1052#1077#1090#1082#1080
            Hint = ''
          end
          object TBXVisibilityToggleItem2: TTBXVisibilityToggleItem
            Control = TBXToolBarSearch
            Caption = #1055#1086#1080#1089#1082
            Hint = ''
          end
          object NSensors: TTBXSubmenuItem
            AutoCheck = True
            DropdownCombo = True
            OnClick = NSensorsClick
            Caption = #1044#1072#1090#1095#1080#1082#1080
            Hint = ''
            object NSensorSpeedBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1057#1082#1086#1088#1086#1089#1090#1100
              Hint = ''
            end
            object NSensorSpeedAvgBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1057#1082#1086#1088#1086#1089#1090#1100' '#1089#1088#1077#1076#1085#1103#1103
              Hint = ''
            end
            object NSensorSpeedMaxBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1072#1103' '#1089#1082#1086#1088#1086#1089#1090#1100
              Hint = ''
            end
            object NSensorPathBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1055#1088#1086#1081#1076#1077#1085#1085#1099#1081' '#1087#1091#1090#1100
              Hint = ''
            end
            object NsensorOdometrBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1054#1076#1086#1084#1077#1090#1088
              Hint = ''
            end
            object NsensorOdometr2Bar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1054#1076#1086#1084#1077#1090#1088' '#8470'2'
              Hint = ''
            end
            object NSensorLenToMarkBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1056#1072#1089#1089#1090#1086#1103#1085#1080#1077' '#1076#1086' '#1084#1077#1090#1082#1080
              Hint = ''
            end
            object NSensorBattaryBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1041#1072#1090#1072#1088#1077#1103
              Hint = ''
            end
            object NSensorAltitudeBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1042#1099#1089#1086#1090#1072
              Hint = ''
            end
            object NSensorAzimutBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1040#1079#1080#1084#1091#1090
              Hint = ''
            end
            object NSignalStrengthBar: TTBXItem
              AutoCheck = True
              Checked = True
              OnClick = NSensorsBarClick
              Caption = #1059#1088#1086#1074#1085#1080' '#1089#1080#1075#1085#1072#1083#1072
              Hint = ''
            end
          end
        end
        object N31: TTBXSubmenuItem
          Caption = #1069#1083#1077#1084#1077#1085#1090#1099' '#1080#1085#1090#1077#1088#1092#1077#1081#1089#1072
          Hint = ''
          object Showstatus: TTBXItem
            AutoCheck = True
            ShortCut = 32851
            OnClick = ShowstatusClick
            Caption = #1057#1090#1088#1086#1082#1072' '#1089#1090#1072#1090#1091#1089#1072
            Hint = ''
          end
          object ShowMiniMap: TTBXItem
            AutoCheck = True
            ShortCut = 32845
            OnClick = ShowMiniMapClick
            Caption = #1050#1072#1088#1090#1072' '#1086#1073#1079#1086#1088#1072
            Hint = ''
          end
          object ShowLine: TTBXItem
            AutoCheck = True
            ShortCut = 32844
            OnClick = ShowLineClick
            Caption = #1051#1080#1085#1077#1081#1082#1072
            Hint = ''
          end
        end
        object NFillMap: TTBXSubmenuItem
          ImageIndex = 7
          Images = MenusImageList
          OnClick = NFillMapClick
          Caption = #1050#1072#1088#1090#1072' '#1079#1072#1087#1086#1083#1085#1077#1085#1080#1103
          Hint = ''
          object TBFillingTypeMap: TTBXSubmenuItem
            Options = [tboDropdownArrow]
            Caption = #1060#1086#1088#1084#1080#1088#1086#1074#1072#1090#1100' '#1076#1083#1103'...'
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
            RowCount = 5
            OnCellClick = TBXToolPalette1CellClick
            Caption = ''
            Hint = ''
          end
        end
        object NShowGran: TTBXSubmenuItem
          ImageIndex = 3
          Images = MenusImageList
          OnClick = NShowGranClick
          Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1075#1088#1072#1085#1080#1094#1099' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1081
          Hint = ''
          object N000: TTBXItem
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = #1053#1077#1090
            Hint = ''
          end
          object N001: TTBXItem
            Tag = 99
            GroupIndex = 1
            RadioItem = True
            OnClick = N000Click
            Caption = #1040#1082#1090#1080#1074#1085#1099#1081' '#1084#1072#1089#1096#1090#1072#1073
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
          Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1073#1083#1072#1085#1082#1086#1074#1082#1091' '#1082#1072#1088#1090' '#1043#1064
          Hint = ''
          object NGShScale0: TTBXItem
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = #1053#1077#1090
            Hint = ''
          end
          object NGShScale1000000: TTBXItem
            Tag = 1000000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:1 000 000 (10 '#1082#1084')'
            Hint = ''
          end
          object NGShScale500000: TTBXItem
            Tag = 500000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:500 000 (5 '#1082#1084')'
            Hint = ''
          end
          object NGShScale200000: TTBXItem
            Tag = 200000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:200 000 (2 '#1082#1084')'
            Hint = ''
          end
          object NGShScale100000: TTBXItem
            Tag = 100000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:100 000 (1 '#1082#1084')'
            Hint = ''
          end
          object NGShScale50000: TTBXItem
            Tag = 50000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:50 000 (500 '#1084')'
            Hint = ''
          end
          object NGShScale25000: TTBXItem
            Tag = 25000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:25 000 (250 '#1084')'
            Hint = ''
          end
          object NGShScale10000: TTBXItem
            Tag = 10000
            AutoCheck = True
            GroupIndex = 1
            RadioItem = True
            OnClick = NGShScale01Click
            Caption = '1:10 000 (100 '#1084')'
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
          OnClick = NFoolSizeClick
          Caption = #1042#1086' '#1074#1077#1089#1100' '#1101#1082#1088#1072#1085
          Hint = ''
        end
        object NGoToCur: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NGoToCurClick
          Caption = #1055#1077#1088#1077#1084#1077#1097#1072#1090#1100' '#1082' '#1082#1091#1088#1089#1086#1088#1091
          Hint = ''
        end
        object Nbackload: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NbackloadClick
          Caption = #1041#1088#1072#1090#1100' '#1082#1072#1088#1090#1099' '#1080#1079' '#1084#1077#1085#1100#1096#1080#1093' '#1084#1072#1089#1096#1090#1072#1073#1086#1074
          Hint = ''
        end
        object NbackloadLayer: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NbackloadLayerClick
          Caption = #1041#1088#1072#1090#1100' '#1089#1083#1086#1080' '#1080#1079' '#1084#1077#1085#1100#1096#1080#1093' '#1084#1072#1089#1096#1090#1072#1073#1086#1074
          Hint = ''
        end
        object Nanimate: TTBXItem
          AutoCheck = True
          Checked = True
          OnClick = NanimateClick
          Caption = #1040#1085#1080#1084#1072#1094#1080#1103' '#1087#1088#1080' '#1084#1072#1089#1096#1090#1072#1073#1080#1088#1086#1074#1072#1085#1080#1080
          Hint = ''
        end
        object N32: TTBXItem
          AutoCheck = True
          OnClick = N32Click
          Caption = #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1096#1082#1072#1083#1091
          Hint = ''
        end
        object Ninvertcolor: TTBXItem
          AutoCheck = True
          ShortCut = 32846
          OnClick = NinvertcolorClick
          Caption = #1053#1086#1095#1085#1086#1081' '#1088#1077#1078#1080#1084' ('#1048#1085#1074#1077#1088#1089#1080#1103' '#1094#1074#1077#1090#1086#1074')'
          Hint = ''
        end
        object NShowSelection: TTBXItem
          AutoCheck = True
          OnClick = NShowSelectionClick
          Caption = #1055#1086#1089#1083#1077#1076#1085#1077#1077' '#1074#1099#1076#1077#1083#1077#1085#1080#1077
          Hint = ''
        end
        object tbitmShowDebugInfo: TTBXItem
          Visible = False
          OnClick = tbitmShowDebugInfoClick
          Caption = 'ShowDebugInfo'
          Hint = ''
        end
      end
      object NSources: TTBXSubmenuItem
        Caption = '&'#1048#1089#1090#1086#1095#1085#1080#1082
        Hint = ''
        object NSRCesh: TTBXItem
          Tag = 1
          AutoCheck = True
          GroupIndex = 1
          ImageIndex = 1
          Images = PanelsImageList
          RadioItem = True
          ShortCut = 32835
          OnClick = NSRCinetClick
          Caption = #1050#1101#1096
          Hint = ''
        end
        object NSRCinet: TTBXItem
          AutoCheck = True
          GroupIndex = 1
          ImageIndex = 0
          Images = PanelsImageList
          RadioItem = True
          ShortCut = 32841
          OnClick = NSRCinetClick
          Caption = #1048#1085#1090#1077#1088#1085#1077#1090
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
          OnClick = NSRCinetClick
          Caption = #1048#1085#1090#1077#1088#1085#1077#1090' '#1080' '#1082#1101#1096
          Hint = ''
        end
      end
      object NSMB: TTBXSubmenuItem
        LinkSubitems = TBSMB
        Caption = '&'#1050#1072#1088#1090#1099
        Hint = ''
      end
      object NLayerSel: TTBXSubmenuItem
        LinkSubitems = TBLayerSel
        Caption = #1057#1083#1086#1080
        Hint = ''
      end
      object NMarks: TTBXSubmenuItem
        Images = MenusImageList
        Caption = #1052#1077#1090#1082#1080
        Hint = ''
        object TBAdd_Point: TTBXItem
          GroupIndex = 1
          ImageIndex = 15
          Images = PanelsImageList
          Options = [tboShowHint]
          Stretch = True
          OnClick = TBAdd_PointClick
          Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1084#1077#1090#1082#1091
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1091#1102' '#1084#1077#1090#1082#1091
        end
        object TBAdd_Line: TTBXItem
          ImageIndex = 16
          Images = PanelsImageList
          MaskOptions = [tboShowHint]
          OnClick = TBAdd_LineClick
          Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1091#1090#1100
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1091#1090#1100
        end
        object TBAdd_Poly: TTBXItem
          ImageIndex = 17
          Images = PanelsImageList
          Options = [tboShowHint]
          OnClick = TBAdd_PolyClick
          Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1083#1080#1075#1086#1085
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1083#1080#1075#1086#1085
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
          Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1084#1077#1090#1082#1072#1084#1080
          Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1084#1077#1090#1082#1072#1084#1080
        end
        object TBHideMarks: TTBXItem
          AutoCheck = True
          ImageIndex = 19
          Images = PanelsImageList
          OnClick = TBHideMarksClick
          Caption = #1057#1082#1088#1099#1090#1100' '#1074#1089#1077' '#1084#1077#1090#1082#1080
          Hint = #1057#1082#1088#1099#1090#1100' '#1074#1089#1077' '#1084#1077#1090#1082#1080
        end
      end
      object tbsbmGPS: TTBXSubmenuItem
        Caption = 'GPS'
        Hint = ''
        object tbitmGPSConnect: TTBXItem
          ImageIndex = 14
          Images = MenusImageList
          ShortCut = 49223
          OnClick = TBGPSconnClick
          Caption = #1055#1086#1076#1082#1083#1102#1095#1080#1090#1100' '#1087#1088#1080#1077#1084#1085#1080#1082
          Hint = ''
        end
        object tbitmGPSTrackShow: TTBXItem
          AutoCheck = True
          ImageIndex = 6
          Images = MenusImageList
          ShortCut = 49236
          OnClick = TBGPSPathClick
          Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1090#1088#1077#1082
          Hint = ''
        end
        object tbitmGPSCenterMap: TTBXItem
          AutoCheck = True
          ImageIndex = 5
          Images = MenusImageList
          OnClick = TBGPSToPointClick
          Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1087#1077#1088#1077#1084#1077#1097#1072#1090#1100' '#1082#1072#1088#1090#1091
          Hint = ''
        end
        object tbitmGPSToPointCenter: TTBXItem
          AutoCheck = True
          OnClick = TBGPSToPointCenterClick
          Caption = #1058#1077#1082#1091#1097#1072#1103' '#1087#1086#1079#1080#1094#1080#1103' '#1074#1089#1077#1075#1076#1072' '#1074' '#1094#1077#1085#1090#1088#1077
          Hint = ''
        end
        object tbsprtGPS1: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmGPSTrackSave: TTBXItem
          ImageIndex = 25
          Images = MenusImageList
          ShortCut = 49235
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1090#1088#1077#1082
          Hint = ''
        end
        object tbitmGPSTrackSaveToDb: TTBXItem
          ImageIndex = 25
          Images = MenusImageList
          OnClick = TBItem5Click
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1090#1088#1077#1082' '#1074' '#1073#1072#1079#1077
          Hint = ''
        end
        object tbitmGPSTrackClear: TTBXItem
          ImageIndex = 35
          Images = MenusImageList
          OnClick = TBItemDelTrackClick
          Caption = #1059#1076#1072#1083#1080#1090#1100' '#1090#1088#1077#1082
          Hint = ''
        end
        object tbsprtGPS2: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object tbitmPositionByGSM: TTBXItem
          OnClick = tbitmPositionByGSMClick
          Caption = #1054#1087#1088#1077#1076#1077#1083#1080#1090#1100' '#1087#1086#1079#1080#1094#1080#1102' '#1087#1086' '#1073#1072#1079#1086#1074#1099#1084' '#1089#1090#1072#1085#1094#1080#1103#1084' '#1089#1086#1090#1086#1074#1086#1081' '#1089#1077#1090#1080
          Hint = ''
        end
      end
      object NParams: TTBXSubmenuItem
        OnClick = NParamsClick
        Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
        Hint = ''
        object NMapParams: TTBXItem
          ShortCut = 49232
          OnClick = NMapParamsClick
          Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1082#1072#1088#1090#1099
          Hint = ''
        end
        object NLayerParams: TTBXSubmenuItem
          Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099' '#1089#1083#1086#1103
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
          Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
          Hint = ''
        end
        object TBLang: TTBXSubmenuItem
          Caption = #1071#1079#1099#1082
          Hint = ''
          object TBXLangRus: TTBXItem
            OnClick = TBItem1Click
            Caption = #1056#1091#1089#1089#1082#1080#1081
            Hint = ''
          end
          object TBXLangEng: TTBXItem
            Tag = 1
            OnClick = TBItem1Click
            Caption = 'English'
            Hint = ''
          end
        end
      end
      object NHelp: TTBXSubmenuItem
        Caption = '&'#1055#1086#1084#1086#1097#1100
        Hint = ''
        object N29: TTBXItem
          ImageIndex = 26
          Images = MenusImageList
          ShortCut = 112
          OnClick = N29Click
          Caption = #1057#1087#1088#1072#1074#1082#1072' (http://sasgis.ru/wikisasiya)'
          Hint = ''
        end
        object N16: TTBXItem
          ImageIndex = 27
          Images = MenusImageList
          OnClick = N16Click
          Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
          Hint = ''
        end
        object TBXSeparatorItem15: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object NGoToSite: TTBXItem
          OnClick = NGoToSiteClick
          Caption = #1057#1072#1081#1090' '#1087#1088#1086#1075#1088#1072#1084#1084#1099' (http://sasgis.ru)'
          Hint = ''
        end
        object tbtmHelpBugTrack: TTBXItem
          OnClick = tbtmHelpBugTrackClick
          Caption = #1041#1072#1075#1090#1088#1077#1082#1077#1088' (http://sasgis.ru/mantis)'
          Hint = ''
        end
        object NGoToForum: TTBXItem
          OnClick = NGoToForumClick
          Caption = #1054#1073#1089#1091#1078#1076#1077#1085#1080#1077' (http://sasgis.ru/forum)'
          Hint = ''
        end
      end
    end
    object TBXToolBarSearch: TTBXToolbar
      Left = 430
      Top = 0
      DockPos = 430
      Stretch = True
      TabOrder = 6
      Visible = False
      Caption = #1055#1072#1085#1077#1083#1100' '#1087#1086#1080#1089#1082#1072
      object TBXSelectSrchType: TTBXSubmenuItem
        Options = [tboDropdownArrow]
        Caption = 'Google'
        Hint = ''
        object TBXSelectYandexSrch: TTBXItem
          GroupIndex = 1
          RadioItem = True
          Caption = #1071#1085#1076#1077#1082#1089
          Hint = ''
        end
        object TBXSelectGoogleSrch: TTBXItem
          GroupIndex = 1
          RadioItem = True
          Caption = 'Google'
          Hint = ''
        end
      end
      object TBXSearchEdit: TTBXEditItem
        EditCaption = #1055#1086#1080#1089#1082
        EditWidth = 150
        OnAcceptText = TBXSearchEditAcceptText
        Caption = ''
        Hint = ''
        EditCaption = #1055#1086#1080#1089#1082
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
    Width = 33
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
      Caption = #1055#1072#1085#1077#1083#1100' '#1084#1072#1089#1096#1090#1072#1073#1072
      object TBZoomIn: TTBXItem
        ImageIndex = 23
        Images = MenusImageList
        MinHeight = 29
        MinWidth = 29
        OnClick = TBZoomInClick
        Caption = ''
        Hint = #1059#1074#1077#1083#1080#1095#1080#1090#1100
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
        Hint = #1059#1084#1077#1085#1100#1096#1080#1090#1100
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
        Hint = #1052#1072#1089#1096#1090#1072#1073' '#1082#1072#1088#1090#1099' '#1085#1072' '#1082#1086#1090#1086#1088#1086#1084' '#1074#1099' '#1085#1072#1093#1086#1076#1080#1090#1077#1089#1100
        Alignment = taCenter
        Caption = 'z1'
        Color = clBtnFace
        Font.Charset = RUSSIAN_CHARSET
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
        Hint = #1059#1076#1072#1083#1080#1090#1100' '#1090#1086#1095#1082#1091
      end
      object TBEditPathLabel: TTBXItem
        ImageIndex = 37
        Images = MenusImageList
        OnClick = TBEditPathLabelClick
        Caption = ''
        Hint = #1057#1082#1088#1099#1090#1100'/'#1055#1086#1082#1072#1079#1072#1090#1100' '#1087#1086#1076#1087#1080#1089#1080
      end
      object TBEditPathSave: TTBXItem
        ImageIndex = 25
        Images = MenusImageList
        OnClick = TBEditPathSaveClick
        Caption = ''
        Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1073#1072#1079#1077
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
        Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1086#1082#1085#1091' '#1086#1087#1077#1088#1072#1094#1080#1081' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
      end
      object TBEditPathMarsh: TTBXSubmenuItem
        ImageIndex = 39
        Images = MenusImageList
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = #1055#1088#1086#1083#1086#1078#1080#1090#1100' '#1084#1072#1088#1096#1088#1091#1090
        object TBXLabelItem2: TTBXLabelItem
          FontSettings.Bold = tsTrue
          Margin = 4
          ShowAccelChar = False
          Caption = #1057#1088#1077#1076#1089#1090#1074#1072#1084#1080' '#1050#1072#1088#1090#1099'@mail.ru'
          Hint = ''
        end
        object TBItem8: TTBXItem
          Tag = 1
          OnClick = TBEditPathMarshClick
          Caption = #1055#1086' '#1088#1072#1089#1089#1090#1086#1103#1085#1080#1102
          Hint = ''
        end
        object TBItem9: TTBXItem
          Tag = 2
          OnClick = TBEditPathMarshClick
          Caption = #1055#1086' '#1074#1088#1077#1084#1077#1085#1080
          Hint = ''
        end
        object TBItem7: TTBXItem
          Tag = 3
          OnClick = TBEditPathMarshClick
          Caption = #1055#1086' '#1074#1088#1077#1084#1077#1085#1080' '#1089' '#1091#1095#1077#1090#1086#1084' '#1087#1088#1086#1073#1086#1082
          Hint = ''
        end
        object TBXLabelItem1: TTBXLabelItem
          FontSettings.Bold = tsTrue
          Margin = 4
          ShowAccelChar = False
          Caption = #1057#1088#1077#1076#1089#1090#1074#1072#1084#1080' yournavigation.org (OSM)'
          Hint = ''
        end
        object TBXItem1: TTBXItem
          Tag = 1
          OnClick = TBXItem1Click
          Caption = #1053#1072' '#1072#1074#1090#1086#1084#1086#1073#1080#1083#1077' ('#1087#1086' '#1089#1082#1086#1088#1086#1089#1090#1080')'
          Hint = ''
        end
        object TBXItem2: TTBXItem
          Tag = 11
          OnClick = TBXItem1Click
          Caption = #1053#1072' '#1072#1074#1090#1086#1084#1086#1073#1080#1083#1077' ('#1087#1086' '#1076#1083#1080#1085#1077')'
          Hint = ''
        end
        object TBXItem4: TTBXItem
          Tag = 2
          OnClick = TBXItem1Click
          Caption = #1053#1072' '#1074#1077#1083#1086#1089#1080#1087#1077#1076#1077' ('#1087#1086' '#1089#1082#1086#1088#1086#1089#1090#1080')'
          Hint = ''
        end
        object TBXItem3: TTBXItem
          Tag = 22
          OnClick = TBXItem1Click
          Caption = #1053#1072' '#1074#1077#1083#1086#1089#1080#1087#1077#1076#1077' ('#1087#1086' '#1076#1083#1080#1085#1077')'
          Hint = ''
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
      Caption = #1044#1072#1090#1095#1080#1082#1080
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 0
        Width = 160
        Height = 396
        Align = alTop
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
          Height = 396
          object TBXSensorSpeedAvgBar: TTBXToolWindow
            Left = 0
            Top = 36
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1089#1088#1077#1076#1085#1102#1102' '#1089#1082#1086#1088#1086#1089#1090#1100' '#1076#1074#1080#1078#1077#1085#1080#1103
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 34
            DockRow = 1
            Stretch = True
            TabOrder = 0
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1057#1082#1086#1088#1086#1089#1090#1100' '#1089#1088#1077#1076#1085#1103#1103
            object SBClearSensor: TSpeedButton
              Tag = 1
              Left = 132
              Top = 1
              Width = 17
              Height = 12
              Hint = #1057#1073#1088#1086#1089#1080#1090#1100
              Anchors = [akTop, akRight]
              Flat = True
              Glyph.Data = {
                46020000424D460200000000000036000000280000000F0000000B0000000100
                1800000000001002000000000000000000000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF8FCD7F80CC80FF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF40B340109F108FD28CFF
                00FFAFDFA640B34040B34040B34040B34040B34040B340000000FF00FFFF00FF
                FF00FF109F10009900FF00FFFF00FFFF00FF80CC7D0099000099000099000099
                0020A620FF00FF000000FF00FFFF00FF00990000990080CC80FF00FFFF00FFFF
                00FF70C670009900009900009900009900009900FF00FF000000FF00FF50B950
                009900009900FF00FFFF00FFFF00FFFF00FF40B3400099000099000099000099
                000099008FD28E000000FF00FF009900009900009900FF00FFAFDFAB109F10FF
                00FF40B34030AC30FF00FF40B34000990000990080CC7B000000FF00FF009900
                00990000990020A620009900009900FF00FF70C66EFF00FFFF00FF80CC800099
                00009900AFDFA3000000FF00FF40B340009900009900009900009900009900FF
                00FFFF00FFFF00FFFF00FF20A62000990020A620FF00FF000000FF00FF80CB7C
                009900009900009900009900009900FF00FFFF00FFFF00FF8FD189009900109F
                10FF00FFFF00FF00000060BE5C00990000990000990000990000990040B340FF
                00FFFF00FF8FCE8300990030AC30FF00FFFF00FFFF00FF000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFAFD69850B95050B9509FD28DFF00FFFF00
                FFFF00FFFF00FF000000}
              Margin = 0
              Spacing = 0
              OnClick = SBClearSensorClick
            end
            object TBXSensorSpeedAvg: TTBXLabel
              Left = 0
              Top = 13
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel9: TTBXLabel
              Left = 0
              Top = 0
              Width = 129
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              Wrapping = twEndEllipsis
              Caption = #1057#1082#1086#1088#1086#1089#1090#1100' '#1089#1088#1077#1076'., '#1082#1084'/'#1095':'
            end
          end
          object TBXSensorSpeedBar: TTBXToolWindow
            Left = 0
            Top = 0
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1090#1077#1082#1091#1097#1091#1102' '#1089#1082#1086#1088#1086#1089#1090#1100' '#1076#1074#1080#1078#1077#1085#1080#1103
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            Stretch = True
            TabOrder = 1
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1057#1082#1086#1088#1086#1089#1090#1100
            object TBXSensorSpeed: TTBXLabel
              Left = 0
              Top = 13
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel8: TTBXLabel
              Left = 0
              Top = 0
              Width = 145
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              ParentShowHint = False
              ShowHint = True
              Wrapping = twEndEllipsis
              Caption = #1057#1082#1086#1088#1086#1089#1090#1100', '#1082#1084'/'#1095':'
            end
          end
          object TBXSensorPathBar: TTBXToolWindow
            Left = 0
            Top = 108
            Hint = 
              #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1087#1088#1086#1081#1076#1077#1085#1085#1099#1081' '#1087#1091#1090#1100' '#1089#1095#1080#1090#1072#1077#1084#1099#1081' '#1086#1090' '#1087#1086#1076#1082#1083#1102#1095#1077#1085#1080#1103' '#1082' GPS-'#1087#1088#1080#1077#1084#1085 +
              #1080#1082#1091
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 10
            DockRow = 3
            Stretch = True
            TabOrder = 2
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1055#1088#1086#1081#1076#1077#1085#1085#1099#1081' '#1087#1091#1090#1100
            object SpeedButton3: TSpeedButton
              Tag = 2
              Left = 132
              Top = 1
              Width = 17
              Height = 12
              Hint = #1057#1073#1088#1086#1089#1080#1090#1100
              Anchors = [akTop, akRight]
              Flat = True
              Glyph.Data = {
                46020000424D460200000000000036000000280000000F0000000B0000000100
                1800000000001002000000000000000000000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF8FCD7F80CC80FF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF40B340109F108FD28CFF
                00FFAFDFA640B34040B34040B34040B34040B34040B340000000FF00FFFF00FF
                FF00FF109F10009900FF00FFFF00FFFF00FF80CC7D0099000099000099000099
                0020A620FF00FF000000FF00FFFF00FF00990000990080CC80FF00FFFF00FFFF
                00FF70C670009900009900009900009900009900FF00FF000000FF00FF50B950
                009900009900FF00FFFF00FFFF00FFFF00FF40B3400099000099000099000099
                000099008FD28E000000FF00FF009900009900009900FF00FFAFDFAB109F10FF
                00FF40B34030AC30FF00FF40B34000990000990080CC7B000000FF00FF009900
                00990000990020A620009900009900FF00FF70C66EFF00FFFF00FF80CC800099
                00009900AFDFA3000000FF00FF40B340009900009900009900009900009900FF
                00FFFF00FFFF00FFFF00FF20A62000990020A620FF00FF000000FF00FF80CB7C
                009900009900009900009900009900FF00FFFF00FFFF00FF8FD189009900109F
                10FF00FFFF00FF00000060BE5C00990000990000990000990000990040B340FF
                00FFFF00FF8FCE8300990030AC30FF00FFFF00FFFF00FF000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFAFD69850B95050B9509FD28DFF00FFFF00
                FFFF00FFFF00FF000000}
              Margin = 0
              Spacing = 0
              OnClick = SBClearSensorClick
            end
            object TBXOdometrNow: TTBXLabel
              Left = 0
              Top = 13
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel10: TTBXLabel
              Left = 0
              Top = 0
              Width = 129
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              Wrapping = twEndEllipsis
              Caption = #1055#1088#1086#1081#1076#1077#1085#1085#1099#1081' '#1087#1091#1090#1100':'
            end
          end
          object TBXSensorBattaryBar: TTBXToolWindow
            Left = 0
            Top = 288
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1089#1086#1089#1090#1086#1103#1085#1080#1077' '#1087#1080#1090#1072#1085#1080#1103
            Align = alTop
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 6
            DockRow = 8
            Stretch = True
            TabOrder = 3
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1041#1072#1090#1072#1088#1077#1103
            object TBXSensorBattary: TTBXLabel
              Left = 0
              Top = 13
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel14: TTBXLabel
              Left = 0
              Top = 0
              Width = 145
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Wrapping = twEndEllipsis
              Caption = #1041#1072#1090#1072#1088#1077#1103':'
            end
          end
          object TBXSensorLenToMarkBar: TTBXToolWindow
            Left = 0
            Top = 216
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1088#1072#1089#1089#1090#1086#1103#1085#1080#1077' '#1076#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1081' '#1084#1077#1090#1082#1080
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 32
            DockRow = 6
            Stretch = True
            TabOrder = 4
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1056#1072#1089#1089#1090#1086#1103#1085#1080#1077' '#1076#1086' '#1084#1077#1090#1082#1080
            object TBXSensorLenToMark: TTBXLabel
              Left = 0
              Top = 13
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel13: TTBXLabel
              Left = 0
              Top = 0
              Width = 145
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              Wrapping = twEndEllipsis
              Caption = #1056#1072#1089#1089#1090#1086#1103#1085#1080#1077' '#1076#1086' '#1084#1077#1090#1082#1080':'
            end
          end
          object TBXSensorAltitudeBar: TTBXToolWindow
            Left = 0
            Top = 252
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1074#1099#1089#1086#1090#1091' '#1085#1072#1076' '#1091#1088#1086#1074#1085#1077#1084' '#1084#1086#1088#1103' '#1087#1086' '#1076#1072#1085#1085#1099#1084' GPS-'#1087#1088#1080#1077#1084#1085#1080#1082#1072
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 6
            DockRow = 7
            Stretch = True
            TabOrder = 5
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1042#1099#1089#1086#1090#1072
            object SpeedButton4: TSpeedButton
              Tag = 2
              Left = 250
              Top = 1
              Width = 17
              Height = 12
              Hint = #1057#1073#1088#1086#1089#1080#1090#1100
              Anchors = [akTop, akRight]
              Flat = True
              Glyph.Data = {
                46020000424D460200000000000036000000280000000F0000000B0000000100
                1800000000001002000000000000000000000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF8FCD7F80CC80FF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF40B340109F108FD28CFF
                00FFAFDFA640B34040B34040B34040B34040B34040B340000000FF00FFFF00FF
                FF00FF109F10009900FF00FFFF00FFFF00FF80CC7D0099000099000099000099
                0020A620FF00FF000000FF00FFFF00FF00990000990080CC80FF00FFFF00FFFF
                00FF70C670009900009900009900009900009900FF00FF000000FF00FF50B950
                009900009900FF00FFFF00FFFF00FFFF00FF40B3400099000099000099000099
                000099008FD28E000000FF00FF009900009900009900FF00FFAFDFAB109F10FF
                00FF40B34030AC30FF00FF40B34000990000990080CC7B000000FF00FF009900
                00990000990020A620009900009900FF00FF70C66EFF00FFFF00FF80CC800099
                00009900AFDFA3000000FF00FF40B340009900009900009900009900009900FF
                00FFFF00FFFF00FFFF00FF20A62000990020A620FF00FF000000FF00FF80CB7C
                009900009900009900009900009900FF00FFFF00FFFF00FF8FD189009900109F
                10FF00FFFF00FF00000060BE5C00990000990000990000990000990040B340FF
                00FFFF00FF8FCE8300990030AC30FF00FFFF00FFFF00FF000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFAFD69850B95050B9509FD28DFF00FFFF00
                FFFF00FFFF00FF000000}
              Margin = 0
              Spacing = 0
              OnClick = SBClearSensorClick
            end
            object TBXSensorAltitude: TTBXLabel
              Left = 0
              Top = 13
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel2: TTBXLabel
              Left = 0
              Top = 0
              Width = 145
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              Wrapping = twEndEllipsis
              Caption = #1042#1099#1089#1086#1090#1072', '#1084':'
            end
          end
          object TBXSensorSpeedMaxBar: TTBXToolWindow
            Left = 0
            Top = 72
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1084#1072#1082#1089#1080#1084#1072#1083#1100#1085#1091#1102' '#1089#1082#1086#1088#1086#1089#1090#1100' '#1076#1074#1080#1078#1077#1085#1080#1103
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 0
            DockRow = 2
            Stretch = True
            TabOrder = 6
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1057#1082#1086#1088#1086#1089#1090#1100' '#1084#1072#1082#1089#1080#1084#1072#1083#1100#1085#1072#1103
            object SpeedButton5: TSpeedButton
              Tag = 1
              Left = 250
              Top = 1
              Width = 17
              Height = 12
              Hint = #1057#1073#1088#1086#1089#1080#1090#1100
              Anchors = [akTop, akRight]
              Flat = True
              Glyph.Data = {
                46020000424D460200000000000036000000280000000F0000000B0000000100
                1800000000001002000000000000000000000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF8FCD7F80CC80FF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF40B340109F108FD28CFF
                00FFAFDFA640B34040B34040B34040B34040B34040B340000000FF00FFFF00FF
                FF00FF109F10009900FF00FFFF00FFFF00FF80CC7D0099000099000099000099
                0020A620FF00FF000000FF00FFFF00FF00990000990080CC80FF00FFFF00FFFF
                00FF70C670009900009900009900009900009900FF00FF000000FF00FF50B950
                009900009900FF00FFFF00FFFF00FFFF00FF40B3400099000099000099000099
                000099008FD28E000000FF00FF009900009900009900FF00FFAFDFAB109F10FF
                00FF40B34030AC30FF00FF40B34000990000990080CC7B000000FF00FF009900
                00990000990020A620009900009900FF00FF70C66EFF00FFFF00FF80CC800099
                00009900AFDFA3000000FF00FF40B340009900009900009900009900009900FF
                00FFFF00FFFF00FFFF00FF20A62000990020A620FF00FF000000FF00FF80CB7C
                009900009900009900009900009900FF00FFFF00FFFF00FF8FD189009900109F
                10FF00FFFF00FF00000060BE5C00990000990000990000990000990040B340FF
                00FFFF00FF8FCE8300990030AC30FF00FFFF00FFFF00FF000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFAFD69850B95050B9509FD28DFF00FFFF00
                FFFF00FFFF00FF000000}
              Margin = 0
              Spacing = 0
              OnClick = SBClearSensorClick
            end
            object SpeedButton6: TSpeedButton
              Tag = 4
              Left = 132
              Top = 1
              Width = 17
              Height = 12
              Hint = #1057#1073#1088#1086#1089#1080#1090#1100
              Anchors = [akTop, akRight]
              Flat = True
              Glyph.Data = {
                46020000424D460200000000000036000000280000000F0000000B0000000100
                1800000000001002000000000000000000000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF8FCD7F80CC80FF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF40B340109F108FD28CFF
                00FFAFDFA640B34040B34040B34040B34040B34040B340000000FF00FFFF00FF
                FF00FF109F10009900FF00FFFF00FFFF00FF80CC7D0099000099000099000099
                0020A620FF00FF000000FF00FFFF00FF00990000990080CC80FF00FFFF00FFFF
                00FF70C670009900009900009900009900009900FF00FF000000FF00FF50B950
                009900009900FF00FFFF00FFFF00FFFF00FF40B3400099000099000099000099
                000099008FD28E000000FF00FF009900009900009900FF00FFAFDFAB109F10FF
                00FF40B34030AC30FF00FF40B34000990000990080CC7B000000FF00FF009900
                00990000990020A620009900009900FF00FF70C66EFF00FFFF00FF80CC800099
                00009900AFDFA3000000FF00FF40B340009900009900009900009900009900FF
                00FFFF00FFFF00FFFF00FF20A62000990020A620FF00FF000000FF00FF80CB7C
                009900009900009900009900009900FF00FFFF00FFFF00FF8FD189009900109F
                10FF00FFFF00FF00000060BE5C00990000990000990000990000990040B340FF
                00FFFF00FF8FCE8300990030AC30FF00FFFF00FFFF00FF000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFAFD69850B95050B9509FD28DFF00FFFF00
                FFFF00FFFF00FF000000}
              Margin = 0
              Spacing = 0
              OnClick = SBClearSensorClick
            end
            object TBXSensorSpeedMax: TTBXLabel
              Left = 0
              Top = 13
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel3: TTBXLabel
              Left = 0
              Top = 0
              Width = 129
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              Wrapping = twEndEllipsis
              Caption = #1057#1082#1086#1088#1086#1089#1090#1100' '#1084#1072#1082#1089'., '#1082#1084'/'#1095':'
            end
          end
          object TBXsensorOdometrBar: TTBXToolWindow
            Left = 0
            Top = 144
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1074#1077#1089#1100' '#1087#1088#1086#1081#1076#1077#1085#1085#1099#1081' '#1087#1091#1090#1100
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 18
            DockRow = 4
            Stretch = True
            TabOrder = 7
            OnVisibleChanged = TBXSensorBarVisibleChanged
            Caption = #1054#1076#1086#1084#1077#1090#1088
            object TBXSensorOdometr: TTBXLabel
              Left = 0
              Top = 17
              Width = 150
              Height = 15
              Align = alClient
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object pnlSensorOdometrTop: TTBXAlignmentPanel
              Left = 0
              Top = 0
              Width = 150
              Height = 17
              Align = alTop
              TabOrder = 1
              object btnSensorOdometrReset: TSpeedButton
                Tag = 3
                Left = 133
                Top = 0
                Width = 17
                Height = 17
                Hint = #1057#1073#1088#1086#1089#1080#1090#1100
                Align = alRight
                Flat = True
                Glyph.Data = {
                  46020000424D460200000000000036000000280000000F0000000B0000000100
                  1800000000001002000000000000000000000000000000000000FF00FFFF00FF
                  FF00FFFF00FFFF00FFFF00FF8FCD7F80CC80FF00FFFF00FFFF00FFFF00FFFF00
                  FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF40B340109F108FD28CFF
                  00FFAFDFA640B34040B34040B34040B34040B34040B340000000FF00FFFF00FF
                  FF00FF109F10009900FF00FFFF00FFFF00FF80CC7D0099000099000099000099
                  0020A620FF00FF000000FF00FFFF00FF00990000990080CC80FF00FFFF00FFFF
                  00FF70C670009900009900009900009900009900FF00FF000000FF00FF50B950
                  009900009900FF00FFFF00FFFF00FFFF00FF40B3400099000099000099000099
                  000099008FD28E000000FF00FF009900009900009900FF00FFAFDFAB109F10FF
                  00FF40B34030AC30FF00FF40B34000990000990080CC7B000000FF00FF009900
                  00990000990020A620009900009900FF00FF70C66EFF00FFFF00FF80CC800099
                  00009900AFDFA3000000FF00FF40B340009900009900009900009900009900FF
                  00FFFF00FFFF00FFFF00FF20A62000990020A620FF00FF000000FF00FF80CB7C
                  009900009900009900009900009900FF00FFFF00FFFF00FF8FD189009900109F
                  10FF00FFFF00FF00000060BE5C00990000990000990000990000990040B340FF
                  00FFFF00FF8FCE8300990030AC30FF00FFFF00FFFF00FF000000FF00FFFF00FF
                  FF00FFFF00FFFF00FFFF00FFFF00FFAFD69850B95050B9509FD28DFF00FFFF00
                  FFFF00FFFF00FF000000}
                Margin = 0
                Spacing = 0
                OnClick = SBClearSensorClick
                ExplicitLeft = 132
                ExplicitTop = 1
                ExplicitHeight = 12
              end
              object lblSensorOdometr: TTBXLabel
                Left = 0
                Top = 0
                Width = 133
                Height = 17
                Align = alClient
                Wrapping = twEndEllipsis
                Caption = #1054#1076#1086#1084#1077#1090#1088', '#1082#1084':'
              end
            end
          end
          object TBXSensorAzimutBar: TTBXToolWindow
            Left = 0
            Top = 324
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1072#1079#1080#1084#1091#1090' '#1085#1072#1087#1088#1072#1074#1083#1077#1085#1080#1103
            Align = alTop
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 6
            DockRow = 9
            Stretch = True
            TabOrder = 8
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1040#1079#1080#1084#1091#1090
            object TBXSensorAzimut: TTBXLabel
              Left = 0
              Top = 13
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel4: TTBXLabel
              Left = 0
              Top = 0
              Width = 135
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Wrapping = twEndEllipsis
              Caption = #1040#1079#1080#1084#1091#1090':'
            end
          end
          object TBXSignalStrengthBar: TTBXToolWindow
            Left = 0
            Top = 360
            Hint = 
              #1054#1090#1085#1086#1096#1077#1085#1080#1077' '#1089#1080#1075#1085#1072#1083'/'#1096#1091#1084' '#1076#1083#1103' '#1089#1087#1091#1090#1085#1080#1082#1086#1074' '#1091#1095#1072#1089#1090#1074#1091#1102#1097#1080#1093' '#1074' '#1086#1087#1088#1077#1076#1077#1083#1077#1085#1080#1080' '#1087#1086#1079 +
              #1080#1094#1080#1080
            Align = alTop
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 6
            DockRow = 10
            Stretch = True
            TabOrder = 9
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1059#1088#1086#1074#1085#1080' '#1089#1080#1075#1085#1072#1083#1072
            object TBXLabel5: TTBXLabel
              Left = 0
              Top = 0
              Width = 145
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Wrapping = twEndEllipsis
              Caption = #1059#1088#1086#1074#1085#1080' '#1089#1080#1075#1085#1072#1083#1072':'
            end
          end
          object TBXsensorOdometr2Bar: TTBXToolWindow
            Left = 0
            Top = 180
            Hint = #1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1074#1077#1089#1100' '#1087#1088#1086#1081#1076#1077#1085#1085#1099#1081' '#1087#1091#1090#1100
            ClientAreaHeight = 32
            ClientAreaWidth = 150
            DockPos = 6
            DockRow = 5
            Stretch = True
            TabOrder = 10
            OnVisibleChanged = TBXSensorBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1054#1076#1086#1084#1077#1090#1088
            object SpeedButton1: TSpeedButton
              Tag = 5
              Left = 132
              Top = 1
              Width = 17
              Height = 12
              Hint = #1057#1073#1088#1086#1089#1080#1090#1100
              Anchors = [akTop, akRight]
              Flat = True
              Glyph.Data = {
                46020000424D460200000000000036000000280000000F0000000B0000000100
                1800000000001002000000000000000000000000000000000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FF8FCD7F80CC80FF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF40B340109F108FD28CFF
                00FFAFDFA640B34040B34040B34040B34040B34040B340000000FF00FFFF00FF
                FF00FF109F10009900FF00FFFF00FFFF00FF80CC7D0099000099000099000099
                0020A620FF00FF000000FF00FFFF00FF00990000990080CC80FF00FFFF00FFFF
                00FF70C670009900009900009900009900009900FF00FF000000FF00FF50B950
                009900009900FF00FFFF00FFFF00FFFF00FF40B3400099000099000099000099
                000099008FD28E000000FF00FF009900009900009900FF00FFAFDFAB109F10FF
                00FF40B34030AC30FF00FF40B34000990000990080CC7B000000FF00FF009900
                00990000990020A620009900009900FF00FF70C66EFF00FFFF00FF80CC800099
                00009900AFDFA3000000FF00FF40B340009900009900009900009900009900FF
                00FFFF00FFFF00FFFF00FF20A62000990020A620FF00FF000000FF00FF80CB7C
                009900009900009900009900009900FF00FFFF00FFFF00FF8FD189009900109F
                10FF00FFFF00FF00000060BE5C00990000990000990000990000990040B340FF
                00FFFF00FF8FCE8300990030AC30FF00FFFF00FFFF00FF000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFAFD69850B95050B9509FD28DFF00FFFF00
                FFFF00FFFF00FF000000}
              Margin = 0
              Spacing = 0
              OnClick = SBClearSensorClick
            end
            object TBXSensorOdometr2: TTBXLabel
              Left = 0
              Top = 12
              Width = 145
              Height = 19
              Anchors = [akLeft, akTop, akRight]
              Font.Charset = RUSSIAN_CHARSET
              Font.Color = clWindowText
              Font.Height = -16
              Font.Name = 'Arial'
              Font.Style = [fsBold]
              ParentFont = False
              Wrapping = twEndEllipsis
              Caption = '-'
            end
            object TBXLabel6: TTBXLabel
              Left = 0
              Top = 0
              Width = 129
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              Wrapping = twEndEllipsis
              Caption = #1054#1076#1086#1084#1077#1090#1088' '#8470'2, '#1082#1084':'
            end
          end
        end
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.hlg'
    Filter = #1060#1072#1081#1083' '#1074#1099#1076#1077#1083#1077#1085#1080#1103'|*.hlg'
    Left = 161
    Top = 84
  end
  object SaveLink: TSaveDialog
    DefaultExt = 'lnk'
    Filter = #1071#1088#1083#1099#1082'|*.lnk'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 113
    Top = 212
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 244
    Top = 81
  end
  object TBXPopupMenuSensors: TTBXPopupMenu
    LinkSubitems = NSensors
    Left = 778
    Top = 297
  end
  object OpenSessionDialog: TOpenDialog
    DefaultExt = '*.sls'
    Filter = 
      #1042#1089#1077' '#1087#1086#1076#1076#1077#1088#1078#1080#1074#1072#1077#1084#1099#1077' '#1090#1080#1087#1099' (*.kml,*.plt,*.kmz,*.sls,*.hlg)|*.kml;*.' +
      'plt;*.kmz;*.sls;*.hlg|Google KML files (*.kml)|*.kml|OziExplorer' +
      ' Track Point File Version 2.1 (*.plt)|*.plt|Google KMZ files (*.' +
      'kmz)|*.kmz|'#1057#1077#1089#1089#1080#1103' '#1079#1072#1075#1088#1091#1079#1082#1080' (*.sls)|*.sls|'#1060#1072#1081#1083' '#1074#1099#1076#1077#1083#1077#1085#1080#1103' (*.hlg)|' +
      '*.hlg'
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
      0050442DCE000050C1494441547801ECBD077855451736BAF63EBD9FF4DE3BA1
      840EA157E94D40294A912A0A0882081614145414F11315110545454141082862
      4141E95204A5496F8100E9C9A9FBBE6B272724E42401F57F9EFBDFEB79E6DD33
      7BCA5A336BEA9E7604BA835FCBA9EBD4F0164D442100ABCB789CD9FE6A1F3BF4
      6A95509D2B0827C27D0E3010F0A63E87E5D360741CBA57257AB585658BC96B5E
      82760C18181960A4F1DD6BD182D1CD694AEF246A532F8CFCCD5A3811333E8688
      7024F8BD12BC324080CF0485727A84BF8E660FA94BD306A651C7B460D24A05E4
      635051B394501AD5BD1E3D3FB41E4505E899E8531C860DB7A312835653D6CE83
      A7812D6B05D1E3F737A586F1816436E829BB18B6FA0032FAE05D2B5080CE495A
      B2D3A2314DA843DD4038D24030A994920A0CE02151128419F121669A37B219D5
      0BD7D3B5AB99545C5C4006B3894C062D890A05699402A54458A8795A22F9F958
      E8E12EB114E52F8B8C53C2F9C60C6528E5E7AD871C83E7873521925CF4D7B9CB
      542898C86AF62597139E0481A028C8AAA3FD272E83591645069AC8A056D0842E
      91347DA59CD74CE33EF89655590A107B356C06C6879828C2574D474E5EA09B64
      25FFA000588B2449444E974476879BEC763B458707935D52D2E6FD97E9AFCBB9
      14625652B88F0A7E6551312D3653F91444137E75A2CCB4F2C79364B558283048
      0FF1B849210AE470B929AFD04E46044D0C67A684D89BA97D5A38ED38709CAE5C
      2FA430B39B2EDC0411A268229293539E41082C2936D44A6EF0BD512C922B2B97
      8A0C1A326A91502481194486E9D81B5224415C02B95D0E7281F9E5AC7CD2095C
      12E45430AD4A0CE4807F5D29A43AC85C2BF2CCD7A2A4E397722853A1243578E4
      173BC8DF401460D6C8C425B79BB273F229D8A2A2CB170B69FF5FD9448A92D4C9
      C4F0100059210F1261389612E94B01BE469ADC239102AC063A732587361FBA4E
      450E8944A4CDE97251B0CE41853627F54D8FA1E2A27C2AC8CBA503478ED392AD
      37C9AEF105194AF2D4EEF2223A43F85DBC964721016699385E293AD84203503C
      7F3874890E9ECE25975BA2A3678B28BBC04649A1060A374B94979B4D9FFF7C86
      5CCA300EC23843A5BFB214F03B52F119F48129D1FEF458F73862DDE922522A95
      F4C50F07E89BC3F9A4461D28B239E82A6A5EDF06666A12A9A26B5959B470DD71
      CAD645137E9F23F665C5F47606B2980C1A2505F96AE9E9BEF1E40F31899293E6
      7E7E88CEE7AA4840FD28B23B292BD746831AE9A96198403FEC394E5BCEFB9143
      A105FD5BE2E1970A0CD802A9E08AF29449A7A221CDACD4B98E2F7DF8FD09FAE6
      989BB41A15D99124BBDD41B97945D43482A85B0AD1BB3B5D74D36DE1E07311FB
      A7D9E0412506EC00269F411F18645252CF14372DFB398B6CA299548283DC4E27
      B9EDC5E4282A243F9428853598F25D1A78A70AA2610B060A1F6B158158B00CE7
      66E63969E90E3B85EBB4F44AFF647AA47918750D9628D8760A8D9E0F1599A23C
      C439E61CA62221BC7965007B02134E6A129AEDCF153A2B2527C7505252530A0B
      AB4BB6623BD9DD72E23F875F2E92EC17C6CA4A51D98A68F4E8D1D3D3D2D2DA58
      0BFEA8D52925EDFECECD12946E954279E242019DBD728D9A35AC25A5F96B9C97
      F7ACDC4717B785444747373E7DFAF47ED0E22611DA2DE535058D1B377E61DCB8
      712F4C9830E185B8385D604C5484C657EF470D1282E881DE4DE99E36CD053F5F
      523DF9E4941173F1EBDCB9F36C226A4B5E7EE52B5A99F3C68D1B69E7CE9DF2BB
      5A6DA1C2C24852A0B9902DF0100481B66FDF41D9D967887FA74E9D62CDC88F3B
      82AFAFEF02B3D9BCD66432AD7BE8A1199767CD5A91376BD6FB794F3EB92C6FC6
      8CA5794F3CB124EF91479ECDB35A7DB6A854AAD5C02720DC09B863A5824F8E11
      231EE60E40272F68003B3380024B2AE8FFA9BB9780E2F6205F7EF9A57AE0C081
      89F7DD775FAB7EFDFAF51D3060C0982143863CF9C0030FCCEDD6AD5B8FFAF5EB
      175EBE7CF964565696EBF6B0DEDE153FFEF8631B107A08BFF163C78E7DC66834
      BEECEFEF3F312020E0BEA0A0A00E56ABB59E5EAF0F33E0E7E7E71705FB7EB56B
      D71E50A74E1DC962B11C3D7EFCB8DD1B618F9D62D2A4495BEBD5ABD71DC4EA80
      702018A8D46A358ABA20389D4E814710EC1916B20637114C02020303BB02C36A
      D5AA658C8888387AE4C89102F6703B44177E889C42A3D19056AB2537FA590688
      13830330710FF89DED99514C4C4C5893264D66376FDEFC286AFD1BFDFBF7E722
      2D3752EC8F215EB97225130641C2A88109EB743A422A8875EEC91418C989A208
      2F24DBB31DFB63FFCC086E42585898B575EBD68F80D16188F9E3DEBD7B374200
      399002F21F8C9A18835408C5C5C504335DBD7A95727373C9E170C05F89628236
      9B4DEE3E39A5F9F9F9B23B04208F309811F24B191F1F5F1BFA43680942FEF8E3
      8FEF4504BA84D83A8F1D3B268F75B2B3B309F9211362B194902F793213267CFD
      FA7502013945EC87010672184844B870E182585050D006A1E2146DDBB6CD429B
      F320324C81E22707CAC9C92164B89C1F9E4C66E22C124E158353C82946CAE9C6
      8D1BF4D75F7FD1EEDDBBE9FCF9F394979727400AE7F0FB523971E2C45F9E7DF6
      D95983070F7E293C3C5C44BB4E288E849810924A9CF9D7AE5D93993103CE0316
      11BB5FBC7891B825657B06A78481984B486911F41B4A3CDCCF3DF7DC1B20148D
      52F07048488870F6EC59428B2A33E10C076334CDD994999929133C730663200C
      C098288BA6942841DC2057A2908ABF60CA6306D0C9FED24B2FCD4089894409E8
      C14C903C42452296377BD8B2658B5C6C3D44B924B13D8B90ED1056F60FD91362
      2F417CD7E1EEF63020C83DFFE38F3F1E85946C420FD500A210D01C10F2471EAE
      978F29971C8E2D13E514B21931965300C21891174B88D86530A8D48552A74E9D
      12DE7AEBADD3191919EEEDDBB74B9F7DF699F4CB2FBF48AFBEFAAAF4FCF3CF4B
      2FBEF8A2F4C20B2F4873E6CC91A64E9D2A3DF2C823D2A851A324B4556E147977
      EBD6AD4FA366AF05F1CE40854A87F712D5B367CFE6EFBEFB6E164A85FBA79F7E
      92366DDA242D5AB4487AE38D3764263367CE94A64C9922A1F64AC3870F67C2F9
      8D1A35DA87C2F13F50180624036AA04A25F4EAD5EBDE0F3EF8A080197CFBEDB7
      D22BAFBC22CD9E3D5B9A3163863479F264F7C891235DDDBB77FF2B2929290395
      F309506A070400955A68D879558A7BEFBD77F27BEFBDE760313171C49A09E7B5
      6CD9722F1ABC45083514480274C0DF52AA1E3D7A3CF5E8A38F9E401FF1079AE8
      AF10DB69A0D49688FC813B8E2DFC56A93470A907B407E2012DF09FFAF7245029
      93D0E96B91A10DD0E9F745C5190FF31454A259C3860D9B834E7F08CABB068DDC
      41D472E79D44C35BA7FF129AEAB12886DDD02FA4A13D8A46EBE98B3E438B0630
      0095A9133AFDFE2851FF75FA3AB90BE416935B529637B79C6865E5CEC7D3C3C1
      EDBF4EBF6464E81189034319C67F9DBEF05FA78F7AD3191080CAEAFFD39DBE84
      D184844E7F1F5ADB7FB7D3C720CB317FFE3C09932412DAA3E9106C0050A93F81
      5D05255678ABFCE2C008EF158DA6DEB25EBD4628A3A25A50BF7E53A86FDF91CD
      E0B5087001D5AA9A1870605B4848DC8DF2F34545450E9E78BDA3D185D7247AE6
      8BEAD6ADDBEAA187E6AF69D020B64F408051999323D0A54B99D4AA55C3D0CE9D
      EF7B4CA170C4464686D58EFEA7F3451111615A9B4D87E1B99A3A774E23CC2789
      56ABA4FE6FBE88E78A183C5CEC80DCEEE405FF17CD17BDF0E28BCA8F3FF9B4C9
      A6AF370F5FFEE1474D5E59F0AAFF134FCC502055D5AA1A3DCC7AEA2961F8F011
      31A9A9B51F4F8A887831302F77904F4868FF8088C89E8141C18DBA74EDE6774F
      972E427A7ABA0DB5DCBE63C70EA93CC76A19BCF5D6DB3E9889195AB75EDA6B61
      E1E17DC5DC5C8BFBD021917C7CF5DAA0A01093D154DFEAEBD3C362B1F6F7F5F3
      EB1E181094764FB76E3E1D3A7494C0B0B87193C60EAF35F995050B34AB3EFBAC
      53F3F4F44F1A366CB408C3C53A448242F20F2477E77B488C8820C9E9C2F28A0B
      EDBDA0546B340121767BD33A39D9E39322A2DEAF5DB7DE3789C9290B4D26732D
      78B895207C458AE1111129B1B1B113A2C2C207E89D4E3FD16412EC9833CD2B28
      C6AA87039FB458B3B979837C7FFE8EB2310B9F95DE92ECF828173069A3BE9125
      39E213ED36726FCABA9A39E7C2F97387941EF26FBEB938382232E28198989831
      98428815CF9E131D3F6DA5C2F04872346F49B985C5945F680313CC5BE714524E
      A183F249A4FC225E289248325824A74A7733F7EC85D70A0BB2DF7EEE99593798
      B662CE9C39264CA7754F49A9F53A33D0E9F4FE82208A4EA58A72541A2AF00BA0
      0212A80853396E4C92388B8AA9581429272492F28343A958D0509E4BA4B3C51A
      3AA68C2ABA2246665F13130DC94D07E7FB1B9ADDE0D9160B649CACD16843B03E
      A3475B6F72BA49918F550E9BC9C2B35058A073910B4B8EC11BD79262F74EBA19
      9B404E30E10158A15B4547F254745A172B08C1B17A5D50481D8D5E1BE3C8BAB9
      2DFF7CE6D9B23C18F5F0227D445C54FF5A75C3E691AB3054254A64369948A7D5
      C85304F6822272EDDF4745907B61AD342AC2DAD9C56BD974C4EE43D981754865
      F127CCF4B9C41B377FB21F3F39ADF8E4E9FDDFAD1C24C90C1E9EB13628B55EF8
      D8C020F538512905EBF4064121B8A9382F870A726F92282A4A81852E4109B9DB
      E82896BF4E141B294F65252920915CA42E16AF5D5F45274F3CFBC3C276E758FE
      0CE523B336A6D76B103C293854DF4B8DE82A541AA1A8309F8C0603090E1BA994
      FE94956FA37C4943174EFF452AAD812E5ECFA70B850229B1CE997DEE8C64CF2C
      C856E4BB5E37144B8BB6BFD53187097BA0A8DB78603E91EA0F49D09E51EB4C4A
      BDD962D2190CDAECAC2B84953E41D41888D7698AB0C274C34674D5AD47E6FBD0
      F56B5728EBCA15B22BACAE82ACDC2F6C9937DF3B7C64CF0DC1DAD4295DFBD543
      9F94EFBDD6E726DEF631864F5AFB4E784C408AC5CFD4D13F48F380C94797E416
      9542565E215D2F26CA13D474F1CA9F94ED409DF037537EC17914DDAD8A226BD7
      FE4526BF969AE0E6471505C57B843A1FEE76DECCF9BDE8FC858B65F5000C68F9
      A2BE7949A3361D0949C84C3499FF102F5FD94EDACBC114E3DF8A349660BAEED6
      528E8F0F154B28A657F760FD72058586F511F2ED563DA61E6345953A466D3274
      110B8D97A5EC4B2F2872F6AD60BA320C9D3E116A4DD89C72EFFB879775FDEE85
      DCE0CF52DDAAFF59A47A4B7B493332464B233FED29755C3C4EAAB7F075296CE6
      5849F35080243E6294425EE82F59666E92948F6D94F453BE719BC6AFBDA06FF7
      C4748D7F4210BE4E512440DE7FE097C6F0C4807BE35283A60504985304BA4F74
      A2749CF6FB8E9AF88E22B5C64646E5454A30EFA1BC3F1753CEF5ABE4D4384961
      56A33058A9F0A693249BDBEDCCBDFEA7F3E8CF2F09C736ACB7E565CA99ADF01F
      B4AE7E645AF0D361C9FED3351A5598E4964411336D01EA104AF4D3A01439E966
      9E95F26F26939F3A968274A789C45CBAE942BDB010E9557528EF72B4D371F9D2
      76E1F7AF678927D67F632BB8818283984309BE3D97C4FB45E8DA584282BBA34A
      3732998DC15AB552A955E50A05BA0FE9B2ED12DDBC76931234DD482F35A436E1
      7FD28D9C8FE83BD4EC8BF61652DED976C5577EBFB45E38FAF54B8ACC5D876DC5
      587406618F124A0DA23EA1A7C99A503FDE272AA1B52524A4B3D9C79CA63790BF
      45272A721419C29E9BAB285E399EDAF825539B98B5F4ED79A5B4E687F49C33FB
      FE584E4733FEA7C8397A061382EE527A556B1A6380C2943AC037A2C773E9B5C6
      2C9FD9E2998C1F7BBCFBFEB5E69F8E75355BF491547FF6B7D2FC0D1FB987BE35
      FBBCB9D5E4C7D5BEF17266564DB11A178DDEA234D51E1410DBFB85B6F5C6BF3F
      2764CCAA5F8D23D65CF71FFAC1219F66C3066B8C7EA66A82DF9D93DA1CA652A5
      0C0951A53FD551993AB4A94667BEA3B1E9DD7129F1CDF9C62879ABE1296CC858
      F70091F026DA76730D7EEFCA199387B9D85EF2089A0AE1CD1EDD7BFDABC44B63
      62CED8B8FE4DA527E6EDE6DF6A014B3D94693E2A272D1BDB887C8C7AD92E1B9D
      CFFD6FEEA51BB6AA473DBB9F49E7151233522087A19B8525FAED4FBD5AA03747
      D72B23CEEE56838E3E7EB801F5FADFEF54505C7DD117394075F878542285FAA1
      4DB8CD939FC9409F8D4EBECDB6F26B8D0CBED975BC72A8529B6F761F2393B6FA
      0255250381244AF671D0A59BC5F4ED9E63A5246F69DB7E3F85B1928DD2FC8AC9
      A7246B6E3996337965200A12D5F2735082AF9BC27D75546073D0C94B5965C1AE
      66E7635A4D82E8CC14E7A7A47ABE360AD04B65EEE50D151870867E312E9186D7
      9528DE4AD8C8A4A3605F2385F89A48143082C3BE160EACC7068E307F0B85610F
      4C28BACE583F6CF248D3D037936B571259190326BE76426D8A0BF5A789F7B6A4
      26494132F140100FF23152B00F986023133330EA3472EC43FDCC1406068D92C2
      68789746F06FA68C496915989431F8EAD13A1404224C80D1BD590AA54405C984
      824148AF55B37519F83D14A9488E0CA4F4DA3165F61614E18D93EB97BD97D583
      008BB1CCD2634828DD6AE579BF5DD7A955C4B8DDDEACD7965995A5A0CCE65F36
      94A5C0E670924655F62AB3C92FB2910E19AAC04057B6B8EDC17BBBF28B6DC462
      29EF5468B3935E5322D2328ADD5EDD455F3FDE94D01DCB7EF71DBF206F0E0B81
      FC194A45C5C43AF08573F97A2E317825B0494AA41C8E89DFF3EA3EDA36B3B9FC
      2E7CB9768DD4B7CFBD94F6CCAFE487B2FCD5C446B461C71F949D574C21E8B018
      5C5A4290A11E2676A496095FBE91070640562E19F51AEADE2C99FA2C3E4479C5
      121D783E9DD6AEFB822A309059E2116C70511D3F274563D8E261901419403EA6
      922A9B935F447F9CCDA4CB59797409A9387DDD4E87AFAB28B3F0562A3D0CCA44
      04BA65EA4A81026BCBA858920D3AF613A1B27988B3278B512757BC0B593974EA
      BA837EBFA1A6EB186DB3DBEDB8C5F236178ECD81EB6A3993CB97738FB7A6B5A2
      90B95A3A785D532571F65B250376E458F56F53878D5ED1A3790ADDE4792FAFAE
      25965E4554E254F2ECF7E66FF4EDB4E6D8FE53312E85C57639434B7C55FDAC91
      01A7A2DB2BBFD2D74FB490E5CEA4B888F67F6317D99C0A146BB6A91AF8A854A2
      F72733E77AD5DE2ABAA8940ADA34BD55454B2F6F209E272C5FF1FE041F1F9FF9
      5884AEDC18790974A756587A29C8CCBCFC4C8DFE03174A2D81D41A3D56E1A162
      CE79F7C463A64560D217B8EB65DD3B6170027C1380E544F40E983400AA1E10C1
      537975270C2E21C00950E4940C85F94BA0037047AA4606571F130A40698F0B03
      4D053E9E600E052A0F9460899409D02AA81A1994FADE09DD06B0B2E391051008
      8A804CD4FF352911767DF01E0BBD4C8965A67206788A07CA67E81F70BE06B0E2
      C6211FEEC97819028C833912E38156308F07C6E03D18BAACBC32804B3A30171E
      83A013462B97A01F0758D9F168013C044C0516000B81614047E07E804B9C0A3A
      A6ACF8591947603518580626B510BB429837B888B8D6C34883F07858415417E0
      4EA23BDEE14C37A14700638024A04A067FC1F104D015988614A44077029F0117
      81E852C2B2FC61D6C02E053809B01D37C1ED61F6CE000473E0F83CB095889290
      82AED0FB006A600FA0026E5781A516E7A1DB0133E09D41D6144142F1FC0E1EEE
      035E03FC8196404F8033570BBD8252E03B0C16466004D00F580160B68F9F3500
      F9D0015E56032CEFF3D03986298A12A2782D512EA20B30B543E44E429795527E
      D6FC3804B1AD83A81AC06B12B013380182B5A14780910AE64298BF07AE01654A
      2833793120E62CEB1838D5078CC053401470020C3F83DE1E4C393522CCEF03EB
      10FB5CE8658A1DE4176CBD7916E0D8C9EF209E0AC3A34032B095883E049600C5
      4000081B8140989B0326C00258114E845EA6CABFCC26A2A3E598D8F07E01F806
      B1CA041C3073ACD7433F8D149C2122AE2F1A888853F82ADE370133C1C417BAAC
      94F2B3E28399240B8F09C7607D1228AF38932361B112780FD84A44A12EA2C660
      C2E24C85793AECB612D176C07B31850333291317DE3D2A178640600AC4138F54
      1D82F961E000084BD05929F1D001B22A2F22D9A2DCA3121388A500EE378160A0
      314060B20FFA78E0702913A6A9C5BBACF8453654F1A8C004B1CE81BF0F8002A0
      1564CDB16526BBF03E1EF813609A3AE8B2E217D950CDE37E8F1B628B48D232BC
      CF039200AEE1D064F52B9E2CAE13D0CB180878B96B859833814720B29FD1ACEC
      F210803DD36B4B441A44E61BE87F5F45BDFB7960D43BABEADC4E01F60A802BE5
      ED4E95DF11A37F3426628A35E581199E1681515F4007F35DAB9A1870862580EA
      72FA1B632284A9B2A2B11BE3121E7F7B4C84B0D5334049E0F2BEC7F537C74435
      32600FC04EC006B0E2A6398B0DC813AF6322349672E5633F0C2EB7AC970101E3
      F17211B12F82CE832BCE83EFD1984520254CBC1BECF380C68011D858CF796998
      4310C7A63BCF1A26DBB6590370B40FF63F0219DE32391D0E773426C266ED579B
      38CFFDA223C7F37F8A41619B5549D64DAA14729648BE1DE8BCEA8DC111380C06
      962135558F8924A91EE690740715A1E1F8E82535D6C6CE0B16E9096DB783E9C6
      098D10BE013041C0A38202512B2CD6032D800FD11CBC8246AE1DCCF58034200A
      E2E2261BC612E596A4AB384B7886D02F006E600A44FC06F492B4B0C10310E416
      F379BC6F252F63229DE4E0CA07A75B0AC4034ADFCE43B703657E44BC545068BC
      AA1C1325B8AEF584672D5041294A862F9CE123E0D00F5801C84A909FD53C20B2
      0E705EAD945C9608778E0859D331D19F1498C3837D994209BB80970A6322BCE3
      F01F3FABC7A1168E5399D9A2DEE72F264CAEDD58D2BA0C82B5112C4281553E98
      0B61FE1EB8065450CA0A6FE55E1073955A72C6CE2CDABCD422D992176A5B5331
      819CA0B4229F36C3AB3F32FF9CABE4ABE77DBC571A13C1AE7226B32588A76A25
      C794578B367CD1DFF17BAB818E83A4905CAF22E65CF9EE784CC4B4BCE6C1872F
      AC1F6222DB07DD1D7FAA54E4BE008F8D822CCFEAA1CF0312908215887D3B987B
      234D588945DD2A3987B90A766FA188DE802E2B25DA8E87613A0E9C061A00A3C8
      F15B67E8AC32F0B84F983DBB3070E1B3A93047022B813B1A13C19F9CC98BD9E0
      057D41785D39FB5C9803011E137D8F581E822839724B910F69488900372550A1
      63E27A3011964B813DC007405B22D2DE469CBFD30A607F1308061A033C5CD907
      7D3CE0754C04FB3BFB3E608F882D224963617E11E0948D422A9C30738BDB02FA
      BB00B7BC4361FF39CCB2E214C8869A1E080449DCDD988869DE3103F60C2636E8
      6F005FA22445419715EC25187E061E0132817FA630EEF13A2662AA70BBF37111
      07F8A7E08CF34A63C78E1D014F3DF5D48059B366CDC5418697870E1DFAC2FDF7
      DFFFDC830F3E38FBE10913663D3E7DFAD48993260DED7DDFC094F4AE5DB343EE
      E974F5B7F5FCAD5E911C97DB0A361F7EF8A1323535B56D4040C093972F5F4EDF
      BC79B3064735E8E2C58B4231B69FA0ED27954EA70A080BB5346CDCD8DC36BD45
      4A6258683F9542F1BEDF5B8BDF9CFFF0840A792096A7BE71E346755454D4484C
      762FFFF8E38FDB3DFCF0C3DAE5CB970BD8DF2BC4C4C45083060DA861A346149F
      94440ECCC8AFDBBE5D98B1E41D71CDF6ED2198549F1661B1BE3E61F1E2A8F234
      CB18AC5AB50A4DBCF0004EA4BCF8F6DB6F872E59B28470628BB07192E2C68D23
      4BFFFEA4EED68D0C60149098482960D6BC7933328786D0DAC3BF0B9F1FD8AF2E
      7439FB852A957387BFBB846BBCCCA78C0188B5282C2C7CE68B2FBEF0DDB2658B
      90909020A4A4A460CB49999792009A92E3DB36B59A9C58328F898FA758F8DB63
      B709BB7372544A8DBA5FBC200E1B39778E2C7E3934CE7858716067DA912347C2
      415C3E4815141A4A4EA592446C419129973E5C202C198DA4319B4989CB016C7A
      3DF9874790312C8C764A9270431475468D666488D9C2B59A7B10A241830675C5
      7AFCE4F5EBD76B70704A080E0E26358ED11B9888564B8A73E7C8805D51BAFC3C
      5281A95BA3A6629552F6E3369AE8A6029B6B708EEDBA428903D25883D3E9AD88
      F9C5F3CD9BEE1070DEC688E3006FE134D610C81E1136109FD00A4B4CC4F50566
      1241D004B138508298B00B5B82B0E9514E8FC362A6DFDAB697CDFCC8DFB78F54
      0505344EA795F476C70FEBCE9E1922E240612AA6801BE280957CFE0F4795B08F
      CB4E9A56ADC859AF1ED95353B1A7C84E2E9CB8C35162C28E7B72DCD6E1337186
      0EA9CD476A78DC63D4EB624389E270B782A231A6818350E61116494460DEB1CF
      013C70B85CB25B44582819213609A2F0B895D739575D085F88C8E8355AB345A1
      8C006D310229D015151595951858960F476EB82911D047A5267F8D8E4200838F
      3F6E7FB052833DFBA9EEFEC314BDF72085173A2906FBBDF4D817A555AB15A2C3
      A1173EFDF4D305384D37FE7FFFFB9FEEE0C183026AB0BCAC827CC19A8182CCC8
      6C74F6A44169D1A2F41463358A9B5427442121A373EC7672904079762CC7605B
      562E8EFC8D6EDF5E6A101B777DEDB7DF3ECC993C0D672B9FFCE8A38FAC5F6FDE
      2C440C1850167BC3B163B88FC2875448328B4954A9C8258AE4446AF460760DBB
      D61C70BB8903E90EC98DBD4685A4403BFEFCB8F192BFD9FCD77BAB3E1D24CC9D
      3BB773BB76ED16EFD9B327EECD65CB84A02E5DCA18F89E3A4584726F641B10E5
      23026EE802861445588CE38C2F282EA2ACEC1C5282F9E56BD7A83E0AC5DC1933
      DCA74F9FDAFCEABBEF3C20E0EC6BD0C89123DF4169EA357BFE7CD15EBB369393
      61440A90BB84037EF2714A058A2CD619E45226C107DA2CBA0191B8DC883D8A67
      31C43579C204A94FCF9E8E2FD6AE9DF9FCCBF35F1740589C376FDE70A4E27FEB
      3332741FAE5E2D18907C238A5C01CE5E6A113303EA0167BC1B66164F4E4000C8
      9728C5F1E3E4C6794E1C9594DB2D1C60745FBB7AF5E8BBEF2D1DF0D9A7ABFEE0
      A106A19D0F073E4206B7C6693A61DBB66D0257363E54C23166D170D155A09239
      0B0B4982184AC813151F3E4CD938DD181F1747931F9B2CC5C6C416AD59B3E6B9
      952B572E3C73E68C436680033D229A8B5E5DBB765D8C3629E4934F3E21743890
      80209F09444328D393D01CB0384434D7B2051ED77FFD95922223091D9284C6D1
      85C8ADFBFCF3CF27E210FB6538DF52494949BA27B1CF79C3860D596893DCD3A7
      4F77376DDA54C2F93E09675FA5E8E868292636568A888991422322A470E80DE1
      8EDE4E0241F70F3FFC605FB060C1E6B66DDBD686D8E58833F5B22E13A7109D38
      CC79189639D8FB582B3131D1829E8DC2C3C3053EA6CA4D080252A0BF3F252305
      2DD3D3A973C78EBC179E7780E423C56BB18DFD29C4FC0F4881CB0048116A88AC
      DD7AE0BC93B6458B16AD71A86A0C88B7C619285FB7DB2DF2D94BE8D8C729A260
      29B1114C94D039F169EB23870E1D5AB96BD7AE353895CADD6519F15B542B9B44
      100FC4A1C2EE38AEF7324E9E6E7EF9E5970FE2E0E1511C103D8CF79F274E9CB8
      1467068743B4092A15DA90CA34EEC8866589026588421FD110F9D1222C2CAC29
      36BC72676205853211C3FCFF4285DDFB22D00C780FC8009E07D0CCDF7964C51A
      BC06C17D18323A04C3191B64DD0AEFBDEAD6ADAB827E47AA260649A0128DF1D1
      39D40D134ED29DC57B3A6000EE48D5C4400B2A6E1CFEAC035D87227C11BA1E60
      7B6835AB9A183005811FE5A081B936C4140FA861AE56D5C4C0CDA1B9B163BD14
      DC944E837901F01898B40512014E19AC2AAA9A18F02792881A5C56DE51172EA3
      CD91DAB4697386887A024F01AF0113C0840B058CB7544D0C3C3ECBC414131373
      AA53A74E7A3486D168A32E8D1933E6F77BEEB987472A1DE1B91B98A8A097A94A
      0CD803A0079868F9764542B35D88062F9743A32951A1B9388CD6B411BA5D8E79
      11ECEF032621AC11BAAC2A30804334118D076600FD005FC0A314F818097EFAE9
      A77D60C122F347777B0FCC2AB4C2071E7BEC311B8EE2F3808353D218F6B22A63
      00E2DCB64C862D7B88853E1A18047091E494687020BD397AB996B0D300CC8475
      3132323279F8F0E1E110D735D8DB011FD0630954980C4981432262988DF11121
      E987D10FCBC510033327DCAA52221C8201E1F0E1C31C19667C054D38474A9E4A
      809BAC1C784A3854CB453312B1B1A205FD6EE1C285FED8B550076E614075EA26
      8E7B9F860766720EBAAC94F2B3E4F127B4FDF0D4101DFC051C5752E1207A6780
      635F3E2FE0CDABCAC1775C025C4E02970059C972924D78406E41D0C6002DD065
      EE59BD7AB519E626800AA856E16A900D38FDDE1C9EDE8278D643975505066C03
      267ED007007D7054E324BE3A3973EBE29D650BCDABBA326EDCB853E897A38968
      CAA1438738153052E53E996DC184E5D817E60730A4398973FA2130C70055A913
      38ED9203C702E05930C8872E2B417E7A798009E7CF20380D418667A3549960F6
      01D44079E5C065011B51E99AC37215887F04BD4C55C9807D8009139C808EA635
      C6485B162F5ECC1D4E28BB95831D83806F70F49BF3EA4930F8BD9C9B771195F7
      0026BE787F0DDF10767C8146C2EC0F94574EDC40B0EFFBEFBFF783E523609005
      FDEE14982CC431A42F7061C6B3A8B133CB0343C6593800B7027E56011C990AC4
      AB1591C727022E80B90D509DFFA3709F8814DC80FE7F919ADA3B5EB97442FDF8
      AAA22C56E570A7F61D3AD6EFDE7568AF5FBE98DD9E455829D8DF66D0A54190F8
      D5C27B7B376A52EB435FAD10D8BA6BCB753FBCD96FD892F169159A95BFC500C4
      15931F6A3BAA43E7C6EB7232AF9AF7FC7A88C4C23C6B9D0629CB936A274E2DCF
      E4AE1994127FA865DBFA4B445B11BE17CCF8D4355211BEE78A332F514C7CCCBC
      9884B8C11E59DD3503C4BC8CB8135F958C005F03DDC4A6CA1B3772E9DAA96314
      1C19F7FA07939B35632677C560D1A4D6755AB469B044B015920BC7E99D988171
      E1F095BD988F34292827CF4E376FE4D0E93F7FB77DF0ED890A79C1CCAA04C422
      67E8F5FD2F65ED593144CADD3143BAF1D3142973F378E9DC97C3A4C31FDC2BFD
      F87247E9DB39ADA5D54FB5BCD2AB79D85010E316B9E6B608C439434BC5524887
      F61F9367B122C37CC88EC36F5959D974ECC425DC16AAA38BB9F6738BD6FCF1F8
      6FA7723680413150A1D3E7F70A0071F196CC0B89E51D171D4421B8EE322F3B0F
      971516507E6E2159F46ABA9053467C1388C8C4A157AF3216F5EF9D7F64BE54F8
      DB3352EECE52B17C334E3ABF76B8B46B710F69FBC27BA4AF9E4A973E99D9F26C
      8358CB0050330077A6164D4CAF73FDB73959E5895FFDF66199F81F2B06487B16
      7797329E6DC932CFAC8E78A5D611622993F9B573E7095B9771BFA9969CB66259
      E6E7CE6542AE12EE99CA2187527DEEF9E5FB471D3A9BFB23A2ED042AA90AC5B4
      3C711145D1D76220B552942B1167685E6E0189981FBA78E9864CFCC58F0E3C0E
      E2DB40D52B71D8DF1A788178A50CE54C55E2BACB3C9CF9B88E8A84138174F55A
      0E166074E798384ACB3FCBD0AB28E79CA12C73CED08D90F99A67DBDC55862ABB
      360C568E1BDAA27BCB36691FFEF53B4FA119B053590999DBC85E88D92C94F322
      1C5BCDCD2EA0421765CE5909999FC965B1DC5951849C8C3F7D38F2DB0214C5E3
      6B474BFB96DF2F659616C523A8A15B5E6C277D3DBBA5542EE64A84B963257B6E
      F3E0FB4FEFFE6464485C88A9B65E29D191236728C4DF445990BB80E9B29B763A
      B760D501AEA12CF32A33B43AAE86F4D4C01E7FAE19798D657E60695F69F593CD
      A41F5E6CFBF72A51159C7CDBD50B1AB4EF8381D7F6BCD91D0D571BE9A3694DAB
      AD4455D0A9D6DAB759B25F8FB5CFB5FB7DEDB3AD2BB48AD586BA4B476E4F9AA6
      849BBA219CDCE442FFD7950E1419D0FE99126809EABE87C658A152DBE471BA2B
      BD1CCD8A0C3C543C8C3C1EEFF6DD43077A85C68E9810030EB28239C822288356
      9101D0C35C8255A4E7776FFE653B3970C9432CB300B1122BAF4F236CEB01FC7D
      D0127A02C08CBD8BD4430B7AC514209417E5869D0AA80534051A03310065DE8F
      235F6CA806250CC0C9E3075F2A4AACA5857FBC6665FAF255EFDF379FDE7F7C9A
      6ADE938D95ADBA874A91B50D4E532839042ECA9C020D44C5BAE0095FA697D214
      F1F959FBABAFBE1AB265CB9627B066F9263EFA564547472F0E89097AD212671A
      AA8D54B656FA0891F9CA1C9556A3D347EBE2A39A51DBEECD2F75EEABCD312483
      6028C00CA15556226EB99ADCFB4AAF95F88269894956CCFD4569F0658F73FA0E
      3A557C8C76E5FD4C078BF7C8212D0A2B7A3835B92D4E9325DAD8A789BAF5CCFA
      175B0D0C3F9918207B28FF282D817C3325CB58BE9C1007967190CD2DC3C03775
      0B7E6497D0943A4E935E34925E812E1407FF0B5D057499CEE3305C84BE5344F7
      36034D23E7E12B740C3EB142CAF36033DF4C996D1B6ECFE06933CC6CC9375262
      4685428C619462AC43B1DA4452E16CB846E4B3CA6A5261FB8C1A4CB4828EA285
      440A5744E38EEC38036EE2EB819B2917E366CAC7712F613C4AA7C80CF866CAD6
      F84C0DC2FC71A59B29052726C24525E5B86E52813B1F370EE02435CEEF1B9526
      8AA4384A92EA92D2A5C22295C04B6402A69C15B899320A7A27CC86F9E266CA83
      7C33E58D949F937B56753365A032849A1A5A5388321C295191DA8DD374EE486A
      E5D391FC8CFE3271AC6430039E95E7AB1AE59B295777FC7C3152102C62717433
      0C84E5153A874551F68C5B25E5EB2F59546AD25030EEB10E500653A8229202C5
      50F29502886EE0A6875C5C1880F51E5CB348882D61FD40BE9D12F73F7A8AAD24
      620AE0CF672FCDEE87F50137E6E178695D8E15A6C988331D8B15A42303C52A93
      7073BD8E8254A19462A98B019940BC30F4CD37DFD0DEBD7BCBC27104116129ED
      EDFA2F43CFE33E59C254C006AC7004A214740FF172336574783499B28D74E9FC
      792ABE6CA703A77EC780CF255F24895931394220265FDAC03A03F73C5E815EC4
      0C084DB6E3A5E9561D26A20250029A30131617EA48D9CD94DF6DF91E17BF6B48
      E9C4A63711D7526330000284E5149911C26215D8429E9B29FF9A7EF23594A4C5
      250CE01337531663A3C0FF9092D9B899320EF374FFE8664A9064E5E2470560D2
      35046B69EF21C3D6A3E9D880E5F80DB89972036EA6DC803B4536E066CA0DB899
      72C39C3973364C9D3A75036EA6DC306AD4A80DB8546F7DBF7EFDD6A33EBC87D9
      E199205A1FF0AE7049553256CA3FDEBD7BF77ACC056DC0CD941B7033E506DC4C
      2933C1CD941B30C3B201B577032646D6F7EBD76F35169516A2408C05C5F64038
      50261DCE03091615142603D37133E56A668019AF0D5820DA307BF6EC0DB89992
      E7E6D663FDF32B2C222D45FBF50C2AEABD085C07300322701BCDD2864976B8F5
      10B1CAD40BB390EB584C4C1CB166C29F63AFC542D49BD1F0DA968878BAF3F659
      B0720C98B807F07D9B526071E23E2C752DC1E5856FE11EC1A710DB7EF0531BB8
      155BBC54501E7AD00514D14AE241F12AA989F020071C2BC44037C3EF41E8E4C5
      5D90ED3DFEE5979247B98C8045692F045389FABBEFE51895702E217747CFE61F
      1D4DF3578B0F046A15ED4D6AD12448C209A7449F392561DDDB3DA2B26F27A2B8
      DDA2AAF75ACB8E9893063C3AA6A1BFE6D9D661FA4E0D03F5A149BE5A6BA4451D
      E9A755B4540854B7D9E0C72EFDFCD1C2F3E56928CABF54650E7BEBB7C048BDF8
      FAA004CB94AEB196D0181FADE06750B97CF52A7790512D869935C65093AA0ECA
      E73D8DEE9B94D5FAC1A987B77EF89A9CB715F3C00B87E4D77FD15AB5EE39A36A
      F90E6D106C76A0EE17143BDC2E0D5A3937E46343188D52540418552E934611A6
      55080B4F6615E7C2FA2B806ACC8380977FE833A951C4C77D93839576A7BBC8E6
      941C0AEC44528A8258807B85B069441214249AB54A958F4EA9CE2972690F5CCC
      DF732DBFA8DFD4F63117AA4D41D88C75EA247FFD032DC2AD3AECEBCD45EC1D4A
      A5E0562B0485C325B9F1098D3D20E81A304EC82972DA3538C86CD4281CC1664D
      5AD6CDC29E48C1DBD5E6415EF301A19D130367B48DF237623F463162EDC6960A
      5262D99AB0271A1D016E70C1873FE420B9098BD882A05509226E0FD2DC28B417
      0E9F32F36BE40BF854A534A29FAF5665552A04870AB7F5A8142420F602769F48
      6A51145402091097C4FB5EE04750811AA7ACC4AF18F9DBB6C341D5A6805A0C08
      6A19E9FF60C3508B02FF05E28004401C5411635124A4031D24CA8A883CD12A45
      25EE3FC21502109944EA4B378AB20A6EE46EAC9E41FD5ECA8410CB7DE9917E3E
      90AF1D4C783331A862B73D64C409474204B0141D90A1CDE97669950A4581C3AD
      397335FF4F774EEE9A6A19B4E937AAF8A2CBDD243DDC372DD0A8411E909B638E
      0440810962EFC4888DFF42C5E996DC4888A85129B4E7728AC50B976E7EB36AC1
      94AF20358E8777FC34A39BE3D8B5FC95DFFEC53320920E992C0012C0277D9112
      4180A4A0CB19CC7FFAA2CAB3BB34A7AFE65F7117147C77F8C0F6826A8B29B32D
      CACD39B37CF7D1EB91566D6CFF5AA1BC39A718C5157187923F0FF00724109241
      256A8A9C927EDF855CDBCDAB37BFD23BED3F73F86A45446317C5BAF3B216E7E5
      DC68B0370B775FA974EA088B4EAD51A238A18C22EAA8CF028F1DF597F31DFAED
      67B28B332F5ECFD01717BD3C6344ABF3CC4096251B2AE1A1576229EFFA124CBB
      7424BD850493AF941A1377BE6174883339C81C1686FFF1D0A08C162213AEE4DA
      8AAF67179E53E5E5AFB7B81DEFCF18DEE298879E77060366C45251DE12B2E577
      248305ED80BF543B36F144AFA8B0578A5DC2C14B92D80C1BC95254225A088C20
      4D6ED749ABE4DC85EB45F63F31BC45BE87B877FDFEA762A8D3F02D94DE47A28E
      C324B1FFE3EE3AB3961F9BF9C1F6875E5EF16BD9C779C6C7DBB4CB3FFCD9B464
      C5CFFA05CB7F51782776FB3A5AEF8910CB8D259473B523E9703F852540AA9D5A
      FF64F7E45A2FF92A141F4F1B967EC793501E86B74AD1906762E8EA3910BFD691
      D43A12F566A976729D133D926BBD6C55283F9936ACF95D13F73021EA3F3D817A
      4CF88ED2FB4AD4B2BF24DE33C25D77EA9BC767BEFFF3C8161DFA696F79BC7B93
      480FCE4DC597C3FFC85ED0019B1B49B0F853EDDA8D4E76AF55F725AB52F5E92F
      DF7FF90F62CE5BFBCDFE3DF0D5D79C8BA2C21A28D54B6D70B447AD3AF37E5DFD
      F627D31F6C5E74F771AE1842E1D369A8138D585D95CE18583726E1CFAE71F188
      B972D57B0B1EFB4731F7B091EBC1D0B7BEEF81323D2C52A3DE6A10850FA60F4B
      2FF478F8A77AC591DDED03ADBF4BBDC2C0ABDC4B193D0F238FDBDDBE9711E24C
      2EF7228F393DC4D81EE6A0FFE68B5812B2686403D17FF345A58228FB845262BB
      A79B2DF17D4C23478EECC1E6CA1070937F6205EB02F9CD4046320C3452D02B31
      FDEBCA36FCC07650B902B35981AFC956CBEF8DBE89B90AC24EB4446CEECEE8DA
      B5EBF1B66DDB1EAFD334F5B854B7F8F8D1E4BDC715F51DC7558DDCC7DD750B8F
      17D6BE7E9CEA141F6F5BBFC3F1A6B59A1DAF935AE738F60D1FC3CCC01F98BF48
      C1BEF98F8F2D1834991988D8926EC35F6DE002443921F24EE5152B562462A78D
      F1DAD9EBC6EBDBF212E335C924EE3026DA2FBB8CEE2C329AF60626C629522848
      0AA3FD7B0E24EEDBB72F71CB962DE9988F5331516CCF5A80BD47E3615657982F
      8285FC771C1D3B76BC842D276DB193B96DA3BA8D2FF17C917FB2F592E2A42E54
      91616D1B18EF7FC9335F74F2E4C944EC4A388E7F40F1C3F40393E05918D1EB7C
      11BB622842984362A30C25FECB88E78BAC0A3FA263EA44BF1E86AD217EA1F99E
      F922F6C4F345AC63FA8135FEBF104F1E48FC4F267F3EFBEC75CC170D1E4EF861
      0B0FEDDFBF3F74C488115B79420ADBA5439B05353BEE3A8ED385BDFCB7FA07F9
      E7A7E8EAD2276F7CDA03A2E0FF9FB88E5DB63D10FBEB1C39906025CF171DC055
      9022DEE4F9224CC66E82993095439877388E6D9FF9F8E8CEC7CCCB719E2F7AA0
      FF03C7430242F35D97890E7CF73B610A2103194AA1A1A13B800C4C85EEC00657
      2621C3335F549214B49A16CC1761A6E51FF7604C1DB397EBBFBCE78B5E682134
      9C02B6E37F3229C6FF0B59310FF13A4FDB8C1F3F3E033329199846C8C0044806
      B6E966E0DF7B32F0A74719D89D9C111313938119C60C14C90C6C22E31464608B
      5C06C6921928993C7065BA18C6B2560E48C57FF345256247BEDE124C859732EB
      FFE68B6451DC1AFCF22B3A79D6CAF077DFCB89BCA4A295512C6728E70915C6BB
      BF3BF0533105E5E8573032214E0DEBEC50DECCEFD5A0A44895F7B0444A2AFFCA
      668C8D8C41BC7666116EADA359E4852276BE052F612B3228F170540EC1B10440
      9C5369845DE575340B3E93E1A79C088F62E2B042046F31B8453C19C4CA2BEEEA
      B8A7AA05CBA640632006F0B68EC6612B3161BF65B87D5CF4C1AAF7A72DF862DE
      E21E6B5A7D59FFD3C8CF62579ADE0CFA48180291F9021AC0FB3A5A2945114992
      3CC064F8BFB38EC685A114B744048EE844D06FFF4BEB68A0C7AA02035E524167
      812F2A37FD6BEB681B42335E614E39F7E76630711C0520EE8BFFD13ADAA63E09
      4C934B973C2EFAB3F5D10D4C9C8F8CF152094EB4C84B2B0A9B8A62D589248F8B
      B02FCAEEB063A78B8BAC6A1FF28C8B14F87B5DEEEC114E807C55E8C35B6330B0
      60C07703278049C57111E696E47111164CF1655B72F8ADFC3ADADA4BDFF7587B
      F1871EA1FFA7D6D1102359FD1F5B479B113129636EF4D31977BF8E163AFB4BCD
      17F3C6FD9BEB6807C6EF9F8E4C7EA6A4A221E133FD9E7C67EDDAB5BB71A9ADC4
      832F2C17CAEB939CE13C94FC6EF3F74487B08E76111325A23C53875024AFA321
      8C3C68C6E85ACEBB8F5BAFE4595FFE4628E6864CF6C88F7F6B1D8D6979508101
      B6D3E6E084EFCB3867300783A9402CDB0A3C56E5354E2EC61850C931E40A093F
      7289E3E122DB43977815107BE7AF7A8857A9FFFBEB68CC8A9BEB92064A6ECFF1
      E5937ED7EB68D3B6352C6D38651A4CB6223C4C4A6CEF7E5C542E822524BC3D99
      09DB9778969B720C80BDAFA395F3C34110FB4A311764076F0F0E5CDE7EAC500F
      040ECA56DC4D5676F74AAB42732D07F6F66082A805A838820CF65362C7A67F06
      DC91F08FEE30BA93149811C54560D417D0C17C57EA4E189C0045EE4096D3DFB8
      C3E84E185C02E1BF7D87518D0C7093430118FCED3B8C6A6400E2AC76E2610358
      D9F1C802F8220CAF7718B19B075E192033E381F219FA07025C035815E1F1FFD7
      7B1D71870B97794880FEC2838B6757E8FFE85EC7B20E07C45B83D84FD0B12C41
      3958B37C1EEF4F029E7B1DBBC07C11D803C403B7AB40589C05CE0301801C5911
      0602510DF49F80B1C2ECD9B9D5DD61043FC98016A8A014242F1D1B613902E807
      AC00644B6630162F8F837802F44A0A25AA032C57037AE03CC045F5AEEE757C18
      81E603CCAC3EF4DFC04C80EE517FFB5E47652985BAD07703AC9EC3E355802B92
      0A7A0C501F79F20BF476800608C5DAE836E835DE6124E7013CB272F10368067C
      07B1A4427F144806B612D187C012A018B8E33B8C447866750A0F26088D76E2D1
      11B00117807FE55EC7F7406812B01A780ED89B99F3DC61E8AB00BE5D9F331546
      D2E31109AC0438CC56BAC37B1DDF81C71628AE5D90B9FB60EE06BC0D140139B0
      374367958B079777BEC3281E2DED21BC3F0C547FAF2388DE84A70780AF41AC05
      DEBF067478676216987361FE5B771879F2804084933D0E84B683C96220016666
      5C00B39C02B5FA483EEC3E000A805628084AE87C51D22EE8E3813F01A6A9832E
      2B7E910DFC009325D0938060E038E0009C802CA64B8FD4B6C3BC0C98072401FE
      8047FD0A038BEB0474EF0CE0C029390E46F7C2AC02228078A04C4C90BB0DEF6F
      00FFE80E234E99002255AAA877BDDFEB5865801A1C383506CC899A19F06B00D8
      0E5AF58A635A9D0F4E85BF46AD6E1BE0EFFF586C6CECCB0C6C209B8A058D0E08
      1800544BA33A4711DFBF31F8A49DD2AB77EFD7712BC794A5EFBE3B74E9D2A543
      5F9A3FFFB13EBD7B2F84DB34F889AB8E49950C10F5607CAB3DF2C88409C3E7CF
      9B973878C8104B8B962DF59862D6DF3F6890190CE3273EFAE80361A1A113C124
      0C4C1004CFDB54550CB47A83A13362D977D8B06101D131310A4C3108988D97C1
      E6A8E868C58370E3BD78D86DDB0574354025E595016724BECB3A23700092813D
      3368AC4B7A2C0F016C9C11058848C43F08FBE37BAE33C2583C8EE5756F0C04CC
      3B98A2A3A262E2E2E3713594A2BCFF0A66EC93A1B8D858554C74742487816325
      31796500992A30558F82A281B15218D02955D87282BF8F23F855C323C7A49267
      B62CF57D4B8367338E6FF7E8D2A54B283E65B1E906946E39979978F2049FAD2E
      AC9B9D3B7DE6CC17F8D4BD59E6586AA8C491EDC1C00F227AE9F5458BEEBBE79E
      7BF4480AFB63B0B30792DD669370E8B6F0D18913BFC452D95430CCF2387AF4DB
      0379ECD5F8D8EED1B74F9FB9B39F7B0E77BEC441DCD8AAE771858ED84AA74F9D
      7262BFF0A9D56BD63C838FF175B0E6C610DA2D551503F681FDA97E63460C1F3E
      76D2A44958330A5308A25C9AF8FA2109172AB9FFF7C61B97DE5BB66C29C4F436
      02548A3DECA83A06909410161A12F2D8E38F3F3E0CEB9C3E26B359F68F290369
      E5472B735E9CF7E22758897A19A2390F62125049C9012AD9DEB2E02BA06AD74F
      4B7B05E7F55B356BDE5C0BAE74E8E0413B52B567C7CE9D53B006FA1BBCBB00AF
      AA26061C4887BF931B8C4DF3CFCF9C352B18B599DE7CF3CD1BB8C7E5252C2FBE
      030FF9C03F525CF1121A376AF4DDBEBD7B8B8F1E3D6AC75AE71EACFCF108D05B
      3DFA5BCCF8AEAFE796BCF34EF69AD5ABF3222322DE81A87CFF16A52A02E18639
      5DF7D10F3D7476FAB4695730AF3A02FEBC366EB0FF5B0AF753A96AB76FD76E6F
      B7AE5DFFC064540B505100FF9AE27C084B8C8FDF989490F0231AB6385016807F
      550568359A55C05A500D06FE75650545AEB11F40F703FE75A505C5FB80A140D9
      C00AE67F553193BB225E5345E1D2A3459FEB8B7E211CF7AB8500C16C869D3F6A
      3533AC9646958EA848028AA3027D814EA7D5FA5B2D96288C32E219585E8F4673
      EEC76E60526587C4B2F3CA808923A08A89E096873A1D3B756A889DFB89CFCD9E
      1D89F63F72EA9429099D6107B77A1861F8B15F0EC3046F87570608202276668C
      1A52860E19123D79D22463EF3E7D94E92D5A28F07FB08A9EBD7A29713F8801A7
      26A282838252E1D782B649713B717EAFC400C30FAE54B8BE4E1FDEB14387308C
      EA34185D08E87DF8A41041FEC4E6D8B838A177AF5E6A74A9A1E8F42310293587
      65A2E55189016AA980D86820EB50B49A1A6C6990EF55432CE5796BB8C907B390
      C914121A2AB4EFD0418DEE2E04F65A0E5B9E389B95FC280FC8924B8E0643427D
      645494807C9009237665DEE047B643ADA62C65845F6EF2B0D8BCD4E47449EB13
      83BDCC67E1917101FAE64A2940F727EF8545E629508AE4350478ACA40E5D2C32
      DDBF22B3FB9C9F85219775A95D2495A1093C5980DA4077602CD0A952C620A93C
      FED461881E8E81AE8EE57D3BA3B99BAEA43EB3FE729FEBF94E1FEC4F226DF61F
      5B8BB2CEBE2B155C5B4CA6B06F4178377014D802945363F6EAC4466395B8E5C7
      8AFD429DB13B67243AF5D1E8E4C76095630C888D19FEC1A98FE2671D3A1437F3
      E0A1F1CBFF7C7BC5CA55A3713B56170E838CAE249172D44B8C902FCB5D8B1A9B
      3A64C890C1BFFEFAEBA86BD7AE8DC1B867CC7DEF9E5C251307838FB69D7B0637
      BF8D1AF6E08383F171521B22D5F2074509951A9E888902BD96353A3A3AFDF1A9
      531F3878E0C0E8414B8EAD64E28CED7F664EFDFDF7DF47CF78E289076363625A
      22F63E1CA606B2159C05144B159A8440EC6369DFF4F1F5EF463FF1DBA1588865
      EBE1ABD3CE9C3933FA8D456F8C80683AFAFAF8F0FF65F2F79A5081424D2F2896
      7C91A74639E0D3319A11DFEE0D7D6CE7C12F779C9981D5A9313F7CFFFD2854B0
      FE682A62501FB4ECB7267ADEDD47ED1A4E0FEDF85A1CFEC3B65E135F9B8E7D5F
      A3316C198DDD3BC3A3A3A39B6359CB5CB368C6EC55D198BDDD80B0322E63F686
      E37D21B08146EFDEA8E9FC62CBD6AD5AF5DB9891310A77078EEAD6ADDB401482
      28642C7F17D4209A12E21B646263F6DEAE2FA761DF472013CD18CEB7C2C7E0C8
      B7DF7A6B2416B63B21F67798B12529E807066F031E062B60EEC629928B2DFEBD
      19152E65C8E0C10F8C1B3B76182A617DC45E7FF7B2474503E14A9505D7BEAA30
      3E0D6F919E3E1063A34128C2B190BD1A11A8413CF071874A89E6C23F363ABA57
      5C4C4C5F98839132E51D86BD236F5C49AD683DBB00DD419CC7A595527A4794AA
      F0C4A230C2AD1DD0113003FF3A030D882603B5002D2000FFAAE218331326CEE6
      3B227E271EB97F50A249D0A03FD6A082E9000BCC66D8E9519A94E054656AAA64
      808CE471910A45D26A319B63D12437C079CBA6E8AB1BA1E2A5A2FF0D811F8DA3
      CFC7A1346A672B14EF31C002602A30086807E0B408D8DFAE1090632D8F8B50A9
      6AB56ADD3A09870E83308AB0B66BD7CE52A7766DDF424D48ECA99851039CC6B0
      1E242A6B63AE24047D2DD70D1E18471311ECB00E0143258564573B2EBA1AD431
      E54CF4B0A13E41E1F554CE5C1FA12073CFB89E69C2C3BD1B9207A5440757EA93
      51FD39F65A8826A67BB76EF183060FD6E2E254BEE394EFA3A5A737DD6CF4C349
      5B1A44440DC355275AD68B0B4E8D0B0D55A875364154E042EF92ECD873ECB2CC
      A3521E206095E3A289AB2F37DF79BA2819B58E06B64DA5DA75EA24994D46915C
      76BDDBE550620A40265AFE518901E42F424495C64553D75C6ABCFF5C511C07EE
      DF2605E2C6C6796C7F40B341382A863F7870E1CE187749F4D953292A31F0362E
      9AF5D5E5B41DA70A92384CFF36C9AC9542C0380B07D0112B5854220E3B6CB9E1
      6739DC3E2E5A7E504CFDEE78613D90A28AC40993E512DF2F4B7979F92E52E9F3
      45A5DA89091389C979F240C12FE581C8F0E0972B4FC0215DBB36FBAEAAD290F1
      205E2216F6CB5768704A5DB8BCB9103749E71514172BF5967C51A1C26DD42509
      F13010B83220500BE028BDDB28931950EFF7A3449DF531B35E533B38D05F39F4
      9E34668AA25E129833938963AC245DCDBAEE7CFF8B1F363BACF105A4D4C52273
      2E8116A32DE1A7A086635A411F0CB483B927D083F4016D248556EF2ACE29ECDF
      26C9A4516B30EB22226C090337CE87F366A6D5EBBF3DF3DD91EBA2D3106A22A5
      361019C2C3174F450349FA8419CC641370053002AC72C6F56EA4AE9B10A1B015
      152805C9A5D268B87F1739F2E4C43FAE2E5BBFDB7D3EC7A972AACCD9B0FC02DC
      BF2251B11A817F050E01A7805F58D6D0896B60B06C287998B1575D52AA7576C9
      6DCEC9CDCF56E33B4D63C01197F3674F1FDD7A343B293B37CF21E5677EA639B4
      E00BFBE96D454895BB24281542CF0464C529E8C9A6C6C9A1AC95017901334422
      0892D3E91025A74D7BFAF4B953BF5D7024E6E4E44AF4C79AB714BB166EB2679D
      2AC2A0D8431C612AAAB21454B42E79E322C72543A93116AE59F3F11973748386
      285152EC85951F1E3DB6612FCEE81763C6AB4AE24CA5CA14B0A3074BD76EDF5E
      AC30A56B14A41D14FAD7977BBFFDF42F5CF87F1A5B7E8AAB8B3D87AF5493D9B2
      3CDEFE6AEF5649634E7709AAE2D4EC4D6B6E9CD8918D1D535988BD0DC4E54A55
      DEFFEDE66A45F4D657FBB6A264B425B7D3AD3CFBE3C202E78E88E31A8D02FBF1
      72201A2732F6EF3300F1B3884D5BE29F2DF72561E76BD99743430390F90AC4BE
      100C58F63532E09ADC06340603DED44D582E440DBF06C2662CB534C3BBC266B7
      EF4053910D33338156B562062CA6F6F0C2CD4530745639786C04E19FA0B312F0
      30008D016EBF7643E7E9CC1A19C05F393566AF066D13132B67291BD94E035332
      500BD0026C07EDDF532248311326CE66BCD6AC6A8A05F7CFFC418E4E4EA59AF6
      F8941C7CBB5141116EDB1704526AC0CBC12D43158C70789CE5EFD51599CAC479
      546DC4803700838000955A4B131E7994309E918FDDBA105221E151855AFCC6A2
      5B77A995F7534A1C74B4BEFEFEFE89383911968E8971BE538D995CB98A0614B1
      B3A35565FA1828C8BB6C59478F48E827E409133EDDE255969087D77191CBE924
      9C0FC7558B56C0442141568A042270435F4420FE7ADDDF480166AD6CE76FD2C9
      71AE24223466DC7B55982FC24E58019F4C38F40CA120CA2A4FB4CAE520339629
      72218641259694E04A0C904CAFE322A48A542242E36A9B69D367D2F8F1132834
      229C9E7BEA09F437259D3F1700B44F041A84FB97D107095E4715BCEA67AE9592
      128769341D3EB679EE420EF4F3F66D3D71010F75B9E71E5CE2A3910FE8B76FD7
      86DAB46943B8DE43D6DBB76F4F1D3A7420B7424DBF6EDF563993D10430671609
      17CF0AF34538684E0AA59A9E7A6A26FF77086105909E9F330FFFC1803B8DDCB8
      6E00E014A0C4D1AC59B32028F29A022E9E5EE78BB6EFD8D5B375CB9684A51632
      19CD98F55250E776AD29BD59636AD32A9D3A22351DF0DEB8611A16EFB4B4FD97
      5F2B2F12A19889C85033BE059AE0DF04C2315C57605284BF15E8B5D75E5B828B
      B4E5D835C3D5992C0ADCE1225F7383C281C1805336737ECD9F3F9F70C74BE514
      948A88476C5C00FCF155AFC25D449C2AC2DE78390F701D14611686B89CE32218
      C254A72CFF56AD5AC9F2C795BD48819EF05F089553C082430C78C6D7844A56AB
      FFBDF7C6617E54CB4575F15B6F2D99397326CD9E3D9B7033B14C1817C6701019
      E823E4C2C0FA2BAFBCE23D05EC133D156734FEACC15D8405391DFE0BC414131D
      2DE28EA29E382B450D1B3624884D8E258B8AFF888753C2E077CCF5C9FFE75265
      0A980964CA79C16B3741B553531B4E9B36CDF7E0A143EF711ECC7872568DF5E0
      C5792FC929285717996C4570866356578F3C48BA6FE0C0541F5FDF159EE2C7ED
      8D88FBA504FCB118174D9E5F65D1701E22727271E64CAE9601B34340153E5703
      526BD56AD6A76FDF2FB03827FF270898CBA58689B13F6F50E34F4ABCD957B003
      21119FADFF60BEA802B9CA2FDC74A33FD0FE3BF34595E9CB36FFCD17C962A8E1
      C1A5CD083FED808EC07FF34510025484D1A89A1B1DEDFB86BF7FDA9B3E3E7D5F
      F7F71F31CFDF7FF8332653DF695A6D83514A25FE9985F8C30FBEBD2B4FF75DC1
      35292A4AFCE695577C3F9F38B169970E1D4637EAD9F3898623473EDA74E2C491
      2D1F7D7464CB51A32635E9D56B7AD3060D463F1610D0EC69B5DA2F52899B742A
      502979A9C420292C4CB5ECF9E7939A366F3E3AA165CB19E17DFB0E4E78F0C1C6
      29F7DD1759A757AF80B49E3D039A0C1810DE72C48846EDC68D1BD475C488195D
      3B7418B3B07EFDD4441F1F7509D95BCF0A0CE2CD66D5B28103EBA5592CE35476
      7B2F8C9EE2B53E3E26A38F0FE90C0607DA96620CE18B757ABDC30A3BFFF07063
      78C3867149BD7BF76CD3B3E7F86F67CD6AB4EFF5D779FC5AC641E93145AA548A
      17636292E2B5DA0770AD5B63477EBE05FDA44A542A0B702BF4C562B7FB8A42AB
      CD474721E1F65CA3C1ED0EC2B44E904064427362D5984CF50D01010A0C6CECC9
      C1C1078E5EB9E264DA70678DE8F58080A0F62D5B8E8E6CDBB6933220C05C288A
      46854A251565671FC31FA5EDD2C4C7DF54A9D5F2680A5F38A2D26E57179C3913
      8CFF67AA255CBC186EB6D9242C6A16BA95CA9FF71F38F0769BCF3F3FCF946511
      85A9546AFC1550239FF8F866F874D53B30C121D9EDB82E4854E1B05478EECE9D
      915276B6CA6832B918BE7E7E0E436060812236F64CA68FCFB693972E1FB41714
      16387272348E0B171AE8F3F29A4763429D19C8221A8D1E25322CAC155A4D1FFC
      0F17EE71B265DAB5B8F750AB351B03022CF64B971A5D5CBF5EA25EBDFEF48D8B
      2B1BAF639B964B1D11717353E6953D616673AEAF4AD510E1CD0697ABC5248361
      FB6336DB25A555A954C4858484631234DE8D591907D1F533376E7C794DAF4F4B
      371AD3C1D4EAE3EF6F9532331B5FCFC820559F3E7F9AA2A26426DC93E15B59FA
      3D3B3B6B4F56D6A5F66161D1E88582944A656CA89F5F943527E72A2E4F121416
      83215CE172599DF9F992A3A0E0DC9E63C7767C5754941D1A19A98A33189A2888
      2C3E66B3B5B8B0B0B16BD76EA150ABFD03575A62AAA8483AF5D75FEE0B172FE6
      ECB2D9FE6A66349EC49E2D7FC1E532616E231A99BF9F902CEDA6C68D471CEBD5
      EBA75383076F3F3C60C0AC87E2E23062F14F7D78E8D0D147B1B1EEDA9B6FEEBF
      B168D1A91B6FBC712A67F9F2DF0A76EF7EF7CA993393CBAFA305EA743E5B5BB5
      7AF464972EDB8EB66BB76D539D3A937D944A3DE1A1DB58AFDEF8DFDBB7DFF647
      F7EEDBBE6FDFFE91189D0EFF9E6594D7D19E9D3469DCF1254B965E993F7FFF95
      39734E65BEF8E2A9AC65CB7E3BFFD5574BE63CF6D878CF3A9A11C436376830EC
      70EBD63F1D6ADE7CFBC694942740DB403E0A85765D62E2A83D8D1BFFFC5BAB56
      DB7F6CDC78DAA31A0DFE284F53B68EB6E8F9E71F3E09266767CEDC7FE6D1474F
      9DC2FF4DFC3E66ECDEA5AD5ACDEDEAE393C80383208542B72E2565DCDE468D7E
      DE9396B67D5D5CDC14D0D6935514359F4546F6FD2925E5FB5FD2D2766CA957EF
      F557B1F886D182BC8E065945B66DD3A66FC6C71F3F7AE28D37961E1D3972FFA1
      7BEF3DF56BE7CE277E68DAF4EB15818143A688A2EF5CADD66F5342C28BDB6BD7
      FEE5E7A4A4ADA0F920686B998172455050E3AFA3A2D66D4948D8B9A556AD751F
      4447B78CD268501894224AB0119BFA1A621DFF41FC4DC3A4DF9E7D76E9D68E1D
      F76D6DDAF4C8F6468DF66E484AFAE26D8BE5FE253E3E6D3362633FFF2E3E7EE7
      3751519B3E0C0E6E0B067235A0577C7C42BE0C0A7A7D7D58D8AF9B6263B7FF58
      BFFEEC2FEBD70F4DF0F1E18F0F154614A19E75B4CDAB564D5CD8ACD9A28DA9A9
      DF7C9F9ABA7B7352D2AED551511B3E0A0959BE3E3272FB86F0F01D6B8383972C
      F1F38BB6A0187165A330E4C33B3E3EF77DE6EFBFF98B9090DDDFA6A57D737CE4
      C8513B9F7D3628C06CE6E5DF0AE3A21609097DDF8F8E1EB53E3A7ADD86E8E85D
      6B232276ADC18D036B8283F77C1610F0DDC77E7E235FB058F43271CF0316512B
      ACD6573EF2F1D9FE6948C8AE1FBB77DF7861F1E2C987E7CFAFF5789326FEFEBE
      BE7506DD7FFFB031A3468D08F4F36B3CD9CF2F1EF29FFA6950D08F9F0404EC01
      D13D08FBCB728BE57F734CA684D0D21D6D1EFA14AA502817FAF8A42F359BDF5F
      6A32FD8AC0BB37A4A76FDD7BFFFDCB7E6DDDFA91A7FDFC060C8A8F9F763F8ADF
      10B5FAFEF93ADDF8C516CB8A6556EBAFCB2C963DEF994C3BDE351856BE6234B6
      0F11C54AFD82CCE8B98000DD3BFEFE1D171B8DCB16EB74DBDF361A772F0B08D8
      8942F03364FAC37C1F9F5D73ADD65D0B4CA61FDE301A7F7ACB60D8F9964EB787
      FDBEA9D37DF43FA3B1C74CFE4093A955F17826284837CF6A6DB6C86098F7BA46
      F335F0CB428D66F7AB1ACD6F2F6A34C7E6020BD4EADF5E53ABF7C07EC7028D66
      CBCB5AED6BAF198DAD9F309978CAA70ACAE5AC8321AEE92653F86CBDBEF74B5A
      EDFCF92AD59A1755AA1F9E51A90E3E0D3C8F367FB652B9F109956AE958956AEC
      435A6DCA64B3B9424F568E5CD5C640C87282C1103049A3A9374EA5EA75AF4231
      B58F42F1E400517CB09328764911C5BA18810561CEC5BBCCAB265DD105E95680
      8816E3130BE0076A46408B56962B5259AF583154C9DB7FF717DD3AC2C712198B
      6930069B2BE0F60384151CFFE64B294D7954F13749DC51B04A2560EE922FD6B9
      DD526FFE1CC5A57E95896042AA8225A4C28D269A9EAF9E1A7B6F9F0A6E784149
      C3B39C6ADDB5FFA7E34774A1A66931D4AC7E1C3565A4C55293B4386A5C2FAE44
      2F353784DE00766975E369EFFE13C95B37ADCE00A9CB40992A4901CBAB34739E
      7EE31369DCB07BA8DE33A7C96254925EA726354ABC5685B97D408D6946DCA549
      1A4C99694890FFD1E1A91E065AB1EA7B9A3371702350DE07F0D96489CF7056CA
      030922B0D91D64D2ABC86A5193D55C028B498D7F9E5491D9A4C19FA1AAC86850
      935EAF248D564936BB535E539309DFF62861501A7B76C3DA0DD98A9D58B1E298
      ABA1AB304FAABAA583A052832B7BD40052A6805E6CC33817B35D1CBE0CA5344B
      1894D972B64A546873605657411C58061A09A5064C186AE820AAC0728802CBD1
      0A7C7730039C8D2A47E596B18401E741A91D4FE51415830188882A251821A64C
      9489233F348056A7241DECB46C877CB1391C98799407DEA554A095D22C6180F7
      32854C2804038512C4C140051461EEF09A4B49A78A14B43B9BE8C72B127D73CE
      41EB4E14D1DAA385E4401E80431989F2861206A5F2620737EE112DB2B9B08AAE
      A0029740A7F2053A5F24D0355CFD9DA3D01096EC08FFE18AE944FCF531228145
      6A62FF1C8EC397A19466493145103820A7881E7FE93DA95EA306F4C90935D924
      4CAC420C844CC5D70E09480DE1F252C98EA3380EDC3F8EBC72D8DC34AA968D0E
      ECDD4F0B9E1875AB9882202B6ECFB9CC3AB8CCB28584B25D80409F8E8826C98D
      A964C20F62C3934AC48038093C22611DEBA7F8685AF5DD51CCC6E35DF654FAE0
      3C402A94A5AF655A517EEE4F470FFCD6E6E97D7B410F19877A21371BACCB6440
      88159A08C405F3D9228998B8B61715FC5446A49C412867F618E360B002B21A3C
      61D6A0C8E4BA838E5FB81EDAA743035AF7FD6F9418EE77E9DCD1439F7EB2F885
      4F654FB71E99305E00CA943706658E6C98F4E2BB52AFFF67EEB60CDCBC3CE0DC
      0AEC59327CFDFC8561D3CEC30C13ABD208EA8728808617C840741C57D4BC9057
      403812180DC0EA18184EC0700186CEEFCF1FDE2E5FD4571B8FAE1ECE9F09D481
      2D0EE00AA00CA021D540E64420863806C800A2FF40FC0A8809A21150E9C3F207
      382C80614A121FA80900201F410F4CA5A7070000000049454E4400000000}
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
      2800000089504E470D0A1A0A0000000D4948445200000012000002D008060000
      009D7120C100006893494441547801ECBD757C1457FB3E7CCFCCBAEFC636EE1E
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
      B3FA2EFE6E429CFEFF69EF3AC0A3A8D6F6373BBB9BDD944D6FA49000A1844E90
      A24142E84505050484202404A9174405149522A020780511430745A4CB956E80
      0872814B409AA1486F092D4080846C363BFFFB9DCD2EE99B20FFF3DF7B7FE739
      EF29DFF9CEBBE79C99F96676CE9C3336452E5812AEB531A802B66798B7B85264
      4473F7B5DE5D1BFB28E101C673CA4BEA79EB4E644D9F3AAE4D49658BC86F7FDC
      5034FD78A7BACF5F7DD9ED5ACAC6D94AD2C1634AAF1EDD4FE2FA1A54A4406982
      A9E33F1277721FC6F7EFFDDE987119AFBFDEFDB7B0B0B0E9B8D43B152EA7C255
      D7BDB0307FDA29B2BDAA7B8F1E5E9035074288889F071015B0908B16BEF5E5AC
      D99B3EFF72F66F53A64DDFF75416D20F1632DBD97D9657DDE71A7A366C8EF963
      D1418606D1BD8CDA7258C8EBB0908EC1A14D9F6FDEEC62BDAAA19995BC5C4CF8
      86C8039D5FC879E7C62FDFF22D9385D469BB663938B5A814D1E4A2878F973726
      C4EB329CDC4D39EEDE94E5E8A2CE34783E7289686FDF425EBB95DE59AA5039C3
      2539C94933778AD671D9674EDE4B3E750F5C3ECD3364F90C4FDF45537D1495F6
      91677558C8C3875AA073A9380B294D9FFEF96E55D4EBA1EDB77CAEAFF1C7564F
      72C09ECCB84758A006B843274EE7D29A5907AF4959A96EFFF8ECEDA9874F9E9D
      CC6445F0E9A79F254EDAFD476AD2F8D1A9B75A57325DEE503DE7428B10D39926
      FE39276B194C3F857818A7FC78E4FA870BD66655F62FC5428E838574A8DF66B0
      EC5EE19C74FBBAA359450A06CF252322E9F8EBEBE9A8D57A54ACA4BAB875A17E
      D6E4522C2465DD5B643C77C494253938DEF30ACD7EE81E6CCEF408CA7DEC1160
      F6A918AC3284D53265C3B8A5EC298785D4D56B9F6AD43A67A9C92CEBB51AD941
      A39573524FBB984FED72C6487BD92DE4AD2D738D7462BBAF72F9A85BD6E97D2E
      57B72D80851C73DDE3692D64D3BF2CE47F96852C720A1516B08574F1F16593DB
      0C792164B59088FC7B39C95A9D769F6C5638FE5D684F0EECA2F785154267EBB8
      0E82432D52F0CCB9F8BE51A57AE45C19A73CD2F69CBBB926DD397FC4A626D838
      D5EAC3358A67E57AF445E7CA9CB48B913F9EA33BE78E50E224CB2DB38D287AEC
      F78A276AF4558734BB24AC3074B39FA8D1CEA9BD0487F03823EADDA58A47685D
      FAFBEB7539691723561EA5F40B4729697A5FC1213C2EF5E2A8458A67681D9ADB
      F92127ED62D08FCE74E7C231DA3DA3BFE0101E976A3672016A549BBEE8F91C27
      ED62E48A83A8D171DAF3459CE0101E978AFC5B0288EA70B4CC48478D7EFD72A0
      E0101E977C61D85C10D5E6283DBC79598485BD0A818136518651256AB477F620
      C1213CCE7D7EC85782E8D1ED6BF4DD072FB1A858F49EFC939033E98593C7E89F
      73860A0EDB0189C7CD98866EB9DBCD319B6948C261CA4C4FA5C5633B8A82C579
      5CC62AB71161663B5E97B2106583C8AAC471AB7291107A5699A816279A0C9CA9
      B805D5A0EC8777E98B77DAD0C8CFB7B358C44524CFB3CAB969E78E27D3FE84B7
      0587F0A0E3D5387EC630841F0134FEED929B337EE62656B1E1C0BC51DE48DCB6
      1121C18EE73570581EA443F936F06C9CB54605D870431A04412BA029100CB0E3
      836B1F22890B162CB882B0A8434141181717AB8A8D8B7B1FB802A4C6C5F6FF61
      60BFDE53181C6719C079EFB32E3359CB725C000A2EB17103B60C88ED7723BE7F
      4C1F1476E81B3F949F6609709C659CC73AACCB65446178121879545887F81A4D
      6EA6E77D7D70FBF31E51D967BDDA0E50534E0C49720DE4E1E63EF7A48934CBAA
      DCDE36BF527A92836BD6E52D39B2E31DE4B165335A9BF481CA688A3BEBD7BA61
      9AA18E479A4BEDEF55440DB10A1C659BC0016D079988C725CD44C97E0F8EF7F2
      CB38965E25EDE764B356BD00A364B3D5717171211891897FE4E71A7BD9BD51C6
      6DA7B00D1A4569F8D844D9216E92A66738491EA8EFAA93A41C4C557240D8F0BA
      A1F662A3AC6BE14FFF1AA0BB93B91023A9DFC90D1A34E886D1DDAACB66CF7D3F
      B7F3ACFE78397D48668E945DCD53D226F52255B340A210570C51562755D26549
      BEF2808C3A49097DA0F3BA7C705C8735F523227A4A44E968013593CDA61DA83D
      694DA61E580C189725498D2522A425C74969B69CA4D8CD24796305AE702F924C
      66498D77FC1507E87299BCB2514CE42529A60B2C5488AA63D449C2B2B6D2C57B
      A48CD84A526410D1B416441851972EDD2745964842AF49ACCB6550F61C423F35
      BC020EED258CF993037274E89BF71A11F93963C87925D1FEEB189AD680062C05
      0A21C135BAAD48EA50C4F9A74E610FB09A02322C2F47CA87BF12C56F25DA7B15
      8BDEE15B8A5052D0A78A44748ACBA06C6584694CB42757A56E890419D5EA1FC8
      CC1F0151F823823C1C2FB50C463F058AAF5EE0F57E3C8A55143C97364B421785
      F2CA26F15E4B47F6B0F0D69127EECA55363C74F08AC6111A8A2662F53D4935B8
      01494E68CEF21452342A8CBE11399864D55EEF477F8C6C1F21456B3273DE80EE
      473206A3EE4544D47772B89F353E57AD5B8ABEDCFEC0C11727AB14AC42E2BBDF
      495A914212C6F99192D46856B2FF83E33101F70F9BFDD252366240693EBAE397
      524F1159C129A2CA3B45CCB92773A5924F11B4D2E262E3E2FED4492B58709A60
      2710B16900616133321967FD647B66441008B67C1E887162503444CD805080DD
      05787B809D0B162CB88AB0805317483D494888CA80029800761C6719E771BA28
      5003915942D3FE332DE4636B93FE94855CB060E16419FD030B297D95E5E31297
      E2DEE5CC0D971AEBD5A4348189C5275824F5BF62F058AB1ED1C263443801B335
      1205DFD7F936C891F5F37CD4C78EA9334D1323221AAC55A1EB5BE3B04F5B3E6D
      56E22DE7B0BE5AC5FC42660E65432E5C6537523CF19A8C48C0CB8409D698CD2F
      B02E97E1B210B766A2622D243285E1B75A48AC07C9BB1FB52AA785C42BF75A26
      5B82FB09B6901C67946421B9469C6F8384EE875123B6902CE42B086C3547F9DD
      7E2D9B145C75607984C8E63151A916D2A68988B6140BC9A7085BC838E8090BA9
      31995B4A122E168AA41205390360DB846AA09F602115C521BF85C4C38B04BB16
      B24365BC417697C88E851CF74C2C240EC88DCFCA423E46EB2D2EF6BFCE425ADA
      F50C7C9939DA8CD9B7AA7EFB41636BB58E1F1DDE6AC0D09AAD060EAFD572C0F0
      2ACD07C456681CA30D08EF7CF4CAA11FAC26978B14011FD9E4E22C575DFD7E84
      CBECA1F569CED0067841B49E3C63505DF99B11F50C6DA2AB8DF00F0EFE2AF2AD
      9F1C8B94CE2710442A49724CCF54E43F30B27CEE8E994EDFCCA5235773C81927
      5C6475377A313228DAC9CBF5BD7CE58A4405118E7BBEF320BD9AEF132572C029
      AED3AAE8E6BD2CAA8E47C0FD9AB852909F53EF22A5F3090411A7F9ACD7A0C7D4
      38FBD54C84B565779D7E14BAEAE0EDD045BB6F84D6AEEA5B79C8E2A38F862E3C
      92193327F94C8B317B3E7A217E839ECB3204116A84DB1E4CB051A9F02A9E8A34
      78C98F6B8595B969106E397B34F5A79EC087AFD5741CFD6AB8FEB33E75C2D077
      13D077ABD077B80D23B21081C9CCB4824EC13C22341135CACD55E86EA642A5F4
      5D27F4DD542E9A4784057A914259511B4B8D64580D4C7141534BEB3B27BCD284
      A2961AE17E0C6BF49A385D04C5F5DDB97489F69CCFA6FB590A69F8D7514AD448
      526439DB54F4C1015A2C1A8B95390AF49D1E77ED2EB8F3C2EB31A2D6E0B1D428
      C798FD48A37221571D8E1D6D0EB9E94CE4E9841A82A9B8BED369B0661B5E8085
      2525B6DF4C84230717BE5CE57ACCE4646FA3895C71F39B855B39E429E482853D
      FABD1880BB3BEC15BC5ECA17051C198417B7B037F04C0CA531A2035D224489B6
      7CDAB4AD4815F2BA4C4CCEC9359BA00357288F9358321A9348B13790107D84B0
      5887BE5317D7775665CEC322C682A9F89FCAD3CCC49748D0771EDC77DC173827
      716DB36472695965C0BF049391259CE6B058B478FBD75F30EB290C47931FFAEE
      91B5EF5899EF95D1B487AE4E5AFDBA898DD0B72C7D0628A946DEE0BE05D875D8
      6B4E5032E27C4750C8618A54CDF9F317BEDBB7EF9B6D6362FAB4C5DB50ED10B6
      EBD1A3673B2CB2D0AE73E757DB75ECF8729B8E1D5F698F35EDFC0D0697DF8A25
      C2CCC39A53A64C9F131656AD6940405053FF0A814DFCFD039BF8F90536A95831
      542024A452D3B0B0AA4DF0E257C69B6FC6AC2954179C111649C71D3BF667CE99
      B3C03870E0C0EC77DF79CFF8F6DBA38CA3478F31625E81D1607035060757CC3C
      70E08471E4C8314BB948B135C26EAEDAA7776CAF070FEEAAC3C3ABC92F44BE20
      D7A85E4DAE5BAF8EBC64C96219F3FD70FAC9D4A3478CE6C48963C7F6EFDFBBBE
      F0012931BB598555BF61261D747AC9DFD757090A0EA6A08AC1984D17447E981A
      121414441E1E1E42D77248E014E282569CCB8BCCC935B7D12F4A901FF33C45BC
      0BC9BF26E15CC31C02C2FC48BCB9E243989D086DD8311C508858CE7E8E302AE3
      A8E3B0BE46D3B4D2ADEB9261D962B31AB592351A183C99AE5DBB86B9D946F2F2
      F2264C9BC0992F88B808A9EEE1C4DD8113B922A0AC58A153A64F371831C7481B
      1AA23CAFC1C701E7CD53F81657053B9E969A46FD63FBE3DFBA44F84A9C20E253
      8799D447882678190C2337F9FBD3B971E3081D4D15AB54D11A2F5E94F40181B2
      7CFCB894317122797E32192F7B9B2971FB76AA54A912DDBB7B9FCBDBCE3DD52D
      222F077F7FBD77870E7A97D6ADF52E6DDBEA0D6DDBCAFC65413A754AD2612EA8
      61CF6E7A38613CE1AD20AA111E4E35C36B62BAA70B6AC45C0A7B24AD239A5BCD
      C3234E5FA58A246766928C79A1EAC78F55EADC5C49321A157CDF50D2A2FEA901
      01A45AB58AFE999C4C58F2817EDDBBD7141DFDB27AFDFA95DFCD9CF9691F352E
      E88E2E0F1EA875BFFF4E1A10E1010BC1C8620567B14B25E42B17DCDCE8F4E0C1
      541B1F07747571211F5F5F727676E1AA00FC50021D8D3EDABB3F27C7071DEC26
      63E63D1E3E985F21AA1D2649AE2035A70607CBC7870FA7C0669174F3EA65F206
      893FFA136F736286AB86975A0199E5687243237DD06B2EB5F114F3389163329A
      1B4154EB945E2FADE9D68DDCA3A3732BA256060F77AA5021009FD5D4E66EFC69
      93AA76ED869AB56B57AE9A3265625F7014714187F5FAA3971C1C941765791B72
      4702AF025D000E5F42D81A780B98080C03AA01459C6AA087C7E0D70C86C9C809
      034A73E842D294A480AE121F68D77CA7D3F1F9C45720B6EDF9C13A0CCE1790D8
      1C94C45856F9E5CB977CF85768DBB6ADE23E27039FB0C4BA50B6F2E7CF9F1747
      B14D5028823759A95BB7EEB8A092A720E27CDD90B6A443E4223DD954885EA452
      B6A9ABAD99061B114BEEC74FC074E04A1C15B88A19CF3E3E7C1D10C9025EC687
      B105D2AA02A93F91786644059AE63AEF63BA9DAF56DC6719F9D2A5456D4477A6
      ADE3C55AA951A346367DBC6F4C6C9F6D8242919B274FDA2436229BA450E4E1DC
      0994BDCDB6776CB90E6DBB114575B7A5EDF6119304AD3F4AC1DBCE8A42C11B53
      A8C2C2C422E47689B8B4848FDDAAD41A0A5C7510DFAA7522B56F05161780681A
      1FC1781F1BD72C3F58D75336054CF2263E2B2FB7AD425C2BB5A70F99EEDCA4AB
      DD9F133A172F5E24EB266AC4C61CC3F0B0C10A61D92B1B700F20F4B839B2C10D
      1300D288C982B79CB1C871E1141178A246084B754A4E0EDE00C8A6AB3D1A1393
      9A6E5C2FA26F97C8A14D37BAD2A5AEADE0E54EE122CE7215FEBB88043C41C467
      7D6A6A2AE18D4CE2385E8817AB0DF0FAC6B9EDF1D8BF435F31151FF74DC4FD86
      72C2DD4A4B13217B82A89BC51470FA6921EEEB9D513A84886A02B5CB895AD0E7
      72216A58C89B48FC29F7BF63216B7CF108B592F154EE10C2FCAE70FA49DE8158
      5BBCA0858C6917411DC26C9974F406515DDF27E9FCB11EB30BFE8038B2F32B3C
      6DFC991189E3C85A8B655B0FD1B2ADD654F9421B51723CE63CE2DEA851392C24
      BE9D65FB351B914D52283234D105B52CD8B1ACC23B664800C72CB04BC4CDB5A8
      5AFCE4888622D2706B320D79B2FBA95C9DAD2414AD996085276A549A85848E70
      569288F8049206E2360CD2725B482B09CADA48388EB7C738102853D32469A050
      B6D644240A79768978EF507C72819A3007CBCB65214755CEA577866A8BB19087
      F062FC2DE614109DDDED2F0B293A239F57EC3D648DDD352C2A472C4159FC03F5
      0E58D50A5AC821F195A80EB5B266D2314A2C90B665203270DE3CF84F9CDDE3E8
      896AE9B16746A4CEFF3B73E69D47B26095890AA7A1528CB31125472497FB1EB2
      5C1672AC4B3DB2D4B4603578C7C4D00A9BD06E1F5949E2232248895788432E6D
      95739C61978895AC8539CE289C6699E8237B163201FDC7CA0C6B7C1E3EFF9BDF
      420A22BE87C44C43D1D97C0FC9051867CE9CE180061EB218FC39C11B69C8E54E
      42C65EB92DE43CCCECE1826A6F3F0EC89A16893CCF6E1FF1DE615D2E2CCD936C
      242CCF6F21C55FD1D5AB57E9ADF790980D85217FB3B887E4659CF83F3E3F93C5
      044A2A7C0F89C9953478F0902CDC6835167DF46F6521B9F94580E7D4E55E5FBC
      486763F926ACC253B527A67B7E123F60C09718C7FDB263C78E13B1AB3BA1735D
      8AFC6A9EA00011164C7AAAF5C599AB0011AF2FDEA163C71EE55D5FBC0811FE76
      3ED5FAE24588F04CC8F034EB8B3391CC9E15784819D1AC59B3A6E8583DF61C8B
      F9010A87A5AE2FCE0A56458EE391A0579B1123474EC3F3D99AA81D1EA3AA44BE
      397139A9763E316242F989375E9ABA698250B4CA700A14BBBEF8FD732966F7C5
      630AD4DE5A06E14C108D2A4004211626730D691619F9EED0A1435F6F1E15E586
      45CCE8E7ED3FDFFD65F197DF7F51CB6938EBE4035F4EBB80E86211229CA03216
      097A118BBB8D47339BE0F360CAEC59B3F6AE5ABD7AD48DD8E7B240720AB0BA23
      20A9CF8922442CC43F498FFAF5EB0F9F3469D250FCA334CD9C39F38B7DFBF67D
      C679CAD88ED5105AC9B8363F225DA22BB2BE787E4D26033ECE2F2B318E49DC7F
      AD2F5E62EF107D8EB522CAB7BE782964FF6759C51E90F96BB368C1B277700ED6
      A6429A18A048C3A38F71B103FAE6B0BEB81C71A424C086B78C6AD1AC9D834E6B
      53E1A18D5F92F69CB97DEB0E1F9416A21FD76F3C69D32821C22430FCB65CACD2
      4832967DC548964DC635C2A270D5C59A8F3629220A064E5175D2E91CC8946382
      E489E33115536E2E46FB149B508D2B07C6FB9DE971F6639B9023AC6CED177E6C
      C6325103943561AE0C0615C4A59DE50C69F5CA754AD5AA55C42DCD7D9396CE3F
      36A0DA180C9055628C04233DE0E3D7FC483C3A339B31A880075089D79DC63181
      15F26BAF761DCFC337998F8D742ADB8D2830809C7CF129305717926D7046DC99
      5406AC5FE4866BA4C1994203B4D1552A6AA3BD0C52F48D74537DF9D52E5DC77B
      7A7AD09D2CA2F3922F9E1E1B488D578CD4E84C0D4687B15CB51837E23877035F
      14642CAB83151EC8010FE0AFA43DA6B45BD97DA5E5DFFEA084550DA3DDD73524
      878692BBAB33998CA8BF624613F1E2278E1FEE2F0418F25100C29809D6E7C160
      F843ACF479F4E4BDA4F4FBA65ED2B7CB56285E15ABD32FF75DC93D34106B88E2
      6D5A630EFA2307231C96F10E6B3F7088B7013040AE25AD46A18B97D3332F5E7A
      38C2DD49B3409ABF68B992ED5397CE39F8938B871B65A289264C67E0DFE58285
      81EFFF9283564DB9395974FCD8F50D598F73FB258DAA7E579AF0F76F95BB014D
      48171C889FD7E13000055A5698C09A46B7915E4774E1DCF5F40BE7D387ED1F5B
      FB7BCE93FA4E5DA718432328A0A217651B257C888CC5C543424761CF530E8EB9
      DF8F5FDB9771FF719BE39F3CF790B5A5C8F7B72F4D975C89C7D55880E38D8362
      011E21C7E0161F8C5F9F9CDAE48010FC777BA2D9C9388DC441F3346D458735C4
      FE66338240A288A4247A9AED505414AA816156C2C6358AD8B993A8BC64203914
      1D4D4F6A0432EC4F2AF520629DC2609B9227B3348D13B07836A23A7588FAF421
      FAE927A2DDBB3997E8C517095F7827FAF65BA263C72C322E6389912A2F2C1830
      094B5E7A897D0BAC716B9E456AF36D7B2D62F366A2AD5B2D19AD5A597EDD922A
      E8732D13132DB276EDE850870EA28F9E106DDC48B4699345C1EA7FFDB5356609
      31566B89E4F91D3BD2A14E9D04913A4F8411DA5CFB9D9D232E61B62258DCD716
      7F52A3F5EB897EFCD192F1DA6BA5376DED5A8B5EE7CE74A84B97526A64ED5856
      7FE30DF689962FB7849CF7C30F96B8DDBDC6BB9855B963396458E3D63C96E5C3
      93A661E49C56AFCE97558668B76E74A87BF7529A56060EA192AF694FF61A1FEE
      5DBB8AFC327B5C264FD94674C8DAA97919FF0F839457DA9FCEDF6C6B5ACE2FB4
      17E74266BC35D2AF66EDD1F347A4BCF0B3F3C565D74D8ABE7758D8DBE238B247
      60CB4F502A6E498A3DAFF0DB2810A66767C327EADD7A29959D28410947A9DF01
      DAB13326D390F3E89EC69C935DAFFD3FE220AB5E36A204C501CA5701AFDD89BD
      EE29196937BD79320904ECC2376CA956BC85E4DC82F80649AFC49D7D33990471
      E2C21CDEC2A0CC2F6D5B9CB64F94A0F05FCD37095BABE8A50108A8F9B65DD538
      B49271DA7ED312945328C405FBD2406919E2C5BAD289129418945A0A9C064975
      84253A558939098A1BF2980401F564AF34944C44B42AAFE012D4E6B7BC788941
      F14409CA0C94680DDC06DE02EC3AFEF8E6606871879E4758131802B407D8D544
      6D5238620F6A28CC010ABB74081A80E412C232396EDA70682E028E00EB00AE4D
      85F290A0CC5FAE0C3D209541A7A04A82A281A011F02A10089C044E978F2841F1
      47A195802B50C0E52772408E33C08704EE71C4A22D9CCF7115F5FA3A941A745D
      8C7C7C21FCE101BA73691D69F5BF5368636FC842286FE38BC027880F02FA0003
      010E7B201C406A5D3FEA353785BACEB84A9103D642C6797C4670192E0B91C539
      228807D885B307D4042CD7F0615BBEA1012B2FE04B44DF40C6AE2E7B00FFA023
      429B7342AC2F500F980A70F39A218CA4803A9368E48EB3D467FE65720B64E2AA
      902F00D8710BF41CB1821383F212317921071D68E69D4DA8C9516ADA771A04DD
      8089C07C8009FB21E44A20B038470403001FE05380D31F50FB0FC6D2E40B2934
      6CF31EC89E03FA03ACF339420F80CBB02EA216C7ACCCEE89641F8044337ACFBB
      4C93FE38496DDEEB005937C0EA42C8B20D44E008D89C8CD8142016E07E88A52E
      9F2652CCA24BD476F406C87A03DD01AE413C42FE31EE8A2988CB4001A747CA17
      F0A38F8E8FC7E7AB5368EAA57F5260BD00C87C013FC007F0063864701924F107
      0B1DD91DB18B447415A80CF4002201765D6097CE73C41ED450F80028ECEE43F0
      3A48521196C93111EFA56AD0AE015C05D84AFE0B243988FFE5FE1D7AA0E5CA96
      51C078CADB547961B902104411D12EE063403849F8E5F0A2BE8F8A2290B46DD7
      96B66DDD4649BD920487F090512617B92C328A40D2B15347F18E0413FD1AF3AB
      E0101E32A9F1C2C6E3098F100EC41E48A26236E44711483A77E98C0F3664218A
      75B45123E80B0EE1B1B4C1370D140E811687DF3A9C44F936E4451148BA75EF46
      D6B5D8B66FDB0E11D9746D44B566D552DEE8FD062DFF6EB9503831FC44126183
      3C8A40C27978A71F51AC2BB5FD670E5B5875386123AA36A39AD2B2554BF146E6
      92C54B085B0B80DDAE37FBBD4957AE5CE138ED48DCC1618BD3A34E2751BE4DB2
      C643A7862AD12DA345B24A952A342F619E88C70F8C179FDDE1C4CE1D3B396871
      61EC85242AB4D98802270646119A80952B111085D70C1761CAEF2922DCB56B17
      872DAE7E7435898AD96C449CE7FBA16F14E523435C382BC98D493792A884AD00
      11EB788EF58CA27C6456923B53EF24517937F7F7DCA300250F51F4D756A61EC8
      95647526A99CF2290F43BC36C0AEC84E6361B1C03C7ECA448E279143A2DEF1A3
      010181A9487602D8E1793C0765C0588DB6C203B3D27E985AFD7558FD063167DD
      DD9D7F7EFEDD501ABE39883A7C504155060EA1A273F7E8EDE3E6B6F2A029E785
      A37E15142D5ED82577EF00647621953CBACC44CF8F49DCD660EE91AB192F0FBB
      5F4B45929BB323CCC08CC9201A808F8BF1CD04A2F61CDF12272849B84F380A8C
      7AAF56ED762F57AAACA05877805D192B94A04C9412946334E3F64294D202EC98
      A8171E16CB483800765C82521DB5E09A305C30448659899216AD33A1E4EB8070
      6A3E3EB23119D991CC8F84846818C224223A0EF0F1F1194276E37063D10CA579
      97F34D2BD7643A325E01B651A9C7C79CEC57F26AB301CAEC1AC2E382CD113600
      9A001D802815A645577820C9DDCFC8EA9F23EBD49D1056B1A21F32F420C04091
      7A12E2EC46B30724034CFA0BC2C3C07E603390A4B21E1F07724DCDCE787B3DD2
      190CB9E4EB833C9A4E1207B4014D3A2562A578AA5DFD62BE19BD6675BFA6F5EA
      AF0ED368F4066767999A0C7E09659A0277016BAD102DD9A9A243A7B69B76B645
      8AFBA0C3235E55878F5E771835760DE63EC06BAD0F63519B9C928B3FC991A597
      C6AFA0AC8CCE10F53E1BD6BACEE96FFFE64BE16DD2C8B5423B7AC72715F25CC0
      AE532B4453496FA806CD1A64CC4CC588672D65CD3B9368C510BE9F84B86CCED2
      9D44BC0B3BA1483D80FBE60AC25F816DC052A0CCAEC4E3A3AC0CFF03A995D94F
      CBBDEAC50000000049454E4400000000}
  end
  object ScalesImageList: TTBXImageList
    Left = 48
    Top = 240
    PngDIB = {
      1900000089504E470D0A1A0A0000000D49484452000000100000019002030000
      00DA5A089E0000000C504C5445000000000000FF0000808080E6239CB1000000
      0174524E530040E6D866000001CE494441547801AD95516E833010446725F36F
      24B8CF46E20054C2876C4F919BA56F205553B5919C34963318330CB3DEDD56BA
      37A235293EDECF5768ED2C5D2E801E1AC5EC498A6D61550A500D09840C1BC06E
      E7DCE04548C30AA81A128150958A92076F1857F798CDCCC49F1285414229C50D
      7B4A60DB29EA1C362454621B79A318EA895536A414E243001B9D734ED911AFA5
      F0D780AA7CDEDF2C46E60F3D6E89943D90073B0556CF8C661609185640952C28
      8150E2D630ACACD43D6633132F1BA0E2DBEA550288FA81F7CCEAFAED64BF7BE8
      B152F599A69052028FE5D71F0D51850355D84620182A866A4843DB29E6F6FCF6
      3E1834A98D5A8464418F6AA6BA29693F45CFC7DB230667FFF8E18F8A430F09A6
      BB85DAE114D5E2481CE48EF94B6FB23F5A2E90CF17C4BBA017057FB84A3BED8E
      37D468792547475B6165434AFFB1F69D5AE9AAC7093E9D5A22E2CF41C11F09B5
      90FD5542C5731EA960D591848372930AC4988E97CBD3FEFE2A65CE94D2C31CF1
      532AA61C5FEFC42139C43A3A374008283A39298128ADA236AF503AE53424C55F
      2BFDA14A7FA816D117AE97E3CAE9CE2B947E3D3E7EF8DBE304BEF4E80F96F87B
      E4BFC84DBC4811E7645F5CF7FE7841BC0B7A27C74B3565D1FD783F010390453E
      098ED64A0000000049454E4400000000}
  end
  object EditCommentsImgs: TTBXImageList
    Left = 48
    Top = 208
    PngDIB = {
      0900000089504E470D0A1A0A0000000D49484452000000100000009008030000
      00369D1B8B000000F3504C5445FF00FF00000000008490A8D090A8C090A0C080
      A0C08098C08090B07088B07080B06080A06078A05070A05068A0FFFFFF204880
      F0F0F0C0B8C0909090908890808880B0B8B0204080C0C8C0707070808080A0A0
      A0505850B0B0B0103880606060605860C0C0C0FFF8FF505050504850D0D8D070
      7870F0F8FFF0F0FF103070403840303030606860E0E8F0A098A0E0E0F0F0F8F0
      D0D0D0706860A04800C0B8B0D0D8F0D0C0B0B05810B06020E0D8D0C050009040
      00C0D0F0C06020E0A880E09860E07020B05000A05010C0C8E0D07830E08850E0
      8840703000B0C0E0B05820B04800803800A0A8C0B0B8E0313163CECEFF6363CE
      4D1E5D7E0000000174524E530040E6D866000001E4494441547801C5538956C2
      30104C4111155904F10004C10350F142C55B04EFA3F0FF5FE3EC366DA91811C5
      67DE668F99CD6C5A8A523F5B965EFE69004A89D398E4E2BE00C05B9A464035A4
      064E88E1F0BF9B5CA4E769828FC2D74307076F8FB8861CCC53FFC704D7B0F863
      F0DF86C5A9387D2DC9C5FD252097E0D17A485FE09BCAEE637E0E881E9C5901A4
      6B86A601344EB91D88A83E3330629F710E26B4E31CE0AFBCCC308B0BED386393
      439B7F4BCD73306A0CFA1CF8B0DE668D51307A88E9695C1AD13C0DA498A14338
      C7193A003B7CEFDF10A06F2E8DE883BFCC42A1505041004C10634A004EBEBFFB
      8E08208A70AC230027DFDF7D4704809E18EB08C0C92F76383C361E99884E4E4D
      C766E2710885A96725008C11CD265373E9F959100B00C689169732D9546E1940
      1E40842853282CAD14D3442506268856D7E6D78BD90D2A57AAE888126D6EA14E
      6F972B350626A16115B3E9DC4EA5B6CBC014D1DEFE41AE503FACED1E31304D74
      DC38392D9F9D278F2E1888115D5E5DD79B37CDD6459B8119A2C6EDDD7DBDD97A
      683F3210A7D2FED373FDFCE5E5F5F14D80C4423E5F7517EE3194D976B0DDEE28
      1FB1B13AAAEB014C0EAAED6ED7D3E47EA56CBBA3159C5AE980C1364834F833D1
      6B61FB77406F175B0B486FA04627E6791359B3B740FDD1DE0120B5321FEB0CA9
      6C0000000049454E4400000000}
  end
  object MainPopupMenu: TTBXPopupMenu
    Images = MenusImageList
    OnPopup = MainPopupMenuPopup
    Left = 208
    Top = 168
    object NMarkEdit: TTBXItem
      ImageIndex = 31
      OnClick = NMarkEditClick
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100
      Hint = ''
    end
    object NMarkDel: TTBXItem
      ImageIndex = 30
      OnClick = NMarkDelClick
      Caption = #1059#1076#1072#1083#1080#1090#1100
      Hint = ''
    end
    object NMarkOper: TTBXItem
      ImageIndex = 10
      OnClick = NMarkOperClick
      Caption = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1086#1073#1083#1072#1089#1090#1100#1102
      Hint = ''
    end
    object NMarkNav: TTBXItem
      ImageIndex = 33
      OnClick = NMarkNavClick
      Caption = #1053#1072#1074#1080#1075#1072#1094#1080#1103' '#1085#1072' '#1084#1077#1090#1082#1091
      Hint = ''
    end
    object NMarkExport: TTBXItem
      ImageIndex = 25
      OnClick = NMarkExportClick
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1084#1077#1090#1082#1080
      Hint = ''
    end
    object NMarksCalcs: TTBXSubmenuItem
      ImageIndex = 9
      Caption = #1048#1079#1084#1077#1088#1077#1085#1080#1103
      Hint = ''
      object NMarksCalcsLen: TTBXItem
        OnClick = NMarksCalcsLenClick
        Caption = #1044#1083#1080#1085#1072
        Hint = ''
      end
      object NMarksCalcsPer: TTBXItem
        OnClick = NMarksCalcsPerClick
        Caption = #1055#1077#1088#1080#1084#1077#1090#1088
        Hint = ''
      end
      object NMarksCalcsSq: TTBXItem
        OnClick = NMarksCalcsSqClick
        Caption = #1055#1083#1086#1097#1072#1076#1100
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
      Caption = #1055#1086#1089#1090#1072#1074#1080#1090#1100' '#1084#1077#1090#1082#1091
      Hint = ''
    end
    object N47: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object N28: TTBXSubmenuItem
      Caption = #1062#1077#1085#1090#1088#1080#1088#1086#1074#1072#1090#1100' '#1089' '#1084#1072#1089#1096#1090#1072#1073#1086#1084
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
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074' '#1073#1091#1092#1077#1088' '#1086#1073#1084#1077#1085#1072
      Hint = ''
      object Google1: TTBXItem
        OnClick = Google1Click
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1074' google maps'
        Hint = ''
      end
      object YaLink: TTBXItem
        OnClick = YaLinkClick
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1074' '#1071#1085#1076#1077#1082#1089'.'#1050#1072#1088#1090#1099
        Hint = ''
      end
      object kosmosnimkiru1: TTBXItem
        OnClick = kosmosnimkiru1Click
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1074' kosmosnimki.ru'
        Hint = ''
      end
      object livecom1: TTBXItem
        OnClick = livecom1Click
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1085#1072' maps.live.com'
        Hint = ''
      end
      object ImageAtlas1: TTBXItem
        OnClick = ImageAtlas1Click
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1074' imageatlas.digitalglobe.com'
        Hint = ''
      end
      object N51: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object N13: TTBXItem
        OnClick = N13Click
        Caption = #1057#1089#1099#1083#1082#1091' '#1085#1072' '#1090#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
        Hint = ''
      end
      object TBCopyLinkLayer: TTBXSubmenuItem
        Caption = #1057#1089#1099#1083#1082#1091' '#1085#1072' '#1090#1072#1081#1083' '#1089#1083#1086#1103
        Hint = ''
      end
      object N30: TTBXItem
        OnClick = N30Click
        Caption = #1050#1086#1086#1088#1076#1080#1085#1072#1090#1099
        Hint = ''
      end
      object N20: TTBXItem
        OnClick = N20Click
        Caption = #1058#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
        Hint = ''
      end
      object N15: TTBXItem
        OnClick = N15Click
        Caption = #1054#1090#1085#1086#1089#1080#1090#1077#1083#1100#1085#1099#1081' '#1087#1091#1090#1100' '#1082' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1102
        Hint = ''
      end
    end
    object Nopendir: TTBXItem
      OnClick = NopendirClick
      Caption = #1055#1088#1086#1089#1084#1086#1090#1088' '#1090#1072#1081#1083#1072' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
      Hint = ''
    end
    object N25: TTBXItem
      ImageIndex = 34
      OnClick = N25Click
      Caption = #1054#1090#1082#1088#1099#1090#1100' '#1087#1072#1087#1082#1091' '#1089#1086#1076#1077#1088#1078#1072#1097#1091#1102' '#1090#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
      Hint = ''
    end
    object TBOpenDirLayer: TTBXSubmenuItem
      ImageIndex = 34
      Caption = #1054#1090#1082#1088#1099#1090#1100' '#1087#1072#1087#1082#1091' '#1089#1086#1076#1077#1088#1078#1072#1097#1091#1102' '#1090#1072#1081#1083' '#1089#1083#1086#1103
      Hint = ''
    end
    object N23: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object N26: TTBXSubmenuItem
      Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1086#1087#1077#1088#1072#1094#1080#1080
      Hint = ''
      object NGTOPO30: TTBXItem
        OnClick = NGTOPO30Click
        Caption = #1042#1099#1089#1086#1090#1072' '#1085#1072#1076' '#1091#1088#1086#1074#1085#1077#1084' '#1084#1086#1088#1103' GTOPO30 ('#1088#1072#1079#1088#1077#1096#1077#1085#1080#1077' ~1 '#1082#1084')'
        Hint = ''
      end
      object NSRTM3: TTBXItem
        OnClick = NSRTM3Click
        Caption = #1042#1099#1089#1086#1090#1072' '#1085#1072#1076' '#1091#1088#1086#1074#1085#1077#1084' '#1084#1086#1088#1103' SRTM3 ('#1088#1072#1079#1088#1077#1096#1077#1085#1080#1077' ~90 '#1084')'
        Hint = ''
      end
      object N49: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object DigitalGlobe1: TTBXItem
        OnClick = DigitalGlobe1Click
        Caption = 
          #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1102' '#1086' '#1076#1086#1089#1090#1091#1087#1085#1099#1093' '#1082#1072#1088#1090#1072#1093' '#1101#1090#1086#1075#1086' '#1084#1077#1089#1090#1072' '#1085#1072' DigitalGl' +
          'obe'
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
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1090#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099' (Ins+MLeft)'
      Hint = ''
    end
    object NDel: TTBXItem
      ImageIndex = 22
      OnClick = NDelClick
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1090#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099' (Del+MLeft)'
      Hint = ''
    end
    object ldm: TTBXSubmenuItem
      ImageIndex = 21
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1090#1072#1081#1083' '#1089#1083#1086#1103
      Hint = ''
    end
    object dlm: TTBXSubmenuItem
      ImageIndex = 22
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1090#1072#1081#1083' '#1089#1083#1086#1103
      Hint = ''
    end
    object N1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object NMapInfo: TTBXItem
      ImageIndex = 27
      OnClick = NMapInfoClick
      Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086' '#1082#1072#1088#1090#1077
      Hint = ''
    end
    object TBLayerInfo: TTBXSubmenuItem
      ImageIndex = 27
      Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086' '#1089#1083#1086#1077
      Hint = ''
    end
  end
end
