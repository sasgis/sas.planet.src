object Fmain: TFmain
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
    ExplicitTop = 60
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
      OnClose = TBMainToolBarClose
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
      OnClose = TBMainToolBarClose
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
        Hint = #1042#1099#1073#1088#1072#1090#1100' '#1090#1080#1087' '#1082#1072#1088#1090#1099
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
      OnClose = TBMainToolBarClose
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
      OnClose = TBMainToolBarClose
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
      DockPos = -6
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
          object NMainToolBarShow: TTBXItem
            AutoCheck = True
            Checked = True
            OnClick = NMainToolBarShowClick
            Caption = #1043#1083#1072#1074#1085#1072#1103
            Hint = ''
          end
          object NZoomToolBarShow: TTBXItem
            AutoCheck = True
            Checked = True
            OnClick = NZoomToolBarShowClick
            Caption = #1052#1072#1089#1096#1090#1072#1073
            Hint = ''
          end
          object NsrcToolBarShow: TTBXItem
            AutoCheck = True
            Checked = True
            OnClick = NsrcToolBarShowClick
            Caption = #1048#1089#1090#1086#1095#1085#1080#1082#1080
            Hint = ''
          end
          object NGPSToolBarShow: TTBXItem
            AutoCheck = True
            Checked = True
            OnClick = NGPSToolBarShowClick
            Caption = 'GPS'
            Hint = ''
          end
          object NMarksBarShow: TTBXItem
            AutoCheck = True
            Checked = True
            OnClick = NMarksBarShowClick
            Caption = #1052#1077#1090#1082#1080
            Hint = ''
          end
          object NToolBarSearch: TTBXItem
            AutoCheck = True
            OnClick = NSensorsBarClick
            Caption = #1055#1086#1080#1089#1082
            Hint = ''
          end
          object NSensorsBar: TTBXItem
            AutoCheck = True
            OnClick = NSensorsBarClick
            Caption = #1044#1072#1090#1095#1080#1082#1080
            Hint = ''
          end
          object NSensors: TTBXSubmenuItem
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
          Caption = #1057#1087#1088#1072#1074#1082#1072
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
          Caption = #1041#1072#1075#1090#1088#1077#1082#1077#1088' http://sasgis.ru/mantis/'
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
      OnVisibleChanged = TBXSensorsBarVisibleChanged
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
      OnClose = TBMainToolBarClose
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
            DesignSize = (
              150
              32)
            Caption = #1054#1076#1086#1084#1077#1090#1088
            object SpeedButton2: TSpeedButton
              Tag = 3
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
            object TBXSensorOdometr: TTBXLabel
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
            object TBXLabel11: TTBXLabel
              Left = 0
              Top = 0
              Width = 129
              Height = 13
              Anchors = [akLeft, akTop, akRight]
              Wrapping = twEndEllipsis
              Caption = #1054#1076#1086#1084#1077#1090#1088', '#1082#1084':'
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
            OnVisibleChanged = TBXSensorsBarVisibleChanged
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
  object tmrMapUpdate: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrMapUpdateTimer
    Left = 48
    Top = 72
  end
  object PanelsImageList: TTBXImageList
    Height = 24
    Width = 24
    Left = 48
    Top = 176
    PngDIB = {
      1400000089504E470D0A1A0A0000000D4948445200000018000001E008060000
      00BFC1AD3800004D91494441547801EDBD0798144517367ABA27E7D99C736609
      4B862547C94982040591280A0882087E82828082222A2A220A8A8A8A82EC8222
      0614942C415092E4CCC2E634A9EF7B7A9961979D5D40FDEF73FFFB384FBD5DD5
      15CEA93A15BBD2087417BFE693D7A9E12D9A8842005697F038BDEDE55E36E8D5
      2AA13A57104E84FB6CA03FE04D7D06CBFF81D131E85E95E8D51696CD26AE7911
      DA51A07F648091C676AD410B4736A5493D93A8559D30F2376BE144CCF82822C2
      91E0F74AF0CA00013E1514CAA911FE3A9A35B8364DE99F46EDD382492B15928F
      41454D52426944D73AF4FC903A1415A067A2CF701836DC8E4A0C5A4C5A3B0F9E
      FA37AF11444F3ED098EAC70792D9A0A79C12D8EA03C8E88377AD40013A0769C9
      468B4735A276B503E148FDC1A4524A2A308087444910A6C5879869DEF0265427
      5C4FD7AE5EA192924232984D64326849542848A3142825C2424DD312C9CFC742
      8F768AA5287F59649C12CE37662843293F6F3DE4183C3FB41191E4A4BFCE5EA2
      22C14456B32F391DF02408044541561DED3B7E09CCB22832D04406B582C6758A
      A4A9ABE4BC661A03E05B569E1420F66AD8F48F0F315184AF9A0E9F384FD96425
      FFA000588B2449440EA74436BB8B6C361B458707934D52D2A67D97E8AF4B7914
      625652B88F0A7E6551312D3653F91444137EB5A2CCB4EAC71364B5582830480F
      F1B848210A6477BA28BFC84646044D0C67A684D89BA96D5A386DDF7F8C2E5F2F
      A230B38BCE678308513411C9C929CF200496141B6A2517F8DE2811C9999547C5
      060D19B5482892C00C22C374EC0D2992202E815C4E3B39C1FC525601E9042E09
      722A9856250672C0BF2E17512D64AE1579E66B51D2B18BB97445A12435781494
      D8C9DF401460D6C8C425978B72720B28D8A2A24B178A68DF5F39448AB2D4C9C4
      F0100059210F1261389A12E94B01BE469AD82D9102AC063A7D3997361DBC4EC5
      768944A4CDE17452B0CE4E45A50EEA9D1E4325C50554989F47FB0F1FA3A55BB2
      C9A6F105194A72D7EEF2223A4DF85DB8964F21016699385E293AD842FD503C7F
      3878910E9CCA23A74BA223678A29A7B09492420D146E96283F2F873EFBF93439
      95611C84719A6EFE3C29E077A4E253E8FD53A2FDE989AE71C4BAC349A4542AE9
      8B1FF6D337870A488D3A505C6AA7ABA879BDEB99A951A48AAE6565D1A275C728
      47174DF87D86D87B8AE9ED0C643119344A0AF2D5D2FF7AC7933FC4244A0E9AF3
      D9413A97A72201F5A3D8E6A0ACBC521AD8404FF5C304FA61F731DA7CCE8FEC0A
      2DE8DF120FBF5460C01648055794674C3A150D6E62A58EB57CE983EF8FD33747
      5DA4D5A8C88624D96C76CACB2FA6C611445D5288DED9E1A46C978583CF41ECFF
      C706372A31600730F9147AFF209392BAA7B868F9CF59542A9A4925D8C9E57090
      CB5642F6E222F24389525883A9C0A98177AA201AB660A0F0B156118805CB70CE
      957C072DDB6EA3709D9616F44DA6C79A8651E76089824B4FA2D1F3A16253949B
      38C79CC354248437AF0C604F60C2494D42B3FD994267A5E4E4184A4A6A4C6161
      B5A9B4C44636979CF8CFE0978B24FB85B1B25254B6221A3972E4D4B4B4B456D6
      C23F6A7448497BA0639304A54BA5501E3F5F48672E5FA326F56B4869FE1AC7A5
      DDABF6D285AD21D1D1D10D4F9D3AB50FB4B84984764B794D41C3860D5F183366
      CC0BE3C68D7B212E4E17181315A1F1D5FB51BD84207AB06763BAAF5553C1CF97
      544F3F3DE9E139F875ECD8711611B5262FBFF215CDE3BC61C306DAB16387FCAE
      565BA8A8289214682E640B3C0441A06DDBB6534ECE69E2DFC993275933F2E3AE
      E0EBEBBBD06C36AF35994CEB1E7964DAA5193356E6CF98F15EFED34F2FCF9F36
      6D59FE534F2DCD7FECB199F956ABCF66954AF539F031087700EE5AA9E09363C4
      8887B91DD0C10BEAC1CE0CA0C0920AFA7FEADE25A0B83DC8975F7EA9EEDFBF7F
      E28001035AF4E9D3A777BF7EFD460D1E3CF8E9071F7C704E972E5DBAD5AD5BB7
      E8D2A54B27B2B2B29CB787F5F6AEF8F1C71F5B81D023F88D1D3D7AF4B346A3F1
      257F7FFFF101010103828282DA59ADD63A7ABD3ECC809F9F9F5F14ECFBD4AC59
      B35FAD5AB5248BC572E4D8B163366F84DD768A0913266CA953A74E5710AB05C2
      8160A052ABD528EA82E07038041E41B06758C81ADC443009080C0CEC0C0CAD51
      A386312222E2C8E1C3870BD9C3ED109DF821720A8D46435AAD965CE86719204E
      0C0EC0C4DDE077B66746313131618D1A359AD5B469D323A8F5AFF5EDDB978BB4
      DC48B13F8678F9F2E52B300812460D4C58A7D3115241AC734FA6C0484E144578
      21D99EEDD81FFB67467013C2C2C2AC2D5BB67C0C8C0E41CC1FF5ECD9B30102C8
      811490FF20D4C418A4422829292198E9EAD5AB9497974776BB1DFECA14132C2D
      2D95BB4F4E69414181EC0E01C8230C6684FC52C6C7C7D784FE085A82903FFEF8
      E37B11812E22B68EA3478FCA639D9C9C1C427EC884582C65E4CB9ECC84095FBF
      7E9D40404E11FB6180811C061211CE9F3F2F161616B642A83845EBD6ADB3D0E6
      3C840C53A0F8C98172737309192EE7873B9399388B8453C5E014728A9172BA71
      E306FDF5D75FB46BD72E3A77EE1CE5E7E70B90C259FCBE548E1F3FFE97993367
      CE183468D08BE1E1E122DA75427124C4849054E2CCBF76ED9ACC8C19701EB088
      D8FDC2850BC42D29DB3338250CC45C424A8BA1DF50E2E17AEEB9E75E03A16894
      8247434242843367CE105A549909673818A369CEA12B57AEC8044F9FC6180803
      3026CAA2B94994206E902B5348C55F30E53303E8647BF1C517A7A1C444A20474
      6326481EA12211CB9B3D6CDEBC592EB66EA25C92D89E45C876082BFB87EC09B1
      9720BEEB7077B91910E45EF0D1471F8D404A36A287AA075108680E08F9230FD7
      CBC7944B0EC79689720AD98C18CB2900618CC84B2444EC121854EA42A943870E
      096FBEF9E6A9CCCC4CD7B66DDBA44F3FFD54FAE5975FA4975F7E597AFEF9E7A5
      B973E74A2FBCF082347BF66C69F2E4C9D2638F3D268D183142425BE5429177B5
      6CD9F2146AF65A10EF0854A874782F53DDBB776FFACE3BEF64A154B87EFAE927
      69E3C68DD2E2C58BA5D75E7B4D66327DFA7469D2A449126AAF346CD830265CD0
      A04183BD281CAF83C25020195003552AA1478F1EF7BFFFFEFB85CCE0DB6FBF95
      162C5820CD9A354B9A366D9A3471E244D7F0E1C39D5DBB76FD2B2929291395F3
      29506A030400955A68D879558AFBEFBF7FE2BBEFBE6B67313171C49A09E7376F
      DE7C0F1ABCC5083504480274C0DF52AA6EDDBA3DF3F8E38F1F471FF1079AE8AF
      10DB29A0D49A88FC81BB8E2DFC56A93470A903B405E2012DF09FFAF724502993
      D0E96B91A1F5D0E9F746C5190BF32454A21943870E9D8D4E7F30CABB068DDC01
      D472C7DD44C35BA7FF229AEAD128865DD02FA4A13D8A46EBE98B3E438B063000
      95A9033AFDBE2851FF75FA3AB90BE416935B529637B79C6865E5CEC7DDC3C1ED
      BF4EBF6C64E816891D4319C67F9DBEF05FA78F7AD3111080CAEAFFD79DBE84D1
      84844E7F2F5ADB7FB7D3C720CB3E7FFE3C09932412DAA3A9106C0050A93F815D
      05255678ABFC62C7086F81465367798F1E0F2BA3A29A519F3E93A877EFE14DE0
      B5187002D5AA3B31E0C0A521217137CACF171517DB79E2F5AE46175E93E89E2F
      AA5DBB768B471E99BFA65EBDD85E010146656EAE40172F5EA1162DEA8776EC38
      E00985C21E1B19195633FA9FCE17454484694B4B75189EABA963C734C27C9268
      B54AEAFFE68B78AE88C1C3C576C8ED0E5EF07FD17CD10B73E72A3FFAF893461B
      BFDE346CC5071F365AB0F065FFA79E9AA640AAAA5577F430E399678461C31E8E
      494DADF9645244C4DCC0FCBC813E21A17D032222BB07060537E8D4B98BDF7D9D
      3A09E9E9E9A5A8E5B6EDDBB74BE53956CBE0CD37DFF2C14CCC90DA75D25E090B
      0FEF2DE6E5595C070F8AE4E3ABD7060585988CA6BA565F9F6E168BB5AFAF9F5F
      D7C080A0B4FBBA74F169D7AEBD0486250D1B35B47BADC90B162ED4ACFEF4D30E
      4DD3D33FAE5FBFC1620C176B11090AC93F905C1DEF23312282248713CB2B4EB4
      F78252ADD10484D86C8D6BE5E68C4D8A887AAF66ED3ADF2426A72C3299CC35E0
      E15682F01529864744A4C4C6C68E8B0A0BEFA77738FC449349B061CE34BFB004
      AB1E767CD262CD26FB06F9FEFC1DE560163E2BBD39D9F0512E60D2467D234BB2
      C727DA4AC9B531EBEA95D9E7CF9D3DA874937FE38D25C11191110FC6C4C48CC2
      1442AC78E6AC68FF690B15854792BD6973CA2B2AA182A25230C1BC756E11E516
      D9A980442A28E685228924834572A874D97967CEBF525498F3D673CFCEB8C1B4
      15B367CF36613AAD6B4A4A8D5799814EA7F717045174285594ABD250A15F0015
      9240C598CA716192C4515C4225A248B9219154101C4A258286F29D229D29D1D0
      516554F1653132E79A9868486E3CA8C0DFD0E406CFB65820E3648D461B82F519
      3DDA7A93C3458A02AC72949A2C3C0B85053A2739B1E418BC612D2976EDA0ECD8
      047280090FC08A5C2A3A9CAFA253BA5841088ED5EB82426A69F4DA187B56F6D6
      827357CE78F260C4A38BF51171517D6BD40E9B47CEA250952891D964229D5623
      4F11D80A8BC9B96F2F1543EE4535D2A8186B6717AEE5D0619B0FE504D62295C5
      9F30D3E7146F64FF643B76624AC98953FBBE5B355092193C3A6D6D506A9DF0D1
      8141EA31A2520AD6E90D82427051497E2E15E66593282A6E020B5D8212722FA5
      2358FE3A5E62A47C9595A480447292BA44BC767D359D383EF387456DCEB2FC19
      CAC7666C48AF532F784270A8BE871AD155A83442715101190D0612ECA5A452FA
      5356412915481A3A7FEA2F52690D74E17A019D2F12488975CE9CB3A725DB95C2
      1C4581F3554389B478DB9BED7399B01B8ADA0DFB1710A9FE9004ED69B5CEA4D4
      9B2D269DC1A0CDC9BA4C58E913448D81789DA6182B4C374A89AEBAF4C87C1FBA
      7EED32655DBE4C3685D5599895F745E995EC770F1DDE7D43B0367648D77E75D3
      27E5BBAFF4CAC6DB5EC6B0096BDF0E8F0948B1F899DAFB07691E34F9E8925CA2
      52C8CA2FA2EB2544F9829A2E5CFE9372ECA813FE662A283C87A2BB45516CEDDC
      B7D8E4D75C13DCF488A2B064B750EB835D8EECDCDF8BCF9DBFE0A90760402B16
      F7CE4F1AB1F17048C2954493F90FF1D2E56DA4BD144C31FE2D486309A6EB2E2D
      E5FAF8508984627A7537D62F575268582FA1C066D563EA315654A963D4264327
      B1C87849CAB9F8822277EF4AA62BC3D0E163A1C6B84D29F7BF776879E7EF5EC8
      0BFE34D5A57ADD22D559D6439A9639521AFE4977A9FD9231529D45AF4A61D347
      4B9A470224F131A314F2425FC9327DA3A47C6283A49FF48DCB3476ED797D9BA7
      A66AFC1382F0758A2201F2FEFDBF34862706DC1F971A342520C09C22D000D181
      D271CAEF3B6AE43B82D49A52322A2F50827937E5FFB98472AF5F2587C6410AB3
      1A85C14A45D90E924A5D2E47DEF53F1D477E7E51389AB1BE34FF8A9CD90AFF81
      EBEA46A605FF2F2CD97FAA46A30A935C922862A62D401D42897E1A94220765E7
      5BA9203B99FCD4B114A43B4524E651B613F5C242A457D5A2FC4BD10EFBA58BDB
      84DFBF9E211E5FFF4D69E10D141CC41C4AF0EDBE34DE2F42D7CA1212DC1555BA
      81C96C0CD6AA954AAD2A4F28D47D40974A2F52F6B56C4AD07421BD549F5A85FF
      4937723FA4EF50B32FD89A49F967DA945CFEFDE27AE1C8D72F2AAEEC3C545A82
      456710762BE1A641D42774375913EAC6FB4425B4B484847434FB98D3F406F2B7
      E84445AE2253D89DBD9AE29563A9955F32B58A594BDF9E534A6B7E48CF3DBDF7
      8F157424F37545EE91D3981074DDA457B5A63106284CA9FD7C23BA3D975E63D4
      8AE9CD9ECDFCB1DB3BEF5D6BFAC9686793C51F4A75677D2BCDCFF8D035E4CD59
      E7CC2D263EA9F68D9733B36A8AD5B868F416A5A9E6C080D89E2FB4AE33F6BDD9
      21A356FF6A7C78CD75FF21EF1FF469327490C6E867AA26F8BD39A9CD612A55CA
      E01055FA33ED95A9431A6B74E6BB1A9BDE1B9732DF9C6F8CB2B73B3C858CCC75
      0F12096FA06D37DFC1EF3D3963F2300FDB4B1E435321BCD1AD6B8F7F95F8CD98
      983337AC7F43E98E799BF9B75AC09B1E3C9A8FCA41CB4737201FA35EB6CB41E7
      F3C01B7BE84669D5A39E5DCFA6F30A89192990C3507651997EFB53AF16E88D91
      753CC4D9DD6AD0D1478FD6A31EAFFF4E8525D5177D910354878F462452A81FDA
      84DB3CF9990CF4E9C8E4DB6C2BBFDE91C1373B8F550E75D3E69B5D47C9A4ADBE
      4055C9402089927DEC7431BB84BEDD7DF426C95BDAD6DF4F62AC544A697E25E4
      539635B71CCB99BC321005896AF8D929C1D745E1BE3A2A2CB5D3898B599E6057
      730A30AD264174668AF353521DDF520AD04B1EF7F2860A0C3843BF189348C36A
      4B146F256C64D251B0AF91427C4D240A18C1615F0B07D663034798BF85C2B007
      26145D67AC1F3679A469E89B89352B89CCC38089AF1D5793E242FD69FCFDCDA9
      5152904C3C10C4837C8C14EC0326D8C8C40C8C3A8D1CFB503F3385814183A430
      1AD6A901FC9B2973425A05261E065F3D5E8B8240840930BA3649A194A8209950
      3008E9B56AB6F680DF43918AE4C8404AAF19E3B1B7A0086F9858D7F3EEA90701
      16A3C7D26D48B8B9D5CAFD7EBBAE53AB8871BBBD59AFF5587952E0B1F9970D9E
      1494DA1DA451795E653605C5A5A443862A30D0952D6E7BF0DEAE82925262B194
      772A2AB5915E5326520FC52E2FEFA4AF9F6C4CE88E65BF7B8F9D9737878540FE
      0CA5A26262EDF8C2B9743D8F18BC12D82825520EC7C4EF7B792F6D9DDE547E17
      BE5CBB46EADDEB7E4A7BF657F24359FE6A7C03CAD8FE07E5E49750083A2C0697
      961064A89B890DA965C2976EE4830190954746BD86BA3649A65E4B0E527E8944
      FB9F4FA7B5EBBEA00A0C649678041B9C54CBCF41D118B6B819244506908FA9AC
      CAE61614D31F67AED0A5AC7CBA88549CBA6EA343D75574A5E8562ADD0C3C2202
      5D8FBA5CA8C0DA322A96540A1DFB8950D9DCC4D993C5A8932BDEF9AC5C3A79DD
      4EBFDF50D3758CB6D9ED76DC62799B0BC766FF75B59CC9E5CBB9DB5BE31A51C8
      5C2D1DB8AEA99238FBAD92013B72ACFAB6AAC546AFE8D63485B279DECBAB6B99
      A557119539953DFBBCF11B7D3BA529B6FF548C4B51894DCED0325F553FEFC880
      53D165C1AFF4F553CD64B933292EA27D5FDB49A50E058A35DB540D7C542AD1FB
      939973BD6A6F155D544A056D9CDAA2A2A5973710CF1756AC7C6F9C8F8FCF7C2C
      42576E8CBC04BA5B2B2CBD145EB972E959C15B8070EAE6B1B62DCA688E976CF5
      13DD0F43BF27759E32311770E7203C665A0C46BD817B5ED6AD5834BC333B0EEB
      04600511BD0D26F580AA0744F0545EDD0D838B08701C1439254360FE126807DC
      95BA2303C8BE1094763B31D054E0E309E650A0F24009964859A53CBD23038463
      B5038F5280950D8F2C8040500464A225AF6424C2AE17DE63A17B94ECE879BB69
      085C24C5C37801B1971B0204E23CF81E2988404A987817B8E7030D0123B001E8
      000C007E035E45D8CBD595A274789A03C241D009A3958BD08F01AC6C7834031E
      0126030B8145C050A03DF000C0254E05BDCA627A188E8380E5605203A3151E1A
      6720F65CEB614D03F1785441541BD0C3DC15803365438F0046014940950CFE82
      E371A0333005294881EE003E052E00D13709CB22865903BB14E004C076DC04B7
      85D93B0310CC85E3F3C016224A420A3A43EF05A881DD800AB85D05DEB43807DD
      069801EF0CB493BA4BC8A4EFE06100F00AE00F3407BA03C98016A8A014F80E83
      85117818E803AC04B0079D9F55004CB2E0B406F9C0B21D0DB301E0E1C3292751
      CA4DA2B0F228334C6711EE04745929E5E79D1F0721B67510553D784D027600C7
      C1A426F4083052C15C04F3F7C035C0A3AA658098B3AC63E0BB2E88FF02BD0DA0
      0142C1702B747FD89F7596D5F0F7F0BE0EB1CF83EE51A2DB84AD37338124F73B
      88A7C2FC38900C6C21A20F80A540091000C2462010E6A68009B000568413A17B
      54F997594474A41C9352BC9F07BE41ACAE0076983F05D603A79082D344C4F545
      A320AA0BF3CBC046603A98F8429795527E567C1C3937AB4172C413DD8FC2DA93
      5930B3E24A1509C32AE05D600B118542440DC14405732ACC53A16F21A26D80F7
      620A0766E21117DEDD2A0F86406012C4138F541D84F951603F084BD05929F1D0
      01B22A2F22D9A2DCA3121388A510EED94030D0102030D90B7D2C70E82613A6A9
      C5BBACF8453654F1A8C004B1CE85BFF78142A00564CDB165263BF13E16F81360
      9A3AE8B2E217D950CDE301B71B628B48D272BCCF039200AEE1D064F52B9E2CAE
      E3D03D0C04BC5452E54715951C6181983381C720B29FD1ACEC8495AC60CFF45A
      13910691F986FB0398FF9E8A7AE7B3C0A8B757D7BA3D34EC1580D16D2F54175B
      C4A8393CFEAD3111C2C9EA4E79C08DD76230EA0DE8E410F7F8B81303CEB004D0
      5C417F634C843055563476635CC4E36F8F8910B67A0628095CDE773BFFE698E8
      8E0CD803B003280558715798251B1665781D13A11D932B1FFB615478610B6466
      3C74CF9808E63F806B4004500C14C04F32F4868011E60D751D1707D8057174BA
      E38CE19A60B082099CE8473C32BD65723A1CEE6A4C84CDDA2F37729CFD4547F6
      E7FF1483C236A992AC1B5529E428937C1BD079D91B83C37018042C47ECAA1E13
      49521DCC21E90E2842C3F1D14B6AAC8D9D132CD253DA2E07D28DE31A207C3D60
      5CA58A06A25638AC079A011FA0395880468E635307EF694014DAFE40E81EE592
      A4AB384B789A881A022E60120AC86BD0CBD2C2063740905BCCE7F1BE85BC8C89
      74929D2B1F9C6E29100FB8F9760EBA0DF0F811F15241A1F1AA724C94E0BCD61D
      9EB54005A5A8624CC49E2A89882DCB03226B87F7CF9592D312E1CA15216B3A2A
      FA93027378B0F72827D179BCB481684E40F728A5C754B5E16033FBC92B39A2DE
      E72F264CCE5D58D2BA04823511A4DA3111DCAB1ED921E62AB5E4889D55BC6999
      452A4D5EA46D4925A480309456E4D32604BEE398087E2A67325B8278AA56B24F
      7AB938E38BBEF6DF5BF4B71F2085E47C1931E78A76D76322A6E5350FE6BE306A
      B0894ADFEF6AFF53A522D779786C106499A9873E0F48400A56DE2CBA3D9126AC
      C4A26E959DC35C0DF737910F37A0CB4A896AFD284CC78053403D6004D97FEB08
      9D55261E032266ED29A245940A7324B00A7817D842771813C15DCE83256CF082
      DE20BCAE9C7D1ECC5CC1784CF43D627910A2E4C82D4386A7212502DC9540858E
      89EBC178582E037603EF03AD89487B1B71FE4E2B847D36100C340478B8B217FA
      58C0EB9808F6E4350FD8E17620B688248D86FD5C80533602A970C0CC9FB3CDA0
      BF0370EF3704F69FC12C2B4E816CB8D3038120897B1B1331CDBB66C09EC1A414
      FA6BC097284951D065057B09869F81C7802B8047891ED35D1A40AC5867F87CA5
      41FF691107710FAE60CF6DD88F70DBAC78E72123BBFDBF02CE38AF8CB66FDF1E
      F0CC33CFF49B3163C61C1C647869C890212F3CF0C003CF3DF4D043B31E1D376E
      C69353A74E1E3F61C2909E03FAA7A477EE9C13725F87ABBFADE76FF58AE4B8DC
      56B0F9E0830F94A9A9A9AD0302029EBE74E952FAA64D9B3438AA41172E5C104A
      B0FD046D3FA9743A554058A8A57EC386E6D6E9CD5212C342FBA8148AF7FCDE5C
      F2C6FC47C7559D071B366C504745450DC764F78A8F3EFAA8CDA38F3EAA5DB162
      8580FDBD424C4C0CD5AB578FEA376840F1494964C78CFCBA6DDB84694BDF16D7
      6CDB168249F5291116EBABE3962C892A1F634F26AF5EBD1A4DBCF0204EA4CC7D
      EBADB742972E5D4A38B145D838497163C690A56F5F5277E94206300A484CA414
      306BDAB40999434368EDA1DF85CFF6EF5317391D7D4295CA39C3DE591AE866E2
      610062CD8A8A8A9EFDE28B2F7C376FDE2C24242408292929D872E2F122871135
      65C7B74BD56A7260C93C263E9E62E16FB7AD54D8959BAB526AD47DE20571E8F0
      39B365F1CBA171C6C38A033B530E1F3E1C0EE2F241AAA0D05072289524620B8A
      4CF9E6C309C292D1481AB39994B81CA054AF27FFF008328685D10E49126E88A2
      CEA8D10C0F315BB856730F423470E0C0CE588F9FB87EFD7A0D0E4E09C1C1C1A4
      C6317A0313D16A4971F62C19B02B4A57904F2A307569D454A252CA7E5C461365
      2BB0B906E7D8AE2B9438208D35389DDE8A985F38D7B4F17601E76D8C380EF026
      4E630D86EC116103F109ADB0C4445C5F602611044D108B1D2588093BB125089B
      1EE5F4D82D66FAAD755BD9CC8F82BD7B4955584863745A496FB3FFB0EECCE9C1
      220E14A6620AB83E0E58C9E7FF705409FBB86CA469D1821C75EA902D35157B8A
      6CE4C4893B1C2526ECB827FB6D1D3E1367E890DA02A486C73D46BD2E3694280E
      772B281A621A3808651E61914404E61DFB1CC00DBBD329BB4584859211629320
      0AB75B799D73D589F045888C5EA3355B14CA08D0162390025D7171B1A7C4C0B2
      7C3872C14D89803E2A35F96B741402187CFC71FB8395EAEDDE47B5F71DA2E83D
      0728BCC84131D8EFA5C7BE28AD5AAD10ED76BDF0C9279F2CC469BAB1AFBFFEBA
      EEC08103026AB0BCAC827CC19A8182CCC86C74F6A44169D1A2F49460358A9B54
      07442121A3736D36B29340F9362CC7605B561E8EFC8D6CDB56AA171B777DEDB7
      DF3ECA993C05672B9FFEF0C30FAD5F6FDA2444F4EBE789BDE1E851DC47E1432A
      2499C524AA54E414457220357A30BB865D6B76B865E340BA5D7261AF511129D0
      8E3F3F66ACE46F36FFF5EEEA4F060A73E6CCE9D8A64D9B25BB77EF8E7B63F972
      21A853270F03DF93278950EEE5B61744F988800BBA8021453116E338E30B4B8A
      292B279794607EE9DA35AA8B423167DA34D7A9532737BDFCCEDB0F0A38FB1A34
      7CF8F0B7519A7ACC9A3F5FB4D5ACE96160440A90BB84037EF2714A058A2CD619
      E45226C117DA2CBA0191385D883D8A6709C43571DC38A957F7EEF62FD6AE9DFE
      FC4BF35F1540589C376FDE30A4E2F5F59999BA0F3EFF5C3020F94614B9429CBD
      D4226606D403CE7817CC2C9EDC8000902F538A63C7C885F39C382A29B75B38C0
      E8BA76F5EA9177DE5DD6EFD34F56FFC1430D423B1F0E7C880C6E89D374C2D6AD
      5B05AE6C7CA88463CCA2E1A2AB40257314159104319491272A3974887270BA31
      3E2E8E263E31518A8D892D5EB366CD73AB56AD5A74FAF469BBCC00077A443417
      3D3A77EEBC046D52C8C71F7F4CE8702001413E13888650A627A13960718868AE
      650B3CAEFFFA2B254546123A24098DA313915BF7D9679F8DC721F64B70BEA592
      9292744F639F7346464616DA24D7D4A9535D8D1B379670BE4FC2D957293A3A5A
      8A898D95226262A4D08808291C7A7DB8A3B79340D0F5C30F3FD8162E5CB8A975
      EBD635217639E24CDDD365E214A20387390FC132177B1F6B2426265AD0B35178
      78B8C0C754B90941400AF4F7A764A4A0797A3A756CDF9EF7C2F30E9002A4782D
      B6B13F8398FF012970190029420D91B55B0F9C77D2366BD6AC250E558D02F196
      3803E5EB72B9443E7B091DFB3845142C25368289123A273E6D7DF8E0C183AB76
      EEDCB906A752B9BBF410BF45B5B24904F1401C2AEC8AE37A2FE1E4E9A6975E7A
      E9000E1E1EC101D14378FF79FCF8F1CB70667018449BA052A10DA94CE3AE6C58
      9628508628F411F5911FCDC2C2C21A63C32B77265650F08818E6FF0F2AECDE17
      8126C0BB4026F03C8066FEEE232BDEC16B10DC8722A343309C2985AC5BE0BD47
      EDDAB555D0EF4ADD894112A844637C741675C384937467F09E0E1880BB527762
      A00515170E7FD682AE4311BE005D0FB03DB43BAB3B31600A023FCA4103734D88
      291E50C35CADBA13031787E6C68EF59BE0A6740ACC0B8127C0A435900870CA60
      5551DD89017F2289A8C19EF28EBA70096D8ED4AA55ABD344D41D780678051807
      265C2860BCA5EEC4C0EDD323A6989898931D3A74D0A3318C461B7571D4A851BF
      DF77DF7D3C52690FCF5DC04405DDA32A31600F801E60A2E5DB1509CD76111ABC
      3C0E8DA64485E6E2105AD306E87639E6C5B01F004C4058237459556000876822
      1A0B4C03FA00BE805B29F03112FCBFFFFDCF07162C327F74B7F7C1AC422BBCFF
      89279E28C5517C1E70704A1AC25E561E0620CE6DCB44D8B28758E82381810017
      494E890607D29BA2976B0E3B0DC04C581723232393870D1B160E715D83BD0DF0
      013D964085C990143824228639181F11927E08FDB05C0C313073C0AD2A25C221
      18100E1D3AC49161C697D18473A4E4A904B8C9CA8EA78443B55C3423111B2B5A
      D0EF162D5AE48F5D0BB5E0160654A7B271DCFB143C3093B3D065A5949F658F3F
      A1ED83A7FAE8E0CFE3B8920A07D13B021CFBF279016F5E552EBEE312E07202B8
      08C84A96936CC203720B82360A68862E73F7E79F7F6E86B911A002AA55B81A24
      03A7DF9BC2D39B10CF7AE8B2AAC0806DC0C40F7A3FA0178E6A9CC05727676E6D
      BCB36CA1795597C78C197312FD7234114D3A78F020A70246AADC27B32D98B01C
      7BC3FC20863427704E3F04E618A02A751CA75D72E15808CC048302E8B212E4A7
      97079870FE0C84D36064780E4A9509661F400D9457765C16B00195AE292C5783
      F887D03DAA4A06EC034C98E03874342D3146DABC64C912EE7042D9AD1C6C1804
      7C83A3DF9C574F83C1EFE5DCBC8BA8BC0730F1C5FB2BF886B0E10B3412667FA0
      BC72E00682BDDF7FFFBD1F2C1F03832CE8F7A6C064118E217D810B3366A2C64E
      2F0F0C1967E000DC4AF8590D70642A10AF56446E9F08B810E6564075FE8FC07D
      3C527003FAFF456A72CF78E5B27175E3AB8AB25895C3DDDAB76B5FB76BE7213D
      7EF962565B1661A5607F9B41A77A41E2578BEEEFD9A0518D0F7CB54260CBCECD
      D7FDF0469FA14BC7A6556856FE160310574C7CA4F588761D1BAECBBD72D5BCFB
      D7832416E55B6BD54B5991543371727926F7CCE026F1479AB7AEBB542C2DC6F7
      82199FBA462AC6F75CC9958B14131F332F26216E905B56F7CC0031F71077E0AB
      9211E06BA06C6CAABC71238FAE9D3C4AC19171AFBE3FB1491366724F0C164F68
      59AB59AB7A4B85D22272E238BD0333304E1CBEB295F0912605E5E6DB28FB462E
      9DFAF3F7D2F7BF3D5E212F9859958058E40CBDBEEFC5ACDD2B074B79DBA74937
      7E9A245DD934563AFBE550E9D0FBF74B3FBED45EFA76764BE9F3679A5FEED134
      6C0888718B7CE7B608C439436F8AA5880EEE3B2ACF624586F9900D87DFB2B272
      E8E8F18BB82D544717F26C6717AFF9E3C9DF4EE66680410950A1D3E7F70A0071
      F196CC8B88E51D171D4421B8EE323F271F97151652415E1159F46A3A9FEB21BE
      114464E2D0AB57998BFBF62C383C5F2AFAED59296FC74DB17C33463AB77698B4
      73493769DBA2FBA4AF9E49973E9EDEFC4CBD584B3F50330077A7168F4FAF75FD
      B7D959E5895FFDF65199F81F2BFB49BB97749532673667995FA98E78A5D61162
      F1C8FCDAD97384ADCBB8DF544B8ED21259E667CF5E815C25DC33954B76A5FAEC
      F32BF68D387826EF4744DB015452158A6979E2228AA2AFC5406AA5285722CED0
      FCBC4212313F74E1E20D99F8DC0FF73F09E25B41D52B71D8DF1A788178A50CE5
      4C55E2BACB7C9CF9B88E8A84138174F55A2E1660746799384ACB3FCBD0AB28E7
      9CA12C73CED00D90F99A99ADEE2943959DEB072BC70C69D6B579ABB40FFEFA9D
      A7D00CD8A9AC84CC4BC95684D92C94F3621C5BCDCB29A422275D99BD0A323F9D
      C762B9BBA20839197FFA60F8B785288AC7D68E94F6AE7840BA72B3281E460DDD
      3CB78DF4F5ACE652B9982B11E6AE95ECB9D543EFFD6FD7C7C343E2424C35F54A
      890E1F3E4D21FE26CA82DC054C9765DBE8ECC2D5FBB986B2CCABCCD0EAB81AD2
      5303BBFDB966F83596F9FE65BDA5CF9F6E22FD30B7F5DFAB445570F26D532768
      E0DEF7FB5FDBFD4657345CADA40FA734AEB6125541A75A6BDF26C97EDDD63ED7
      E6F7B5335B566815AB0D758F8EDC9E344E0937754138B9C985FEAF2B1D2832A0
      FD3325D052D47D378DD142A5B6C9ED744F7A399A1519B8A9B819B93DDEEBBB9B
      0EF40A8D1D3121061C64057390455006AD2603A087B90CAB49CFEFDEFCCB7672
      E0B287E8B100B1322BAF4F236CEB00FC7DD01C7A02C08CBD8BD44D0B7AC51420
      9417E5829D0AA80134061A0231005D790047BED8500DCA188093DB0FBE549458
      4B0BFF68CDAAF415ABDF1B309FDE7B728A6ADED30D952DBA864A91350D0E5328
      D9052ECA9C020D44C5BAE00EEFD16FD214F1F959F3ABAFBE1ABC79F3E6A7B066
      F9063EFA564747472F0989097ADA12671AA28D54B654FA089105CA5C9556A3D3
      47EBE2A39A50EBAE4D2F76ECADCD3524836028C00CA15556226EB99AD8F3728F
      55F882698E4956CCFD4569F0658F73FA763A59729476E6FF4C074A76CB212D0A
      2B7A3835B92C0E9325DAD8AB91BAE5F4BA175AF40F3F9118207B28FFB85902F9
      664A96B17C39210E2CE3209B4B86816FEA16FCC826A129B59F22BD6824BD025D
      280EFE17390BE9129DC361B8087D8788AEADFA9B86CFC357E8287C628594E7C1
      66BE9932A774982D93A7CD30B325DF488919150A3186518AB116C56A134985B3
      E11A91CF2AAB4985ED336A30D10A3A8A1612295C118D3BB2E30CB889AF1B6EA6
      5C829B299FC4BD84F1289D2233E09B295BE2333508F3C7956EA6141C98081795
      94EBCCA64257016E1CC0496A9CDF372A4D1449719424D526A55385452A8197C8
      044C392B70336514F40E980DF3C5CD9407F866CA1B293F2777AFEA66CA406508
      3536B4A410653852A222B50BA7E95C91D4C2A73DF919FD65E258C960063C2BCF
      5735CA37537EDEFEB3254841B088C5D14D30109657E82C1645D9336E9594AFBF
      6451A94943C1B8C73A40194CA18A480A1443C9570A20BA819B1EF2706100D67B
      70CD2221B684F503F9764ADCFFE82EB6928829803F675E9CD507EB032ECCC3F1
      D2BA1C2B4C9311673A162B4847068A5526E1E67A1D05A94229C5521B03328178
      61E89B6FBEA13D7BF678C27104116129EDADBA2F41CFE73E59C2544006563802
      510ABA8678B999323A3C9A4C3946BA78EE1C955CB2D1FE93BF63C0E7942F92C4
      AC981C2110932F6D609D817B1E2F432F66068426DBFEE254AB0E13510128018D
      98098B0B75C47333E5779BBFC7C5EF1A523AB0E94DC4B5D4180C800061394566
      84B05805B690FB66CABFA69E78052569491903F8C4CD9425D828F03A52320B37
      53C6619EEE1FDD4C0992AC9CFCA8004CBA86602DED5D64D87A341D19588ECFC0
      CD9419B8993203778A64E066CA0CDC4C99317BF6EC8CC9932767E066CA8C1123
      4664E052BDF57DFAF4598FFAF02E6687A783685DC0BBC22555C95829FF68D7AE
      5DEB311794819B2933703365066EA69499E066CA0CCCB064A0F6666062647D9F
      3E7D3EC7A2D2221488D1A0D81608073CD2E13C906051416132301D37537ECE0C
      30E3958105A28C59B36665E0664A9E9B5B8FF5CFAFB088B40CEDD7B3A8A8F723
      702DC00C88C06D346F364CB2C3AD878855A61E98855CC76262E2883513FE0C7B
      2D16A1DE8C84D7D644C4D39DB7CF829563C0C4DD80EFDB94028B1303B0D4B514
      9717BE897B049F416CFBC04F4DE0566CF15241B9E9411750442B8907C5ABAC26
      C2831C70B41003DD0CBF07A093177741B677FB975FCA1EE5320216377B2198CA
      D4DF7D2FC7A88C7319B9BB7A36FDF0489ABF5A7C3050AB686B528B2641128E3B
      24FAD42109EBDEEA1695733B11C5ED1655BDD7587ED89CD4EFF151F5FD35335B
      86E93BD40FD48726F96AAD911675A49F56D15C2150ED26839EB8F8F3878BCE95
      A7A128FF529539ECCDDF0223F5E2AB03132C933AC75A42637CB4829F41E5F4D5
      AB5C4146B51866D618434DAA5A289FF735183021ABE543930F6DF9E015396F2B
      E681170EC9AFFEA2B56A5DB347D4F01D522FD86C47DD2F2CB1BB9C1AB4722EC8
      A71461344A51116054394D1A459856212C3A91559207EBAF00BA631E04BCF443
      AF090D223EEA9D1CACB4395CC5A50EC9AEC04E24A5288885B857089B46244141
      A259AB54F9E894EADC62A776FF8582DDD70A8AFB4C6E1B73BEDA14844D5BA74E
      F2D73FD82CDCAAC3BEDE3CC4DEAE540A2EB54250D89D920B9FD0D80382AE01E3
      84DC62874D8383CC468DC21E6CD6A465651775470ADEAA360FF29BF60BED9818
      38AD7594BF11FB314A106B17B6549012CBD6843DD1E80870830B3EFC2107C945
      58C41604AD4A10717B90E64691AD68D8A4E95F235FC0A72AA511FD7CB52AAB52
      21D855B8AD47A52001B117B0FB44528BA2A0124880B824DEF7023F820AD43865
      657EC5C8DFB61E0AAA3605D4AC5F50F348FF87EA875A14F82F103B2400E2A08A
      188B22211DE820515644E48956292A71FF11AE1080C824525FBC519C5578236F
      43F50CEAF65026845806A447FAF940BE3630E1CDC4A08ADDF69011271C0911C0
      52B44386A50E9753AB54280AED2ECDE9AB057FBA72F3D654CBA0559F1125179C
      AE46E9E1BE6981460DF2805C1C7324000A4C107B07466CFC172A0E97E4424244
      8D4AA13D9B5B229EBF98FDCDEA8593BE82D4381EDEF1D3B42EF6A3D70A567DFB
      17CF80483A64B20048009FF4454A040192822E6730FFE98B2ADFE6D49CBA5A70
      D95558F8DDA1FDDB0AAB2DA6CCB6382FF7F48A5D47AE475AB5B17D6B84F2E69C
      121457C41D4AFE3CC01F9040480695A8297648FABDE7F34AB3AF667FA577D87E
      E6F0D58A88462F8E75E5672DC9CFBD516F4F16EEBE52E9D411169D5AA3447142
      1945D4519F051E3BEA2F15D8F5DB4EE7945CB9703D535F52FCD2B4875B9C6306
      B22CD950098F2C88A5FCEB4B31EDD29EF416124CBE526A4CDCB9FAD1218EE420
      735818FEC74383325A844CB89C575A723DA7E8AC2ABF60BDC5657F6FDAB06647
      DDF4BC33E8372D968AF3975269417B3258D00EF84B3563138FF7880A5B50E214
      0E5C94C426D84896A212D14260046972394F5825C74E5C2FB2EFA961CD0ADCC4
      BDEB0F3C13431D866DA6F45E12B51F2A897D9F74D59AB1E2E8F4F7B73DF2D2CA
      5F3D1FE7991F6DD5AEF8E067D3D2953FEB17AEF845E19DD8EDEB683DC7432C37
      9652EED5F6A4C3FD149600A9666ADD135D936BBCE8AB507C346568FA5D4F42B9
      19DE2A45839F8DA1AB6741FC5A7B52EB48D49BA59AC9B58E774BAEF19255A1FC
      78CAD0A6F74CDCCD84A8EFD404EA36EE3B4AEF2D51F3BE9278DFC3AEDA93DF38
      36FDBD9F87376BD7477BCBE3BD9B447A684E2ABE1C5E275B613B6C6E24C1E24F
      356B3638D1B546ED17AD4AD527BF7CFFE53F88391F7734FB77C3575F532E8A0A
      6BA05427B5DE916E356ACDFBF5F3B73E9EFA50D3E27B8F73C5100A9F0E431C68
      C46AAB74C6C0DA31097F768E8B47CC95ABDF5DF8C43F8AB99B8D5C0F86BCF97D
      3794E9A1911AF5168328BC3F75687A91DBC33FD52B8EEC6E1F68FD5DEA15065E
      E55E3CF4DC8CDC6EF7FAEE21C4995CEE451E73BA89B13DCC41FFCD17B12464D1
      C806A2FFE68B6E0AC2F309A5C4764F175BE2FB98860F1FDE8DCD9521E026FFC4
      0AD685F29B818C64E86FA4A005317D6BCB36FCC07650B902B35981AFC9162BEE
      8FCEC65C0561275A2236776776EEDCF958EBD6AD8FD56A9C7A4CAA5D72EC48F2
      9E638ABAF663AA06AE63AEDA45C78A6A5E3F46B54A8EB5AEDBEE58E31A4D8ED5
      4AAD750CFB868F6266E00FCC5FA460DFFC4747170E9CC80C446C492FC55F6DE0
      02443921F24EE5952B572662A78DF1DA99EBC6EB5BF313E335C9246E3726DA2E
      398DAE2C329AF60426C6295228480AA37DBBF727EEDDBB3771F3E6CDE9988F53
      31516CCF5A88BD4763615657982F8285FC771CEDDBB7BF882D27ADB193B97583
      DA0D2FF27C917FB2F5A2E2842E5491696D1D18EF7FD13D5F74E2C48944EC4A38
      867F40F1C3F40393E05918D1EB7C11BB622842984362A30C25FECB88E78BAC0A
      3FA2A3EA44BF6E862D217EA105EEF922F6C4F345AC63FA8135FEBF10771E48FC
      4F267FCE9C791DF3458386117ED8C243FBF6ED0B7DF8E187B7F08414B64B8736
      096A72CC790CA70B7BF86FF10FF22F48D1D5A68F5FFBA41B44C1FF3F711DBB6C
      BB21F6D7397220C14A9E2FDA8FAB2045BCC9F345988CDD0833612A8730EF700C
      DB3E0BF0D15D809997633C5FF460DF078F85048416382F11EDFFEE77C2144226
      3294424343B70399980ADD8E0DAE4C42867BBEA82C2968352D982FC24CCB3FEE
      C1983A662FD77F79DF173DD0426838056CC7FF645282FF17B2621EE2559EB619
      3B766C26665232318D908909904C6CD3CDC4BFF764E24F8F32B13B3933262626
      13338C99289299D844C629C8C416B94C8C2533513279E0CA74318C65AD1C908A
      FFE68BCAC48E7CBD25980A2F1EEBFFE68B6451DC1AFCF22B3A79D63CF8BBEFE5
      445E56D13C14CB19CA794285F1EEEF2EFC544C4139FA158C4C8853C33A3B9437
      F37B35282B52E53D2C9592CABFB21963236310AF9D59845BEB681679A1889D6F
      C14BD88A0CCA3C1C9143702C0110E7541A6157791DCD82CF64F82927C2239838
      AC10C15B0C6E114F06B1F28ABB3AEEA96AC0B231D0108801BCADA371D84A4CD8
      AF07B78F8BDE5FFDDE94855FCC5BD26D4D8B2FEB7E12F969EC2AD31B411F0A83
      21325F4003785F47BB4951449224373019FEEFACA37161B8895B2202477422E8
      B7FFA57534D0635581012FA9A0B3C017958BFEB575B48CD0CC05CC29F781BC4C
      268EA300C47DF13F5A47DBD82B816972E992C7457FB63C92C1C4F9C8182F95E0
      448BBCB4A2285551AC3A91E47111F645D9EC36EC74719255ED43EE719142F29C
      6113205F15FAF096180C2CECF75DFF716052715C84B925795C8405537CD99605
      2CBF8EB6F6E2F7DDD65EF8A15BE8FFA97534C44856FFC7D6D1A6454CC89C13FD
      BFCC7B5F470B9DF5A5E68B7963FECD75B4FD63F74D45263F5B56D190F0E97E4F
      BFBD76EDDA5DB8D456E2C117960BE5F549CE701E4A7EB7E97BA2835847BB8089
      12519EA9432892D7D110461E3463742DE7DD472D57F1AC2F7F23947043267BE4
      C7BFB58EC6B4DCA8C000DB697371C2F7259C33988DC15420966D051EABF21A27
      17630CA8E4187285841FB9C4F17091EDA14BBC0A88BDF357DDC4ABD4FFFD7534
      66C5CD75590325B7E7F8F249BFE775B4295BEBDF6C38651A4CB622DC4CCA6CEF
      7D5C542E826524BC3D9909DB9779969B720C80BDAFA395F3C34110FB4A311764
      076F0F0E5CDE7EB45007040EC856DC4D5676F74AAB42732D07F6F66082A805A8
      38820CF65366C7A66AE1956BF93B8D7047427350C8C641FFC3D0EF49F1250177
      930233A82E06A3DE800EE67B5277C3E038287207B282FEC61D4677C3E02208FF
      ED3B8CEEC800B22F0483BF7D87D11D198038AB1D789402AC6C786401843CF17A
      8711BBB9E1B514FD77AF23C4330A909B8D0A7980CBF2B8CCC38DFEC2838B6767
      E8FFE85E474F8703E22D41EC27E85896A05CAC593E8FF7A701F7BD8E9D60BE00
      EC06E281DB55202CCE00E78000408EAC080381A806FA4FC06861D6ACBCEAEE30
      829F64400B54500A92978E8DB07C18E803AC043CBB968711D109107F07BA47A1
      0E64E1650D8A6336F4D180018804EEF95EC74711683EC0A9A90BFD37DC3C2340
      77AB7F7CAF636D50DA05B07A0E8F9701AE482AE831405DE4C92FD0DB001AE0DE
      EF75442027C0AA091EDF412CA9D01F0792812D44F401B0142801EEFA0E23119E
      599DC4830942A31D78B4074A81F3C03FBAD7D1CDE05D109A00B07A0E8FC95772
      9F6B0EAC07B478772B3D0C91C02A80C3CC84CE0D2106F7A44249E2484E855D0D
      40566E066FE3AD198A6B2794A4BD307701DE028A815C5CA26486CE2A0F8F4080
      EF308A47293B08F3A340F5F73A8268363C3D087C0D26CD5082BE067478676216
      98F360FE5B7718B9534060C2C91E0342DB10E3254002CCCCB8106639056AF561
      5E087D1FF685400B14042574BE286927F4B1C09F00D3D4419715BFC8067E80C9
      52E8494030700CB0030E401693F8D8341BCCCB81794012E00FB8D5AF30B0B88E
      43F7CE000E9C926310C9FD30AB8008201EF08809722FC5FB6BC05DDF6104BF95
      14A74CA8645BCE22EA1DEFF73A96F3724F464E8D0173A26606421A00B68356BD
      E29856E78353E1AF51AB5B07F8FB3F111B1BFB12031BC8266341A31D020600D5
      D2A8CE51C464770C3E6927F5E8D9F355DCCA3169D93BEF0C59B66CD99017E7CF
      7FA257CF9E8BE036057EE2AA63522503443D18DF6A8F3D366EDCB0F9F3E6250E
      1A3CD8D2AC79733DA698F50F0C1C6806C3F8F18F3FFE605868E8783009031304
      C1F336551503ADDE60E88858F61E3A746840744C8C02530C0266E365B0392A3A
      5AF110DC782F1E76DB76025D0D50497965C01989EFB28E081C806460CF0C1AEB
      B21ECB4D001B6744012212F10FC2FEF89EEB883016B76379DD1B0301F315A6E8
      A8A898B8F8785C0DA528EFBF8219FB64282E365615131D1DC961E058494C5E19
      40A60A4CD5A3A06860AC1406746E2A6C39C1DFC711FCAAE1916352C9335BDEF4
      7D4B8367338E6F77EBD4A953283E65B1E906946E397B4C3C7982CF5627D6CDCE
      9E3A7DFA0B7CEA667B1C6F1A2A71647B30F083885E7C75F1E201F7DD779F1E49
      617F0C767643B295964A38745BF4F8F8F15F62A96C3218F220C1ED2EEBB70792
      2DF150E363BB5BEF5EBDE6CC7AEE39DCF912077163AB1E1CDC0AB1954E9D3CE9
      C07EE1939FAF59F32C3EC6D7C18D1B4368B754550CD807F6A7FA8D7A78D8B0D1
      13264CC09A51984210E5D2C4D70F49B850C9F5FA6BAF5D7C77F9F26510D35B08
      5029F6B0A3EA18405242586848C8134F3EF9E450AC73FA98CC66D93FA60CA455
      1FAECA9D3B6FEEC758897A09A2390762125049C9012AD9DEB2E02BA06AD64D4B
      5B80F3FA2D9A346DAA05573A78E0800DA9DABD7DC78E495803FD0DDE9D805775
      27061C4887BF931B844DF3CF4F9F312318B599DE78E38D1BB8C7E5452C2FBE0D
      0F05C03F525CF1121A3668F0DDDE3D7B4A8E1C3962C35AE76EACFCF108D05B3D
      FA5BCCF8AEAFE796BEFD76CE9ACF3FCF8F8C88781BA2F2FD5B94AA08841BE674
      5D473EF2C899A953A65CC6BCEAC3F0E7B57183FDDF52B89F4A55B36D9B367BBA
      74EEFC0726A39A818A02F8D714E74358627CFC86A484841FD1B0C581B200FCAB
      2A40ABD1AC06D6826A30F0AF2B2B28728D7D1FBA1FF0AF2B2D280E0086009E81
      15CCFFAA6226F744FC4E15854B8F167DAE2FFA8570DCAF160204B31976FEA8D5
      CCB05A1A553AA22209288E0AF4053A9D56EB6FB558A230CA886760793D1ACDB9
      1FBB8149951D12CBCE2B03268E802A26825B1E6AB5EFD0A13E76EE273E376B56
      24DAFFC8C993262574841DDCEA6084E1C77E390C13BC1D5E1920002E3FD49831
      6A4819327870F4C409138C3D7BF552A6376BA6C0FFC12ABAF7E8A1C4FD20069C
      9A880A0E0A4A454A2C689B14B713E7F74A0C30FCE04A85EBEBF4E1EDDBB50BC3
      A84E83D18580DE874F0A11E44F6C8E8D8B137AF6E8A146971A8A4E3F02915273
      58265A1E9518A0960A888D06B20E45ABA9C19606F95E35C4529EB7869B7C300B
      994C21A1A142DB76EDD4E8EE4260AFE5B0E589B359C98FF2802CB9E4683024D4
      47464509C807993062E7F1063FB21D6A35652923FCF29287C6E6A726A74B5A9F
      18EC653E038F8CF3D037554A01BA3F792F2C324F815224AF21C0632575F042B1
      E9819557BACEFE59187C4997DA4952191AC19305A809740546031D2A650C92CA
      E34F1D86E8E118E8EA58DEB7339AB3F172EAB3EB2FF5BA5EE0F0C1FE24D2E6FC
      B1A538EBCC3B52E1B525640AFB168477014780CD4039356A8F4E6C305A895B7E
      ACD82FD411BB7386A3531F894E7E1456394681D8A861EF9FFC307EC6C18371D3
      0F1C1CBBE2CFB756AE5A3D12B76375E230C8E84A122947BDCC08F9B2DCB5A8B1
      A983070F1EF4EBAFBF8EB876EDDA288C7B460D78E7C46A9938187CB8F5ECB3B8
      F96DC4D0871E1A848F939A10A9963F28CAA8DCE1899828D06B59A3A3A3D39F9C
      3CF9C103FBF78F1CB8F4E82A26CED8F6E795C9BFFFFEFBC8694F3DF5506C4C4C
      73C4DE87C3DC816C056701C55285262110FB58DA367E72FD3BD14FFD76301662
      D972E8EA94D3A74F8F7C6DF16B0F4334ED7D7D7CF8FF32F97B4DA840E14E2F28
      967C91A746D9EF93519A87BFDD13FAC48E035F6E3F3D0DAB53A37EF8FEFB11A8
      607DD154C4A03E68D9EF9DE879771FB173183DB2FD6B71D80F5B7B8C7F652AF6
      7D8DC4B0652476EF0C8B8E8E6E8A652DF39D45336A8F8A46EDE9028479B88CDA
      138EF74540068DDCB541D3716EF3962D5AF4D99099390277078EE8D2A54B7F14
      8228642C7F17DC413465C4336462A3F6DCAEAFA0A1DF472013CD18CEB7C0C7E0
      F0B7DE7C733816B63B20F67799B16529E803066F016E062B61EEC229928B2DFE
      BD19152E65F0A0410F8E193D7A282A615DC45E7FEFB2474503E14A9505D7BEAA
      303E0D6F969EDE1F63A38128C2B190BD1A11B88378E0E32E9512CD857F6C7474
      8FB89898DE30072365CABB0C7B57DEB8925AD17A7602BA82388F4B2BA5F4AE28
      55E1894561845B1BA03D6006FE75061A104D066A005A4000FE55C53166264C9C
      CD7745FC6E3C72FFA04493A0417FAC4105D3011698CDB0D3A33429C1A9CAD454
      C90019C9E322158AA4D56236C7A249AE87F3968DD1573740C54B45FF1B023F1A
      7BAF8F4269C48E1628DEA38085C0646020D006C06911B0BF5D2120C75A1E17A1
      52D568D1B265120E1D066114616DD3A68DA556CD9ABE459A90D8933123FA398C
      61DD4854D6C45C4908FA5AAE1B3C308E2622D8611D02864A0AC9AE765C7435A8
      7DCAE9E8A1437C82C2EBA81C793E42E195DD63BAA7098FF6AC4F6EDC243AA852
      9F8CEACFB1D74234315DBB74891F3868901617A7F21DA77C1F2DFD6F6376831F
      4E94A64144543F5C75BC799DB8E0D4B8D050855A572A880A5CE85D961DBB8F5E
      927954CA0304AC725C34FEF34B4D779C2A4E46ADA3FEAD53A966AD5A49669351
      24A74DEF72DA9598029089967F546200F98B1051A571D1E435171BEE3B5B1CC7
      81FBB64A81B8B1711EDB1FD06C108E8AE10F1E9CB833C655167DF67413951878
      1B17CDF8EA52DAF69385491CA66FAB64D66E42C0380B07D0112B5854220E3B9E
      9567ED166E1F17AD3820A67E77ACA80E485145E284C97289EF97A5FCFC0227A9
      F405A252EDC08489C4D4DC79A0E097F2406478F0CB9527E0A0AE4DABBD575569
      C878102F130BFBE52B3438A54E5CDE5C849BA4F30B4B4A947A4B81A850E136EA
      B284B819085C1910A8197084DE6970851950CFF7A2449DF509B35E533338D05F
      39E4BE34668AA25E169833938963AC245DCDBAEE78EF8B1F36D9ADF185A4D4C5
      22732E8216A335E1A7A0FAA35A401F04B481B93BD08DF401AD248556EF2CC92D
      EADB2AC9A4516B30EB22226C190317CE87F366A6CFD77F7BFABBC3D7458721D4
      444A6D203284872FEE8A0692F4313398CE26E032600458E58EE9D9405D3B2142
      515A5CA81424A74AA3E1FE5DE4C89303FFB8BA7CFD2ED7B95C87CAA132E7C0F2
      0B70FF8A44C5E708FC2B70103809FCC2B2864E5C03836543D9C38CBDEA9252AD
      B3492E736E5E418E1ADF691A038EB89C3B73EAC8962339493979F976A9E0CAA7
      9A830BBFB09DDA5A8C54B9CA825211F42B80AC3805DDD9D4303994350F901730
      43248220391C765172946A4F9D3A7BF2B7F3F6C4DCDC3C89FE58F3A662E7A28D
      B6AC93C51814BB89234C45E5494145EBB2372E725C32941A63D19A351F9D3647
      D7AB8F1225C59E5FF5C191A3197B7046BF04335E5512672A55A6801DDD58B676
      DBB61285295DA320EDC0D0BFBEDCF3ED277FE1C2FF53D8F253525DEC397CA59A
      CC96E5F1D6577BB6481A73BA535095A4E66C5C73E3F8F61CEC98CA42EC4B415C
      AE54E5FDDF6EAE56446F7EB5770B4A466B72395CCA333F2E2A746C8F38A6D128
      B01F2F17A2712063FF3E03103F83D8B426FE95E6BD28EC7825E752686800325F
      81D8178101CBFE8E0CB826B7028D418037950DCB45A8E1D740D88CA596267857
      94DA6CDBD154E4C0CC4CA055AD98018BA92DBC7073110C9D552E1E1B40F827E8
      AC043C0C404380DBAF5DD0793AF38E0CE0AF9C1AB54783B6898995B3948D6CA7
      812919A8016801B683F6EF2911A498091367335EEFACEE140BEE9FF9831C9D9C
      4A35E5C949B9F876A3C262DCB62F08A4D480979D5B862A18E1F038CBDFAB2B32
      9589F3A8DA88016F000601012AB596C63DF638613C231FBB7522A442C2A30AB5
      E4B5C59E5D0915BCDC240E3A5A5F7F7FFF449C9C084BC7C438DFA9C64C2E5F45
      038AD8D9D0AA327D0C14E45DB6ACA34724F413F284099F6EF12A4BC8C3EBB8C8
      E97010CE87E3AA452B60A290202B450211B8A12F22107FBDEE6FA400B356B6F3
      37E9E4485712111A33EEBD2ACC176127AC804F261C7A86501065953B5AE57290
      19CB14B910C3A012CB4A70250648A6D7711152452A11A171B5CD94A9D369ECD8
      71141A114ECF3DF314FA9BB2CE9F0B00DA27020DC2FDCBE88304AFA30A5EF533
      D7484989C3349A0E1FDB3C772107FA79DBD6EEB880873ADD771F2EF1D1C807F4
      DBB66945AD5AB5225CEF21EB6DDBB6A576EDDA914BA1A65FB76DAD9CC9680298
      338B848B6785F9221C342785524DCF3C339DFF3B84B00248CFCF9E87FF60C09D
      462E5C3700700A50E268C68C191014794D01174FAFF345DBB6EFECDEB27973C2
      520B998C66CC7A29A8639B9694DEA421B56A914EED919A76786F583F0D8B775A
      DAF6CBAF95178950CC4464A819DF028DF06F02E118AE2B3029C2DF0AF4CA2BAF
      2CC545DA72EC9AE0EA4C1605EE7091AFB941E1C060C0219B39BFE6CF9F4FB8E3
      A5720A6E8A88476C5C00FCF155AFC25D449C2AC2DE78390F701D14611686B89C
      E32218C254A72CFF162D5AC8F2C795BD48819EF05F089553C082430C78C6D784
      4A56A3EFFDF7C7617E54CB4575C99B6F2E9D3E7D3ACD9A358B7033B14C1817C6
      701019E823E4C2C0FA82050BBCA7807DA2A7E28CC69F35B88AB120A7C37F8198
      62A2A345DC51D41D67A5A87EFDFA04B1C9B16451F11FF1704A18FC8EB93EF9FF
      5CAA4C0133814C392F78ED26A8666A6AFD2953A6F81E3878F05DCE83694FCFB8
      633D983BEF453905E5EA2293AD08CE70CCEAEA91074903FAF74FF5F1F55DE92E
      7EDCDE88B85F4AC01F8B71D1E4F955160DE72122271767CEE46A19303B0454E1
      733520B5468D26BD7AF7FE028B73F27F8280B95C6A9818FBF30635FEA4C49B7D
      053B1012F1D9FA0FE68B2A90ABFCC24D37FA03EDBF335F5499BE6CF3DF7C912C
      863B3CB8B419E1A70DD01EF86FBE0842808A301A5573A2A37D5FF3F74F7BC3C7
      A7F7ABFEFE0FCFF3F71FF6ACC9D47B8A565B6F8452897F6621FEF0836FEFCADD
      7D57704D8A8A12BF59B0C0F7B3F1E31B776AD76E6483EEDD9FAA3F7CF8E38DC7
      8F1FDEFCF1C787371F316242A31E3DA636AE576FE41301014DFEA756FB452A71
      934E052A652F9518248585A9963FFF7C52E3A64D4726346F3E2DBC77EF41090F
      3DD43065C080C85A3D7A04A475EF1ED0A85FBFF0E60F3FDCA0CD9831033B3FFC
      F0B4CEEDDA8D5A54B76E6AA28F8FBA8CECAD670506F166B36A79FFFE75D22C96
      312A9BAD07464FF15A1F1F93D1C7877406831D6D4B0986F0253ABDDE6E859D7F
      78B831BC7EFDB8A49E3DBBB7EADE7DECB7336634D8FBEAAB3C7EF57050BA4D91
      2A95626E4C4C52BC56FB20AE756B682F28B0A09F54894A65216E85BE50E2725D
      5668B505E82824DC9E6B34B85C4198D60912884C684EAC1A93A9AE2120408181
      8D2D393878FF91CB971D4C1BEEAC11BD1A1010D4B679F39191AD5B7750060498
      8B44D1A850A9A4E29C9CA3F8A3B49D9AF8F86C955A2D8FA6F085232A6D3675E1
      E9D3C1F87FA61AC2850BE1E6D252098B9A452EA5F2E77DFBF7BFD5EAB3CFCE31
      655944612A951A7F05D4C0273EBE093E5DF5764C7048361BAE0B1255382C159E
      B76347A49493A3329A4C4E86AF9F9FDD101858A8888D3D7DC5C767EB898B970E
      D80A8B0AEDB9B91AFBF9F3F5F4F9F94DA331A1CE0C64118D448F121916D602AD
      A60FFE870BF738955EB16971EFA1566B360604586C172F36B8B07EBD443D7AFC
      E91B17E719AF639B96531D1191BDF1CAE5DD6166739EAF4A551FE1CD06A7B3D9
      048361DB13A5A5179556A552111712128E49D078176665EC44D74FDFB8F1E535
      BD3E2DDD684C0753AB8FBFBF55BA72A5E1F5CC4C52F5EAF5A7292A4A66C23D19
      BE95A5DF7372B27667655D6C1B16168D5E2848A954C686FAF945597373AFE2F2
      2441613118C2154EA7D5515020D90B0BCFEE3E7A74FB77C5C539A19191AA3883
      A19182C8E263365B4B8A8A1A3A77EE128AB4DA3F70A525A68A8AA5937FFDE53A
      7FE142EECED2D2BF9A188D27B067CB5F703A4D98DB8846E6EF23244BBBB161C3
      878FF6E8F1D3C94183B61DEAD76FC623717118B1F8A73E3A64C8C823D85877ED
      8D37F6DD58BCF8E48DD75E3B99BB62C56F85BB76BD73F9F4E989E5D7D102753A
      9F2D2D5A3C7EA253A7AD47DAB4D9BAB156AD893E4AA59EF0D06DA85367ECEF6D
      DB6EFDA36BD7ADDFB76DFB588C4E877FCF32CAEB6833274C18736CE9D26597E7
      CFDF7779F6EC9357E6CE3D99B57CF96FE7BEFA6AE9EC279E18EB5E473382D8A6
      7AF5861E6AD9F2A7834D9B6EDB9092F214681BC847A1D0AE4B4C1CB1BB61C39F
      7F6BD162DB8F0D1B4E795CA3C11FE5693CEB688B9F7FFED113607266FAF47DA7
      1F7FFCE449FCDFC4EFA346EF59D6A2C59CCE3E3E893C3008522874EB5252C6EC
      69D0E0E7DD6969DBD6C5C54D026D3D594551F3696464EF9F5252BEFF252D6DFB
      E63A755E7D198B6F182DC8EB68905564EB56AD7A677EF4D1E3C75F7B6DD991E1
      C3F71DBCFFFE93BF76EC78FC87C68DBF5E191838789228FACED16AFD362624CC
      DD56B3E62F3F27256D01CD87405BCB0C942B83821A7E1D15B56E7342C28ECD35
      6AAC7B3F3ABA79944683C2A01451828DD8D4571FEBF80FE16F1A26FC3673E6B2
      2DEDDBEFDDD2B8F1E16D0D1AECC9484AFAE22D8BE581A53E3EAD3363633FFB2E
      3E7EC73751511B3F080E6E0D067235A0053E3E215F0605BDBA3E2CECD78DB1B1
      DB7EAC5B77D69775EB8626F8F8F0C7870A238A50F73ADAA6D5ABC72F6AD264F1
      86D4D46FBE4F4DDDB5292969E7E75151191F8684AC581F19B92D233C7CFBDAE0
      E0A54BFDFCA22D28465CD9280CF9F0B68FCF804FFDFD377D1112B2EBDBB4B46F
      8E0D1F3E62C7CC99410166332FFF561817354B48E8FD5E74F488F5D1D1EB32A2
      A377AE8D88D8B906370EAC090EDEFD6940C0771FF9F90D7FC162D1CBC4DD0F58
      44ADB45A177CE8E3B3ED9390909D3F76EDBAE1FC9225130FCD9F5FE3C9468DFC
      FD7D7D6B0D7CE081A1A3468C7838D0CFAFE1443FBF78C87FF22741413F7E1C10
      B01B447723EC2F2B2C96D7679B4C09A13777B4B9E953A842A15CE4E393BECC6C
      7E6F99C9F42B02EFCA484FDFB2E7810796FFDAB2E563FFF3F3EB37303E7ECA03
      287E83D5EA07E6EB746397582C2B975BADBF2EB75876BF6B326D7FC76058B5C0
      686C1B228A95FA0599D1730101BAB7FDFDDB2F311A972FD1E9B6BD6534EE5A1E
      10B00385E067C8F487F93E3E3BE758AD3B179A4C3FBC6634FEF4A6C1B0E34D9D
      6E37FB7D43A7FBF075A3B1DB74FE4093A955F17836284837CF6A6DB2D86098F7
      AA46F335F0CB228D66D7CB1ACD6F73359AA37380856AF56FAFA8D5BB61BF7DA1
      46B3F925ADF695578CC6964F994C3CE55305E572D6C110D75493297C965EDFF3
      45AD76FE7C956ACD5C95EA876755AA03FF039E479B3F4BA9DCF0944AB56CB44A
      35FA11AD3665A2D95CA1272B47AE6A63206439CE600898A0D1D419A352F5B85F
      A198DC4BA178BA9F283ED441143BA588626D8CC08230E7E25DE65593AEE88274
      2B40448BF18905F0033523A0452BCB15C9D32B560C55F6F6FF005C491AE07F05
      6EC30000000049454E4400000000}
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
  end
end
