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
    Left = 36
    Top = 59
    Width = 642
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
    ExplicitLeft = 30
    ExplicitTop = 60
  end
  object TBDock: TTBXDock
    Left = 0
    Top = 0
    Width = 842
    Height = 59
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
        ImageIndex = 4
        Images = PanelsImageList
        Options = [tboDefault]
        OnClick = TBmoveClick
        Caption = ''
        Hint = #1055#1077#1088#1077#1084#1077#1097#1072#1090#1100
      end
      object TBRectSave: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 6
        Images = PanelsImageList
        LinkSubitems = NRectSave
        Options = [tboShowHint]
        OnClick = TBRectSaveClick
        Caption = ''
        Hint = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
      end
      object TBCalcRas: TTBXItem
        AutoCheck = True
        ImageIndex = 5
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
        ImageIndex = 3
        Images = PanelsImageList
        LinkSubitems = NFillMap
        Options = [tboDropdownArrow, tboShowHint]
        Caption = ''
        Hint = #1050#1072#1088#1090#1072' '#1079#1072#1087#1086#1083#1085#1077#1085#1080#1103' '#1089#1083#1086#1103
      end
      object TBGoTo: TTBXSubmenuItem
        DropdownCombo = True
        ImageIndex = 7
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
        ImageIndex = 0
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
      Images = ImagesSrc24
      Stretch = True
      TabOrder = 1
      OnClose = TBMainToolBarClose
      Caption = #1055#1072#1085#1077#1083#1100' '#1080#1089#1090#1086#1095#1085#1080#1082#1086#1074
      object TBSrc: TTBXSubmenuItem
        ImageIndex = 0
        Images = ImagesSrc24
        LinkSubitems = NSources
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = #1042#1099#1073#1077#1088#1080#1090#1077' '#1080#1089#1090#1086#1095#1085#1080#1082' '#1080#1079' '#1082#1086#1090#1086#1088#1086#1075#1086' '#1087#1088#1086#1075#1088#1072#1084#1084#1072' '#1073#1091#1076#1077#1090' '#1073#1088#1072#1090#1100' '#1082#1072#1088#1090#1099
      end
      object TBSMB: TTBXSubmenuItem
        DisplayMode = nbdmImageAndText
        ImageIndex = 3
        Options = [tboDropdownArrow]
        Caption = ''
        Hint = #1042#1099#1073#1088#1072#1090#1100' '#1090#1080#1087' '#1082#1072#1088#1090#1099
      end
      object TBLayerSel: TTBXSubmenuItem
        ImageIndex = 3
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
      LinkSubitems = NMarks
      Stretch = True
      TabOrder = 2
      OnClose = TBMainToolBarClose
      Caption = #1052#1077#1090#1082#1080
    end
    object GPSToolbar: TTBXToolbar
      Left = 509
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
        ImageIndex = 10
        Images = PanelsImageList
        OnClick = TBGPSconnClick
        Caption = ''
        Hint = #1055#1086#1076#1082#1083#1102#1095#1080#1090#1100#1089#1103' '#1082' GPS '#1087#1088#1080#1077#1084#1085#1080#1082#1091
      end
      object TBGPSPath: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 2
        Images = PanelsImageList
        OnClick = TBGPSPathClick
        Caption = ''
        Hint = #1056#1080#1089#1086#1074#1072#1090#1100' '#1087#1088#1086#1081#1076#1077#1085#1085#1099#1081' '#1087#1091#1090#1100
        object TBXItem5: TTBXItem
          OnClick = TBXItem5Click
          Caption = #1055#1086#1089#1090#1072#1074#1080#1090#1100' '#1084#1077#1090#1082#1091
          Hint = ''
        end
        object TBXSeparatorItem16: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object TBItem3: TTBXItem
          ImageIndex = 18
          Images = TBImageList1
          OnClick = TBItem3Click
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' KML'
          Hint = ''
        end
        object TBItem5: TTBXItem
          ImageIndex = 18
          Images = TBImageList1
          OnClick = TBItem5Click
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1073#1072#1079#1077
          Hint = ''
        end
        object TBXSeparatorItem17: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object TBItemDelTrack: TTBXItem
          ImageIndex = 11
          Images = TBImageList1
          OnClick = TBItemDelTrackClick
          Caption = #1059#1076#1072#1083#1080#1090#1100' '#1090#1088#1077#1082
          Hint = ''
        end
      end
      object TBGPSToPoint: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 1
        Images = PanelsImageList
        OnClick = TBGPSToPointClick
        Caption = ''
        Hint = ''
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
        ImageIndex = 16
        Images = TBImageList1
        OnClick = TBItem2Click
        Caption = ''
        Hint = #1042#1099#1081#1090#1080' '#1080#1079' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
      end
    end
    object TBXMainMenu: TTBXToolbar
      Left = 0
      Top = 0
      CloseButton = False
      DockPos = 0
      Stretch = True
      TabOrder = 5
      Caption = #1043#1083#1072#1074#1085#1086#1077' '#1084#1077#1085#1102
      object NOperations: TTBXSubmenuItem
        Caption = '&'#1054#1087#1077#1088#1072#1094#1080#1080
        Hint = ''
        object N35: TTBXItem
          Images = TBImageList1
          OnClick = N35Click
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1103#1088#1083#1099#1082
          Hint = ''
        end
        object TBXItem6: TTBXItem
          ImageIndex = 6
          Images = TBImageList1
          OnClick = TBXItem6Click
          Caption = #1054#1090#1082#1088#1099#1090#1100'...'
          Hint = ''
        end
        object TBXSeparatorItem6: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object NZoomIn: TTBXItem
          ImageIndex = 7
          Images = TBImageList1
          ShortCut = 33
          OnClick = TBZoomInClick
          Caption = #1059#1074#1077#1083#1080#1095#1080#1090#1100
          Hint = ''
        end
        object NZoomOut: TTBXItem
          ImageIndex = 0
          Images = TBImageList1
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
          ImageIndex = 8
          Images = TBImageList1
          ShortCut = 16455
          OnClick = TBSubmenuItem1Click
          Caption = #1055#1077#1088#1077#1081#1090#1080' '#1082'...'
          Hint = ''
        end
        object NCalcRast: TTBXItem
          ImageIndex = 4
          Images = TBImageList1
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
            ImageIndex = 6
            Images = PanelsImageList
            ShortCut = 32850
            OnClick = TBRECTClick
            Caption = #1055#1088#1103#1084#1086#1091#1075#1086#1083#1100#1085#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
            Hint = ''
          end
          object TBREGION: TTBXItem
            ImageIndex = 9
            Images = PanelsImageList
            ShortCut = 32848
            OnClick = TBREGIONClick
            Caption = #1055#1086#1083#1080#1075#1086#1085#1072#1083#1100#1085#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
            Hint = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1087#1086#1083#1080#1075#1086#1085#1072#1083#1100#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
          end
          object TBCOORD: TTBXItem
            ImageIndex = 8
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
          ImageIndex = 16
          Images = TBImageList1
          OnClick = N6Click
          Caption = #1042#1099#1093#1086#1076
          Hint = ''
        end
      end
      object NView: TTBXSubmenuItem
        SubMenuImages = TBImageList1
        Caption = '&'#1042#1080#1076
        Hint = ''
        object N4: TTBXSubmenuItem
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
          ImageIndex = 5
          Images = TBImageList1
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
            Images = TBImageList1_24
            PaletteOptions = []
            RowCount = 5
            OnCellClick = TBXToolPalette1CellClick
            Caption = ''
            Hint = ''
          end
        end
        object NShowGran: TTBXSubmenuItem
          ImageIndex = 15
          Images = TBImageList1
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
          ImageIndex = 3
          Images = TBImageList1
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
        Images = ImagesSrc24
        Caption = '&'#1048#1089#1090#1086#1095#1085#1080#1082
        Hint = ''
        object NSRCesh: TTBXItem
          Tag = 1
          AutoCheck = True
          GroupIndex = 1
          ImageIndex = 1
          Images = ImagesSrc24
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
          Images = ImagesSrc24
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
          Images = ImagesSrc24
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
        Images = PanelsImageList
        Caption = #1052#1077#1090#1082#1080
        Hint = ''
        object TBAdd_Point: TTBXItem
          GroupIndex = 1
          ImageIndex = 11
          Images = PanelsImageList
          Options = [tboShowHint]
          Stretch = True
          OnClick = TBAdd_PointClick
          Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1084#1077#1090#1082#1091
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1091#1102' '#1084#1077#1090#1082#1091
        end
        object TBAdd_Line: TTBXItem
          ImageIndex = 12
          Images = PanelsImageList
          MaskOptions = [tboShowHint]
          OnClick = TBAdd_LineClick
          Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1091#1090#1100
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1091#1090#1100
        end
        object TBAdd_Poly: TTBXItem
          ImageIndex = 13
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
          ImageIndex = 14
          Images = PanelsImageList
          Options = [tboShowHint]
          OnClick = TBItem6Click
          Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1084#1077#1090#1082#1072#1084#1080
          Hint = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1084#1077#1090#1082#1072#1084#1080
        end
      end
      object tbsbmGPS: TTBXSubmenuItem
        SubMenuImages = TBImageList1
        Caption = 'GPS'
        Hint = ''
        object tbitmGPSConnect: TTBXItem
          ShortCut = 49223
          OnClick = TBGPSconnClick
          Caption = #1055#1086#1076#1082#1083#1102#1095#1080#1090#1100' '#1087#1088#1080#1077#1084#1085#1080#1082
          Hint = ''
        end
        object tbitmGPSTrackShow: TTBXItem
          AutoCheck = True
          ShortCut = 49236
          OnClick = TBGPSPathClick
          Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1090#1088#1077#1082
          Hint = ''
        end
        object tbitmGPSCenterMap: TTBXItem
          AutoCheck = True
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
          ImageIndex = 18
          Images = TBImageList1
          ShortCut = 49235
          OnClick = TBItem3Click
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1090#1088#1077#1082
          Hint = ''
        end
        object tbitmGPSTrackSaveToDb: TTBXItem
          ImageIndex = 18
          Images = TBImageList1
          OnClick = TBItem5Click
          Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1090#1088#1077#1082' '#1074' '#1073#1072#1079#1077
          Hint = ''
        end
        object tbitmGPSTrackClear: TTBXItem
          ImageIndex = 11
          Images = TBImageList1
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
        SubMenuImages = TBImageList1
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
          ImageIndex = 12
          Images = TBImageList1
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
        SubMenuImages = TBImageList1
        Caption = '&'#1055#1086#1084#1086#1097#1100
        Hint = ''
        object N29: TTBXItem
          ImageIndex = 17
          Images = TBImageList1
          ShortCut = 112
          OnClick = N29Click
          Caption = #1057#1087#1088#1072#1074#1082#1072
          Hint = ''
        end
        object N16: TTBXItem
          ImageIndex = 1
          Images = TBImageList1
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
        object tbtmHelpBugTrack: TTBItem
          OnClick = tbtmHelpBugTrackClick
          Caption = #1041#1072#1075#1090#1088#1077#1082#1077#1088' http://sasgis.ru/mantis/'
          Hint = ''
        end
        object NGoToForum: TTBXItem
          OnClick = NGoToForumClick
          Caption = #1054#1073#1089#1091#1078#1076#1077#1085#1080#1077' (http://sasgis.ru/forum)'
          Hint = ''
        end
        object TBXSeparatorItem19: TTBXSeparatorItem
          Caption = ''
          Hint = ''
        end
        object TBXItem8: TTBXItem
          OnClick = TBXItem8Click
          Caption = #1054#1089#1085#1086#1074#1085#1086#1081' '#1085#1072#1073#1086#1088' '#1087#1086#1076#1082#1083#1102#1095#1072#1077#1084#1099#1093' '#1082#1072#1088#1090
          Hint = ''
        end
        object TBXItem9: TTBXItem
          OnClick = TBXItem9Click
          Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1081' '#1085#1072#1073#1086#1088' '#1087#1086#1076#1082#1083#1102#1095#1072#1077#1084#1099#1093' '#1082#1072#1088#1090
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
    Position = dpBottom
  end
  object TBDockLeft: TTBXDock
    Left = 0
    Top = 59
    Width = 36
    Height = 467
    Position = dpLeft
    object ZoomToolBar: TTBXToolbar
      Left = 0
      Top = 0
      DockPos = -8
      Stretch = True
      TabOrder = 0
      OnClose = TBMainToolBarClose
      OnDockChanging = ZoomToolBarDockChanging
      Caption = #1055#1072#1085#1077#1083#1100' '#1084#1072#1089#1096#1090#1072#1073#1072
      object TBZoomIn: TTBXItem
        ImageIndex = 7
        Images = TBImageList1
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
        Control = RxSlider1
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
        ImageIndex = 0
        Images = TBImageList1
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
        Left = 9
        Top = 257
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
      object RxSlider1: TRxSlider
        Left = 0
        Top = 32
        Width = 32
        Height = 189
        Hint = #1048#1079#1084#1077#1085#1080#1090#1100' '#1084#1072#1089#1096#1090#1072#1073' '#1082#1072#1088#1090#1099
        BevelStyle = bvLowered
        ImageHThumb.Data = {
          52020000424D520200000000000036000000280000000C0000000F0000000100
          1800000000001C020000000000000000000000000000000000005F5F5F5F5F5F
          5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F
          5FA1A1A1A1A1A1A1A1A1A1A1A1AE4E2780381CA1A1A1A1A1A1A2A2A2FFFFFF5F
          5F5F5F5F5FA1A1A1F0F0F0F0F0F0F0F0F0AE4E2780381CF0F0F0F0F0F0F0F0F0
          FFFFFF5F5F5F5F5F5FA1A1A1FFFFFFB7B7B7F0F0F0AE4E2780381CFFFFFFB7B7
          B7F0F0F0FFFFFF5F5F5F5F5F5FA1A1A1FFFFFFB7B7B7F0F0F0AE4E2780381CFF
          FFFFB7B7B7F0F0F0FFFFFF5F5F5F5F5F5FA1A1A1FFFFFFB7B7B7F0F0F0AE4E27
          80381CFFFFFFB7B7B7F0F0F0FFFFFF5F5F5F5F5F5FA1A1A1FFFFFFB7B7B7F0F0
          F0AE4E2780381CFFFFFFB7B7B7F0F0F0FFFFFF5F5F5F5F5F5FA1A1A1FFFFFFB7
          B7B7F0F0F0AE4E2780381CFFFFFFB7B7B7F0F0F0FFFFFF5F5F5F5F5F5FA1A1A1
          FFFFFFB7B7B7F0F0F0AE4E2780381CFFFFFFB7B7B7F0F0F0FFFFFF5F5F5F5F5F
          5FA1A1A1FFFFFFB7B7B7F0F0F0AE4E2780381CFFFFFFB7B7B7F0F0F0FFFFFF5F
          5F5F5F5F5FA1A1A1FFFFFFB7B7B7F0F0F0AE4E2780381CFFFFFFB7B7B7F0F0F0
          FFFFFF5F5F5F5F5F5FA1A1A1FFFFFFA1A1A1F0F0F0AE4E2780381CFFFFFFA1A1
          A1F0F0F0FFFFFF5F5F5F5F5F5FA2A2A2F0F0F0F0F0F0F0F0F0AE4E2780381CF0
          F0F0F0F0F0F0F0F0FFFFFF5F5F5F5F5F5FFFFFFFFFFFFFFFFFFFFFFFFF80381C
          80381CFFFFFFFFFFFFFFFFFFFFFFFF5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F
          5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F}
        ImageHRuler.Data = {
          66270000424D66270000000000003600000028000000AF000000130000000100
          1800000000003027000000000000000000000000000000000000535353535353
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505050505050505050505050
          5050505050505050505050505050505050505050505454545353535353535454
          54535353545454000000555555C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0535353000000505050C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F3F3F3F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000505050C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0535353000000505050C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000505050C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0535353000000505050C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7
          D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7
          D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7
          D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7
          D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7
          D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000505050C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D700
          0000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D70000
          00F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7
          D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0535353000000505050C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7
          D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7
          D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7
          000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D700
          0000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D70000
          00F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000505050C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D700
          0000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D70000
          00F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7
          D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0535353000000505050C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7
          D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7
          D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7
          000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D700
          0000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D70000
          00F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000505050C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D700
          0000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D70000
          00F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7
          D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0535353000000505050C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7
          D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7
          D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7
          000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D700
          0000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D70000
          00F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7000000F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000505050C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0535353000000505050C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0535353000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000535353C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0535353000000535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          5353535353535353535353535353535353535353535353535353535353535353
          53535353535353000000}
        ImageVThumb.Data = {
          76020000424D760200000000000036000000280000000F0000000C0000000100
          18000000000040020000000000000000000000000000000000005F5F5F5F5F5F
          5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F
          5F5F5F5F5F5F5F0000005F5F5FFFFFFFA2A2A2A1A1A1A1A1A1A1A1A1A1A1A1A1
          A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A1A15F5F5F0000005F5F5FFFFFFF
          F0F0F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0F0
          F0A1A1A15F5F5F0000005F5F5FFFFFFFF0F0F0A1A1A1B7B7B7B7B7B7B7B7B7B7
          B7B7B7B7B7B7B7B7B7B7B7B7B7B7F0F0F0A1A1A15F5F5F0000005F5F5FFFFFFF
          F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0
          F0A1A1A15F5F5F0000005F5F5F80381CAE4E27AE4E27AE4E27AE4E27AE4E27AE
          4E27AE4E27AE4E27AE4E27AE4E27AE4E27AE4E275F5F5F0000005F5F5F80381C
          80381C80381C80381C80381C80381C80381C80381C80381C80381C80381C8038
          1C80381C5F5F5F0000005F5F5FFFFFFFF0F0F0FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFF0F0F0A1A1A15F5F5F0000005F5F5FFFFFFF
          F0F0F0A1A1A1B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7B7F0F0
          F0A1A1A15F5F5F0000005F5F5FFFFFFFF0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0
          F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0A2A2A25F5F5F0000005F5F5FFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF5F5F5F0000005F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F
          5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F5F000000}
        ImageVRuler.Data = {
          3A290000424D3A29000000000000360000002800000013000000AF0000000100
          1800000000000429000000000000000000000000000000000000535353535353
          5353535353535050505050505050505050505050505050505050505050505050
          50505050505050505050505050555555535353000000535353C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0535353000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7
          D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F3F3F3C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F600000000000000000000000000000000
          0000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7
          D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6000000000000000000000000
          000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7
          D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F60000000000000000
          00000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F600000000
          0000000000000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          000000000000000000000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6000000000000000000000000000000000000D7D7D7F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
          D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6000000000000000000000000000000000000D7D7D7F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
          D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6000000000000000000000000000000000000D7D7
          D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7
          D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F600000000000000000000000000000000
          0000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7
          D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6000000000000000000000000
          000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7
          D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F60000000000000000
          00000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F600000000
          0000000000000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          000000000000000000000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6000000000000000000000000000000000000D7D7D7F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
          D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6000000000000000000000000000000000000D7D7D7F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
          D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6000000000000000000000000000000000000D7D7
          D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7
          D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F600000000000000000000000000000000
          0000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7
          D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6000000000000000000000000
          000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7
          D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F60000000000000000
          00000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F600000000
          0000000000000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          000000000000000000000000000000000000D7D7D7F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6000000000000000000000000000000000000D7D7D7F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
          D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6000000000000000000000000000000000000D7D7D7F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          505050000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C050505000
          0000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C05050500000005353
          53C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
          D7D7D7D7D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0
          F6F6F6F6F6F6F6F6F6F6F6F6000000000000000000000000000000000000D7D7
          D7F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0505050000000535353C0C0C0F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6C0C0C0545454000000535353C0C0C0F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6C0C0C0535353000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6C0C0C0535353000000535353C0C0C0F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6
          F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6C0C0C0
          545454000000535353C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
          C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C053535300
          0000535353535353535353535353535353535353535353535353535353535353
          535353535353535353535353535353535353535353535353545454000000}
        Increment = 1
        MaxValue = 23
        NumThumbStates = 1
        Orientation = soVertical
        Options = [soSmooth, soRulerOpaque, soThumbOpaque]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        TabStop = False
        OnChange = RxSlider1Change
        OnChanged = RxSlider1Changed
        UserImages = {0F}
      end
    end
    object TBEditPath: TTBXToolbar
      Left = 0
      Top = 280
      DockPos = 257
      TabOrder = 1
      OnClose = TBEditPathClose
      object TBEditPathDel: TTBXItem
        ImageIndex = 16
        Images = TBImageList1
        OnClick = TBEditPathDelClick
        Caption = ''
        Hint = #1059#1076#1072#1083#1080#1090#1100' '#1090#1086#1095#1082#1091
      end
      object TBEditPathLabel: TTBXItem
        ImageIndex = 13
        Images = TBImageList1
        OnClick = TBEditPathLabelClick
        Caption = ''
        Hint = #1057#1082#1088#1099#1090#1100'/'#1055#1086#1082#1072#1079#1072#1090#1100' '#1087#1086#1076#1087#1080#1089#1080
      end
      object TBEditPathSave: TTBXItem
        ImageIndex = 18
        Images = TBImageList1
        OnClick = TBEditPathSaveClick
        Caption = ''
        Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074' '#1073#1072#1079#1077
      end
      object TBEditPathOk: TTBXItem
        FontSettings.Bold = tsTrue
        FontSettings.Color = clNavy
        FontSettings.Name = 'Arial'
        Options = [tboImageAboveCaption, tboNoRotation, tboSameWidth]
        OnClick = TBEditPathOkClick
        Caption = 'Ok'
        Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1086#1082#1085#1091' '#1086#1087#1077#1088#1072#1094#1080#1081' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
      end
      object TBEditPathMarsh: TTBXSubmenuItem
        ImageIndex = 20
        Images = TBImageList1
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
  object PopupMenu1: TPopupMenu
    Images = TBImageList1
    OnPopup = PopupMenu1Popup
    Left = 208
    Top = 168
    object NMarkEdit: TMenuItem
      Caption = #1048#1079#1084#1077#1085#1080#1090#1100
      OnClick = NMarkEditClick
    end
    object NMarkDel: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100
      OnClick = NMarkDelClick
    end
    object NMarkOper: TMenuItem
      Caption = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1086#1073#1083#1072#1089#1090#1100#1102
      OnClick = NMarkOperClick
    end
    object NMarkNav: TMenuItem
      Caption = #1053#1072#1074#1080#1075#1072#1094#1080#1103' '#1085#1072' '#1084#1077#1090#1082#1091
      OnClick = NMarkNavClick
    end
    object NMarkExport: TMenuItem
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1084#1077#1090#1082#1080
      OnClick = NMarkExportClick
    end
    object NMarksCalcs: TMenuItem
      Caption = #1048#1079#1084#1077#1088#1077#1085#1080#1103
      object NMarksCalcsLen: TMenuItem
        Caption = #1044#1083#1080#1085#1072
        OnClick = NMarksCalcsLenClick
      end
      object NMarksCalcsPer: TMenuItem
        Caption = #1055#1077#1088#1080#1084#1077#1090#1088
        OnClick = NMarksCalcsPerClick
      end
      object NMarksCalcsSq: TMenuItem
        Caption = #1055#1083#1086#1097#1072#1076#1100
        OnClick = NMarksCalcsSqClick
      end
    end
    object NMarkSep: TMenuItem
      Caption = '-'
    end
    object NaddPoint: TMenuItem
      Caption = #1055#1086#1089#1090#1072#1074#1080#1090#1100' '#1084#1077#1090#1082#1091
      ImageIndex = 19
      OnClick = NaddPointClick
    end
    object N47: TMenuItem
      Caption = '-'
    end
    object N28: TMenuItem
      Caption = #1062#1077#1085#1090#1088#1080#1088#1086#1074#1072#1090#1100' '#1089' '#1084#1072#1089#1096#1090#1072#1073#1086#1084
      object N012: TMenuItem
        Tag = 1
        Caption = '01'
        OnClick = N012Click
      end
      object N022: TMenuItem
        Tag = 2
        Caption = '02'
        OnClick = N012Click
      end
      object N032: TMenuItem
        Tag = 3
        Caption = '03'
        OnClick = N012Click
      end
      object N042: TMenuItem
        Tag = 4
        Caption = '04'
        OnClick = N012Click
      end
      object N052: TMenuItem
        Tag = 5
        Caption = '05'
        OnClick = N012Click
      end
      object N062: TMenuItem
        Tag = 6
        Caption = '06'
        OnClick = N012Click
      end
      object N072: TMenuItem
        Tag = 7
        Caption = '07'
        OnClick = N012Click
      end
      object N082: TMenuItem
        Tag = 8
        Caption = '08'
        OnClick = N012Click
      end
      object N091: TMenuItem
        Tag = 9
        Caption = '09'
        OnClick = N012Click
      end
      object N101: TMenuItem
        Tag = 10
        Caption = '10'
        OnClick = N012Click
      end
      object N111: TMenuItem
        Tag = 11
        Caption = '11'
        OnClick = N012Click
      end
      object N121: TMenuItem
        Tag = 12
        Caption = '12'
        OnClick = N012Click
      end
      object N131: TMenuItem
        Tag = 13
        Caption = '13'
        OnClick = N012Click
      end
      object N141: TMenuItem
        Tag = 14
        Caption = '14'
        OnClick = N012Click
      end
      object N151: TMenuItem
        Tag = 15
        Caption = '15'
        OnClick = N012Click
      end
      object N161: TMenuItem
        Tag = 16
        Caption = '16'
        OnClick = N012Click
      end
      object N171: TMenuItem
        Tag = 17
        Caption = '17'
        OnClick = N012Click
      end
      object N181: TMenuItem
        Tag = 18
        Caption = '18'
        OnClick = N012Click
      end
      object N191: TMenuItem
        Tag = 19
        Caption = '19'
        OnClick = N012Click
      end
      object N201: TMenuItem
        Tag = 20
        Caption = '20'
        OnClick = N012Click
      end
      object N211: TMenuItem
        Tag = 21
        Caption = '21'
        OnClick = N012Click
      end
      object N221: TMenuItem
        Tag = 22
        Caption = '22'
        OnClick = N012Click
      end
      object N231: TMenuItem
        Tag = 23
        Caption = '23'
        OnClick = N012Click
      end
      object N241: TMenuItem
        Tag = 24
        Caption = '24'
        OnClick = N012Click
      end
    end
    object N22: TMenuItem
      Caption = '-'
    end
    object N43: TMenuItem
      Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074' '#1073#1091#1092#1077#1088' '#1086#1073#1084#1077#1085#1072
      ImageIndex = 10
      object Google1: TMenuItem
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1074' google maps'
        OnClick = Google1Click
      end
      object YaLink: TMenuItem
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1074' '#1071#1085#1076#1077#1082#1089'.'#1050#1072#1088#1090#1099
        OnClick = YaLinkClick
      end
      object kosmosnimkiru1: TMenuItem
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1074' kosmosnimki.ru'
        OnClick = kosmosnimkiru1Click
      end
      object livecom1: TMenuItem
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1085#1072' maps.live.com'
        OnClick = livecom1Click
      end
      object ImageAtlas1: TMenuItem
        Caption = #1057#1089#1099#1083#1082#1091' '#1076#1083#1103' '#1087#1088#1086#1089#1084#1086#1090#1088#1072' '#1074' imageatlas.digitalglobe.com'
        OnClick = ImageAtlas1Click
      end
      object N51: TMenuItem
        Caption = '-'
      end
      object N13: TMenuItem
        Caption = #1057#1089#1099#1083#1082#1091' '#1085#1072' '#1090#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
        OnClick = N13Click
      end
      object N30: TMenuItem
        Caption = #1050#1086#1086#1088#1076#1080#1085#1072#1090#1099
        OnClick = N30Click
      end
      object N20: TMenuItem
        Caption = #1058#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
        OnClick = N20Click
      end
      object N15: TMenuItem
        Caption = #1054#1090#1085#1086#1089#1080#1090#1077#1083#1100#1085#1099#1081' '#1087#1091#1090#1100' '#1082' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1102
        OnClick = N15Click
      end
    end
    object Nopendir: TMenuItem
      Caption = #1055#1088#1086#1089#1084#1086#1090#1088' '#1090#1072#1081#1083#1072' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
      ImageIndex = 14
      OnClick = NopendirClick
    end
    object N25: TMenuItem
      Caption = #1054#1090#1082#1088#1099#1090#1100' '#1087#1072#1087#1082#1091' '#1089#1086#1076#1077#1088#1078#1072#1097#1091#1102' '#1090#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
      ImageIndex = 6
      OnClick = N25Click
    end
    object N23: TMenuItem
      Caption = '-'
    end
    object N26: TMenuItem
      Caption = #1044#1086#1087#1086#1083#1085#1080#1090#1077#1083#1100#1085#1099#1077' '#1086#1087#1077#1088#1072#1094#1080#1080
      object NGTOPO30: TMenuItem
        Caption = #1042#1099#1089#1086#1090#1072' '#1085#1072#1076' '#1091#1088#1086#1074#1085#1077#1084' '#1084#1086#1088#1103' GTOPO30 ('#1088#1072#1079#1088#1077#1096#1077#1085#1080#1077' ~1'#1082#1084')'
        OnClick = NGTOPO30Click
      end
      object NSRTM3: TMenuItem
        Caption = #1042#1099#1089#1086#1090#1072' '#1085#1072#1076' '#1091#1088#1086#1074#1085#1077#1084' '#1084#1086#1088#1103' SRTM3 ('#1088#1072#1079#1088#1077#1096#1077#1085#1080#1077' ~90'#1084')'
        OnClick = NSRTM3Click
      end
      object N49: TMenuItem
        Caption = '-'
      end
      object DigitalGlobe1: TMenuItem
        Caption = 
          #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1102' '#1086' '#1076#1086#1089#1090#1091#1087#1085#1099#1093' '#1082#1072#1088#1090#1072#1093' '#1101#1090#1086#1075#1086' '#1084#1077#1089#1090#1072' '#1085#1072' DigitalGl' +
          'obe'
        OnClick = DigitalGlobe1Click
      end
      object N27: TMenuItem
        Caption = '-'
      end
    end
    object N24: TMenuItem
      Caption = '-'
    end
    object N21: TMenuItem
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1090#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099' (Ins+MLeft)'
      ImageIndex = 13
      OnClick = N21Click
    end
    object NDel: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1090#1072#1081#1083' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099' (Del+MLeft)'
      ImageIndex = 11
      OnClick = NDelClick
    end
    object ldm: TMenuItem
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1090#1072#1081#1083' '#1089#1083#1086#1103
      ImageIndex = 13
    end
    object dlm: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1090#1072#1081#1083' '#1089#1083#1086#1103
      ImageIndex = 11
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object NMapInfo: TMenuItem
      Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086' '#1082#1072#1088#1090#1077
      OnClick = NMapInfoClick
    end
  end
  object TBImageList1: TTBImageList
    BkColor = 14933984
    DrawingStyle = dsTransparent
    Height = 18
    ShareImages = True
    Width = 18
    Left = 16
    Top = 136
    Bitmap = {
      494C010115001700200012001200E0DFE300FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000480000006C00000001002000000000008079
      0000000000000000000000000000000000008C46009C8B4500B28D4802B38F4B
      06B48E4A06B48C4701B38B4500B28C4600B38C4600B38C4600B38C4600B38C46
      00B38C4600B38C4600B38C4600B38C4600B18C4600B78C46007B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008B4500B48D4701B3873E00B07D2E
      00AC7E2F00AD884000B18D4701B38B4500B28C4600B38C4600B38C4600B38C46
      00B38C4600B38C4600B38C4600B38C4600B38C4600B28C4600B8000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008D4903B3863C00AFA46E37BFCFB3
      96D8CCAD8ED69C6024BA863E00B08D4701B38B4500B28C4600B38C4600B38C46
      00B38C4600B38C4600B38C4600B38C4600B28C4600B38C4600B2000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000782801A9A7713EC0F8F5F1F8FFFF
      FFFFFFFFFFFFF2EAE3F3935213B6884000B18D4802B38C4600B38B4500B28C46
      00B38C4600B38C4600B38C4600B38C4600B38C4600B38C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005F0000A0D7BEA6DDFFFFFFFFD6BE
      A5DCF5EFEAF5FFFFFFFFD5BBA2DC853C03AF873E00B0894200B28D4802B38B45
      00B28C4600B28C4600B38C4600B38C4600B38C4600B38C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000670B00A3C6A481D2FFFFFFFFB182
      53C6E7D8CAEAFFFFFFFFC39F7BD0AF8054C4AB7947C2904D15B4833800AE8A44
      00B28E4903B48D4803B48C4601B38B4500B28C4600B38C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000863D02B08E4A0BB4CEB295D8CCAD
      8ED6C09B76CFCCAE8FD6803300AD894208B1A26A3BBDB4865BC7AB7952C2904D
      19B4803300AD7F3200AD8A4300B28C4600B38B4500B28C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008D4700B3894100B1853C00AFE2D1
      C0E6A1682FBD7E3100AD8C4704B38C4700B3853B00AF894201B1A2692EBDB182
      57C5BD9479CCC39F7BD0935211B6894200B18C4600B38C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008B4500B28B4400B28C4802B3F6F2
      EDF6AE7E4CC4843A02AF8D4802B38B4500B28D4802B38D4803B37D2F00ACB487
      59C7FFFFFFFFFFFFFFFFE1CFBEE5873E00B08B4600B28C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008C4601B2894100B1945514B7FEFE
      FDFEBE9871CD833701AF8E4B07B38B4500B28C4600B28D4803B3823700AEBC94
      6BCCE5D6C7E8DFCCB9E3E2D1C0E6873E00B08B4600B28C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008D4905B3863D00AFA56F38BFFEFE
      FFFED7C0A8DE7F3300AD863D00B08E4904B48D4702B38B4500B28C4701B28339
      00AEBD956DCCC19B76CF8E4911B38A4201B18C4600B38C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008F4B07B4833800AEB38659C7FEFE
      FEFEFEFDFCFDE1CFBEE5A66F40BF823500AE873E00B08C4600B38D4803B37F31
      01ACBB9168CBD0B498D9803301AD8E4A04B38C4500B38B4500B2000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000904D0AB5803400ADC19C76CFFFFF
      FFFEFEFEFEFDFFFFFFFFFBF9F6FAD4BAA3DB9E632FBB894100B18E4905B47F32
      00ADC9A888D4E7D9CBEA792900AB874000B08D4802B38C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000924F0EB67E3000ACCDAE90D7FFFF
      FFFFFEFFFFFEFFFFFFFFFFFFFFFFFFFFFFFFEEE4DAF0AF7E51C4894101B18439
      00AFD0B498D9FCFBF9FCD7C0A7DD9D6228BB833900AF8B4500B2000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008F4D0AB4813400AEDBC6B1E1FEFE
      FEFEF5F0EBF6E4D4C4E7CCAD8ED6B4875BC7A0672EBD8F4A06B48C4702B2863C
      00B0DDCAB6E3FFFFFFFFFFFFFFFFFBF9F7FBCAAA8AD4873F00B0000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008B4400B28A4300B2B18354C6B182
      53C6904E0BB5873F00B1823600AE823600AE873E00B08B4500B28C4702B3873F
      00B1E1CFBDE6E9DCCFECD0B498D9BD9570CDA6703BBF884100B1000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008C4600B38C4600B3843A00AF843A
      00AF8A4300B28B4500B38D4904B38D4803B38C4701B38C4600B38B4500B38B45
      00B2965617B78D4803B4803300AD803400AE863D00B08C4600B3000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008B4500AA8B4500B28F4B08B48F4B
      07B48C4600B38B4500B28C4600B38C4600B38C4600B38C4600B38C4600B38C46
      00B3813500AE883F00B1904E0BB6904D0AB58D4803B58B45009D000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000A39F
      A4008A85860084858300848889007E8489007E878400838284008A808C00877E
      8B00868B890079847C0085868A008883850087878700838383008C8C8C00E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0006E6EB500E3DF
      E000E3DFE0007778BE003035DA003133BA00E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000ACACAC00CDCA
      CC00F3F1F100EFEFEF00EBECF000E7E6EF00EDEFE900EBE9EF00EBF2E300EBF0
      E700EDF1EC00EAF0E500EDEEF200ECEEEF00ECECEC00EDEDED00C5C5C5008D8D
      8D00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0003638C0002F31
      B1006F70B700585DE100737AF5004A4FE3003032B800E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00083818100F0F3
      F100E9EEED00F0F1EF00F0F0EA00F0EEE400ECEFE6009283FF00636BF4006564
      FF009081FA00E7EAEE00F1F2EE00E9F3ED00EFEFEF00EAEAEA00F4F4F4008787
      8700E3DFE000F0B8B000C0686000B0585000A0505000A0505000A05050009048
      500090484000904840008040400080384000803840007038400070383000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0005A5FD4003439
      DA005D64DB00969EEF007B82E900787FF1005259E500383BBC00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000887DD9003C2BBC003F3DBF009F9FD500E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00086848400F0F2
      F200F1F6F400EBF1EC00EFF3ED00F3F6ED00ECF5F8006F65FF003644F100373F
      FE006F6DF500E7F2FA00F1F0F200EBE8F100F1F1F100F0F0F000EFEFEF007F7F
      7F00E3DFE000D0687000F0909000E0808000B048200040302000C0B8B000C0B8
      B000D0C0C000D0C8C00050505000A0403000A0403000A038300070384000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000484BD600474C
      F9005D62F2007177ED008B93EB00868EE8008E94F2003539D1003C3CB500E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE0008383DD001708BE004C3FFF002F27EA003D36C100E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00087878700F1F0
      F200EDF3EE00E9F0F300E6ECFF00E4E6FF00E4EEF5007A6DF9004C47FC004842
      FF007873F800E3EAFD00F4EEF300FDF0F800EEEEEE00EEEEEE00F5F5F5008484
      8400E3DFE000D0707000FF98A000F0888000E080800070585000404030009078
      7000F0E0E000F0E8E00090807000A0403000A0404000A040300080384000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE00087C3E30072BFDC004646E0002628
      F7003C3FF300474BF200595EEF00838AF0004F58D9002222B700E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE0008B88E4000F00BF004B4DFF00424BF3005358FF002612C300E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00085868A00F6F3
      F500F2F6F000E4EAFD00ACABED008681EA007F7EFC00615AFE005D5BFF005856
      FF005B5CFA007A7FF9007F7AF300978EFD00F5F5F500EFEFEF00F3F3F3008686
      8600E3DFE000D0787000FFA0A000F0909000F088800070585000000000004040
      3000F0D8D000F0E0D00080786000B0484000B0484000A040400080404000E3DF
      E000E3DFE000E3DFE000E3DFE00087C3E3007AC2E00087C3E3004947EB000C0D
      FA002629F5003235F3004445F2002F34D80033239B00C6705D00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E0008488D6001102C1004B4CFF00454AFF004B4FFF003D43EA003D3FBC00E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0007F7E8700F8F3
      F500F4F7EE00E6E8FF00918CF5007465FF006065FF006C6AFF006E64FE00685F
      FF005F61FF005963FD005A65F5007986FF00F3F3F300F7F7F700EFEFEF008585
      8500E3DFE000D0788000FFA8B000FFA0A000F090900070585000705850007058
      5000705850007060500080686000C0585000B0505000B048400080404000E3DF
      E000E3DFE000E3DFE00087C3E3007FC9E50076A7C100BF907D002F30F3000000
      FF000404FD000F10FB002026FF00121DE500382AA200D87B4300D4745300E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008882
      E100100BAC00525EFE00444AFF003937FB00312CEB000703BA000000D6001611
      C8001104BE00140FC9003234C80099A2E200E3DFE000E3DFE00087878D00F9F2
      F700F7F8EF00EBECFF00A29FF5008076FF00786CFF006F72FC006B7CF1006C73
      FE006F6AFF00766AFF007469FF009097FF00F7F7F700F3F3F300F8F8F8008585
      8500E3DFE000E0808000FFB0B000FFB0B000FFA0A000F0909000F0888000E080
      8000E0788000D0707000D0687000C0606000C0585000B050500090484000E3DF
      E000E3DFE000E3DFE00087C3E30091B8D200A1818100559ED6005058F000413F
      E7004D4FE4005656DE006456CA00796AC600695EBB00744A8200E77F3800D07F
      7500E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000A0AED8001209
      B7005D64FF004051FB00403FF9004037F9004340FF004853FB00525AFB00635D
      FF006266F5005B57FC003E40E4003D36C100E3DFE000E3DFE00085848600F9F3
      F800FCFEF800EEF1FF00B8BAF000A6A3F9009D9CFA008C8BFF007E82FF007C7A
      FF008783FF009E9AFA009F9BF400A9AEED00F3F3F300F9F9F900F3F3F3008888
      8800E3DFE000E0889000FFB8C000FFB8B000D0606000C0605000C0585000C050
      4000B0503000B0483000A0402000A0381000C0606000C058500090484000E3DF
      E000E3DFE000E3DFE00087C3E30083BEDF00AF8F75002E9DDD006597BA00AC92
      78006B9DB30095978D00E44E0C00DD632400EE8F4400E6844200E1823F00D876
      5100EEAD9400E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0006A72D000645A
      E3006568FF005458EC004138FF004847FF004B48FB004C58F2005958FC005E4B
      FF006058FF00716DFF006060FF002D1EB500E3DFE000E3DFE00082838100FFFD
      FF00F2F5F300F5F9FA00EEF1FF00F4F5FF00EAF5FD00AEACF8008A87FA008A84
      FF00AAA9F900E6EDFF00F4FBFE00F6FCFB00F8F8F800FBFBFB00F3F3F3008888
      8800E3DFE000E0909000FFC0C000D0686000FFFFFF00FFFFFF00FFF8F000F0F0
      F000F0E8E000F0D8D000E0D0C000E0C8C000A0381000C060600090485000E3DF
      E000E3DFE000E3DFE000E3DFE00063B4DD00849A9900B19A7900ADA183007FA6
      A8005FA5BD00C0916D00E1581300D4642600EB8B4900E7894600E4844000DE79
      4800EEAD9400E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0009FACDA001400
      CA00938BFF00666AFF00534EF7004C48FF006067FF006C71FF007A8AFB007E7B
      FF007E88FF007A89FD004E51EF004A3ACE00E3DFE000E3DFE0008A898500FFF8
      FB00FFF8FB00F9FCFA00FEFFFA00F7F8EF00F7FFF800B4ACF3009C93FF009990
      FF00B3B0F500F6FBFC00FFFFF500FAF6F100F8F8F800FDFDFD00FDFDFD008787
      8700E3DFE000E098A000FFC0C000D0707000FFFFFF00FFFFFF00FFFFFF00FFF8
      F000F0F0F000F0E8E000F0D8D000E0D0C000A0402000D0686000A0505000E3DF
      E000E3DFE000E3DFE000E3DFE000EEAD94005EA2BC0066B1CA0066B0CE0060B0
      D300A2B7AE00F7864C00C7723700A47B3400EA714000E17D4000E4843F00DF7B
      4900EEAD9400E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008F8B
      D3000C00BE008C92FD006E71FF006764F6004845E8000D03C1000205BE00220F
      D0000C01C1001612BF00383DC80096A4EC00E3DFE000E3DFE00086848300FDFF
      FE00FFF8FB00FBFAFC00FEF9F600FFFFF800F1F6F500CCC8FC00B8B9FD00B3B3
      F900C6C6F400F6FBFA00FFFDF900FFF8FB00FCFCFC00F8F8F800FFF8FB008282
      8200E3DFE000F0A0A000FFC0C000E0787000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFF8F000F0F0F000F0E8E000F0D8D000B0483000D0707000A0505000E3DF
      E000E3DFE000E3DFE000E3DFE000EEAD9400B9644200A3B2A900A1D2D300CED6
      C000FDC28D00E9966200EB946000EA885900E4815700E16D4300DF733000DD7B
      5100EEAD9400E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000898DD4001201C7009298FF007683FD00857EFD005447E9003F38C100E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000AEAEAE00C7C7
      C700FFF8FB00FFF8FB00FFF8FB00FFF8FB00FFF8FB00FFF8FB00FFF8FB00FFF8
      FB00FFF8FB00FFF8FB00FFF8FB00F3F7FC00FEFEFE00FFF8FB00CECECE00AAAA
      AA00E3DFE000F0A8A000FFC0C000E0808000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFF8F000F0F0F000F0E8E000B0503000E0788000A0505000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000CF5E1A00DE8C3A00FFE8C100FFEA
      C100EFBC9400F3AD7E00BEB37600E1A37500EB946F00DF956F00DB602300C95C
      4300E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE0008985DE001207C200A8B4FF008F9FFF00B1ADFF002015C500E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000B6B3
      B5008C8C86007D80770081878200D5DADB00C1BDB8008C898400898A8000847D
      8000978D8600DBD4CB00C1BDC9007F8885008686860084848400AFAFAF00E3DF
      E000E3DFE000F0B0B000FFC0C000F0889000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFF8F000F0F0F000C050400060303000B0585000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000BE907700C16A1D00F5C4A400F4E4
      C900F4CBAA00F0B89500F0B59200EDAB8A00EEAD9400C0A77400C8400F00C393
      9000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE0008D8ADA000F01BD00BFD2FD00726AEC004434D500E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE0008B8B8B00E5E5E500E7E7E70081818100BDBDBD00CECE
      CE008C8C8C00E4E4E400E7E7E70086868600E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000F0B0B000FFC0C000FF909000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFF8F000C0585000B0586000B0586000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000CD6D5500EB9A6D00FAE8
      CB00F8DBC100F6D2B800F4CDB300F4C9AF00F0B69A00DD674300C4857E00E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE0008A87D7002E1CD5003C3CCC00A3AAE300E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE00084848400FFF8FB00FFF8FB0089898900838383008282
      820082828200FFF8FB00FFF8FB0084848400E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000F0B8B000F0B8B000F0B0B000F0B0B000F0A8B000F0A0A000E098
      A000E0909000E0909000E0889000E0808000D0788000D0787000D0707000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000CF8B8200E699
      7D00EFBDA000EFBDA000EFBDA000EDB79D00DE826E00CB929100E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE0009F9F9F00FFF8FB00FFF8FB00FFF8FB00FFF8FB00FFF8
      FB00FFF8FB00FFF8FB00FFF8FB0095959500E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E6997D00E6997D00E6997D00E6997D00E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000999999008888880083838300848484007E7E
      7E0087878700848484009A9A9A00E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0007F7F7F008E8E8E008783
      880084878F0076808A0064788300627C880053788C002F7B87001F7E87002C60
      6D00847F94009B99AC00BFCFDF00E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E0DBD800D2CBD000DECDD000D1DDD100D1DAC600CEDAC400C8DAC300D2DC
      C500CAD2C100D3D5CF00D7D0D700D8D2DD00E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E2E3DF00E3DAD700D5E2E000B8D5D200A9C5CC00B6CA
      CF00CAD6D600D5D8D600D8D7D300D9D7D600D7D6D800D6D6D600DFDFDF00E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000C0C0C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008A8A8A00FFFFFC00FFFF
      FC00C8E2F00076A1B4004F899F005FA5BC003B88A200DEECFF00E0F5FF003D7B
      930071B4C90029798A003088960080B5C900E3DFE000E3DFE000848484008383
      83008C878400868B8E00B7C2B8005BAB8000268542000C792A00077D2A000E82
      290027843F006FA88100A9BAAF00BCB6BB00ACA0A600E3DFE000E3DFE000E3DF
      E000888D8B00888C870085807D008D888A00437385002A798E002D739000235C
      75004F73830077868F00828485008B868700878584008B8B8B0083838300DFDF
      DF00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000C0C0C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00084848400FFFFFC00ECE6
      F100479CAB00BDCDDE00FFFFFC001179960018769A00C6F3FF00C8EAFA001573
      97000D789400FFFFFC00ACCFD9003089B500E3DFE000E3DFE00095959500FDFD
      FD00FFFEFD00FBFAFE00F3F9E8003B99580046B350004BC84D003BBF3D0037C1
      44004EBF5E004F965D00EDFFF400FDFFFF0087868800E3DFE00098D6A00098D6
      A00084828100FFFDF900FFFFFE00FBFAFF00227396004FC5EF005CC3F4002076
      A0005C93B200BFD8EC00F0F2FC00FFFDFE00FFFDF900FDFDFD008B8B8B00D6D6
      D600E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000C0C0C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00084848400FFFFFC00DEE9
      F1005AA2AA00FFFFFC00CAECFF00C8F1FF00A0C9F600A5D3EB0097D3E9009DCB
      F400C2E8FF00D1EEF500FFFFFC00468BAC00E3DFE000E3DFE00084848400FFFF
      FF00FBFFFF00E5E7EF00E6F1DD00007925003BC9480012AF11000FB40D002DAA
      3C00147A280098D6A800F2FFF600FFFDFE008685810098D6A00069996F0098D6
      A00089848500FDFFFE00FFFAFD001B7D8900D2F0FF0082E3FF0069CFF9006FCA
      E50068798E00616C7A0082989D00D4DFDD00E2D8D800E3F4F100787A8200DDD5
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000C0C0C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00084848400FFFFFC00D3D0
      D20088B3C6000B718E00DDF3FF00B2F0F000C6EBF900C9E7F800BFECFF00B2E8
      FF00B1E5FC00C5EDFF00146F8A0071ADDB00C0E2F200E3DFE00085858500FDFD
      FD00F9FCFF00D4CDDA00DFDFD1000C7B270041C542002EB5310071DB760020A1
      2E00197F270094C99700E5F5E300E0EFE10094BC93003BA4530094C99D00C0EA
      C7007C848300FEFCFB00F9FFFE00DEE1E6001B7D8900D6F6FF0087D1E9008FA1
      A800E0DAD300B4C4BD00776C74008F888D00848D90008C9CA2006C727F00B8B8
      C400E3DFE000E3DFE000E3DFE000E3DFE000C0C0C000C0C0C000C0C0C0008080
      8000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000E3DFE00084848400FFFFFC00B4D1
      D500398492000D7C9200B1CCE700CBF0F800D5F5FA008EC6D90092C3DD00BFED
      FE00AAEAF500AFCDF000177B9D001F76960069A2B800E3DFE00086868600FEFE
      FE00FDFFFE00F2ECF100EFEEE0000578280049C84D0045BB44001082250045BC
      4F0037C14B0000892300137F36006EBD7E005AAB66002B83410098D6A000E3DF
      E000827F8100FFFFFE00FFFDFD00E6EAEB00BCCCD9002E7C8300B0C0BF00FFFF
      F800EAE8DD00768B880095A8A500D8D9CF00E9E9D900D6CBB5009B928E009290
      9600CFD3D400E3DFE000E3DFE000808080008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000E3DFE000E3DFE00084848400FFFFFC002090
      9C00DFF7FF00EEF4FF009DD3EA00E9ECFF0096BFDF0014739E00066F960098C3
      D400D4F1F800A3D4E200CDEDFF00FFFFFC00357E9400E3DFE00086868600FEFE
      FE00F9FFF800FDF5F500E5ECDD002A92510083E787008AE78C000F752E0089E0
      900066D5690048C6440047BF420057BD580078D07C0073AD8400E3DFE000E3DF
      E0008B868800F7FCFB00FFFCFB00FFF0F100D6E9EE00A5CED10062748500E3E1
      F700D4CBDF00D0BCBB00FFDCB500FFB46800F9953D00F8C47D00FFDCB200B0A0
      94009A9E9F00DADDE100E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000FF800000FF800000FF800000FF800000FF800000FF80000080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00084848400FFFFFC002A90
      9D00F1F9FF00DBF4FE00B9E2F100CCF1FF0088C2D80011729600187A9C008FC2
      D200CAF7FB00A8D4E500C1EFFF00FFFFFC004B90A100E3DFE00086868600FEFE
      FE00FFFFF900E3EAE700FFF8F50098CCA700418C4E003D894F00A8D4B5001C77
      2C0053B3550093E48F00A4E49C0099E19400379E430098D6A000E3DFE000E3DF
      E0008C858800FFFFFF00F4FBF800EAF3F000F0FAFA00F4EDF200A3B5AE008376
      7E00B2AEAD00FFD8B100F49F4900FCA22B00FAB03200F5A53400D8934400FFE6
      C40096938F00BFC3C800E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000FF800000FF800000FF800000FF800000FF800000FF80000080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00084848400FFFFFC00A4BD
      C70028859A001A7B8900E1F5FF00D0F1FF00D8FAFA008DCDD80095BCD200DBF8
      FF00B2EAF500E0F1FF00177795001984A0009FC9D500E3DFE00086868600FEFE
      FE00F8FFFF00DBD8DA00E0D9E000CEE6DC00D2EBD100CCE2CF00CED9D700BBCB
      C40058AB770004822300018923001D8A46006A997F00E3DFE000E3DFE000E3DF
      E0007E848300FFFBFD00FAFFFF00DBE9E700F1D7DD00E1DADF00D9DCD3008197
      9200EDE9D000FFAF6F00FCA73A00FFBC4200FFB13A00FFB23400F89E3900FABA
      8300C1B3A700A4ADB100E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000FF800000FF800000FF800000FF800000FF800000FF80000080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00084848400FFFFFC00EDEC
      F500B0D0ED0019759200B9D6EB00E0F8F600D9F7FC00E1F1FF00D3F0FF00DBF3
      FF00DFF0FF00AECCE7002E71900059A3C700D7E8F100E3DFE00086868600FEFE
      FE00FDFDFF00FBF6F300F5F1F700EEF4FB00FDF2F400FFF1F400EFF0FA00FBF8
      FA00FBFAFF00EBF4F800F5FEFF00FEFDFF008F7F8600E3DFE000E3DFE000E3DF
      E0008B868700FFFEFF00FFFFFF00F5F5F500F5F7F700F5F7F700F7F2EF008884
      8A00FFF2F000FA974700FFD6AB00FFD79C00F1C06A00FABB4B00F7983100E49F
      5A00D4D1BC0098A1A400E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000FF800000FF800000FF800000FF800000FF800000FF80000080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00084868700FFFFFC00EDF1
      F6005E9EA200E4F8FF00FFF4FF00AAD5EA00FFF9FF00BBDAE900BFE0E900D6F2
      FD00B2D9EF00E0F4FF00FFFFFC00438BA300E3DFE000E3DFE00086868600FEFE
      FE00FFFCFF00F0FAEE00F5F4F600FFF7FE00F6F9F700EEFAF400F6F5F700F4F5
      F300F2F3F100FAF5F400FFFFFE00FAFDFB00878C8B00E3DFE000E3DFE000E3DF
      E00088868600FFFFFF00FFFFFF00F8F8F800F5F7F700F5F7F700F1FAFD00A3A4
      A200DBDCD800EEB27C00DEAA7500FFE2C600F8EAD800F8B75B00E9943E00FBC2
      8A00BBB8AA00B6BEC500E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000FF800000FF800000FF800000FF800000FF800000FF80000080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00084868700FFFFFC00F3FC
      FF0058A7AA00A4CFDE00E6F2FF002A7B8A0000709800EDF9FF00F5FBFF001D75
      99001B718F00FFFFFC00BBC3D4005F9BB800E3DFE000E3DFE00084848400FFFF
      FF00FBFBFB00E2E2E200E1E1E100E3E3E300E2E2E200E0E0E000E1E1E100E7E7
      E700E2E2E200E3E3E300E7E7E700EBEBEB0085858500E3DFE000E3DFE000E3DF
      E00086858700FEFDFF00FBFAFC00EDEDED00E1E1E100DFDFDF00D7DEE700BCBE
      BF009FA6A300FAE1C100E6A45600DCA67D00CEAC8E00E7A96D00D1925600FFDB
      B50089868200E5E7F100E3DFE000E3DFE000C0C0C000C0C0C000C0C0C0008080
      8000FF800000FF800000FF800000FF800000FF800000FF80000080808000C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000E3DFE00086868600FFFFFC00D6DA
      E500B5D2DB0065A0A90056919B0098C3D2002A89AA00EBFDFF00E2F9FF003D7D
      A0008AC1DC00347A8700549FB500C2DAF800E3DFE000E3DFE00084848400FEFE
      FE00FFFFFF00F7F7F700F9F9F900FBFBFB00FAFAFA00FAFAFA00FBFBFB00FCFC
      FC00D5D5D500D7D7D700E5E5E500D7D7D70085858500E3DFE000E3DFE000E3DF
      E00081838400FFFEFF00FFFEFF00FCFCFC00FBF9F800FDFBFA00FFFFF100EEF7
      FA00A9B5B700ABAB9D00FFE4A400FFC08000FFA14900E7BB9C00F7D6B500A799
      8600CFD0D400E3DFE000E3DFE000808080008080800080808000808080008080
      8000808080008080800080808000808080008080800080808000808080008080
      8000808080008080800080808000E3DFE000E3DFE00087858400FFFFFC00FFFF
      FC00FFFFFC00FFFFFC00FFFFFC00FFFFFC00C3CFE100368C9800348D9700A7BF
      CB00DCD9E2007F898900CFDDDC00E3DFE000E3DFE000E3DFE00086868600FEFE
      FE00FFFCFB00FEFAF900FFFDFC00FFFDFC00FEFAF900FEFAF900FFFDFC00DCDC
      DC009797970086868600868686009292920087878700E3DFE000E3DFE000E3DF
      E00084868700FFFFFF00FFFDFD00FFFDFC00FFFBF800FFFFFA00FFF9F900FBF7
      FF00F3ECEF00B9C0BD0094999000BDC2C100DFD9DA00CDB8B000948F8000C0C4
      BE00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000C0C0C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00087858400FFFFFC00FFFF
      FB00FFFEFB00FFFBF700FFFFFC00FFF6F500DCD8D7008A969A007D7F7F008C85
      88009097A0007B867E00ECECE000E3DFE000E3DFE000E3DFE00086868600FFFF
      FF00F3ECE300F1EAE100F2EBE200F1EAE100EFE8DF00F0E9E000F3ECE3009393
      9300FFFFFF00FBFBFB00F2F2F200F2F2F20098989800E3DFE000E3DFE000E3DF
      E00081818100FEFCFB00F0EDE900F1EBE600F2E9E000F3E8E000EEE9E000F3EA
      E100FCEADF00F4F8F3008B949700B4BFC3009B9EAD00BFBABB008E999600E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000C0C0C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008D898400FFFFFC00F2E7
      DF00FAECE600FFE5E500FFE5E400F3FCFF008B8A8000EFF5F000EDF5F4008087
      8400BFC0B700E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00081818100FFFF
      FF00F9EDE100F8ECE000F9EDE100FAEEE200F9EDE100F8ECE000F7EBDF008686
      8600F6F6F600F2F2F20089898900E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E00087847F00FFFFFB00F1EAE100F6ECE200FDEDE000FDEDDD00FFE8D000ECF2
      E700F9EEDA00F0FCFF00817E8E00F3F6FA00F8F0E900BABBBF00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000C0C0C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00080808000C0C0
      C000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00086868600FFFFFC00FFFF
      FC00FFFFFC00FFFFFC00FFFFFC00FFFFFC009A9A9A00EAEAEA008D8D8D00B6B6
      B600E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00091919100F9F9
      F900FFFEFB00FFFFFC00FFFFFC00FFFFFC00FFFDFA00FFFCF900FFFCF9009C9C
      9C00F3F3F30086868600E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E00084848400FCFCFC00FFFFFF00FAFAFA00FFFFFF00FFFFFF00FAFAFA00FFFF
      FF00FBFBFB00FFFFFF009A9A9A00EFEFEF0087878700E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008080
      8000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00080808000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000868686008E8E8E008585
      8500858585008585850085858500858585007E7E7E0092929200C0C0C000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0007E7E7E008B8B
      8B008083880083868B0082858A0080838800808388008184890083868B008686
      86008F8F8F00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E0008585850088888800818181008A8A8A0085858500808080008B8B8B008686
      860087878700828282007D7D7D0093939300C4C4C400E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0009F9FA000827F8800C8A3
      A900E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE0007CC18800E3DFE000E3DFE000E1D6
      E000DDD4DE00D7D7D700D7D7D700D7D7D700D7D7D700D7D7D700D7D7D700D7D7
      D700DCDCDC00E3DFE000E3DFE000E3DFE000E6E6E600DCDCDC00D6D6D600D8D8
      D800D9D9D900D9D9D900D2D6D100D3D6DE00D0D5EA00C5D1DD00C2D1D300D4DA
      D500E3DFE000E3DFE000E3DFE000E3DFE000C8A3A9004AAFFF003C85DF005A6D
      AD00C8A3A900E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE0001AA323007CC188001AA32300978D9900978D
      9900948795008686860086868600868686008686860086868600868686008686
      86008C8C8C00D8D8D8008D8D8D00858585008383830082828200858585008686
      860085858500838383008584860083967B004A469F001F0BB8002517BD00667E
      8A00665FB0002012BE00402CCB00877BDF0099D0F9005EC1FF0049AEFE003D84
      DF009181A100CCADB200E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000CCD1
      C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1
      C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1C600C3DBCD00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE0004197510039B64E000B7A1E0000801A00F3F9
      F800F7F5FB00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084848400DBDBDB0084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFC00ECF5FF003827C4005D62FB005D66FF00484B
      D1001904B700515FFF005C5DFF003131A300E3DFE00098CEF8005DC0FE004AAF
      FF00596DAE009E94A600CCADB200E3DFE000C9A9A000C9A9A000C9A9A000C9A9
      A000C9A9A000C9A9A000E3DFE000E3DFE000E3DFE000E3DFE000CCD1C600CCD1
      C600F1FFF700EDFFF500ECFFF000ECFFF000E2FBEB00D9F3E500D8F1E100D4ED
      DE00D1EADB00CEE7D800CAE4D500C7E2D300C3DBCD00BCD9BC00C3DBCD00E3DF
      E000A9D7BA002E8C5100067C3100158029001AA3230036BA38004FCA4A000080
      1A00A3D5AD00E7E7E700EDEDED00EDEDED00EBEBEB00EAEAEA00E6E6E600FFFF
      FF0084848400DADADA0085858500FEFEFE00FFFFFF00EBEBEB00EDEDED00ECEC
      EC00EAEAEA00E9E9E900F2EBE800D7F1E100828CDA000D00D100798AF9004249
      F6004847FF007885FF001107C5008A8AE400E3DFE000E3DFE00098CEF8009DD1
      F90090CAF200A1989600AA8A8600CC9E8D00E0C0A200F3E7BB00FAF8CD00FEFE
      D800FAF8D500E0CFB900C9A9A000C9A9A000E3DFE000E3DFE000CCD1C600FBFB
      FD00DDC0B400D5AD9A00C3A99E00B2988A00D9B39800EEC1A000E5BB9D00E4B9
      9B00E0B49600DBAD8F00D7A68A00D09B7E00D19F8600CFC4CC00BCD4BD00E3DF
      E0003F8F4E0047BD540042C6440048BD4A0031B7350010BD15002CBB29004BC7
      4B0025944A00C2C2C200BEBEBE00BBBBBB00BDBDBD00BFBFBF00C4C4C400FFFF
      FF0084848400DADADA0085858500FFFFFF00FDFDFD00E1E1E100D6D6D600D4D4
      D400D5D5D500D7D7D700D2D6CA00D1DFDE00C1D8D400383BD3000300BA004233
      FF004751FF000C00C7003849C100E3DFE000E3DFE000E3DFE000E3DFE00098CE
      F80090CAF200BA989600CE9B8800F4DAB100FFFAC900FFFECE00FFFFD100FFFF
      D900FFFFDF00FFFFF400F4EEE900C9A9A000C9A9A000E3DFE000E0D8D800FBFE
      FF00CA936F00B6541300764E34001D10090014030000985B2700DC863E00D27F
      3B00D27A3500CB6F2A00C3642000B8510C00BB5B1E00D4CCCD00BDCFC000E3DF
      E000458C540082E288008DEE80008BE37F0063D0500025BA2A0042BB310096D8
      860027904D00F0F0F000EBEBEB00EAEAEA00EDEDED00EEEEEE00EBEBEB00FFFF
      FF0084848400DADADA0085858500FDFDFD00FFFFFF00ECECEC00EEEEEE00EEEE
      EE00EEEEEE00EEEEEE00FFE9FC00E3EDDC009D8FDD000703C6007F85FF008888
      FA006266F4007C86FF000F09BA008E88E100E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000CE9D8D00F4D7AE00FFF7C800FFF1BE00FFFBC700FFFFD600FFFF
      E100FFFFEA00FFFFFF00FFFFFF00F4EEE300BB968E00C9A9A000DEDAD600FCFE
      FE00CA967600BD530C009A501C004E505200464B4F0004000000B96B3000CD74
      3300C36C2E00C36A2B00BD632500B4501200B75B2600D6CFD000BDD2C000E3DF
      E000A8DBB60032925200047B2D000B8128002F9E34002DB33B008CED8F000082
      2C00B0D4C400BBBBBB00C1C1C100C6C6C600E3DFE000BABABA00C7C7C700FDFD
      FD0084848400DADADA0085858500FFFFFF00FFFFFF00EEEEEE00EEEEEE00EDED
      ED00EEEEEE00EEEEEE00F2E8F400E4F1E3002D1BD400ACA1FD008381FF000800
      C4009DB4F8007979FF00949BFF002D29B800E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E0BCA100FFF6C500FFE9B600FFECB900FFFDCB00FFFFD800FFFF
      E200FFFFEF00FFFFFE00FFFFFA00FFFFE400E0D0B600B18D8D00DCD7D400FCFF
      FF00C58E6D00AB410200B9551400774D3300777C80001113140086593B00BA54
      1100B7551600B04F1100B04B0E00AA430500B0502100DAD2D300BED1C100E3DF
      E000E3E5E500F4FFFE00F8FFFC0010792E003FBD5300A1DE8E00037C2B00B9DE
      C400F2FFFA00F9F9F900EEEEEE00F0F0F000F7F7F700F2F2F200EDEDED00FFFF
      FF0084848400DADADA0085858500FCFCFC00FFFFFF00EFEFEF00F2F2F200F1F1
      F100F0F0F000F0F0F000E7EFFC00E1F0FF002619C300BBC6FF00BFCDFE004C4A
      D9001000C400BAD1FE00B9CDF600271EC200E3DFE000E3DFE000E3DFE000E3DF
      E000D0A79D00F1DFB700FFF2C300FFDFAC00FFECB900FFFDCA00FFFFD700FFFF
      E300FFFFEF00FFFFF600FFFFED00FFFFDA00F3EDC900AD857F00D9D4D100FBFF
      FF00C0876800A2300000AE490E00B64F0D007E4A2500AAAEB100A3A7AB008366
      5100BF652400BE5E1D00AB460A00A2350000AE4D2000DDD5D600BFD2C100E3DF
      E0007E848300FFFDFF00FFDEE5002C904F0095DA97000B843300B7D9BA00F0F0
      F000BDCDC600E9E9E900CACACA00BDBDBD00C3C3C300C3C3C300C9C9C900FFFF
      FF0084848400DADADA0085858500FFFFFF00FEFEFE00E4E4E400DBDBDB00D9D9
      D900DBDBDB00DEDEDE00DCDFD000D6DED4008386DB002C20B8002B1ECD00EDFD
      FF00524C99003B19C4003B1EC800988DED00E3DFE000E3DFE000E3DFE000E3DF
      E000CCA49A00FAF5C500FFECBA00FFD9A600FFE6B300FFFBC700FFFFD500FFFF
      DF00FFFFE800FFFFEB00FFFFE600FFFFD800F9F6CD00B9988B00D8D2CF00FBFE
      FF00BD836600A2300000BA5C1F00C7703000C86D2A00B09C8C00CACED0009199
      9E00856F5D00C1682900C66B2A00AE480D00AD4B1F00E0D9DA00BED3C200E3DF
      E00085858500FDFFFF00E6E9ED00F8F7F300F7F9F900FFFFFE00F1EDE800C6C3
      BE00C2C4C500EBF6FA00BCC5C900BFC6C900BDC2C500C2C6C700C5C4C600FFFF
      FF0084848400DADADA0085858500FFFFFE00FFFFFE00F5F5F500F6F8F800F2F4
      F500F2F4F500F3F5F600F0F2F300F7F9FA00F0F2F300F6F8F900F4F4F400FFFF
      FF0084848400D8D8D800E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000CAA39900FEFDCA00FFE7B200FFD6A300FFE8B400FFF7C300FFFFCD00FFFF
      D700FFFFDD00FFFFDF00FFFFDC00FFFFD300FDFCD000BC988900D5D0CD00F9FF
      FF00BD846400B9541000CD793900CE7C3D00D47F3B00B9764000C7C6C600D6CA
      C400808A91003C586300B1622900C5682500B65B2800E2DADA00C0D3C200E3DF
      E00085858500FAFCFD00E3E5E600C0C2C300878B9000FCFEFF00FDFAF600FFFC
      F700F6F7F500F9FBFB00F7F7F700F7F7F700FAFAFA00F9F9F900F8F6F600FFFF
      FF0085858500DBDBDB0085858500FDFDFD00FFFFFF00F7F7F700F9FBFC00F7F9
      FA00F7F9FA00F8FAFB00F5F7F800F6F8F900F3F5F600F8F8F800F7F7F700FFFF
      FF0085858500D8D8D800E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000CCA59A00FAF4C400FFF3C700FFE9BD00FFE2B200FFECB800FFFBC800FFFF
      CF00FFFFD200FFFFD500FFFFD100FFFFCD00F8E9BE00B48B8000D2CECB00F5FC
      FE00C5937200C5682400CF7E4000D6894800D88E5000E6975100AF9B88007DA3
      B9001D7EAF00007CBD0044575E00BB601F00C4713700E6DFE000C0D4C300E3DF
      E00085858500FCFCFC00EBEBEB00BFC3C8007C828900FBFEFF00F7F3EE00FFFE
      F500FFF9F200FFFAF200FFFBF300FFFDF600FCF6F100FDF8F500EEEAE900E5E5
      E50085858500E4E4E40085858500FFFFFF00FDFDFD00EDEDED00DFE1E200DDDF
      E000DEE0E100DFE1E200E3E3E300DEDEDE00E2E2E200DFDFDF00E0E0E000E8E8
      E80083838300DADADA00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000D0ACA400EEDCBA00FFFCDA00FFEECE00FFDFB200FFDAA800FFEDBB00FFF1
      BF00FFF6C400FFFBC800FFF3C100FFFECB00F3D6AC00A97F7D00D1C9C800F4FA
      FE00BF876400C7651B00D5823F00DC8E4800E49A5400F1AB6200DA9D5D003886
      AA000690D7000684C2000576B00062524200C3642600F5EBEB00C8DACB00E3DF
      E00084848400FFFFFE00FCF9F500F8FAFB0082858900FFFDFD00EDE4DA00F0E1
      D100F6E8D600F9E4D500EEDBCE00F7E7DB00EEE1D900B4ACA5008E8B87009092
      92008B8B8B00E3DFE00085858500FFFEFF00FFFFFF00FBFBFB00FBFBFB00FEFC
      FC00FEFCFC00FCFAF900FDFBFA00FCFAF900FCFAF900DCDCDC00D9D9D900DDDD
      DD0086868600E1E1E100E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000C9A49B00EDDECF00FFFFFF00FFFFFF00FFEAC900FFEABF00FFDA
      A900FFD4A100FFF4BF00FFF2C600EDC09800B0848100E3DFE000CCD1C600F9FC
      FF00C1988900C2815800D39D7700D7A37C00DDAD8500DFB38C00F1BE8F009293
      8E002390CE000488CA000788C80010648E008C5E4600FFF8FA00C7E4C900E3DF
      E00086848300FFFFFC00FDF7F000FFFDF80088848300FFFEF900F1E4D600F2DF
      CA00F7E2CC00F9E2D200F4DFD000EEDDD000EFE3D9009C948D00FFFFFC00F1F3
      F300AAAAAA00E3DFE00085858500FFFFFF00FFFEFE00FEFCFB00FFFEFA00FFFE
      FB00FFFCF900FFFBF600FCF9F500FFFFFB00DDDBDA0095939200858585009494
      940084868700EBEBEB00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000C4A19B00EDE0D700FFFFFD00FFFFEC00FFF7CA00FFF2
      BE00FFF6C200FFEBBD00EEBD9500B8888100E3DFE000E3DFE000CCD1C600E8F2
      E700F4FFFA00EFFFFB00F0FFFA00EFFFF900EFFFF800EDFFF600F0FFFA00E0F0
      DA004F9AAD001692D3000487C800007FC000456F8800EEFBE700E9F5E900E3DF
      E00087858400FFFDF600EEE7DE00EDE1D5008E888300FFFEF900EBDFD300F4E4
      D300F1E0CD00EFDFD200F1E3D700EDE1D700E6DCD500908A8500F5F1F0008587
      8800DBDBDB00E3DFE00085858500FFFEFB00F4EFEC00EEE9E600EEE6DF00F1E9
      E200F4EBE200F5ECE300EEEAE500FFFFFA0094918D00FFFFFE00FBF9F800EDEF
      F00096989900E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000B88F8C00CBA79100EDD6B100F8E9C000FDF0
      C500F9E6BB00D3A79300BC929000E3DFE000E3DFE000E3DFE000E3DFE000CCD1
      C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1C600CCD1
      C6009971990032A0C9000786CA000089D0000374A30099719900E3DFE000E3DF
      E00088868500FFFFF800EBE2D500E5DAD2008584860081848C0084878C008081
      850086868C00848883008185800083868400828583008385850089888A00E3DF
      E000E3DFE000E3DFE00085858500FFFFF900F5ECE300F3E8E000FAECE000FDED
      E000FDEEDE00FFEFDF00F4EEE700FFFEF70089858000FEFBF700F4F2F100BCBE
      BF00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000AE868500AD827B00B98D
      8500C5989100E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000997199002EA4D5001287CC000B9EC80099719900E3DFE000E3DF
      E00086868600FAFAFA00FFFFFF00FFFFFF00F9F9F900FEFEFE00FCFCFC00FFFF
      FF00F8F8F800F1F1F1008B8B8B00BEBEBE00E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE00082828200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FAFAFA00FFFFFF0093939300F0F0F00086868600E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE00099719900997199009971990099719900E3DFE000E3DFE000E3DF
      E000868686008E8E8E0082828200888888008585850088888800838383008181
      81008A8A8A008F8F8F00BDBDBD00E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE0008D8D8D00858585008585850085858500858585008585
      850085858500858585008A8A8A0080808000808080009B9B9B00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000C6B2AA00AF9E9800AE9E9900AFA39D00BCB5
      B300E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000B2B3B500938C8D008D75730092747000977773009977730093716C007E61
      5B0084706C00A59D9D00A0A1A5009FA0A400A0A0A300A0A9AF00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000AAAAAA00A6A6A600A6A6
      A600ADADAD00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000A2BAD600A8BCD500A7C0D40093C1D3009BC3
      D6009BC0D6009FBDD600A5C0DA00A5BFD7009EBBD000ACCFDD00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000B1AE
      AF00A7A5A200B7B8B800E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000AC95
      8E007DA7B6008ACAD9007BB8CC0078B7CD0070B1C90069ABC50062A5C1005A9F
      BC004F94B4004B91B200458DB0003D87AD003E88AE003B769700E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE0009797970064646400454545003A3A3A003F3F
      3F005858580085858500B3B3B300E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E00080B9D9001B759800237A94000C7799000070A10017789C001775A0001371
      9C001977A200106F9B000F709C001877A300106F97001C759A00A8C6D900E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008493E300315B
      DF00ADAEAC009F9C9B00B7B8B900E3DFE000E3DFE000E3DFE000B19992007034
      1E007FA9B600AFFBFF009DE7F80095E3F6008ADBF2007FD4EE0073CBE9006AC5
      E50061BFE40055B7DF004DB1DC0040A8D60041ADDC00407A9B00E3DFE000E3DF
      E000E3DFE000E3DFE0005F5F5F00151515000B0B0B0010101000111111001010
      10000B0B0B000B0B0B003C3C3C00A1A1A100CACACA00E3DFE000E3DFE000E3DF
      E000327CA000FFF5FF00ACE4F500C2E0FD00ACE7FB00A1DAFA00AAD9F8009BDD
      F6009DDFF80091D2EE009CDDF9008FD0EC00A2E2FB0036778C00308CA500C2D4
      EB00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE0008695DF00154CFF003592
      FF0089D2F600BFBDB600ABA9A800E3DFE000E3DFE000AC958E0098604D00823F
      26007DA6B300AEF7FE007399A50092DBEE00669FB50081D7F300669FB50062A5
      C00063C1E500669FB5004DA4C800477F9A0041ADDC0041779600E3DFE000E3DF
      E000CBCBCB0043434400020202001F1F1F002525250025252500272727002525
      2500222222001F1F1F000808080013131300A0A09F00D7D7D700E3DFE000E3DF
      E0000B6EA2000D7A9A00C0EDFF0070D8FB0067CCEC0069CFE6005DC9F80076C4
      F3006BC0F00060BFF00054BCEB0057BFEE0058B8E60092DFFF001B7695005799
      B200BCCDE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE0008695E300134CFF00429AFC006DD2
      FC0043A7FE00446FD800ADAAAD00E3DFE000E3DFE000A27F7500B17564005E21
      0D006E99AB0078B1C3006696AA0069A2B800639EB6005E9AB4004D829C004982
      9C004C8CAA004A8DAD004687A6003A7491003D84A70037759900E3DFE000DFDF
      E0006A6A6A000606010031313100232323002B2B2B0030303100323232003030
      30002D2D2C00212121002B2B2A001616150013131400C9CACA00D1D1D100E3DF
      E0001B6F9F0095C6EC00127A9100D2F3FF0089DDF70084D5F80075D6F80073D0
      F7006CCBF3006ED0FA006BCEFA0062C5F10064C5F1006AC9F100AFE1FF000B78
      8E005CA5B900B6D2E300E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000A5A4AA001D4CD400409AFE006CD1FC004BA7
      FD00205DFF008292DF00E3DFE000E3DFE00097837C00A97B6F00D2A09300905D
      4D00663D30006B3A2A0073402E006F3C2A006C3928006B3F3200BBABAB00C5B8
      B80084635800552B200061403B00BBA19D008A676000AC958E00E3DFE000B7B7
      B80014141100323220002323240039393800383839003D3D3D00414141004040
      3F00383836003B3B38002A2B26002A2A2400090A030057585300E4E5E500E3DF
      E0000A739C00BBF1FF0089C4D30012779000ADE3FF009BDEFD0097DEFF0089DA
      FF0082DAFF007AD9FF0072D7FE006CD2FC0072D2FA0074C9F1008DD6F400B8E6
      FF001E78900075ADC000E3DFE000E3DFE000E3DFE000E3DFE000A7AAAD008D92
      95008D9192007E81860097999E009E9A9500929FA9005EB9E90048A5FF001C59
      FF008795DF00E3DFE000E3DFE000E3DFE000946E6000B6867A00C99A9000E1B8
      B100A16D5D00661B00007E270500802906007520000090513700FFE8DF00F3D9
      D200D9AC9A006A250C0076331C00BA7B660080381D00AA817000B7B7B8007575
      740008080700272727002A2A2A003838380049494900494A4900474744004949
      4100515246004A4B3B0041422D0036381D003234160024250D00BDBCB800E3DF
      E00015799600C7EAFE00B7EAF40088D0E1000F729200BFEDFF00B4EBFF00C7F3
      FA00CBF4FD00C8EDFB00C5E9F900C6ECFE00C5EEFD00BFEFFB00B2E2FF00D2E6
      FF00EEEFFF0076A7B700E3DFE000E3DFE000E3DFE0008E909400988D7B00CDAC
      8300E3B88D00CFAE88008C8273006E727800E5DFD90095A7B6001A50DF008796
      E200E3DFE000E3DFE000E3DFE000E3DFE00098675800B9877B00C7998E00D2AC
      A500DDB8AF00753B2600762E1100823518006A1F0200976F5F00FBF3EE00E3D1
      CF00EED6D100AD7F70005E210B0098574000873F220080523F00ABABAB005353
      53000D0D0D00272727003131310038383B00474747004D4D49005F5F58006364
      510066664A006C6C490068683D006464320065652D004C4A18009E9F8E00B7B7
      B50014779700CBF1FF00BDE9FF00ADE3FF0088D0E1000F729200207595000F74
      9B0010749E0011719F001271A2001172A4000C6FA1000973A20016759600097A
      95003590A500BCD3E200E3DFE000E3DFE00095969A00B09D7B00F8CB8400BCB4
      6500879F4D00BCB26E00FAD19300B3A2840072757A0094908D009FA1B300E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE0009A665500BE8D7F00D4A69B00D3A8
      9D008C5A4A0084442D0089452B00955D47009D6F5C00D2BDB500FCF4F200EBD9
      D600E4CEC900E6C6BE00A1736200854B37009C5A4400895A4900ABABAB004F4F
      4F000E0E0E00272727002F2F30004E4E3F004E4E490053534D008F8F86009492
      6E0093905300A2A05200A9A64600B5B23900ADA93200A29D1900A3A37700B7B7
      B5000E719D00E2EEFF00C9ECFF00C1E4FF00BCF1FE00C1EBFE00C7ECFA00BAEC
      FF00BDEBFF00C3ECFF00C9E9FF00C0EEFF00C2E9F70013739700E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000B1B3B700A59B8800F9D29100FFCD9B006FAC
      570005BB440062943A00FFC89800F9D196008D847600A4A7AB00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE0007C4D3B00A8716000965F4E008C56
      44009A614C009C604B00A1715F00EAD8D000FDFAF700F9EEEC00F1E4E200EBDB
      D900E7D4CF00D8BEB800CDA9A000AF7F7100B87C6B0099706400ABABAB006363
      63000B0B0B00262626002E2E3200757546006363520052524F006A6A55007677
      4D00888B45009EA04200A4A63100D8D92100AAAA1B0093940300A0A17700B2B2
      B30005769700E2F6FF00C8EFF800CDEFFF00CAF0FC00CDEEFF00C9EDFD00D3F0
      F900CDF0FA00D0F2F800D0E5F400C8E9F900DDF2FA000A779300E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE0009FA2A500D7BB9500BFC87E0061A348004DB0
      4C0041DE7600369A2F0067953D00BBB67500CEB18D009BA0A400E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE0009F8176008E523E0070301900A172
      6000B3816F00A7736000C9A99C00FFF1ED00EFE3E100F3E7E500F3E5E300F1E1
      DE00E8D5D100DDC6C100E4C7C200D7ABA300A46B5B00BDA19800E3DFE0008989
      89000F0F0F0023232300393939004A4A4D00545454005E5E5900606053007374
      5100888A550096994800A1A33F009E9F2700A0A01A00949414009B9B8500B7B7
      B50007789900E4F4FF00CCEDF600D1F1FF00D0EDFB00D8F0FF00CDEFFF00DBEC
      FF00CCEFFD00CBF5FF00D5F6FF00C6EFFF00D5EDFF000D789D00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000A3A6A700EECDA80090C5720028D65F004BE7
      7D004CE17C0041DE760007BA410086A55500E1B89300979C9E00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000AC958E009C695A00BA867400B291
      8500B18F8200BD958600B8938400D1B6AD00F2E3E000E7D9D700DBCCC900D8C8
      C400E9D7D300E6D2CD00E6CECA00D6ABA20090635600AC958E00E3DFE000ABAB
      AB003C3C3C000A0A0A00464646004F4F4E005353530057575200646456006A6A
      530076774A008687490090923E009B9C3700898B06008B8C3D00A2A29800E3DF
      E00010749600F4F7FF00E6F9FC00DCF2FD00D7F3F400E2F5FF00DAF4FF00C9F7
      FF00E7FDFF00EAFDFF00E1F3FE00E4F5FF00FAFDFF003F8B9D00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000AAACAD00DFCAAD00C1DFA50074C8790067CC
      73004BE77D004EB150006FAE5B00BEBC7200CBAE8C00959A9E00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000A38A8200B3877A00E8CD
      C500B8A49C00BDA59B00C2A69C00BEA19600DECAC500E3D3D000E4D5D200E1D2
      CE00E5D4D200E6D5D000F5E1DD00BF94870098847D00E3DFE000E3DFE000E3DF
      E0009B9B9B0019191900252525005B5B590048485000787851009B9B51005555
      4F00696A4B007375440086874C00747420006D6E150092947600B6B6AB00E3DF
      E00009729D00F0F4FF00E3EDFE00E5EEFF00DFF5FA00E3F3FF00E8F6FF000C70
      A6001C71990005759300077A9B000C77980004728A00A0C4F200E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000B6B7BA00B6B0A000FFFCDF00FFFAFD0074C8
      7A0026D55D0060A64C00FFD5A800F9D49400968E7F00ACAEB100E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000AC958E009B746600DAC2
      B800F9EFED00DACAC700D4C4BE00D3C2BB00DED0CA00F7EBE900F3E6E400F2E4
      E200EDDFDC00F6EAE700E1CFC800A4867C00AC958E00E3DFE000E3DFE000E3DF
      E000E3DFE000ABABAB002424240013131300474748005C5C5800626259005E5E
      57006363520060614300414211004C4D17008F907700AFAFA500E3DFE000E3DF
      E00005749400EAFDFF00DEF9FF00E0EBFF00DEF5F700E7F8FF00F0FCFF000D78
      8D00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000B1B1B300C6C4AF00FDFDE700C4E4
      B00095CA7D00C3CF8B00FADBA000AFA0800094959A00E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000AC958E009C7C
      6F00D4C3BB00FEF9F700FFF8F600F3ECEA00F0EAE700F4EEEE00F7F0F000FCF5
      F300FBF4F300E1D5CF00AC938800AC958E00E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000ABABAB007272720023232200121213001B1B1C001E1E
      1D001A1B1300292A1A006E6F5B00B3B3AA00E3DFE000E3DFE000E3DFE000E3DF
      E0005C8F9F00F9F4F300E6FFFF00EEF8FF00F2FEFF00F7FDFF00FCFAFA00268A
      9C00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000B0B0B200B5B3A300E1D2
      B900F1D8B700DAC3A100A39B8B0098999D00E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000AC95
      8E00A4887D00B5A19900D9CDC700ECE3DE00F9EFE900FBF2ED00F1E9E500DBD3
      D000C4B3AD00B0999100AC958E00E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000ABABAB00ABABAB008A8A8A008787
      8700A3A3A300ABABAB00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E00084D0F4005A90A1000076890004749E000F7A9C0014739A003A8BAC00B0D4
      EC00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000B9B9BD00B1B3
      B400ADAFAF00A8AAAE00B2B4B800E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000AC958E00B68E7E00B68E7E00B68E7E00C09B8C00AC95
      8E00B3877A00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000AFACA700A4A3A100E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000A498
      9800C1AEAE00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000F8BDE500F38A
      BD00C77D9400C47D9300F388BC00F8BBE400E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000939FE100365FDA00ABACAC0099969600E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000CCAFAF00E1C4C400B7AFAF00E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000F8B8E200F382AA00CD7C8B00C47B
      8900BF7A8500B77A8200B37A8300B07A8500B07B8800F8BCE400E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000BF9C9C00B6919100B5939300B5929200B592
      9200B5929200B5919100B5919100B5919100B4919100B4919100B5919100B491
      9100B5929200B38F8F00BC999900E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00096A1
      DF001C52FF003390FF008BD3F700BBBAB300A5A4A200E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000B7A3A300FAD9D900E3C5C500E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000F59BD700FAD0EC00F2F2F200F385AF00F384
      AC00F6A9DD00FAC8E900F8BBE400F7ACDE00F389BE00C47E9600F493D300E3DF
      E000E3DFE000E3DFE000D4B5B500D1BABA00D7D1D100D4CBCB00D3CBCB00D1C9
      C900D1C8C800CFC7C700CEC5C500CCC3C300CCC3C300CBC3C300CBC4C400CCC3
      C300C9BEBE00CAC5C500BFA3A300E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE00096A1E3001B52
      FF004097FC006ED2FC0041A5FD004B73D800A9A4A200E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000C0A9AA00F9CDCD00F5CECE00D3BCBC00E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000F8B5E100FACBEA00F2F2F200F2F2F200F2F2F200F2F2
      F200FBD3ED00F6A8DC00F492C900F386B300F38ABC00E881AA00B87D9100F9BF
      E500E3DFE000E3DFE000CBC0C000D2BEBE009F2C2C009F2C2C009F2C2C009E2B
      2B009F2B2B009E2B2B009E2B2B009E2B2B009D27270099202000961D1D00951F
      1F009D282800A23E3E00CBD0D000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000ACACB1002150D6003E99
      FE006ED2FC0049A5FD002763FF00929FE400E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000D5C1C100DDC6C600DFC5C300E7C8C700E5C4
      C400EDC6C400FFCECD00F7C7C700F2D2D200BBAFAF00E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000F48CBC00F2F2F200F2F2F200FCDDF100F599CE00F27C
      9000E1798300D67A8100D37A8600CE7B8800C47B8900BF7B8A00BD7C8E00B27C
      8F00E3DFE000E3DFE000D4CDCD00D4B5B5008F00000090000000910000009000
      00009000000091000000900000008F000000940909009F242400B24E4E00CF95
      9500C4767600940F0F00CACACA00E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000A2A5A90084878C007B7C7F0076797E00E3DFE00096928B00919EA8005EBA
      E90046A3FF00235FFD0097A2DF00E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000C9C0C000E7D7D600FEE6E700FFE4DD00F3A04B00F2912500F391
      2500F49C4600FCCFC900FCC9CA00FFD9D800CDB7B700E3DFE000E3DFE000E3DF
      E000E3DFE000F9C1E600F385A900F6A0D900F386A000F27C8500F27C8300F27E
      8A00F27D8900F27A8500E57A8A00DC7B8C00D37B8C00C27C8A00B17A8400AD7B
      8900FACBEA00E3DFE000D9D2D200DABFBF009202020093000000930000009401
      0100940202009404040092050500AD414100D8A6A600F3E5E500FFFFFF00F9F2
      F200A72F2F00A2282800CED2D200E3DFE000E3DFE000E3DFE000E3DFE0008F90
      940094897700C3A87E00CFB18000C6AB8300897F6F00696D7400E6E0DA0095A7
      B6002054DF0096A1DF00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000CFC4C400F9E9E900FFF2F200FDEAEA00FDEBF000F5D2B600DD760000DD77
      0000F6CBB200FEDADE00FCCDCC00FEDFDF00E9D0D000B4A9A900E3DFE000E3DF
      E000E3DFE000F490C200F2798400F27B8100F3828C00F3899600F48B9800F38A
      9800F3869400F3818E00F27B8800DD798500C97A8300F381A800F7B2E000F38A
      C000F48CC700E3DFE000DED5D500E0C4C40095070700960A0A00980E0E00990F
      0F009B1313009E1A1A00980D0D00AB383800E3BEBE00F1E2E200F5E9E900C06F
      6F009F1D1D00B5565600CFD3D300E3DFE000E3DFE000E3DFE00094969900B49F
      7F00F2C88500EEC58400EBC38500EFC88D00F5CE9500B5A28600727579008F8C
      8A00A2A3B100E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000CDBF
      BF00FAE9E900FFF3F300FDEEEE00FDEDEC00FFF2F700FAEBE700CF6A0500CF6B
      0700F9DBD500FFDEE100FDD3D300FCCFCF00FFDDDD00DCC2C200B2AEAE00E3DF
      E000E3DFE000F27C8E00F27E8D00F3889500F4909C00F596A100F59AA400F599
      A300F4929D00F3889300F27C8100F384A200FACFEC00F2F2F200F59AD700CD7D
      9700E983B100E3DFE000E0D7D700E4CACA00A0202000A2262600A3272700A730
      3000A8353500A3262600C4767600F3E4E400E3BEBE00CE8E8E00DBABAB00AB3A
      3A00B7575700C1727200D1D3D300E3DFE000E3DFE000E3DFE000A2978600FDD6
      8C00F9D58800F7D38700F8D18300F6CC7900F6D08200FBD49200948A7E00E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000C8ABAB00EFD9
      D900FFF7F700FCEFEF00FEF0F000FEEEEE00FDF0F100EAC8AC00C76B1400C96E
      1B00F7DBD300FEE1E300FDD8D700FED3D300FCCACA00FDD8D800C3B1B100E3DF
      E000E3DFE000F27E9200F3849000F48F9A00F59BA400F6A8B000F7AEB400F6A9
      AC00F59A9C00F493A100F9BFE500F2F2F200F2F2F200F381A600B1798000B37A
      8700EB83B300E3DFE000E1D9D900E7D0D000AC3A3A00AF424200AA383800D9A6
      A600CC898900E0B7B700F8F1F100D2979700B14A4A00BD666600BE686800BF6A
      6A00CA838300CA878700D4D5D500E3DFE000E3DFE0009A9DA300D7BF8C00BFB1
      B000636DC3006C74BB00696EB7006867AC0065549B00BCA19300CFB689009DA0
      A600E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3B8B800F7E7
      E700FEFDFD00FEF5F500FEF0F000FEF1F200FAEBE800D3945F00CF8C5200D08D
      5500F7DED800FEE4E600FEDBDB00FDD7D700FCCECE00FFD4D400E3CACA00C7AF
      AF00E3DFE000F598C800F3879100F4949D00F7ACB700FACCDA00FBD7E100F9C2
      C800FAC8E800F2F2F200F2F2F200F2F2F200F382A200BD797E00F387B700F7B0
      E000F490CD00E3DFE000E2DBDB00E9D5D500B8575700B44F4F00CD8E8E00F7EE
      EE00F3E7E700F1E0E000CF909000C67B7B00CD8C8C00CC898900CF8F8F00D399
      9900D3999900D39A9A00D7D8D800E3DFE000E3DFE000A1A2A700F0D697008995
      D6000F48FF001E52FF00093FFF000038FF00000CFA0087749800E1C386009C9E
      A400E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000EFC2C200FAEA
      EA00FEFEFE00FDFAFA00FEF6F600FDF1F000FDF1F200FFF5F800EFDCD200E4BF
      AA00FEE8E600FEE5E500FDDEDD00FED9D900FCD1D100FED5D500F0CDCD00CFAC
      AC00E3DFE000FACEEC00F59ABE00F59EB000F9BFD300F2F2F200F2F2F200F2F2
      F200F2F2F200F2F2F200F2F2F200F3869E00D9787A00F27FA200F2F2F200FCE0
      F200FACEEC00E3DFE000E3DDDD00ECDCDC00C06C6C00C8808000F8F1F100FFFF
      FF00F8F1F100ECD4D400DDB1B100D69F9F00D7A4A400D9A7A700DAA9A900DAAA
      AA00DBADAD00DCB0B000DADBDB00E3DFE000E3DFE000A6A7A900DFCFA100BFC4
      DE006987F3007690E9006E84D7006D7CCA005F60B600BFA39400CBB386009799
      9F00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E0BABA00F9E2
      E200FDFEFE00FDF9F900FEFAFA00FDF5F500FFF9FB00D4B3A700B2806800CCB2
      A400E0C6BD00FEE9E900FDDFDF00FEDCDC00FDD2D200FFDDDD00DDBDBD00E3DF
      E000E3DFE000E3DFE000F59EC700F2F2F200F2F2F200F2F2F200F2F2F200F2F2
      F200F2F2F200F2F2F200F490A300F27B7E00E97B8C00F2F2F200F2F2F200E580
      AA00E3DFE000E3DFE000E5E0E000EFE1E100CB868600EAD0D000F4E7E700E9CE
      CE00E4C1C100E0B9B900DEB4B400DFB5B500E0B8B800E0BABA00E2BDBD00E3C0
      C000E4C3C300E5C6C600DCDDDD00E3DFE000E3DFE000E3DFE000B6B2A100FFFE
      DB00FFFFF300FEFBE000FFF6C300FCE7A900FADC9300FED99300948A7D00E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E8CC
      CC00FFF8F800FBFDFD00FDF8F800FEFDFD00EADAD400A46A4F00C19A8500E4D5
      CD00C2A89900EACEC700FEE7E800FCD9D900FFDCDC00F3D1D100BEB1B100E3DF
      E000E3DFE000E3DFE000FBD9F000F9C3E700F2F2F200F2F2F200F2F2F200F2F2
      F200FACCEA00F4909200F27F8000F27C8900FBDBF100F2F2F200F59ED800FACC
      EB00E3DFE000E3DFE000E6E4E400F1E7E700DCB0B000E1BDBD00DEB5B500DFB7
      B700E1BCBC00E3C1C100E5C5C500E7C8C800E7CBCB00E8CECE00EAD2D200EBD5
      D500ECD7D700EEDBDB00EAEAEA00E3DFE000E3DFE000E3DFE000A8A8A700C5C2
      B100FDFDE700FEFAE400F8EECC00F6E1B000F3D59E00B2A184008A8C8E00E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000EED2D200FFF9F900FBFCFB00FEFFFF00D8BFB300D5B9A600F4EFE500EBE0
      D400B48B7100D7B5A600FEEBEE00FFE1E000F8DCDC00C5B3B300E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000F7AFDF00F59CB700F9C6E800F9C2E700F59D
      AA00F48D8D00F48B9900F69FD100F2F2F200F2F2F200F6A1D900F6A7DC00E3DF
      E000E3DFE000E3DFE000D5D1D100F9F4F400F8F1F100F6EFEF00F9F3F300F9F4
      F400F8F4F400F8F3F300F8F3F300F8F3F300F7F4F400F8F4F400F8F5F500F7F5
      F500F6F4F400F8F6F600F1F2F200E3DFE000E3DFE000E3DFE000E3DFE000A8A8
      A800B6B3A400D6CDAE00E0D2A900D0C19B00A49C8A0096989B00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000ECD0D000FFEBEA00FFFCFC00F8F1EF00D8C1AC00F2EDE600E8DC
      D000C9A68E00F7E3E100FFEAEB00EBCFCF00C7B4B400E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000FCDEF200F3868B00F48C9600F7AB
      D700FCDEF200F2F2F200F2F2F200FBD2ED00F383AD00FACFEC00E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000D5D1D100E1DCDC00E2DEDE00E2DFDF00E2DF
      DF00E3E1E100E3E2E200E3E3E300E4E3E300E4E4E400E5E5E500E5E5E500E5E5
      E500E4E3E300E6E5E500D7D6D600E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000B1B2B500A5A5A600AAAAA900A7A9AC00B0B2B600E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000D5C3C300E6D0D000F1DCDC00F1DDDB00DBC1B600DDC3
      B700F5E2E000F1DBDC00DFC7C600BDB5B500E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000FCE2F400F7AF
      DF00F6A1D900F59AD200F59DD800FBD3ED00E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000CAADAC00EFD8D900F3E2E600F3E2
      E600EED6D700CAAEAD00E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DFE000E3DF
      E000E3DFE000E3DFE000E3DFE000E3DFE000424D3E000000000000003E000000
      28000000480000006C0000000100010000000000100500000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FFFFE0001FFFFFD8FF000000FFFFC0000FFFFFC07F000000
      FFFFC00008001FC03F000000FE1FC00008001FC01F000000FC1FC00008001F00
      3F000000F81FC00008001E003F000000F01FC00008001C001F000000E000C000
      08001C000F000000C000C00008001C0007000000C000C00008001E0007000000
      C000C00008001E0007000000E000C00008001E0007000000F01FC00008001F00
      0F000000F81FE00018001F000F000000FC1FFC00F8001F801F000000FE1FFC00
      F8001FC03F000000FFFFFC00FFFFFFF0FF000000FFFFFE01FFFFFFFFFF000000
      8001F000FC001FF7EF0000008000C00070000FE7CF0000008000C00040000FE7
      CF0000008000C00000000FE7CF0000008000400000000F000000000080004000
      100006000100000080004000300003E00F00000080004000300003E00F000000
      80004000700003E00F00000080004000700003E00F0000008000C000700003E0
      0F0000008000C00070000300000000008000C00070000600010000008001C000
      70000FE7CF0000008001C00070001FE7CF0000008007C001F0003FE7CF000000
      800FC003F0007FEFDF000000801FC007F0007FFFFF0000008FFFFFFFFF600700
      0F00000007FFFFFFFE0000000000000003FFE0003E000000000000008103C000
      1000000000000000C000C0001000000001000000E00040001000000000000000
      F80000001000800000000000F80000001000000000000000F000000010000000
      00000000F00000001000000003000000F00000001000000003000000F0000000
      1000000003000000F00000001000040003000000F80040001000040003000000
      FC00C0001000040007000000FE01E00030001C000F000000FF87FFF83000FC00
      1F000000FFFFFFF87001FC003F000000FE0FFFFFFFFFFFFFFF000000F0003F87
      FE003FFFE3000000E0003E01F0001FFFC1000000C0003C0070000FFF81000000
      80003000300007FF0100000080002000100003FE0300000000002000100003C0
      0700000000000000100003800F00000000000000000003001F00000000000000
      00003E003F0000000000000000003E003F0000000000200000003E003F000000
      0000200010003E003F0000008000700010003E003F00000080007800300FFF00
      7F000000C000FC00F00FFF80FF000000E001FF03F00FFFC1FF000000FC07FFFF
      FFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFF9FFE7FFC0FFFFFF000000
      FFF0FFF1FF003E0001000000FFE07FF1FE001C0001000000FFC07FF0FC000C00
      01000000FF80FE007C000C0001000000F081F8007800040001000000E003F000
      3800040001000000C007E0001800040001000000C01FC0001800040001000000
      800FC0000800040001000000800FC0000800040001000000800FC0001C000C00
      01000000C01FE0001C000C0001000000C01FF0003E001C0001000000E03FF800
      7F003E0001000000F07FFC00FFC0FFFFFF000000FFFFFF03FFFFFFFFFF000000
      00000000000000000000000000000000000000000000}
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
  object TBImageList1_24: TTBImageList
    Height = 17
    Width = 17
    Left = 12
    Top = 233
    Bitmap = {
      494C010119001B00200011001100FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000044000000770000000100200000000000707E
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008080800080808000808080008080800080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF00000000008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000000080808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000424D3E000000000000003E00000028000000440000007700000001000100
      00000000940500000000000000000000000000000000000000000000FFFFFF00
      FFFF80000000000000000000FFFF80000000000000000000FFFF800000000000
      00000000FFFF80000000000000000000741E80000000000000000000AEFE8000
      0000000000000000DF7000000000000000000000DFB680000000000000000000
      AFDA8000000000000000000077DA80000000000000000000FDDC800000000000
      00000000FE3E80000000000000000000FFFF80000000000000000000FFFF8000
      0000000000000000FFFF80000000000000000000FFFF80000000000000000000
      FFFF80000000000000000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFF
      F0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF00000007418BA0F
      5D040E8310000000AEF7577F6BBEF5DEE0000000DF776FBF77DF7BEFE0000000
      DFB76FDF77EFBBF7E0000000AFD757EF6BF7D5FB9000000077D73BED5DF7CEFB
      E0000000FDD77EEE7F75DFBAE0000000FE38FF1F7F8E3FC710000000FFFFFFFF
      FFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000
      FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFF
      F0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFF
      FFFFFFFFF0000000FFFFFFFFFFFFFFFFF00000007778BBBDDDDE2EEF10000000
      AF7757BDEBDDD5EEE0000000DF776FBDF7DDDBEFE0000000DF776FBEF7DDDBEF
      00000000AF70D7BEEBDE35EEE00000007577BABF5D5DCEAEE0000000FE777F3F
      7F9DDFCEE0000000FF78FFB83FDE3FEF10000000FFFFFFFFFFFFFFFFF0000000
      FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFF
      F0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFF
      FFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000
      77703BBC5DDFAEEF10000000AF7BD7BBABDFB5EEE0000000DF7DEFBFB7DC1BEF
      E0000000DF7EEFBFB7DDBBEFE0000000AF7F57BE6BDEB5EE10000000757F3ABF
      9D5EAEAEF0000000FE777F3BBF9F3FCF70000000FF78FFBC7FDFBFEF00000000
      FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFF
      F0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFF
      FFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000
      FFFFFFFFFFFFFFFFF0000000EEC7F763DDDE2EEFD0000000F5BBFADDEBDDD5EF
      D0000000FBBBFDFDF7DDDBEFD0000000FBBBFDE1F7DDDBEFD0000000F5C7FADD
      EBDDD5EFD0000000EEBBF75DDD5DCEAF50000000FFBBFFDDFF9DDFCF90000000
      FFC7FFE3FFDE3FEFD0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFF
      F0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFF
      FFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000
      FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000EEF7FBB1FDD8FDDB
      F0000000F5F7FD6EFEB77EBBF0000000FB83FEFEFF777F7BF0000000FBB7FEFE
      FF777F7DF0000000F5D7FD61FEB0FEBDF0000000EED7FBAFFDD7FDDEF0000000
      FFE7FFF7FFF77FFEF0000000FFF7FFF0FFF8FFF070000000FFFFFFFFFFFFFFFF
      F0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFF
      FFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000
      FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFF
      F0000000FFFFFBBBFBA0FDD8F0000000F803FD7BFD77FEB770000000F003FEFB
      FEFBFF7F70000000F003FEFBFEFDFF7F70000000F003FD7BFD7EFEBCF0000000
      F007FBABFBBEFDDF70000000FFFFFFF3FFEEFFF770000000FFFFFFFBFFF1FFF8
      F0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000FFFFFFFF
      FFFFFFFFF0000000FFFFFFFFFFFFFFFFF0000000000000000000000000000000
      00000000000000000000}
  end
  object EditCommentsImgs: TImageList
    Left = 16
    Top = 108
    Bitmap = {
      494C010109000B00200010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000063313100633131006331
      3100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006331310000000000CE63
      6300633131006331310063313100633131000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000063313100FFCECE000000
      0000CE63630063313100FFCECE00000000006331310000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000063313100FFCE
      CE0000000000CE63630063313100FFCECE000000000063313100000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000633131006331
      3100FFCECE00000000006331310063313100FFCECE0000000000633131000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000633131000000
      00006331310063313100000000000000000063313100FFCECE00000000006331
      3100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000063313100FFCE
      CE00000000006331310000000000000000000000000063313100FFCECE000000
      0000633131000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006331
      3100FFCECE00000000006331310000000000000000006331310063313100FFCE
      CE00633131000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000063313100FFCECE00000000006331310063313100CE636300CE6363006331
      3100633131000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000063313100FFCECE000000000063313100FFCECE0000000000CE63
      6300633131000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000063313100FFCECE000000000063313100FFCECE000000
      0000CE6363006331310000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000063313100633131006331310063313100FFCE
      CE0000000000CE63630063313100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006331
      3100FFCECE000000000063313100000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000633131006331310000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A0685000804820008040
      2000803810008038100070301000703010007030100070301000703010007030
      1000703010007030100070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A0685000FFFFFF00FFF8
      FF00D0D0D0002058B0000048B0000048A000004090000038800000388000C0A8
      A000E0C0B000E0B8B00070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A0705000FFFFFF00FFFF
      FF001058B0003078D0005088E0004088E0000048A0002070E0001050A0000030
      7000E0C8C000E0C0B00070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A0786000FFFFFF00FFFF
      FF002060C00080A8E0006098E0000048A0002070E0000050B0002070E0001050
      A000F0D0C000E0C8C00070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A0806000FFFFFF00FFFF
      FF00B0C0D0001058B0002060B000D0D8E000FFF8F0000050C00000409000C0B8
      C000F0D8D000F0D0C00070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B0807000FFFFFF00FFFF
      FF00F0F8F000D0D0D0006068700050585000606060000048A000B0B8C000F0E8
      E000F0E0E000F0D8D00070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B0887000FFFFFF00FFFF
      FF00C0C8C00000000000C0C0C000A0A0A0008088800050585000A098A000FFF0
      F000F0E8E000F0E0E00070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B0908000FFFFFF00FFFF
      FF004038400030303000D0D8D000C0C0C000A0A0A0008088800060686000FFF8
      F000FFF0F000F0E8E00070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0988000FFFFFF00FFFF
      FF005050500050485000B0B8B000D0D8D000C0C0C000A0A0A00070787000FFFF
      FF00FFF8F000FFF0F00070301000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0A08000FFFFFF00FFFF
      FF008080800060606000606060007070700060586000C0C0C00080888000FFFF
      FF00FFFFFF00FFF8FF0080381000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0A09000FFFFFF00FFFF
      FF00C0C8C0007070700080808000A0A0A0009090900050585000B0B0B000FFFF
      FF00FFFFFF00FFFFFF0080381000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C0A89000FFFFFF00FFFF
      FF00F0F0F000C0B8C000909090009088900080888000B0B8B000F0F0F000FFFF
      FF00FFFFFF00FFFFFF0080402000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D0A89000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0080482000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D0A89000D0A89000C0A8
      9000C0A09000C0A08000C0988000B0908000B0887000B0807000A0806000A078
      6000A0705000A0685000A0685000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00FFFF0000000000008FFF000000000000
      A0FF000000000000917F000000000000C8BF000000000000C05F000000000000
      D32F000000000000CB97000000000000E587000000000000F207000000000000
      F927000000000000FC93000000000000FE09000000000000FFE5000000000000
      FFF3000000000000FFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF8001
      FFFFFFFF8FFF8001C007C0078C038001FFFFFFFF8FFF8001F83FF807FFFF8001
      FFFFFFFFFFFF8001C007C0078FFF8001FFFFFFFF8C038001F01FF8078FFF8001
      FFFFFFFFFFFF8001C007C007FFFF8001FFFFFFFF8FFF8001F83FF8078C038001
      FFFFFFFF8FFF8001FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFE00FFFFFFFFFFFFFFFFFC007F00F81FFF83FFFFFF8C7E3FFF39FC03F
      F8C7F1FFF39FFFFFF8C7F8FFF39FC007F80FFC7FF39FFFFFF8C7FE3FF39FC03F
      F8C7FF1FF39FFFFFF8C7FF8FF39FC007F00FFF03E10FFFFFFFFFFFFFFFFFC03F
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
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
    Left = 16
    Top = 168
    PngDIB = {
      1000000089504E470D0A1A0A0000000D49484452000000180000018008060000
      00B735A26200004AFE494441547801EDBD07745555F63FFEB9AFF7E42579E9BD
      92840EA184D0514004458A208888341D1111B1E16850EC0E8A6303C636EAD8B0
      2B5610E95D12407A08253DA4979797F7DEFD7DCE8D890909C838F3FFAFF55D6B
      DEBAFBD673F63E679FB6CFDEFB9C07FC5FFF49CD19282E2E93ABAB6BA1D1A8F9
      4A8624490410C4553CF3354470F1DCF41E90F9B2F95DD3BD2CCBD0E974F0F3B3
      8B0F90F2F24AE4868646C8B297E06985507C17C80488C84D482549DC8B6FE299
      2464F15D5C65C66F4A885AAD45636303A2A2A224F5B8713765464438101AEA0F
      9BCD06BBDD07BEBEBECABD8F8F95D726B05A7FBF5AAD1608B058ACB058CCBC37
      F36AF90D9AEE1B1AEA71DB6D77666A2C16033F6871F0E07914170B16A9A052A9
      A0D5AAA026B7542A89CFBFA758AD563197E03B01E29B08DF7C2FC2C93099B4C4
      69C4F19C22481B361C90D3D3E3B07163310A0A6AD1A58B1FCACA1A505FEF81D7
      2BB22C8849E4AB4024402095485C2611F10CDE8BAB7827310E885C22911A1C3F
      E184F4DD7707E4C183E3F1EDB7F9D8B5AB081327C6A17B7707735387AA2A172A
      2B1B505DDDC8AB8B050A5682A61C884AD03A87E259A391E0F108028023B00E07
      F7D642FAE28B7DF2155774C2DAB567B06D5B3ECB408F79F33A2332D2A620ACAB
      73A1B4B40166B31ADBB71792703DF47A35D9D4946256B6DF7220884B70BB411C
      6AC4C6B9B0F18772486BD7EE96C78C49C1EBAF9FC0962D050A2B6263ADB8FBEE
      5EAC1580C1A056CAE7C89132B2B08EC835442858212BE524CA4849094F22072E
      978CC04035B9007CF27111340D0D4D55B3BADAA520080A32915021A2A38FE3CA
      2BA3B075EB790C1F1E0E7F7F03D6AD3B8DECEC52F25743C21AB85C1E8588CDA6
      63C2D4BC9758765EE53BA04555850BD2EBAF6F95A74EED8EE5CBB3F0C517B908
      0830B0A06446F6E2B9E732482C1F274F56E1A187D2101C6CC6975FE6E0DD778F
      A1BCDC8598180BD32D2965E4EFAF67CA8D8C272322428BD1A30D78F6E91C48AF
      BEBA599E39B327EEB967373EF8E004440E446B1435C8CFCF08A3518D33676A10
      1262C22DB72463DAB44E2CF0062C5DBA8DE5A2C3F4E949B8EFBEED64A7CCC4E9
      E1E363445C9C0E932659F9FE08A4E79FDF28CF9B97865B6FDD8677DE39CA549A
      5850A2B9AB582344359558ED043B44AEDC183C380C0B167445DFBEC14CD03174
      EEEC87ACACF378ECB13D4A0E2C163DFAF7F7C14D37F962FEFC039056AC584F02
      BD316BD616463842DE1A14C4A2F0341A51BF0590A35A35EBBDE8128021434231
      664C14A64C49444D4DA3C2F3397336B02DE5C1E130B3EC02B0706120AEBF7E0F
      A4679FFD5E9E3DBB17E6CCD9868F3E3A42244696012BB3D2B191C5CA211A17F8
      DE8BF8785FBCF8E26076213AE5DEE1302A218E1F2FC7C8915FA2A8A80E336746
      9385A1183B761BA4A79FFE569E35AB0759B4936DE1386B82911D9557A9E74A4C
      A5C76C4AB9243511311A25A4A4D8919AEACF961F881E3D02D0B5AB83352E1F33
      667C4DC41178F6D958E6F267484F3EB94E9E3E3D15B367EF626B3E419C064273
      8FD97CE5AB368778EFE61B9153D1ABEAD830CD484C0C269BCE60D8303FBCF146
      170C1DFA0DD4FDFB4FCA4C4B0BC4D9B3F56481844E9D7C20AA5F139895FBE868
      716D82E86813A2A32D7CEF83D8585FE51A196962D9A958BBEA58DBF4C8C8F047
      AF5E56ECD9530D75D7AED333C55820F836614214FBA2704C9820200CD75D2720
      04E3C787F23E04D75E1B826BAE09E63598D740B22210E3C60529CF93278763EA
      54113E14E9E97EF8F4D31CDC7CF320685E7CF16A69C89013F28A1547D80D34F5
      A0E438640E24E24A3E3067A2BAF2899CE180C56FE2990FAC0812CB45BC13E1C4
      55A3015BB307E1E1512C97307E155FFE0797E0C07F9D47F9B247AE64D90410CE
      79BD60915C82FC657EFAC5ED96AD6A098D5EE06DF6B2C7EA1B51DDE0458A5DF7
      9F13F87B4D83FCC8B96A1C3EEF821F07A762F64DA7CF3BA1D56B116DB55F9AC0
      8E46B7BCB1CE0515077F157373F5E9E7E063CB408329055A4F2DCAF5063C5A5E
      871F8E57A29629EE1D6A869E01034C3A64445931DFAE6F2250E4F6C825928C0A
      B6FC0FAB1A50EBF4424BE62D3C5C8182AA5A947A7DF040C05EF8956D82CB5905
      6F5834EABD2A58D432EC8D80D9A843B7402D024D2A1C3FDF883AB70CB7E4452C
      C750A590B33C6EF92F3955C82D71C2C994D4533488B46A70B8A09E3209101DE8
      87F7ED1F22FEF44768501B20F98D823B60106CA8C0863A09B3CF8553DAE08866
      D2A0ACB6115D42F4B829CC861BCC7A89E904CAD96FED3B5787BA8A7AA80C5A74
      0FD4A1AAD183087F1DFC2D368CF62B87E3CC16B835BE90E082D1B38983792EB3
      DF8863B50311E5D305FD435548E160A361D39E6FD14BDF91A5E220C78075CE06
      24DBB488E460916C3721CEC7009B4E0789DD45F6F95AF89368B0D6C481280FBE
      0E66DFFF2A9468E7235FB70463C207E3E5381953CD126EB71A24815C206E064D
      4175A3BCA4BC1615758DA080C114BAB1AFA411274BC81E924F0836C1CF3714C6
      EE4F42AADE834A27502FC75374D172CCF0C09D7F0605274FC06612024033DADF
      AF9A7FB25037E5D5E34C31D94381AA6F84093A0E8FD1BE260C0BD2C3AE95304B
      2F49B37E8B73A6A054D6A29E5D73054E9F3D8353A7723162E815080E0AFE2D44
      DB8B26AFDE0D9B51850149768452BE196CD7208175D8975D635FBD4EA904ADA3
      D4D516E1E0815F71F2D44998D4260C1C34109D1212DB856B8EC33291F170AA1F
      26699954BEFD8870A9C3111A8564498F6EBDFAB0E668E163F5B95470B2FC929F
      7FFF58BE61A75CFBDEEB3038ECF018C96FAD8C1A951925F52AD47BD847F8F9A3
      AEA40C27F61DC7C26F5E6EC991524D7F47D37427AF5B27CB13274252A9E09D36
      1DEA55AF4AC623FBA05FB38A5593030FABA2C76882C5A447989F156AFF009CF3
      BAB1FF740942A7DE097CD384479C55E2D41AE44F3F95291A409A3C19940420AD
      FB1A7266A6AC8E89849A8861D029F2B9CA6686C41911F4569C2AABA2E89F8DC6
      9BEEC1A4E7EE6949BDC0DB3E07DBB609D904484A12D316C8C9C990DE7D17AE61
      57527C5043E3E7DBC4228B095E136528123191D341556538FEC33AECDF714AEE
      DE2FA68548CB8DA02640FEF14799721F28FF815345C85F7E09E9FEFB50DDB907
      E4EBAF832EC8018FC904D96C866CA414A867871612846ACEDB8EBCFD364E7B82
      306ACB170849885770B7639134628484CF3E037EFA09F8F043C8B3E740BA63A1
      5473F4089C2603E4A04078FCEC80AF0FBC810E78533BA136311E7A42FCACD988
      3196236BFA54146767C922C1ED59C4B7525A9A429DB7C0A38F2817AF0849A4AA
      B050B025C2652381C830780D06A8EBEAE03E7D167E36131AFBF4C72F9F7D81AA
      E3279478229A72F347278F24410A09C499BC3CA8223821E9D9159EE262A8CF9F
      875C5B07A3863DE9BE6C1CDEB419DDDE7B1FF113262889BC6C0292468DDC6327
      71DECD9C7366D359B309010909709595739051A3E2C831ECDFBA03892FFC1D09
      13262AC845A2391316973F8685A347671E78FB5D842C7D0057BCF68674CB9011
      99F53BB62228321225EC3AF66FDD8E4E7F7F099DA64C69412EB0AAC4E972C0D6
      B72FE25F7D0519F7DCA720E8BA6285E4EED50707DE7D0FD9BBF72079D56A245D
      805CE055028B9B3F0BA5DF7F27AB831CB077EBD921AE0E5FFE596222DE69CA45
      ECF8E1C7873C0EBD975DC80CDFE1514281218F93140BE522963DFE4935C42921
      17B964F4F6FB0FE5A2228F47FE679D1B3F97D4E128052E87518373150D3857EE
      E4BC4D8F2EBE9720502D7BE57D14D5B6D53540ED95400D10C69D5E01937D141A
      0C31D079EA89AC149BABB5F8F17815EA19D63FC20C1DEB6590D980A19C43CFF2
      D1B39762C6CF52C0AA544928661DFF8443A8105DD40CB8AFD889C24A8ED72A3B
      96D9B76046E1DFE0148257F0ECA60E91FD5066B501DF17399164D350B253E158
      A90BF9D4D45C9568C5BF227CD83C49606B63A37C574E35CE51E4ABE7C8DF48A1
      35D4ACC1D1C27A452E4A09F5C37BA6B71076EE4BCA45ECA603C6C2EDD71736B9
      0C5FD56A31FF5C187C0C7AD838A6D770081672D1EC302BAE33E9250D91C9DF39
      3DD87BB606EE6AB2C3A8458F403DCA1ADC88A45C1460B5E31A5F8A2B6776C0AD
      B133CB2ECA45EBA1711D85E4F5E058DD5024F877C6E05009711C800CE4C41C0A
      5CEB987071A80A38B87F5FE7442A0B242AD082143F13626D4C8D4ECBA993842C
      CA45C1661D1C6A1D3C5E120A54A3D13E8E72D1AD28D42FC6E488FEF87B8C17D7
      536C5C40B9482017889B416394543853D1881AE6A2BE5106598F9D452E9C3EDF
      F01B7B34F0F18D81A1C7B3B0D7EE4745BD1A4E502E5271F0913CA83C9B8B829C
      E308B43B9A71B6B9AADEAE71620BC546216895D5BA106E5321DC578FD149FE78
      BC7F08EE4CB0638A569254A620C9E41829356ABBB1B756531B568EFD07B2B163
      EF1EF4ECD51FDDFBF46F83B8F94153C89407509B9512684788558BE1948B22C9
      9E40B6F19EBF8932CD81C5B5A6BA800AAAC338957B0A66AD99BAA4E1888B8965
      68F1B53D680C7A151EEFEC8FFE060D85600F523A40DA3A5AA07F08BA246BD0AB
      5B2F68D41AAACF38BAB50EF0FFF77DBBAC799F7B4E9638A04B73E7B6FBF66712
      D7663CF05E71852C3DF104B06811E4FBEEE3D0F56750B68DD3924AF9EAAB658A
      CAA0B202E0208E4387008EB76D835FC693560BF9EC59480F3E0869CE1C49D312
      E5E85150810D2AE92860916E78381482A23761636C0977B19BE630EC9FE4D3A7
      8173E794901AE52C4E2FBDA4888A58BF1EA8A80035DEA0AA119CD6B0C189E627
      025D06A8549004912E5D8065CBD8B5B48A23FFF0834C8DACD0C6025F7D05C9E1
      905A7DFEDFEDFF38D031072EAB96949797CB1ECA381E0EA5028D9A55312020E0
      B2E25E32507575B55C275A35B16A283DAB290988088D6E371A1B1BA1A1C6C4CC
      7ECB643489D70CD5FEE8F0434343835C505040F5B18DB37989834B154E1C3F4E
      4B4829452CD096E08FD8D8584E80FCA8CE77A1B0B0909AF7008485B5D732B623
      20524D964018DBB66DDB86F7DE7B0FE25A5D53032FD924D2A8624E2C8E00F41F
      90816BAF1E8344122B62827AF6E889F00B88B42170F4E8515A03AB6970A8C633
      CF3C438BC73A251751D1D108F0F78751CCCDC8FF3AB2A788DDC9D9A242D4324F
      A3AEB812E3475C0103FBADE1E9E96D88B4743267CF9E954F9E3C89E364C5FCF9
      F3B17FFF7E0C1932047DFAF44190C301030D759228079D1E264EA582A2A310D7
      B52B4C4141D87CE0000EE6E7218493F1C3274FE0B555AB3357AE58B14CE45612
      2701DF7EFBAD7CECD8313CF6D863601960C488114AAF5A5B5DCDA191F699254B
      20EDDB07EFD6AD14BAECA865CC46A311363F3FE642C2A69C934824DBAE8D8A86
      DD6AC3ECEBAE83BFCD475272B061C30699ECC19A356B909B9B4BEB465F2ABB0D
      A82272617ED45277E4ADAC845C5404AFB0795A2C3032B5348DE03C5367652E74
      8E401CAEAB8745AF4710596824F1B7D6FC6399C454CBA210B76FDF8E55AB56D1
      FE1247334BB052C8611C136A6A6B29807961241131947A7D7CE0E2FCB842A386
      2520001EB2EEB4CB0535BF9FA9AF87BBB61E0B7D7D29A8A9307F1AD51037DE78
      63661153266A8B8B01ED763BDC4C41706020AA388B3471266FF3B5C3A9922073
      CADAA8A7268C48ED9CD594B11D0836B9C9880AC6319BCC2866413B285226F09B
      0FE3D150B7563E75EA141E7EF86138982235873C03AB6124070C0F6791FEF9F9
      68A8ADE37C580F37B3AFB2FBC26DB5401B1408A7D982F34E27D43D7AE29C460B
      4B563672DC2E7425ABEE080956669FEA19336664FEFAEBAFB4976D41006B8BE8
      0E043BDC4CAD3925051E0E7F8DAE06D898ED94CE9D51A7D612D4A816C448A08A
      7A3D8F7F00AAD52AA8CE97A292A282B6D185110C6FE77735AB64E6DEBD7B2140
      2120B24A1648E4A79639A3B1001E958A2DD58184D0103432A5061D6B0F85611B
      25598B4A03DFD20A98F38AE0A735C244858F9FD38501FE3E9CF9FB402262F9ED
      B7DFC6CA952B698349549ABE0F0B524B5659C97FD1B1496499C946BD1035BD2E
      3117E348EB22AF756613CED7D006CDDC9EAFAD81996DA488837D346BD863B7DF
      8EC69A1A680CFCC83E046A362251C82A467492AFA29A55B2B5D26C4E29A60E35
      15955069A993606EDC6C03FE010E9C3B7D062E6ABB6A644AD924246A570167FC
      7DAEB802FEE4828A09D4B8D85B76266F03D860AA483128341455948744FD1744
      4A4B4B59DD2D5093B7B2DB03B58A2A7BD6A862B6170D45158950CA7EC8876DA3
      AAA484AACF6A0C4FEB0D0B7B59D19F696CFC6065D51C3C70203EF8E61B384848
      57550591FA1012AB21D15AB605414C18A54587E76C688087884DEC9BCA4B4BA0
      628E65122D3A719266DEEE183B722465B632A54D69E29293A5A3C78EC9B366CF
      C6FA9F7FC6597607DD525250CCD4882EDAC1F6205AB2E83E8411950D57E9C2BD
      AC0CB9AC043A965508C31C3C741006B2F9EE4577B283F4417E5E8190BC9B243B
      8FDB8D082A35162D5C8847282C1D3C748856A4706A77D54CC9799A1F855D5FAB
      5CC5C826887939BA093608A2A29ABB1A1A71CFBDF760F8B0E1B439672399AA38
      91189538A5A4A4481C073072D4282C66A7A662411E3E7C9886D01A4EA885114E
      58078590D764DC16EC1220C60DD1FB8A8AB264C9DD9876C30DC8CACE427474B4
      E86E24815B234E02FAF6ED2BEDDCB9531E3D7AB4F848CBEC07D8477689914D20
      1005260837E740E4420C9743870EA5ED7892C2FBD36C949DBB7446546494825C
      E0557A537123E01FFFF8C7B2D9B367670A843D7BF6A4D9B193E0230DA85A855D
      625C166D44A430232303D75D771D2DDFA39521548CD1224E7858780B72815361
      91B86986010306480909090AE2949454229980791C8016B27C16DDB58876E23B
      700B2BC4B871E39404E8D96588B62208060707B7412E70B623205E76EAD449BA
      F2CA2BA5EE3DBA23A9530CC24203A95D0FE4359C1DA283EA7E2BEDF6A1E89DD6
      0B03070EA249B1AB44D79476C805AE0E5F8A0F1DC1E6CD9BD9DC40A4032F3BDE
      45035E88ECAD37D6C8697D072874776CDB8459B3E75F34AE12E8B7539B42FEED
      1D0472516BE2E2E2D039252973F1DDF7645AA8C64F4B4B83835D7A7945153AA7
      A666DE77FFFD9993274FCEFCD7BFFEB5AC39EE85D70ECB20E7C411459A08640B
      EDD6A33736ACFF91F114EEF00A6538EDC486346CD8309AD5C7E0B57FACF9FDA3
      12E2F753870454B25B91E644B0200EE806AAF26B39AA896701A22D889C887B01
      16F69CE2DA11B434B4D61FFB0FBA82BE1259A23039D0044054C57A0E40ADC388
      2EA2F9D9EB9569C37F8343B68156F1A952F37B716DF3205E34C35B6FBD25CF98
      3143797CF3CD3715C962D6AC59CAF3C68D1B1536A5A7A7738EE8A10EFD437A27
      74513CAB3EFFE403DCBEF0EE16BC1DE6406069A82D53845A361EC4C4C4E0C891
      23E2B502A2D58AAE5B3C88414ADC8B562E722A712815EF9BE1A204E6DEB648DA
      DCA5B72C088856DADC3B8A88E29D40287A54414CD438F15E940D870971DB02AA
      E6BBCF3EFB4CCECECE96291FB5D488B3674E2B9FC5BC40D428F12090B2F755C4
      773110B9D9D50B5609C733414084690D927810F53E222242B8AD8947BAF87C89
      73270F40D69871E34DB33827B72AEF057291E2E6ABB81748058877A2E03F59FB
      21162E5AACE015915A58949393D34260ECD8B1D8B8D18ADDBB77239F82575252
      92529822B502A9B83683402EEE053241C0E5AC15B72DD04249E4821D16BA75EB
      D6F2518C05E29D185C5A236EBE17A916209EC558210AFCE37757E38147FED682
      B7A50C060E1C286DFEE97B7A439D6C21101212A2B40111B1190432916281585C
      9B9F85005049095C434B790B02DEB4B088F758B0E81EE945B54E1E3F6112152F
      A1CA3C41B0A01989B81748C5B519C4B310D2440EF6EFDD055BC4EF1C10385BB2
      221E9A61D5ABAFCA578D19A3746CA2053713684E75F35510117CE7945629ABCF
      DF5B83879F7AA90DCE36396826C0118CBE5FAFCA2329DF08014CA4B235528158
      80782FCA48A43E3B6B3F82623A37A368B9B6A1D6F2F6B71BCAACB218D4C5A3C8
      456B22E259B45CD1E8CE9C3983CF3E78130F3DB6A21DBE964216482E0431B80B
      B1443434F14DB0438020245AAF608DC8C921CA5181110922483B6847F1C210AF
      BEFA8ADCA347CF368D4D10114445F988F6F3EBFE9D78F8B1673BC4D5E1CB0B89
      BCF4EC32B9C14B65794B274293AFEC85ABBE169EFA723CF4F4AB9785E742BCFF
      7B5638D0A15421BED4D4BAE4E1A32764DAC3FB67F61C302973C0D089997D075F
      973960F8D4CC4123A62ACFFD068DCB7C74C5B399FF5CFDC23211A723E890C0AE
      5F4EC92FFEE333E49E2D40526C1032FA77C255A3FA63F8E09E18949E823EBD12
      111162E6E4BC11B94772D1ABFFA8CC45772DCEFCE0BD37DA116A4760F5DBEBE5
      773FFA0EA3AF1C80E9138662409F1484D2794623B920D3B4A5E3148ADE3D080D
      F243D7D43874EB9E0C3F7F3BB6ED3E84C937CCCBFCFAB377DA10915A676BD5DB
      3FC91B36EDC0D22533901C178AF365F57035BA2173E62EAE060ABA3A8D8ACFF4
      B19654F4EA74434C1A2D360B9CAE1ABCFAFA57080CB0E2D699635BF0B6DC7CFA
      DD7EF9E57F7C8EC71F9A83F8E8409C3B53809ABA7A3450BD40A94451A13902E8
      3D121C082F87C946B7F08714C9A3972CE76C3A4E7125B506CFFCED0D0C1AD81D
      D75F3340C1AD9CF20ACAE4850FBC42396818460FEF8163470E4268B85C1EBA3E
      7396A5D6EA94D426440523D0E1A7DC5B2D4CB5D389DADA7AE8E80A51EF74D3CF
      C28CA3A7CAB0E68D77F0E45F6F40A7C418C6662236EF3881CA5A0322C3433811
      DC81EAAA0A46D243A531D1970270D657C2E16F53663E05796751EF6CE0A4448D
      DA7A27FCFD0220F41B1E66B39C73693FBB1FE3FAE0BB9FF6113320D5D63AE5B9
      77FF13954E1D521203A151D5414BF5805A67A2DD8C61641762E9BADEAF671268
      D853F4455E2FCD929C85EAF5463A8B19947272D1B0E7E5A4BCAA4E852D3B0F63
      D79E5D78EDF97950F71D342173C7FE5CEC3F741A8505F908E630594F1FAFDA7A
      2FCE9757213A2200578FECC789B887DA394E02394154AB2458388B174EF4624C
      103972377A981A2D2A2AEB5151D588B3F9A5F409A6E662DB9E63A8A5C5352ACC
      814307F7D229D88CD0B07022A3530C05DE53B985A8AAA943107DE6844D56C8A1
      F5B4177BDD8DD42BB9396706F51B6E949494C360F2C5B193A761B20621342402
      BF641D8326BFA406E17467E8D3AB270A07A4A2A0B814A74EE7C1E5966032FB60
      77F61934ACFA1C7F993D926E2D6AE8345A9858A875344F4ACC899BC655356B8F
      C9E2839C3325D8B1271B5AB33F54D0C3E5AC8174FBA35FC93AEA204249C44C55
      5928E7631B376FC5E7DF6C82D92F022AEA871AEA6B58BF6D54779663CAD8DE58
      38EF6ACAAD952C7C0F73E7645596719E7EF21F7CFA238EE7E4B35CACAC2054F9
      E8AB21CDBEF30DD96CF145645828352E327C8C1A44444760EF8113C87CFA35D8
      ECA1D0E8F42C70FA135557A27B82196FBF7A372B820AE74BABD9C0DC70BAF578
      61F5C7F8F4F3EFD093D3AC94D49E58BF692BDDE7E8977AFF939FC955F4D08F8F
      8EA526C5033F3A665BAD16BCF6EEC7F871DB31E84C016093E23C5943E98E9A2F
      433D3E58F320C21D7A141655D2BE63C48E5F4E63C95F5720243C9C5A604AE2C7
      4E21BFB4126387C6435AFDEE66797F560E85D914EA221A11464F9BADBB7FC50B
      2FBD43579F1848EA265775157B2D2F596950D7E0B5954BD0A773084ACB6BB0EF
      D742BCFAC6E7D8BF6D2FFC837C388F9E078F5B85F73FFD0A3326F581FAE5179E
      C8FC7CDD4E040547414FABF6EA373FC2E183BB593D7DE167F1C26E74C2617122
      C0E444B0CD039DB71C5A4F19B3AF4351C119ECDDB30339C7B2A83E936972AF41
      F79460D84D8D283D7710D78D88864A34FD1E5D229157508CA3C773B173EB415C
      3FB0112B6F93B0645C19964FABC6B22995B86F5C21EE1A7D06CBA7D7A1ABE57B
      647F7F2FF2762C43BCFC316E1F968F7BC616E3B651E5B016BE00EFB14C8C49DC
      8DBCAD8F43A5A7ABDBC8613D7126F7084A2BEAE1C7B268F068D1B96F778C9C34
      1CC70B2524774FC520F651E983BBA257DF248CBC321915EE00A4A5C561504634
      7A74F5672F6041EFCE4614D7FB2025DE8ADEC91A98D85F2972518FAE71D288A1
      9DA984CA47001DF22449A5740BC28F3D2CC8C882D7C09F2679071B9BAF45055F
      AB0A0E5F15AC462F7CCC648D95B5CF24C3D74CDB82859D9EDECD7B0F6B183B4B
      B66FE598726D06BAC49A907F26873D1475A0EC129CD50DB861524FD858B35C8D
      5EAAD1A0F4FFB5F532C65F19C286486F8B46B096D1FAC744391B258CECE58651
      CFB52074171563458B6CEAF0374B65E595729DF7632AB8774292399890746545
      15CF34A934D4D140E186D7E584DB4557C43AF64B5EAE9090658E0FF49364D72D
      24BE5A2729CA5EB8555C62C0442A2C2206E5F0B3FB48CB164F46FA8041F8E0D3
      E354966FC39BEF1CC2BB1F9EC6079F97E2C32FCBF1D137B5F8F8072F3EFADE8D
      37BF70E2E50F4AF0E6571E7CBD3710EB76DBF07D5608BECF0EC157FB4250EC0C
      8632E028D82F38EDCBCE950F1C3A8AEC4385F4E2146B0FB4CA1820C94D0B1FEA
      1B6A515B5D0A03BD65AFB92A1D575FD1B3435C6D72D09A46CFAED1D24D53474A
      77DF710D6E9B351C7D7B042226D44D69C2434943667D3763D9FD53B1E6B9DBA5
      8B2117F82E4A407C141012E82B597D4C18333A1D37DF3411B7CCBC1693268EC1
      55A387B303F415412E091D66AB3946ADCB2DFFCCEE5AA596101362A7DBA88EDD
      B817251575C8395B86F2B20A8CEC1387E870C745F15C34072EB757DEB8ED38C2
      4C6A8C480C461CFD7983B540984E42345D51FA46F9233222046B3EDF8F1D59A7
      E5E6445D78ED9072556D83BCF6175A958E7E0F47C339688A5955B98C4C6A7402
      750D701A8C28A9AD42BFCD07F0C3CC67F1E1A15C2C9DD617BDBBC6B4C3D7610E
      3EFAFE202A6A88C85E078B6F34B44B96C23CE356D86E5D02D39D0FC0326311AA
      26CFC0FA9E9170E9D43093E013ABD7636FF6C97639694720AFB0522EA24B55AC
      C3073A186032FA20421B083F952FAC1E1B2C1E0BECB21F2CF4FCD5507C29AFAC
      43556D2D720A9DF87AFD2F1772A87D3B38967B5E5EF5DE2E74A3C06B39F31106
      6843601C3F05C5A5E5A872D1A39C49925D1A6457D28B61ED2B381934035FED3F
      86B8C00044FA4A78FAC1EBDBB0A9A5AB68214D6343B8C3C481BF1CA1D54E5418
      3D385140C1B7B21A55146542024CB01BE8D95C25C187BA2129DC8E2BF53108F7
      A300BC656F0B9AE61BA6A7F9B6E99A18132859E9E9E4ABD3E050B113DB0AAAB1
      ED68317D9028A2188998F2906FA015FEA1018ADD20D0CF8884083B9C4E97D249
      3661F9FDDC2E07DF6E3C28FF6DF54EF8244663F8D93388CC3D8D025718EA750D
      E8D92D1ABE652A346AAD30541C46AE836554550F8FE4E6C2122797C804FC8EF9
      B7BB36FC3A90BD4F7E6CE5C7286D88479D4687FEBADD08D2D296E9A4EDC66C42
      97283F54D3BF57E7EB83E28642243684A238792CEACD124E1C2B45B0DD43F969
      7C1B9C2D2CDA41D5FE5B6FBD83C4701D5BAC13413A60A737055F7807E21BBF71
      48987E2F7C272D8269CABDF00E9B0555FA5D3832783ACAD55EECFAE50C4E9CCE
      6F875C64A285DADAB51FC92FBDF42AA22343A0B5C6213471304E71D9E399822A
      D0B88101BD222908D8111668A1BBB4571178AB58F0B5AE0645B21896164E2962
      580B3E815C404B0E8422CAEB7153682DC4B4894310E1EF46E7E4600485DA60F0
      D7E3A7C367B1E2939DD87624175E350561A6FC5441257665E761485A346E9C38
      143939A76481B435B4104848489456AF59854E499D581B24440478A1F2942AA3
      98C56440343BBB84505FC894590D1A034E9FA9A4034E09AE1E18832957F7C2AF
      BF1EC6860DEB3948AD6A43A48580A09A94D4495ABC78318E1D3B0E2DC5C59E5C
      78754DBA055D431B11A4AF4398D983C6EA721CF9F5186725C598714D120632F5
      5BB66EC377DF7DA318948A69C17D38F3E116226D08082231313192D0D1E5E6E6
      C268F641625C18AE1A18854923C271FD15E1B8B2AF3FFA74F5C375A37BC0873D
      ECF73F6CA4AFD3210A6EC12C3B2DB6676FC7E11387F1E1071F2A44DA111044A6
      4F9F2E09355931EDCC169ACFC3C363E8489EC2856D3D90913180F53D1459078F
      60D3E62D9459B934239562FFB9027CF0EB07C8E99A83F88478982D946188AC5D
      A9F35DCBF1EEBBEFCAC2802DB4BE42372449127DDC77D372BB479908C6C7C753
      7431E1C71F7F5454A07E3DFD90DC27196986344CFCCD73F9920404A537DF7C53
      168884F5898E05387DFAB4A2911786A18A8A0A6CDAB48966C94A74EEDA19B151
      B1149EC33069EAA416BC1DB248206E86EEDDBB7365DC1EC5AE260A5028A2844A
      79DDBA758A538760655A9F34C4C6C4E2CEBBEE945A2317385A2889878B015329
      DF73CF3D14F1639520221779F420EF42B3BCD0690B16DE7DF7EF2A7D25D06FA7
      CB2220C2AE58B142FEFBDFFF4E273623E7112AC5D22A6C3C42313E71E2C48BE2
      5189C8970377DD7597442B21D77D6814B3224D935C663C8F6E7917477E3978FF
      E33017CDDA1F617EECF14F64B1A8CA64B6D0FA6DA1E38CCC1C65B4C3A7FE2344
      177E7FF6B96FE531331766260EE9819481A9884B4B806F6C302706FE1834685A
      E6C8C11332BFFDE69FCB9AE35D761988086BDEDA228F199F86DBA7F4C680643F
      986C7A345044D7737212936C43CA88CED0720ABBECC96F956E42C46937648A97
      1DC19A7776CABDD3E3D189A39AF87EB28432186F4A2AD8EFB940C336101008F4
      48B3235B9D8665CB3F931F7EF05AE9B20958E97B1D19435D9197BA0902053CC5
      05C8682672023382DA2ACE807C81D41E7EA82B8A23794EA395F31F9CDE59BB5F
      0E48A2B18253A973449255427FCB06FA5BDA00A3812BEACCBCB73611F2708223
      161A416D425240F7CCCB2A833AEA824CC17494A9E7E23822801AF0F5235212F0
      25D8B844990BF020C0D7C25C485C61E34F2B3A7BD4CB2220D3034167D4A0A406
      C83F4F041408CC4CB5960C6606C05B90064CE484B8E7676A22B5305A0DB82C02
      625ADBE874C340F19D6E2ED43A822D1A945C9B4020B5123913AF106130785C62
      12E9B93C026AEA83AA8AEBA988056D984D406F1F25323341CD10CB810404213D
      AF22D5EE7A176ACED72861F8EAD2C72D53BB4BC527CB41559E521D4568B25991
      9C9B2BBC48B56097C88960D5F9B3E761A32F378B4B04FF6318387846A6967269
      202532D652EA96400F2870E103CBE4B7E822F5026155792D367D9385C50BAFE2
      18F8DBC74B5DAA763D2FBFB32F0555EA00240F4E4470B4595918AA663644AA45
      A10AC4AC58A825F21F7F3E8C69E3D3F8154A2E2F8ABB21EB356EF751AD7C7F67
      9F9DDA4423F401B170C4062084CEF77E5C9A6AA6142E0AB4A1B6012567CEA3E0
      5811665EDF57412E228A3212D776E0DCB75AD6957C0F5D752D769FA00E4EDB13
      2A6A7917DF33597AE5F5CDF2F6AC93146BE80A67D6B35C8C14C8BC9839A1670B
      E26684ED5E880F0AF28A9FD95CCB907BA80A59C521A84FBA0A37DC3E4B12DFFF
      1D1035AA4D78E7DE55B2BE6A13A4CA729C3A548D5F4A2251E93F109690AE6DC2
      5DEE83289B96B0CEBD2FC9BAEA6D40D979E4FE5A8DFDC511A80A48872DB91FC6
      4F6A2AB496C09779D35206CE7DAFCABAAAAD90CACFE394405E1481EA807EB026
      F6C784EB7BFFDBAC69A6AFB0A8613FD9A2A4BC943CAFC6BEA22805B939311D13
      A7FE79E48288C2A2C1438764C6BA0EE1F461813C12B501F4D68CED8BC9D37EAF
      6E22F09F018545E572289EDB4E754D831F6A02FAC018DF8FC8FBFC69B6B44E88
      C2A2E8383A08270FC651DF44583BA5FFD7900B427F98CAD28A1A7953F6196A1A
      F588090FA2F296BB999455E26C692DCD6025880DE44A96F4D48BE25158242875
      04256575F2A62345E8D9258E6B73744AD72B30F91A1CD444DAE90B1C88F5544B
      AFF970BB3C67727FF1A91D9A0E5F8A50C54595F227C70BD1EFC7378170BAE032
      C5424DE9B651974D9DB5B3D18B6AD989CEB91A2C1D7423226A0BF1E082ABDAE1
      6BF702FC9557D5CAAB3FCE863D95EBC577BF8380FEE3200BED8BCCE02A49B17C
      94BBB438B9FD43C4ED2EC3F6E17FC1C66FB76074BF082C9C35928188E4B74329
      E4DFEE5B2E3BB2F2B8704D4365B8055A2A40A2FD121115D205610E5A05FD9211
      4808F74B81C6C70FC566232A69EB116EB9EF7F7B007BB38E378F410ABE36D494
      373CBDFB65967C2AB70A49830311FAC54A749F763FEA4382388BE4A49C063AAD
      24A3B2418B43D96F41BFAB10DF5A875195701C0EBA962E9CDE072306756DC1DB
      6121AB240FF45A15B27F2D40089709E795D22027D570AA54231C68116A37D11C
      A68644712642A7A3AAD30709F628DA6C34D8B8691793F8FBD1218190008EAC54
      3AED395F88BD2739B6DAF3E08DF028EBF5CD5C1067A220E443A93AF757AE01D4
      D7A357A7204A722E641DAD80DE2306D4DF09B464E5F757C093AF6C90B3B34AA1
      4F35E3FA6F9FC1B1A45B504316A5865AD12D350C1CCB2149466C3AF62E74DC76
      A8B2C754C875C5D8B2EB2CAE1A9C80F1A3FBB6E06DB96926F0F9E75FC8CFBF7B
      885A76BA9E0449B83DE79F28088A848E262B1FAA918329AA7BAB69BE923D38EB
      D380EEE509D837FC5A989DD5F879EB11CC9B3E80F684A416BC4A67D78C7CEDC7
      9FC859FBB6B3B5729B20B51F87C85AEC88EE86A3EA5078BAF645DF9913D198D8
      15E5F1DD5195D40395D66EA8EED40365A7F2B03DEB1C22822C98327E500B7281
      B7CDC39CB973E506AAE9852062091B048D2108478F14A2823B384406FBE2EAE1
      9DB85F05C7608A91F50D0D349AB2F0EB6A515459C3D575E7B070467F0CE8FB7B
      0D1204DA14720FCE891F7BFC71CC987E03D207C5E0643E2D1CD670EC3E9C8F73
      34D63DB5761BEC362D6E1E934A3D850E0D34A41E3C598CCAEA3ADC76E360F4EF
      1D8773797972EB450E6D1ADA6DB7DD264D9F368D36490B82FC0D083457B1EED3
      139936AE003F33A283AD5C492DA1C1E96141EB71F0D722949FAFC464DA7406F4
      8CC6BEFDD9F866DDD762C5454B636B434064E9A9A79E92860D1D4A437403BAA4
      C660FAD8448C1F148A6E5134FE84FB23353288BEB92A54D3A0941861C1D2BF0C
      C7503A12ECDC45EFF33DBB51565E8E6FBEFD065F7FFDB542A44D197CF2C9275C
      4A76969AAB1CC249848486518A663DF034D2624B7E53C26E6427271262356B11
      11CEC91F34389193C34658C139832F9D932370FCD4713A1A84E29EC5F788A984
      08DE04DBB76FCF6C560F74EA94AC38CCF8D8B8B79A9F3F57A884232E369A1AF7
      500571707008AC365FAA7334F4A60AA1BA2709C768DEFD20FF0344F688448A35
      05C9A9C94A17DF849D67E1B0D7BF7F7FE1018BABAFBE5AF1EF123A0A512EC249
      898EF8DC28AC048B17DFCDDD9646623DD538D3A74FC3BDF7DECB557E8BB80BD0
      B5748708C5D05E7497E83700498949529B323871E204860D1BA6AC45E8D7AF1F
      37A35AC0BE47660A43495E4C3A3458BEFC51C5F74BF8050BB7F5701A48ABAAAA
      68B4E67A9C8A42A494A720C39181E123872BEC6F534D23E9DCCDF520DC97682C
      3D14062A4E92C283D9CF8FDD3275715BB66C012B01EEBFFF7E45C523565E9C3C
      7952518C881408A7A61EBD7BD08A95AE2017EF5A6EC4037543F2A38F3EAAAC10
      321A8D2CE81C16B286D62731BD007755EAA52016AE88252525CACA22A111133A
      24A1377AF6D9673164C810CC6DB5E4B24D0E846A86EEB58ABA4CB8560922B22C
      D38ECCB9964A859933672A4B0584A380D080091F2FE1D81A1D1DCDCE4F6281AB
      DB201789FEBF0FD21F65E1E0F173F211EED4A7E770A8A13AB9BC92FE2E747B48
      EFE48FA4B8F03F8CAFBE14819FF79F964B38CDEB16EB4052B43F2202ED08F4B7
      B1BE6AB1FB4831162CB937F39DD52B975D0A8774B18F1B77D1A29418858C979E
      466D5404A49222A8CC66D47AD59C9C5480CB47F0D3DE7C54DD7A1B160D8DBD28
      1E5547043EFEF190EC8D8B41C62B4F23ABF434760EEC85BD83D2B0AB6F371CE8
      9B8A9D037A61DFA80C0C33E4A2FC891790F9C97EA563EB08573B023F6E3D22FF
      52A942F2B38FE097C21C9857BC8241C149E8DD2D03BD93FAA05B421AA2427BC0
      5A1F8CF57FC9C4CC407A8E3CFB12FEF6F18E0E89B469072205D9E7E93CBFF52B
      E4A304BECFBF8A883A17CA5C8D7036BAD91E6408C7A53A8E7A5EB901E547AAF1
      FA750B70E367ABF0CA1B1F8AE8EDA01D819A9A52A8627508EBB700AE4A0F4E95
      57C3C5C65643DD8345CFBD04F55CAAC46E5BE214DB6155E3C0990A1CEF330035
      EB0A71BCA0444E0871B4298F76B568C2ECF999DA9CA3387C1A38C5BEBEA2AC86
      4EDD349F6BB8D0841A9040EE0AA497403B8F163A5E9D6A37526ACEE2641ECDEC
      7424F8ECC37796B5CE46BB1CA854063858478AB7EFC47EBF482473F550C17903
      92A9AB8B0CB471B874C1439D455D0D4D8CEC8CFD6243219FAAA7ADDF05ABD04E
      B5C6CEFB3604724E17C87F79F05BECEED41B0FFAFF0BAAAF3FC0C6C1D7A14743
      0DAC2921F4C8A1C9CB87FE75DF7F0350DDE74B56C935F9A8D3DB20AB1A3034A3
      3751B63D5A089C3B77565EB1F215B8118213BFA8F070C65C2CCB5E0DCF8F6BE1
      BFFC3EF8E99D28F70D84FFD2BB91EF0FD4C4C482FB03C0D7C0C55656BA6FC569
      61D70BA6B525D0524D57AEFC3BFCE880E13055D2C2A147F1DE3C2CEF35175787
      9D8676E54A7C5DA247C0FD77A1D48F12DD4D8BE0EA7B15F292696333F5C263D9
      BEE83177263AFAB5E460EFDEBD5C404233AECD81A8F8EEA8AE3561DF9653B8B3
      F3543C70FC13A43E721B8A7B86E2E8F50B61CE2945396B55554D3DAA783F209C
      5D498050A8B527D192838C8C747A96EDC5D5570D6563322034D08C944EC1C83F
      50899F26DE08CFC424ECB86A1AA4E3C5D073CB8322DAF5771C3887601A4EEFBA
      6D0CBD43809339EC5E2EA021B57E7EE4914764DAD31011168CDC7C9A4FB2AA51
      54AB818DF5DD6D37C356558D61FD9251C31AB465E711F4E8148069D7A5432CD0
      DDF8F3CF74072AC0E02143C0B5222D785B6E9A09BDF1C61B725070304C063D44
      8B3DCA99CED9E206D455BBD98BAA101B6683466A447C4C00BA53303B975FC861
      742FCDBDF51410A8F6CCCFE73E83E3307C78D3A0AF6A46DC7C1572D1A18307A9
      93D329C6A08CDED19874650CA68D8BC1E411E148A703C7B851DD1113E1C0966D
      BBB073E72E65A9926F901DBB8EEEC2D17347F96E67333AB423D0BB776F49882C
      274E1CE7BE1424E208464C4C129253BAA24FBFFE8A1DE7F8F15358BF61236A6A
      AB21122458F4D9C6CFB02B7E17E27AC5212438A48540BBAE427C219B9651E0CA
      3CC89C84040753C2A311822B550E1C384893E277C8CDCD65550E46283DDB280D
      E28B2FBEA0A8AFC1E06E83B9574B17DC32F39616D6B7DC08C417020D70B25873
      D6B9736770F980B2B245B8A20B2942481DDBB66DA3C1F49CB21A323925990B0D
      FD71D3EC9BDAE06C6907172217CF5CE12B7141AEFCFEFBEFD389AF9ACA3F134D
      EBF5423C57EC97A1A1A1A2C6D07DD107772DBAAB0D62115FC0250988006235C5
      82050B14214B2CE3E03A66083B9AB817E64721B3FEF5AF7FED10B9887FD10FE2
      6333646565C93366CCE0B6AFE550ABD54A418B35E2428E9D3A75EA65E168C6F5
      BF6B3B0EA8DBBDF90F5F7C5CEF9287DCBF3473F4BD4B33A7FDF5A1CCFF98C0C7
      752E3962D17D999DEFB82FB3DFE207323FE52E361F9DACC0D7050D88F0D3E292
      35E00C77583AECF1C22C7B51ABD1A1EBAA9711D4A31B5C3EBED0B95C38C1F9DB
      720B3D9D7F2D455E792392424D28A79F9DAB01E81F69C58331168A0DAD589247
      87DECF399050C6E5D0092CCBE78ECFE7AB51A2B362D2AF3F21F5AE3B513DFE5A
      6866CE44235753FBB1CF290DF341B957853E9166C4FBE970AA4283DC8A46D8CD
      DC46502F065512D8EA6A945F2F73A2B0CA8D33352E8E561E049BD5D873A69656
      3917021223F1C97BCBD1F9E927E1A46FB675DE7CA8D2D260AAAFC7DBBE11B85B
      0AE62E951AC5229247E7583FBB1673127CB1D46EA26840026A8A1FEF1FAB44ED
      F97AA5CF0FF3D5228FAECD36A384B088400CAD3F87D08D3FC26D35C2535C02F7
      E62D30923DDEEA2AE405330D71A1E8E4A7E6E63E7A18A32DB89139E9AB6B1200
      94EEFACB2A272C2C0D8B558F24870543C2AD88F6E1B0C9AD2C8B284D77AE2942
      C0DE7D687436C0273D1DF2F55350DAAF17CE8E1C89D4A1BDF0B76EBEB835D68E
      D7C26CD28BC116A91939D30E4D19F9BE28BF0AB1162DCA75D4127864ECE7A691
      87A84603F57301B60A1806F587F5ADB7206FD90CF7A891A862EFEAE12451B848
      F774D6E344D616CEA9AD025F3BD06CA6DE6DDBB97ACA95E4376474A5FEC12D4B
      88135B3DB19A75F2D5219D72B37AEA34E691E224DD4A341E0F2437CBEB6C2E0E
      641F447C5C0237621026A276F8212D29A991FF71887EBCECC412EC3A0C0CD4C1
      AAD3228A4AC1E9DC6CEAC228274F1D97F30AF271F8E811B829D975EDD2150333
      062AC42F0C2B9E551E96C29D5DFCF0665F0716C4D9308B295FEA6B943A422E22
      08959FF0410D0D0C457262B2927AF1FE62A0B98105DB5BA7BD680A2E8C981093
      D061D8976B9C72B2468DA106ADB4BBA1514E631B107135FF0E7211A12358C325
      066A7E10C879C17EB7475C14D028E73F7962AB974FB0C083A80518676CAAF7CB
      ABEB959C08946F93F09F2270965BCC9D631FA522B3AE119B5F0B6C84E5D57572
      06B72ECDF88D3D7C75E9CE4E04B85C789C29EF47C7CA61FAA69C7CC96E3B8EB3
      A23F95836C3ABA6691CF4649525228E670E16A09037F43FE0D911FE0F7B1641B
      3379B969BCBC70DF381BE4EC462FEEB5B223631451F8BCFC67471ECBE4FAA57F
      CDEC7ADFD2CC6A36943B7F432EB0FEE91C6C70BAE4522E8DD1904D6CABA8E354
      B7370B3851ABF9D3384582FEEF82964937534CB40910F704F18E974B1F827D97
      0A21F819A0D7E98638020216515C7C5A00F5748BA9AF1BCE880EC225715CEA23
      CD66520C57B9DF35EE9A6B9E7FE28927EE5AB37AF5744ADBD39F7AF2C945D75E
      73CD73FCB684BE777197227251024C3A572D85DC7EFB5FFE32F3C9279E48BC61
      DA349F011919A601030698A64C9D6A23C1F83B162CB8312C34F40E1209231146
      E1F982E362040C26B3F94AA672FC4D37DDE4888E895153C5295137AA80B88F8A
      8E56CFE0B70913268C339BCDA388574F6877744840142455C65732B283D9E0D6
      2DA25B537A85660412C34864916AFC75D70550637C259F7D9A3FB6BE764480FB
      C0A8ADD151513171F1F15A311F681DA1F5BD86C36C5C6CAC36263A3A92E1C4A8
      DF8E4D1D12204FD55C31CD8AA2E76DBB38BFD3602BD6E969D33199740CA8E687
      7681C54BBE6F7B30B08DCB82AF1E356A542897C7738362626A1B4479126A676A
      E03D6BF92722A772733FE61A9072E543AB533B8AE21B09F893454F3DBF72E5F5
      5C046D6256443801E27333C82E6EC7B57EFDFABA0577DCF10957F92E26C1D2E6
      8FCDD70B2335BFD751AD7FF5F86BAF5D9EB96C593C7D1AC96E76F8CD5F79656A
      E5533939EE65CB96E57CB476ED43DC6FE733BE7611DA1C17232002F9F337F7E6
      9933E72D5CB890FBF7845184556A137512B25C5050E0FDFB0B2FE4FFE3B5D7D6
      904DAF3042BBD4F31D2E45809C92C242434216D1D5F3A659B36671732B9B129E
      FEA6F23B6FBF53F9F8138FFFEBDCB9734F933567898CF21FCF171C4A840BDEB5
      7EE4A663EACE34E03DF3DC73CF0DECD7BF3FF70293909D95E562AE766FDFB1E3
      2EDA12F631C2EF720A1F5A1F7F44408435728F9C1BE6CD9BF7C8034B9706B335
      F36F3A5E2C7BFAE9A79FA24BEEAB0C5043F88F0ED1F012D27AF7FE71EF9E3DCE
      23478EB8A862D8ADD1687A106B47ED88AFFFFD835B76052C5BF5EAAB156B3FFA
      A83A3222E2551650C7E2F4BF8F5B89A165073766CE2DB79CBE67C992428BC572
      33DF76D8B9F1FD9F3A54E47D67DA38F75C357AF4AF34670D201635E1BF768872
      084B8C8FFF3A2921E127766C71C42C11FEAB87830BA0DF277C4AACC184FFFAE1
      4B8CA2C5BEC12B95CB3CFF970F03F15D4F984E3012FE3F3904917F0BF91F3514
      517B0C1C73FD382E84538516420816F77C17C09A25085E12C7453FB22149AC8E
      6A8E0546A3C110E0EBE313C53D44E205D0401ACDEEDC5F7C23918B0E4882871D
      1210C819512B90D079BECB882BAEE845D7F4C465999991ECFF2317DF7557C295
      7CC76FDDD800FD4558114720BC103A24C0082AA6CE46A92199D6F0E83B172EB4
      5C73EDB59AF40103D4B4D8AAC78E1BA7B9F3CE3BCDDCCD2F2A38282895617DD8
      37A92F442E9EDB11A0F8211A959E837EF888E1C3C328D5E9295D481C7C24F25E
      D9E147DCC7C6C549D78C1BA7E3901ACAB0114C944EC415485B433B026CA5DCBE
      51A327AF43D96BEAA97CE55E8566EA22B889BF46A318B099622E3037D36B2154
      1A367CB88EC35D0873C09D02DB0EAB8290469C5A0379296A8E9E22A129927F01
      C772509032752DC1184679C7568D524D847F55A79B62AB533BA5CB067B0C9719
      9D664001E778FDAE5D0E38FC09EBB6582AA3662D5216333060BB233BAFDE3AE5
      ADA2318F6E92A615185347C95A731F06F22174268C21CC235CD1AE609855217F
      1A29A28753D0350A7E5F4868F9BAC2D487BE28B8F67C8DDB2E94E4868A5F37D6
      979E5E2DD796BC046BD8F744BC8B7084F003A1D531778F51D57B9E861A765F3A
      065CC97D5B6671509FC3417E6E6363E35C229B3BF38D9CB7E3976667C73D9095
      7DEB9B875F79EB9DF7E7503B3F4AC46141B7E3482BEC4DB7E4AF44BE1BD86253
      A74D9B760355F8B3E981309772CFDCEB579F785F414E026F6F3EF3D08E1D3B66
      DF3463C60D9C9C74667BE076E31D4B804D985B9D991235472DDFE8E8E8F4BB17
      2FBE316BFFFE3953571D7D472017B0E570D1E203070ECCB9EFDE7B67C4C6C464
      30F57611A7158A3FBC955815B5EC1202B9046658DFBBBF581D7DEFBEEC58B265
      E3C1E225B9B9B9735E58F9C2CD64CD083FBB3D883916F335E90FB1B60EC06A49
      0BBB59AF99F4DE5CFDCDDFEF095DB423EB93EDB9F7151515CDDDB07EFD6C36B0
      89EC2A62D8E91944D8D6712FFF7EF6CE99B865FB37AA991B368FBB63C53DB4E9
      CCA1D83287F6B699D1D1D1FDE9BC61FB63D6CCDDA3C5DC3D5711C25A28CFDD13
      CEE7E7085F62CEAEAFF5573E9E3168E0C0EBBEFEEAABD9F47D997DD555574D66
      258862C18A79C11FB0A609F9970AB2B97B2EBCBE899BD647B0106D14E7077232
      38EB95975F9EC54DABAE60EA2FB3609B72701D09BC426826F016EFAF123952AA
      AD5E6F60834BE6AE8737CE9F37EF2636C21E4CBDE9DFE73D1B1A11B76B2C5A8D
      464BF9347C407AFA64CA4653598563C97BAA712F299D8BF45D3668D85D04C446
      478F8B8B8919CFFB60E64C73D9B12F23A098F5FBB2F71C451843E4422E6D97D3
      CBC073D120A2A658F875286104C146F8AF13D0136927420AC1409008FFD543A4
      581011C8C5FD6521BF9C80627CD0B04BD0734CD6B38119093EBCB7F19D89B549
      434A17CDCD4509B020855CA46595F4F5B1D962D925F7A4FDB22FC7EADE6C78A9
      1CBB431846DF78EDBBA198BD6320ABF75CC2B384C584A984A1046AB349FEC283
      1145AA15B9888D2A65E0A04149DCBF31885284EFD0A1437DBAF01F20EBF421B1
      3931B327B92D615743A5E94C4F8E108EB5A26D08C1381A00DFA1B2C31C30DB97
      948B8A834624E746DF34DD1E14DE4DEBAEB24BB545BBE78FED2EDD764D2F3403
      0988E3867663329BBF48BD81AC891973D555F1536FB8C110131BCB3F50B24994
      7FF0D775E5BD379C68E84E16A157B8F67846B7B8E0D4B8D050B5CED820A9D432
      732F1063F7D102E5DA2E078C7851B9E88E8F0AFAEF3855DF89AD0E9387A4A273
      972E4936AB85FB4BB84C5E4FA3862A000569EB533B024C818A2C6A27172D5E9B
      9FF6CB99FA381179E2E064B21B8A48C36E83FB614B1A6EABAE92656FBBDAD48E
      404772D1D2CF0BBA6FCFA94D6A42DE495C7E03897216FF4B93A9E28B76C8F90E
      6A716A0D17CA456F66A9527F3C56D78DA83071706BE48097EA648A33B4F7D7F0
      EF574D352A8DCE4D5D9B2CF03597413B024C8C107E45E371641B870EDE5BACED
      CE8227F226B688C8C44B760B1F482FEA686EACAE753A35269F1AEE14C43F5F6A
      CA483301493406461A403882D5BD8B04015CF37A94CAE8BBC866D2770E0E0CD0
      4C1FD95D1065556F8A4CEC7428F3D2DDC42917979E77BFFEF186EF1A7DE3E968
      648C65E1E413978021E04F8D5E7307F27A036128EFC712AE86C93158561B4C1E
      6765DDC4C14956BD4E4FAD8B8A719B0878B984CFD5E8C2475F7C9FFBE3A1F32A
      B7993EEB1A43200B44882FCD0D8D28F12F41E001714728245808E2A89C7F4D6F
      5DD7840875437DAD46923D5A3DAD1B128DAA823D6E5A375EFB6297F76CA55BEB
      D6D21629CB1F93FAE750A93F62E46D846C420E61ABE035AF102D3058B9693AD9
      B8D050D6E88C2ED96BABACAAA9D0719EA6379B55D2D9D3A78E6C3C52915451C5
      7FCFAB29FA409FFDECC7AE539BEB992B6F5354D4F15A44500E9183B1E22EAD53
      A8B8B400CB82F76489C43D68DC8D2AD9DD603875EA4CCEBE738D89959555327E
      5DFBB27AE773EB5CA539FCDF429AA418BAA3A325071D7D14554ED40C8DDE52B7
      76EDBBB9B6E89EBD58A3E4D873EFFCF3C8D12FF7B85C2E27355ECD29EF0805DB
      41AFB91DE6A075E8359F6ED9E2545BD3F56A18A6869EFC64CFF7EF9DA4A7F229
      EEB7ECA4487F4902ED5A726BC4E2FE95CFF76C94F5B6748FA475A656AC5B5B76
      7C7B455959592953DF40E44AA312E12E069764D1CB9FEFDDC89A3184DBFB7835
      A77F7AAED6BD3DE2985EAFAEAFAFAF246BDC2CD83F4F80C84F335543207E0D55
      4F493B56541484863A58F86AA6BE8E04046BFE908068C98389E306424747395F
      3EC7165E42C4369A5AFAF159CD3DEEB6B353ACE0BD20C2CBC50F4140B0691883
      88EE2298577154F2F43511FFCCAB38249ECC843482E8BF76F12AD4997F4880E1
      5A1D73F7E8D9370964AD5E2AB7E29D9E779D0829040341BCE3E5BF77A8884A10
      11C8C53D1FFFF8F8A35488F1594CC839C869B54BEEBEAB927337EEC4D7C8AE47
      82464F5A8DA267B8082149DDD6B7A5753016AA402EA46A0B055E078500875667
      C05F6E5F00CA337490A00F0623A8659E2E72BCF4C2CA8E09FC869C780C7E94E2
      12F9471761E9548C5773E9AA2052585C0199A973B15715F82928D06DD70B71E5
      8828C60945792276BDEF9097E44787729187CE1A74FB55B67612FB378604719B
      3A42043D3623027D10469750878D9B1FF25D00FD6044C6441515D71660672646
      AF36FA226E7E21942310ABBC046FB4CDC96A558282B082445462DE68B983252F
      ED59C46C7628173157DC248FB1252F96DCF300FF54FB2F088D08C7B207EFE508
      4A7FECC646852DEC9F14562D5FBE5CA9088C21E8FC0EE4A1B0FAD9529293E3A8
      463372B22D74174AA44D5B368F158B5046D12BC768E4BE8F9CA20D1B3A987F5E
      3E58D91A7CF0E0C1CA1A92E1C38773F3181DB66DD98C762C621720280B9688EA
      A90857CDE4B96F95E226FAE0830F800E7CFCE7ED483CF2E8135C98AE530A999D
      1F85018FF27F094B972E55A2759403513D3BD4176DD9BE73ECA08C0CD0D402AB
      858EAD14CEAF1C3A08E9FDD23078603AFFDE753086F339AD5777FA451AB0851B
      C6B42A2685A05095A93807B6712ED087AB55C229AEABA914117305705B8F5574
      13E5BF9C2F85F04915AC107FAD22F8CECAD1B2764494D7934F3E89C71F7F9C23
      5A13DE96F36F2C12BB810AF6057056CFC5ED36912B70998C52065CEDA2FCBF81
      A8E783063107F49A12FCE786D0C21557F9AB1521890B17D2763910949802A1F1
      B5B291A54C9C30218EFA5183A8AA2FBDFCF2AA071E7800999999CAB6D5D4A12A
      FFBE21E208E018A15406717DE699673ACE8108280A8BAD99FF39E2ADA7418EFE
      623ED698E86815FFA961AC58CED4AB572F906DE4B3496195F85B029113018275
      D4F5296ACF8BE64010214F455908DB4D10B7C1EFB564C912BFACECEC7F8832B8
      EFFEA57FD80E1E7FE22925071DB2481010C036A1222FF97F2FB6A4EB274F4EB5
      FBF9BDD55CFDA8C36315D6800B5995AA49AD9752C8A20C9938A53A8B42BE2401
      418411B59CAE3A525352FA5D3B7EFCC734CE294B97485C41289089701D818E8B
      423B7ADFE61D11A9386DFD0FF4456DD0B57F60614B1C0FFE4BFAA2F6F89537FF
      D317296CF88393A86D1686194A1841F89FBE884CE01161B168974747FBBD1010
      D0FD45BB7DFCF30101373F111030F321AB75FC1283A1E76C8D2620A069DB1086
      EEF8681EBEDB7C4D8A8A527DFBCC337E1FDE7147DF51C387CFE93D76ECBDBD66
      CD5AD0F78E3B66652C58302B63F6EC857DC68DBBA76FCF9E7316391CFDFEAAD3
      F9476AE854D7064BD3433B02496161DAD71E7924A96FFFFE73123232EE0B1F3F
      FE86841933D292AFBF3EB2CBB8718EEE63C73AFA4C9A149E71F3CDBD87CE9F3F
      75F4CD37DF377AF8F0B9CFF5E8919A68A73F6913DE96731B02F1369BF6B5C993
      BB75F7F199AF75B9C6514C8837D8ED568BDDCEFD41CC8DEC5B9C14E19DFC1BB3
      465FBE0B080FB784F7EA159774CD3563078F1D7BEBF74B97F6DEFBFCF3427E6D
      21A069BE8BD46AD58FC7C424C51B0C3772E9681AFF72C987E324F775D6D4BA1B
      1AF29C5E6FA1DA60A8A14420BB5D2E8BD9EB0D52495290C43D7AD89DF8EAADD6
      1E668783FE3A70750A0EDE7FA4B0D02D70F3BBB800CF3B1C41C33232E6440E19
      7285C6E1B0D5A954166E332DD757541C95929377EAE3E3CBF9DF44CA7C80331C
      95C6E5D2D5E6E6067B4F9D4A91F2F2C26D0D0D328D9A755E8D66D32FFBF7BF32
      F8C30FCF0ACC0A8BC2B45A5D444C4C6F7B7C7C3F4E5D4D8D5470C82E97A455A9
      B42AA733BC6AC78E48B9A242CB7F2CF108F0F3F76F340706D6AA6363738BECF6
      CD27F20BB25CB575B58D9595FAC673E77A9AAAABFB4753A12E08282C9AC3FF20
      880C0B1BC85ED3EEADAFF772FD7791CB609038BBB7591C0E1F577E7EEFBC2FBE
      90B9C0E6B05F5C5C8BBC4E372D8F2E22A27C5D51E1EE309BADCA4FABEDC5F836
      B3C73360A1D9BC65514343BEC657A351C785848453091AEFA556A611389F5B56
      F64989C9D43DDD624927515F7B40802FFF7C2DED3CFF81527BEDB587AD51510A
      1121AE70AE2C1FA8A828DD5D5A9A3F2C2C2C9AA3509046A3890DF5F78FF2ADAC
      2CA6ABA2A4F6319BC3D51E8FAFBBA6466EACAD3DB3FBE8D1ED3FD6D757844646
      6AE3CCE63E6AC0C76EB3F93AEBEAD23C3B77497506C3AF2A1F1FAA8AEAE59C93
      27BDE7F2F22A7736349CEC67B19CA0CF5680E4F15899FB6816FE2FFC4730AD61
      5D5ADACD47C78DFB39E7861BB61C9C3469E92D7171945802526F9B3E7DCE113A
      D695BCF8E22F652B57E694BDF0424EE59B6FEEABDDB56B75616EEE9DADED6881
      46A37DE3C0810B4E8C1AB5F9C8D0A19BD775E972A75DA3E13E991A8DF1EB6EDD
      6E3D306CD8E65FC78CD9BC7ED8B0DB638C463367348A1DEDE1850BE71F5BB56A
      4DE1934FFE52F8E8A339458F3F9E53FADA6BFBCE7EFEF9AA47172DBAB5D98E66
      21B2EF7AF6BCE9E0A0413F67F7EFBFE5EBE4E47B49C00CBB5A6DF82C3171F6EE
      B4B44DFB060EDCF2535ADA92057A3DFFCB4EDF62475BF9C823B79D2091D30F3C
      F04BEE82053939F3E7E71C983B6FCF9A8103978FB6DB13856010A4561B3F4B4E
      9EBFA777EF4DBBBB77DFF2595CDC5DC46D82AF4AA5FF203272FCCFC9C9EBB776
      EFBEFD876EDD9EFF1B8D6F9416143B1A79153964F0E0F15FBDFBEE82E32FBCB0
      E6C8AC59BF644F9890B3EDCA2B8F6FE8DBF79BB70203A7DDA552F92D3718FCD7
      25243CBEA573E7AD9B92923612E70CE23608029AB78282D2BE898AFAEC878484
      1D3FA4A47CF646747446945ECFCAA0E15F9CD92C74EAEB453BFE8CBDEBD72FDC
      F7F0C36B368E18B17763DFBE87B6F4EEBDE7CBA4A48F5FF1F199B2CA6E1FF255
      6CEC873FC6C7EFF8362A6ADD3F8383879080D20CF08CDD1EF24950D0F35F8485
      6D5B171BBBE5A71E3D323FE9D12334C16E17930F2D2D50A1CD76B4EFDE7FFF8E
      E7FAF55BF9756AEAB7EB5353777D9794B4F3A3A8A82FDF0E0979F38BC8C82D5F
      86876FFF343878D52A7FFF681F5623D1D810C67278D56EBFFE838080EF3E0E09
      D9F57DF7EEDF1E9B356BF68E871F0E72D86CC2FCDB462E1A909030FEF5E8E8D9
      5F44477FF66574F4CE4F232276AE0D0DDDBE363878F7070EC78FEFFAFBCF7ACC
      C7C7A4206F3EF145D45BBEBECFBC6DB76F792F2464E74F63C67C7DEEA597EE3C
      F8E4932977F7E913C07F80EB3275CA949BE6CE9E7D73A0BF7FDA9DFEFEF1E4FF
      E2F782827EFA97C3B19B487733EED6377D7CFEFEA8D59A10AA6AF2686BC68F50
      B55AF39CDD9EBEC6667B7D8DD5BA8D91777D999EBE71CF9429AF6D1B34E8F6BF
      FAFB4F9A1A1FBF640AABDF349D6ECA9346E3AD2FF9F8BCF59AAFEFB6D77C7C76
      FFC36ADDBEDA6C7EE7198B6558884AA56B41DCFA6699C3617C352060C44B16CB
      6B2F198D5B5EB15876BDE670EC6025D8449E6E78D26EDFB9DCD777E7B356EB86
      172C969F5F369B77BC6C34EE16615F341ADFFEBBC572F5036C40AD71B6BB7F28
      28C8F884AF6FBF9566F313CFEBF5DF10B63EA7D7EFFA9B5EBFEF71BDFEE872C2
      B33ADDBE153ADD6EBEDFFEAC5EFFC3D306C38A1516CBA07BAD56A1F26987B3DD
      8B60B2EB1EAB353CD364BAE62983E1C927B5DAB58F6BB51B1ED26AB3FE4A7884
      7D7EA646F3F5BD5AED9A795AEDBC5B0C86E43B6DB63623593BA41DBD08242FFF
      62363B16EAF5DDE66BB5E326A8D58BAF55ABEF9FA452CDB842A51A95AC5275A5
      0416449D4BC73CEF086947EF986F6E2FAF6C06E743E5B43FB1590806F6B2A221
      B58C8A1DC5FD7FEB868F04D05EC8220000000049454E4400000000}
  end
  object ImagesSrc24: TTBXImageList
    Height = 24
    Width = 24
    Left = 16
    Top = 200
    PngDIB = {
      0400000089504E470D0A1A0A0000000D49484452000000180000006008030000
      0030D84266000001B9504C5445FF00FFD5E6EB91C8F26FA8E752A9EB73A3CB55
      A7A9B6C6D5B2D7EF34A9F532A8AC13A09B0990EB77BCF14F89D7748591A8A8AF
      B8B7B9A8B6C488A9C5D4D5D11BA7FA3EBAAC5EC39C6DD0A866C6628CC57490D8
      F95288AF92B9E1CBC9C8AEEABDD3F9F98ADA7A33AA1E0C76D9358BE11EA46CFB
      FCFC45BC1C3390A0ADCAEC49BA4965CC2D77D94C087957D6DAE3B1ECFA74D2FC
      0886970F8918066A598898B8E8E8EA96E5F545BBFB0B9594067816346C6C0967
      D11477B95EC0FA2CC0E407661B2B499898A9BE11585D7388B014A22410456F10
      59CE0A8B696E76901468B80E58A631566B0839981049D33359A32F395A8F8D8B
      0E49A00E278E4E69AA4C495B3158D14D589E0D2A690D386E4B48A856585BB3B9
      DB5368CE3348653168A79BA08898B4C83675B55659DB7795ABF8E9EA5495D952
      699451666D6C6967C8B9AEC8C9E58989B4F2D9DCAA9BAA999AB0949BDA91C6C3
      AAABD6CBB9DC8DA8DAAC93FC7074C966598C887AA66B68A8B5AAA7737AAE8C87
      ED6C93D0845E9CA4C29C777775AB94848DEA683379D18F69A9B3A28AAD8D5595
      8D5508185C8A69694B7AD53637376F472F6C5A405078A63569D34E362FDBC985
      EFD8BAF6E49914DA03960000000174524E530040E6D866000006CA4944415478
      016D958B5BDA581AC64FB86C4003920B550709200CAE650CA8045448116D2920
      BA528BB5C6B1D8C256939234BA5C8C74C632BB33D3BDB897BF78BF13D676FA3C
      FB793891F797EF3BE49CF79C20F4751036BBC3F93B3BF9597539DC1393D484CD
      E39DF2D18C83E5C68470F81F4CCFCC7E13989EA4E61884822C6F01C23DF16026
      1422C291798A8AC64864635C181013DF4ECF84C2E1786461818A46A3BF271769
      0203C7C3C46C24148F87BF9B8571A24B4B738C8075D7E4FCC344882042E164CA
      4F51CB2BABAB69110347F4DB8508918C244299AC9F5A5B5A5F5FDDC03F8AA072
      F969577236319B921EE184F54261B30819AE688EB2B91E2E3C7C04F753D4D2FA
      52A1B0B50D2090CB7902D38F2727A1FE3861ABF0640AC062296693969FAEADAD
      51F0103002243C290370E524299A8B2E2FAF3C8D467350A750D8B0320887E428
      E572B9A59595A54AAE54DDDADAD8A9E1522E47B6542D9572ABEBEB85D55275B7
      B6B1B1F7877D28E57914ABD576EBA56AA15028EDD66BCF760E369FC3B4130E77
      BE56ABD56BF54261B7567B56AB350E5F4CC194D8DCB19DAA256C1D61B9FEB2F1
      E258864A417FBECC567637EBB51DC8ABEF9D348EBEF7818E3CFED357A8795639
      6D1C341A2727AFEB2F8E2D1D9114F50621F6ACB257DADBAB6F1E7D5FA6F1FD08
      B5EC14D576B4DFFCB1FCF6EDE1F1F9940FC61D13E1CDDCDCF61B5A1045512044
      6B21C60021FE82C4F37FFFF5AB2BDCAE28B24F7DF71BB5C5412874A7A8A5F56D
      5579D7FA1FE3059EF6695A31F8DE95CCA40D59867689214FA69BEF03C970389C
      4C66AE645111145190550371BC27934926414E66324E4314E3A2FCA76EB78738
      D996CA00CA809EB16B50AADF1D0C00A06B5B0A1380999447972FF5EEC054CD1E
      42974109482013C8A452922AD3C640D5D53E008E0590B242926EA014DC6F6280
      74494A0193700C8D4BC3EC9B262E853AD96C168B9294CD7A2D00B5A014321C40
      C6CDD1356443553555C5401EFAB359B73B2BD96FFA867C094035CDE730318AD7
      EFF74B1EBDFBC1B0422B0FCA568692F6DB83F0EB5FBF8609363ABA59568B3FE0
      528A7A6568DA76777062D097248CD1F175D46358268197857786D1EFF675BDDF
      9165DFB53AE8694811E2F1B828D2068CA877CCBEDA517FECDD228468516CC1FA
      C154F455CD30AF05A3BF0F3242320FB220F006BD6F76F4C1B525E2AE55BC1444
      8153685535B4C1F9F844C000B534B543CB17B2A1F57BE54B4BBAEF385F7FD0EB
      1DF74CDF57AEBAC7FFEF1A4E46A6A76702610421BE13844B03FE41480C453E7E
      8C24E6D7620C384CA6754D33354C661221886F7E20659E2E82C3C04DE91F0184
      16121100A4E8D38A2CE9B28CE41908084DCC27BE8B7C0C2DF26C0AFC65E919CF
      070EBD8FAE2D241289190F17040725716432B6430EB5736BF3F30B0FB27E3298
      4A0580410BA4606A62B9E8D3C78F27298A65C05EA066C05FA9031579ABB9DCF2
      533847DAB284CD15C05DCA5491B30A07497E657D7DC480BF2C352549DE3E224B
      35384A0AEB859261B7EC05B294F57E40A88D4F9242A15E573DD85CD87660BC01
      42E1111C255B4F6A35D3EEC7C49DCDDA8703EC2B62B4BB79B0D9686CA7DD7EF7
      23B7E4BCE9F5CEAD851187A78DBDC6E6E18DDFEFF0E837DD973D1AA60AB716AF
      99E5B2AA3A3C860ADBE9350C80E5FB8F60E06DA377BB03D52A74AF2BE096B1BF
      BA9AAEDDAB088CD7C2C6E3C192A66EF4EF4781ADC861E381BF2ED43E18AFFF39
      45F481BD048E338CFD2943EB8DDD68515EE50591E3E48E6EA83DBC0B2C157742
      07DCEBEB6866AFF7A5100608011A9CF77EDAE7C65FC73D54E1439CAB25FEC6BD
      702BC7891C4F103C21D09AFA25017601CF711717FCB52C2882FEF23E8954880B
      FAE2E2FAF6B6F38A5C649A87CF14ABBC6228FCF5EDF59FF7FFF2CAD7648257C3
      C6B3170210B1689074E7E7FDDB9FD3ECD595333D3CDBACFD82DF3872DA08B2E7
      BF4E4D99DB735EF8FB745ADDFDEBDF04249A43FD8A2DFFDDFCC7DBD3CAE9E9D9
      1DF4F5E7FFBC40CAC9F6D0E91C9E552AF9FCE82C0F7DA552891E5DA3ED839321
      C4C8DB64482E4E369BC526C9B04734DA6B7CF2B687C3BC93E34886608A804571
      71EF5F68B771F7C902A2C893CD2229B4C47FB75C3BFF419BB93BAFB7DD1E7919
      8123A10C875F0B6C954377A318E8760024093B0A968A234867C985D2F958BB6D
      BF8ACD312CC3334D5E842BE32C2948B8733BEC764FCCCBB0458E6419AE596479
      B2EA42682EE6B07B6C6DC868926CB15984C6B079982B65E4963C8B6D2F034A93
      8528B2C5611B006A8E248FCD31D7649B508D24399213831640C398646B0F454E
      84276C89AD782BCE8D0162DB1E775B70916C902715C1B5A804BDB8147C94616C
      148BC5DAB11855A9B42182205A0D9E89B52D2E06D96150B0842F1DBC1B099120
      3E1F65FF0528CFC66484B049DF0000000049454E4400000000}
  end
end
