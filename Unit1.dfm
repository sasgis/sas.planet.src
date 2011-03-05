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
      DockPos = -6
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
        Images = PanelsImageList
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
    Left = 48
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
    Left = 44
    Top = 209
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
    Left = 48
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
    Left = 48
    Top = 168
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
end
