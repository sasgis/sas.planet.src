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
  PixelsPerInch = 96
  TextHeight = 13
  object WebBrowser1: TEmbeddedWB
    Left = 0
    Top = 0
    Width = 850
    Height = 562
    TabOrder = 5
    DisableCtrlShortcuts = 'N'
    DownloadOptions = [DownloadImages, DownloadVideos]
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    OnAuthenticate = WebBrowser1Authenticate
    About = ' EmbeddedWB http://bsalsa.com/'
    EnableMessageHandler = False
    DisableErrors.EnableDDE = False
    DisableErrors.fpExceptions = False
    DisableErrors.ScriptErrorsSuppressed = False
    DialogBoxes.ReplaceCaption = False
    DialogBoxes.ReplaceIcon = False
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.Header = '&w&bPage &p of &P'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Footer = '&u&b&d'
    PrintOptions.Orientation = poPortrait
    UserAgent = 
      'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.' +
      '50727)'
    ControlData = {
      4C000000A4100000550D00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
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
  end
  object TBDock: TTBXDock
    Left = 0
    Top = 0
    Width = 842
    Height = 59
    object TBMainToolBar: TTBXToolbar
      Left = 0
      Top = 25
      DockPos = 0
      DockRow = 1
      Images = TBImageList2
      Stretch = True
      TabOrder = 0
      OnClose = TBMainToolBarClose
      Caption = #1043#1083#1072#1074#1085#1072#1103' '#1087#1072#1085#1077#1083#1100
      object TBmove: TTBXItem
        Checked = True
        ImageIndex = 4
        Options = [tboDefault]
        OnClick = TBmoveClick
        Caption = ''
        Hint = #1055#1077#1088#1077#1084#1077#1097#1072#1090#1100
      end
      object TBRectSave: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 6
        LinkSubitems = NRectSave
        Options = [tboShowHint]
        OnClick = TBRectSaveClick
        Caption = ''
        Hint = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
      end
      object TBCalcRas: TTBXItem
        AutoCheck = True
        ImageIndex = 5
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
        LinkSubitems = NFillMap
        Options = [tboDropdownArrow, tboShowHint]
        Caption = ''
        Hint = #1050#1072#1088#1090#1072' '#1079#1072#1087#1086#1083#1085#1077#1085#1080#1103' '#1089#1083#1086#1103
      end
      object TBGoTo: TTBXSubmenuItem
        DropdownCombo = True
        ImageIndex = 7
        Options = [tboShowHint]
        OnClick = TBSubmenuItem1Click
        Caption = ''
        Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1085#1077#1082#1086#1090#1086#1088#1086#1084#1091' '#1084#1077#1089#1090#1091
        object TBEditItem2: TTBEditItem
          EditCaption = #1051#1086#1082#1072#1083#1100#1085#1099#1081
          Visible = False
          Caption = #1051#1086#1082#1072#1083#1100#1085#1099#1081
          Hint = ''
          EditCaption = #1051#1086#1082#1072#1083#1100#1085#1099#1081
        end
        object TBEditItem1: TTBEditItem
          EditCaption = #1071#1085#1076#1077#1082#1089
          OnAcceptText = TBEditItem1AcceptText
          Caption = #1071#1085#1076#1077#1082#1089
          Hint = ''
          EditCaption = #1071#1085#1076#1077#1082#1089
        end
        object EditGoogleSrch: TTBEditItem
          EditCaption = 'Google!'
          EditWidth = 150
          OnAcceptText = EditGoogleSrchAcceptText
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
      Images = TBImageList2
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
      Images = TBImageList2
      Stretch = True
      TabOrder = 3
      OnClose = TBMainToolBarClose
      Caption = #1055#1072#1085#1077#1083#1100' GPS'
      object TBGPSconn: TTBXItem
        AutoCheck = True
        ImageIndex = 10
        OnClick = TBGPSconnClick
        Caption = ''
        Hint = #1055#1086#1076#1082#1083#1102#1095#1080#1090#1100#1089#1103' '#1082' GPS '#1087#1088#1080#1077#1084#1085#1080#1082#1091
      end
      object TBGPSPath: TTBXSubmenuItem
        AutoCheck = True
        DropdownCombo = True
        ImageIndex = 2
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
      object TBControlItem3: TTBControlItem
        Control = Label1
        Caption = ''
        Hint = ''
      end
      object Label1: TLabel
        Left = 119
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Label1'
        Visible = False
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
            ImageIndex = 6
            Images = TBImageList2
            ShortCut = 32850
            OnClick = TBRECTClick
            Caption = #1055#1088#1103#1084#1086#1091#1075#1086#1083#1100#1085#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
            Hint = ''
          end
          object TBREGION: TTBXItem
            ImageIndex = 9
            Images = TBImageList2
            ShortCut = 32848
            OnClick = TBREGIONClick
            Caption = #1055#1086#1083#1080#1075#1086#1085#1072#1083#1100#1085#1072#1103' '#1086#1073#1083#1072#1089#1090#1100
            Hint = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1087#1086#1083#1080#1075#1086#1085#1072#1083#1100#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
          end
          object TBCOORD: TTBXItem
            ImageIndex = 8
            Images = TBImageList2
            OnClick = TBCOORDClick
            Caption = #1055#1086' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1072#1084
            Hint = ''
          end
          object TBPrevious: TTBXItem
            Images = TBImageList2
            ShortCut = 16450
            OnClick = TBPreviousClick
            Caption = #1055#1088#1077#1076#1099#1076#1091#1097#1077#1077' '#1074#1099#1076#1077#1083#1077#1085#1080#1077
            Hint = ''
          end
          object TBLoadSelFromFile: TTBXItem
            Images = TBImageList2
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
            object TBfillMapAsMain: TTBXItem
              OnClick = TBfillMapAsMainClick
              Caption = #1042#1099#1073#1088#1072#1085#1085#1086#1081' '#1086#1089#1085#1086#1074#1085#1086#1081' '#1082#1072#1088#1090#1099
              Hint = ''
            end
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
      end
      object NSources: TTBXSubmenuItem
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
        Caption = #1052#1077#1090#1082#1080
        Hint = ''
        object TBAdd_Point: TTBXItem
          GroupIndex = 1
          ImageIndex = 11
          Images = TBImageList2
          Options = [tboShowHint]
          Stretch = True
          OnClick = TBAdd_PointClick
          Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1084#1077#1090#1082#1091
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1091#1102' '#1084#1077#1090#1082#1091
        end
        object TBAdd_Line: TTBXItem
          ImageIndex = 12
          Images = TBImageList2
          MaskOptions = [tboShowHint]
          OnClick = TBAdd_LineClick
          Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1091#1090#1100
          Hint = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1099#1081' '#1087#1091#1090#1100
        end
        object TBAdd_Poly: TTBXItem
          ImageIndex = 13
          Images = TBImageList2
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
          Images = TBImageList2
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
          AutoCheck = True
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
          Tag = 1
          GroupIndex = 1
          RadioItem = True
          OnClick = TBXSelectYandexSrchClick
          Caption = #1071#1085#1076#1077#1082#1089
          Hint = ''
        end
        object TBXSelectGoogleSrch: TTBXItem
          GroupIndex = 1
          RadioItem = True
          OnClick = TBXSelectYandexSrchClick
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
      object N11: TMenuItem
        Caption = #1057#1076#1077#1083#1072#1090#1100' '#1088#1080#1089#1091#1085#1082#1086#1084' '#1088#1072#1073#1086#1095#1077#1075#1086' '#1089#1090#1086#1083#1072
        ImageIndex = 9
        OnClick = N11Click
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
      494C010115001700040012001200E0DFE300FF10FFFFFFFFFFFFFFFF424D3600
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
  object TBImageList2: TTBImageList
    Height = 24
    Width = 24
    Left = 16
    Top = 168
    Bitmap = {
      494C01010F001100040018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000006000000001002000000000000090
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A7A7D900000000000000000000000000959B
      D7001C23C4001618B600A7A7D900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7D90000000000000000000000
      0000959BD7001C23C4001618B600A7A7D9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A7A7D90000000000000000000000
      0000959BD7001C23C4001618B600A7A7D9000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BEBCEB000705AB00907BB600000000009497EE002F37
      D9007579F9004448DE001618B600959BD7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BEBCEB000705AB00907BB600000000009497
      EE002F37D9007579F9004448DE001618B600959BD70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BEBCEB000705AB00907BB600000000009497
      EE002F37D9007579F9004448DE001618B600959BD70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BEBCEB001C23C4001618B6004A48B600555ADF007A87
      EE007379EA006365F200555ADF001618B600959BD70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BEBCEB001C23C4001618B6004A48B600555A
      DF007A87EE007379EA006365F200555ADF001618B600959BD700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BEBCEB001C23C4001618B6004A48B600555A
      DF007A87EE007379EA006365F200555ADF001618B600959BD700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BEBCEB004448DE004448DE004448DE00959BD7009497
      EE007A87EE007379EA007579F900555ADF001618B600A7A7D900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BEBCEB004448DE004448DE004448DE00959B
      D7009497EE007A87EE007379EA007579F900555ADF001618B600A7A7D9000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BEBCEB004448DE004448DE004448DE00959B
      D7009497EE007A87EE007379EA007579F900555ADF001618B600A7A7D9000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BEBCEB00292ADC005257F6006365F2007379EA007A87
      EE009497EE008689F5007379EA008689F5006266DB000705AB00959BD7000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BEBCEB00292ADC005257F6006365F2007379
      EA007A87EE009497EE008689F5007379EA008689F5006266DB000705AB00959B
      D700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BEBCEB00292ADC005257F6006365F2007379
      EA007A87EE009497EE008689F5007379EA008689F5006266DB000705AB00959B
      D700000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AFE8F800A7A7D9001F1EDC004347F5004347F5005257F6006365
      F2007379EA009497EE009497EE007A87EE003437BC001618B600BCB2DC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AFE8F800A7A7D9001F1EDC004347F5004347F5005257
      F6006365F2007379EA009497EE009497EE007A87EE003437BC001618B600BCB2
      DC00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AFE8F800A7A7D9001F1EDC004347F5004347F5005257
      F6006365F2007379EA009497EE009497EE007A87EE003437BC001618B600BCB2
      DC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004EBDE90066C9EF007387CF002929FA002929FA003437FB004347F5004347
      F5005257F6006365F2007387CF002F37D9000705AB00BEBCEB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004EBDE90066C9EF007387CF002929FA002929FA003437FB004347
      F5004347F5005257F6006365F2007387CF002F37D9000705AB00BEBCEB000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000004EBDE90066C9EF007387CF002929FA002929FA003437FB004347
      F5004347F5005257F6006365F2007387CF002F37D9000705AB00BEBCEB000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008ED7
      F000AFE8F8008ED7F0009497EE001618F9001618F9001618F9002929FA002F37
      D9004347F5004347F5001C23C4002C22A400B6595800D1ACA900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008ED7F000AFE8F8008ED7F0009497EE001618F9001618F9001618F9002929
      FA002F37D9004347F5004347F5001C23C4002C22A400B6595800D1ACA9000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008ED7F000AFE8F8008ED7F0009497EE001618F9001618F9001618F9008E8E
      8E007E7E7E008383830087878700838383008585850085858500858585008585
      85008585850085858500858585008B8B8B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000066C9EF00AFE8
      F8004EBDE900AA9689008C88CC001618F9000706FA00040AE4001618F9002929
      FA003437FB00292ADC000705AB00794C7000FC962C00D2684E00CC9EA4000000
      00000000000000000000000000000000000000000000000000000000000066C9
      EF00AFE8F8004EBDE900AA9689008C88CC001618F9000706FA00040AE4001618
      F9002929FA003437FB00292ADC000705AB00794C7000FC962C00D2684E00CC9E
      A4000000000000000000000000000000000000000000000000000000000066C9
      EF00AFE8F8004EBDE900AA9689008C88CC001618F9000706FA00040AE4008484
      8400FFFFFF00FFFFFF00F7F7F700FBF9F900F7F9FA00F4F6F700FCFEFF00F8FA
      FB00F7F9FA00F9FBFC00FFFFFF00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008ED7F0008ED7F00066C9
      EF00AF7A8900BC966E007378CA000706FA000706FA000706FA000706FA001618
      F9001618F9003437FB003437FB001C23C40084547800ED853C00D2684E00D8B3
      B4000000000000000000000000000000000000000000000000008ED7F0008ED7
      F00066C9EF00AF7A8900BC966E007378CA000706FA000706FA000706FA000706
      FA001618F9001618F9003437FB003437FB001C23C40084547800ED853C00D268
      4E00D8B3B40000000000000000000000000000000000000000008ED7F0008ED7
      F00066C9EF00AF7A8900BC966E007378CA000706FA000706FA000706FA008686
      8600FCFCFC0077757400E2DEDD00FEFBF70075777800747677006F7172007678
      79007375760076787900FFFFFF00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008ED7F000B4F2FC009CA5
      C700A97A6A00149EEC007A87EE00555ADF005257F6006266DB006266DB006469
      C9006469C9008770B6009789AC008770B6004C369800AF754900ED853C00D275
      6A00ECE6F80000000000000000000000000000000000000000008ED7F000B4F2
      FC009CA5C700A97A6A00149EEC007A87EE00555ADF005257F6006266DB006266
      DB006469C9006469C9008770B6009789AC008770B6004C369800AF754900ED85
      3C00D2756A00ECE6F800000000000000000000000000000000008ED7F000B4F2
      FC009CA5C700A97A6A00149EEC007A87EE00555ADF005257F6006266DB008787
      8700FFFFFF00F2F2F200E1E1E100EEEEEE00EBEBEB00E4E4E400ECECEC00EEEE
      EE00EBEBEB00EAEAEA00FFFFFF00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008ED7F000AFE8F8009CA5
      C700C18A5B000000FF00808080007089B300C18A5B006C97AD006C97AD00AA96
      8900E24C0F00C93A1100E87B3A00ED853C00FC962C00E87B3A00E7874900D268
      4E00D1ACA90000000000000000000000000000000000000000008ED7F000AFE8
      F8009CA5C70000808000008080000080800000808000C18A5B006C97AD006C97
      AD00AA968900E24C0F00C93A1100E87B3A00ED853C00FC962C00E87B3A00E787
      4900D2684E00D1ACA900000000000000000000000000000000008ED7F000AFE8
      F8009CA5C700C18A5B0010A2F40010A2F4007089B300C18A5B006C97AD008585
      8500FFFFFF007F7F7F00AEAEAE00E8E8E8007979790079797900797979007575
      75007878780074747400FFFFFF00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D8D9FA0066C9EF005DB7
      DB009A9474000000FF0080808000CC96580079A3A40079A3A4006DA8BA00C18A
      5B00E24C0F00E24C0F00ED853C00E7874900E7874900DF774A00ED853C00DF77
      4A00CC9EA4000000000000000000000000000000000000000000D8D9FA0066C9
      EF005DB7DB000080800000FFFF0000FFFF0000FFFF000080800079A3A4006DA8
      BA00C18A5B00E24C0F00E24C0F00ED853C00E7874900E7874900DF774A00ED85
      3C00DF774A00CC9EA40000000000000000000000000000000000D8D9FA0066C9
      EF005DB7DB009A947400BC966E009A989800CC96580079A3A40079A3A4008484
      8400FAFAFA00EAEAEA00F7F7F700EAEAEA00F0F0F000EDEDED00F2F2F200F0F0
      F000F4F4F400ECECEC00FFFFFF00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000066C9EF009A98
      98006DA8BA006A6AFF000000FF00808080006DA8BA006DA8BA006DA8BA00E87B
      3A00D25B3000B1671C00DE6C3400E7874900E7874900E7874900ED853C00DF77
      4A00CE9B980000000000000000000000000000000000000000000000000066C9
      EF009A9898000080800000FFFF00FF80000000FFFF00008080006DA8BA006DA8
      BA00E87B3A00D25B3000B1671C00DE6C34000080800000808000008080000080
      800000808000CE9B9800000000000000000000000000000000000000000066C9
      EF009A9898006DA8BA0079A3A400B6A392006DA8BA006DA8BA006DA8BA008989
      8900FFFFFF007A7A7A00B2B2B200F5F5F5007A7A7A0077777700797979007575
      75007272720077777700FFFFFF00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000409E
      CC006DA8BA006DA8BA006A6AFF000000FF008080800080808000808080008080
      80008080800080808000808080008080800080808000ED853C00ED853C00DF77
      4A00CE9B98000000000000000000000000000000000000000000000000000000
      0000409ECC000080800000FFFF00FF80000000FFFF0000FFFF00008080000080
      8000008080000080800000808000008080000080800000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000409ECC006DA8BA006DA8BA005FABCA0072B8D1005FABCA0072B8D1008887
      8300FFFFFE00F5F6F400F1F1F100F3F5F500F3F6FA00EDF0F400EEF1F500F2F5
      F900F2F5F900EAEDF100FBFEFF00868686000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D268
      4E0097766A0072B8D10072B8D1006A6AFF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF008080800080808000DF77
      4A00CC9EA4000000000000000000000000000000000000000000000000000000
      0000D2684E000080800000FFFF00FF800000FF80000000FFFF0000FFFF0000FF
      FF000080800000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00FF800000FF80
      0000FF80000000FFFF0000000000000000000000000000000000000000000000
      0000D2684E0097766A0072B8D10072B8D1007DC9DF00C7C7C700E6CA99008283
      8100FFFFFE00797B7B00D2D4D400F0F2F200777574007D7B7A007B7978007977
      76007674730082807F00FDFBFA00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D275
      6A00E24C0F00ED853C00F3D7B700FCF8B000E9E6B900F3D7B700EBA37400EBA3
      7400C8A46800EBA37400E28C6A00E28C6A006A6AFF000000FF000000FF008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000D2756A000080800000FFFF00FF800000FF800000FF800000FF80000000FF
      FF0000FFFF0000FFFF00FF800000FF800000FF800000FF800000FF800000FF80
      0000FF80000000FFFF0000000000000000000000000000000000000000000000
      0000D2756A00E24C0F00ED853C00F3D7B700FCF8B000E9E6B900F3D7B7008385
      8500F9FBFB00FFFFFF00F7F7F700FBF9F900F7F9FA00F4F6F700FCFEFF00F8FA
      FB00F7F9FA00F9FBFC00FFFEFA00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E4CA
      C800B1671C00AC7E1C00F1AB8900F1E6C900F1E6C900E6CA9900F1AB8900F1B7
      9600C8CC7400C9A07C00EBA37400E4947100EBA37400DF774A006A6AFF000000
      FF000000FF008080800000000000000000000000000000000000000000000000
      0000E4CAC8000080800000FFFF00FF800000FF800000FF800000FF800000FF80
      0000FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF80
      0000FF80000000FFFF0000000000000000000000000000000000000000000000
      0000E4CAC800B1671C00AC7E1C00F1AB8900F1E6C900F1E6C900E6CA99008485
      8900FDFFFF0077757400E2DEDD00FEFBF70075777800747677006F7172007678
      79007375760076787900FAFCFD00858585000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E28C6A00B1671C00EBA37400F3D7B700E9E6B900F3D7B700E6CA9900F1B7
      9600F1B79600F1AB8900F1AB8900F1AB8900A9B68C00B1671C00C93A1100E4D5
      CA006A6AFF000000FF0080808000000000000000000000000000000000000000
      0000000000000080800000FFFF00FF800000FF800000FF800000FF800000FF80
      0000FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF80
      0000FF80000000FFFF0000000000000000000000000000000000000000000000
      000000000000E28C6A00B1671C00EBA37400F3D7B700E9E6B900F3D7B7008183
      8400FFFEFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FDFFFF00848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D2684E00D25B3000F3D7B700F1E6C900F3D7B700F3D7B700EABB
      A800E3C7A900EABBA800F1B79600EABBA800E28C6A00C93A1100D1A4A6000000
      0000000000006A6AFF000000FF00808080000000000000000000000000000000
      0000000000000000000000FFFF00FF800000FF800000FF800000FF800000FF80
      0000FF800000FF800000FF800000FF800000FF800000FF800000FF800000FF80
      0000FF80000000FFFF0000000000000000000000000000000000000000000000
      00000000000000000000D2684E00D25B3000F3D7B700F1E6C900F3D7B7008482
      8200FFFFFF00FFFDFA00FFFEF900FFFDF600FFFDFA00FFFFFC00FFFDFA00FFFE
      FB00FFFCF900FFFDFA00FFFAF700848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000DF774A00E4947100F3D7B700F1E6C900F3D7B700F3D7
      B700F3D7B700F3D7B700E0CAB900E4947100D25B3000D8B3B400000000000000
      000000000000000000006A6AFF000000FF000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00FF800000FF800000FF80
      0000FF80000000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000DF774A00E4947100F3D7B700F1E6C9008886
      8500FFFDFC00EFE9E400F2EAE300EFE6DD00EFE6DC00F2E9DF00F0E7DD00F3EA
      E000F7EEE400EFE6DC00F6EDE300848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C68B8A00D2756A00E4947100F1B79600EABB
      A800EABBA800F1AB8900E4947100D2756A00D8B3B40000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C68B8A00D2756A00E4947100F1B7
      9600EABBA800EABBA800F1AB8900E4947100E494710000FFFF0000FFFF0000FF
      FF00FF80000000FFFF0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C68B8A00D2756A00E49471008A8A
      8A00F7F7F700FFFFFC00FFFFFB00FFFFF800FFFEFB00FFFEFB00FFFEFB00FFFE
      FB00FFFEFB00FFFEFB00FFFEFB00909090000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EADAD800D8B3B400CE9B9800F1AB
      8900C68B8A00CC9EA400D8B3B400ECE6F8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EADAD800D8B3B400CE9B
      9800F1AB8900C68B8A00CC9EA400D8B3B400ECE6F80000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EADAD800D8B3B4008284
      8500898B8C0084828100858382008D8A86008385860083858600838586008385
      8600838586008385860083858600868686000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D1D1
      D1002F2F2F001E1E1E00212121004343430065656500A7A7A700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7D900000000000000
      000000000000959BD7001C23C4001618B600A7A7D90000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006A6A
      6A005959590055555500525252005B5B5B005A5A5A0057575700595959001515
      15000B0B0B0007070700161616001D1D1D003232320052525200535353005656
      5600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E8E8E8005555
      5500414141005E5E5E0041414100222222003F3F3F0061616100767676000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FAF8BE00F9F6AA00FAF7AE00FAF7B000F9F6A600FCFB
      DA00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BEBCEB000705AB00907BB6000000
      00009497EE002F37D9007579F9004448DE001618B600959BD700000000000000
      0000000000000000000000000000000000000000000000000000000000003030
      3000B3B3B300C5C5C500CECECE00ACACAC00B0B0B000B5B5B500B5B5B500E8E8
      E800B4B4B400DEDEDE00C9C9C900B5B5B500C6C6C600C9C9C900CECECE005A5A
      5A009A9A9A000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B9B9B007F7F
      7F0072727200757575006F6F6F005E5E5E00333333004D4D4D00747474008383
      8300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFCE500FBF9C900F8F4
      9200F8F497000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BEBCEB001C23C4001618B6004A48
      B600555ADF007A87EE007379EA006365F200555ADF001618B600959BD7000000
      0000000000000000000000000000000000000000000000000000000000005454
      5400BDBDBD0089898900C6C6C600A9A9A900B1B1B100C3C3C3007B7B7B00C4C4
      C40086868600FFFFFF00CBCBCB0021212100DCDCDC00FFFFFF00FFFFFF008A8A
      8A007D7D7D000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BCBCBC00ABABAB00D7D7
      D7009A9A9A006B6B6B007878780098989800959595005B5B5B003F3F3F006565
      65008E8F8E00A19CA10000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F9F8B700F7F3890000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BEBCEB004448DE004448DE004448
      DE00959BD7009497EE007A87EE007379EA007579F900555ADF001618B600A7A7
      D900000000000000000000000000000000000000000000000000000000003636
      3600FFFFFF003C3C3C001A1A1A00FFFFFF00FEFEFE00FFFFFF00919191000000
      0000FDFDFD00F8F8F800F8F8F80038383800FFFFFF00F1F1F100F9F9F9008484
      84004C494C00C6C5C60000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C5C5C5009E9E9E00D7D7D700ECEC
      EC00F9F9F800CAC9C9009090900083838300A2A2A200B8B9B800818181004040
      400049584900729E72009BA29B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FCFAD200F9F6A300FAF7AC00FAF7AD00F9F6A700FAF9C3000000
      000000000000FAF7B700F7F38A00000000000000000000000000000000000000
      000000000000000000000000000000000000BEBCEB00292ADC005257F6006365
      F2007379EA007A87EE009497EE008689F5007379EA008689F5006266DB000705
      AB00959BD7000000000000000000000000000000000000000000000000005A5A
      5A00BDBDBD00BDBDBD00E2E2E20092929200FFFFFF00E5E5E50091919100FCFC
      FC007A797900FFFFFF00D3D3D3004C4C4C00E3E3E300FFFFFF00FFFFFF009C94
      9C00225A22008AB08A00BDB9BD00000000000000000000000000000000000000
      0000000000000000000000000000C3C3C3009F9F9F00D0D0D000E0E0DF00E9E8
      E800ECECEC00F4F4F400E5E6E600B7B7B700919090009A9B9A00BCBCBC009B9B
      9B004A614A001D861D006B866B009F9E9F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F7F38600FAF9
      BE000000000000000000FAF7B300FCFBD6000000000000000000000000000000
      0000000000000000000000000000AFE8F800A7A7D9001F1EDC004347F5004347
      F5005257F6006365F2007379EA009497EE009497EE007A87EE003437BC001618
      B600BCB2DC000000000000000000000000000000000000000000000000001E1E
      1E00848484008F8F8F00969696008B8B8B00898989008A8A89008F8F8F009494
      94008F8F8F008B8C8C008C8C8C0090908F0088878700888888008E8E8E004C44
      4C0008430800497D490082818200D9DAD9000000000000000000000000000000
      00000000000000000000CCCCCC008B8B8B00C4C3C300CACBCA00C8CBCB00C2C7
      C700BEC2C300BBBCBE00C3C4C400D8DCDC00D1D2D300A7A6A6008F8F8F00A5A5
      A5009AA19A005362530048534800868686000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FDFCE300FBF9C700FBFAC900000000000000000000000000F6F3
      8400FAF8C0000000000000000000F8F5A3000000000000000000000000000000
      000000000000000000004EBDE90066C9EF007387CF002929FA002929FA003437
      FB004347F5004347F5005257F6006365F2007387CF002F37D9000705AB00BEBC
      EB0000000000000000000000000000000000000000000000000000000000AEAE
      AE00919191006F6F6F004C4D4D00676766006A6B6A0065686A005D5E5F005956
      540057514F0057514F005C5C5C00747879007878790056565500464646005658
      5600737673004E554E00686B6800DEDEDE000000000000000000000000000000
      000000000000000000009F9F9F00B7B7B600BEBFC000B6B7B600A8978F00A181
      72009D7468009F786800AD938A00A8948E00BCB5B200DDE0E200C5C6C6009292
      9200898A8A00898989006F6F6F00BDBDBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FAF7B100F7F48D00FAF7B200000000000000
      0000F7F38400FBF9C40000000000F8F5A3000000000000000000000000000000
      0000000000008ED7F000AFE8F8008ED7F0009497EE001618F9001618F9001618
      F9002929FA002F37D9004347F5004347F5001C23C4002C22A400B6595800D1AC
      A900000000000000000000000000000000000000000000000000000000000000
      000000000000C3C3C300C0BFBF00D0D3D300CAC8C600BCA59A00B68E7C00B689
      7400B3856F00C5A39500CAAEA600BBA39B00DFDCDB00F4F7F900C5C4C4009899
      9900898A89007D7E7D00B5B5B500000000000000000000000000000000000000
      00000000000082828200A8A7A700C0C2C400B0A8A400A3796600A76E4F00AA7B
      5D00B58C5900A87E6700D1B6A900C7AA9D009F736100B39B9200EDEDED00E3E3
      E300A4A4A3008B8B8C00CBCBCB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F8F59B00F8F59E000000
      000000000000F8F59B0000000000000000000000000000000000000000000000
      000066C9EF00AFE8F8004EBDE900AA9689008C88CC001618F9000706FA00040A
      E4001618F9002929FA003437FB00292ADC000705AB00794C7000FC962C00D268
      4E00CC9EA4000000000000000000000000000000000000000000000000000000
      00007777770082828100C7CACA00AFA8A600A47B6A00A2674900A76F4D00A36D
      4A009C674400C3A69500D3BEB300AF8675009C716000CABDB800F8FAFB00CDCD
      CC009A9B9B00C6C6C60000000000000000000000000000000000000000000000
      00007D7E7E0022222100ABAEAF00C0B6B100BD9C8F00B1816D009F583700B9B4
      8A007C9AD500CAC28F00C8AAA100D0C1B200BB9B8B00A0725C00B2938300FCFA
      F800EDEDED00DDDEDD000000000000000000FBFACC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F8F49600FBF8
      BF0000000000FBF9CB0000000000000000000000000000000000000000008ED7
      F0008ED7F00066C9EF00AF7A8900BC966E007378CA000706FA000706FA000706
      FA000706FA001618F9001618F9003437FB003437FB001C23C40084547800ED85
      3C00D2684E00D8B3B40000000000000000000000000000000000000000009696
      96001C1C1B0076787900BFB7B300BD9D9000B98E7D009F6548009C5D39009655
      2A009B633B00C6AE9C00CCBAAB00C6B0A100AB847100A1776200D8C8C000FBFD
      FE00E5E5E4000000000000000000000000000000000000000000000000000000
      000043424200555657006A646300B8918200B286750099594300AE8551005C60
      CB005546D8006F78C700DECFA000CBB4AB00C5ABA200BC9F9400A57D6B00B190
      8600EDEDEF00000000000000000000000000F4ED7200F5ED8100FBFAB600FAF7
      A800FCFCDA000000000000000000000000000000000000000000000000000000
      0000DBDFDB00B3B6B300B3B5B300BEBDBE00E8E7E70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008ED7
      F000B4F2FC009CA5C700A97A6A00149EEC007A87EE00555ADF005257F6006266
      DB006266DB006469C9006469C9008770B6009789AC008770B6004C369800AF75
      4900ED853C00D2756A00ECE6F800000000000000000000000000000000006666
      65005E5E5E00605E5E009E796A00B5816E009D594700984E3700A8665500D0A9
      A100D7B7B000DBC0BA00D4B9B100CCB1A800C1A79D00B5988600996B5600D2C9
      C500000000000000000000000000000000000000000000000000000000000000
      0000767575007D82840088675A00A86D56009F5D4600BA9561005565E1007D5F
      AD00EBC48F006C62DD006B76CB00DFD6A800DACFB300CEC2A300B99F79009C74
      4B00E1D9C800000000000000000000000000F4EB7500E3C01100E6C31600ECD4
      3B00F2EA7800F7F08500F9F5A400000000000000000000000000000000000000
      0000DBDFDD00C1C6C200999F990095979500C3C4C30000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008ED7
      F000AFE8F8009CA5C700C18A5B0010A2F40010A2F4007089B300C18A5B006C97
      AD006C97AD00AA968900E24C0F00C93A1100E87B3A00ED853C00FC962C00E87B
      3A00E7874900D2684E00D1ACA90000000000000000000000000000000000AEAD
      AC00777D7F0088716700A6674F00995F47009D753F00996F4000AE936700D9DB
      BC00D4D6B700CECBA700CAC8A300C3B49100D4BDA200B89A8D00A3766100AC8D
      8100000000000000000000000000000000000000000000000000000000000000
      0000E3E4E4007E757200975D4500A1543D00BA905C005462DD00744A9500ECCE
      A800E4D9DA00FAE9C9006E64DF006E77C5009098B9009199BE008695C2008CA2
      C60081739700000000000000000000000000FAF7B100E7C82500DCAF0000DBAF
      0000E7CC2E00E7CB2A00E8C82300EDDA4500F3EA6D00FCFAD800000000000000
      0000E7EBDF00E4E8E300E0E4E100C4CBC500989D9800BDC0BC00D9DBD9000000
      000000000000000000000000000000000000000000000000000000000000D8D9
      FA0066C9EF005DB7DB009A947400BC966E009A989800CC96580079A3A40079A3
      A4006DA8BA00C18A5B00E24C0F00E24C0F00ED853C00E7874900E7874900DF77
      4A00ED853C00DF774A00CC9EA400000000000000000000000000000000000000
      0000B2B1B10087564100A1574300A26E3C00444CA80062638F00565B95005C64
      A4007486BC006879BA006578BB005E879E008376CE00D3B99C00C6B1A7009E76
      6800D9D2CF000000000000000000000000000000000000000000000000000000
      000000000000E7E1DE009F5D4900AE8561005462DD00764F9800C88E5800BF98
      8900E4D2C800E4D1CC00EFE0D000AFA0D900B3A1CD00AF99C300958DB2009193
      BB007C506D00FAF9EF000000000000000000FEFEEE00F0E45F00E9CF3900E9C9
      2800E8C62500E6CA2B00DEB60000DCB20000E1C01400FCFB9E00A5A7FD00686B
      EF00AFB3EE00DBDFE000E4E8DD00E9ECE300E0E4E400BAC2C000C7C9C7000000
      0000000000000000000000000000000000000000000000000000000000000000
      000066C9EF009A9898006DA8BA0079A3A400B6A392006DA8BA006DA8BA006DA8
      BA006DA8BA00E87B3A00D25B3000B1671C00DE6C3400E7874900E7874900E787
      4900ED853C00DF774A00CE9B9800000000000000000000000000000000000000
      000000000000BE978800AA685A00B5936100404BB800A3729200BA8B8600C7A5
      B100E2D0E000E2D4E800EBD5E90090AFAB007E87B100D7B8A200C1AEA100A478
      6500C9B7AF000000000000000000000000000000000000000000000000000000
      00000000000000000000D2B996007A83C100784F9600CD9A6600C39F8F00CBA8
      9200DDC7BB00F1E9E400EEE6E300F8EDD800EEE1CD00DBC6B300DACF9900838C
      C5007B4E6E00C6BDAE00000000000000000000000000F5ED7B00E4C31700E2BC
      0C00E9C82700EFDF5B00E9D13900E3C52800F1CD1200C3BD96000406F0000408
      E5004043F2009FA0EE00C5CCE400CBCEE000E2E0B200F6F19D00FCFAB3000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000409ECC006DA8BA006DA8BA005FABCA0072B8D1005FABCA0072B8
      D100EBA37400E87B3A00E87B3A00818A3100DE6C3400DF774A00E87B3A00ED85
      3C00ED853C00DF774A00CE9B9800000000000000000000000000000000000000
      000000000000DBC8C000D2AEA700BA9B7300616D9100C0A19F00D8BE9D00DDC6
      B000E2CFBB00F3EEE300FFF9E80096B3AC008284C700DFC5AC00C2AEA200B287
      73009A817A000000000000000000000000000000000000000000000000000000
      00000000000000000000CAB794008683AC00AF88A100D8BCA000D5BBAB00DECA
      BC00E3D3C600E7DAD000F4EEEA00EADFDB00E8DCD700D9C5C100C0AD87008C94
      C60081547000B8ADA100000000000000000000000000FDFBC800E4C20B00DBAF
      0000DDB30000E2C31B00E1C21900DFBB0E00EECF2900DAC96A003639DC000B10
      EA000609E3004749EF009294F8008487FD00AF9F8700E6C21000EBD84A00F2E5
      6300F4E86C00FAFAC90000000000000000000000000000000000000000000000
      000000000000D2684E0097766A0072B8D10072B8D1007DC9DF00C7C7C700E6CA
      9900EBA37400E7874900E28C6A00E7874900E7874900DF774A00DF774A00D25B
      3000ED853C00DF774A00CC9EA400000000000000000000000000000000000000
      000000000000CEB4A700C5988B00C0A87D007490C400DBCBDA00EEE0CF00E0D0
      C000E0CFBF00EAE0D900F1DDCC008BA69E007D7DC100C89D7900B3958100B78D
      7B009C847C000000000000000000000000000000000000000000000000000000
      00000000000000000000C4A682009194BB00D8CCE800F9F0E200EBDFD700EADE
      D100E4D5C500E8DCD100EDE3D900E0CEBE00DCC7B800CAADA000AF905E009394
      B1007B4A6100EBE7DB00000000000000000000000000FBFAC500F3E86300EFDA
      4B00E9CE2D00E6C51C00E9D03400DEB20000D8AC0000F6D214007A79C2000203
      EE001B1FE6008386F300B8BBF500AFB2FF00CCC2A200E4C31600E8C62200E8C8
      2900E1B90E00E6CA2000F6EF8500000000000000000000000000000000000000
      000000000000D2756A00E24C0F00ED853C00F3D7B700FCF8B000E9E6B900F3D7
      B700EBA37400EBA37400C8A46800EBA37400E28C6A00E28C6A00E28C6A00DE6C
      3400E87B3A00D25B3000DAD7E800000000000000000000000000000000000000
      000000000000D1B3A500C3938400D9D2AB006A84C500E3DFF500FAF8EC00ECE5
      D900EAE1D200E9E2D700EEDBC8008FAD9D007777A400B9845C00945E38009C66
      4D00D2C2BC000000000000000000000000000000000000000000000000000000
      00000000000000000000D8B28E008B72B9006B72CD00FCFFDB00F8F5F800F3EF
      E600EFEADE00EEE7DB00ECE3D600E4D6C800DBC7B700C7A08B00BFAE7C00585E
      D600724C6D00000000000000000000000000000000000000000000000000FEFE
      E500F8F59600F8F39900F8F39500F3E36300E6D03700F3D62200B8AC77000508
      E4005559F100B5B9F500A1A6F900BABEEC00ECD94A00E4BE0A00E8CD3600EEDF
      5A00EBCF3900E6BF1900F1E35A00000000000000000000000000000000000000
      000000000000E4CAC800B1671C00AC7E1C00F1AB8900F1E6C900F1E6C900E6CA
      9900F1AB8900F1B79600C8CC7400C9A07C00EBA37400E4947100EBA37400DF77
      4A00C93A1100BD6A690000000000000000000000000000000000000000000000
      000000000000DAC3B700CBA28F00DDD8B200738CC200E9E0F100FFFCEF00FDF7
      F100FDF6F000F9F1E800FFEBDD0091AFA2007676AB00CA9D79009F6744008C56
      3900DBD1CD000000000000000000000000000000000000000000000000000000
      00000000000000000000D8C5BC00D7AD7C008079E700646FCB00FEFFD800FDF9
      F800FDFCF300FCF8F200FAF4EB00EEDED200E2C8B900D4C99C00525CDE00653A
      7900C8AD8D000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FDFDE200FBFAB900F9F9C400FFFDB9009596
      E700C4C7FE00D4D6F800CED0FE00DDDEDA00F6EA5E00E2C01B00DDAF0000E7C9
      2700E2C11600E2BA1000EFD84900FBF9C5000000000000000000000000000000
      00000000000000000000E28C6A00B1671C00EBA37400F3D7B700E9E6B900F3D7
      B700E6CA9900F1B79600F1B79600F1AB8900F1AB8900F1AB8900A9B68C00B167
      1C00C93A1100E4D5CA0000000000000000000000000000000000000000000000
      000000000000F5F2EF00C79F8900D1CA9A007595BD00C3CED100D2E3D000D3E2
      D600D4E4D600D0DFD200D5D9C1007FA09E006D6BC100D8AE88009C664800B99E
      9100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000D5BCAD00FEEDBE008687FC006771CD00D6E1
      BA00C1CDBF00C6D1BE00C5CFBC00BDC4B300CAD1AC00545EDF0070467E00D0AE
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FAF6AD00F5EC7300F2E46500F3E5
      6A00ECD23A00E2C01300E5BF0800F4EC73000000000000000000000000000000
      0000000000000000000000000000D2684E00D25B3000F3D7B700F1E6C900F3D7
      B700F3D7B700EABBA800E3C7A900EABBA800F1B79600EABBA800E28C6A00C93A
      1100D1A4A6000000000000000000000000000000000000000000000000000000
      00000000000000000000F1E9EA00D9C498007980D000859CD0007A88D9007B89
      DB008397D7007786D5007581CD006D80C0007160EC00CE9E6700B69384000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DDCAC000FCF0C0008685FC00A3A4
      F1008C8FF200979AF4009A98E9008A89EA008E89DE0074569D00DEBF8F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FAF7AD00F8F49600F7F19400F9F5A5000000000000000000000000000000
      000000000000000000000000000000000000DF774A00E4947100F3D7B700F1E6
      C900F3D7B700F3D7B700F3D7B700F3D7B700E0CAB900E4947100D25B3000D8B3
      B400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000F6EEEB00DFC8AE00F9EFE000FFFFFC00FFFF
      FE00FFFFFF00FFFBF600FBF0E600F5E2D200D3B38F00C7AA9B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E1D0C100E7D5B500F5ED
      DF00FEFBF000FFFAF000F8EFE100E9D6C000D0B19700DFCDB800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C68B8A00D2756A00E494
      7100F1B79600EABBA800EABBA800F1AB8900E4947100D2756A00D8B3B4000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F4F0EC00D6C2AE00DDCFBE00F4EB
      DF00F9F3E600EAE1D800DFCDBB00CAB09B00D5C6BD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E4DBD400D9C2
      AC00E1CAB300DFCDB700D6BEA600D1B59F00E4DDD90000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EADAD800D8B3
      B400CE9B9800F1AB8900C68B8A00CC9EA400D8B3B400ECE6F800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F0EAE700E7DBD600BFACA500A89A
      9400AB989100AC9E9800AD9F9800E7E1DE000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000858585002E2E
      2E005C5C5C0086868600D9D9D900000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C8CBCA009F9FA000827F
      8800CCA9AE000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E0D2CE00B9A7A100A17C7300A4796C00A97B6E00AC80
      7300AE7F7400AD7C7100A27568008E685D0094716500CEBBB400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008E8E8E00212121002A2A
      2A002C2C2C004F4F4F0072727200ACACAC000000000000000000000000000000
      00000000000000000000000000000000000000000000A6A8AA0060768E005C6F
      AF009281A100CAA5AA0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F4E8E500A5B7C000799AA70068839200667F8F006E8594006E8393006D82
      92006B809100697D8D00647888005A6E7E004B62740053687900607687005672
      86005670850054708500536E8200597C92000000000000000000000000000000
      000000000000000000000000000000000000C2C2C200383838005A5A5A005A5A
      5A003B3B3B00202020004D4D4D00636363008C8C8C0000000000000000000000
      000000000000000000000000000000000000000000008CA9BB004AAFFF003C85
      DF005A6DAD009181A100C8A3A900000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000818C9500868A8E008689
      8C0086898C0086898C0084888B00797F85006876820000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F1EF
      EE009E756400677C86009EEBFE00A2EAF90091DBEF008DDBEF0082D2EA007ECF
      E80076C9E6006BC2E00068BEDD0062BCDE0060B9DE0056B4D9004AA9D20048A7
      D0003D9FCB003B9ECB003AA0CE004184A8000000000000000000000000000000
      0000000000000000000000000000D0D0D000909090007C7C7C00757575006F6F
      6F00757575005555550036363600565656007777770090909000000000000000
      0000000000000000000000000000000000000000000099D0F9005EC1FF0049AE
      FE003D84DF005B6DAC009181A100CBA7AB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097989800C9C9C900D6D6D600D6D6
      D600D4D4D400D3D3D300D6D6D600CDCDCD00ABABAB00747B8100000000000000
      0000000000000000000000000000000000000000000000000000F5EDEB009677
      6E006D270C0069737A00A2ECFD00A6EAF7009FEBFE0098E3F5008CD9EF0088D7
      ED0080D1EC0074C8E40073CDEC0068C2E10061BADD005BB8DB0052B1D9004EAE
      D60042ACDA0041A5D2003FA8D6004782A2000000000000000000000000000000
      0000000000000000000000000000A0A0A000C4C4C400D3D3D3008B8B8B006C6C
      6C00808080009F9F9F008F8F8F0051515100424142006E6F6E00908E9000ABA7
      AB0000000000000000000000000000000000000000000000000098CEF8005DC0
      FE004AAFFF003C84DE00596DAE009181A000CCA7AC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C3C3C300BFBFBF00E7E7E700DFDFDF00DCDC
      DC00D8D8D800D4D4D400D7D7D700ECECEC00EDEDED00A7A7A700979898000000
      0000000000000000000000000000000000000000000000000000B99D93008C55
      41008F45260067727A00A0EAFB00ADF7FF006D96A4008FD4E7008EDEF50088D8
      F0007FD0EA0075D1EF005A879B0063B7D70061BFE20059B8DD0050B0D8004DB3
      DE0046799300409EC9003DAAD9004681A0000000000000000000000000000000
      00000000000000000000A5A5A500B6B6B600DFDFDF00F3F3F300F4F4F400BFBE
      BE008B8B8B0087878700ABABAB00B5B6B500717271003C3D3C004E6F4E0077A5
      7700ACADAC0000000000000000000000000000000000000000000000000095CD
      F8005DC0FE0049AEFE003C84DE00596DAD0093819F00CAA9AD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000A3A3A300E4E4E400E4E4E400DFDFDF00DDDD
      DD00D9D9D900D6D6D600D1D1D100D8D8D800F0F0F000D2D2D200727B82000000
      00000000000000000000000000000000000000000000DDD2CF008D5C4F00A66A
      5700772E1200657179009FE3F100A2E2EA00647F89008BC5D50088CEE20082C7
      DB007DC4DA0073C4DF005877860065AECA0062B4D3005AABCB0051A6C9004DA8
      CE004A6D80004497BD00409FC9004787A8000000000000000000000000000000
      000000000000A4A4A400B2B2B200D8D8D700E3E2E200E7E7E700EBEBEB00F2F2
      F200DDDEDE00B0AFAE008F8F8F00A2A2A200BBBCBB008C8A8C00376C37002883
      28007A817A00B5B5B50000000000000000000000000000000000000000000000
      000099CDF8005CC0FF004AAFFF003C83DD00596EAD009180A000CCADB2000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000098989800DFDFDF00EDEDED00E6E6E600E4E4E400E0E0
      E000DDDDDD00DADADA00D7D7D700D3D3D300E0E0E000ECECEC00A0A1A2009798
      980000000000000000000000000000000000FAF9F900AB908800B57C6D00AF7A
      6B00550E0000607480006FACC5005F8DA3006397AE006095AC00578BA4005488
      A100568DA7004A7E9800447B96004279950042789500477D9A0045809F00447C
      9A003B769600377393003C7695003B7B9E000000000000000000000000000000
      0000A4A4A4009F9F9F00C9C9C800CACBCB00C2C6C600BDC0C000BABABA00B7B6
      B600C3C5C500D8DBDD00CBCCCD00A09F9F0090919100A5A6A5008E968E004A55
      4A00565C5600AAA9AA0000000000000000000000000000000000000000000000
      0000000000009ACEF8005BBEFE0049AEFE003B83DE00546DAF009E94A6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000098989900E3E3E300F8F8F800EFEFEF00EAEAEA00E6E6E600E4E4
      E400E0E0E000DEDEDE00DADADA00D7D7D700D6D6D600EAEAEA00D0D0D0006B77
      810000000000000000000000000000000000E8E4E3008D635700CB958800CF9F
      920081493700653729006E423400713F2E00703C2A006F3D2B006F3D2C00723F
      2F006031230090726A00CDC1C400C1B1B00095797100613B2E005B3329005534
      3000A98E8900B799950071504A00B5B6B900000000000000000000000000DFDF
      DF009B9B9B00BFBFBF00BABEBF00AFA8A500A3897C00A17C6B009E756300A47F
      7000B1978F00A7928B00C4C0BE00E0E4E500BBBCBC008D8E8E00898989008383
      83007F807F000000000000000000000000000000000000000000000000000000
      000000000000000000009ACEF80057BCFF004CAFFF00598BCA008B909800827F
      880000000000C39E9700BD918200C09C8B00C7A79700D0B1A100CEADA0000000
      0000000000000000000000000000000000000000000000000000000000000000
      000090929400E5E5E500FFFFFF00F8F8F800F5F5F500F0F0F000ECECEC00E9E9
      E900E5E5E500E2E2E200DFDFDF00DCDCDC00D8D8D800DDDDDD00EBEBEB009497
      9A0097989800000000000000000000000000C3A59A00A1766A00CB988C00C69A
      8E00DDB3A900B37E6E005713000078250400802805007F2603007F2905007C25
      030070280B00E5B9A800FEE5DE00F2D4CA00DAAA98008B493000580F00006726
      1300CE958200D0917F0084402700BA9585000000000000000000D8D9D9006C6C
      6C00C0C1C100B8B9BA00AB968D00A36E5600A86F4E00AB755400A16A4800B58C
      7500D7C1B700BD9A8B009D716000C0AFA800F5F7F800D6D5D5009C9C9C009999
      9900000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009DD1F90090CAF20090CAF200A1989600AA8A
      8600CC9E8D00E0C0A200F3E7BB00FAF8CD00FEFED800FAF8D500F3EDCE00E0CF
      B900C9A9A0000000000000000000000000000000000000000000000000008A8F
      9300DCDCDC00FFFFFF00FFFFFF00FFFFFF00FAFAFA00F6F6F600F1F1F100EDED
      ED00EAEAEA00E6E6E600E4E4E400E0E0E000DDDDDD00D9D9D900E9E9E900CACA
      CA00687B8A00000000000000000000000000B0887900A9796B00CB998F00C89B
      9200D1AAA400DDB7B1008A5C4E0064220A00782C1000792C0E007D2F1100762C
      0E006D341C00E9CFC500F0E4E100E1CDC900EAD0C900CB9F9100591C06007538
      1F00965A450075341D00662003008A61500000000000DADBDB003F3E3F004445
      4400BFC0C100BAA59D00BF998B00A7715800A06542009A5C340091502300B490
      7600CFBDAF00CCB8AA00B28D7C009F725C00C2ABA000FFFFFF00DFDEDE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000090CAF200BA989600CE9B
      8800F4DAB100FFFAC900FFFECE00FFFFD100FFFFD900FFFFDF00FFFFE200FFFF
      F400F4EEE900CBAFA50000000000000000000000000000000000C3C3C300D0D0
      D000FFFFFF00FFFFFF00FFFFFF00E6E6E600F3F3F300FCFCFC00F7F7F700F3F3
      F300EFEFEF00EAEAEA00E6E6E600E4E4E400E2E2E200DDDDDD00DFDFDF00D9D9
      D900737E86000000000000000000000000009F706400AF7C6F00C9978D00C89B
      9000CCA49F00E9C4BC00C5978900622109007C32170083381B00823618007329
      0C007E4E3C00F2E6DF00F4E8E400E7D4D100E3CEC900EACAC100976453005015
      02008B4C3400BD775E0082391B00814F3B0000000000ABACAC00434141005C5F
      61007F6C6600BE928000A7705F009957410099523A00B7857500C8A29500D4B8
      B000D2BAB100CEB5AB00C1A69D00BAA09200A2766000BEABA200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CE9D8D00F4D7
      AE00FFF7C800FFF1BE00FFFBC700FFFFD600FFFFE100FFFFEA00FFFFF300FFFF
      FF00FFFFFF00F4EEE300BB968E000000000000000000C3C3C300BEBEBE00FFFF
      FF00FFFFFF00FFFFFF00BFBFBF008A8A8A00D8D8D800FFFFFF00FCFCFC00F8F8
      F800F3F3F300EFEFEF00EBEBEB00E7E7E700E4E4E400E0E0E000E0E0E000DFDF
      DF0082878B00000000000000000000000000B4836C00B17F7200C7988C00CFA1
      9700E5BBB100AA7E71006C35220089472D008E4A2F008241280090553E007F48
      3100B3958900FFFCF800F0E4E000EDDBD800E5CFCA00E4C9C300DEBBB0009A6A
      5900804834008D4F390096533B00895A490000000000C9C9C80072737400817F
      7E0097655200A56651009A643C00A0693C00A7754500DACAAF00E6E2C700DED3
      B400D8CDAE00D0BE9C00D4BC9800C3A99900A8806B00986C5900000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E0BCA100FFF6
      C500FFE9B600FFECB900FFFDCB00FFFFD800FFFFE200FFFFEF00FFFFFC00FFFF
      FE00FFFFFA00FFFFE400E0D0B600B18D8D0000000000C3C3C300E0E0E000FFFF
      FF00FFFFFF00B2B2B200858F960090939500F2F2F200FFFFFF00FFFFFF00FDFD
      FD00F8F8F800F6F6F600F1F1F100ECECEC00E9E9E900E6E6E600E4E4E400E3E3
      E3008D8F91000000000000000000000000009D6F5D00A36F5F00D5A29500BB8B
      7D00A77564008C5643008D523D009C5D47008B503A00A4786800EBDCD300EEE2
      DB00F3E6E100F8EDEB00F0E3E100ECDCD900E8D5D100E2CBC500DCBFB700C8A1
      95008F5F5100A26A5600A96A5800966B5D000000000000000000CED2D3007B63
      58009F5C4400A5603A0070667300636B8A0059638E00636E8A00758DB700677B
      AA00768BB0005E849A00787DC000C5A6AA00C6B1A100B28B7D00AF9C95000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D0A79D00F1DFB700FFF2
      C300FFDFAC00FFECB900FFFDCA00FFFFD700FFFFE300FFFFEF00FFFFF700FFFF
      F600FFFFED00FFFFDA00F3EDC900AD857F000000000000000000B0B0B000D2D2
      D200A7A7A7000000000000000000A4A5A600FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FDFDFD00FCFCFC00F8F8F800F2F2F200EDEDED00E7E7E700E7E7E700E4E4
      E40092939400000000000000000000000000A98477008D56430090503C005D24
      110089544000A56F5A00A76F59009D645000A97E6F00F3DFD600F9F0ED00F8F0
      EE00F7EEEC00F2E5E300EFE2E000EBDAD800EAD9D500DEC8C200CDB3AD00DDBD
      B500BD938900D49F9000AE736300A27C7000000000000000000000000000D4C0
      B8009D5A4500BD8054005C6197005B41BD00AE7E8600A7849D00C7B3D800C9BE
      E800D6C3EB00ADB3C5006287A500CBAEAF00C2AF9D00B8968600A98576000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CCA49A00FAF5C500FFEC
      BA00FFD9A600FFE6B300FFFBC700FFFFD500FFFFDF00FFFFE800FFFFEC00FFFF
      EB00FFFFE600FFFFD800F9F6CD00B9988B000000000000000000000000000000
      0000000000000000000000000000C3C3C300FFFFFF00FCFCFC00E9E9E900FFFF
      FF00FFFFFF00D2D2D200D6D6D600FDFDFD00F1F1F100ADADAD00D7D7D700E7E7
      E70092939400000000000000000000000000CFB8B10094604E007B351D007E45
      3100AE806D00AD7D6D00B17E6C00A7756200C19E9000FDEBE400F3E3E100EFE3
      E100F1E5E200F1E5E300F4E6E200F1DFDC00EAD8D400E2CEC800E0C9C400E6CA
      C500DCB6AF00C6928600915A4900C8ABA1000000000000000000000000000000
      0000C8A69C00D2A791007A7E700093849F00DFB88B00D9C0A600E5D0B700F3E9
      D500FFF8F000DAE6BD005A72BB00D4BCBB00C6B2A000C09F8F00956C5D000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CAA39900FEFDCA00FFE7
      B200FFD6A300FFE8B400FFF7C300FFFFCD00FFFFD700FFFFDD00FFFFDF00FFFF
      DF00FFFFDC00FFFFD300FDFCD000BC9889000000000000000000000000000000
      000000000000000000008F969A00E7E7E700FFFFFF00C0C0C00098989800FFFF
      FF00FFFFFF00BABABA00C0C0C000FFFFFF00F6F6F600A7A7A700D7D7D700EAEA
      EA0092949500000000000000000000000000F8F5F5009A695B00A0614C00B687
      7600AC897A00B08B7F00BA918200BA8F7F00B48E7F00C9ABA100F0DDD900F1E4
      E300EFE3E100F0E1DF00DBCCC900E4D3D000ECDAD500E8D5CF00E4CEC900E2CB
      C700E4C1B900BB8A7D0085564700ECE6E400000000000000000000000000EADF
      DA00C19C8F00CEA1810082949B00A09DD900F5DFC500DDD0C400DBC7B600E5DB
      CF00F2E2E000C6CCAA00627AB900CBAAA500B5957B00C6A79700936B5D000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000CCA59A00FAF4C400FFF3
      C700FFE9BD00FFE2B200FFECB800FFFBC800FFFFCF00FFFFD200FFFFD600FFFF
      D500FFFFD100FFFFCD00F8E9BE00B48B80000000000000000000000000000000
      00000000000000000000A1A1A100FFFFFF00FFFFFF009F9F9F009D9D9D00FFFF
      FF00FFFFFF00BFBFBF00C5C5C500FFFFFF00FCFCFC00A6A6A600D3D3D300E3E3
      E3008C91940000000000000000000000000000000000CAAFA8008D564500E0BA
      AF00C0A29700B0948A00B79C9000C1A09200C3A19500BA988D00C8ABA200F5E3
      DF00D8C8C600D1C0BD00D4C5C200CCBCB700E0CECA00EBDAD600E7D3CD00E4CF
      CB00F3CFC70094655600AD99910000000000000000000000000000000000EDE0
      DA00B7877300DFC0A2008AA2AE00A3A7E900FBF8F200EAE4DC00E8DECE00E8E0
      D300ECDAD200C5CBA4005C77A400BA8E8100945E3500A8745800A98476000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D0ACA400EEDCBA00FFFC
      DA00FFEECE00FFDFB200FFDAA800FFEDBB00FFF1BF00FFF6C400FFFBC900FFFB
      C800FFF3C100FFFECB00F3D6AC00A97F7D000000000000000000000000000000
      000000000000DCDCDC00C2C2C200FFFFFF00FAFAFA0090909000AAAAAA00FFFF
      FF00FFFFFF00C0C0C000C6C6C600FFFFFF00FFFFFF00A7A7A7009F9F9F00A7A7
      A7000000000000000000000000000000000000000000EEE7E50085615400C79E
      9100F8E1DA00CCB9B400C0ADA700BFAAA100C3ABA300C9B0A600C5ACA200DDCB
      C700EFE0DD00EBDCD900F7E8E600EFE0DE00E6D5D300ECDDD900E8D5D000F3DF
      DC00E1BEB500855D5000DFDAD80000000000000000000000000000000000F1E8
      E500C39A8600E6CDB00096AEB500ABAFE200FFFAF200F6F1ED00F8F2EC00F4EF
      E700FAE7E600CCD4B200536CAD00C1999000A6744E0094543100AC8F82000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DBBEA800FFFF
      ED00FFFDFB00FFFBF500FFE3BF00FFE4B400FFE5B200FFE5B300FFE7B500FFE6
      B400FFF6C100FFF1C300D3A98E00000000000000000000000000000000000000
      000000000000979EA300F1F1F100FFFFFF00E6E6E60089898900B3B3B300FFFF
      FF00FFFFFF00C0C0C000C7C7C700FFFFFF00FFFFFF00A8A9A900000000000000
      0000000000000000000000000000000000000000000000000000D0B9AF008155
      4500E6D3CB00FCF3EF00E3D5D300DACBC700D4C5C000D4C4BE00CFBFB900E0D3
      CE00F6EBE800F2E7E600F0E3E100F0E4E200F1E4E200EADDD900F2E4DF00F6E7
      E00095746B00C5B1A90000000000000000000000000000000000000000000000
      0000CDAF9D00D7B88E008EAEAF009CA6D800E9F5D500E6EFDB00EAF5DF00E8F2
      DB00E8E8D500C7D5A6005A74A900CBA8A100B17F5C009C6C5300DDD2CD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C9A49B00EDDE
      CF00FFFFFF00FFFFFF00FFEAC900FFEABF00FFDAA900FFD4A100FFDEAB00FFF4
      BF00FFF2C600EDC09800B0848100000000000000000000000000000000000000
      0000000000009B9FA100FFFFFF00FFFFFF00CFCFCF007F808100BDBEBE00FFFF
      FF00FFFFFF00C3C3C300B2B2B200FFFFFF00FFFFFF009B9C9D00000000000000
      000000000000000000000000000000000000000000000000000000000000BFA3
      990087655700E5DAD200FFFDFB00F6EBE800F2E6E200EFE4E100EBDFDB00F2E6
      E300F4EAE900F3E8E800F5E9E700F1E5E300F2E6E500FEF4F100F2EEE900A68C
      8100B3A097000000000000000000000000000000000000000000000000000000
      0000F9F5F200DEC49E00989DB7007C96CC006E80D0007D91D000758CD1006E82
      D100778AC900697FB4003C42EC00C8A2A700A8775100DFD2CC00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C4A1
      9B00EDE0D700FFFFFD00FFFFEC00FFF7CA00FFF2BE00FFF6C200FFFBCB00FFEB
      BD00EEBD9500B888810000000000000000000000000000000000000000000000
      000000000000C3C3C300C9C9C900E9E9E9009B9EA000C3C3C300BEBEBE00FFFF
      FF00FFFFFF00BDBDBD0089898A00BDBDBD00B4B4B40000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000BDA39A0087645700CCB7AE00F5F0EF00FFFFFF00FCF8F800F3F1F000F0EE
      EC00F5F2F200F8F6F600FDFAFA00FFFFFF00FEFAF700DFD1CB00A58D8300B9A0
      9700000000000000000000000000000000000000000000000000000000000000
      000000000000FEFCFA00D7C0B500E1D6D100F7F5FC00F7F4FC00F6F6FF00F5F3
      FC00F0E8EC00EBDFE100E1CBB700BB957B00E1D1C80000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B88F8C00CBA79100EDD6B100F8E9C000FDF0C500F9E6BB00F1D2AA00D3A7
      9300BC9290000000000000000000000000000000000000000000000000000000
      00000000000000000000C3C3C300C3C3C30000000000000000009B9FA100D6D6
      D600D6D6D600989C9E0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000DFCFCA009C7E73009D837800B6A29A00D5C6BD00F6E9E000FFF2
      EA00FFF3EC00FCEEE700DED1CB00BBAEA800AC9592009E837C00C2B3AF000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FCFAF500DECCB600DFCFB800F0E8D900F6F1E700F4ED
      E100EADDCC00D8C0AA00C8AE9900EBE4E0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AE868500AD827B00B98D8500C5989100C49999000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C3C3
      C300C3C3C3000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FAF5F400D0C3BD00A17D6F00A5796900A67D6F00A57D
      6E00AB817200AE847700AB837600A37F7300DDCDBC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F1EAE300F9F5F000FCF9F500FBF8
      F400F7F2ED00F0E8E200F7F5F400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B9BAEC00CED1EF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000DCDCDB00C6CCCB00B1B5B500A9AAAB00ACAB
      AB00A9A9AA00A8ABAC00C0C3C300DFE1E000F0EFEF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000CFD0EE009798
      C9000000000000000000000000007578D5001A1FCC001315B200ABACDA000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D8D8D800BABABA00ACABAB00A5A4A4009B9B9A00939393009091
      9000949595009B9B9A00A6A6A600ACACAC00B2B2B200D2D2D200F3F3F3000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009899E0000000
      A500888AC400000000007272D2003F45D900737AF900474AE0001113B100A8A8
      D900000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C5C5C500B3B3B300A9A9A900818181005252520033333300242424001F1F
      1F0026262600373737005858580084848400ABABAB00B3B3B300B7B7B700E8E8
      E8000000000000000000000000000000000000000000DCACAC00B97E7E00B685
      8500B5838300B4838300B5828200B4828200B4828200B3808000B3818100B380
      8000B37F7F00B27E7E00B17F7F00B27F7F00B2808000B27F7F00B27F7F00B27E
      7E00B2828200AF797900CE959500000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009EA0E2001E22
      C5001D1EB2003B3DB2005D64DE008289F2006C73EC006F75F2004D52E2001517
      B200A8A9D8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C9C9
      C900C3C3C300929292003B3B3B0009090900020202000A0A0A00101010001212
      12000F0F0F0008080800010101000C0C0C004141410096969600C6C6C600B5B5
      B500E1E2E100000000000000000000000000B46C6C00C7A7A700E1F0F000E0E8
      E800DFEAEA00DEE9E900DDE8E800DCE7E700DAE6E600D9E4E400D8E4E400D7E2
      E200D5E0E000D5E0E000D3DFDF00D2DDDD00D2DDDD00D2DEDE00D4E0E000D3E0
      E000D2DBDB00D5E4E400B0929200E5D2D2000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009B9CE1004148
      E5003E43DA005A60DC009BA4EF00858EEA007D85EC00737AEB00777EF2005358
      E2001518B400ACADDB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C9C9C900CCCC
      CC007A7A7A0010101000010101001B1B1B002828280028282800252525002424
      240025252500262626002424240015151500000000001313130078787800D4D4
      D400B8B9B800DFDFDF000000000000000000B07D7D00DEE5E500BB797900A23A
      3A00A7454500A6434300A6424200A5424200A6424200A5414100A5414100A541
      4100A4404000A5404000A6414100A6424200A5424200A33F3F009F3737009D2F
      2F009E333300B4797900CFE2E2009E5050000000000000000000000000000000
      00000000000000000000000000000000000000000000000000009A9BE4002B2F
      E600595EF8005E64F1006D72ED008890EB009098E900868EEB00777EE900818A
      F4005256DD000406AC00ADADDC0000000000000000000000000000000000BBBB
      FF002F2FFF003737FF00EBEBFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6D6D600D6D6D6008484
      8500050507000E0E0E002A2A2A00272727001E1E1E0020202000252525002727
      2700242424001D1D1D001B1B1B0026262600252525000B0B0B00040303007A7A
      7900E7E7E700BFBFBF00E8E8E80000000000B6888800E1E4E400AB4848008700
      00008F0000008E0000008F0000008E0000008F0000008F0000008F0000008F00
      00008F0000008E0000008A00000087000000870000008D010100981F1F00B045
      450092050500A4474700D1E6E600882727000000000000000000000000000000
      000000000000CBCBFF001313FF001010FF007E7DFD00000000009A97E6001C1F
      E9003F43F6004448F1004D51F3005B60F000757CED008890E800949DED00777C
      E2001920C0003537B800D1D2EA00000000000000000000000000BABAFF001E1E
      FF004545FF003535FF004242FF00A7A7FF00A4A4FF00A7A7FF00A7A7FF00A7A7
      FF00A7A7FF00A7A7FF00A7A7FF00A7A7FF00A7A7FF00A7A7FF00A7A7FF00A7A7
      FF00A9A9FF00B0B0FE00DDDDFD000000000000000000DADAD900B8B8B9001515
      14000F0F0B0039393A00242424001B1B1C003131310037373600353535003434
      340034343400343434002E2E2E001B1B1A002020200035353500090A09000808
      0800A4A5A400EDEDED00C5C4C50000000000B88A8A00E9ECEC00B25555008D01
      0100960606009401010095020200950202009402020094020200940101009402
      02009200000093090900A4333300B75E5E00CD8B8B00E1BCBC00F6EAEA00DCAF
      AF00910D0D00AE5A5A00D0E6E600913131000000000000000000000000000000
      0000BCBCFF001B1BFF005454FF006666FF001919FE009192FF00C2BEF2001415
      F100292CF7003336F4003E41F300464AF2004D51EF00646AF3007077E600131A
      BE002A179F00DCDBE700000000000000000000000000000000002525FF003F3F
      FF008888FF008080FF006666FF002929FF002A2AFF002929FF002929FF002929
      FF002929FF002929FF002929FF002929FF002929FF002929FF002828FF002828
      FF002F2FFF000000FD007F7FFE0000000000D7D7DD00DCDCDC00545457000B0B
      00004141280021212200262627004040400034343400333333003C3C3C003D3D
      3D003A3A3B0033333300323230003C3C3A00272825001F1F1C00282924000404
      000034343000DEDFDF00CCCCCC00EFEFEF00BA8C8C00EDF0F000B35555008B00
      0000940202009100000092000000920000009200000093000000930202009100
      0000A1242400E0B8B800E6C6C600F6EAEA00FFFFFF00FFFFFF00EEDADA00A121
      2100970A0A00B76A6A00D1E5E500973232000000000000000000000000000000
      00007575FF001212FF008787FF008989FF004646FD002525FF00C6C2F5000402
      F500161AF9002223F6002C2FF6003539F4004347F4004447E4001319C0004632
      9B00DFC8D00000000000000000000000000000000000000000004C4CFF002B2B
      FF007F7FFF007575FF004A4AFF003737FF003737FF003737FF003737FF003737
      FF003737FF003737FF003737FF003737FF003737FF003737FF003737FF003737
      FF003C3CFF001F1FFE009B9BFF0000000000CBCBCF00B7B7B700171718001515
      13002F2F2A001C1C1C00363636003C3C3C003E3E3E004A4A4A00484747004847
      4600494945004A4B460042423B00414139004040360027271A00333325002728
      17001111010097989200DDDDDD00E3E3E300BD8E8E00F1F5F500B55757008D00
      0000950808009405050095070700970B0B00990D0D00990F0F009C1414009C16
      16009C151500B6555500D5A0A000FBF8F800F6ECEC00FAF6F600B85B5B009D1B
      1B00A8303000BC767600D2E5E5009E3C3C000000000000000000000000000000
      0000AFAFFF001414FF006060FF007070FF001D1DFE008080FF00B5B7FB000101
      F9000608FA000D0DF9001618F8002022F6002C2DF8002527DA000101A9009077
      AA00000000000000000000000000000000000000000000000000000000004545
      FE001616FF000B0BFF007878FF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D5D5D5008B8B8B00050504002121
      2100222223002828280039393900333333005252520048484800464644004B4B
      46004D4D42004D4E3E00626251004C4D35004A4A2E00474928003C3D16004446
      1F001E1E000063634F00D2D2D100D3D3D300C0909000F5F7F700BB6060009404
      04009E1C1C009D1818009F1E1E009E1B1B009F1C1C00A3282800A22222009E1C
      1C00B95B5B00E7C8C800FFFFFF00D9A8A800EBD4D400D8A5A500A42A2A00AD40
      4000B44C4C00C3858500D3E4E400A64C4C000000000000000000000000000000
      000000000000B6B6FF000A0AFE000C0CFF006867FD00000000009796F8000000
      F6000000FA000101F9000B0AF6001214F5001F26F8003339FB002F37DF001A18
      AF008A71AB000000000000000000000000000000000000000000000000000000
      00008F8FFE009A9AFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000CDCDCD0070707000010101002727
      2700212121003030300035353500434342004545450048484600555650005D5D
      53005E604D005F604700606141006A6C47005F603400676838005B5D25006263
      30004647190059593900C5C5BE00B9B9B900C3989800F5F8F800C06E6E009C17
      1700A62E2E00A52D2D00A42B2B00AD3D3D00B0464600A4282800AE414100D7A4
      A400FFFFFF00F8F0F000BE6B6B00C4767600EBD2D200B24A4A00B1494900B85D
      5D00C16C6C00C78F8F00D4E4E400AC5C5C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C9BEEF006765
      E400787AEC008483E9008388E9008E8ADD009081C8008E7EBF00A190BF008F7B
      B5006A4991000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C2C2C20068686800020202002828
      27002020200032323200333335003D3D4400474748004C4C46006D6C64009797
      9500939172009491640099965E00A29E5E00A4A05100A6A04F00A29C3E00ABA5
      4100A9A22C0097923700B1B3A800C4C4C100C59E9E00F5F7F700C67B7B00A62D
      2D00AE414100AD3F3F00AB393900E7C8C800CD8C8C00BF696900F4E7E700FFFF
      FF00DDB2B200B7575700B1464600BD676700BE676700B95A5A00C06D6D00C983
      8300C87D7D00CD9A9A00D5E3E300B36D6F00EFDD6F00FBFBAB00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F4F6ED00DCDDD500E1E2DB00E6E6DF00F5F3EE0000000000000000000000
      000000000000000000000000000000000000F6F17E00FAF7AE00FEFFE700FBFB
      D600000000000000000000000000000000000000000000000000000000000000
      0000EBEDEB00D6D7D600DCDDDC00E0E0E000F1F1F10000000000000000000000
      000000000000000000000000000000000000B4B4B40075757500030303002525
      2500222220002A2A2E00464637008989490054544D004D4D4A0064645A008F8F
      7C0085845A008485470093953D00A5A643009F9F2D00C8C82E00CDCE1700A3A4
      20009F9F030092931F00AEAFA300B9B9B600C8A5A500F6F8F800CB888800AE40
      4000B8575700AF454500D5A1A100F4E7E700E1B9B900FDFCFC00F2E3E300D399
      9900BF6B6B00C3757500C77D7D00C4777700C67C7C00CC8A8A00CF909000D095
      9500CE8C8C00D2A6A600D7E2E200BB7F8200F0DC6E00E9D02C00ECD43B00F2E2
      5500F6F39400FBFAB900FAF9C900000000000000000000000000000000000000
      0000D5DCD800ABAFAC00888E890092919300CFCFD00000000000000000000000
      000000000000000000000000000000000000F4EA7600E7CB2800EDD63E00F2E3
      5700F6F49A00FBFAB800FBFBCE00000000000000000000000000000000000000
      0000D2DAD500A9AEA900888E880093939300D6D6D60000000000000000000000
      000000000000000000000000000000000000ACACAC008B8B8B000A0A0A001E1E
      1F002323220026262A0053534800787845005E5E5800545351005B5B4F006262
      4B0072734D007E80490094974E00A0A24600989A3300CACB3600CFCF1A00A1A0
      26009B9C040092923200AAABA000ADADA700CBACAC00F5F7F700D1959500B859
      5900BA5F5F00C7818100FAF5F500FAF4F400FDFCFC00EAD0D000D8A8A800CE8F
      8F00D2969600D3999900D2979700D49C9C00D59E9E00D59E9E00D59E9E00D6A2
      A200D59C9C00D8B2B200D8E2E200C18F9000F4E58900E7C71E00DBAD0000DEB4
      0200E8D13900EBD63F00EFD74300F6EE8500F7F3960000000000000000000000
      0000ECE7E200E3E7E300D3D8D400B5BBB500959A9400DADBD800F2F2F0000000
      000000000000000000000000000000000000F7F28E00E4C01400DCAF0000DEB4
      0400E9D33E00EBD53B00F0D94900F6EE8600F8F49A0000000000000000000000
      0000EBE7E100E3E7E400D2D7D300B2B9B200969B9500DEDFDD00F3F3F1000000
      000000000000000000000000000000000000CACACA009E9E9E00292929000E0E
      0E00292929002D2D2E004D4D4F003F3F47005656550064635F005E5E53006767
      540071725000818251009396590095984000A1A43F009E9F300098991E00A0A1
      210096960000959652009C9E9500DBDBD500CEB2B200F6F7F700D8A3A300BE69
      6900C57A7A00F1E1E100FFFFFF00FFFFFF00F9F5F500F2E1E100F5ECEC00DDB3
      B300D49D9D00D7A4A400D8A5A500D9A8A800D9A9A900DBACAC00DBAEAE00DCB2
      B200DCAEAE00DFBFBF00D9E2E100C89DA100FBF3E700F0E25C00E4C32300E4C0
      1700E6C41F00E5C72300DCB20000DCB40000E6CC2500FFF99E00EEF8FE00BAEB
      F600BAE3E700DEE2DE00E3E5E100E0E6E200CCD1CF00A8AEAE00B4B8BC000000
      000000000000000000000000000000000000FFFEEB00EEDC5000E5C42300E4BF
      1600E7C52200E4C51F00DDB20000DDB50000E7CF2A00FFFBA800E9F6FE00B9EB
      F600BCE2E600E0E2DE00E3E5E100E0E6E200CACECD00A5ACAB00BABDC1000000
      000000000000000000000000000000000000DADADA00A0A0A000676767000000
      0000232323003B3B3B00414141005D5D5C005252500050504E00565657006161
      580070705600787950007B7D40008F914C009C9E490096982A009C9E2D009395
      0F008A8A0B009A9A7A009C9D8F00F8F8F700D3B9B900F7F8F800DDB0B000C67A
      7A00E9CECE00FEFEFE00F4E8E800EEDADA00E7CBCB00E2BEBE00DCAFAF00DCAF
      AF00DEB3B300DEB4B400DFB5B500DFB8B800E0BABA00E1BCBC00E2BDBD00E3C1
      C100E3BEBE00E3CACA00D9E0DF00CEADB30000000000F5F17600E9CD3300E6C4
      1E00EAC82B00EEDC5500E9CE3200E3C21E00E5BC0200E4E98C0066C8F50038BC
      E5004CD4F6009FE0EB00DBE2DE00E3E3DD00EAEBD300EEF0C700F2F2C3000000
      000000000000000000000000000000000000FDFDE200F3EC6E00E8CC2F00E7C5
      1F00EACA2D00EEDC5600E8CD3000E3C11C00E8BD0300DEEB99005DC5F60038BD
      E50050D5F600A5E0EA00DCE3DE00E3E3DC00EAEBD400EEF0C400F3F3C7000000
      00000000000000000000000000000000000000000000BEBEBE00949494002E2E
      2E000202020040404000525252004646450055555500565656007C7C52006E6E
      51005E5F4F006D6E4D007B7C5000808146007F813500969847008F902A007778
      00008B8C480090917F00C9CABF0000000000D3BEBE00F6F8F800E0B8B800D59F
      9F00E7CCCC00E0BABA00DDB1B100DCAFAF00DEB0B000DFB5B500E1BBBB00E3BD
      BD00E4BFBF00E5C1C100E4C4C400E6C6C600E6C8C800E8CBCB00E9CDCD00EAD0
      D000EACECE00EBDADA00DFE4E400D7C0BF0000000000FCF9C000E4BF0A00DBAF
      0000E0B70200E6CD3200E3C82400E2C31D00F0CF3000DFE179005CC3DD0053BF
      E8004DBFE60055CDEF0073DDF3006EDDF700ABCF8800EFCD3100F1E76A00F8F3
      9400F9F49800000000000000000000000000FEFEFB00FBF6B400E2BC0400DBAF
      0000E0B80300E7CE3400E3C72200E2C31E00F3D03200D8E0810057C2E10053BF
      E8004DBFE60057CFEF0073DEF30070DCF400B1CE7E00F0CF3100F1E97000F8F3
      9300F9F59B000000000000000000000000000000000000000000B8B8B8009292
      920017171700101010005454540055555400484849004B4B4D00C0C052008E8E
      53005353510064654D00646642006D6F3D007F814B00808140005D5F02007174
      2800919378009FA08900F1F2ED0000000000DCCCCC00F7F8F800EBD2D200E0BC
      BC00E2C0C000E4C2C200E6C7C700E6CACA00E7CCCC00E8CECE00E8CECE00E8CF
      CF00EAD2D200EAD3D300EAD5D500EBD7D700EDD9D900EDDBDB00EEDDDD00EFDF
      DF00EEDFDF00F3E9E900EFF3F300D8C5C20000000000FCFDD100EEDB3C00E6C6
      1C00E4BF1400E1BC0B00E5C92900DCB00000DCB30000EEC9200098DAC80043BB
      EC004BBEE50093D0E600D1E4E200C1E4F000C2DAAE00DEBE1200E5C21600E9CC
      2C00E4C11A00EBDA4700F9F5A6000000000000000000FBFBC500EED93700E6C6
      1B00E3BF1200E1BD0D00E5C82700DCB00000DCB30000EECD2A008FD7D10042BB
      EC004DBEE5009AD2E600D1E4E200BFE3F000C4D8A400DFBC0C00E6C41B00E9CB
      2900E5C21D00ECDB4A00F9F6AE00000000000000000000000000E3E3E300BBBB
      BB009E9E9E001616160000000000404040005D5D5D005A5A5C005A5A56005656
      5100585750005F5F51006A6B5300737452006061330035360000545620009292
      790097988500E5E5DF000000000000000000D4C0C000F2ECEC00FEFEFE00FDFB
      FB00FEFDFD00FFFDFD00FEFCFC00FDFCFC00FDFCFC00FDFCFC00FDFBFB00FDFC
      FC00FDFCFC00FCFCFC00FCFCFC00FDFCFC00FCFBFB00FBFCFC00FBFBFB00FBFA
      FA00FAFAFA00FDFCFC00E6E7E700E3DDDD0000000000FDFCE900FCFDD400FCFA
      C400F5EC6E00F3E46300F3E86800ECD23700E1C41E00F0C20A00BDD6840046BE
      EA0076C8E400E0E3DE00E1E1DC00E6E8E000EFDE6800E6C41700EBD13B00EDDB
      5100E8C82C00E4BC0F00F0E051000000000000000000FCFBE300FDFDD400FCF9
      BE00F5EB6C00F3E46400F3E76700EBD13500E0C21D00F1C40D00B5D58E0042BD
      EC007ECBE400E2E3DD00E0E1DD00E7E8DD00EFDB5D00E7C41700EBD23E00EDDA
      5000E7C62A00E4BD1000F1E35A0000000000000000000000000000000000D9D9
      D900CCCCCC00BFBFBF004A4A4A00000000000E0E0E002E2E2D00414141004B4B
      4A004D4D490048483F003838280020210B001F21000065664800AAAA9900ABAB
      A100E5E5E400000000000000000000000000EBE5E500E6DEDE00E4DDDD00E1DC
      DC00E2DDDD00E1DEDE00E2DFDF00E3DFDF00E4E0E000E3E1E100E3E1E100E4E2
      E200E3E3E300E4E3E300E4E3E300E4E4E400E5E3E300E5E4E400E4E3E300E3E2
      E200E3E3E300E8E6E600E1E1E100000000000000000000000000000000000000
      0000000000000000000000000000FCFBC800F6F18800F5EF8A00F5EF8C0084D0
      D800C0DEE700EAE8E000DFE5E500E7ECD400EFE35700E0BB0C00DEB40100E9CE
      3500E7CB2B00E5C01900F2DF5500FCFCDE000000000000000000000000000000
      00000000000000000000FEFFF300FCFAC100F5F08700F6EE8A00F1EE8F007FCF
      DD00C7E0E700E8E8E000DFE5E600E9EDCD00EEE05000E0B90A00DEB40100E9D1
      3A00E6C92600E6C01B00F2E15C00FDFDE4000000000000000000000000000000
      0000F3F3F300E2E2E200E8E8E800AEAEAE00555555001B1B1B000F0F0E000C0C
      0C000C0C0C000F0F0E0023241A005F5F5300AAABA300CCCDC900B3B4B200E5E5
      E400000000000000000000000000000000000000000000000000EEEAEA00E3DD
      DD00E6E1E100E5E1E100E6E2E200E6E3E300E7E4E400E6E5E500E6E6E600E7E6
      E600E8E6E600E8E7E700E9E8E800E8E8E800E9E8E800EAE8E800E9E9E900EAE9
      E900E9E8E800EFEEEE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F9F49300EFE05100E9CD2800ECD5
      3A00E7C92400DFB60500E4BD0A00F5EB76000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FDFDE900F9F38D00EFDE4E00E9CD2700EDD6
      3C00E6C62000DEB50400E5BF0E00F6EE82000000000000000000000000000000
      000000000000F9F9F900E0E0E000EEEEEE00EFEFEF00E1E1E100BCBCBD00A8A8
      A900A8A8A800BBBBBB00DDDDDD00EFEFEF00E9E9E900D1D1D100E5E5E5000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCFCCF00FDFFDC00FDFD
      D700F7EE8900F2E56300F0DE5600F6EF80000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FDFCEB00FCFCD000FDFFDC00FDFC
      D600F6ED8300F1E46200F0DE5500F6F089000000000000000000000000000000
      0000000000000000000000000000F3F3F300E9E9E900E9E9E900EDEDED00F0F0
      F000F0F0F100ECECEC00E7E7E700EBECEB00F6F7F60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFFE600FEFFD700FFFFEB00FDFDCF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDFFE100FFFFD800FFFFEA00FCFCCF000000000000000000000000000000
      00000000000000000000000000000000000000000000EFEFEF00ECECEC00EAEA
      E900EAEAE900ECECEC0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000600000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFFFFFFFF000000FEE1FFFF
      70FFFF70FF000000FC40FFFE207FFE207F000000FC007FFE003FFE003F000000
      FC003FFE001FFE001F000000FC001FFE000FFE000F000000F8001FFC000FFC00
      0F000000F0003FF8001FF8001F000000E0003FF0001FF00000000000C0001FE0
      000FE0000000000080000FC00007C00000000000800007C00003C00000000000
      800007C00003C00000000000800007C00003C00000000000C00007E00003E000
      00000000E00007F00003F00000000000E00007F00003F00000000000E00007F0
      0003F00000000000E00003F00003F00000000000F00001F80003F80000000000
      F80018FC0003FC0000000000FC003CFE0003FE0000000000FE007FFF0003FF00
      00000000FF00FFFF8073FF8000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      E03FFFFFFFFFB87FE0000FFFC01FFFFC0FFF103FE00007FFC00FFFFF87FF001F
      E00007FF8003FFFFF3FF000FE00003FF0001FFF819FF0007E00001FE0000FFFF
      CCFE0007E00000FC0000FFF8E6FC000FE00000FC0000FFFE32F8000FF80001F8
      0001FFFF9BF00007F00003F000037FFFCBE00003E00007F0000707F07FE00001
      E0000FF0000701F07FE00001E0000FF0000700301FE00001F00007F800030000
      1FF00001F80007FC000380001FF80001F80007FC0003800003F80001F80007FC
      0003800001F80001F80007FC0007E00001F80003F80007FC0007FE0000FC0003
      F8000FFE000FFFFF00FE0007FC001FFF001FFFFFF0FF000FFE003FFF803FFFFF
      FFFF801FFF007FFFC07FFFFFFFFFC03FFFFFFFFF00FFFFC1FF87FFFFFFFFFFFC
      003FFF80FF83FFFFFFFFFFF00000FF007F81FFFFFF807FE00000FE003F80FFFF
      FF003FC00000FE000FC07FFFFE001FC00000FC0007E03FFFFE001F800000F800
      03F01FFFFC000F000000F00003F81FFFF8000F000000E00007FC081FF0000700
      0000C0000FFE0007E0000700000080001FFF8003C0000700000080003FFFC001
      80000700000080003FFFC000800007000000C0001FFF8000C60007000000E000
      1FFF8000FE0007000000F0001FFF8000FC0007000000E0001FFF8000FC000780
      0001E0001FFF8000F8000F800001E0001FFFC001F8003FC00003F0001FFFC001
      F8003FE00007F0003FFFE003F8007FF0000FF8007FFFF007FCC3FFF8001FFC00
      FFFFFC1FFFE7FFFC007FFF01FFFFFFFFFFFFFFFFFF3FFFFFFFFE007FFFFFFFFF
      CE1FFFFFFFF8001FFFFFFFFFC40FFFFFFFF0000F800001FFC007FFFFFFE00007
      000000FFC003FFFFFFC00003000000FFC001E1FFFF800001000000F84001C000
      01800001000000F00003C00001000000000000F00007C00001000000000000F0
      000FE1FFFF000000000000F84007F3FFFF000000000000FFC007FFFFFF000000
      0000003FF07F0FF07F00000000000001F07F01F07F00000000000000701F0070
      1F00000000000000001F00001F00000000000080001F00001F80000100000080
      0007000007C00001000000800001800001C00003000000800001800001E00007
      000001FE0000FC0000F0000FC00003FFFF00FFFE00F8001FFFFFFFFFFF80FFFF
      00FE007FFFFFFFFFFFF0FFFFF0FF83FF00000000000000000000000000000000
      000000000000}
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
    Left = 113
    Top = 212
  end
  object TBImageList1_24: TTBImageList
    Height = 17
    Width = 17
    Left = 12
    Top = 233
    Bitmap = {
      494C010119001B00040011001100FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
  object PMNRObject: TPopupMenu
    Left = 749
    Top = 225
    object NGoHim: TMenuItem
      Caption = #1054#1090#1089#1083#1077#1078#1080#1074#1072#1090#1100
    end
  end
  object ImagesSrc24: TTBImageList
    Height = 24
    Width = 24
    Left = 12
    Top = 201
    Bitmap = {
      494C010104000600040018001800FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000003000000001002000000000000048
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C8C9
      CB00AEB9C800D5C6B600D1D5D400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E3DAD600AA9BAA00A67A8800A8686B0067696C008C596600A9698F008B8D
      8F00D1D5D4000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EBE6D500EAE8E800EAE8E800EBE6
      D500EAE8E800EBE6D500EBE6D500EAE8E8000000000000000000000000000000
      00000000000000000000000000000000000000000000E3DAD600AB957700AF88
      5200AF885200B0887300AB957700B9B7B800E3DAD600EAE8E800EAE8E800EAE8
      E800E3DAD600EBE6D500EAE8E800EAE8E8000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000D1D5
      D400AFA8A800A67A8800A8686B008C5966008C5966005B5856008C596600AE7A
      7300AFA8A800EAE8E80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E3DAD600AA9B
      AA00A67A8800B4898900B09A9900AFA8A800AA9BAA00B09A9900AE7A73008C59
      66008C596600A67A880000000000000000000000000000000000000000000000
      00000000000000000000EBE6D500D5C6B600CBA37300C5A98800F2C89100ECCA
      AD00ECCAAD00E1B99200C5A98800D0936C00E1B99200E3DAD600000000000000
      000000000000000000000000000000000000EAE9F800E7A86F00EB900900EB90
      0900D9955400EBA95200D9955400AB9577009469520090766E006D6651006D66
      51009E584D005B58560067696C0091857400AFA8A800E3DAD600000000000000
      00000000000000000000000000000000000000000000EAE9F800B09A9900B489
      8900AA9BAA00B9B7B800DBB9B300AEB9C800DBB9B300D6ABAA00B09A9900A67A
      88008C59660090766E00DCB9CB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C8C9CB00B4898900C8C9
      CB00E3DAD600EAE8E800D1D5D400D1D5D400DBB9B300AA9BAA00DA9B9400D6AB
      AA00B48989008C5966008C596600000000000000000000000000000000000000
      000000000000E5C9C800D0936C00E18B3500D1793300E18B3500E18B3500E18B
      3500D9955400E18B3500E18B3500EB900900B8681400B8681400D9955400D995
      5400D9955400D9955400E1B9920000000000D5C6B600EB900900EB900900AB95
      77000000000000000000C5A98800B5753600B8681400A3593300CE591000A359
      3300A3593300A0490E0098492B006E380D006548330090766E00B9B7B8000000
      000000000000000000000000000000000000EAE9F800BEA99800DCB9CB00E3DA
      D600EAE8E800E3DAD600D1D5D400E5C9C800B9B7B800B4898900B4898900D6AB
      AA00B09A9900A8686B0067696C00C8C9CB000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EAE8E800AFA8A800E3DAD600EAE8
      E800E3DAD600E3DAD600D1D5D400D1D5D400D6ABAA00D6ABAA008B8D8F009185
      7400D6ABAA00B09A99008C5966008B8D8F000000000000000000000000000000
      0000C5A98800D9955400F1BC7700F5A93400D9955400E3DAD600EFD7B200D5C6
      B600C5A98800E1B99200C8C9CB00D5C6B600E5C9C800E3DAD600EFD7B200ECCA
      AD00E5C9C800E1B99200D7894F0000000000F1BC7700FAA71B00A768310088A0
      9B0000000000C8B49800B5753600B8681400A7683100D1583100AA694E00D158
      3100D1583100CE685300CE685300DB595600A0490E00692A0D005B585600AFA8
      A80000000000000000000000000000000000DBB9B300C8C9CB00EAE8E800EAE8
      E800EAE8E800E3DAD600E3DAD600C8C9CB00D6ABAA00B9B7B8008B8D8F00AE7A
      7300AA9BAA00DA9B9400A8686B00A7AAB5000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DCB9CB00D1D5D400EAE8E800EAE9
      F800EAE8E800EAE8E800E3DAD600C8C9CB00D6ABAA00AEB9C800B4898900A67A
      8800B09A9900D6ABAA00AE7A7300A67A8800000000000000000000000000D093
      6C00E18B3500FAC05E00F2C89100D9955400D0936C00EAE8E800D1D5D400EAE8
      E800D5C6B600C4B6A800EBE6D500EAE9F800EAE8E800EAE9F800FCFCFB00EAE9
      F800FCFCFB00D1D5D400D995540000000000E1B99200FAA71B00B97714009076
      6E00C5A98800B8681400D349100098492B00A0490E00983908006B5631009839
      080098390800A0490E00A0490E00A3593300CE685300D15831006E380D006548
      3300AFA8A800000000000000000000000000B9B7B800EAE8E800EAE8E800EAE9
      F800EAE8E800EAE8E800EAE8E800DBB9B300B9B7B800D6ABAA00A67A8800B489
      8900B09A9900D6ABAA00AE7A7300A7AAB500E3DAD600D6ABAA00A67A88008B8D
      8F00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E5C9C800E3DAD600EAE9F800FCFC
      FB00FCFCFB00EAE8E800EAE8E800C8C9CB00D6ABAA00D6ABAA00A67A8800AE7A
      7300DA9B9400D6ABAA00A67A8800B9B7B8000000000000000000C8B49800D179
      3300FAC05E00F1BC7700F2C89100EBA95200AF885200C8B49800C5A98800C8B4
      9800C4B6A800C5A98800BEA99800D5C6B600D5C6B600D1D5D400D5C6B600D1D5
      D400EAE8E800E1B99200D995540000000000F2C89100F5A93400E18B35006B56
      3100A6580E0098390800983908008E270E0098390800983908005D5811006E38
      0D00983908008E270E009839080098390800A0490E00A8484B00D1583100692A
      0D005B585600DBB9B3000000000000000000E5C9C800E3DAD600FCFCFB00FCFC
      FB00FCFCFB00EAE8E800EAE8E800B9B7B800D6ABAA00B4898900AE7A7300AE7A
      7300DA9B9400AA9BAA00AE7A7300A67A8800B4898900AA9BAA00B48989007577
      77008C5966008494AB0000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E5C9C800EAE8E800EAE8
      E800EAE8E800EAE8E800EAE8E800B9B7B800D6ABAA00B4898900AE7A7300C974
      7000B09A9900DA9B9400B09A99000000000000000000E5C9C800D1793300F5A9
      3400FAC05E00F1BC7700ECCAAD00D9955400D7894F00C4B6A800BEA99800C8B4
      9800BEA99800C8B49800C5A98800C5A98800C5A98800C5A98800BEA99800C5A9
      8800BEA99800D0936C00D995540000000000EBE6D500EBA95200F5A93400B977
      1400983908009839080098390800A0490E0098390800A0490E001B6607001B66
      0700D349100098390800983908008E270E009839080098390800D15831009E58
      4D00692A0D0091857400E3DAD6000000000000000000C8C9CB00E3DAD600C8C9
      CB00DBB9B300E3DAD600DCD9F200D6ABAA00D6ABAA00A67A8800C9747000A67A
      8800DA9B9400DA9B9400A67A8800AA9BAA00DBB9B300DBB9B300AFA8A800A7AA
      B500B4898900A8686B008C596600B09A99000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E3DAD600D6AB
      AA00B4898900DBB9B300B9B7B800D6ABAA00DA9B9400C9747000C9747000A868
      6B00B4898900DCB9CB00000000000000000000000000AF885200E18B3500EBA9
      5200E7A86F00F1BC7700F2C89100D9955400AF885200C4B6A800C5A98800C8B4
      9800D5C6B600C5A98800BEA99800C8B49800BEA99800BEA99800C8B49800BEA9
      9800C4B6A800C5A98800B97714000000000000000000F1BC7700E4C02C00E18B
      35009839080098390800D3491000A0490E00CE591000A6580E001B6607001B66
      07001B660700CE591000D3491000A0490E00983908008E270E006B563100AA69
      4E0098492B005B494C00D5C6B600000000000000000000000000E5C9C800B489
      8900DAA88D00D6ABAA00B09A9900B09A9900B0887300C9747000A8686B00CE68
      5300D6ABAA00B9B7B800DCB9CB00D5C6B600DBB9B300D6ABAA00B4898900AA9B
      AA00AFA8A800AA9BAA00B48989005B5856000000000000000000000000000000
      0000EAE9F800C8C9CB00B09A9900AE7A730090766E0090766E00B4898900B489
      8900EBA95200F1BC7700D6ABAA00B4898900C9747000A8686B00A8686B00DB59
      5600A8686B00000000000000000000000000E3DAD600D3693500F5A93400EBA9
      5200E7A86F00E7A86F00F1BC7700E18B3500D7894F00C4B6A800C5A98800BEA9
      9800D1D5D400D5C6B600C5A98800C4B6A800D1D5D400C8B49800BEA99800C5A9
      8800C8C9CB00D5C6B600A6580E00EFD7B20000000000EFD7B200EBA95200FBBB
      4500CE591000A6580E00CE591000CE591000D1670900596A0600167806001678
      06001B660700596A0600CE591000A6580E00D3491000983908006C6C34006C6C
      3400A35933005A392F008B8D8F00000000000000000000000000E5C9C800B898
      8800FAA71B00E7A86F00E1B99200DA9B9400C9747000A8686B00DB595600DB59
      5600E5C9C800E5C9C800DBB9B300D6ABAA00AA9BAA00DA9B9400AE7A73009076
      6E00B4898900BEA998009CC2A4008C596600000000000000000000000000EAE8
      E800D0936C00A7683100D1583100A3593300D158310098492B009E584D00ED87
      8C00FAA71B00FAA71B00EBA95200F1BC7700DA9B9400ED878C00C9747000A848
      4B008C596600B9B7B8000000000000000000B8988800B9771400E18B3500D995
      5400D9955400E7A86F00DAA88D00E18B3500AF885200C4B6A800C5A98800C4B6
      A800C8C9CB00C4B6A800C8B49800C8C9CB00EAE8E800C4B6A800C5A98800C4B6
      A800C4B6A800CBA37300CE591000E5C9C80000000000EBE6D500EBA95200FAC0
      5E00EBA95200CE591000D1670900D1670900D16709005779080018890F001889
      0F0057790800B8681400D1670900CE591000A6580E005D5811005D5811006C6C
      34006B5631009839080091857400000000000000000000000000E5C9C800D789
      4F00FAA71B00FAA71B00FAA71B00FAC05E00F1BC7700DA9B9400C97470009C5E
      8400D6ABAA00D6ABAA00D6ABAA00D6ABAA00D6ABAA00D6ABAA00B09A99009076
      6E00A8686B0090766E00B48989008C5966000000000000000000EBE6D500D179
      3300D349100098492B00AA694E0098492B00D1583100D1583100B0887300B898
      8800F5A93400FAA71B00F5A93400E4C02C00F5A93400FAC05E00CBA37300ED87
      8C008C5966008C59660067696C00DCB9CB00D3693500D1793300E18B3500D789
      4F00D9955400D9955400E7A86F00D1793300D7894F00C4B6A800D5C6B600D5C6
      B600BEA99800C5A98800D5C6B600E3DAD600D1D5D400D1D5D400D1D5D400EBE6
      D500D5C6B600CBA37300CE591000EFD7B20000000000EFD7B200D1670900FAC0
      5E00FAC05E00D9760C00D1670900D9760C00978608009786080018890F00698B
      0A00D9760C00D9760C00D1670900577908001B6607001B6607001B6607001B66
      07006C6C34006F45100091857400000000000000000000000000DBB9B300D093
      6C00FAA71B00FBBB4500FBBB4500F5A93400F5A93400FBBB4500FAC05E00E7A8
      6F00ED878C00A67A8800D6ABAA00DBB9B300DCB9CB00D6ABAA00B4898900B489
      8900B4898900AE7A7300AE7A73005B494C0000000000F9F9D300D17933009839
      080098390800983908005D5811006F4510009839080098390800AE7A7300E7A8
      6F00FAA71B00FBBB4500FBBB4500FBBB4500FBBB4500F5A93400FBBB4500FBBB
      4500E7A86F00D0936C00A8686B005B494C0098492B00D1670900D3693500D789
      4F00D7894F00D9955400D9955400D1793300D7894F00C4B6A800E3DAD600D1D5
      D400D5C6B600BEA99800C8B49800D5C6B600D1D5D400E3DAD600EAE8E800EAE8
      E800EAE8E800C5A98800A6580E00D1D5D40000000000ECCAAD00CE591000E18B
      3500F2C89100FAC05E0097860800698B0A0094950B0094950B0094950B00EB90
      0900EB900900EB900900D9760C005779080016780600167806001B6607001B66
      07005D5811006F45100090766E00000000000000000000000000D6ABAA00EBA9
      5200FBBB4500FBBB4500FBBB4500FBBB4500FBBB4500FBBB4500FBBB4500FBBB
      4500FBBB4500EBA95200C9747000D6ABAA00DBB9B300D6ABAA00B4898900B489
      8900B09A9900B4898900B48989008C59660000000000D9955400983908009839
      0800D3491000A0490E001B6607001B660700A0490E00A0490E00B4898900F1BC
      7700FBBB4500FBBB4500FBBB4500FBBB4500FAC05E00FAC05E00FBBB4500FBBB
      4500FBBB4500FBBB4500D7894F00A8686B00D3491000D1583100D3693500D57A
      4B00D9955400D0936C00D0936C00B9771400CBA37300EAE8E800EAE8E800C8C9
      CB00D5C6B600C8B49800BEA99800D5C6B600E3DAD600EAE9F800EAE8E800BAD8
      EF00EAE9F800EFD7B2006B56310099E4F60000000000F9D89000D1670900D167
      0900F1BC7700F9D890009CC35E0024A214009BA013009BA01300FAA71B00EB90
      0900EB900900EB900900EB9009009786080018890F00167806001B6607005D58
      11001B6607006F45100091857400000000000000000000000000AA9BAA00FBBB
      4500FBBB4500FBBB4500FAC05E00FAC05E00FAC05E00FAC05E00FAC05E00FAC0
      5E00FBBB4500FBBB4500D7894F00D6ABAA00DCB9CB00D6ABAA00AE7A7300A67A
      8800B4898900DA9B9400B48989008C596600D5C6B600CE59100098390800CE59
      1000A6580E00CE5910001B660700167806001B660700CE591000D6ABAA00E7A8
      6F00FBBB4500FAC05E00FAC05E00FAC05E00FAC05E00FCD27400FAC05E00FAC0
      5E00FAC05E00FBBB4500D7894F009C5E8400A3593300D1583100D57A4B00D789
      4F00D9955400D0936C00D0936C00B9771400CBA37300D1D5D400D1D5D400D5C6
      B600C4B6A800EBE6D500C4B6A800BEA99800C4B6A800D5C6B600D1D5D400EAE8
      E800EAE8E800ECCAAD00A359330085C9DB0000000000EFD7B200D9760C00D167
      0900EB900900F2C89100EFD7B20049BA49001CBC450049BA4900E4C02C00FBBB
      4500FAA71B00EB900900EB900900EB9009005779080016780600577908001B66
      07001B6607005D581100B0887300000000000000000000000000DA9B9400FAC0
      5E00FAC05E00FCD27400FAC05E00FCD27400FCD27400FAC05E00FAC05E00FAC0
      5E00FAC05E00FAC05E00D0936C00D6ABAA00DCB9CB00D6ABAA00AE7A7300A67A
      8800AE7A7300AE7A7300AE7A73008C596600CBA37300A6580E00CE591000D167
      0900D1670900B977140018890F001678060016780600D1793300C4B6A800FAC0
      5E00FAC05E00FCD27400FCD27400FCD27400FCD27400FCD27400FCD27400FCD2
      7400FAC05E00FAC05E00D7894F00A67A8800B0887300AA694E00CE685300D093
      6C00D0936C00D0936C00D0936C00D1793300D7894F00C8B49800C4B6A800D5C6
      B600D1D5D400FCFCFB00D5C6B600C8B49800C8B49800BEA99800C8B49800D5C6
      B600C4B6A800C5A988006E380D00B9B7B80000000000EBE6D500EB900900D167
      0900B97714009CC35E00EFD7B200FAECB10062C666002DCC650062C66600FCD2
      7400FAC05E00E4C02C00EB900900EB9009009786080018890F00167806001B66
      07001B66070098492B00BEA998000000000000000000D1D5D400C5A98800FAC0
      5E00FCD27400FCD27400FCD27400FCD27400FCD27400FCD27400FCD27400FCD2
      7400FAC05E00FAC05E00ED878C00DBB9B300DBB9B300D6ABAA00AE7A7300B489
      8900AE7A7300AE7A7300A67A88008C596600D7894F00A6580E00D1670900D167
      090097860800698B0A0018890F00698B0A00D9760C00D9955400DBB9B300FAC0
      5E00FCD27400FCD27400FCD27400FCD27400F9D89000FCD27400FCD27400FCD2
      7400FCD27400FCD27400AE7A7300B4898900E5C9C800CE685300B0887300D093
      6C00CBA37300CBA37300D0936C00B5753600B8681400B8681400B9771400B977
      1400D1793300B5753600B9771400B8681400B9771400B9771400B8681400B868
      1400B8681400D9760C006E380D00A7AAB5000000000000000000EBA95200D976
      0C006CA41E001CBC45007ADA8A00EBE6D500FAECB1007ADA8A002DCC6500BDEA
      AE00F5E59600FBBB4500FAA71B00EB90090094950B0057790800167806001678
      0600596A06006C6C3400D1D5D4000000000000000000DCB9CB00E1B99200FAC0
      5E00FCD27400FCD27400F5E59600F9D89000F5E59600F9D89000F5E59600FCD2
      7400FCD27400FAC05E00B4898900DBB9B300B9B7B800D6ABAA00A8686B00B489
      8900DA9B9400B4898900A67A88008C596600E18B3500D1670900D9760C00698B
      0A0024A2140024A214009BA01300EB900900EB900900C5A98800C4B6A800FCD2
      7400FCD27400F5E59600F5E59600F5E59600F5E59600F5E59600F5E59600FCD2
      7400FCD27400FCD27400AA694E00B4898900EAE8E800AE7A7300D0936C00DA9B
      9400DAA88D00C5A9880090766E00405A6C00AE7A7300A67850006D665100A768
      3100B8681400A7683100A7683100B5753600D3693500A7683100B8681400A768
      3100A0490E005B494C002F364E00AEB9C8000000000000000000EFD7B2009BA0
      13001EAA330049BA49002DCC6500BDEAAE00EBE6D500EBE6D500BDEAAE00F9F9
      D300FAECB100FCD27400FAA71B00EB900900EB9009009786080018890F00596A
      0600596A0600B8988800EAE8E8000000000000000000A7AAB500F2C89100FCD2
      7400FCD27400F5E59600F5E59600F5E59600F5E59600F5E59600F5E59600F5E5
      9600FCD27400F1BC7700DA9B9400DBB9B300DCB9CB00DA9B9400C9747000A868
      6B00AE7A7300B4898900AE7A73008C596600D9955400D167090094950B001EAA
      33001EAA33001CBC4500ACBA3E00FBBB4500FAA71B00D5C6B600E1B99200FCD2
      7400FCD27400F9D89000F5E59600F5E59600F5E59600F5E59600F5E59600F5E5
      9600F9D89000FCD27400A8686B00A7AAB50000000000DBB9B300B8988800D6AB
      AA00E1B99200C5A9880067696C0069698A009185740091857400AE7A7300B088
      7300AF885200D7894F00AF885200D7894F00AF885200D57A4B00A7683100A658
      0E00D3491000373736002F476F00E3DAD600000000000000000000000000EBA9
      520049BA49002DCC650062C666007ADA8A00BDEAAE00FCFCFB00FCFCFB00F9F9
      D3004CD9770062C66600ACA832009BA01300EB900900D9760C00577908005779
      0800AF885200C4B6A800E3DAD60000000000D1D5D400DBB9B300F2C89100FCD2
      7400F5E59600F5E59600F5E59600FAECB100F5E59600FAECB100F5E59600F5E5
      9600F5E59600E7A86F00B09A9900DBB9B300DBB9B300D6ABAA00A67A8800C974
      7000A8686B00A8686B00C97470008C596600C3C69100D9760C0094950B001CBC
      45002DCC65002DCC65002DCC6500A8D06D00F1BC7700D1D5D400E1B99200F5E5
      9600F5E59600F5E59600FAECB100FAECB100FAECB100FAECB100FAECB100F5E5
      9600F5E59600F2C891009E584D00D1D5D40000000000EAE8E800BEA99800D6AB
      AA00D5C6B600B9B7B800AA9BAA00558D95008B8D8F008494AB00558D9500B898
      8800C5A98800DAA88D00DAA88D00CBA37300B8988800D0936C00B57536009849
      2B00983908005C18080090766E0000000000000000000000000000000000EBE6
      D500ACA832006CA41E009CC35E00A8D06D00BDEAAE00F9F9D300F9F9D300FCFC
      FB007ADA8A001CBC45001CBC4500EB900900D9760C00D9760C00D9760C00A090
      3300D5C6B600ECCAAD00B9B7B800EBE6D500D1D5D400C8C9CB00F1BC7700F5E5
      9600F5E59600FAECB100FAECB100F9F9D300FAECB100FAECB100FAECB100F5E5
      9600F5E59600CBA37300D6ABAA00EAE8E800EAE8E800FCFCFB00EAE8E800DBB9
      B300FC93AC00C9747000C97470008C59660000000000ACA832001EAA33002DCC
      65004CD977007ADA8A0068EA8D00BDEAAE00ECCAAD00C8C9CB00C5A98800FCD2
      7400F5E59600FAECB100FAECB100FAECB100F9F9D300FAECB100FAECB100FAEC
      B100F5E59600F1BC77009C5E8400D1D5D4000000000000000000E5C9C800D6AB
      AA00E5C9C800C8C9CB00A7AAB5008494AB00A7AAB5008AA2B3008B8D8F00558D
      AD0088A09B00AFA8A800C8B49800C4B6A800E1B99200DAA88D00D0936C00D158
      31008E270E00692A0D00E3DAD600000000000000000000000000000000000000
      0000EBE6D500F5A93400ACA83200ACBA3E00A8D06D00BDEAAE00BDEAAE00EBE6
      D500F9F9D3007ADA8A001EAA3300EB900900EB900900D9760C00E18B3500D5C6
      B60000000000F2C89100C4B6A800EFD7B200C8C9CB00D5C6B600B8988800C8B4
      9800C3C69100F9D89000FAECB100F9F9D300F9F9D300F9F9D300FAECB100FAEC
      B100F5E59600DAA88D00DBB9B300EAE8E800E5C9C800E3DAD600E5C9C800EAE8
      E800E3DAD600DBB9B300B4898900D6ABAA0000000000BDEAAE001CBC45002DCC
      65007ADA8A00BDEAAE00BDEAAE00BDEAAE00D5C6B600EAE8E800AFA8A800B09A
      9900BEA99800C5A98800D5C6B600EFD7B200FAECB100F9F9D300FAECB100FAEC
      B100FAECB100F1BC7700A8686B00E3DAD600000000000000000000000000D5C6
      B600E5C9C800EBE6D500AEB9C800B9B7B800AEB9C800AEB9C800A7AAB5008494
      AB008494AB00AA9BAA0088A09B00D5C6B600ECCAAD00C4B6A800C8B498009E58
      4D008E270E00E5C9C80000000000000000000000000000000000000000000000
      000000000000EBE6D500EBA95200FAA71B00ACBA3E009CC35E00A8D06D0062C6
      660074C58C00F9D890009CC35E009BA01300EB900900AF885200C4B6A8000000
      000000000000E1B99200C5A98800C8C9CB00C8C9CB00E3DAD600C8C9CB00B9B7
      B800AFA8A800B09A9900B09A9900BEA99800E1B99200C3C69100EFD7B200FAEC
      B100F5E59600B8988800D6ABAA00DBB9B300DBB9B300D6ABAA00DBB9B300DBB9
      B300DCB9CB00EAE9F80000000000000000000000000000000000A8D06D002DCC
      6500BDEAAE00BDEAAE00BDEAAE00BDEAAE0074C58C009CC2A400D5C6B600DBB9
      B300C8C9CB00B9B7B800BEA99800B09A9900B09A9900BEA99800C8B49800C3C6
      9100F9D89000B898880075777700000000000000000000000000000000000000
      0000E3DAD600E3DAD600C8C9CB00C8C9CB00D1D5D400D1D5D400AEB9C800AEB9
      C800C8C9CB00A7AAB500DBB9B300E3DAD600E5C9C800E3DAD600DA9B9400A848
      4B00C8C9CB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EFD7B200EBA95200F5A93400ACA832009BA0
      1300EB900900ACA83200F2C89100F1BC7700D7894F0091857400AFA8A800B9B7
      B800C4B6A800EBA95200C5A98800D1D5D40000000000EAE9F800D1D5D400D1D5
      D400D1D5D400D1D5D400E5C9C800B9B7B800AA9BAA00BEA99800B09A9900B898
      8800DA9B9400B0887300B4898900E5C9C800DCD9F200EAE9F800000000000000
      0000000000000000000000000000000000000000000000000000EAE8E800BDEA
      AE007ADA8A0068EA8D0068EA8D007ADA8A002DCC65001EAA330094950B00E18B
      3500C4B6A800D1D5D400D1D5D400D1D5D400DBB9B300B9B7B800DA9B9400B09A
      9900B09A9900AE7A7300B09A9900000000000000000000000000000000000000
      000000000000E3DAD600D1D5D400D1D5D400EAE8E800D1D5D400C8C9CB00EBE6
      D500EBE6D500C8C9CB00EBE6D500E3DAD600B9B7B800B09A9900B4898900D1D5
      D400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000EBE6D500F2C89100E7A86F00EBA95200CBA3
      7300A9A75500E7A86F00D5C6B600000000000000000000000000000000000000
      00000000000000000000EAE8E800EAE8E800EAE8E800E5C9C800E5C9C800DBB9
      B300AFA8A800B4898900DCD9F200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F9F9D300FAECB1007ADA8A009CC35E009CC35E00A8D06D00F9D89000F9F9
      D30000000000000000000000000000000000EAE8E800DCD9F200E3DAD600E3DA
      D600DCB9CB00D6ABAA0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EAE8E800D1D5D400D1D5D400C8C9
      CB00BDEAAE00D1D5D400EFD7B200EAE9F800EAE8E800C8C9CB00000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000300000000100010000000000400200000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000E1FFFFFFFFFFFFF007FF00FF8000FFE0
      03FFFFC003FC003F00003F8001FFFF8001F800010C001F0000FFFF0000F00001
      08000F0000FFFF0000E0000100000700000FFF0000C00001000003000003FF80
      01800001000001800000FFC003800001800001C00000F00007000000800001C0
      0000E00003000000800001C00000C00000000000800001C00000800000000000
      800001C00000800000000000800001C00000000000000000800001C000000000
      00000000800001800000000000000000C00001800000000000000000C0000180
      0000000000800000E00001000000000000800001E00000000000800000C00001
      F00008000000800000E00003F80018000003C00001F00007FE000080003FC000
      01F8000FFFFE01FC01FFF00F03FF003F00000000000000000000000000000000
      000000000000}
  end
  object EditCommentsImgs: TImageList
    Left = 16
    Top = 108
    Bitmap = {
      494C010109000B00040010001000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
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
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
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
end
