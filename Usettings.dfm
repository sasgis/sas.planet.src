object FSettings: TFSettings
  Left = 293
  Top = 114
  BorderWidth = 3
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
  ClientHeight = 424
  ClientWidth = 624
  Color = clBtnFace
  Constraints.MinHeight = 457
  Constraints.MinWidth = 638
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 393
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    object TabSheet9: TTabSheet
      BorderWidth = 3
      Caption = #1050#1072#1088#1090#1099
      ImageIndex = 8
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object MapList: TListView
        Left = 0
        Top = 0
        Width = 513
        Height = 359
        Align = alClient
        Columns = <
          item
            Caption = #1053#1072#1079#1074#1072#1085#1080#1077' '#1082#1072#1088#1090#1099
            Width = 140
          end
          item
            Caption = #1055#1072#1087#1082#1072' '#1074' '#1082#1101#1096#1077
            Width = 85
          end
          item
            Caption = #1055#1091#1090#1100' '#1074' '#1084#1077#1085#1102
            Width = 110
          end
          item
            Caption = #1043#1086#1088'. '#1082#1083'.'
            Width = 55
          end
          item
            Caption = #1055#1091#1090#1100' '#1082' '#1092#1072#1081#1083#1091
            Width = 118
          end>
        FlatScrollBars = True
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnCustomDrawItem = MapListCustomDrawItem
        OnCustomDrawSubItem = MapListCustomDrawSubItem
        OnDblClick = Button15Click
      end
      object pnlMapsRightButtons: TPanel
        Left = 513
        Top = 0
        Width = 97
        Height = 359
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object Button15: TButton
          AlignWithMargins = True
          Left = 3
          Top = 65
          Width = 91
          Height = 25
          Align = alTop
          Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
          TabOrder = 0
          OnClick = Button15Click
        end
        object Button11: TButton
          AlignWithMargins = True
          Left = 3
          Top = 34
          Width = 91
          Height = 25
          Align = alTop
          Caption = #1042#1085#1080#1079
          TabOrder = 1
          OnClick = Button11Click
        end
        object Button12: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 91
          Height = 25
          Align = alTop
          Caption = #1042#1074#1077#1088#1093
          TabOrder = 2
          OnClick = Button12Click
        end
        object Button18: TButton
          AlignWithMargins = True
          Left = 3
          Top = 96
          Width = 91
          Height = 25
          Align = alTop
          Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103
          TabOrder = 3
          OnClick = Button18Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = #1050#1101#1096
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object flwpnlMemCache: TFlowPanel
        AlignWithMargins = True
        Left = 3
        Top = 163
        Width = 610
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label30: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 281
          Height = 13
          Align = alLeft
          Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1090#1072#1081#1083#1086#1074' '#1082#1101#1096#1080#1088#1091#1077#1084#1086#1077' '#1074' '#1086#1087#1077#1088#1072#1090#1080#1074#1085#1091#1102' '#1087#1072#1084#1103#1090#1100
          Layout = tlCenter
        end
        object SETilesOCache: TSpinEdit
          Left = 287
          Top = 0
          Width = 65
          Height = 22
          MaxValue = 10000
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
      end
      object grdpnlCache: TGridPanel
        Left = 0
        Top = 0
        Width = 616
        Height = 160
        Align = alTop
        BevelOuter = bvNone
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 150.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 20.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 20.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = Label15
            Row = 0
          end
          item
            Column = 1
            Control = NewCpath
            Row = 0
          end
          item
            Column = 2
            Control = Button6
            Row = 0
          end
          item
            Column = 3
            Control = Button7
            Row = 0
          end
          item
            Column = 0
            Control = Label2
            Row = 1
          end
          item
            Column = 1
            Control = OldCpath
            Row = 1
          end
          item
            Column = 2
            Control = Button4
            Row = 1
          end
          item
            Column = 3
            Control = Button5
            Row = 1
          end
          item
            Column = 0
            Control = Label1
            Row = 2
          end
          item
            Column = 1
            Control = EScPath
            Row = 2
          end
          item
            Column = 2
            Control = Button8
            Row = 2
          end
          item
            Column = 3
            Control = Button9
            Row = 2
          end
          item
            Column = 0
            Control = Label19
            Row = 3
          end
          item
            Column = 1
            Control = GMTilesPath
            Row = 3
          end
          item
            Column = 2
            Control = Button13
            Row = 3
          end
          item
            Column = 3
            Control = Button14
            Row = 3
          end
          item
            Column = 0
            Control = Label31
            Row = 4
          end
          item
            Column = 1
            Control = GECachePath
            Row = 4
          end
          item
            Column = 2
            Control = Button10
            Row = 4
          end
          item
            Column = 3
            Control = Button17
            Row = 4
          end
          item
            Column = 0
            Control = Label37
            Row = 5
          end
          item
            Column = 1
            Control = CBCacheType
            Row = 5
          end>
        RowCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 27.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 27.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 27.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 27.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 27.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 1
        DesignSize = (
          616
          160)
        object Label15: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 144
          Height = 21
          Align = alClient
          Caption = #1055#1091#1090#1100' '#1082' "'#1088#1086#1076#1085#1086#1084#1091'" '#1082#1101#1096#1091':'
          Layout = tlCenter
          ExplicitWidth = 120
          ExplicitHeight = 13
        end
        object NewCpath: TEdit
          AlignWithMargins = True
          Left = 153
          Top = 3
          Width = 420
          Height = 21
          Align = alClient
          TabOrder = 0
        end
        object Button6: TButton
          Tag = 2
          Left = 576
          Top = 3
          Width = 20
          Height = 21
          Anchors = []
          Caption = '<>'
          TabOrder = 1
          OnClick = Button4Click
        end
        object Button7: TButton
          Tag = 2
          Left = 596
          Top = 3
          Width = 20
          Height = 21
          Anchors = []
          Caption = '...'
          TabOrder = 2
          OnClick = Button5Click
        end
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 30
          Width = 144
          Height = 21
          Align = alClient
          Caption = #1055#1091#1090#1100' '#1082' '#1082#1101#1096#1091' GoogleMV:'
          Layout = tlCenter
          ExplicitWidth = 117
          ExplicitHeight = 13
        end
        object OldCpath: TEdit
          AlignWithMargins = True
          Left = 153
          Top = 30
          Width = 420
          Height = 21
          Align = alClient
          TabOrder = 3
        end
        object Button4: TButton
          Tag = 1
          Left = 576
          Top = 30
          Width = 20
          Height = 21
          Anchors = []
          Caption = '<>'
          TabOrder = 4
          OnClick = Button4Click
        end
        object Button5: TButton
          Tag = 1
          Left = 596
          Top = 30
          Width = 20
          Height = 21
          Anchors = []
          Caption = '...'
          TabOrder = 5
          OnClick = Button5Click
        end
        object Label1: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 57
          Width = 144
          Height = 21
          Align = alClient
          Caption = #1055#1091#1090#1100' '#1082' '#1082#1101#1096#1091' EarthSliser:'
          Layout = tlCenter
          ExplicitWidth = 117
          ExplicitHeight = 13
        end
        object EScPath: TEdit
          AlignWithMargins = True
          Left = 153
          Top = 57
          Width = 420
          Height = 21
          Align = alClient
          TabOrder = 6
        end
        object Button8: TButton
          Tag = 3
          Left = 576
          Top = 57
          Width = 20
          Height = 21
          Anchors = []
          Caption = '<>'
          TabOrder = 7
          OnClick = Button4Click
        end
        object Button9: TButton
          Tag = 3
          Left = 596
          Top = 57
          Width = 20
          Height = 21
          Anchors = []
          Caption = '...'
          TabOrder = 8
          OnClick = Button5Click
        end
        object Label19: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 84
          Width = 144
          Height = 21
          Align = alClient
          Caption = #1055#1091#1090#1100' '#1082' '#1082#1101#1096#1091' GM Tiles:'
          Layout = tlCenter
          ExplicitWidth = 109
          ExplicitHeight = 13
        end
        object GMTilesPath: TEdit
          AlignWithMargins = True
          Left = 153
          Top = 84
          Width = 420
          Height = 21
          Align = alClient
          TabOrder = 9
        end
        object Button13: TButton
          Tag = 4
          Left = 576
          Top = 84
          Width = 20
          Height = 21
          Anchors = []
          Caption = '<>'
          TabOrder = 10
          OnClick = Button4Click
        end
        object Button14: TButton
          Tag = 4
          Left = 596
          Top = 84
          Width = 20
          Height = 21
          Anchors = []
          Caption = '...'
          TabOrder = 11
          OnClick = Button5Click
        end
        object Label31: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 111
          Width = 144
          Height = 21
          Align = alClient
          Caption = #1055#1091#1090#1100' '#1082' '#1082#1101#1096#1091' Google Earth:'
          Layout = tlCenter
          ExplicitWidth = 129
          ExplicitHeight = 13
        end
        object GECachePath: TEdit
          AlignWithMargins = True
          Left = 153
          Top = 111
          Width = 420
          Height = 21
          Align = alClient
          TabOrder = 12
        end
        object Button10: TButton
          Tag = 5
          Left = 576
          Top = 111
          Width = 20
          Height = 21
          Anchors = []
          Caption = '<>'
          TabOrder = 13
          OnClick = Button4Click
        end
        object Button17: TButton
          Tag = 5
          Left = 596
          Top = 111
          Width = 20
          Height = 21
          Anchors = []
          Caption = '...'
          TabOrder = 14
          OnClick = Button5Click
        end
        object Label37: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 138
          Width = 144
          Height = 19
          Align = alClient
          Caption = #1058#1080#1087' '#1082#1101#1096#1072
          Layout = tlCenter
          ExplicitWidth = 48
          ExplicitHeight = 13
        end
        object CBCacheType: TComboBox
          AlignWithMargins = True
          Left = 153
          Top = 138
          Width = 420
          Height = 21
          Align = alClient
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 1
          TabOrder = 15
          Text = 'SAS.'#1055#1083#1072#1085#1077#1090#1072
          Items.Strings = (
            'GoogleMV'
            'SAS.'#1055#1083#1072#1085#1077#1090#1072
            'EarthSlicer 1.95'
            'Googe maps tiles')
          ExplicitHeight = 19
        end
      end
    end
    object TabSheet1: TTabSheet
      BorderWidth = 3
      Caption = #1048#1085#1090#1077#1088#1085#1077#1090
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlDownloadParams: TPanel
        Left = 0
        Top = 0
        Width = 610
        Height = 131
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object CBDblDwnl: TCheckBox
          AlignWithMargins = True
          Left = 6
          Top = 114
          Width = 598
          Height = 17
          Align = alTop
          Caption = #1055#1086#1074#1090#1086#1088#1085#1086' '#1087#1099#1090#1072#1090#1100#1089#1103' '#1079#1072#1075#1088#1091#1079#1080#1090#1100' '#1090#1072#1081#1083' '#1087#1088#1080' '#1077#1075#1086' '#1086#1090#1089#1090#1091#1090#1089#1074#1080#1080
          TabOrder = 0
        end
        object CkBGoNextTile: TCheckBox
          AlignWithMargins = True
          Left = 6
          Top = 137
          Width = 598
          Height = 17
          Align = alTop
          Caption = #1055#1077#1088#1077#1093#1086#1076' '#1082' '#1089#1083#1077#1076#1091#1102#1097#1077#1084#1091' '#1090#1072#1081#1083#1091' '#1077#1089#1083#1080' '#1089#1077#1088#1074#1077#1088' '#1085#1077' '#1086#1090#1074#1077#1095#1072#1077#1090
          TabOrder = 1
        end
        object CBSaveTileNotExists: TCheckBox
          AlignWithMargins = True
          Left = 6
          Top = 160
          Width = 598
          Height = 17
          Align = alTop
          Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1102' '#1086#1073' '#1086#1090#1089#1091#1090#1089#1090#1074#1080#1080' '#1090#1072#1081#1083#1072' '#1085#1072' '#1089#1077#1088#1074#1077#1088#1077
          TabOrder = 2
        end
        object flwpnlDownloadTimeOut: TFlowPanel
          Left = 3
          Top = 180
          Width = 604
          Height = 22
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 3
          object Label32: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 175
            Height = 13
            Caption = #1058#1072#1081#1084#1072#1091#1090' '#1085#1072' '#1089#1077#1090#1077#1074#1099#1077' '#1086#1087#1077#1088#1072#1094#1080#1080', '#1084#1089
          end
          object SETimeOut: TSpinEdit
            Left = 181
            Top = 0
            Width = 73
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
        end
        object CBLastSuccess: TCheckBox
          AlignWithMargins = True
          Left = 6
          Top = 205
          Width = 598
          Height = 17
          Align = alTop
          Caption = 
            #1053#1072#1095#1072#1090#1100' '#1089#1086#1093#1088#1072#1085#1077#1085#1085#1091#1102' '#1089#1077#1089#1089#1080#1102' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1089' '#1087#1086#1089#1083#1077#1076#1085#1077#1075#1086' '#1091#1076#1072#1095#1085#1086' '#1079#1072#1075#1088#1091#1078#1077#1085#1085 +
            #1086#1075#1086' '#1090#1072#1081#1083#1072
          TabOrder = 4
        end
        object GroupBox4: TGroupBox
          Left = 3
          Top = 3
          Width = 604
          Height = 108
          Align = alTop
          Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1086#1076#1082#1083#1102#1095#1077#1085#1080#1103
          TabOrder = 5
          object pnlProxyUrl: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 47
            Width = 594
            Height = 23
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lblUseProxy: TLabel
              AlignWithMargins = True
              Left = 24
              Top = 3
              Width = 150
              Height = 13
              Align = alLeft
              Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1087#1088#1086#1082#1089#1080' (ip:port)'
              Layout = tlCenter
            end
            object CBProxyused: TCheckBox
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 15
              Height = 17
              Align = alLeft
              TabOrder = 0
              OnClick = CBProxyusedClick
            end
            object EditIP: TEdit
              Left = 177
              Top = 0
              Width = 417
              Height = 23
              Align = alClient
              TabOrder = 1
              ExplicitHeight = 21
            end
          end
          object flwpnlProxyAuth: TFlowPanel
            AlignWithMargins = True
            Left = 5
            Top = 76
            Width = 594
            Height = 23
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 1
            object CBLogin: TCheckBox
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 17
              Height = 17
              TabOrder = 0
              OnClick = CBLoginClick
            end
            object lblProxyLogin: TLabel
              AlignWithMargins = True
              Left = 26
              Top = 3
              Width = 108
              Height = 13
              Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1083#1086#1075#1080#1085':'
            end
            object EditLogin: TEdit
              Left = 137
              Top = 0
              Width = 81
              Height = 21
              TabOrder = 1
            end
            object Label25: TLabel
              AlignWithMargins = True
              Left = 221
              Top = 3
              Width = 48
              Height = 13
              Caption = #1080' '#1087#1072#1088#1086#1083#1100':'
            end
            object EditPass: TEdit
              Left = 272
              Top = 0
              Width = 81
              Height = 21
              PasswordChar = '*'
              TabOrder = 2
            end
          end
          object pnlUseIEProxy: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 594
            Height = 23
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 2
            object chkUseIEProxy: TCheckBox
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 588
              Height = 17
              Align = alTop
              Caption = #1041#1088#1072#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1086#1076#1082#1083#1102#1095#1077#1085#1080#1103' '#1080#1079' '#1088#1077#1077#1089#1090#1088#1072
              TabOrder = 0
              OnClick = chkUseIEProxyClick
            end
          end
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 616
        Height = 42
        Align = alTop
        Caption = ' '#1050#1086#1083#1077#1089#1080#1082#1086' '#1084#1099#1096#1080' '
        TabOrder = 0
        object ScrolInvert: TCheckBox
          AlignWithMargins = True
          Left = 5
          Top = 18
          Width = 606
          Height = 17
          Align = alTop
          Caption = #1074#1088#1072#1097#1077#1085#1080#1077' '#1085#1072' '#1089#1077#1073#1103' - '#1087#1088#1080#1073#1083#1080#1078#1077#1085#1080#1077
          TabOrder = 0
        end
      end
      object GroupBox5: TGroupBox
        Left = 0
        Top = 42
        Width = 616
        Height = 323
        Align = alClient
        Caption = ' '#1043#1086#1088#1103#1095#1080#1077' '#1082#1083'. '
        TabOrder = 1
      end
    end
    object TabSheet4: TTabSheet
      Caption = #1042#1085#1077#1096#1085#1080#1081' '#1074#1080#1076
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grdpnlUI: TGridPanel
        Left = 0
        Top = 0
        Width = 616
        Height = 365
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = pnlUILeft
            Row = 0
          end
          item
            Column = 1
            Control = pnlUIRight
            Row = 0
          end>
        RowCollection = <
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        object pnlUILeft: TPanel
          Left = 0
          Top = 0
          Width = 308
          Height = 365
          Align = alClient
          TabOrder = 0
          object pnlNumbersFormat: TPanel
            Left = 1
            Top = 1
            Width = 306
            Height = 29
            Align = alTop
            TabOrder = 0
            object Label3: TLabel
              Left = 1
              Top = 1
              Width = 198
              Height = 27
              Align = alLeft
              Alignment = taRightJustify
              Caption = #1060#1086#1088#1084#1072#1090' '#1095#1080#1089#1077#1083' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1084#1072#1089#1096#1090#1072#1073#1072
              Layout = tlCenter
              ExplicitHeight = 13
            end
            object ComboBox1: TComboBox
              AlignWithMargins = True
              Left = 202
              Top = 4
              Width = 100
              Height = 21
              Align = alClient
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 0
              Text = '12 '#1082#1084'. 423 '#1084'.'
              Items.Strings = (
                '12 '#1082#1084'. 423 '#1084'.'
                '23,4 '#1082#1084'.')
            end
          end
          object pnlLonLatFormat: TPanel
            Left = 1
            Top = 30
            Width = 306
            Height = 51
            Align = alTop
            TabOrder = 1
            object pnlCoordFormat: TPanel
              Left = 1
              Top = 1
              Width = 304
              Height = 29
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Label84: TLabel
                Left = 0
                Top = 0
                Width = 178
                Height = 29
                Align = alLeft
                Alignment = taRightJustify
                Caption = #1060#1086#1088#1084#1072#1090' '#1087#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1080#1103' '#1082#1086#1086#1088#1076#1080#1085#1072#1090
                Layout = tlCenter
                ExplicitHeight = 13
              end
              object CB_llstrType: TComboBox
                AlignWithMargins = True
                Left = 181
                Top = 3
                Width = 120
                Height = 21
                Align = alClient
                Style = csDropDownList
                ItemHeight = 13
                ItemIndex = 0
                TabOrder = 0
                Text = 'WS '#1075'.'#1084'.'#1089'. (W12'#176'23"43.35'#39')'
                Items.Strings = (
                  'WS '#1075'.'#1084'.'#1089'. (W12'#176'23"43.35'#39')'
                  'WS '#1075'.'#1084'. (W12'#176'23.454)'
                  'WS '#1075'. (W12.1233'#176')'
                  '-- '#1075'.'#1084'.'#1089'. (-12'#176'23"43.35'#39')'
                  '-- '#1075'.'#1084'. (-12'#176'23.454)'
                  '-- '#1075'. (-12.1233'#176')')
              end
            end
            object ChBoxFirstLat: TCheckBox
              AlignWithMargins = True
              Left = 4
              Top = 33
              Width = 298
              Height = 17
              Align = alTop
              Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1074' '#1087#1086#1088#1103#1076#1082#1077': '#1096#1080#1088#1086#1090#1072'-'#1076#1086#1083#1075#1086#1090#1072
              TabOrder = 1
            end
          end
          object flwpnlMiniMap: TFlowPanel
            Left = 1
            Top = 81
            Width = 306
            Height = 26
            Align = alTop
            TabOrder = 2
            object Label14: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 241
              Height = 13
              Alignment = taRightJustify
              Caption = #1056#1072#1079#1085#1086#1089#1090#1100' '#1084#1072#1089#1096#1090#1072#1073#1086#1074' '#1082#1072#1088#1090#1099' '#1086#1073#1079#1086#1088#1072' '#1080' '#1086#1089#1085#1086#1074#1085#1086#1081
            end
            object smmapdif: TSpinEdit
              Left = 248
              Top = 1
              Width = 49
              Height = 22
              MaxValue = 24
              MinValue = 1
              TabOrder = 0
              Value = 1
            end
          end
          object pnlImageProcess: TPanel
            Left = 1
            Top = 107
            Width = 306
            Height = 104
            Align = alTop
            TabOrder = 3
            object LabelGamma: TLabel
              Left = 1
              Top = 1
              Width = 6
              Height = 13
              Align = alTop
              Caption = '_'
            end
            object LabelContrast: TLabel
              Left = 1
              Top = 39
              Width = 6
              Height = 13
              Align = alTop
              Caption = '_'
            end
            object TrBarGamma: TTrackBar
              Left = 1
              Top = 14
              Width = 304
              Height = 25
              Align = alTop
              Max = 100
              Min = 1
              ParentShowHint = False
              Frequency = 5
              Position = 1
              ShowHint = False
              TabOrder = 0
              ThumbLength = 15
              TickMarks = tmTopLeft
              OnChange = TrBarGammaChange
            end
            object TrBarContrast: TTrackBar
              Left = 1
              Top = 52
              Width = 304
              Height = 25
              Align = alTop
              Max = 100
              Min = -100
              Frequency = 10
              TabOrder = 1
              ThumbLength = 15
              TickMarks = tmTopLeft
              OnChange = TrBarContrastChange
            end
            object CBinvertcolor: TCheckBox
              AlignWithMargins = True
              Left = 4
              Top = 80
              Width = 298
              Height = 17
              Align = alTop
              Caption = #1048#1085#1074#1077#1088#1089#1080#1103' '#1094#1074#1077#1090#1086#1074
              TabOrder = 2
            end
          end
          object pnlResize: TPanel
            Left = 1
            Top = 211
            Width = 306
            Height = 29
            Align = alTop
            TabOrder = 4
            object Label16: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 193
              Height = 21
              Align = alLeft
              Alignment = taRightJustify
              Caption = #1040#1083#1075#1086#1088#1080#1090#1084' '#1088#1072#1089#1090#1103#1075#1080#1074#1072#1085#1080#1103' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103
              Layout = tlCenter
              ExplicitHeight = 13
            end
            object ComboBox2: TComboBox
              AlignWithMargins = True
              Left = 203
              Top = 4
              Width = 99
              Height = 21
              Align = alClient
              Style = csDropDownList
              ItemHeight = 13
              ItemIndex = 0
              TabOrder = 0
              Text = 'Box'
              Items.Strings = (
                'Box'
                'Linear'
                'Cosine'
                'Spline'
                'Mitchell'
                'Cubic'
                'Hermite'
                'Lanczos'
                'Gaussian'
                'Blackman'
                'Hann'
                'Hamming'
                'Sinsh')
            end
          end
          object pnlTileBorders: TPanel
            Left = 1
            Top = 240
            Width = 306
            Height = 56
            Align = alTop
            AutoSize = True
            TabOrder = 5
            object Label23: TLabel
              Left = 1
              Top = 1
              Width = 304
              Height = 13
              Align = alTop
              Caption = #1054#1090#1086#1073#1088#1072#1078#1077#1085#1080#1077' '#1075#1088#1072#1085#1080#1094':'
              ExplicitWidth = 110
            end
            object flwpnlTileBorders: TFlowPanel
              Left = 1
              Top = 14
              Width = 304
              Height = 41
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Label27: TLabel
                AlignWithMargins = True
                Left = 3
                Top = 3
                Width = 25
                Height = 13
                Caption = #1062#1074#1077#1090
              end
              object ColorBoxBorder: TColorBox
                Left = 31
                Top = 0
                Width = 78
                Height = 22
                Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
                ItemHeight = 16
                TabOrder = 0
              end
              object Label28: TLabel
                AlignWithMargins = True
                Left = 112
                Top = 3
                Width = 72
                Height = 13
                Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100
              end
              object SpinEditBorderAlpha: TSpinEdit
                Left = 187
                Top = 0
                Width = 41
                Height = 22
                MaxValue = 255
                MinValue = 0
                TabOrder = 1
                Value = 255
              end
              object CBBorderText: TCheckBox
                AlignWithMargins = True
                Left = 231
                Top = 3
                Width = 65
                Height = 17
                Caption = #1055#1086#1076#1087#1080#1089#1100
                TabOrder = 2
              end
            end
          end
        end
        object pnlUIRight: TPanel
          Left = 308
          Top = 0
          Width = 308
          Height = 365
          Align = alClient
          TabOrder = 1
          object flwpnlMiniMapAlfa: TFlowPanel
            Left = 1
            Top = 1
            Width = 306
            Height = 26
            Align = alTop
            TabOrder = 0
            object Label17: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 145
              Height = 13
              Alignment = taRightJustify
              Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' '#1082#1072#1088#1090#1099' '#1086#1073#1079#1086#1088#1072
            end
            object SpinEditMiniMap: TSpinEdit
              Left = 152
              Top = 1
              Width = 57
              Height = 22
              MaxValue = 255
              MinValue = 0
              TabOrder = 0
              Value = 1
            end
          end
          object flwpnlTileBorder: TFlowPanel
            Left = 1
            Top = 27
            Width = 306
            Height = 26
            Align = alTop
            TabOrder = 1
            object Label69: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 255
              Height = 13
              Alignment = taRightJustify
              Caption = #1050#1086#1083'-'#1074#1086' '#1090#1072#1081#1083#1086#1074' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1093' '#1079#1072' '#1075#1088#1072#1085#1080#1094#1077#1081' '#1101#1082#1088#1072#1085#1072
            end
            object SpinEdit3: TSpinEdit
              Left = 262
              Top = 1
              Width = 37
              Height = 22
              MaxValue = 24
              MinValue = -2
              TabOrder = 0
              Value = 0
            end
          end
          object pnlShowMapName: TPanel
            Left = 1
            Top = 53
            Width = 306
            Height = 28
            Align = alTop
            TabOrder = 2
            object CBShowmapname: TCheckBox
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 298
              Height = 17
              Align = alTop
              Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1085#1072#1079#1074#1072#1085#1080#1077' '#1082#1072#1088#1090#1099' '#1085#1072' '#1087#1072#1085#1077#1083#1080' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
              TabOrder = 0
            end
          end
          object pnlLang: TPanel
            Left = 1
            Top = 81
            Width = 306
            Height = 29
            Align = alTop
            TabOrder = 3
            object Label8: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 28
              Height = 13
              Align = alLeft
              Alignment = taRightJustify
              Caption = #1071#1079#1099#1082
              Layout = tlCenter
            end
            object CBoxLocal: TComboBox
              AlignWithMargins = True
              Left = 38
              Top = 4
              Width = 264
              Height = 21
              Align = alClient
              Style = csDropDownList
              ItemHeight = 0
              TabOrder = 0
              OnChange = CBoxLocalChange
            end
          end
          object pnlFillMap: TPanel
            Left = 1
            Top = 110
            Width = 306
            Height = 51
            Align = alTop
            TabOrder = 4
            object Label24: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 298
              Height = 13
              Align = alTop
              Caption = #1050#1072#1088#1090#1072' '#1079#1072#1087#1086#1083#1085#1077#1085#1080#1103':'
              ExplicitWidth = 96
            end
            object flwpnlFillMap: TFlowPanel
              Left = 1
              Top = 20
              Width = 304
              Height = 30
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              object Label26: TLabel
                AlignWithMargins = True
                Left = 3
                Top = 3
                Width = 25
                Height = 13
                Caption = #1062#1074#1077#1090
              end
              object MapZapColorBox: TColorBox
                Left = 31
                Top = 0
                Width = 78
                Height = 22
                Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
                ItemHeight = 16
                TabOrder = 0
              end
              object Label29: TLabel
                AlignWithMargins = True
                Left = 112
                Top = 3
                Width = 72
                Height = 13
                Alignment = taRightJustify
                Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100
              end
              object MapZapAlphaEdit: TSpinEdit
                Left = 187
                Top = 0
                Width = 41
                Height = 22
                MaxValue = 255
                MinValue = 0
                TabOrder = 1
                Value = 255
              end
            end
          end
          object pnlLockToolbars: TPanel
            Left = 1
            Top = 161
            Width = 306
            Height = 28
            Align = alTop
            TabOrder = 5
            object CBlock_toolbars: TCheckBox
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 298
              Height = 17
              Align = alTop
              Caption = #1047#1072#1082#1088#1077#1087#1080#1090#1100' '#1087#1072#1085#1077#1083#1080' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
              TabOrder = 0
            end
          end
          object pnlShowPointDescr: TPanel
            Left = 1
            Top = 189
            Width = 306
            Height = 28
            Align = alTop
            TabOrder = 6
            object CBShowHintOnMarks: TCheckBox
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 298
              Height = 17
              Align = alTop
              Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1086#1087#1080#1089#1072#1085#1080#1077' '#1084#1077#1090#1082#1080' '#1087#1088#1080' '#1085#1072#1074#1077#1076#1077#1085#1080#1080' '#1082#1091#1088#1089#1086#1088#1072
              TabOrder = 0
            end
          end
          object pnlBgColor: TPanel
            Left = 1
            Top = 217
            Width = 306
            Height = 29
            Align = alTop
            TabOrder = 7
            object Label35: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 54
              Height = 21
              Align = alLeft
              Caption = #1062#1074#1077#1090' '#1092#1086#1085#1072
              Layout = tlCenter
              ExplicitHeight = 13
            end
            object ColorBoxBackGround: TColorBox
              AlignWithMargins = True
              Left = 64
              Top = 4
              Width = 238
              Height = 21
              Align = alClient
              Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
              ItemHeight = 16
              TabOrder = 0
              ExplicitHeight = 22
            end
          end
        end
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'GPS'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlGPSLeft: TPanel
        Left = 0
        Top = 0
        Width = 370
        Height = 365
        Align = alLeft
        BorderWidth = 3
        TabOrder = 0
        object flwpnlGpsPort: TFlowPanel
          Left = 4
          Top = 4
          Width = 362
          Height = 32
          Align = alTop
          BorderWidth = 3
          TabOrder = 0
          object Label4: TLabel
            AlignWithMargins = True
            Left = 7
            Top = 7
            Width = 50
            Height = 13
            Caption = 'COM-'#1087#1086#1088#1090
          end
          object ComboBoxCOM: TComboBox
            Left = 60
            Top = 4
            Width = 89
            Height = 21
            ItemHeight = 0
            TabOrder = 0
            Text = 'COM1'
          end
          object SBGetComNum: TSpeedButton
            Left = 149
            Top = 4
            Width = 21
            Height = 21
            Caption = '?'
            Margin = 5
            OnClick = SBGetComNumClick
          end
          object Label65: TLabel
            AlignWithMargins = True
            Left = 173
            Top = 7
            Width = 48
            Height = 13
            Alignment = taRightJustify
            Caption = #1057#1082#1086#1088#1086#1089#1090#1100
          end
          object ComboBoxBoudRate: TComboBox
            Left = 224
            Top = 4
            Width = 89
            Height = 21
            ItemHeight = 13
            ItemIndex = 5
            TabOrder = 1
            Text = '4800'
            Items.Strings = (
              '110'
              '300'
              '600'
              '1200'
              '2400'
              '4800'
              '9600'
              '14400'
              '19200'
              '38400'
              '57600'
              '115200')
          end
        end
        object flwpnlGpsParams: TFlowPanel
          Left = 4
          Top = 36
          Width = 362
          Height = 227
          Align = alTop
          FlowStyle = fsTopBottomLeftRight
          TabOrder = 1
          object Label6: TLabel
            Left = 1
            Top = 1
            Width = 226
            Height = 13
            Caption = #1042#1088#1077#1084#1103' '#1086#1078#1080#1076#1072#1085#1080#1103' '#1086#1090#1074#1077#1090#1072' '#1086#1090' '#1087#1088#1080#1077#1084#1085#1080#1082#1072' ('#1089#1077#1082'.)'
          end
          object SpinEdit2: TSpinEdit
            Left = 1
            Top = 15
            Width = 57
            Height = 22
            MaxValue = 86400
            MinValue = 1
            TabOrder = 0
            Value = 1
          end
          object Label11: TLabel
            Left = 1
            Top = 38
            Width = 160
            Height = 13
            Caption = #1055#1077#1088#1080#1086#1076' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' ('#1089#1077#1082'./1000)'
          end
          object SpinEdit1: TSpinEdit
            Left = 1
            Top = 52
            Width = 57
            Height = 22
            MaxValue = 3600000
            MinValue = 100
            TabOrder = 1
            Value = 100
          end
          object Label10: TLabel
            Left = 1
            Top = 75
            Width = 166
            Height = 13
            Caption = #1056#1072#1079#1084#1077#1088' '#1091#1082#1072#1079#1072#1090#1077#1083#1103' '#1085#1072#1087#1088#1072#1074#1083#1077#1085#1080#1103':'
          end
          object SESizeStr: TSpinEdit
            Left = 1
            Top = 89
            Width = 57
            Height = 22
            MaxValue = 150
            MinValue = 10
            TabOrder = 2
            Value = 100
          end
          object Label20: TLabel
            Left = 1
            Top = 112
            Width = 74
            Height = 13
            Caption = #1064#1080#1088#1080#1085#1072' '#1090#1088#1077#1082#1072':'
          end
          object SESizeTrack: TSpinEdit
            Left = 1
            Top = 126
            Width = 57
            Height = 22
            MaxValue = 50
            MinValue = 1
            TabOrder = 3
            Value = 50
          end
          object Label12: TLabel
            Left = 1
            Top = 149
            Width = 60
            Height = 13
            Caption = #1062#1074#1077#1090' '#1090#1088#1077#1082#1072':'
          end
          object ColorBoxGPSstr: TColorBox
            Left = 1
            Top = 163
            Width = 105
            Height = 22
            Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
            ItemHeight = 16
            TabOrder = 4
          end
          object Label5: TLabel
            Left = 1
            Top = 186
            Width = 283
            Height = 13
            Caption = #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1086#1077' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1093' '#1090#1086#1095#1077#1082' '#1090#1088#1077#1082#1072':'
          end
          object SE_NumTrackPoints: TSpinEdit
            Left = 1
            Top = 200
            Width = 73
            Height = 22
            MaxValue = 1000000
            MinValue = 10
            TabOrder = 5
            Value = 10000
          end
        end
        object pnlGpsTrackSave: TPanel
          Left = 4
          Top = 263
          Width = 362
          Height = 50
          Align = alTop
          TabOrder = 2
          object CB_GPSlog: TCheckBox
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 354
            Height = 17
            Align = alTop
            Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1089#1086#1093#1088#1072#1085#1103#1090#1100' '#1090#1088#1077#1082#1080' '#1074' .plt'
            TabOrder = 0
          end
          object CB_GPSlogNmea: TCheckBox
            AlignWithMargins = True
            Left = 4
            Top = 27
            Width = 354
            Height = 17
            Align = alTop
            Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1089#1086#1093#1088#1072#1085#1103#1090#1100' '#1090#1088#1077#1082#1080' '#1074' .nmea'
            TabOrder = 1
          end
        end
        object pnlGpsSensors: TPanel
          Left = 4
          Top = 313
          Width = 362
          Height = 32
          Align = alTop
          TabOrder = 3
          object CBSensorsBarAutoShow: TCheckBox
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 354
            Height = 17
            Align = alTop
            Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1087#1086#1082#1072#1079#1099#1074#1072#1090#1100'/'#1089#1082#1088#1099#1074#1072#1090#1100' '#1087#1072#1085#1077#1083#1100' '#1076#1072#1090#1095#1080#1082#1086#1074
            TabOrder = 0
          end
        end
      end
      object pnlGpsRight: TPanel
        Left = 370
        Top = 0
        Width = 246
        Height = 365
        Align = alClient
        TabOrder = 1
        object GroupBox2: TGroupBox
          Left = 1
          Top = 296
          Width = 244
          Height = 68
          Align = alBottom
          Caption = ' '#1055#1086#1087#1088#1072#1074#1082#1072' '
          TabOrder = 0
          object Label21: TLabel
            Left = 10
            Top = 19
            Width = 57
            Height = 13
            Caption = #1055#1086' '#1096#1080#1088#1086#1090#1077':'
            Color = clBtnFace
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object Label22: TLabel
            Left = 7
            Top = 41
            Width = 60
            Height = 13
            Caption = #1055#1086' '#1076#1086#1083#1075#1086#1090#1077':'
            Color = clBtnFace
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
          end
          object lat_ns: TComboBox
            Left = 72
            Top = 16
            Width = 33
            Height = 21
            BevelInner = bvNone
            BevelKind = bkSoft
            BevelOuter = bvNone
            Style = csDropDownList
            BiDiMode = bdLeftToRight
            Ctl3D = False
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ItemHeight = 13
            ItemIndex = 0
            ParentBiDiMode = False
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 0
            Text = 'N'
            Items.Strings = (
              'N'
              'S')
          end
          object Lon_we: TComboBox
            Left = 72
            Top = 38
            Width = 33
            Height = 21
            BevelInner = bvNone
            BevelKind = bkSoft
            BevelOuter = bvNone
            Style = csDropDownList
            BiDiMode = bdLeftToRight
            Ctl3D = False
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ItemHeight = 13
            ItemIndex = 0
            ParentBiDiMode = False
            ParentCtl3D = False
            ParentFont = False
            TabOrder = 4
            Text = 'E'
            Items.Strings = (
              'E'
              'W')
          end
          object lat2: TCurrencyEdit
            Left = 153
            Top = 16
            Width = 40
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            AutoSize = False
            BorderStyle = bsNone
            Ctl3D = True
            DecimalPlaces = 5
            DisplayFormat = '0.#####`'
            FormatOnEditing = True
            MaxValue = 60.000000000000000000
            ParentCtl3D = False
            TabOrder = 2
          end
          object lat3: TCurrencyEdit
            Left = 193
            Top = 16
            Width = 40
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            AutoSize = False
            BorderStyle = bsNone
            Ctl3D = True
            DecimalPlaces = 4
            DisplayFormat = '0.####``'
            FormatOnEditing = True
            MaxValue = 60.000000000000000000
            ParentCtl3D = False
            TabOrder = 3
          end
          object lon1: TCurrencyEdit
            Left = 105
            Top = 38
            Width = 48
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            AutoSize = False
            BorderStyle = bsNone
            Ctl3D = True
            DecimalPlaces = 8
            DisplayFormat = '0.########'#176
            FormatOnEditing = True
            MaxValue = 180.000000000000000000
            ParentCtl3D = False
            TabOrder = 5
          end
          object lon2: TCurrencyEdit
            Left = 153
            Top = 38
            Width = 40
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            AutoSize = False
            BorderStyle = bsNone
            Ctl3D = True
            DecimalPlaces = 5
            DisplayFormat = '0.#####`'
            FormatOnEditing = True
            MaxValue = 60.000000000000000000
            ParentCtl3D = False
            TabOrder = 6
          end
          object lon3: TCurrencyEdit
            Left = 193
            Top = 38
            Width = 40
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            AutoSize = False
            BorderStyle = bsNone
            Ctl3D = True
            DecimalPlaces = 4
            DisplayFormat = '0.####``'
            FormatOnEditing = True
            MaxValue = 60.000000000000000000
            ParentCtl3D = False
            TabOrder = 7
          end
          object Lat1: TCurrencyEdit
            Left = 105
            Top = 16
            Width = 48
            Height = 21
            Margins.Left = 0
            Margins.Top = 0
            AutoSize = False
            BorderStyle = bsNone
            Ctl3D = True
            DecimalPlaces = 8
            DisplayFormat = '0.########'#176
            FormatOnEditing = True
            MaxValue = 180.000000000000000000
            ParentCtl3D = False
            TabOrder = 1
          end
        end
        object GroupBox3: TGroupBox
          Left = 1
          Top = 1
          Width = 244
          Height = 295
          Align = alClient
          Caption = ' C'#1087#1091#1090#1085#1080#1082#1080' '
          TabOrder = 1
          object PaintBox1: TPaintBox
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 234
            Height = 272
            Align = alClient
            OnPaint = PaintBox1Paint
            ExplicitLeft = 40
            ExplicitTop = 29
            ExplicitWidth = 225
            ExplicitHeight = 225
          end
        end
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Wikimapia'
      ImageIndex = 7
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object grdpnlWiki: TGridPanel
        Left = 0
        Top = 0
        Width = 616
        Height = 78
        Align = alTop
        ColumnCollection = <
          item
            SizeStyle = ssAuto
            Value = 50.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = Label76
            Row = 0
          end
          item
            Column = 1
            Control = CBWMainColor
            Row = 0
          end
          item
            Column = 0
            Control = Label77
            Row = 1
          end
          item
            Column = 1
            Control = CBWFonColor
            Row = 1
          end
          item
            Column = 1
            Control = Label78
            Row = 2
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end
          item
            SizeStyle = ssAuto
          end>
        TabOrder = 0
        DesignSize = (
          616
          78)
        object Label76: TLabel
          Left = 8
          Top = 10
          Width = 76
          Height = 13
          Alignment = taRightJustify
          Anchors = []
          Caption = #1054#1089#1085#1086#1074#1085#1086#1081' '#1094#1074#1077#1090
          ExplicitLeft = 38
          ExplicitTop = 20
        end
        object CBWMainColor: TColorBox
          Left = 280
          Top = 5
          Width = 145
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
          Anchors = []
          ItemHeight = 16
          TabOrder = 0
        end
        object Label77: TLabel
          Left = 1
          Top = 41
          Width = 90
          Height = 13
          Alignment = taRightJustify
          Anchors = []
          Caption = #1062#1074#1077#1090' '#1086#1082#1072#1081#1084#1083#1077#1085#1080#1103
          ExplicitLeft = 24
          ExplicitTop = 44
        end
        object CBWFonColor: TColorBox
          Left = 280
          Top = 36
          Width = 145
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
          Anchors = []
          ItemHeight = 16
          TabOrder = 1
        end
        object Label78: TLabel
          Left = 201
          Top = 63
          Width = 303
          Height = 13
          Anchors = []
          Caption = '*'#1055#1088#1080' '#1074#1099#1073#1086#1088#1077' '#1095#1077#1088#1085#1086#1075#1086' '#1094#1074#1077#1090#1072' '#1074#1099' '#1087#1086#1083#1091#1095#1080#1090#1077' '#1087#1088#1086#1079#1088#1072#1095#1085#1091#1102' '#1083#1080#1085#1080#1102
          ExplicitLeft = 181
          ExplicitTop = 119
        end
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'GSM'
      ImageIndex = 7
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlGSM: TPanel
        Left = 0
        Top = 0
        Width = 616
        Height = 365
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object chkPosFromGSM: TCheckBox
          Left = 3
          Top = 3
          Width = 610
          Height = 17
          Align = alTop
          Caption = 
            #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1086#1087#1088#1077#1076#1077#1083#1103#1090#1100' '#1087#1072#1088#1072#1084#1077#1090#1088#1099' '#1073#1072#1079#1086#1074#1099#1093' '#1089#1090#1072#1085#1094#1080#1081' '#1095#1077#1088#1077#1079' '#1087#1086#1076#1082#1083#1102#1095 +
            #1077#1085#1085#1099#1081' '#1090#1077#1083#1077#1092#1086#1085
          TabOrder = 0
          OnClick = chkPosFromGSMClick
        end
        object flwpnlGSM: TFlowPanel
          Left = 3
          Top = 20
          Width = 610
          Height = 342
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          object Label33: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 50
            Height = 13
            Caption = 'COM-'#1087#1086#1088#1090
          end
          object CBGSMComPort: TComboBox
            Left = 56
            Top = 0
            Width = 89
            Height = 21
            ItemHeight = 0
            TabOrder = 0
            Text = 'COM1'
          end
          object Label34: TLabel
            AlignWithMargins = True
            Left = 148
            Top = 3
            Width = 48
            Height = 13
            Alignment = taRightJustify
            Caption = #1057#1082#1086#1088#1086#1089#1090#1100
          end
          object CBGSMBaundRate: TComboBox
            Left = 199
            Top = 0
            Width = 89
            Height = 21
            ItemHeight = 13
            ItemIndex = 5
            TabOrder = 1
            Text = '4800'
            Items.Strings = (
              '110'
              '300'
              '600'
              '1200'
              '2400'
              '4800'
              '9600'
              '14400'
              '19200'
              '38400'
              '57600'
              '115200')
          end
          object Label36: TLabel
            AlignWithMargins = True
            Left = 291
            Top = 3
            Width = 89
            Height = 13
            Alignment = taRightJustify
            BiDiMode = bdRightToLeft
            Caption = #1054#1078#1080#1076#1072#1085#1080#1077' '#1086#1090#1074#1077#1090#1072
            ParentBiDiMode = False
          end
          object SEWaitingAnswer: TSpinEdit
            Left = 383
            Top = 0
            Width = 63
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 2
            Value = 200
          end
        end
      end
    end
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 393
    Width = 624
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 546
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 465
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = #1054#1050
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = btnApplyClick
    end
    object btnApply: TButton
      AlignWithMargins = True
      Left = 384
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
      TabOrder = 2
      OnClick = btnApplyClick
    end
  end
  object XPManifest1: TXPManifest
    Left = 236
    Top = 480
  end
end
