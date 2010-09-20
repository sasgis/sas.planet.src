object FSettings: TFSettings
  Left = 293
  Top = 114
  BorderStyle = bsDialog
  BorderWidth = 3
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1088#1086#1075#1088#1072#1084#1084#1099
  ClientHeight = 410
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 643
    Height = 379
    ActivePage = TabSheet9
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 633
    ExplicitHeight = 369
    object TabSheet9: TTabSheet
      Caption = #1050#1072#1088#1090#1099
      ImageIndex = 8
      ExplicitWidth = 625
      ExplicitHeight = 341
      object Button11: TButton
        Left = 544
        Top = 40
        Width = 75
        Height = 25
        Caption = #1042#1085#1080#1079
        TabOrder = 0
        OnClick = Button11Click
      end
      object Button12: TButton
        Left = 544
        Top = 8
        Width = 75
        Height = 25
        Caption = #1042#1074#1077#1088#1093
        TabOrder = 1
        OnClick = Button12Click
      end
      object Button15: TButton
        Left = 544
        Top = 72
        Width = 75
        Height = 25
        Caption = #1055#1072#1088#1072#1084#1077#1090#1088#1099
        TabOrder = 2
        OnClick = Button15Click
      end
      object MapList: TListView
        Left = 8
        Top = 8
        Width = 529
        Height = 321
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
        TabOrder = 3
        ViewStyle = vsReport
        OnCustomDrawItem = MapListCustomDrawItem
        OnCustomDrawSubItem = MapListCustomDrawSubItem
        OnDblClick = Button15Click
      end
      object Button18: TButton
        Left = 544
        Top = 112
        Width = 75
        Height = 25
        Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103
        TabOrder = 4
        OnClick = Button18Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = #1050#1101#1096
      ImageIndex = 1
      ExplicitWidth = 625
      ExplicitHeight = 341
      object Label2: TLabel
        Left = 8
        Top = 40
        Width = 117
        Height = 13
        Caption = #1055#1091#1090#1100' '#1082' '#1082#1101#1096#1091' GoogleMV:'
      end
      object Label15: TLabel
        Left = 8
        Top = 16
        Width = 120
        Height = 13
        Caption = #1055#1091#1090#1100' '#1082' "'#1088#1086#1076#1085#1086#1084#1091'" '#1082#1101#1096#1091':'
      end
      object Label1: TLabel
        Left = 8
        Top = 64
        Width = 117
        Height = 13
        Caption = #1055#1091#1090#1100' '#1082' '#1082#1101#1096#1091' EarthSliser:'
      end
      object Label19: TLabel
        Left = 8
        Top = 88
        Width = 109
        Height = 13
        Caption = #1055#1091#1090#1100' '#1082' '#1082#1101#1096#1091' GM Tiles:'
      end
      object Label30: TLabel
        Left = 8
        Top = 160
        Width = 281
        Height = 13
        Caption = #1050#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1090#1072#1081#1083#1086#1074' '#1082#1101#1096#1080#1088#1091#1077#1084#1086#1077' '#1074' '#1086#1087#1077#1088#1072#1090#1080#1074#1085#1091#1102' '#1087#1072#1084#1103#1090#1100
      end
      object Label31: TLabel
        Left = 8
        Top = 112
        Width = 129
        Height = 13
        Caption = #1055#1091#1090#1100' '#1082' '#1082#1101#1096#1091' Google Earth:'
      end
      object Label37: TLabel
        Left = 8
        Top = 136
        Width = 48
        Height = 13
        Caption = #1058#1080#1087' '#1082#1101#1096#1072
      end
      object OldCpath: TEdit
        Left = 144
        Top = 36
        Width = 432
        Height = 21
        TabOrder = 0
      end
      object NewCpath: TEdit
        Left = 144
        Top = 12
        Width = 432
        Height = 21
        TabOrder = 1
      end
      object Button4: TButton
        Tag = 1
        Left = 576
        Top = 36
        Width = 21
        Height = 21
        Caption = '<>'
        TabOrder = 2
        OnClick = Button4Click
      end
      object Button5: TButton
        Tag = 1
        Left = 596
        Top = 36
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 3
        OnClick = Button5Click
      end
      object Button6: TButton
        Tag = 2
        Left = 576
        Top = 12
        Width = 21
        Height = 21
        Caption = '<>'
        TabOrder = 4
        OnClick = Button4Click
      end
      object Button7: TButton
        Tag = 2
        Left = 596
        Top = 12
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 5
        OnClick = Button5Click
      end
      object EScPath: TEdit
        Left = 144
        Top = 60
        Width = 432
        Height = 21
        TabOrder = 6
      end
      object Button8: TButton
        Tag = 3
        Left = 576
        Top = 60
        Width = 21
        Height = 21
        Caption = '<>'
        TabOrder = 7
        OnClick = Button4Click
      end
      object Button9: TButton
        Tag = 3
        Left = 596
        Top = 60
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 8
        OnClick = Button5Click
      end
      object GMTilesPath: TEdit
        Left = 144
        Top = 84
        Width = 432
        Height = 21
        TabOrder = 9
      end
      object Button13: TButton
        Tag = 4
        Left = 576
        Top = 84
        Width = 21
        Height = 21
        Caption = '<>'
        TabOrder = 10
        OnClick = Button4Click
      end
      object Button14: TButton
        Tag = 4
        Left = 596
        Top = 84
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 11
        OnClick = Button5Click
      end
      object SETilesOCache: TSpinEdit
        Left = 296
        Top = 157
        Width = 65
        Height = 22
        MaxValue = 10000
        MinValue = 0
        TabOrder = 12
        Value = 0
      end
      object GECachePath: TEdit
        Left = 144
        Top = 108
        Width = 432
        Height = 21
        TabOrder = 13
      end
      object Button10: TButton
        Tag = 5
        Left = 576
        Top = 108
        Width = 21
        Height = 21
        Caption = '<>'
        TabOrder = 14
        OnClick = Button4Click
      end
      object Button17: TButton
        Tag = 5
        Left = 596
        Top = 108
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 15
        OnClick = Button5Click
      end
      object CBCacheType: TComboBox
        Left = 144
        Top = 132
        Width = 433
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 16
        Text = 'SAS.'#1055#1083#1072#1085#1077#1090#1072
        Items.Strings = (
          'GoogleMV'
          'SAS.'#1055#1083#1072#1085#1077#1090#1072
          'EarthSlicer 1.95'
          'Googe maps tiles')
      end
    end
    object TabSheet1: TTabSheet
      Caption = #1048#1085#1090#1077#1088#1085#1077#1090
      ImageIndex = 1
      ExplicitWidth = 625
      ExplicitHeight = 341
      object Label13: TLabel
        Left = 8
        Top = 10
        Width = 147
        Height = 13
        Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1086#1076#1082#1083#1102#1095#1077#1085#1080#1103
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel14: TBevel
        Left = 8
        Top = 152
        Width = 609
        Height = 9
        Shape = bsBottomLine
      end
      object Label32: TLabel
        Left = 8
        Top = 248
        Width = 175
        Height = 13
        Caption = #1058#1072#1081#1084#1072#1091#1090' '#1085#1072' '#1089#1077#1090#1077#1074#1099#1077' '#1086#1087#1077#1088#1072#1094#1080#1080', '#1084#1089
      end
      object Bevel16: TBevel
        Left = 8
        Top = 272
        Width = 609
        Height = 9
        Shape = bsBottomLine
      end
      object RBWinCon: TRadioButton
        Left = 17
        Top = 32
        Width = 344
        Height = 17
        Caption = #1041#1088#1072#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1086#1076#1082#1083#1102#1095#1077#1085#1080#1103' '#1080#1079' '#1088#1077#1077#1089#1090#1088#1072
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object GroupBox4: TGroupBox
        Left = 8
        Top = 59
        Width = 361
        Height = 86
        Caption = '  '
        TabOrder = 1
        object Label25: TLabel
          Left = 223
          Top = 56
          Width = 48
          Height = 13
          Caption = #1080' '#1087#1072#1088#1086#1083#1100':'
        end
        object CBProxyused: TCheckBox
          Left = 8
          Top = 23
          Width = 169
          Height = 17
          Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1087#1088#1086#1082#1089#1080' (ip:port)'
          TabOrder = 0
        end
        object EditPass: TEdit
          Left = 272
          Top = 53
          Width = 81
          Height = 21
          PasswordChar = '*'
          TabOrder = 1
        end
        object EditLogin: TEdit
          Left = 136
          Top = 53
          Width = 81
          Height = 21
          TabOrder = 2
        end
        object CBLogin: TCheckBox
          Left = 8
          Top = 55
          Width = 128
          Height = 17
          Caption = #1048#1089#1087#1086#1083#1100#1079#1086#1074#1072#1090#1100' '#1083#1086#1075#1080#1085':'
          TabOrder = 3
        end
        object EditIP: TEdit
          Left = 184
          Top = 21
          Width = 169
          Height = 21
          TabOrder = 4
        end
      end
      object RBMyCon: TRadioButton
        Left = 17
        Top = 57
        Width = 172
        Height = 17
        Caption = #1057#1074#1086#1080' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1087#1086#1076#1082#1083#1102#1095#1077#1085#1080#1103
        TabOrder = 2
      end
      object CBDblDwnl: TCheckBox
        Left = 8
        Top = 168
        Width = 305
        Height = 17
        Caption = #1055#1086#1074#1090#1086#1088#1085#1086' '#1087#1099#1090#1072#1090#1100#1089#1103' '#1079#1072#1075#1088#1091#1079#1080#1090#1100' '#1090#1072#1081#1083' '#1087#1088#1080' '#1077#1075#1086' '#1086#1090#1089#1090#1091#1090#1089#1074#1080#1080
        TabOrder = 3
      end
      object CkBGoNextTile: TCheckBox
        Left = 8
        Top = 192
        Width = 305
        Height = 17
        Caption = #1055#1077#1088#1077#1093#1086#1076' '#1082' '#1089#1083#1077#1076#1091#1102#1097#1077#1084#1091' '#1090#1072#1081#1083#1091' '#1077#1089#1083#1080' '#1089#1077#1088#1074#1077#1088' '#1085#1077' '#1086#1090#1074#1077#1095#1072#1077#1090
        TabOrder = 4
      end
      object CBSaveTileNotExists: TCheckBox
        Left = 8
        Top = 216
        Width = 313
        Height = 17
        Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1080#1085#1092#1086#1088#1084#1072#1094#1080#1102' '#1086#1073' '#1086#1090#1089#1091#1090#1089#1090#1074#1080#1080' '#1090#1072#1081#1083#1072' '#1085#1072' '#1089#1077#1088#1074#1077#1088#1077
        TabOrder = 5
      end
      object SETimeOut: TSpinEdit
        Left = 192
        Top = 245
        Width = 73
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 6
        Value = 0
      end
      object CBLastSuccess: TCheckBox
        Left = 8
        Top = 288
        Width = 609
        Height = 17
        Caption = 
          #1053#1072#1095#1072#1090#1100' '#1089#1086#1093#1088#1072#1085#1077#1085#1085#1091#1102' '#1089#1077#1089#1089#1080#1102' '#1079#1072#1075#1088#1091#1079#1082#1080' '#1089' '#1087#1086#1089#1083#1077#1076#1085#1077#1075#1086' '#1091#1076#1072#1095#1085#1086' '#1079#1072#1075#1088#1091#1078#1077#1085#1085 +
          #1086#1075#1086' '#1090#1072#1081#1083#1072
        TabOrder = 7
      end
    end
    object TabSheet3: TTabSheet
      Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077
      ImageIndex = 2
      ExplicitWidth = 625
      ExplicitHeight = 341
      object GroupBox1: TGroupBox
        Left = 8
        Top = 8
        Width = 313
        Height = 57
        Caption = ' '#1050#1086#1083#1077#1089#1080#1082#1086' '#1084#1099#1096#1080' '
        TabOrder = 0
        object ScrolInvert: TCheckBox
          Left = 24
          Top = 24
          Width = 201
          Height = 17
          Caption = #1074#1088#1072#1097#1077#1085#1080#1077' '#1085#1072' '#1089#1077#1073#1103' - '#1087#1088#1080#1073#1083#1080#1078#1077#1085#1080#1077
          TabOrder = 0
        end
      end
      object GroupBox5: TGroupBox
        Left = 8
        Top = 72
        Width = 609
        Height = 265
        Caption = ' '#1043#1086#1088#1103#1095#1080#1077' '#1082#1083'. '
        TabOrder = 1
        object Label40: TLabel
          Left = 31
          Top = 17
          Width = 57
          Height = 13
          Caption = #1054#1087#1077#1088#1072#1094#1080#1103
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label55: TLabel
          Left = 536
          Top = 17
          Width = 43
          Height = 13
          Caption = #1043#1086#1088'. '#1082#1083'.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object List: TListBox
        Left = 16
        Top = 104
        Width = 593
        Height = 225
        Style = lbOwnerDrawFixed
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 20
        ParentFont = False
        TabOrder = 2
      end
    end
    object TabSheet4: TTabSheet
      Caption = #1042#1085#1077#1096#1085#1080#1081' '#1074#1080#1076
      ImageIndex = 3
      ExplicitWidth = 625
      ExplicitHeight = 341
      object Bevel1: TBevel
        Left = 8
        Top = 8
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label3: TLabel
        Left = 13
        Top = 23
        Width = 198
        Height = 13
        Alignment = taRightJustify
        Caption = #1060#1086#1088#1084#1072#1090' '#1095#1080#1089#1077#1083' '#1086#1090#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1084#1072#1089#1096#1090#1072#1073#1072
      end
      object Bevel2: TBevel
        Left = 8
        Top = 120
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object LabelGamma: TLabel
        Left = 8
        Top = 130
        Width = 6
        Height = 13
        Caption = '_'
      end
      object LabelContrast: TLabel
        Left = 8
        Top = 170
        Width = 6
        Height = 13
        Caption = '_'
      end
      object Bevel3: TBevel
        Left = 8
        Top = 40
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label14: TLabel
        Left = 11
        Top = 103
        Width = 241
        Height = 13
        Alignment = taRightJustify
        Caption = #1056#1072#1079#1085#1086#1089#1090#1100' '#1084#1072#1089#1096#1090#1072#1073#1086#1074' '#1082#1072#1088#1090#1099' '#1086#1073#1079#1086#1088#1072' '#1080' '#1086#1089#1085#1086#1074#1085#1086#1081
      end
      object Bevel4: TBevel
        Left = 8
        Top = 232
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label16: TLabel
        Left = 8
        Top = 248
        Width = 193
        Height = 13
        Alignment = taRightJustify
        Caption = #1040#1083#1075#1086#1088#1080#1090#1084' '#1088#1072#1089#1090#1103#1075#1080#1074#1072#1085#1080#1103' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103
      end
      object Bevel5: TBevel
        Left = 8
        Top = 264
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label17: TLabel
        Left = 408
        Top = 16
        Width = 145
        Height = 13
        Alignment = taRightJustify
        Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' '#1082#1072#1088#1090#1099' '#1086#1073#1079#1086#1088#1072
      end
      object Bevel6: TBevel
        Left = 320
        Top = 32
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Bevel7: TBevel
        Left = 312
        Top = 8
        Width = 9
        Height = 321
        Shape = bsLeftLine
      end
      object Bevel12: TBevel
        Left = 320
        Top = 72
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label69: TLabel
        Left = 320
        Top = 51
        Width = 255
        Height = 13
        Alignment = taRightJustify
        Caption = #1050#1086#1083'-'#1074#1086' '#1090#1072#1081#1083#1086#1074' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1093' '#1079#1072' '#1075#1088#1072#1085#1080#1094#1077#1081' '#1101#1082#1088#1072#1085#1072
      end
      object Bevel8: TBevel
        Left = 8
        Top = 88
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label84: TLabel
        Left = 9
        Top = 55
        Width = 178
        Height = 13
        Alignment = taRightJustify
        Caption = #1060#1086#1088#1084#1072#1090' '#1087#1088#1077#1076#1089#1090#1072#1074#1083#1077#1085#1080#1103' '#1082#1086#1086#1088#1076#1080#1085#1072#1090
      end
      object Bevel13: TBevel
        Left = 320
        Top = 104
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label27: TLabel
        Left = 8
        Top = 304
        Width = 25
        Height = 13
        Caption = #1062#1074#1077#1090
      end
      object Label28: TLabel
        Left = 120
        Top = 304
        Width = 72
        Height = 13
        Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100
      end
      object Label8: TLabel
        Left = 440
        Top = 120
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = #1071#1079#1099#1082
      end
      object Bevel10: TBevel
        Left = 320
        Top = 136
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label23: TLabel
        Left = 8
        Top = 280
        Width = 110
        Height = 13
        Caption = #1054#1090#1086#1073#1088#1072#1078#1077#1085#1080#1077' '#1075#1088#1072#1085#1080#1094':'
      end
      object Label24: TLabel
        Left = 320
        Top = 148
        Width = 96
        Height = 13
        Caption = #1050#1072#1088#1090#1072' '#1079#1072#1087#1086#1083#1085#1077#1085#1080#1103':'
      end
      object Label26: TLabel
        Left = 320
        Top = 168
        Width = 25
        Height = 13
        Caption = #1062#1074#1077#1090
      end
      object Label29: TLabel
        Left = 502
        Top = 168
        Width = 72
        Height = 13
        Alignment = taRightJustify
        Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100
      end
      object Bevel9: TBevel
        Left = 320
        Top = 184
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Bevel11: TBevel
        Left = 320
        Top = 216
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Bevel15: TBevel
        Left = 320
        Top = 248
        Width = 297
        Height = 9
        Shape = bsBottomLine
      end
      object Label35: TLabel
        Left = 320
        Top = 264
        Width = 54
        Height = 13
        Caption = #1062#1074#1077#1090' '#1092#1086#1085#1072
      end
      object ComboBox1: TComboBox
        Left = 216
        Top = 21
        Width = 89
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = '12 '#1082#1084'. 423 '#1084'.'
        Items.Strings = (
          '12 '#1082#1084'. 423 '#1084'.'
          '23,4 '#1082#1084'.')
      end
      object TrBarGamma: TTrackBar
        Left = 8
        Top = 144
        Width = 297
        Height = 25
        Max = 100
        Min = 1
        ParentShowHint = False
        Frequency = 5
        Position = 1
        ShowHint = False
        TabOrder = 1
        ThumbLength = 15
        TickMarks = tmTopLeft
        OnChange = TrBarGammaChange
      end
      object TrBarContrast: TTrackBar
        Left = 8
        Top = 184
        Width = 297
        Height = 25
        Max = 100
        Min = -100
        Frequency = 10
        TabOrder = 2
        ThumbLength = 15
        TickMarks = tmTopLeft
        OnChange = TrBarContrastChange
      end
      object smmapdif: TSpinEdit
        Left = 256
        Top = 100
        Width = 49
        Height = 22
        MaxValue = 24
        MinValue = 1
        TabOrder = 3
        Value = 1
      end
      object ComboBox2: TComboBox
        Left = 208
        Top = 245
        Width = 97
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 4
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
      object SpinEditMiniMap: TSpinEdit
        Left = 560
        Top = 12
        Width = 57
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 5
        Value = 1
      end
      object SpinEdit3: TSpinEdit
        Left = 580
        Top = 48
        Width = 37
        Height = 22
        MaxValue = 24
        MinValue = -2
        TabOrder = 6
        Value = 0
      end
      object CB_llstrType: TComboBox
        Left = 192
        Top = 53
        Width = 113
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 7
        Text = 'WS '#1075'.'#1084'.'#1089'. (W12'#176'23"43.35'#39')'
        Items.Strings = (
          'WS '#1075'.'#1084'.'#1089'. (W12'#176'23"43.35'#39')'
          'WS '#1075'.'#1084'. (W12'#176'23.454)'
          'WS '#1075'. (W12.1233'#176')'
          '-- '#1075'.'#1084'.'#1089'. (-12'#176'23"43.35'#39')'
          '-- '#1075'.'#1084'. (-12'#176'23.454)'
          '-- '#1075'. (-12.1233'#176')')
      end
      object CBShowmapname: TCheckBox
        Left = 320
        Top = 88
        Width = 297
        Height = 17
        Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1085#1072#1079#1074#1072#1085#1080#1077' '#1082#1072#1088#1090#1099' '#1085#1072' '#1087#1072#1085#1077#1083#1080' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
        TabOrder = 8
      end
      object CBinvertcolor: TCheckBox
        Left = 8
        Top = 216
        Width = 121
        Height = 17
        Caption = #1048#1085#1074#1077#1088#1089#1080#1103' '#1094#1074#1077#1090#1086#1074
        TabOrder = 9
      end
      object SpinEditBorderAlpha: TSpinEdit
        Left = 194
        Top = 300
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 10
        Value = 255
      end
      object ColorBoxBorder: TColorBox
        Left = 39
        Top = 300
        Width = 78
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 11
      end
      object CBoxLocal: TComboBox
        Left = 472
        Top = 117
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 12
        Items.Strings = (
          #1056#1091#1089#1089#1082#1080#1081
          'English')
      end
      object ChBoxFirstLat: TCheckBox
        Left = 8
        Top = 75
        Width = 225
        Height = 17
        Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1074' '#1087#1086#1088#1103#1076#1082#1077': '#1096#1080#1088#1086#1090#1072'-'#1076#1086#1083#1075#1086#1090#1072
        TabOrder = 13
      end
      object CBBorderText: TCheckBox
        Left = 240
        Top = 304
        Width = 65
        Height = 17
        Caption = #1055#1086#1076#1087#1080#1089#1100
        TabOrder = 14
      end
      object MapZapColorBox: TColorBox
        Left = 351
        Top = 164
        Width = 78
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 15
      end
      object MapZapAlphaEdit: TSpinEdit
        Left = 578
        Top = 164
        Width = 41
        Height = 22
        MaxValue = 255
        MinValue = 0
        TabOrder = 16
        Value = 255
      end
      object CBlock_toolbars: TCheckBox
        Left = 320
        Top = 200
        Width = 193
        Height = 17
        Caption = #1047#1072#1082#1088#1077#1087#1080#1090#1100' '#1087#1072#1085#1077#1083#1080' '#1080#1085#1089#1090#1088#1091#1084#1077#1085#1090#1086#1074
        TabOrder = 17
      end
      object CBShowHintOnMarks: TCheckBox
        Left = 320
        Top = 232
        Width = 297
        Height = 17
        Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1086#1087#1080#1089#1072#1085#1080#1077' '#1084#1077#1090#1082#1080' '#1087#1088#1080' '#1085#1072#1074#1077#1076#1077#1085#1080#1080' '#1082#1091#1088#1089#1086#1088#1072
        TabOrder = 18
      end
      object ColorBoxBackGround: TColorBox
        Left = 383
        Top = 260
        Width = 78
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 19
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'GPS'
      ImageIndex = 4
      ExplicitWidth = 625
      ExplicitHeight = 341
      object Label4: TLabel
        Left = 8
        Top = 19
        Width = 50
        Height = 13
        Caption = 'COM-'#1087#1086#1088#1090
      end
      object Label6: TLabel
        Left = 8
        Top = 51
        Width = 226
        Height = 13
        Caption = #1042#1088#1077#1084#1103' '#1086#1078#1080#1076#1072#1085#1080#1103' '#1086#1090#1074#1077#1090#1072' '#1086#1090' '#1087#1088#1080#1077#1084#1085#1080#1082#1072' ('#1089#1077#1082'.)'
      end
      object Label11: TLabel
        Left = 8
        Top = 91
        Width = 160
        Height = 13
        Caption = #1055#1077#1088#1080#1086#1076' '#1086#1073#1085#1086#1074#1083#1077#1085#1080#1103' ('#1089#1077#1082'./1000)'
      end
      object Label10: TLabel
        Left = 8
        Top = 139
        Width = 166
        Height = 13
        Caption = #1056#1072#1079#1084#1077#1088' '#1091#1082#1072#1079#1072#1090#1077#1083#1103' '#1085#1072#1087#1088#1072#1074#1083#1077#1085#1080#1103':'
      end
      object Label12: TLabel
        Left = 8
        Top = 219
        Width = 60
        Height = 13
        Caption = #1062#1074#1077#1090' '#1090#1088#1077#1082#1072':'
      end
      object Label20: TLabel
        Left = 8
        Top = 179
        Width = 74
        Height = 13
        Caption = #1064#1080#1088#1080#1085#1072' '#1090#1088#1077#1082#1072':'
      end
      object Label65: TLabel
        Left = 200
        Top = 19
        Width = 48
        Height = 13
        Alignment = taRightJustify
        Caption = #1057#1082#1086#1088#1086#1089#1090#1100
      end
      object SBGetComNum: TSpeedButton
        Left = 156
        Top = 16
        Width = 21
        Height = 21
        Caption = '?'
        Margin = 5
        OnClick = SBGetComNumClick
      end
      object Label5: TLabel
        Left = 8
        Top = 259
        Width = 283
        Height = 13
        Caption = #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1086#1077' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1086' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1093' '#1090#1086#1095#1077#1082' '#1090#1088#1077#1082#1072':'
      end
      object ComboBoxCOM: TComboBox
        Left = 64
        Top = 16
        Width = 89
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'COM1'
      end
      object SpinEdit1: TSpinEdit
        Left = 8
        Top = 106
        Width = 57
        Height = 22
        MaxValue = 3600000
        MinValue = 100
        TabOrder = 1
        Value = 100
      end
      object SpinEdit2: TSpinEdit
        Left = 8
        Top = 67
        Width = 57
        Height = 22
        MaxValue = 86400
        MinValue = 1
        TabOrder = 2
        Value = 1
      end
      object GroupBox2: TGroupBox
        Left = 376
        Top = 261
        Width = 241
        Height = 68
        Caption = ' '#1055#1086#1087#1088#1072#1074#1082#1072' '
        TabOrder = 3
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
      object CB_GPSlog: TCheckBox
        Left = 8
        Top = 304
        Width = 217
        Height = 17
        Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1089#1086#1093#1088#1072#1085#1103#1090#1100' '#1090#1088#1077#1082#1080' '#1074' .plt'
        TabOrder = 4
      end
      object SESizeStr: TSpinEdit
        Left = 8
        Top = 154
        Width = 57
        Height = 22
        MaxValue = 150
        MinValue = 10
        TabOrder = 5
        Value = 100
      end
      object ColorBoxGPSstr: TColorBox
        Left = 8
        Top = 234
        Width = 105
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 6
      end
      object SESizeTrack: TSpinEdit
        Left = 8
        Top = 194
        Width = 57
        Height = 22
        MaxValue = 50
        MinValue = 1
        TabOrder = 7
        Value = 50
      end
      object ComboBoxBoudRate: TComboBox
        Left = 256
        Top = 16
        Width = 89
        Height = 21
        ItemHeight = 13
        ItemIndex = 5
        TabOrder = 8
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
      object GroupBox3: TGroupBox
        Left = 376
        Top = 8
        Width = 241
        Height = 249
        Caption = ' C'#1087#1091#1090#1085#1080#1082#1080' '
        TabOrder = 9
        object PaintBox1: TPaintBox
          Left = 8
          Top = 16
          Width = 225
          Height = 225
          OnPaint = PaintBox1Paint
        end
      end
      object CBSensorsBarAutoShow: TCheckBox
        Left = 8
        Top = 320
        Width = 305
        Height = 17
        Caption = #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1087#1086#1082#1072#1079#1099#1074#1072#1090#1100'/'#1089#1082#1088#1099#1074#1072#1090#1100' '#1087#1072#1085#1077#1083#1100' '#1076#1072#1090#1095#1080#1082#1086#1074
        TabOrder = 10
      end
      object SE_NumTrackPoints: TSpinEdit
        Left = 8
        Top = 274
        Width = 73
        Height = 22
        MaxValue = 1000000
        MinValue = 10
        TabOrder = 11
        Value = 10000
      end
      object CB_GPSlogNmea: TCheckBox
        Left = 224
        Top = 304
        Width = 65
        Height = 17
        Caption = #1074' .nmea'
        TabOrder = 12
      end
    end
    object TabSheet8: TTabSheet
      Caption = 'Wikimapia'
      ImageIndex = 7
      ExplicitWidth = 625
      ExplicitHeight = 341
      object Label76: TLabel
        Left = 38
        Top = 20
        Width = 76
        Height = 13
        Alignment = taRightJustify
        Caption = #1054#1089#1085#1086#1074#1085#1086#1081' '#1094#1074#1077#1090
      end
      object Label77: TLabel
        Left = 24
        Top = 44
        Width = 90
        Height = 13
        Alignment = taRightJustify
        Caption = #1062#1074#1077#1090' '#1086#1082#1072#1081#1084#1083#1077#1085#1080#1103
      end
      object Label78: TLabel
        Left = 120
        Top = 64
        Width = 303
        Height = 13
        Caption = '*'#1055#1088#1080' '#1074#1099#1073#1086#1088#1077' '#1095#1077#1088#1085#1086#1075#1086' '#1094#1074#1077#1090#1072' '#1074#1099' '#1087#1086#1083#1091#1095#1080#1090#1077' '#1087#1088#1086#1079#1088#1072#1095#1085#1091#1102' '#1083#1080#1085#1080#1102
      end
      object CBWMainColor: TColorBox
        Left = 120
        Top = 16
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 0
      end
      object CBWFonColor: TColorBox
        Left = 120
        Top = 40
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 1
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'GSM'
      ImageIndex = 7
      ExplicitWidth = 625
      ExplicitHeight = 341
      object Label33: TLabel
        Left = 8
        Top = 19
        Width = 50
        Height = 13
        Caption = 'COM-'#1087#1086#1088#1090
      end
      object Label34: TLabel
        Left = 208
        Top = 19
        Width = 48
        Height = 13
        Alignment = taRightJustify
        Caption = #1057#1082#1086#1088#1086#1089#1090#1100
      end
      object Label36: TLabel
        Left = 379
        Top = 19
        Width = 89
        Height = 13
        Alignment = taRightJustify
        BiDiMode = bdRightToLeft
        Caption = #1054#1078#1080#1076#1072#1085#1080#1077' '#1086#1090#1074#1077#1090#1072
        ParentBiDiMode = False
      end
      object CBGSMComPort: TComboBox
        Left = 64
        Top = 16
        Width = 89
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'COM1'
      end
      object CBGSMBaundRate: TComboBox
        Left = 264
        Top = 16
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
      object RBGSMAuto: TRadioButton
        Left = 8
        Top = 56
        Width = 473
        Height = 17
        Caption = 
          #1040#1074#1090#1086#1084#1072#1090#1080#1095#1077#1089#1082#1080' '#1086#1087#1088#1077#1076#1077#1083#1103#1090#1100' '#1087#1072#1088#1072#1084#1077#1090#1088#1099' '#1073#1072#1079#1086#1074#1099#1093' '#1089#1090#1072#1085#1094#1080#1081' '#1095#1077#1088#1077#1079' '#1087#1086#1076#1082#1083#1102#1095 +
          #1077#1085#1085#1099#1081' '#1090#1077#1083#1077#1092#1086#1085
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object RBGSMManual: TRadioButton
        Left = 8
        Top = 72
        Width = 273
        Height = 17
        Caption = #1042#1074#1086#1076#1080#1090#1100' '#1087#1072#1088#1072#1084#1077#1090#1088#1099' '#1073#1072#1079#1086#1074#1099#1093' '#1089#1090#1072#1085#1094#1080#1081' '#1074#1088#1091#1095#1085#1091#1102
        TabOrder = 3
      end
      object SEWaitingAnswer: TSpinEdit
        Left = 474
        Top = 16
        Width = 63
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 4
        Value = 200
      end
    end
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 379
    Width = 643
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      AlignWithMargins = True
      Left = 565
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      TabOrder = 0
      OnClick = Button1Click
      ExplicitLeft = 538
      ExplicitTop = 6
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 484
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = #1054#1050
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = Button3Click
      ExplicitLeft = 410
      ExplicitTop = 6
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 403
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
      TabOrder = 2
      OnClick = Button3Click
      ExplicitLeft = 177
      ExplicitTop = 4
    end
  end
  object XPManifest1: TXPManifest
    Left = 28
    Top = 384
  end
end
