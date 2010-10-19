object frMapCombine: TfrMapCombine
  Left = 0
  Top = 0
  Width = 621
  Height = 304
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 451
  object Bevel1: TBevel
    Left = 0
    Top = 152
    Width = 621
    Height = 5
    Align = alTop
    Shape = bsTopLine
    ExplicitWidth = 451
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 621
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1057#1082#1083#1077#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1081' '#1092#1088#1072#1075#1084#1077#1085#1090
    TabOrder = 0
    ExplicitWidth = 451
    object lblStat: TLabel
      Left = 612
      Top = 3
      Width = 6
      Height = 12
      Align = alRight
      Caption = '_'
      ExplicitLeft = 442
      ExplicitHeight = 13
    end
  end
  object pnlTargetFile: TPanel
    Left = 0
    Top = 47
    Width = 621
    Height = 25
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitTop = 50
    ExplicitWidth = 451
    object lblTargetFile: TLabel
      Left = 3
      Top = 3
      Width = 86
      Height = 19
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitTop = 6
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 89
      Top = 3
      Width = 508
      Height = 19
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 338
      ExplicitHeight = 21
    end
    object btnSelectTargetFile: TButton
      Left = 597
      Top = 3
      Width = 21
      Height = 19
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
      ExplicitLeft = 427
    end
  end
  object pnlOutputFormat: TPanel
    Left = 0
    Top = 20
    Width = 621
    Height = 27
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    ExplicitWidth = 451
    object lblOutputFormat: TLabel
      Left = 3
      Top = 3
      Width = 134
      Height = 21
      Align = alLeft
      Caption = #1056#1077#1079#1091#1083#1100#1090#1080#1088#1091#1102#1097#1080#1081' '#1092#1086#1088#1084#1072#1090':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object cbbOutputFormat: TComboBox
      Left = 137
      Top = 3
      Width = 481
      Height = 21
      Align = alClient
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Ecw (Enhanced Compression Wavelet)'
      OnChange = cbbOutputFormatChange
      Items.Strings = (
        'Ecw (Enhanced Compression Wavelet)'
        'Bmp (Bitmap Picture) '#1073#1077#1079' '#1089#1078#1072#1090#1080#1103
        'Kmz '#1076#1083#1103' Garmin'
        'JPEG (Joint Photographic Experts Group)'
        'Jpeg2000')
      ExplicitLeft = 133
      ExplicitWidth = 315
    end
  end
  object pnlRight: TPanel
    Left = 456
    Top = 157
    Width = 165
    Height = 147
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 3
    object grpSplit: TGroupBox
      Left = 3
      Top = 3
      Width = 159
      Height = 63
      Align = alTop
      Caption = #1056#1072#1079#1073#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077
      TabOrder = 0
      ExplicitTop = 37
      object lblSplitHor: TLabel
        Left = 7
        Top = 16
        Width = 92
        Height = 13
        Caption = #1087#1086' '#1075#1086#1088#1080#1079#1086#1090#1072#1083#1080', '#1085#1072
      end
      object lblSplitVert: TLabel
        AlignWithMargins = True
        Left = 7
        Top = 40
        Width = 88
        Height = 13
        Caption = #1087#1086' '#1074#1077#1088#1090#1080#1082#1072#1083#1080', '#1085#1072
      end
      object seSplitHor: TSpinEdit
        Left = 106
        Top = 13
        Width = 49
        Height = 22
        MaxValue = 1000
        MinValue = 1
        TabOrder = 0
        Value = 1
      end
      object seSplitVert: TSpinEdit
        Left = 106
        Top = 37
        Width = 49
        Height = 22
        MaxValue = 1000
        MinValue = 1
        TabOrder = 1
        Value = 1
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 72
    Width = 621
    Height = 80
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 4
    ExplicitWidth = 451
    object Panel2: TPanel
      Left = 3
      Top = 3
      Width = 543
      Height = 74
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      Padding.Right = 3
      TabOrder = 0
      ExplicitWidth = 373
      object lblMap: TLabel
        Left = 0
        Top = 0
        Width = 540
        Height = 13
        Align = alTop
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitWidth = 57
      end
      object lblHybr: TLabel
        Left = 0
        Top = 34
        Width = 540
        Height = 13
        Align = alTop
        Caption = #1053#1072#1083#1086#1078#1080#1090#1100':'
        ExplicitLeft = 3
        ExplicitTop = 31
        ExplicitWidth = 370
      end
      object cbbMap: TComboBox
        Left = 0
        Top = 13
        Width = 540
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitLeft = -3
        ExplicitTop = 96
        ExplicitWidth = 451
      end
      object cbbHybr: TComboBox
        Left = 0
        Top = 47
        Width = 540
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = #1053#1077#1090
        Items.Strings = (
          #1053#1077#1090)
        ExplicitTop = 14
        ExplicitWidth = 370
      end
    end
    object Panel3: TPanel
      Left = 546
      Top = 3
      Width = 72
      Height = 74
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 379
      object lblZoom: TLabel
        Left = 0
        Top = 0
        Width = 72
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
        ExplicitWidth = 49
      end
      object cbbZoom: TComboBox
        Left = 0
        Top = 13
        Width = 72
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbZoomChange
        ExplicitLeft = 3
        ExplicitTop = 16
        ExplicitWidth = 159
      end
    end
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 157
    Width = 294
    Height = 147
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 5
    ExplicitWidth = 289
    object chkUseMapMarks: TCheckBox
      Left = 3
      Top = 20
      Width = 288
      Height = 17
      Align = alTop
      Caption = #1053#1072#1082#1083#1072#1076#1099#1074#1072#1090#1100' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1077' '#1084#1077#1090#1082#1080
      TabOrder = 0
      ExplicitWidth = 323
    end
    object chkUseRecolor: TCheckBox
      Left = 3
      Top = 3
      Width = 288
      Height = 17
      Align = alTop
      Caption = #1055#1088#1080#1084#1077#1085#1103#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1094#1074#1077#1090#1072
      TabOrder = 1
      ExplicitWidth = 323
    end
    object flwpnlJpegQuality: TFlowPanel
      Left = 3
      Top = 37
      Width = 288
      Height = 25
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Padding.Top = 3
      TabOrder = 2
      ExplicitWidth = 323
      object lblJpgQulity: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 6
        Width = 144
        Height = 13
        Alignment = taRightJustify
        Caption = #1050#1072#1095#1077#1089#1090#1074#1086' ('#1076#1083#1103' JPEG '#1080' ECW):'
        Layout = tlCenter
      end
      object seJpgQuality: TSpinEdit
        Left = 150
        Top = 3
        Width = 73
        Height = 22
        MaxValue = 100
        MinValue = 1
        TabOrder = 0
        Value = 95
      end
    end
  end
  object Panel4: TPanel
    Left = 294
    Top = 157
    Width = 162
    Height = 147
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 6
    ExplicitLeft = 271
    object lblPrTypes: TLabel
      Left = 3
      Top = 3
      Width = 156
      Height = 13
      Align = alTop
      Caption = #1057#1086#1079#1076#1072#1074#1072#1090#1100' '#1092#1072#1081#1083' '#1087#1088#1080#1074#1103#1079#1082#1080':'
      WordWrap = True
      ExplicitWidth = 138
    end
    object chklstPrTypes: TCheckListBox
      Left = 3
      Top = 16
      Width = 156
      Height = 49
      Align = alTop
      ItemHeight = 13
      Items.Strings = (
        '.map'
        '.tab'
        '.w'
        '.dat'
        '.kml')
      TabOrder = 0
      ExplicitWidth = 179
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Left = 48
    Top = 232
  end
end
