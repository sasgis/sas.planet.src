object frMapCombine: TfrMapCombine
  Left = 0
  Top = 0
  Width = 552
  Height = 301
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 451
  ExplicitHeight = 304
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 552
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1057#1082#1083#1077#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1081' '#1092#1088#1072#1075#1084#1077#1085#1090
    TabOrder = 0
  end
  object pnlTargetFile: TPanel
    Left = 0
    Top = 49
    Width = 552
    Height = 25
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitTop = 47
    ExplicitWidth = 451
    object lblTargetFile: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 86
      Height = 19
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 92
      Top = 3
      Width = 436
      Height = 19
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 89
      ExplicitWidth = 338
      ExplicitHeight = 21
    end
    object btnSelectTargetFile: TButton
      Left = 528
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
    Top = 22
    Width = 552
    Height = 27
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    ExplicitTop = 20
    ExplicitWidth = 451
    object lblOutputFormat: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 134
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      Caption = #1056#1077#1079#1091#1083#1100#1090#1080#1088#1091#1102#1097#1080#1081' '#1092#1086#1088#1084#1072#1090':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object cbbOutputFormat: TComboBox
      Left = 140
      Top = 3
      Width = 409
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
      ExplicitLeft = 137
      ExplicitWidth = 311
    end
  end
  object pnlRight: TPanel
    Left = 384
    Top = 173
    Width = 168
    Height = 128
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 3
    ExplicitTop = 157
    ExplicitHeight = 144
    object grpSplit: TGroupBox
      Left = 3
      Top = 3
      Width = 162
      Height = 63
      Align = alTop
      Caption = #1056#1072#1079#1073#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077
      TabOrder = 0
      ExplicitWidth = 159
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
        Width = 47
        Height = 22
        MaxValue = 1000
        MinValue = 1
        TabOrder = 0
        Value = 1
      end
      object seSplitVert: TSpinEdit
        Left = 106
        Top = 37
        Width = 47
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
    Top = 74
    Width = 552
    Height = 99
    Align = alTop
    AutoSize = True
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 4
    object Panel2: TPanel
      Left = 3
      Top = 3
      Width = 474
      Height = 91
      Align = alClient
      BevelOuter = bvNone
      Padding.Right = 2
      TabOrder = 0
      ExplicitLeft = -3
      ExplicitTop = 5
      ExplicitHeight = 93
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 472
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 57
      end
      object lblHybr: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 40
        Width = 472
        Height = 13
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        Caption = #1053#1072#1083#1086#1078#1080#1090#1100':'
        ExplicitLeft = 3
        ExplicitWidth = 55
      end
      object lblStat: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 80
        Width = 472
        Height = 13
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = '_'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitTop = 73
        ExplicitWidth = 6
      end
      object cbbMap: TComboBox
        Left = 0
        Top = 16
        Width = 472
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitTop = 13
        ExplicitWidth = 370
      end
      object cbbHybr: TComboBox
        Left = 0
        Top = 56
        Width = 472
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = #1053#1077#1090
        Items.Strings = (
          #1053#1077#1090)
        ExplicitTop = 47
        ExplicitWidth = 370
      end
    end
    object Panel3: TPanel
      Left = 477
      Top = 3
      Width = 72
      Height = 91
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 376
      ExplicitHeight = 74
      object lblZoom: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 72
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
      end
      object cbbZoom: TComboBox
        Left = 0
        Top = 16
        Width = 72
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbZoomChange
        ExplicitTop = 13
      end
    end
  end
  object pnlCenter: TPanel
    Left = 0
    Top = 173
    Width = 228
    Height = 128
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 5
    ExplicitLeft = -3
    ExplicitTop = 158
    ExplicitHeight = 144
    object chkUseMapMarks: TCheckBox
      Left = 3
      Top = 20
      Width = 222
      Height = 17
      Align = alTop
      Caption = #1053#1072#1082#1083#1072#1076#1099#1074#1072#1090#1100' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1077' '#1084#1077#1090#1082#1080
      TabOrder = 0
      ExplicitWidth = 118
    end
    object chkUseRecolor: TCheckBox
      Left = 3
      Top = 3
      Width = 222
      Height = 17
      Align = alTop
      Caption = #1055#1088#1080#1084#1077#1085#1103#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1094#1074#1077#1090#1072
      TabOrder = 1
      ExplicitWidth = 118
    end
    object flwpnlJpegQuality: TFlowPanel
      Left = 3
      Top = 37
      Width = 222
      Height = 24
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      Padding.Top = 2
      TabOrder = 2
      ExplicitWidth = 219
      object lblJpgQulity: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 5
        Width = 144
        Height = 13
        Alignment = taRightJustify
        Caption = #1050#1072#1095#1077#1089#1090#1074#1086' ('#1076#1083#1103' JPEG '#1080' ECW):'
        Layout = tlCenter
      end
      object seJpgQuality: TSpinEdit
        Left = 150
        Top = 2
        Width = 41
        Height = 22
        MaxValue = 100
        MinValue = 1
        TabOrder = 0
        Value = 95
      end
    end
  end
  object Panel4: TPanel
    Left = 228
    Top = 173
    Width = 156
    Height = 128
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 6
    ExplicitLeft = 225
    ExplicitTop = 179
    ExplicitHeight = 123
    object lblPrTypes: TLabel
      Left = 3
      Top = 3
      Width = 150
      Height = 15
      Align = alTop
      AutoSize = False
      Caption = #1057#1086#1079#1076#1072#1074#1072#1090#1100' '#1092#1072#1081#1083' '#1087#1088#1080#1074#1103#1079#1082#1080':'
      WordWrap = True
      ExplicitTop = 6
    end
    object chklstPrTypes: TCheckListBox
      Left = 3
      Top = 18
      Width = 150
      Height = 47
      Align = alTop
      ItemHeight = 13
      Items.Strings = (
        '.map'
        '.tab'
        '.w'
        '.dat'
        '.kml')
      TabOrder = 0
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Left = 48
    Top = 232
  end
end
