object frMapCombine: TfrMapCombine
  Left = 0
  Top = 0
  Width = 476
  Height = 226
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 476
    Height = 20
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
    Top = 42
    Width = 476
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitTop = 48
    ExplicitWidth = 576
    object lblTargetFile: TLabel
      Left = 3
      Top = 3
      Width = 86
      Height = 19
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 89
      Top = 3
      Width = 363
      Height = 19
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 463
      ExplicitHeight = 21
    end
    object btnSelectTargetFile: TButton
      Left = 452
      Top = 3
      Width = 21
      Height = 19
      Align = alRight
      Caption = '...'
      TabOrder = 1
      ExplicitLeft = 552
      ExplicitHeight = 21
    end
  end
  object pnlOutputFormat: TPanel
    Left = 0
    Top = 20
    Width = 476
    Height = 22
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    ExplicitTop = 18
    object lblOutputFormat: TLabel
      Left = 3
      Top = 3
      Width = 130
      Height = 16
      Align = alLeft
      Caption = #1056#1077#1079#1091#1083#1100#1090#1080#1088#1091#1102#1097#1080#1081' '#1092#1086#1088#1084#1072#1090
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object cbbOutputFormat: TComboBox
      Left = 133
      Top = 3
      Width = 340
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
        'JPEG (Joint Photographic Experts Group)')
      ExplicitWidth = 440
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 67
    Width = 476
    Height = 159
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 79
    ExplicitWidth = 576
    ExplicitHeight = 177
    object pnlRight: TPanel
      Left = 311
      Top = 0
      Width = 165
      Height = 159
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitLeft = 411
      ExplicitHeight = 177
      object lblPrTypes: TLabel
        Left = 3
        Top = 100
        Width = 159
        Height = 13
        Align = alTop
        Caption = #1057#1086#1079#1076#1072#1074#1072#1090#1100' '#1092#1072#1081#1083' '#1087#1088#1080#1074#1103#1079#1082#1080':'
        WordWrap = True
        ExplicitTop = 112
        ExplicitWidth = 138
      end
      object lblZoom: TLabel
        Left = 3
        Top = 3
        Width = 159
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
        ExplicitWidth = 49
      end
      object grpSplit: TGroupBox
        Left = 3
        Top = 37
        Width = 159
        Height = 63
        Align = alTop
        Caption = ' '#1056#1072#1079#1073#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077' '
        TabOrder = 0
        object lblSplitHor: TLabel
          Left = 7
          Top = 16
          Width = 92
          Height = 13
          Caption = #1087#1086' '#1075#1086#1088#1080#1079#1086#1090#1072#1083#1080', '#1085#1072
        end
        object lblSplitVert: TLabel
          Left = 8
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
      object chklstPrTypes: TCheckListBox
        Left = 3
        Top = 113
        Width = 159
        Height = 43
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          '.map'
          '.tab'
          '.w'
          '.dat'
          '.kml')
        TabOrder = 1
        ExplicitTop = 125
        ExplicitHeight = 49
      end
      object cbbZoom: TComboBox
        Left = 3
        Top = 16
        Width = 159
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 311
      Height = 159
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitWidth = 411
      ExplicitHeight = 177
      object lblMap: TLabel
        Left = 3
        Top = 3
        Width = 305
        Height = 13
        Align = alTop
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
        ExplicitWidth = 57
      end
      object lblHybr: TLabel
        Left = 3
        Top = 37
        Width = 305
        Height = 13
        Align = alTop
        Caption = #1053#1072#1083#1086#1078#1080#1090#1100':'
        ExplicitWidth = 55
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 16
        Width = 305
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 405
      end
      object cbbHybr: TComboBox
        Left = 3
        Top = 50
        Width = 305
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = #1053#1077#1090
        Items.Strings = (
          #1053#1077#1090)
        ExplicitWidth = 405
      end
      object chkUseMapMarks: TCheckBox
        Left = 3
        Top = 88
        Width = 305
        Height = 17
        Align = alTop
        Caption = #1053#1072#1082#1083#1072#1076#1099#1074#1072#1090#1100' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1077' '#1084#1077#1090#1082#1080
        TabOrder = 2
        ExplicitWidth = 405
      end
      object chkUseRecolor: TCheckBox
        Left = 3
        Top = 71
        Width = 305
        Height = 17
        Align = alTop
        Caption = #1055#1088#1080#1084#1077#1085#1103#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1094#1074#1077#1090#1072
        TabOrder = 3
        ExplicitWidth = 405
      end
      object pnlJpegQuality: TPanel
        Left = 3
        Top = 105
        Width = 305
        Height = 32
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 2
        TabOrder = 4
        ExplicitWidth = 405
        object lblJpgQulity: TLabel
          Left = 2
          Top = 2
          Width = 144
          Height = 28
          Align = alLeft
          Caption = #1050#1072#1095#1077#1089#1090#1074#1086' ('#1076#1083#1103' JPEG '#1080' ECW):'
          ExplicitHeight = 13
        end
        object pnlJpegQualityValue: TPanel
          Left = 146
          Top = 2
          Width = 87
          Height = 28
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object seJpgQuality: TSpinEdit
            Left = 6
            Top = 0
            Width = 73
            Height = 22
            MaxValue = 100
            MinValue = 1
            TabOrder = 0
            Value = 95
          end
        end
      end
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Left = 48
    Top = 232
  end
end
