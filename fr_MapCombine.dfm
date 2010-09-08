object frMapCombine: TfrMapCombine
  Left = 0
  Top = 0
  Width = 576
  Height = 256
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 576
    Height = 27
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1057#1082#1083#1077#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1081' '#1092#1088#1072#1075#1084#1077#1085#1090
    TabOrder = 0
    ExplicitWidth = 637
  end
  object pnlTargetFile: TPanel
    Left = 0
    Top = 52
    Width = 576
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitWidth = 637
    object lblTargetFile: TLabel
      Left = 3
      Top = 3
      Width = 86
      Height = 21
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 89
      Top = 3
      Width = 463
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 524
    end
    object btnSelectTargetFile: TButton
      Left = 552
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      ExplicitLeft = 613
    end
  end
  object pnlOutputFormat: TPanel
    Left = 0
    Top = 27
    Width = 576
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    ExplicitWidth = 637
    object lblOutputFormat: TLabel
      Left = 3
      Top = 3
      Width = 130
      Height = 19
      Align = alLeft
      Caption = #1056#1077#1079#1091#1083#1100#1090#1080#1088#1091#1102#1097#1080#1081' '#1092#1086#1088#1084#1072#1090
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object cbbOutputFormat: TComboBox
      Left = 133
      Top = 3
      Width = 440
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
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 79
    Width = 576
    Height = 177
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitLeft = -24
    ExplicitTop = 85
    ExplicitWidth = 637
    ExplicitHeight = 376
    object pnlRight: TPanel
      Left = 411
      Top = 0
      Width = 165
      Height = 177
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitLeft = 472
      ExplicitHeight = 376
      object lblPrTypes: TLabel
        Left = 3
        Top = 112
        Width = 159
        Height = 18
        Align = alTop
        Caption = #1057#1086#1079#1076#1072#1074#1072#1090#1100' '#1092#1072#1081#1083' '#1087#1088#1080#1074#1103#1079#1082#1080':'
        WordWrap = True
        ExplicitLeft = 4
        ExplicitTop = 79
        ExplicitWidth = 157
      end
      object lblZoom: TLabel
        Left = 3
        Top = 3
        Width = 159
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
        ExplicitTop = -1
      end
      object grpSplit: TGroupBox
        Left = 3
        Top = 37
        Width = 159
        Height = 75
        Align = alTop
        Caption = ' '#1056#1072#1079#1073#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077' '
        TabOrder = 0
        ExplicitLeft = 4
        ExplicitTop = 4
        ExplicitWidth = 177
        object lblSplitHor: TLabel
          Left = 8
          Top = 22
          Width = 92
          Height = 13
          Caption = #1087#1086' '#1075#1086#1088#1080#1079#1086#1090#1072#1083#1080', '#1085#1072
        end
        object lblSplitVert: TLabel
          Left = 8
          Top = 46
          Width = 88
          Height = 13
          Caption = #1087#1086' '#1074#1077#1088#1090#1080#1082#1072#1083#1080', '#1085#1072
        end
        object seSplitHor: TSpinEdit
          Left = 104
          Top = 19
          Width = 49
          Height = 22
          MaxValue = 1000
          MinValue = 1
          TabOrder = 0
          Value = 1
        end
        object seSplitVert: TSpinEdit
          Left = 104
          Top = 43
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
        Top = 130
        Width = 159
        Height = 44
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          '.map'
          '.tab'
          '.w'
          '.dat'
          '.kml')
        TabOrder = 1
        ExplicitTop = 102
        ExplicitHeight = 204
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
        ExplicitTop = 10
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 411
      Height = 177
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitLeft = 8
      ExplicitTop = 46
      ExplicitWidth = 433
      ExplicitHeight = 307
      object lblMap: TLabel
        Left = 3
        Top = 3
        Width = 405
        Height = 13
        Align = alTop
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
        ExplicitLeft = 24
        ExplicitTop = 6
        ExplicitWidth = 57
      end
      object lblHybr: TLabel
        Left = 3
        Top = 37
        Width = 405
        Height = 13
        Align = alTop
        Caption = #1053#1072#1083#1086#1078#1080#1090#1100':'
        ExplicitLeft = 88
        ExplicitTop = 155
        ExplicitWidth = 55
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 16
        Width = 405
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitLeft = 88
        ExplicitTop = 128
        ExplicitWidth = 121
      end
      object cbbHybr: TComboBox
        Left = 3
        Top = 50
        Width = 405
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = #1053#1077#1090
        Items.Strings = (
          #1053#1077#1090)
        ExplicitLeft = 88
        ExplicitTop = 172
        ExplicitWidth = 113
      end
      object chkUseMapMarks: TCheckBox
        Left = 3
        Top = 88
        Width = 405
        Height = 17
        Align = alTop
        Caption = #1053#1072#1082#1083#1072#1076#1099#1074#1072#1090#1100' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1077' '#1084#1077#1090#1082#1080
        TabOrder = 2
        ExplicitLeft = 11
        ExplicitTop = 151
        ExplicitWidth = 289
      end
      object chkUseRecolor: TCheckBox
        Left = 3
        Top = 71
        Width = 405
        Height = 17
        Align = alTop
        Caption = #1055#1088#1080#1084#1077#1085#1103#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1094#1074#1077#1090#1072
        TabOrder = 3
        ExplicitLeft = 11
        ExplicitTop = 96
        ExplicitWidth = 289
      end
      object pnlJpegQuality: TPanel
        Left = 3
        Top = 105
        Width = 405
        Height = 32
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 2
        TabOrder = 4
        ExplicitWidth = 466
        object lblJpgQulity: TLabel
          Left = 2
          Top = 2
          Width = 144
          Height = 28
          Align = alLeft
          Caption = #1050#1072#1095#1077#1089#1090#1074#1086' ('#1076#1083#1103' JPEG '#1080' ECW):'
          ExplicitLeft = 16
          ExplicitTop = 22
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
          ExplicitHeight = 36
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
