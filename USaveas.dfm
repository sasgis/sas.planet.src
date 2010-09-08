object Fsaveas: TFsaveas
  Left = 234
  Top = 298
  BorderStyle = bsDialog
  Caption = #1054#1087#1077#1088#1072#1094#1080#1103' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
  ClientHeight = 252
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  DesignSize = (
    512
    252)
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 319
    Top = 220
    Width = 25
    Height = 25
    Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1080#1077' '#1074' '#1086#1090#1076#1077#1083#1100#1085#1099#1081' '#1092#1072#1081#1083
    Anchors = [akRight, akBottom]
    Flat = True
    Glyph.Data = {
      9E020000424D9E0200000000000036000000280000000E0000000E0000000100
      1800000000006802000000000000000000000000000000000000F0B8B0C06860
      B05850A05050A05050A050509048509048409048408040408038408038407038
      407038300000D06870F09090E08080B04820403020C0B8B0C0B8B0D0C0C0D0C8
      C0505050A04030A04030A038307038400000D07070FF98A0F08880E080807058
      50404030907870F0E0E0F0E8E0908070A04030A04040A040308038400000D078
      70FFA0A0F09090F08880705850000000404030F0D8D0F0E0D0807860B04840B0
      4840A040408040400000D07880FFA8B0FFA0A0F0909070585070585070585070
      5850706050806860C05850B05050B048408040400000E08080FFB0B0FFB0B0FF
      A0A0F09090F08880E08080E07880D07070D06870C06060C05850B05050904840
      0000E08890FFB8C0FFB8B0D06060C06050C05850C05040B05030B04830A04020
      A03810C06060C058509048400000E09090FFC0C0D06860FFFFFFFFFFFFFFF8F0
      F0F0F0F0E8E0F0D8D0E0D0C0E0C8C0A03810C060609048500000E098A0FFC0C0
      D07070FFFFFFFFFFFFFFFFFFFFF8F0F0F0F0F0E8E0F0D8D0E0D0C0A04020D068
      60A050500000F0A0A0FFC0C0E07870FFFFFFFFFFFFFFFFFFFFFFFFFFF8F0F0F0
      F0F0E8E0F0D8D0B04830D07070A050500000F0A8A0FFC0C0E08080FFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFF8F0F0F0F0F0E8E0B05030E07880A050500000F0B0
      B0FFC0C0F08890FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8F0F0F0F0C0
      5040603030B058500000F0B0B0FFC0C0FF9090FFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFF8F0C05850B05860B058600000F0B8B0F0B8B0F0B0B0F0
      B0B0F0A8B0F0A0A0E098A0E09090E09090E08890E08080D07880D07870D07070
      0000}
    Layout = blGlyphTop
    Margin = 5
    ParentShowHint = False
    ShowHint = True
    OnClick = SpeedButton1Click
  end
  object Button1: TButton
    Left = 430
    Top = 220
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = #1053#1072#1095#1072#1090#1100
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 501
    Height = 206
    ActivePage = TabSheet3
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    TabWidth = 83
    object TabSheet1: TTabSheet
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
      OnShow = TabSheet1Show
      object Bevel1: TBevel
        Left = 0
        Top = 0
        Width = 493
        Height = 17
        Align = alTop
        Shape = bsBottomLine
      end
      object Label22: TLabel
        Left = 5
        Top = 26
        Width = 56
        Height = 13
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
      end
      object Label3: TLabel
        Left = 372
        Top = 26
        Width = 49
        Height = 13
        Alignment = taRightJustify
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
      end
      object Label6: TLabel
        Left = 8
        Top = 52
        Width = 6
        Height = 13
        Caption = '_'
      end
      object Label4: TLabel
        Left = 5
        Top = 0
        Width = 193
        Height = 13
        Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1080#1079' '#1080#1085#1090#1077#1088#1085#1077#1090#1072
      end
      object Bevel6: TBevel
        Left = 208
        Top = 88
        Width = 9
        Height = 81
        Shape = bsLeftLine
      end
      object CheckBox2: TCheckBox
        Left = 216
        Top = 106
        Width = 153
        Height = 17
        Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1089#1090#1072#1088#1099#1077' '#1092#1072#1081#1083#1099
        TabOrder = 0
        OnClick = CheckBox2Click
      end
      object CheckBox7: TCheckBox
        Left = 232
        Top = 128
        Width = 145
        Height = 17
        Caption = #1090#1086#1083#1100#1082#1086' '#1087#1088#1080' '#1080#1093' '#1088#1072#1079#1083#1080#1095#1080#1080
        Enabled = False
        TabOrder = 1
      end
      object CBDateDo: TCheckBox
        Left = 232
        Top = 145
        Width = 129
        Height = 17
        Caption = #1090#1086#1083#1100#1082#1086' '#1089#1086#1079#1076#1072#1085#1085#1099#1077' '#1076#1086
        Enabled = False
        TabOrder = 2
        OnClick = CBDateDoClick
      end
      object DateDo: TDateTimePicker
        Left = 368
        Top = 144
        Width = 81
        Height = 21
        Date = 39513.436381111110000000
        Time = 39513.436381111110000000
        Enabled = False
        TabOrder = 3
      end
      object CBMapLoad: TComboBox
        Left = 88
        Top = 24
        Width = 241
        Height = 21
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 4
      end
      object CBZoomload: TComboBox
        Left = 424
        Top = 24
        Width = 41
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 5
        OnChange = CBZoomloadChange
      end
      object CBSecondLoadTNE: TCheckBox
        Left = 216
        Top = 90
        Width = 249
        Height = 17
        Caption = #1055#1099#1090#1072#1090#1100#1089#1103' '#1079#1072#1075#1088#1091#1078#1072#1090#1100' '#1086#1090#1089#1091#1090#1089#1090#1074#1091#1102#1097#1080#1077' '#1090#1072#1081#1083#1099
        TabOrder = 6
        OnClick = CheckBox2Click
      end
    end
    object TabSheet2: TTabSheet
      Tag = 1
      Caption = #1057#1082#1083#1077#1080#1090#1100
      ImageIndex = 1
      OnShow = TabSheet2Show
      object Label25: TLabel
        Left = 5
        Top = 26
        Width = 56
        Height = 13
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
      end
      object Label26: TLabel
        Left = 8
        Top = 115
        Width = 144
        Height = 13
        Caption = #1050#1072#1095#1077#1089#1090#1074#1086' ('#1076#1083#1103' JPEG '#1080' ECW):'
      end
      object Bevel2: TBevel
        Left = 0
        Top = 0
        Width = 493
        Height = 17
        Align = alTop
        Shape = bsBottomLine
      end
      object Label8: TLabel
        Left = 5
        Top = 0
        Width = 162
        Height = 13
        Caption = #1057#1082#1083#1077#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1085#1099#1081' '#1092#1088#1072#1075#1084#1077#1085#1090
      end
      object Label27: TLabel
        Left = 8
        Top = 137
        Width = 137
        Height = 13
        Caption = #1057#1086#1079#1076#1072#1074#1072#1090#1100' '#1092#1072#1081#1083' '#1087#1088#1080#1074#1103#1079#1082#1080':'
      end
      object Label28: TLabel
        Left = 198
        Top = 26
        Width = 54
        Height = 13
        Alignment = taRightJustify
        Caption = #1053#1072#1083#1086#1078#1080#1090#1100':'
      end
      object CBscleit: TComboBox
        Left = 64
        Top = 24
        Width = 121
        Height = 21
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
      end
      object QualitiEdit: TSpinEdit
        Left = 160
        Top = 111
        Width = 73
        Height = 22
        MaxValue = 100
        MinValue = 1
        TabOrder = 1
        Value = 95
      end
      object GroupBox1: TGroupBox
        Left = 304
        Top = 96
        Width = 161
        Height = 73
        Caption = ' '#1056#1072#1079#1073#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077' '
        TabOrder = 2
        object Label19: TLabel
          Left = 8
          Top = 22
          Width = 91
          Height = 13
          Caption = #1087#1086' '#1075#1086#1088#1080#1079#1086#1090#1072#1083#1080', '#1085#1072
        end
        object Label20: TLabel
          Left = 8
          Top = 46
          Width = 86
          Height = 13
          Caption = #1087#1086' '#1074#1077#1088#1090#1080#1082#1072#1083#1080', '#1085#1072
        end
        object EditNTg: TSpinEdit
          Left = 104
          Top = 19
          Width = 49
          Height = 22
          MaxValue = 1000
          MinValue = 1
          TabOrder = 0
          Value = 1
        end
        object EditNTv: TSpinEdit
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
      object CBSclHib: TComboBox
        Left = 256
        Top = 24
        Width = 113
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 3
        Text = #1053#1077#1090
        Items.Strings = (
          #1053#1077#1090)
      end
      object CBusedReColor: TCheckBox
        Left = 8
        Top = 72
        Width = 289
        Height = 17
        Caption = #1055#1088#1080#1084#1077#1085#1103#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080' '#1094#1074#1077#1090#1072
        TabOrder = 4
      end
      object PrTypesBox: TCheckListBox
        Left = 160
        Top = 136
        Width = 73
        Height = 33
        ItemHeight = 13
        Items.Strings = (
          '.map'
          '.tab'
          '.w'
          '.dat'
          '.kml')
        TabOrder = 5
      end
      object CBUsedMarks: TCheckBox
        Left = 8
        Top = 88
        Width = 289
        Height = 17
        Caption = #1053#1072#1082#1083#1072#1076#1099#1074#1072#1090#1100' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1077' '#1084#1077#1090#1082#1080
        TabOrder = 6
      end
    end
    object TabSheet3: TTabSheet
      Tag = 2
      Caption = #1057#1092#1086#1088#1084#1080#1088#1086#1074#1072#1090#1100
      ImageIndex = 2
    end
    object TabSheet4: TTabSheet
      Tag = 3
      Caption = #1059#1076#1072#1083#1080#1090#1100
      ImageIndex = 3
    end
    object TabSheet5: TTabSheet
      Tag = 4
      Caption = #1069#1082#1089#1087#1086#1088#1090
      ImageIndex = 4
      object Bevel5: TBevel
        Left = 0
        Top = 0
        Width = 493
        Height = 33
        Align = alTop
        Shape = bsBottomLine
      end
      object Label9: TLabel
        Left = 5
        Top = 8
        Width = 205
        Height = 13
        Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1091#1095#1072#1089#1090#1082#1072' '#1074' '#1092#1086#1088#1084#1072#1090
      end
      object CBFormat: TComboBox
        Left = 220
        Top = 4
        Width = 249
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = CBFormatChange
      end
      object pnlExport: TPanel
        Left = 0
        Top = 33
        Width = 493
        Height = 145
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
    object TabSheet6: TTabSheet
      Tag = 5
      Caption = #1057#1082#1086#1087#1080#1088#1086#1074#1072#1090#1100
      ImageIndex = 5
      object Label32: TLabel
        Left = 5
        Top = 8
        Width = 205
        Height = 13
        Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1091#1095#1072#1089#1090#1082#1072' '#1074' '#1092#1086#1088#1084#1072#1090
      end
      object Bevel7: TBevel
        Left = 0
        Top = 0
        Width = 493
        Height = 33
        Align = alTop
        Shape = bsBottomLine
      end
      object CBCahceType: TComboBox
        Left = 220
        Top = 4
        Width = 249
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 0
        Text = 'SAS.'#1055#1083#1072#1085#1077#1090#1072
        Items.Strings = (
          'GoogleMV'
          'SAS.'#1055#1083#1072#1085#1077#1090#1072
          'ES1.95'
          'GMT (GlobalMapper >=10.02)')
      end
      object Panel1: TPanel
        Left = 0
        Top = 36
        Width = 473
        Height = 133
        BevelOuter = bvNone
        TabOrder = 1
        object Label11: TLabel
          Left = 5
          Top = 2
          Width = 167
          Height = 13
          Caption = #1042#1082#1083#1102#1095#1080#1090#1100' '#1089#1083#1077#1076#1091#1102#1097#1080#1077' '#1090#1080#1087#1099' '#1082#1072#1088#1090':'
        end
        object Label13: TLabel
          Left = 272
          Top = 2
          Width = 82
          Height = 13
          Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
        end
        object Label12: TLabel
          Left = 208
          Top = 2
          Width = 57
          Height = 13
          Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
        end
        object CheckListBox1: TCheckListBox
          Left = 5
          Top = 16
          Width = 196
          Height = 97
          ItemHeight = 13
          TabOrder = 0
        end
        object CBReplace: TCheckBox
          Left = 272
          Top = 56
          Width = 153
          Height = 17
          Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1087#1088#1080' '#1089#1086#1074#1087#1072#1076#1077#1085#1080#1080
          TabOrder = 1
        end
        object Button2: TButton
          Left = 448
          Top = 16
          Width = 21
          Height = 21
          Caption = '...'
          TabOrder = 2
          OnClick = Button2Click
        end
        object CBMove: TCheckBox
          Left = 272
          Top = 40
          Width = 97
          Height = 17
          Caption = #1055#1077#1088#1077#1084#1077#1097#1072#1090#1100
          TabOrder = 3
        end
        object EditPath: TEdit
          Left = 272
          Top = 16
          Width = 177
          Height = 21
          TabOrder = 4
        end
        object CheckBox4: TCheckBox
          Left = 208
          Top = 113
          Width = 41
          Height = 17
          Caption = #1042#1089#1077
          TabOrder = 5
          OnClick = CheckBox4Click
        end
        object CheckBox3: TCheckBox
          Left = 5
          Top = 113
          Width = 41
          Height = 17
          Caption = #1042#1089#1077
          TabOrder = 6
          OnClick = CheckBox3Click
        end
        object CheckListBox2: TCheckListBox
          Left = 208
          Top = 16
          Width = 57
          Height = 97
          ItemHeight = 13
          TabOrder = 7
        end
      end
    end
  end
  object Button3: TButton
    Left = 351
    Top = 220
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 2
    OnClick = Button3Click
  end
  object CBCloseWithStart: TCheckBox
    Left = 8
    Top = 223
    Width = 169
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086' '#1087#1086#1089#1083#1077' '#1089#1090#1072#1088#1090#1072
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object SaveSelDialog: TSaveDialog
    DefaultExt = '*.hlg'
    Filter = #1060#1072#1081#1083' '#1074#1099#1076#1077#1083#1077#1085#1080#1103'|*.hlg'
    Left = 240
    Top = 208
  end
end
