object Fsaveas: TFsaveas
  Left = 234
  Top = 298
  BorderStyle = bsDialog
  Caption = #1054#1087#1077#1088#1072#1094#1080#1103' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
  ClientHeight = 320
  ClientWidth = 680
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 680
    Height = 283
    ActivePage = TabSheet5
    Align = alClient
    TabOrder = 0
    TabWidth = 83
    object TabSheet1: TTabSheet
      Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100
    end
    object TabSheet2: TTabSheet
      Tag = 1
      Caption = #1057#1082#1083#1077#1080#1090#1100
      ImageIndex = 1
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
        Width = 672
        Height = 33
        Align = alTop
        Shape = bsBottomLine
        ExplicitWidth = 493
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
        Width = 672
        Height = 222
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
      end
    end
    object TabSheet6: TTabSheet
      Tag = 5
      Caption = #1057#1082#1086#1087#1080#1088#1086#1074#1072#1090#1100
      ImageIndex = 5
    end
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 283
    Width = 680
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object SpeedButton1: TSpeedButton
      AlignWithMargins = True
      Left = 487
      Top = 6
      Width = 25
      Height = 25
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1080#1077' '#1074' '#1086#1090#1076#1077#1083#1100#1085#1099#1081' '#1092#1072#1081#1083
      Align = alRight
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
      ExplicitLeft = 319
      ExplicitTop = 220
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 599
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Caption = #1053#1072#1095#1072#1090#1100
      Default = True
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 518
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1072
      TabOrder = 1
      OnClick = Button3Click
    end
    object CBCloseWithStart: TCheckBox
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 475
      Height = 25
      Align = alClient
      Caption = #1047#1072#1082#1088#1099#1090#1100' '#1086#1082#1085#1086' '#1087#1086#1089#1083#1077' '#1089#1090#1072#1088#1090#1072
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object SaveSelDialog: TSaveDialog
    DefaultExt = '*.hlg'
    Filter = #1060#1072#1081#1083' '#1074#1099#1076#1077#1083#1077#1085#1080#1103'|*.hlg'
    Left = 32
    Top = 48
  end
end
