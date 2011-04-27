object frmRegionProcess: TfrmRegionProcess
  Left = 234
  Top = 298
  Caption = #1054#1087#1077#1088#1072#1094#1080#1103' '#1089' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1081' '#1086#1073#1083#1072#1089#1090#1100#1102
  ClientHeight = 316
  ClientWidth = 572
  Color = clBtnFace
  Constraints.MinHeight = 343
  Constraints.MinWidth = 580
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 572
    Height = 279
    ActivePage = TabSheet5
    Align = alClient
    TabOrder = 0
    TabWidth = 92
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
        Width = 564
        Height = 31
        Align = alTop
        Shape = bsBottomLine
        ExplicitWidth = 565
      end
      object Label9: TLabel
        Left = 5
        Top = 8
        Width = 207
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
        Top = 31
        Width = 564
        Height = 220
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
    Top = 279
    Width = 572
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object SpeedButton1: TSpeedButton
      AlignWithMargins = True
      Left = 379
      Top = 6
      Width = 25
      Height = 25
      Hint = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1074#1099#1076#1077#1083#1077#1085#1080#1077' '#1074' '#1086#1090#1076#1077#1083#1100#1085#1099#1081' '#1092#1072#1081#1083
      Align = alRight
      Flat = True
      Glyph.Data = {
        46030000424D460300000000000036000000280000000E0000000E0000000100
        20000000000010030000000000000000000000000000000000008D6637818D66
        37FF8D6637FFD8D8B7FFD8D8B7FFD8D8B7FFD8D8B7FFD8D8B7FFD8D8B7FFD8D8
        B7FF8D6637FF8D6637FF8D6637FF8D6637FF8E6738FFB78448FFA67841FFECEC
        D0FFA67841FFA67841FFECECD0FFECECD0FFECECD0FFECECD0FFA67841FFA678
        41FFC2945BFF8E6738FF906839FFBD894AFFA67841FFEFEFD9FFBD894AFFA678
        41FFEFEFD9FFEFEFD9FFEFEFD9FFEFEFD9FFA67841FFA67841FFC2945BFF9068
        39FF926A39FFBF8A4BFFA67841FFF1F1E0FFBF8A4BFFA67841FFF1F1E0FFF1F1
        E0FFF1F1E0FFF1F1E0FFA67841FFA67841FFC2945BFF926A39FF946B3AFFC18C
        4CFFA67841FFF3F3E5FFF3F3E5FFF3F3E5FFF3F3E5FFF3F3E5FFF3F3E5FFF3F3
        E5FFA67841FFA67841FFC2945BFF946B3AFF976D3BFFC48E4DFFC48E4DFFA678
        41FFA67841FFA67841FFA67841FFA67841FFA67841FFC2945BFFC2945BFFC294
        5BFFC48E4DFF976D3BFF996F3CFFC7904EFFC7904EFFC7904EFFC7904EFFC790
        4EFFC7904EFFC7904EFFC7904EFFC7904EFFC7904EFFC7904EFFC7904EFF996F
        3CFF9C713DFFC9924FFFFEEBDEFFFEEBDEFFFEEBDEFFFEEBDEFFFEEBDEFFFEEB
        DEFFFEEBDEFFFEEBDEFFFEEBDEFFFEEBDEFFC9924FFF9C713DFF9E733EFFCC94
        51FFC0E4E8FFC0E4E8FFC0E4E8FFC0E4E8FFC0E4E8FFC0E4E8FFC0E4E8FFC0E4
        E8FFC0E4E8FFC0E4E8FFCC9451FF9E733EFFA17540FFD09652FFFFEDE2FFFFED
        E2FFFFEDE2FFFFEDE2FFFFEDE2FFFFEDE2FFFFEDE2FFFFEDE2FFFFEDE2FFFFED
        E2FFD09652FFA17540FFA67942FFD69B55FFBEE5E9FFBEE5E9FFBEE5E9FFBEE5
        E9FFBEE5E9FFBEE5E9FFBEE5E9FFBEE5E9FFBEE5E9FFBEE5E9FFD69B55FFA679
        42FFAC7D44FFDCA057FFFFF0E6FFFFF0E6FFFFF0E6FFFFF0E6FFFFF0E6FFFFF0
        E6FFFFF0E6FFFFF0E6FFFFF0E6FFFFF0E6FFDCA057FFAC7D44FFB18146FFE1A3
        59FF07C8F8FF07C8F8FF07C8F8FF07C8F8FF07C8F8FF07C8F8FF07C8F8FF07C8
        F8FF07C8F8FF07C8F8FFE1A359FFB18146FFB68448FFB68448FF0FA5ECFF0FA5
        ECFF0FA5ECFF0FA5ECFF0FA5ECFF0FA5ECFF0FA5ECFF0FA5ECFF0FA5ECFF0FA5
        ECFFB68448FFB68448FF}
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
      Left = 491
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
      Left = 410
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
      Width = 367
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
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 32
    Top = 48
  end
end
