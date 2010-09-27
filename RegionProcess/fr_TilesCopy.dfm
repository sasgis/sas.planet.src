object frTilesCopy: TfrTilesCopy
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 451
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlRight: TPanel
      Left = 376
      Top = 0
      Width = 75
      Height = 277
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblZooms: TLabel
        Left = 3
        Top = 3
        Width = 57
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
      end
      object chkAllZooms: TCheckBox
        Left = 3
        Top = 257
        Width = 69
        Height = 17
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 0
        OnClick = chkAllZoomsClick
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 16
        Width = 69
        Height = 241
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 277
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object lblNamesType: TLabel
        Left = 3
        Top = 3
        Width = 370
        Height = 13
        Align = alTop
        Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1091#1095#1072#1089#1090#1082#1072' '#1074' '#1092#1086#1088#1084#1072#1090
        ExplicitWidth = 207
      end
      object cbbNamesType: TComboBox
        Left = 3
        Top = 16
        Width = 370
        Height = 21
        Align = alTop
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
      object chkDeleteSource: TCheckBox
        Left = 3
        Top = 37
        Width = 370
        Height = 17
        Align = alTop
        Caption = #1055#1077#1088#1077#1084#1077#1097#1072#1090#1100
        TabOrder = 1
      end
      object chkReplaseTarget: TCheckBox
        Left = 3
        Top = 54
        Width = 370
        Height = 17
        Align = alTop
        Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1087#1088#1080' '#1089#1086#1074#1087#1072#1076#1077#1085#1080#1080
        TabOrder = 2
      end
      object chkAllMaps: TCheckBox
        Left = 3
        Top = 257
        Width = 370
        Height = 17
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 3
        OnClick = chkAllMapsClick
      end
      object chklstMaps: TCheckListBox
        Left = 3
        Top = 71
        Width = 370
        Height = 186
        Align = alClient
        ItemHeight = 13
        TabOrder = 4
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object lblTargetPath: TLabel
      Left = 3
      Top = 3
      Width = 86
      Height = 13
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
    end
    object edtTargetPath: TEdit
      Left = 89
      Top = 3
      Width = 338
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetPath: TButton
      Left = 427
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetPathClick
    end
  end
end
