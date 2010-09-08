object frTilesCopy: TfrTilesCopy
  Left = 0
  Top = 0
  Width = 678
  Height = 248
  Align = alClient
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 678
    Height = 221
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = -37
    ExplicitWidth = 451
    ExplicitHeight = 277
    object pnlRight: TPanel
      Left = 603
      Top = 0
      Width = 75
      Height = 221
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitLeft = 376
      ExplicitHeight = 277
      object lblZooms: TLabel
        Left = 3
        Top = 3
        Width = 69
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
        ExplicitWidth = 57
      end
      object chkAllZooms: TCheckBox
        Left = 3
        Top = 201
        Width = 69
        Height = 17
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 0
        OnClick = chkAllZoomsClick
        ExplicitTop = 257
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 16
        Width = 69
        Height = 185
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        ExplicitHeight = 241
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 603
      Height = 221
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitWidth = 376
      ExplicitHeight = 277
      object lblNamesType: TLabel
        Left = 3
        Top = 3
        Width = 597
        Height = 13
        Align = alTop
        Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1091#1095#1072#1089#1090#1082#1072' '#1074' '#1092#1086#1088#1084#1072#1090
        ExplicitTop = 37
        ExplicitWidth = 207
      end
      object cbbNamesType: TComboBox
        Left = 3
        Top = 16
        Width = 597
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
        ExplicitTop = 50
        ExplicitWidth = 370
      end
      object chkDeleteSource: TCheckBox
        Left = 3
        Top = 37
        Width = 597
        Height = 17
        Align = alTop
        Caption = #1055#1077#1088#1077#1084#1077#1097#1072#1090#1100
        TabOrder = 1
        ExplicitLeft = 272
        ExplicitTop = 40
        ExplicitWidth = 97
      end
      object chkReplaseTarget: TCheckBox
        Left = 3
        Top = 54
        Width = 597
        Height = 17
        Align = alTop
        Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1087#1088#1080' '#1089#1086#1074#1087#1072#1076#1077#1085#1080#1080
        TabOrder = 2
        ExplicitLeft = 272
        ExplicitTop = 56
        ExplicitWidth = 153
      end
      object chkAllMaps: TCheckBox
        Left = 3
        Top = 201
        Width = 597
        Height = 17
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 3
        OnClick = chkAllMapsClick
        ExplicitTop = 174
        ExplicitWidth = 41
      end
      object chklstMaps: TCheckListBox
        Left = 3
        Top = 71
        Width = 597
        Height = 130
        Align = alClient
        ItemHeight = 13
        TabOrder = 4
        ExplicitTop = 77
        ExplicitWidth = 196
        ExplicitHeight = 97
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 678
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitWidth = 451
    object lblTargetPath: TLabel
      Left = 3
      Top = 3
      Width = 86
      Height = 21
      Align = alLeft
      AutoSize = False
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitLeft = 8
      ExplicitTop = 6
      ExplicitHeight = 13
    end
    object edtTargetPath: TEdit
      Left = 89
      Top = 3
      Width = 565
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 338
    end
    object btnSelectTargetPath: TButton
      Left = 654
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetPathClick
      ExplicitLeft = 427
    end
  end
end
