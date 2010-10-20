object frTilesCopy: TfrTilesCopy
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 49
    Width = 451
    Height = 255
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 27
    ExplicitHeight = 277
    object pnlRight: TPanel
      Left = 376
      Top = 0
      Width = 75
      Height = 255
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitHeight = 277
      object lblZooms: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 69
        Height = 14
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
      end
      object chkAllZooms: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 235
        Width = 69
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 0
        OnClick = chkAllZoomsClick
        ExplicitTop = 257
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 20
        Width = 69
        Height = 212
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        ExplicitTop = 11
        ExplicitHeight = 240
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 255
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitHeight = 277
      object lblNamesType: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 370
        Height = 14
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = #1050#1086#1085#1074#1077#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1074' '#1092#1086#1088#1084#1072#1090
      end
      object cbbNamesType: TComboBox
        Left = 3
        Top = 20
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
        ExplicitTop = 16
      end
      object chkDeleteSource: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 44
        Width = 370
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = #1055#1077#1088#1077#1084#1077#1097#1072#1090#1100
        TabOrder = 1
        ExplicitTop = 37
      end
      object chkReplaseTarget: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 61
        Width = 370
        Height = 17
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1087#1088#1080' '#1089#1086#1074#1087#1072#1076#1077#1085#1080#1080
        TabOrder = 2
        ExplicitTop = 58
      end
      object chkAllMaps: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 235
        Width = 370
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 3
        OnClick = chkAllMapsClick
        ExplicitTop = 263
      end
      object chklstMaps: TCheckListBox
        Left = 3
        Top = 81
        Width = 370
        Height = 151
        Align = alClient
        ItemHeight = 13
        TabOrder = 4
        ExplicitTop = 78
        ExplicitHeight = 185
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 22
    Width = 451
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitTop = 0
    object lblTargetPath: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 86
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetPath: TEdit
      Left = 92
      Top = 3
      Width = 335
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 89
      ExplicitWidth = 338
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1082#1101#1096'  '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1091#1095#1072#1089#1090#1082#1072' '#1082#1072#1088#1090#1099
    TabOrder = 2
    ExplicitLeft = 3
    ExplicitTop = -5
  end
end
