object frExportYaMaps: TfrExportYaMaps
  Left = 0
  Top = 0
  Width = 423
  Height = 127
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 41
    Width = 423
    Height = 86
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    ExplicitLeft = -3
    ExplicitTop = 57
    ExplicitWidth = 777
    ExplicitHeight = 318
    object pnlRight: TPanel
      Left = 243
      Top = 0
      Width = 180
      Height = 86
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 472
      ExplicitHeight = 179
      DesignSize = (
        180
        86)
      object lblCompress: TLabel
        Left = 9
        Top = 1
        Width = 43
        Height = 13
        Caption = #1057#1078#1072#1090#1080#1077':'
      end
      object lblSatCompress: TLabel
        Left = 62
        Top = 42
        Width = 55
        Height = 13
        Caption = '100..1 max'
      end
      object lblMapCompress: TLabel
        Left = 62
        Top = 18
        Width = 43
        Height = 13
        Caption = '0..9 max'
      end
      object lblZooms: TLabel
        Left = 121
        Top = 1
        Width = 57
        Height = 13
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
      end
      object seMapCompr: TSpinEdit
        Left = 9
        Top = 15
        Width = 49
        Height = 22
        MaxValue = 9
        MinValue = 0
        TabOrder = 0
        Value = 2
      end
      object seSatCompr: TSpinEdit
        Left = 9
        Top = 39
        Width = 49
        Height = 22
        MaxValue = 100
        MinValue = 1
        TabOrder = 1
        Value = 85
      end
      object chklstZooms: TCheckListBox
        Left = 121
        Top = 20
        Width = 55
        Height = 62
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 2
        ExplicitHeight = 73
      end
    end
    object pnlMapsSelect: TPanel
      Left = 0
      Top = 0
      Width = 243
      Height = 86
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 100
      ExplicitTop = 5
      ExplicitWidth = 411
      ExplicitHeight = 289
      DesignSize = (
        243
        86)
      object lblMapsSelect: TLabel
        Left = 5
        Top = -4
        Width = 153
        Height = 13
        Caption = #1042#1082#1083#1102#1095#1080#1090#1100' '#1089#1083#1077#1076#1091#1102#1097#1080#1077' '#1082#1072#1088#1090#1099':'
      end
      object lblMap: TLabel
        Left = 20
        Top = 13
        Width = 31
        Height = 13
        Alignment = taRightJustify
        Caption = #1050#1072#1088#1090#1072
      end
      object lblSat: TLabel
        Left = 9
        Top = 37
        Width = 43
        Height = 13
        Alignment = taRightJustify
        Caption = #1057#1087#1091#1090#1085#1080#1082
      end
      object lblHybr: TLabel
        Left = 2
        Top = 61
        Width = 51
        Height = 13
        Alignment = taRightJustify
        Caption = #1053#1072#1083#1086#1078#1080#1090#1100
      end
      object cbbMap: TComboBox
        Left = 56
        Top = 10
        Width = 181
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 349
      end
      object cbbSat: TComboBox
        Left = 56
        Top = 34
        Width = 181
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 1
        ExplicitWidth = 349
      end
      object cbbHybr: TComboBox
        Left = 56
        Top = 58
        Width = 181
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 2
        ExplicitWidth = 349
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 423
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 192
    ExplicitTop = 40
    ExplicitWidth = 185
    DesignSize = (
      423
      41)
    object lblTargetPath: TLabel
      Left = 8
      Top = 5
      Width = 86
      Height = 13
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
    end
    object edtTargetPath: TEdit
      Left = 100
      Top = 2
      Width = 294
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 523
    end
    object btnSelectTargetPath: TButton
      Left = 400
      Top = 2
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetPathClick
      ExplicitLeft = 754
    end
    object chkReplaseTiles: TCheckBox
      Left = 96
      Top = 23
      Width = 185
      Height = 17
      Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1080#1077' '#1090#1072#1081#1083#1099
      TabOrder = 2
    end
  end
end
