object frExportIPhone: TfrExportIPhone
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      451
      32)
    object lblTargetPath: TLabel
      Left = 8
      Top = 6
      Width = 86
      Height = 13
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
    end
    object btnSelectTargetPath: TButton
      Left = 427
      Top = 3
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 0
      OnClick = btnSelectTargetPathClick
    end
    object edtTargetPath: TEdit
      Left = 100
      Top = 3
      Width = 324
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 32
    Width = 451
    Height = 272
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMaps: TPanel
      Left = 0
      Top = 0
      Width = 324
      Height = 272
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        324
        272)
      object lblMaps: TLabel
        Left = 5
        Top = 4
        Width = 174
        Height = 13
        Caption = #1042#1082#1083#1102#1095#1080#1090#1100' '#1089#1083#1077#1076#1091#1102#1097#1080#1077' '#1090#1080#1087#1099' '#1082#1072#1088#1090':'
      end
      object lblSat: TLabel
        Left = 5
        Top = 45
        Width = 43
        Height = 13
        Caption = #1057#1087#1091#1090#1085#1080#1082
      end
      object lblMap: TLabel
        Left = 5
        Top = 21
        Width = 31
        Height = 13
        Caption = #1050#1072#1088#1090#1072
      end
      object lblHybr: TLabel
        Left = 5
        Top = 69
        Width = 37
        Height = 13
        Caption = #1043#1080#1073#1088#1080#1076
      end
      object lblCompress: TLabel
        Left = 210
        Top = 4
        Width = 43
        Height = 13
        Anchors = [akTop, akRight]
        Caption = #1057#1078#1072#1090#1080#1077':'
        ExplicitLeft = 216
      end
      object lblSatCompress: TLabel
        Left = 263
        Top = 45
        Width = 55
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '100..1 max'
        ExplicitLeft = 269
      end
      object lblMapCompress: TLabel
        Left = 263
        Top = 21
        Width = 43
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '0..9 max'
        ExplicitLeft = 269
      end
      object lblHybrCompress: TLabel
        Left = 263
        Top = 69
        Width = 55
        Height = 13
        Anchors = [akTop, akRight]
        Caption = '100..1 max'
        ExplicitLeft = 269
      end
      object cbbSat: TComboBox
        Left = 48
        Top = 42
        Width = 139
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
      end
      object cbbMap: TComboBox
        Left = 48
        Top = 18
        Width = 139
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 1
      end
      object cbbHybr: TComboBox
        Left = 48
        Top = 66
        Width = 139
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 2
      end
      object rbSat: TRadioButton
        Left = 188
        Top = 44
        Width = 17
        Height = 17
        Hint = #1042#1099#1073#1088#1072#1090#1100' '#1089#1087#1091#1090#1085#1080#1082' '#1074' '#1091#1072#1095#1077#1089#1090#1074#1077' '#1086#1089#1085#1086#1074#1085#1086#1075#1086' '#1089#1083#1086#1103
        Anchors = [akTop, akRight]
        TabOrder = 3
      end
      object rbMap: TRadioButton
        Left = 188
        Top = 20
        Width = 17
        Height = 17
        Hint = #1042#1099#1073#1088#1072#1090#1100' '#1082#1072#1088#1090#1091' '#1074' '#1091#1072#1095#1077#1089#1090#1074#1077' '#1086#1089#1085#1086#1074#1085#1086#1075#1086' '#1089#1083#1086#1103
        Anchors = [akTop, akRight]
        Checked = True
        TabOrder = 4
        TabStop = True
      end
      object rbHybr: TRadioButton
        Left = 188
        Top = 68
        Width = 17
        Height = 17
        Hint = #1042#1099#1073#1088#1072#1090#1100' '#1075#1080#1073#1088#1080#1076' '#1074' '#1082#1072#1095#1077#1089#1090#1074#1077' '#1086#1089#1085#1086#1074#1085#1086#1075#1086' '#1089#1083#1086#1103
        Anchors = [akTop, akRight]
        TabOrder = 5
      end
      object seSatCompress: TSpinEdit
        Left = 210
        Top = 42
        Width = 49
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 100
        MinValue = 1
        TabOrder = 6
        Value = 85
      end
      object seMapCompress: TSpinEdit
        Left = 210
        Top = 18
        Width = 49
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 9
        MinValue = 0
        TabOrder = 7
        Value = 2
      end
      object chkAppendTilse: TCheckBox
        Left = 8
        Top = 94
        Width = 233
        Height = 17
        Caption = #1044#1086#1073#1072#1074#1083#1103#1090#1100' '#1090#1072#1081#1083#1099' '#1074' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1091#1102' '#1073#1072#1079#1091
        TabOrder = 8
      end
      object seHybrCompress: TSpinEdit
        Left = 210
        Top = 66
        Width = 49
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 100
        MinValue = 1
        TabOrder = 9
        Value = 85
      end
    end
    object pnlRight: TPanel
      Left = 324
      Top = 0
      Width = 127
      Height = 272
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        127
        272)
      object lblZooms: TLabel
        Left = 4
        Top = 6
        Width = 57
        Height = 13
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
      end
      object chklstZooms: TCheckListBox
        Left = 68
        Top = 4
        Width = 57
        Height = 263
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 0
      end
      object chkAllZooms: TCheckBox
        Left = 21
        Top = 25
        Width = 41
        Height = 17
        Caption = #1042#1089#1077
        TabOrder = 1
        OnClick = chkAllZoomsClick
      end
    end
  end
end
