object frTilesDownload: TfrTilesDownload
  Left = 0
  Top = 0
  Width = 373
  Height = 144
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 498
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 373
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1080#1079' '#1080#1085#1090#1077#1088#1085#1077#1090#1072
    TabOrder = 0
    ExplicitWidth = 498
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 20
    Width = 373
    Height = 124
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 26
    ExplicitWidth = 498
    ExplicitHeight = 220
    object pnlRight: TPanel
      Left = 314
      Top = 0
      Width = 59
      Height = 124
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitLeft = 430
      object lblZoom: TLabel
        Left = 3
        Top = 3
        Width = 53
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
        ExplicitLeft = 25
        ExplicitTop = 52
        ExplicitWidth = 49
      end
      object cbbZoom: TComboBox
        Left = 3
        Top = 16
        Width = 53
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        ExplicitLeft = 25
        ExplicitTop = 67
        ExplicitWidth = 41
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 314
      Height = 124
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitLeft = 8
      ExplicitTop = 43
      ExplicitWidth = 345
      ExplicitHeight = 158
      object lblMap: TLabel
        Left = 3
        Top = 3
        Width = 308
        Height = 13
        Align = alTop
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
        ExplicitLeft = 20
        ExplicitTop = 5
        ExplicitWidth = 57
      end
      object lblStat: TLabel
        Left = 3
        Top = 37
        Width = 308
        Height = 13
        Align = alTop
        Caption = '_'
        ExplicitLeft = 8
        ExplicitTop = 52
        ExplicitWidth = 6
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 16
        Width = 308
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitLeft = 66
        ExplicitTop = 24
        ExplicitWidth = 241
      end
      object chk1: TCheckBox
        Left = 3
        Top = 63
        Width = 308
        Height = 13
        Align = alTop
        Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1089#1090#1072#1088#1099#1077' '#1092#1072#1081#1083#1099
        TabOrder = 1
        ExplicitLeft = 20
        ExplicitTop = 90
        ExplicitWidth = 153
      end
      object chkTryLoadIfTNE: TCheckBox
        Left = 3
        Top = 50
        Width = 308
        Height = 13
        Align = alTop
        Caption = #1055#1099#1090#1072#1090#1100#1089#1103' '#1079#1072#1075#1088#1091#1078#1072#1090#1100' '#1086#1090#1089#1091#1090#1089#1090#1074#1091#1102#1097#1080#1077' '#1090#1072#1081#1083#1099
        TabOrder = 2
        ExplicitLeft = 20
        ExplicitTop = 71
        ExplicitWidth = 249
      end
      object pnlTileReplaceCondition: TPanel
        Left = 3
        Top = 76
        Width = 308
        Height = 117
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 3
        ExplicitLeft = 0
        ExplicitTop = 82
        ExplicitWidth = 418
        object chkReplaceIfDifSize: TCheckBox
          Left = 3
          Top = 3
          Width = 302
          Height = 13
          Align = alTop
          Caption = #1090#1086#1083#1100#1082#1086' '#1087#1088#1080' '#1080#1093' '#1088#1072#1079#1083#1080#1095#1080#1080
          Enabled = False
          TabOrder = 0
          ExplicitLeft = 20
          ExplicitTop = 28
          ExplicitWidth = 145
        end
        object pnlReplaceOlder: TPanel
          Left = 3
          Top = 16
          Width = 302
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitWidth = 412
          object lblReplaceOlder: TLabel
            Left = 13
            Top = 0
            Width = 110
            Height = 25
            Align = alLeft
            Caption = #1090#1086#1083#1100#1082#1086' '#1089#1086#1079#1076#1072#1085#1085#1099#1077' '#1076#1086
            Layout = tlCenter
            ExplicitLeft = 45
            ExplicitTop = 20
            ExplicitHeight = 13
          end
          object chkReplaceOlder: TCheckBox
            Left = 0
            Top = 0
            Width = 13
            Height = 25
            Align = alLeft
            Enabled = False
            TabOrder = 0
            ExplicitLeft = 1
            ExplicitTop = 1
            ExplicitHeight = 39
          end
          object dtpReplaceOlderDate: TDateTimePicker
            Left = 123
            Top = 0
            Width = 81
            Height = 25
            Align = alLeft
            Date = 39513.436381111110000000
            Time = 39513.436381111110000000
            Enabled = False
            TabOrder = 1
            ExplicitLeft = 168
            ExplicitTop = 6
            ExplicitHeight = 17
          end
        end
      end
    end
  end
end
