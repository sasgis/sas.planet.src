object frTilesDownload: TfrTilesDownload
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
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1047#1072#1075#1088#1091#1079#1080#1090#1100' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1103' '#1080#1079' '#1080#1085#1090#1077#1088#1085#1077#1090#1072
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 20
    Width = 451
    Height = 284
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlRight: TPanel
      Left = 392
      Top = 0
      Width = 59
      Height = 284
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object lblZoom: TLabel
        Left = 5
        Top = 5
        Width = 49
        Height = 15
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 53
      end
      object cbbZoom: TComboBox
        Left = 5
        Top = 20
        Width = 49
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        ExplicitLeft = 6
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 392
      Height = 284
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 1
      ExplicitLeft = -3
      ExplicitTop = 6
      object lblStat: TLabel
        Left = 5
        Top = 41
        Width = 382
        Height = 17
        Align = alTop
        AutoSize = False
        Caption = '_'
        ExplicitLeft = 3
        ExplicitTop = 39
        ExplicitWidth = 386
      end
      object lblMap: TLabel
        Left = 5
        Top = 5
        Width = 382
        Height = 15
        Align = alTop
        AutoSize = False
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
        ExplicitLeft = 3
        ExplicitTop = 3
        ExplicitWidth = 386
      end
      object Bevel1: TBevel
        Left = 5
        Top = 58
        Width = 382
        Height = 5
        Align = alTop
        Shape = bsTopLine
        ExplicitLeft = 3
        ExplicitTop = 54
        ExplicitWidth = 386
      end
      object cbbMap: TComboBox
        Left = 5
        Top = 20
        Width = 382
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitLeft = 4
        ExplicitTop = 14
      end
      object chkReplace: TCheckBox
        Left = 5
        Top = 79
        Width = 382
        Height = 16
        Align = alTop
        Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1089#1090#1072#1088#1099#1077' '#1092#1072#1081#1083#1099
        TabOrder = 1
        OnClick = chkReplaceClick
        ExplicitLeft = 3
        ExplicitTop = 66
        ExplicitWidth = 386
      end
      object chkTryLoadIfTNE: TCheckBox
        Left = 5
        Top = 63
        Width = 382
        Height = 16
        Align = alTop
        Caption = #1055#1099#1090#1072#1090#1100#1089#1103' '#1079#1072#1075#1088#1091#1078#1072#1090#1100' '#1086#1090#1089#1091#1090#1089#1090#1074#1091#1102#1097#1080#1077' '#1090#1072#1081#1083#1099
        TabOrder = 2
        ExplicitLeft = 4
        ExplicitTop = 64
      end
      object pnlTileReplaceCondition: TPanel
        Left = 5
        Top = 95
        Width = 382
        Height = 40
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        BorderWidth = 3
        Padding.Left = 15
        TabOrder = 3
        ExplicitLeft = 0
        ExplicitTop = 82
        ExplicitWidth = 386
        object chkReplaceIfDifSize: TCheckBox
          Left = 18
          Top = 3
          Width = 361
          Height = 13
          Align = alTop
          Caption = #1090#1086#1083#1100#1082#1086' '#1087#1088#1080' '#1080#1093' '#1088#1072#1079#1083#1080#1095#1080#1080
          Enabled = False
          TabOrder = 0
          ExplicitTop = 6
          ExplicitWidth = 365
        end
        object pnlReplaceOlder: TPanel
          Left = 18
          Top = 16
          Width = 361
          Height = 21
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitTop = 22
          ExplicitWidth = 365
          object lblReplaceOlder: TLabel
            Left = 16
            Top = 0
            Width = 113
            Height = 21
            Align = alLeft
            Caption = #1090#1086#1083#1100#1082#1086' '#1089#1086#1079#1076#1072#1085#1085#1099#1077' '#1076#1086' '
            Layout = tlCenter
            ExplicitLeft = 21
            ExplicitTop = 6
            ExplicitHeight = 15
          end
          object chkReplaceOlder: TCheckBox
            Left = 0
            Top = 0
            Width = 16
            Height = 21
            Align = alLeft
            Enabled = False
            TabOrder = 0
            OnClick = chkReplaceOlderClick
          end
          object dtpReplaceOlderDate: TDateTimePicker
            Left = 129
            Top = 0
            Width = 81
            Height = 21
            Align = alLeft
            Date = 39513.436381111110000000
            Time = 39513.436381111110000000
            Enabled = False
            TabOrder = 1
            ExplicitLeft = 135
          end
        end
      end
    end
  end
end
