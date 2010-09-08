object frTilesDelete: TfrTilesDelete
  Left = 0
  Top = 0
  Width = 442
  Height = 275
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 451
  ExplicitHeight = 304
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 442
    Height = 25
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1059#1076#1072#1083#1080#1090#1100' '#1092#1072#1081#1083#1099' '#1082#1072#1088#1090#1099
    TabOrder = 0
    ExplicitWidth = 451
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 25
    Width = 442
    Height = 250
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 451
    ExplicitHeight = 279
    object pnlRight: TPanel
      Left = 380
      Top = 0
      Width = 62
      Height = 250
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitLeft = 389
      ExplicitHeight = 279
      object lblZoom: TLabel
        Left = 3
        Top = 3
        Width = 56
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
        ExplicitWidth = 49
      end
      object cbbZoom: TComboBox
        Left = 3
        Top = 16
        Width = 56
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 380
      Height = 250
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitWidth = 389
      ExplicitHeight = 279
      DesignSize = (
        380
        250)
      object lblMap: TLabel
        Left = 3
        Top = 3
        Width = 374
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
        ExplicitLeft = 7
        ExplicitTop = -1
        ExplicitWidth = 57
      end
      object lblStat: TLabel
        Left = 10
        Top = 66
        Width = -6
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        ExplicitWidth = 3
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 16
        Width = 374
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 383
      end
      object pnlDelBySize: TPanel
        Left = 3
        Top = 37
        Width = 374
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitWidth = 383
        object chkDelBySize: TCheckBox
          Left = 0
          Top = 0
          Width = 285
          Height = 36
          Align = alClient
          Caption = #1059#1076#1072#1083#1103#1090#1100' '#1090#1086#1083#1100#1082#1086' '#1090#1072#1081#1083#1099', '#1088#1072#1079#1084#1077#1088' '#1082#1086#1090#1086#1088#1099#1093', '#1073#1072#1081#1090
          TabOrder = 0
          ExplicitWidth = 294
        end
        object pnlDelSize: TPanel
          Left = 285
          Top = 0
          Width = 89
          Height = 36
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitLeft = 294
          object seDelSize: TSpinEdit
            Left = 4
            Top = 4
            Width = 78
            Height = 22
            MaxValue = 0
            MinValue = 0
            TabOrder = 0
            Value = 0
          end
        end
      end
    end
  end
end
