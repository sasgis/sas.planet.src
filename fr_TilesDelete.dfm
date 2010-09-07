object frTilesDelete: TfrTilesDelete
  Left = 0
  Top = 0
  Width = 429
  Height = 133
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 526
  ExplicitHeight = 151
  object seDelSize: TSpinEdit
    Left = 280
    Top = 109
    Width = 97
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 429
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 526
    object bvlTop: TBevel
      Left = 0
      Top = 0
      Width = 429
      Height = 25
      Align = alTop
      Shape = bsBottomLine
      ExplicitWidth = 526
    end
    object lblHeader: TLabel
      Left = 7
      Top = 4
      Width = 116
      Height = 13
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1092#1072#1081#1083#1099' '#1082#1072#1088#1090#1099
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 32
    Width = 429
    Height = 101
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 7
    ExplicitTop = 31
    ExplicitWidth = 526
    ExplicitHeight = 173
    object pnlRight: TPanel
      Left = 367
      Top = 0
      Width = 62
      Height = 101
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 464
      ExplicitHeight = 119
      object lblZoom: TLabel
        Left = 7
        Top = -1
        Width = 49
        Height = 13
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
      end
      object cbbZoom: TComboBox
        Left = 3
        Top = 16
        Width = 51
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 367
      Height = 101
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 152
      ExplicitTop = 66
      ExplicitWidth = 185
      ExplicitHeight = 41
      DesignSize = (
        367
        101)
      object lblMap: TLabel
        Left = 7
        Top = -1
        Width = 57
        Height = 13
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
      end
      object lblStat: TLabel
        Left = 10
        Top = 66
        Width = 351
        Height = 13
        Anchors = [akLeft, akTop, akRight]
      end
      object cbbMap: TComboBox
        Left = 7
        Top = 16
        Width = 354
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 451
      end
      object chkDelBySize: TCheckBox
        Left = 7
        Top = 43
        Width = 354
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = #1059#1076#1072#1083#1103#1090#1100' '#1090#1086#1083#1100#1082#1086' '#1090#1072#1081#1083#1099', '#1088#1072#1079#1084#1077#1088' '#1082#1086#1090#1086#1088#1099#1093', '#1073#1072#1081#1090
        TabOrder = 1
        ExplicitWidth = 451
      end
    end
  end
end
