object frTilesDelete: TfrTilesDelete
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
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1059#1076#1072#1083#1080#1090#1100' '#1092#1072#1081#1083#1099' '#1082#1072#1088#1090#1099
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 22
    Width = 451
    Height = 282
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 25
    ExplicitHeight = 279
    object pnlRight: TPanel
      Left = 389
      Top = 0
      Width = 62
      Height = 282
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitHeight = 279
      object lblZoom: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 56
        Height = 14
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
      end
      object cbbZoom: TComboBox
        Left = 3
        Top = 20
        Width = 56
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        ExplicitTop = 16
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 389
      Height = 282
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitLeft = -3
      ExplicitHeight = 279
      DesignSize = (
        389
        282)
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 383
        Height = 14
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
      end
      object lblStat: TLabel
        Left = 10
        Top = 66
        Width = 3
        Height = 13
        Anchors = [akLeft, akTop, akRight]
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 20
        Width = 383
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitTop = 16
      end
      object flwpnlDelBySize: TFlowPanel
        AlignWithMargins = True
        Left = 6
        Top = 44
        Width = 377
        Height = 24
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        Padding.Top = 2
        TabOrder = 1
        ExplicitTop = 41
        object chkDelBySize: TCheckBox
          Left = 0
          Top = 2
          Width = 13
          Height = 21
          TabOrder = 0
        end
        object lblDelSize: TLabel
          AlignWithMargins = True
          Left = 16
          Top = 5
          Width = 238
          Height = 13
          Caption = #1059#1076#1072#1083#1103#1090#1100' '#1090#1086#1083#1100#1082#1086' '#1090#1072#1081#1083#1099', '#1088#1072#1079#1084#1077#1088' '#1082#1086#1090#1086#1088#1099#1093', '#1073#1072#1081#1090
        end
        object seDelSize: TSpinEdit
          Left = 257
          Top = 2
          Width = 69
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
    end
  end
end
