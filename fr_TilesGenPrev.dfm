object frTilesGenPrev: TfrTilesGenPrev
  Left = 0
  Top = 0
  Width = 549
  Height = 239
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 451
  ExplicitHeight = 304
  object pnlBottom: TPanel
    Left = 0
    Top = 25
    Width = 549
    Height = 214
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 32
    ExplicitWidth = 451
    ExplicitHeight = 272
    object pnlRight: TPanel
      Left = 435
      Top = 0
      Width = 114
      Height = 214
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitLeft = 337
      ExplicitHeight = 272
      object lblFromZoom: TLabel
        Left = 3
        Top = 3
        Width = 108
        Height = 13
        Align = alTop
        Caption = #1048#1079' '#1089#1083#1086#1103' '#1084#1072#1089#1096#1090#1072#1073#1072':'
        ExplicitLeft = 6
        ExplicitTop = -1
        ExplicitWidth = 94
      end
      object lblZooms: TLabel
        Left = 3
        Top = 37
        Width = 108
        Height = 13
        Align = alTop
        Caption = #1042' '#1089#1083#1086#1080' '#1084#1072#1089#1096#1090#1072#1073#1072':'
        ExplicitLeft = 6
        ExplicitTop = 45
        ExplicitWidth = 88
      end
      object cbbFromZoom: TComboBox
        Left = 3
        Top = 16
        Width = 108
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbFromZoomChange
        ExplicitLeft = 53
        ExplicitTop = 18
        ExplicitWidth = 57
      end
      object chkAllZooms: TCheckBox
        Left = 3
        Top = 194
        Width = 108
        Height = 17
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 1
        OnClick = chkAllZoomsClick
        ExplicitLeft = 22
        ExplicitTop = 200
        ExplicitWidth = 41
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 50
        Width = 108
        Height = 144
        Align = alClient
        ItemHeight = 13
        TabOrder = 2
        ExplicitLeft = 53
        ExplicitTop = 56
        ExplicitWidth = 57
        ExplicitHeight = 61
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 435
      Height = 214
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitWidth = 337
      ExplicitHeight = 272
      DesignSize = (
        435
        214)
      object lblMap: TLabel
        Left = 3
        Top = 3
        Width = 429
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
        Top = 100
        Width = 101
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        ExplicitWidth = 3
      end
      object lblResampler: TLabel
        Left = 3
        Top = 37
        Width = 429
        Height = 13
        Align = alTop
        AutoSize = False
        Caption = #1040#1083#1075#1086#1088#1080#1090#1084':'
        ExplicitLeft = 5
        ExplicitWidth = 52
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 16
        Width = 429
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 331
      end
      object cbbResampler: TComboBox
        Left = 3
        Top = 50
        Width = 429
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = 'Box'
        Items.Strings = (
          'Box'
          'Linear'
          'Cosine'
          'Spline'
          'Mitchell'
          'Cubic'
          'Hermite'
          'Lanczos'
          'Gaussian'
          'Blackman'
          'Hann'
          'Hamming'
          'Sinsh')
        ExplicitWidth = 331
      end
      object chkReplace: TCheckBox
        Left = 3
        Top = 71
        Width = 429
        Height = 17
        Align = alTop
        Caption = #1048#1079#1084#1077#1085#1103#1090#1100' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1080#1077' '#1092#1072#1081#1083#1099
        TabOrder = 2
        ExplicitWidth = 331
      end
      object chkSaveFullOnly: TCheckBox
        Left = 3
        Top = 88
        Width = 429
        Height = 17
        Align = alTop
        Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1090#1086#1083#1100#1082#1086' '#1087#1086#1083#1085#1086#1089#1090#1100#1102' '#1079#1072#1087#1086#1083#1085#1077#1085#1085#1099#1077' '#1090#1072#1081#1083#1099
        TabOrder = 3
        ExplicitWidth = 331
      end
      object chkFromPrevZoom: TCheckBox
        Left = 3
        Top = 105
        Width = 429
        Height = 17
        Align = alTop
        Caption = #1060#1086#1088#1084#1080#1088#1086#1074#1072#1090#1100' '#1082#1072#1078#1076#1099#1081' '#1074#1099#1073#1088#1072#1085#1085#1099#1081' '#1084#1072#1089#1096#1090#1072#1073' '#1080#1079' '#1087#1088#1077#1076#1099#1076#1091#1097#1077#1075#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1075#1086
        TabOrder = 4
        ExplicitWidth = 331
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 549
    Height = 25
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = #1057#1092#1086#1088#1084#1080#1088#1086#1074#1072#1090#1100' '#1087#1088#1077#1076#1099#1076#1091#1097#1080#1077' '#1089#1083#1086#1080' '#1089#1087#1091#1090#1085#1080#1082#1086#1074#1086#1081' '#1082#1072#1088#1090#1099
    TabOrder = 1
    ExplicitWidth = 451
  end
end
