object frTilesGenPrev: TfrTilesGenPrev
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlBottom: TPanel
    Left = 0
    Top = 22
    Width = 451
    Height = 282
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlRight: TPanel
      Left = 337
      Top = 0
      Width = 114
      Height = 282
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblFromZoom: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 94
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = #1048#1079' '#1089#1083#1086#1103' '#1084#1072#1089#1096#1090#1072#1073#1072':'
      end
      object lblZooms: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 43
        Width = 88
        Height = 13
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        Caption = #1042' '#1089#1083#1086#1080' '#1084#1072#1089#1096#1090#1072#1073#1072':'
      end
      object cbbFromZoom: TComboBox
        Left = 3
        Top = 19
        Width = 108
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
        OnChange = cbbFromZoomChange
      end
      object chkAllZooms: TCheckBox
        Left = 3
        Top = 262
        Width = 108
        Height = 17
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 1
        OnClick = chkAllZoomsClick
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 59
        Width = 108
        Height = 203
        OnClickCheck = chklstZoomsClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 337
      Height = 282
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      DesignSize = (
        337
        282)
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 331
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
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
        Width = 3
        Height = 13
        Anchors = [akLeft, akTop, akRight]
      end
      object lblResampler: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 43
        Width = 331
        Height = 13
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = #1040#1083#1075#1086#1088#1080#1090#1084':'
        ExplicitLeft = 5
        ExplicitTop = 37
        ExplicitWidth = 52
      end
      object Bevel1: TBevel
        AlignWithMargins = True
        Left = 3
        Top = 85
        Width = 331
        Height = 3
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Shape = bsTopLine
        ExplicitTop = 83
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 19
        Width = 331
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 0
        TabOrder = 0
      end
      object cbbResampler: TComboBox
        Left = 3
        Top = 59
        Width = 331
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
      end
      object chkReplace: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 91
        Width = 331
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = #1048#1079#1084#1077#1085#1103#1090#1100' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1080#1077' '#1092#1072#1081#1083#1099
        TabOrder = 2
      end
      object chkSaveFullOnly: TCheckBox
        Left = 3
        Top = 108
        Width = 331
        Height = 17
        Align = alTop
        Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1090#1086#1083#1100#1082#1086' '#1087#1086#1083#1085#1086#1089#1090#1100#1102' '#1079#1072#1087#1086#1083#1085#1077#1085#1085#1099#1077' '#1090#1072#1081#1083#1099
        TabOrder = 3
      end
      object chkFromPrevZoom: TCheckBox
        Left = 3
        Top = 125
        Width = 331
        Height = 17
        Align = alTop
        Caption = #1060#1086#1088#1084#1080#1088#1086#1074#1072#1090#1100' '#1082#1072#1078#1076#1099#1081' '#1074#1099#1073#1088#1072#1085#1085#1099#1081' '#1084#1072#1089#1096#1090#1072#1073' '#1080#1079' '#1087#1088#1077#1076#1099#1076#1091#1097#1077#1075#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1075#1086
        TabOrder = 4
        OnClick = chkFromPrevZoomClick
      end
    end
  end
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
    Caption = #1057#1092#1086#1088#1084#1080#1088#1086#1074#1072#1090#1100' '#1087#1088#1077#1076#1099#1076#1091#1097#1080#1077' '#1089#1083#1086#1080' '#1089#1087#1091#1090#1085#1080#1082#1086#1074#1086#1081' '#1082#1072#1088#1090#1099
    TabOrder = 1
  end
end
