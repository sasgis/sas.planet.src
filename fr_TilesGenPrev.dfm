object frTilesGenPrev: TfrTilesGenPrev
  Left = 0
  Top = 0
  Width = 606
  Height = 211
  Align = alClient
  TabOrder = 0
  object pnlBottom: TPanel
    Left = 0
    Top = 32
    Width = 606
    Height = 179
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 7
    ExplicitTop = 31
    ExplicitWidth = 526
    ExplicitHeight = 173
    object pnlRight: TPanel
      Left = 492
      Top = 0
      Width = 114
      Height = 179
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 220
      DesignSize = (
        114
        179)
      object lblFromZoom: TLabel
        Left = 6
        Top = -1
        Width = 94
        Height = 13
        Caption = #1048#1079' '#1089#1083#1086#1103' '#1084#1072#1089#1096#1090#1072#1073#1072':'
      end
      object lblZooms: TLabel
        Left = 6
        Top = 45
        Width = 88
        Height = 13
        Caption = #1042' '#1089#1083#1086#1080' '#1084#1072#1089#1096#1090#1072#1073#1072':'
      end
      object ComboBox: TComboBox
        Left = 53
        Top = 18
        Width = 57
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object chkAllZooms: TCheckBox
        Left = 6
        Top = 64
        Width = 41
        Height = 17
        Caption = #1042#1089#1077
        TabOrder = 1
      end
      object chklstZooms: TCheckListBox
        Left = 53
        Top = 64
        Width = 57
        Height = 111
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
    end
    object pnlCenter: TPanel
      Left = 0
      Top = 0
      Width = 492
      Height = 179
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 367
      ExplicitHeight = 101
      DesignSize = (
        492
        179)
      object lblMap: TLabel
        Left = 7
        Top = -1
        Width = 57
        Height = 13
        Caption = #1058#1080#1087' '#1082#1072#1088#1090#1099':'
      end
      object lblStat: TLabel
        Left = 10
        Top = 100
        Width = 128
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        ExplicitWidth = 28
      end
      object lblResampler: TLabel
        Left = 5
        Top = 37
        Width = 52
        Height = 13
        Caption = #1040#1083#1075#1086#1088#1080#1090#1084':'
      end
      object cbbMap: TComboBox
        Left = 7
        Top = 16
        Width = 479
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 354
      end
      object cbbResampler: TComboBox
        Left = 7
        Top = 51
        Width = 161
        Height = 21
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
        Left = 7
        Top = 73
        Width = 209
        Height = 17
        Caption = #1048#1079#1084#1077#1085#1103#1090#1100' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1080#1077' '#1092#1072#1081#1083#1099
        TabOrder = 2
      end
      object chkSaveFullOnly: TCheckBox
        Left = 7
        Top = 91
        Width = 281
        Height = 17
        Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1090#1086#1083#1100#1082#1086' '#1087#1086#1083#1085#1086#1089#1090#1100#1102' '#1079#1072#1087#1086#1083#1085#1077#1085#1085#1099#1077' '#1090#1072#1081#1083#1099
        TabOrder = 3
      end
      object chkFromPrevZoom: TCheckBox
        Left = 7
        Top = 110
        Width = 393
        Height = 17
        Caption = #1060#1086#1088#1084#1080#1088#1086#1074#1072#1090#1100' '#1082#1072#1078#1076#1099#1081' '#1074#1099#1073#1088#1072#1085#1085#1099#1081' '#1084#1072#1089#1096#1090#1072#1073' '#1080#1079' '#1087#1088#1077#1076#1099#1076#1091#1097#1077#1075#1086' '#1074#1099#1073#1088#1072#1085#1085#1086#1075#1086
        TabOrder = 4
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 606
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 526
    object bvlTop: TBevel
      Left = 0
      Top = 0
      Width = 606
      Height = 25
      Align = alTop
      Shape = bsBottomLine
      ExplicitWidth = 526
    end
    object lblCaption: TLabel
      Left = 5
      Top = 0
      Width = 274
      Height = 13
      Caption = #1057#1092#1086#1088#1084#1080#1088#1086#1074#1072#1090#1100' '#1087#1088#1077#1076#1099#1076#1091#1097#1080#1077' '#1089#1083#1086#1080' '#1089#1087#1091#1090#1085#1080#1082#1086#1074#1086#1081' '#1082#1072#1088#1090#1099
    end
  end
end
