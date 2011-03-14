object frLonLat: TfrLonLat
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  VertScrollBar.Visible = False
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object grdpnlMain: TGridPanel
    Left = 0
    Top = 24
    Width = 451
    Height = 280
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    ColumnCollection = <
      item
        Value = 31.676816935065850000
      end
      item
        Value = 68.323183064934160000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = lblLat
        Row = 0
      end
      item
        Column = 0
        Control = lblLon
        Row = 1
      end
      item
        Column = 1
        Control = EditLat
        Row = 0
      end
      item
        Column = 1
        Control = EditLon
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 0
    ExplicitTop = 30
    DesignSize = (
      451
      280)
    object lblLat: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 3
      Width = 134
      Height = 134
      Margins.Top = 0
      Align = alClient
      Alignment = taRightJustify
      Caption = #1064#1080#1088#1086#1090#1072':'
      Layout = tlCenter
      ExplicitLeft = 96
      ExplicitWidth = 44
      ExplicitHeight = 13
    end
    object lblLon: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 140
      Width = 134
      Height = 134
      Margins.Top = 0
      Align = alClient
      Alignment = taRightJustify
      Caption = #1044#1086#1083#1075#1086#1090#1072':'
      Layout = tlCenter
      ExplicitLeft = 93
      ExplicitTop = 135
      ExplicitWidth = 47
      ExplicitHeight = 13
    end
    object EditLat: TEdit
      Left = 143
      Top = 62
      Width = 131
      Height = 18
      Anchors = [akLeft]
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      ExplicitTop = 60
    end
    object EditLon: TEdit
      Left = 143
      Top = 199
      Width = 131
      Height = 18
      Anchors = [akLeft]
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 1
      ExplicitTop = 192
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object ComboBoxCoordType: TComboBox
      Left = 6
      Top = 3
      Width = 123
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = #1043#1077#1086#1075#1088#1072#1092#1080#1095#1077#1089#1082#1080#1077
      OnSelect = ComboBoxCoordTypeSelect
      Items.Strings = (
        #1043#1077#1086#1075#1088#1072#1092#1080#1095#1077#1089#1082#1080#1077
        #1055#1080#1082#1089#1077#1083#1100#1085#1099#1077
        #1058#1072#1081#1083#1086#1074#1099#1077)
    end
  end
end
