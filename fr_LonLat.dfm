object frLonLat: TfrLonLat
  Left = 0
  Top = 0
  Width = 222
  Height = 60
  VertScrollBar.Visible = False
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  ExplicitWidth = 451
  ExplicitHeight = 304
  object grdpnlMain: TGridPanel
    Left = 0
    Top = 0
    Width = 222
    Height = 60
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    ColumnCollection = <
      item
        Value = 31.676816935065850000
      end
      item
        Value = 68.323183064934140000
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
    ExplicitWidth = 209
    ExplicitHeight = 304
    DesignSize = (
      222
      60)
    object lblLat: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 3
      Width = 62
      Height = 24
      Margins.Top = 0
      Align = alClient
      Alignment = taRightJustify
      Caption = #1064#1080#1088#1086#1090#1072':'
      Layout = tlCenter
      ExplicitWidth = 59
      ExplicitHeight = 146
    end
    object lblLon: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 30
      Width = 62
      Height = 24
      Margins.Top = 0
      Align = alClient
      Alignment = taRightJustify
      Caption = #1044#1086#1083#1075#1086#1090#1072':'
      Layout = tlCenter
      ExplicitLeft = 10
      ExplicitTop = 29
      ExplicitWidth = 47
      ExplicitHeight = 13
    end
    object EditLat: TEdit
      Left = 71
      Top = 7
      Width = 136
      Height = 18
      Anchors = [akLeft]
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
    end
    object EditLon: TEdit
      Left = 71
      Top = 34
      Width = 136
      Height = 18
      Anchors = [akLeft]
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 1
    end
  end
end
