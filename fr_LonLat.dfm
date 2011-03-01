object frLonLat: TfrLonLat
  Left = 0
  Top = 0
  Width = 242
  Height = 52
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
    Width = 242
    Height = 52
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    ColumnCollection = <
      item
        Value = 24.242619197679670000
      end
      item
        Value = 75.757380802320330000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = lblLat
        Row = 0
      end
      item
        Column = 1
        Control = flwpnlLat
        Row = 0
      end
      item
        Column = 0
        Control = lblLon
        Row = 1
      end
      item
        Column = 1
        Control = flwpnlLon
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
    ExplicitWidth = 451
    ExplicitHeight = 304
    DesignSize = (
      242
      52)
    object lblLat: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 51
      Height = 17
      Align = alClient
      Alignment = taRightJustify
      Caption = #1064#1080#1088#1086#1090#1072':'
      Layout = tlCenter
      ExplicitLeft = 63
      ExplicitWidth = 44
      ExplicitHeight = 13
    end
    object flwpnlLat: TFlowPanel
      Left = 60
      Top = 4
      Width = 179
      Height = 20
      Anchors = [akLeft]
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 3
      object EditLat: TEdit
        Left = 0
        Top = 0
        Width = 153
        Height = 19
        BorderStyle = bsNone
        TabOrder = 0
      end
    end
    object lblLon: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 29
      Width = 51
      Height = 17
      Align = alClient
      Alignment = taRightJustify
      Caption = #1044#1086#1083#1075#1086#1090#1072':'
      Layout = tlCenter
      ExplicitLeft = 60
      ExplicitTop = 155
      ExplicitWidth = 47
      ExplicitHeight = 13
    end
    object flwpnlLon: TFlowPanel
      Left = 60
      Top = 27
      Width = 179
      Height = 21
      Anchors = [akLeft]
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 216
      object EditLon: TEdit
        Left = 0
        Top = 0
        Width = 153
        Height = 19
        BorderStyle = bsNone
        TabOrder = 0
      end
    end
  end
end
