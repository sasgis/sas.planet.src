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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
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
  object grdpnlFull: TGridPanel
    Left = 0
    Top = 24
    Width = 451
    Height = 280
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = Panel2
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAuto
      end>
    TabOrder = 1
    DesignSize = (
      451
      280)
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 71
      Top = 115
      Width = 308
      Height = 49
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Anchors = []
      BevelOuter = bvNone
      TabOrder = 0
      object grdpnlMain: TGridPanel
        Left = 0
        Top = 0
        Width = 258
        Height = 49
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 3
        ColumnCollection = <
          item
            Value = 44.812362030905070000
          end
          item
            Value = 55.187637969094930000
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
        DesignSize = (
          258
          49)
        object lblLat: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 106
          Height = 18
          Margins.Top = 0
          Align = alClient
          Alignment = taRightJustify
          Caption = #1064#1080#1088#1086#1090#1072':'
          Layout = tlCenter
          ExplicitLeft = 68
          ExplicitWidth = 44
          ExplicitHeight = 13
        end
        object lblLon: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 24
          Width = 106
          Height = 19
          Margins.Top = 0
          Align = alClient
          Alignment = taRightJustify
          Caption = #1044#1086#1083#1075#1086#1090#1072':'
          Layout = tlCenter
          ExplicitLeft = 65
          ExplicitWidth = 47
          ExplicitHeight = 13
        end
        object EditLat: TEdit
          Left = 115
          Top = 5
          Width = 140
          Height = 17
          Anchors = [akLeft]
          AutoSize = False
          BorderStyle = bsNone
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 0
        end
        object EditLon: TEdit
          Left = 115
          Top = 26
          Width = 140
          Height = 18
          Anchors = [akLeft]
          AutoSize = False
          BorderStyle = bsNone
          Ctl3D = False
          ParentCtl3D = False
          TabOrder = 1
        end
      end
      object pnlZoom: TPanel
        Left = 258
        Top = 0
        Width = 50
        Height = 49
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        DesignSize = (
          50
          49)
        object lblZoom: TLabel
          Left = 0
          Top = 16
          Width = 10
          Height = 13
          Margins.Left = 0
          Anchors = [akTop, akRight]
          Caption = ',x'
        end
        object cbbZoom: TComboBox
          Left = 10
          Top = 13
          Width = 36
          Height = 21
          Hint = #1042#1099#1073#1086#1088' '#1084#1072#1089#1096#1090#1072#1073#1072
          Style = csDropDownList
          Anchors = [akTop, akRight]
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 0
          Text = '01'
          Items.Strings = (
            '01'
            '02'
            '03'
            '04'
            '05'
            '06'
            '07'
            '08'
            '09'
            '10'
            '11'
            '12'
            '13'
            '14'
            '15'
            '16'
            '17'
            '18'
            '19'
            '20'
            '21'
            '22'
            '23'
            '24')
        end
      end
    end
  end
end
