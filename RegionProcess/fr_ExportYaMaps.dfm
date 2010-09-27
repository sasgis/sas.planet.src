object frExportYaMaps: TfrExportYaMaps
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 451
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlRight: TPanel
      Left = 360
      Top = 0
      Width = 91
      Height = 277
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblZooms: TLabel
        Left = 3
        Top = 3
        Width = 57
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
        Layout = tlCenter
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 16
        Width = 85
        Height = 258
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pnlMapsSelect: TPanel
      Left = 0
      Top = 0
      Width = 360
      Height = 277
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object chkReplaseTiles: TCheckBox
        Left = 0
        Top = 93
        Width = 360
        Height = 20
        Align = alTop
        Caption = #1047#1072#1084#1077#1085#1103#1090#1100' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1080#1077' '#1090#1072#1081#1083#1099
        TabOrder = 0
      end
      object grdpnlMaps: TGridPanel
        Left = 0
        Top = 0
        Width = 360
        Height = 93
        Align = alTop
        BevelOuter = bvNone
        ColumnCollection = <
          item
            SizeStyle = ssAuto
            Value = 25.731584258324920000
          end
          item
            Value = 100.000000000000000000
          end
          item
            SizeStyle = ssAuto
            Value = 20.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 40.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 80.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 4
            Control = lblMapCompress
            Row = 1
          end
          item
            Column = 3
            Control = seMapCompress
            Row = 1
          end
          item
            Column = 3
            Control = seSatCompress
            Row = 2
          end
          item
            Column = 1
            Control = cbbHybr
            Row = 3
          end
          item
            Column = 1
            Control = cbbMap
            Row = 1
          end
          item
            Column = 1
            Control = cbbSat
            Row = 2
          end
          item
            Column = 4
            Control = lblSatCompress
            Row = 2
          end
          item
            Column = 3
            Control = lblCompress
            Row = 0
          end
          item
            Column = 0
            Control = lblHybr
            Row = 3
          end
          item
            Column = 0
            Control = lblMap
            Row = 1
          end
          item
            Column = 0
            Control = lblSat
            Row = 2
          end
          item
            Column = 1
            Control = lblMaps
            Row = 0
          end>
        RowCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 20.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 21.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 21.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 21.000000000000000000
          end>
        TabOrder = 1
        DesignSize = (
          360
          93)
        object lblMapCompress: TLabel
          Left = 280
          Top = 20
          Width = 80
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = '0..9 max'
          Layout = tlCenter
          ExplicitLeft = 269
          ExplicitTop = 21
          ExplicitWidth = 43
          ExplicitHeight = 13
        end
        object seMapCompress: TSpinEdit
          Left = 240
          Top = 20
          Width = 40
          Height = 21
          Anchors = []
          MaxValue = 9
          MinValue = 0
          TabOrder = 0
          Value = 2
        end
        object seSatCompress: TSpinEdit
          Left = 240
          Top = 41
          Width = 40
          Height = 21
          Anchors = []
          MaxValue = 100
          MinValue = 1
          TabOrder = 1
          Value = 85
        end
        object cbbHybr: TComboBox
          Left = 43
          Top = 62
          Width = 197
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 0
          TabOrder = 2
        end
        object cbbMap: TComboBox
          Left = 43
          Top = 20
          Width = 197
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 0
          TabOrder = 3
        end
        object cbbSat: TComboBox
          Left = 43
          Top = 41
          Width = 197
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 0
          TabOrder = 4
        end
        object lblSatCompress: TLabel
          Left = 280
          Top = 41
          Width = 80
          Height = 21
          Align = alClient
          Caption = '100..1 max'
          Layout = tlCenter
          ExplicitWidth = 55
          ExplicitHeight = 13
        end
        object lblCompress: TLabel
          Left = 240
          Top = 3
          Width = 40
          Height = 13
          Anchors = []
          Caption = #1057#1078#1072#1090#1080#1077':'
        end
        object lblHybr: TLabel
          Left = 0
          Top = 62
          Width = 43
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = #1043#1080#1073#1088#1080#1076
          Layout = tlCenter
          ExplicitLeft = 6
          ExplicitWidth = 29
          ExplicitHeight = 37
        end
        object lblMap: TLabel
          Left = 0
          Top = 20
          Width = 43
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = #1050#1072#1088#1090#1072
          Layout = tlCenter
          ExplicitLeft = 5
          ExplicitTop = 21
          ExplicitWidth = 31
          ExplicitHeight = 13
        end
        object lblSat: TLabel
          Left = 0
          Top = 41
          Width = 43
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = #1057#1087#1091#1090#1085#1080#1082
          Layout = tlCenter
          ExplicitLeft = 5
          ExplicitTop = 45
          ExplicitHeight = 13
        end
        object lblMaps: TLabel
          Left = 43
          Top = 0
          Width = 197
          Height = 20
          Align = alClient
          Alignment = taCenter
          AutoSize = False
          Caption = #1042#1082#1083#1102#1095#1080#1090#1100' '#1089#1083#1077#1076#1091#1102#1097#1080#1077' '#1090#1080#1087#1099' '#1082#1072#1088#1090':'
          Layout = tlCenter
          ExplicitLeft = 5
          ExplicitTop = 4
          ExplicitWidth = 174
          ExplicitHeight = 13
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object lblTargetPath: TLabel
      Left = 3
      Top = 3
      Width = 86
      Height = 13
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
    end
    object edtTargetPath: TEdit
      Left = 89
      Top = 3
      Width = 338
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetPath: TButton
      Left = 427
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetPathClick
    end
  end
end
