object frExportIPhone: TfrExportIPhone
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
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object lblTargetPath: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 86
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object btnSelectTargetPath: TButton
      Left = 427
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 0
      OnClick = btnSelectTargetPathClick
    end
    object edtTargetPath: TEdit
      Left = 92
      Top = 3
      Width = 335
      Height = 21
      Align = alClient
      TabOrder = 1
      ExplicitLeft = 89
      ExplicitWidth = 338
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 27
    Width = 451
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMaps: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 277
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object chkAppendTilse: TCheckBox
        Left = 0
        Top = 91
        Width = 376
        Height = 17
        Align = alTop
        Caption = #1044#1086#1073#1072#1074#1083#1103#1090#1100' '#1090#1072#1081#1083#1099' '#1074' '#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1091#1102' '#1073#1072#1079#1091
        TabOrder = 0
        ExplicitTop = 93
      end
      object grdpnlMaps: TGridPanel
        Left = 0
        Top = 0
        Width = 376
        Height = 91
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
            Column = 2
            Control = rbHybr
            Row = 3
          end
          item
            Column = 2
            Control = rbMap
            Row = 1
          end
          item
            Column = 2
            Control = rbSat
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
            Control = lblHybrCompress
            Row = 3
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
          end
          item
            Column = 3
            Control = seHybrCompress
            Row = 3
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
          376
          91)
        object lblMapCompress: TLabel
          Left = 290
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
          Left = 250
          Top = 20
          Width = 40
          Height = 22
          Anchors = []
          MaxValue = 9
          MinValue = 0
          TabOrder = 0
          Value = 2
          ExplicitLeft = 256
        end
        object seSatCompress: TSpinEdit
          Left = 250
          Top = 41
          Width = 40
          Height = 22
          Anchors = []
          MaxValue = 100
          MinValue = 1
          TabOrder = 1
          Value = 85
          ExplicitLeft = 256
        end
        object rbHybr: TRadioButton
          Left = 233
          Top = 62
          Width = 17
          Height = 21
          Hint = #1042#1099#1073#1088#1072#1090#1100' '#1075#1080#1073#1088#1080#1076' '#1074' '#1082#1072#1095#1077#1089#1090#1074#1077' '#1086#1089#1085#1086#1074#1085#1086#1075#1086' '#1089#1083#1086#1103
          Align = alClient
          TabOrder = 2
          ExplicitLeft = 239
        end
        object rbMap: TRadioButton
          Left = 233
          Top = 20
          Width = 17
          Height = 21
          Hint = #1042#1099#1073#1088#1072#1090#1100' '#1082#1072#1088#1090#1091' '#1074' '#1091#1072#1095#1077#1089#1090#1074#1077' '#1086#1089#1085#1086#1074#1085#1086#1075#1086' '#1089#1083#1086#1103
          Align = alClient
          Checked = True
          TabOrder = 3
          TabStop = True
          ExplicitLeft = 239
        end
        object rbSat: TRadioButton
          Left = 233
          Top = 41
          Width = 17
          Height = 21
          Hint = #1042#1099#1073#1088#1072#1090#1100' '#1089#1087#1091#1090#1085#1080#1082' '#1074' '#1091#1072#1095#1077#1089#1090#1074#1077' '#1086#1089#1085#1086#1074#1085#1086#1075#1086' '#1089#1083#1086#1103
          Align = alClient
          TabOrder = 4
          ExplicitLeft = 239
        end
        object cbbHybr: TComboBox
          Left = 43
          Top = 62
          Width = 190
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 13
          TabOrder = 5
          ExplicitWidth = 196
        end
        object cbbMap: TComboBox
          Left = 43
          Top = 20
          Width = 190
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 13
          TabOrder = 6
          ExplicitWidth = 196
        end
        object cbbSat: TComboBox
          Left = 43
          Top = 41
          Width = 190
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 13
          TabOrder = 7
          ExplicitWidth = 196
        end
        object lblHybrCompress: TLabel
          Left = 290
          Top = 62
          Width = 80
          Height = 21
          Align = alClient
          Caption = '100..1 max'
          Layout = tlCenter
          ExplicitLeft = 296
          ExplicitWidth = 55
          ExplicitHeight = 13
        end
        object lblSatCompress: TLabel
          Left = 290
          Top = 41
          Width = 80
          Height = 21
          Align = alClient
          Caption = '100..1 max'
          Layout = tlCenter
          ExplicitLeft = 296
          ExplicitWidth = 55
          ExplicitHeight = 13
        end
        object lblCompress: TLabel
          Left = 250
          Top = 3
          Width = 40
          Height = 13
          Anchors = []
          Caption = #1057#1078#1072#1090#1080#1077':'
          ExplicitLeft = 256
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
          Width = 190
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
        object seHybrCompress: TSpinEdit
          Left = 250
          Top = 62
          Width = 40
          Height = 22
          Anchors = []
          MaxValue = 100
          MinValue = 1
          TabOrder = 8
          Value = 85
          ExplicitLeft = 256
        end
      end
    end
    object pnlRight: TPanel
      Left = 376
      Top = 0
      Width = 75
      Height = 277
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object lblZooms: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 69
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
        ExplicitWidth = 57
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 19
        Width = 69
        Height = 235
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitTop = 16
        ExplicitHeight = 241
      end
      object chkAllZooms: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 257
        Width = 69
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 1
        OnClick = chkAllZoomsClick
      end
    end
  end
end
