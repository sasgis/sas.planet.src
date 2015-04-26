object frExportIPhone: TfrExportIPhone
  Left = 0
  Top = 0
  Width = 521
  Height = 150
  Align = alClient
  Constraints.MinHeight = 150
  Constraints.MinWidth = 480
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 521
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object lblTargetPath: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object btnSelectTargetPath: TButton
      Left = 497
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 0
      OnClick = btnSelectTargetPathClick
    end
    object edtTargetPath: TEdit
      Left = 47
      Top = 3
      Width = 450
      Height = 21
      Align = alClient
      TabOrder = 1
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 27
    Width = 521
    Height = 123
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMaps: TPanel
      Left = 0
      Top = 0
      Width = 446
      Height = 123
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvNone
      TabOrder = 0
      object chkAppendTilse: TCheckBox
        Left = 0
        Top = 91
        Width = 446
        Height = 17
        Align = alTop
        Caption = 'Add tiles to database'
        TabOrder = 0
      end
      object grdpnlMaps: TGridPanel
        Left = 0
        Top = 0
        Width = 446
        Height = 91
        Align = alTop
        BevelOuter = bvNone
        ColumnCollection = <
          item
            SizeStyle = ssAbsolute
            Value = 50.000000000000000000
          end
          item
            Value = 100.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 15.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 60.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 70.000000000000000000
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
            ColumnSpan = 2
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
          end
          item
            Column = 1
            Control = pnlHyb
            Row = 3
          end
          item
            Column = 1
            Control = pnlMap
            Row = 1
          end
          item
            Column = 1
            Control = pnlSat
            Row = 2
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
          end
          item
            Value = 100.000000000000000000
          end>
        TabOrder = 1
        DesignSize = (
          446
          91)
        object lblMapCompress: TLabel
          Left = 376
          Top = 20
          Width = 70
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = '0..9 max'
          Layout = tlCenter
        end
        object seMapCompress: TSpinEdit
          Left = 321
          Top = 20
          Width = 49
          Height = 22
          Anchors = []
          MaxValue = 9
          MinValue = 0
          TabOrder = 0
          Value = 2
        end
        object seSatCompress: TSpinEdit
          Left = 321
          Top = 41
          Width = 49
          Height = 22
          Anchors = []
          MaxValue = 100
          MinValue = 1
          TabOrder = 1
          Value = 85
        end
        object rbHybr: TRadioButton
          Left = 301
          Top = 62
          Width = 15
          Height = 21
          Hint = 'Select Hybrid as main layer'
          Align = alClient
          TabOrder = 2
        end
        object rbMap: TRadioButton
          Left = 301
          Top = 20
          Width = 15
          Height = 21
          Hint = 'Select Map as main layer'
          Align = alClient
          Checked = True
          TabOrder = 3
          TabStop = True
        end
        object rbSat: TRadioButton
          Left = 301
          Top = 41
          Width = 15
          Height = 21
          Hint = 'Select Satellite as main layer'
          Align = alClient
          TabOrder = 4
        end
        object lblHybrCompress: TLabel
          Left = 376
          Top = 62
          Width = 70
          Height = 21
          Align = alClient
          Caption = '100..1 max'
          Layout = tlCenter
        end
        object lblSatCompress: TLabel
          Left = 376
          Top = 41
          Width = 70
          Height = 21
          Align = alClient
          Caption = '100..1 max'
          Layout = tlCenter
        end
        object lblCompress: TLabel
          Left = 316
          Top = 0
          Width = 130
          Height = 20
          Align = alClient
          Caption = 'Compression:'
        end
        object lblHybr: TLabel
          Left = 0
          Top = 62
          Width = 50
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = 'Hybrid'
          Layout = tlCenter
        end
        object lblMap: TLabel
          Left = 0
          Top = 20
          Width = 50
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = 'Map'
          Layout = tlCenter
        end
        object lblSat: TLabel
          Left = 0
          Top = 41
          Width = 50
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = 'Satellite'
          Layout = tlCenter
        end
        object lblMaps: TLabel
          Left = 50
          Top = 0
          Width = 251
          Height = 20
          Align = alTop
          AutoSize = False
          Caption = 'Enable the following map types:'
          Layout = tlCenter
        end
        object seHybrCompress: TSpinEdit
          Left = 321
          Top = 62
          Width = 49
          Height = 22
          Anchors = []
          MaxValue = 100
          MinValue = 1
          TabOrder = 5
          Value = 85
        end
        object pnlHyb: TPanel
          Left = 50
          Top = 62
          Width = 251
          Height = 21
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 6
        end
        object pnlMap: TPanel
          Left = 50
          Top = 20
          Width = 251
          Height = 21
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 7
        end
        object pnlSat: TPanel
          Left = 50
          Top = 41
          Width = 251
          Height = 21
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 8
        end
      end
    end
    object pnlZoom: TPanel
      Left = 446
      Top = 0
      Width = 75
      Height = 123
      Align = alRight
      Anchors = []
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
    end
  end
end
