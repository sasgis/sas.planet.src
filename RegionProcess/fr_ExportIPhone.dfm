object frExportIPhone: TfrExportIPhone
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
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
      Height = 13
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
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
        Caption = 'Add tiles to database'
        TabOrder = 0
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
          Left = 296
          Top = 20
          Width = 80
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = '0..9 max'
          Layout = tlCenter
        end
        object seMapCompress: TSpinEdit
          Left = 256
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
          Left = 256
          Top = 41
          Width = 40
          Height = 21
          Anchors = []
          MaxValue = 100
          MinValue = 1
          TabOrder = 1
          Value = 85
        end
        object rbHybr: TRadioButton
          Left = 239
          Top = 62
          Width = 17
          Height = 21
          Hint = 'Select Hybrid as main layer'
          Align = alClient
          TabOrder = 2
        end
        object rbMap: TRadioButton
          Left = 239
          Top = 20
          Width = 17
          Height = 21
          Hint = 'Select Map as main layer'
          Align = alClient
          Checked = True
          TabOrder = 3
          TabStop = True
        end
        object rbSat: TRadioButton
          Left = 239
          Top = 41
          Width = 17
          Height = 21
          Hint = 'Select Satellite as main layer'
          Align = alClient
          TabOrder = 4
        end
        object cbbHybr: TComboBox
          Left = 43
          Top = 62
          Width = 196
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 0
          TabOrder = 5
        end
        object cbbMap: TComboBox
          Left = 43
          Top = 20
          Width = 196
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 0
          TabOrder = 6
        end
        object cbbSat: TComboBox
          Left = 43
          Top = 41
          Width = 196
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 0
          TabOrder = 7
        end
        object lblHybrCompress: TLabel
          Left = 296
          Top = 62
          Width = 80
          Height = 21
          Align = alClient
          Caption = '100..1 max'
          Layout = tlCenter
        end
        object lblSatCompress: TLabel
          Left = 296
          Top = 41
          Width = 80
          Height = 21
          Align = alClient
          Caption = '100..1 max'
          Layout = tlCenter
        end
        object lblCompress: TLabel
          Left = 256
          Top = 3
          Width = 40
          Height = 13
          Anchors = []
          Caption = 'Compression:'
        end
        object lblHybr: TLabel
          Left = 0
          Top = 62
          Width = 43
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = 'Hybrid'
          Layout = tlCenter
        end
        object lblMap: TLabel
          Left = 0
          Top = 20
          Width = 43
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = 'Map'
          Layout = tlCenter
        end
        object lblSat: TLabel
          Left = 0
          Top = 41
          Width = 43
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = 'Satellite'
          Layout = tlCenter
        end
        object lblMaps: TLabel
          Left = 43
          Top = 0
          Width = 196
          Height = 20
          Align = alClient
          Alignment = taCenter
          AutoSize = False
          Caption = 'Enable the following map types:'
          Layout = tlCenter
        end
        object seHybrCompress: TSpinEdit
          Left = 256
          Top = 62
          Width = 40
          Height = 21
          Anchors = []
          MaxValue = 100
          MinValue = 1
          TabOrder = 8
          Value = 85
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
        Width = 57
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Zooms:'
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 19
        Width = 69
        Height = 235
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
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
        Caption = 'All'
        TabOrder = 1
        OnClick = chkAllZoomsClick
      end
    end
  end
end
