object frExportYaMobileV4: TfrExportYaMobileV4
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
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
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 85
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Zooms:'
        Layout = tlCenter
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 19
        Width = 85
        Height = 255
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
        TabOrder = 0
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
        end
        object seMapCompress: TSpinEdit
          Left = 240
          Top = 20
          Width = 40
          Height = 22
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
          Height = 22
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
          ItemHeight = 13
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
          ItemHeight = 13
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
          ItemHeight = 13
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
        end
        object lblCompress: TLabel
          Left = 240
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
          Width = 197
          Height = 20
          Align = alClient
          Alignment = taCenter
          AutoSize = False
          Caption = 'Enable the following map types:'
          Layout = tlCenter
        end
      end
      object chkReplaseTiles: TCheckBox
        Left = 0
        Top = 93
        Width = 360
        Height = 20
        Align = alTop
        Caption = 'Replace existing tiles'
        TabOrder = 1
      end
      object rgTileSize: TRadioGroup
        Left = 0
        Top = 119
        Width = 240
        Height = 42
        Caption = 'Tile Size'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          '128x128 pix'
          '256x256 pix')
        TabOrder = 2
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
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetPath: TEdit
      Left = 47
      Top = 3
      Width = 380
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
