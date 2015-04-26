object frExportYaMobileV4: TfrExportYaMobileV4
  Left = 0
  Top = 0
  Width = 480
  Height = 200
  Align = alClient
  Constraints.MinHeight = 200
  Constraints.MinWidth = 480
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 480
    Height = 173
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlZoom: TPanel
      Left = 389
      Top = 0
      Width = 91
      Height = 173
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
    end
    object pnlMapsSelect: TPanel
      Left = 0
      Top = 0
      Width = 389
      Height = 173
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object grdpnlMaps: TGridPanel
        Left = 0
        Top = 0
        Width = 389
        Height = 93
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
            Value = 50.000000000000000000
          end
          item
            SizeStyle = ssAbsolute
            Value = 60.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 3
            Control = lblMapCompress
            Row = 1
          end
          item
            Column = 2
            Control = seMapCompress
            Row = 1
          end
          item
            Column = 2
            Control = seSatCompress
            Row = 2
          end
          item
            Column = 3
            Control = lblSatCompress
            Row = 2
          end
          item
            Column = 2
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
            Column = 0
            ColumnSpan = 2
            Control = lblMaps
            Row = 0
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
          end>
        TabOrder = 0
        DesignSize = (
          389
          93)
        object lblMapCompress: TLabel
          Left = 329
          Top = 20
          Width = 60
          Height = 21
          Align = alClient
          AutoSize = False
          Caption = '0..9 max'
          Layout = tlCenter
        end
        object seMapCompress: TSpinEdit
          Left = 284
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
          Left = 284
          Top = 41
          Width = 40
          Height = 22
          Anchors = []
          MaxValue = 100
          MinValue = 1
          TabOrder = 1
          Value = 85
        end
        object lblSatCompress: TLabel
          Left = 329
          Top = 41
          Width = 60
          Height = 21
          Align = alClient
          Caption = '100..1 max'
          Layout = tlCenter
        end
        object lblCompress: TLabel
          Left = 279
          Top = 0
          Width = 110
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
          Left = 0
          Top = 0
          Width = 279
          Height = 20
          Align = alClient
          AutoSize = False
          Caption = 'Enable the following map types:'
          Layout = tlCenter
        end
        object pnlHyb: TPanel
          Left = 50
          Top = 62
          Width = 229
          Height = 21
          Align = alClient
          Anchors = []
          BevelOuter = bvNone
          TabOrder = 2
        end
        object pnlMap: TPanel
          Left = 50
          Top = 20
          Width = 229
          Height = 21
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 3
        end
        object pnlSat: TPanel
          Left = 50
          Top = 41
          Width = 229
          Height = 21
          Align = alClient
          Anchors = []
          BevelOuter = bvNone
          TabOrder = 4
        end
      end
      object chkReplaseTiles: TCheckBox
        Left = 0
        Top = 93
        Width = 389
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
    Width = 480
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
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetPath: TEdit
      Left = 47
      Top = 3
      Width = 409
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetPath: TButton
      Left = 456
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
