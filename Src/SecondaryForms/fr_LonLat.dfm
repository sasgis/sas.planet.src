object frLonLat: TfrLonLat
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  VertScrollBar.Visible = False
  Align = alClient
  Constraints.MaxHeight = 304
  Constraints.MinHeight = 72
  Constraints.MinWidth = 133
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object cbbCoordType: TComboBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 445
      Height = 21
      Align = alTop
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Geographic Coordinates'
      OnSelect = cbbCoordTypeSelect
      Items.Strings = (
        'Geographic Coordinates'
        'Pixel'
        'Tile')
      ExplicitLeft = 6
      ExplicitWidth = 219
    end
  end
  object grdpnlLonLat: TGridPanel
    Left = 0
    Top = 24
    Width = 451
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    ColumnCollection = <
      item
        SizeStyle = ssAuto
        Value = 35.216377299818610000
      end
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = lblLon
        Row = 1
      end
      item
        Column = 1
        Control = edtLat
        Row = 0
      end
      item
        Column = 1
        Control = edtLon
        Row = 1
      end
      item
        Column = 0
        Control = lblLat
        Row = 0
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 1
    object lblLon: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 27
      Width = 51
      Height = 13
      Align = alRight
      Alignment = taRightJustify
      Caption = 'Longitude:'
      Layout = tlCenter
    end
    object edtLat: TEdit
      AlignWithMargins = True
      Left = 60
      Top = 3
      Width = 388
      Height = 18
      Align = alClient
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
    end
    object edtLon: TEdit
      AlignWithMargins = True
      Left = 60
      Top = 27
      Width = 388
      Height = 18
      Align = alClient
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 1
    end
    object lblLat: TLabel
      AlignWithMargins = True
      Left = 11
      Top = 3
      Width = 43
      Height = 13
      Align = alRight
      Alignment = taRightJustify
      Caption = 'Latitude:'
      Layout = tlCenter
    end
  end
  object pnlXY: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 72
    Width = 451
    Height = 48
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object grdpnlXY: TGridPanel
      Left = 0
      Top = 0
      Width = 393
      Height = 48
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAuto
          Value = 25.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = lblY
          Row = 1
        end
        item
          Column = 1
          Control = edtX
          Row = 0
        end
        item
          Column = 1
          Control = edtY
          Row = 1
        end
        item
          Column = 0
          Control = lblX
          Row = 0
        end>
      RowCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      TabOrder = 0
      object lblY: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 27
        Width = 10
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        Caption = 'Y:'
        Layout = tlCenter
      end
      object edtX: TEdit
        AlignWithMargins = True
        Left = 19
        Top = 3
        Width = 371
        Height = 18
        Align = alClient
        AutoSize = False
        BorderStyle = bsNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
      end
      object edtY: TEdit
        AlignWithMargins = True
        Left = 19
        Top = 27
        Width = 371
        Height = 18
        Align = alClient
        AutoSize = False
        BorderStyle = bsNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
      end
      object lblX: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 10
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        Caption = 'X:'
        Layout = tlCenter
      end
    end
    object grdpnlZoom: TGridPanel
      Left = 393
      Top = 0
      Width = 58
      Height = 48
      Align = alRight
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAuto
          Value = 50.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = lblZoom
          Row = 0
        end
        item
          Column = 1
          Control = cbbZoom
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      DesignSize = (
        58
        48)
      object lblZoom: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 3
        Width = 10
        Height = 42
        Margins.Left = 0
        Align = alLeft
        Caption = 'Z:'
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object cbbZoom: TComboBox
        AlignWithMargins = True
        Left = 16
        Top = 13
        Width = 39
        Height = 21
        Hint = 'Zoom'
        Style = csDropDownList
        Anchors = [akLeft, akTop, akBottom]
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
  object pnlProjected: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 120
    Width = 451
    Height = 48
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    ExplicitTop = 80
    object grdpnlProjected: TGridPanel
      Left = 0
      Top = 0
      Width = 296
      Height = 48
      Align = alClient
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAuto
          Value = 25.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = lbl1
          Row = 1
        end
        item
          Column = 1
          Control = edtProjectedX
          Row = 0
        end
        item
          Column = 1
          Control = edtProjectedY
          Row = 1
        end
        item
          Column = 0
          Control = lbl2
          Row = 0
        end>
      RowCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      TabOrder = 0
      ExplicitLeft = 19
      ExplicitWidth = 374
      object lbl1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 27
        Width = 10
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        Caption = 'Y:'
        Layout = tlCenter
      end
      object edtProjectedX: TEdit
        AlignWithMargins = True
        Left = 19
        Top = 3
        Width = 274
        Height = 18
        Align = alClient
        AutoSize = False
        BorderStyle = bsNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        ExplicitWidth = 371
      end
      object edtProjectedY: TEdit
        AlignWithMargins = True
        Left = 19
        Top = 27
        Width = 274
        Height = 18
        Align = alClient
        AutoSize = False
        BorderStyle = bsNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        ExplicitWidth = 371
      end
      object lbl2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 10
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        Caption = 'X:'
        Layout = tlCenter
      end
    end
    object grdpnlZone: TGridPanel
      Left = 296
      Top = 0
      Width = 155
      Height = 48
      Align = alRight
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 60.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = lblZone
          Row = 0
        end
        item
          Column = 1
          Control = cbbZone
          Row = 0
        end
        item
          Column = 1
          Control = chkNorth
          Row = 1
        end>
      RowCollection = <
        item
          Value = 49.999999656341050000
        end
        item
          Value = 50.000000343658950000
        end>
      TabOrder = 1
      object lblZone: TLabel
        AlignWithMargins = True
        Left = 29
        Top = 6
        Width = 28
        Height = 13
        Margins.Top = 6
        Align = alTop
        Alignment = taRightJustify
        Caption = 'Zone:'
      end
      object cbbZone: TComboBox
        AlignWithMargins = True
        Left = 63
        Top = 3
        Width = 89
        Height = 21
        Align = alTop
        TabOrder = 0
      end
      object chkNorth: TCheckBox
        AlignWithMargins = True
        Left = 63
        Top = 26
        Width = 89
        Height = 20
        Align = alTop
        Caption = 'North'
        TabOrder = 1
        ExplicitLeft = 100
        ExplicitTop = 48
        ExplicitWidth = 145
      end
    end
  end
end
