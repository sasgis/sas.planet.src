object frLonLat: TfrLonLat
  Left = 0
  Top = 0
  Width = 133
  Height = 72
  VertScrollBar.Visible = False
  Align = alClient
  Constraints.MaxHeight = 304
  Constraints.MinHeight = 72
  Constraints.MinWidth = 133
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  ExplicitWidth = 451
  ExplicitHeight = 304
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 133
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 451
    object cbbCoordType: TComboBox
      Left = 6
      Top = 3
      Width = 123
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = 'Geographic'
      OnSelect = cbbCoordTypeSelect
      Items.Strings = (
        'Geographic'
        'Pixel'
        'Tile')
    end
  end
  object grdpnlLonLat: TGridPanel
    Left = 0
    Top = 24
    Width = 133
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
    ExplicitWidth = 451
    object lblLon: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 27
      Width = 51
      Height = 18
      Align = alRight
      Alignment = taRightJustify
      Caption = 'Longitude:'
      Layout = tlCenter
      ExplicitTop = 24
      ExplicitHeight = 13
    end
    object edtLat: TEdit
      AlignWithMargins = True
      Left = 60
      Top = 3
      Width = 70
      Height = 18
      Align = alClient
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
      ExplicitWidth = 388
    end
    object edtLon: TEdit
      AlignWithMargins = True
      Left = 60
      Top = 27
      Width = 70
      Height = 18
      Align = alClient
      AutoSize = False
      BorderStyle = bsNone
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 1
      ExplicitWidth = 388
    end
    object lblLat: TLabel
      AlignWithMargins = True
      Left = 11
      Top = 3
      Width = 43
      Height = 18
      Align = alRight
      Alignment = taRightJustify
      Caption = 'Latitude:'
      Layout = tlCenter
      ExplicitTop = 0
      ExplicitHeight = 13
    end
  end
  object pnlXY: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 72
    Width = 133
    Height = 48
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    ExplicitWidth = 451
    object pnlZoom: TPanel
      Left = 83
      Top = 0
      Width = 50
      Height = 48
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 401
      DesignSize = (
        50
        48)
      object lblZoom: TLabel
        Left = 0
        Top = 16
        Width = 10
        Height = 13
        Margins.Left = 0
        Anchors = [akTop, akRight]
        Caption = 'Z:'
      end
      object cbbZoom: TComboBox
        Left = 10
        Top = 13
        Width = 36
        Height = 21
        Hint = 'Zoom'
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
    object grdpnlXY: TGridPanel
      Left = 0
      Top = 0
      Width = 83
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
      TabOrder = 1
      ExplicitWidth = 401
      object lblY: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 27
        Width = 19
        Height = 18
        Align = alRight
        Alignment = taRightJustify
        Caption = 'Y:'
        Layout = tlCenter
        ExplicitLeft = 12
        ExplicitTop = 24
        ExplicitWidth = 10
        ExplicitHeight = 13
      end
      object edtX: TEdit
        AlignWithMargins = True
        Left = 28
        Top = 3
        Width = 52
        Height = 18
        Align = alClient
        AutoSize = False
        BorderStyle = bsNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 0
        ExplicitWidth = 370
      end
      object edtY: TEdit
        AlignWithMargins = True
        Left = 28
        Top = 27
        Width = 52
        Height = 18
        Align = alClient
        AutoSize = False
        BorderStyle = bsNone
        Ctl3D = False
        ParentCtl3D = False
        TabOrder = 1
        ExplicitWidth = 370
      end
      object lblX: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 19
        Height = 18
        Align = alRight
        Alignment = taRightJustify
        Caption = 'X:'
        Layout = tlCenter
        ExplicitLeft = 12
        ExplicitTop = 0
        ExplicitWidth = 10
        ExplicitHeight = 13
      end
    end
  end
end
