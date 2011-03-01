object frLonLat: TfrLonLat
  Left = 0
  Top = 0
  Width = 242
  Height = 45
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
    Height = 45
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
      45)
    object lblLat: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 51
      Height = 13
      Align = alClient
      Alignment = taRightJustify
      Caption = #1064#1080#1088#1086#1090#1072':'
      Layout = tlCenter
      ExplicitLeft = 63
      ExplicitWidth = 44
    end
    object flwpnlLat: TFlowPanel
      Left = 60
      Top = 3
      Width = 179
      Height = 19
      Anchors = [akLeft]
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 67
      DesignSize = (
        179
        19)
      object cbbLatNS: TComboBox
        Left = 0
        Top = 0
        Width = 33
        Height = 21
        BevelInner = bvNone
        BevelKind = bkSoft
        BevelOuter = bvNone
        Style = csDropDownList
        Anchors = []
        Ctl3D = False
        ItemHeight = 13
        ItemIndex = 0
        ParentCtl3D = False
        TabOrder = 0
        Text = 'N'
        Items.Strings = (
          'N'
          'S')
      end
      object edtLatDeg: TCurrencyEdit
        Left = 33
        Top = 0
        Width = 48
        Height = 21
        Margins.Top = 0
        AutoSize = False
        BorderStyle = bsNone
        DecimalPlaces = 8
        DisplayFormat = '0.########'#176
        FormatOnEditing = True
        Anchors = []
        MaxValue = 180.000000000000000000
        TabOrder = 1
      end
      object edtLatMin: TCurrencyEdit
        Left = 81
        Top = 0
        Width = 48
        Height = 21
        Margins.Top = 0
        AutoSize = False
        BorderStyle = bsNone
        DecimalPlaces = 5
        DisplayFormat = '0.#####'#8217
        FormatOnEditing = True
        Anchors = []
        MaxValue = 60.000000000000000000
        TabOrder = 2
      end
      object edtLatSec: TCurrencyEdit
        Left = 129
        Top = 0
        Width = 48
        Height = 21
        Margins.Top = 0
        AutoSize = False
        BorderStyle = bsNone
        DecimalPlaces = 4
        DisplayFormat = '0.####'#8221
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Pitch = fpVariable
        Font.Style = []
        FormatOnEditing = True
        Anchors = []
        MaxValue = 60.000000000000000000
        ParentFont = False
        TabOrder = 3
      end
    end
    object lblLon: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 25
      Width = 51
      Height = 14
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
      Top = 22
      Width = 179
      Height = 20
      Anchors = [akLeft]
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 216
      object cbbLonWE: TComboBox
        Left = 0
        Top = 0
        Width = 33
        Height = 21
        BevelInner = bvNone
        BevelKind = bkSoft
        BevelOuter = bvNone
        Style = csDropDownList
        Ctl3D = False
        ItemHeight = 13
        ItemIndex = 0
        ParentCtl3D = False
        TabOrder = 0
        Text = 'E'
        Items.Strings = (
          'E'
          'W')
      end
      object edtLonDeg: TCurrencyEdit
        Left = 33
        Top = 0
        Width = 48
        Height = 21
        Margins.Top = 0
        AutoSize = False
        BorderStyle = bsNone
        DecimalPlaces = 8
        DisplayFormat = '0.########'#176
        FormatOnEditing = True
        MaxValue = 180.000000000000000000
        TabOrder = 1
      end
      object edtLonMin: TCurrencyEdit
        Left = 81
        Top = 0
        Width = 48
        Height = 21
        Margins.Top = 0
        AutoSize = False
        BorderStyle = bsNone
        DecimalPlaces = 5
        DisplayFormat = '0.#####'#8217
        FormatOnEditing = True
        MaxValue = 60.000000000000000000
        TabOrder = 2
      end
      object edtLonSec: TCurrencyEdit
        Left = 129
        Top = 0
        Width = 48
        Height = 21
        Margins.Top = 0
        AutoSize = False
        BorderStyle = bsNone
        DecimalPlaces = 4
        DisplayFormat = '0.####'#8221
        FormatOnEditing = True
        MaxValue = 60.000000000000000000
        TabOrder = 3
      end
    end
  end
end
