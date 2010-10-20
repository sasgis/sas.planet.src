object frLonLat: TfrLonLat
  Left = 0
  Top = 0
  Width = 288
  Height = 63
  VertScrollBar.Visible = False
  Align = alClient
  TabOrder = 0
  ExplicitWidth = 451
  ExplicitHeight = 304
  object grdpnlMain: TGridPanel
    Left = 0
    Top = 0
    Width = 288
    Height = 63
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    ColumnCollection = <
      item
        Value = 24.242619197679670000
      end
      item
        Value = 75.757380802320350000
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
      288
      63)
    object lblLat: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 62
      Height = 22
      Align = alClient
      Alignment = taRightJustify
      Caption = #1064#1080#1088#1086#1090#1072':'
      Color = clBtnFace
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 181
      ExplicitWidth = 41
      ExplicitHeight = 13
    end
    object flwpnlLat: TFlowPanel
      Left = 71
      Top = 6
      Width = 214
      Height = 21
      Anchors = [akLeft]
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 50
      DesignSize = (
        214
        21)
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
        BiDiMode = bdLeftToRight
        Ctl3D = False
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 13
        ItemIndex = 0
        ParentBiDiMode = False
        ParentCtl3D = False
        ParentFont = False
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
        Ctl3D = True
        DecimalPlaces = 8
        DisplayFormat = '0.########'#176
        FormatOnEditing = True
        Anchors = []
        MaxValue = 180.000000000000000000
        ParentCtl3D = False
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
        Ctl3D = True
        DecimalPlaces = 5
        DisplayFormat = '0.#####`'
        FormatOnEditing = True
        Anchors = []
        MaxValue = 60.000000000000000000
        ParentCtl3D = False
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
        Ctl3D = True
        DecimalPlaces = 4
        DisplayFormat = '0.####``'
        FormatOnEditing = True
        Anchors = []
        MaxValue = 60.000000000000000000
        ParentCtl3D = False
        TabOrder = 3
      end
    end
    object lblLon: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 34
      Width = 62
      Height = 23
      Align = alClient
      Alignment = taRightJustify
      Caption = #1044#1086#1083#1075#1086#1090#1072':'
      Color = clBtnFace
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 176
      ExplicitTop = 155
      ExplicitWidth = 46
      ExplicitHeight = 13
    end
    object flwpnlLon: TFlowPanel
      Left = 71
      Top = 35
      Width = 214
      Height = 21
      Anchors = [akLeft]
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 178
      object cbbLonWE: TComboBox
        Left = 0
        Top = 0
        Width = 33
        Height = 21
        BevelInner = bvNone
        BevelKind = bkSoft
        BevelOuter = bvNone
        Style = csDropDownList
        BiDiMode = bdLeftToRight
        Ctl3D = False
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 13
        ItemIndex = 0
        ParentBiDiMode = False
        ParentCtl3D = False
        ParentFont = False
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
        Ctl3D = True
        DecimalPlaces = 8
        DisplayFormat = '0.########'#176
        FormatOnEditing = True
        MaxValue = 180.000000000000000000
        ParentCtl3D = False
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
        Ctl3D = True
        DecimalPlaces = 5
        DisplayFormat = '0.#####`'
        FormatOnEditing = True
        MaxValue = 60.000000000000000000
        ParentCtl3D = False
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
        Ctl3D = True
        DecimalPlaces = 4
        DisplayFormat = '0.####``'
        FormatOnEditing = True
        MaxValue = 60.000000000000000000
        ParentCtl3D = False
        TabOrder = 3
      end
    end
  end
end
