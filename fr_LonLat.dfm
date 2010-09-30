object frLonLat: TfrLonLat
  Left = 0
  Top = 0
  Width = 260
  Height = 44
  Constraints.MinHeight = 44
  Constraints.MinWidth = 260
  TabOrder = 0
  object grdpnlMain: TGridPanel
    Left = 0
    Top = 0
    Width = 260
    Height = 44
    Align = alClient
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
    ExplicitWidth = 231
    ExplicitHeight = 47
    DesignSize = (
      260
      44)
    object lblLat: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 4
      Width = 41
      Height = 13
      Alignment = taRightJustify
      Anchors = []
      Caption = #1064#1080#1088#1086#1090#1072':'
      Color = clBtnFace
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitLeft = 3
      ExplicitTop = 3
    end
    object flwpnlLat: TFlowPanel
      Left = 67
      Top = 0
      Width = 177
      Height = 21
      Anchors = []
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 103
      ExplicitTop = 1
      DesignSize = (
        177
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
      Left = 3
      Top = 26
      Width = 46
      Height = 13
      Alignment = taRightJustify
      Anchors = []
      Caption = #1044#1086#1083#1075#1086#1090#1072':'
      Color = clBtnFace
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      ExplicitTop = 3
    end
    object flwpnlLon: TFlowPanel
      Left = 67
      Top = 22
      Width = 177
      Height = 21
      Anchors = []
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 18
      ExplicitTop = 137
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
