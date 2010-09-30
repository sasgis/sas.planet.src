object FaddPoint: TFaddPoint
  Left = 193
  Top = 178
  BorderStyle = bsSizeToolWin
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1091#1102' '#1084#1077#1090#1082#1091
  ClientHeight = 428
  ClientWidth = 314
  Color = clBtnFace
  Constraints.MinHeight = 452
  Constraints.MinWidth = 322
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 0
    Top = 59
    Width = 314
    Height = 9
    Align = alTop
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 96
    ExplicitWidth = 329
  end
  object Bevel2: TBevel
    Left = 0
    Top = 356
    Width = 314
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 336
    ExplicitWidth = 329
  end
  object Bevel5: TBevel
    Left = 0
    Top = 388
    Width = 314
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
    ExplicitLeft = 8
    ExplicitTop = 360
    ExplicitWidth = 329
  end
  object CheckBox2: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 368
    Width = 308
    Height = 17
    Align = alBottom
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 5
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 397
    Width = 314
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
    object Button2: TButton
      AlignWithMargins = True
      Left = 238
      Top = 3
      Width = 73
      Height = 25
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 1
    end
    object Badd: TButton
      AlignWithMargins = True
      Left = 159
      Top = 3
      Width = 73
      Height = 25
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      Default = True
      TabOrder = 0
      OnClick = BaddClick
    end
  end
  object grdpnlStyleRows: TGridPanel
    Left = 0
    Top = 243
    Width = 314
    Height = 113
    Align = alBottom
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = grdpnlLine1
        Row = 0
      end
      item
        Column = 0
        Control = grdpnlLine2
        Row = 1
      end
      item
        Column = 0
        Control = flwpnlTrahsparent
        Row = 2
      end>
    RowCollection = <
      item
        SizeStyle = ssAuto
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAuto
        Value = 50.000000000000000000
      end
      item
        SizeStyle = ssAuto
        Value = 100.000000000000000000
      end>
    TabOrder = 4
    DesignSize = (
      314
      113)
    object grdpnlLine1: TGridPanel
      Left = 0
      Top = 0
      Width = 314
      Height = 41
      Align = alBottom
      Anchors = []
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = flwpnlTextColor
          Row = 0
        end
        item
          Column = 1
          Control = flwpnlFontSize
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      DesignSize = (
        314
        41)
      object flwpnlTextColor: TFlowPanel
        Left = 11
        Top = 6
        Width = 135
        Height = 28
        Anchors = []
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        TabOrder = 0
        object Label3: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 62
          Height = 13
          Alignment = taRightJustify
          Caption = #1062#1074#1077#1090' '#1090#1077#1082#1089#1090#1072
        end
        object ColorBox1: TColorBox
          AlignWithMargins = True
          Left = 71
          Top = 3
          Width = 38
          Height = 22
          Selected = clYellow
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 16
          ParentFont = False
          TabOrder = 0
        end
        object SpeedButton1: TSpeedButton
          AlignWithMargins = True
          Left = 115
          Top = 3
          Width = 17
          Height = 22
          Caption = '...'
          OnClick = SpeedButton1Click
        end
      end
      object flwpnlFontSize: TFlowPanel
        Left = 168
        Top = 6
        Width = 134
        Height = 28
        Anchors = []
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        TabOrder = 1
        object Label5: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 81
          Height = 13
          Alignment = taRightJustify
          Caption = #1056#1072#1079#1084#1077#1088' '#1096#1088#1080#1092#1090#1072
        end
        object SpinEdit1: TSpinEdit
          AlignWithMargins = True
          Left = 90
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 24
          MinValue = 0
          TabOrder = 0
          Value = 11
        end
      end
    end
    object grdpnlLine2: TGridPanel
      Left = 0
      Top = 41
      Width = 314
      Height = 41
      Align = alBottom
      Anchors = []
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = flwpnlShadowColor
          Row = 0
        end
        item
          Column = 1
          Control = flwpnlIconSize
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      DesignSize = (
        314
        41)
      object flwpnlShadowColor: TFlowPanel
        Left = 16
        Top = 6
        Width = 124
        Height = 28
        Anchors = []
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        TabOrder = 0
        object Label4: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 51
          Height = 13
          Alignment = taRightJustify
          Caption = #1062#1074#1077#1090' '#1090#1077#1085#1080
        end
        object ColorBox2: TColorBox
          AlignWithMargins = True
          Left = 60
          Top = 3
          Width = 38
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindow
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ItemHeight = 16
          ParentFont = False
          TabOrder = 0
        end
        object SpeedButton2: TSpeedButton
          AlignWithMargins = True
          Left = 104
          Top = 3
          Width = 17
          Height = 22
          Caption = '...'
          OnClick = SpeedButton2Click
        end
      end
      object flwpnlIconSize: TFlowPanel
        Left = 170
        Top = 6
        Width = 131
        Height = 28
        Anchors = []
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        TabOrder = 1
        object Label6: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 78
          Height = 13
          Alignment = taRightJustify
          Caption = #1056#1072#1079#1084#1077#1088' '#1080#1082#1086#1085#1082#1080
        end
        object SpinEdit2: TSpinEdit
          AlignWithMargins = True
          Left = 87
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 64
          MinValue = 1
          TabOrder = 0
          Value = 32
        end
      end
    end
    object flwpnlTrahsparent: TFlowPanel
      Left = 89
      Top = 82
      Width = 136
      Height = 28
      Anchors = []
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 2
      object Label7: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 83
        Height = 13
        Alignment = taRightJustify
        Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
      end
      object SEtransp: TSpinEdit
        AlignWithMargins = True
        Left = 92
        Top = 3
        Width = 41
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 0
        Value = 35
      end
    end
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 115
    Width = 314
    Height = 128
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 3
  end
  object pnlLonLat: TPanel
    Left = 0
    Top = 68
    Width = 314
    Height = 47
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 32
    ExplicitTop = 62
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 314
    Height = 59
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object pnlImage: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 53
      Height = 53
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 1
      object Image1: TImage
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 45
        Height = 45
        Cursor = crHandPoint
        Align = alClient
        OnMouseDown = Image1MouseDown
        ExplicitLeft = 10
        ExplicitTop = 14
      end
    end
    object pnlTopMain: TPanel
      Left = 59
      Top = 0
      Width = 255
      Height = 59
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnlCategory: TPanel
        Left = 0
        Top = 0
        Width = 255
        Height = 26
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label8: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 56
          Height = 20
          Align = alLeft
          Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object CBKateg: TComboBox
          AlignWithMargins = True
          Left = 65
          Top = 3
          Width = 187
          Height = 21
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          Text = #1053#1086#1074#1072#1103' '#1082#1072#1090#1077#1075#1086#1088#1080#1103
        end
      end
      object pnlName: TPanel
        Left = 0
        Top = 26
        Width = 255
        Height = 27
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label1: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 25
          Height = 21
          Align = alLeft
          Caption = #1048#1084#1103':'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object EditName: TEdit
          AlignWithMargins = True
          Left = 34
          Top = 3
          Width = 218
          Height = 21
          Align = alClient
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
        end
      end
    end
  end
  object DrawGrid1: TDrawGrid
    Left = 3
    Top = 58
    Width = 310
    Height = 182
    ColCount = 8
    Ctl3D = False
    DefaultColWidth = 36
    DefaultRowHeight = 36
    FixedCols = 0
    FixedRows = 0
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goThumbTracking]
    ParentCtl3D = False
    TabOrder = 1
    Visible = False
    OnDrawCell = DrawGrid1DrawCell
    OnMouseUp = DrawGrid1MouseUp
  end
  object ColorDialog1: TColorDialog
    Left = 56
    Top = 472
  end
end
