object FaddPoint: TFaddPoint
  Left = 193
  Top = 178
  BorderStyle = bsSizeToolWin
  Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1085#1086#1074#1091#1102' '#1084#1077#1090#1082#1091
  ClientHeight = 404
  ClientWidth = 314
  Color = clBtnFace
  Constraints.MinHeight = 428
  Constraints.MinWidth = 322
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
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
  object chkVisible: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 353
    Width = 308
    Height = 17
    Align = alBottom
    Caption = #1054#1090#1086#1073#1088#1072#1078#1072#1090#1100' '#1101#1090#1086' '#1084#1077#1089#1090#1086' '#1085#1072' '#1082#1072#1088#1090#1077
    TabOrder = 5
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 373
    Width = 314
    Height = 31
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 6
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 238
      Top = 3
      Width = 73
      Height = 23
      Hint = #1054#1090#1084#1077#1085#1080#1090#1100
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 1
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 159
      Top = 3
      Width = 73
      Height = 23
      Align = alRight
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      Default = True
      TabOrder = 0
      OnClick = btnOkClick
    end
  end
  object grdpnlStyleRows: TGridPanel
    Left = 0
    Top = 259
    Width = 314
    Height = 91
    Align = alBottom
    BevelEdges = [beBottom]
    BevelKind = bkTile
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
      89)
    object grdpnlLine1: TGridPanel
      Left = 0
      Top = 0
      Width = 314
      Height = 29
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
        29)
      object flwpnlTextColor: TFlowPanel
        Left = 16
        Top = 3
        Width = 125
        Height = 22
        Anchors = []
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        TabOrder = 0
        object lblTextColor: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 64
          Height = 13
          Alignment = taRightJustify
          Caption = #1062#1074#1077#1090' '#1090#1077#1082#1089#1090#1072
        end
        object clrbxTextColor: TColorBox
          Left = 70
          Top = 0
          Width = 38
          Height = 22
          Selected = clYellow
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
        end
        object btnTextColor: TSpeedButton
          Left = 108
          Top = 0
          Width = 17
          Height = 22
          Caption = '...'
          OnClick = btnTextColorClick
        end
      end
      object flwpnlFontSize: TFlowPanel
        Left = 173
        Top = 3
        Width = 125
        Height = 22
        Anchors = []
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        TabOrder = 1
        object lblFontSize: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 78
          Height = 13
          Alignment = taRightJustify
          Caption = #1056#1072#1079#1084#1077#1088' '#1096#1088#1080#1092#1090#1072
        end
        object seFontSize: TSpinEdit
          Left = 84
          Top = 0
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
      Top = 29
      Width = 314
      Height = 29
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
        29)
      object flwpnlShadowColor: TFlowPanel
        Left = 21
        Top = 3
        Width = 114
        Height = 22
        Anchors = []
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        TabOrder = 0
        object lblShadowColor: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 53
          Height = 13
          Alignment = taRightJustify
          Caption = #1062#1074#1077#1090' '#1090#1077#1085#1080
        end
        object clrbxShadowColor: TColorBox
          Left = 59
          Top = 0
          Width = 38
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
        end
        object btnShadowColor: TSpeedButton
          Left = 97
          Top = 0
          Width = 17
          Height = 22
          Caption = '...'
          OnClick = btnShadowColorClick
        end
      end
      object flwpnlIconSize: TFlowPanel
        Left = 175
        Top = 3
        Width = 121
        Height = 22
        Anchors = []
        AutoSize = True
        AutoWrap = False
        BevelOuter = bvNone
        TabOrder = 1
        object lblIconSize: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 74
          Height = 13
          Alignment = taRightJustify
          Caption = #1056#1072#1079#1084#1077#1088' '#1080#1082#1086#1085#1082#1080
        end
        object seIconSize: TSpinEdit
          Left = 80
          Top = 0
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
      Left = 91
      Top = 58
      Width = 132
      Height = 22
      Anchors = []
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 2
      object lblTransp: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 85
        Height = 13
        Alignment = taRightJustify
        Caption = #1055#1088#1086#1079#1088#1072#1095#1085#1086#1089#1090#1100' %'
      end
      object seTransp: TSpinEdit
        Left = 91
        Top = 0
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
    Height = 144
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
    TabOrder = 1
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
      object imgIcon: TImage
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 45
        Height = 45
        Cursor = crHandPoint
        Align = alClient
        OnMouseDown = imgIconMouseDown
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
        object lblCategory: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 58
          Height = 20
          Align = alLeft
          Caption = #1050#1072#1090#1077#1075#1086#1088#1080#1103':'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object CBKateg: TComboBox
          AlignWithMargins = True
          Left = 67
          Top = 3
          Width = 185
          Height = 21
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
          Text = #1053#1086#1074#1072#1103' '#1082#1072#1090#1077#1075#1086#1088#1080#1103
          ExplicitTop = 2
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
        object lblName: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 23
          Height = 21
          Align = alLeft
          Caption = #1048#1084#1103':'
          Layout = tlCenter
          ExplicitHeight = 13
        end
        object edtName: TEdit
          AlignWithMargins = True
          Left = 32
          Top = 3
          Width = 220
          Height = 21
          Align = alClient
          TabOrder = 0
        end
      end
    end
  end
  object drwgrdIcons: TDrawGrid
    Left = 3
    Top = 58
    Width = 310
    Height = 185
    ColCount = 8
    Ctl3D = False
    DefaultColWidth = 36
    DefaultRowHeight = 36
    FixedCols = 0
    FixedRows = 0
    GridLineWidth = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goThumbTracking]
    ParentCtl3D = False
    TabOrder = 2
    Visible = False
    OnDrawCell = drwgrdIconsDrawCell
    OnMouseUp = drwgrdIconsMouseUp
  end
  object ColorDialog1: TColorDialog
    Left = 56
    Top = 472
  end
end
