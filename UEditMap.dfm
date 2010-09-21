object FEditMap: TFEditMap
  Left = 198
  Top = 305
  BorderStyle = bsDialog
  ClientHeight = 274
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 237
    Width = 556
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    ExplicitTop = 351
    ExplicitWidth = 586
    object Button3: TButton
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 105
      Height = 25
      Align = alLeft
      Caption = #1042#1089#1077' '#1087#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
      TabOrder = 0
      OnClick = Button3Click
      ExplicitLeft = 37
      ExplicitTop = 16
    end
    object Button1: TButton
      AlignWithMargins = True
      Left = 394
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Caption = #1055#1088#1080#1085#1103#1090#1100
      ModalResult = 1
      TabOrder = 1
      OnClick = Button1Click
      ExplicitLeft = 280
      ExplicitTop = 16
    end
    object Button2: TButton
      AlignWithMargins = True
      Left = 475
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 2
      OnClick = Button2Click
      ExplicitLeft = 360
      ExplicitTop = 16
    end
  end
  object pnlSeparator: TPanel
    Left = 0
    Top = 206
    Width = 556
    Height = 31
    Align = alBottom
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitTop = 320
    ExplicitWidth = 586
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 544
      Height = 17
      Align = alTop
      Caption = #1044#1086#1073#1072#1074#1083#1103#1090#1100' '#1074' '#1084#1077#1085#1102' '#1088#1072#1079#1076#1077#1083#1080#1090#1077#1083#1100' '#1087#1086#1089#1083#1077' '#1085#1072#1079#1074#1072#1085#1080#1103' '#1101#1090#1086#1081' '#1082#1072#1088#1090#1099
      TabOrder = 0
      ExplicitLeft = 8
      ExplicitTop = 24
      ExplicitWidth = 329
    end
  end
  object pnlCacheType: TPanel
    Left = 0
    Top = 173
    Width = 556
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    ExplicitTop = 287
    ExplicitWidth = 586
    object Label5: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 48
      Height = 21
      Align = alLeft
      Caption = #1058#1080#1087' '#1082#1101#1096#1072
      Layout = tlCenter
      ExplicitLeft = 8
      ExplicitTop = 28
      ExplicitHeight = 13
    end
    object CBCacheType: TComboBox
      AlignWithMargins = True
      Left = 60
      Top = 6
      Width = 463
      Height = 21
      Align = alClient
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        #1055#1086' '#1091#1084#1086#1083#1095#1072#1085#1080#1102
        'GoogleMV'
        'SAS.'#1055#1083#1072#1085#1077#1090#1072
        'EarthSlicer 1.95'
        'Googe maps tiles'
        'Google Earth')
      ExplicitLeft = 72
      ExplicitTop = 14
      ExplicitWidth = 329
    end
    object Button9: TButton
      AlignWithMargins = True
      Left = 529
      Top = 6
      Width = 21
      Height = 21
      Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
      Align = alRight
      Caption = '<>'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = Button9Click
      ExplicitLeft = 424
      ExplicitTop = 14
    end
  end
  object pnlParentItem: TPanel
    Left = 0
    Top = 140
    Width = 556
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 3
    ExplicitTop = 182
    ExplicitWidth = 531
    object Label3: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 134
      Height = 21
      Align = alLeft
      Caption = #1056#1086#1076#1080#1090#1077#1083#1100#1089#1082#1080#1081' '#1087#1091#1085#1082#1090' '#1084#1077#1085#1102
      Layout = tlCenter
      ExplicitLeft = 8
      ExplicitTop = 20
      ExplicitHeight = 13
    end
    object EditParSubMenu: TEdit
      AlignWithMargins = True
      Left = 146
      Top = 6
      Width = 377
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 152
      ExplicitTop = 12
      ExplicitWidth = 257
    end
    object Button5: TButton
      AlignWithMargins = True
      Left = 529
      Top = 6
      Width = 21
      Height = 21
      Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
      Align = alRight
      Caption = '<>'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = Button5Click
      ExplicitLeft = 413
      ExplicitTop = 12
    end
  end
  object pnlCacheName: TPanel
    Left = 0
    Top = 107
    Width = 556
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 4
    ExplicitTop = 149
    ExplicitWidth = 531
    object Label2: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 88
      Height = 21
      Align = alLeft
      Caption = #1053#1072#1079#1074#1072#1085#1080#1077' '#1074' '#1082#1101#1096#1077
      Layout = tlCenter
      ExplicitLeft = 8
      ExplicitTop = 20
      ExplicitHeight = 13
    end
    object EditNameinCache: TEdit
      AlignWithMargins = True
      Left = 100
      Top = 6
      Width = 423
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 104
      ExplicitTop = 12
      ExplicitWidth = 305
    end
    object Button4: TButton
      AlignWithMargins = True
      Left = 529
      Top = 6
      Width = 21
      Height = 21
      Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
      Align = alRight
      Caption = '<>'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = Button4Click
      ExplicitLeft = 413
      ExplicitTop = 12
    end
  end
  object pnlUrl: TPanel
    Left = 0
    Top = 0
    Width = 556
    Height = 74
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 5
    ExplicitLeft = 24
    ExplicitTop = 21
    ExplicitHeight = 61
    object Label1: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 22
      Height = 62
      Align = alLeft
      Caption = 'URL'
      ExplicitLeft = 8
      ExplicitTop = 11
      ExplicitHeight = 13
    end
    object pnlUrlRight: TPanel
      Left = 526
      Top = 3
      Width = 27
      Height = 68
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 55
      object Button6: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 21
        Height = 21
        Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
        Align = alTop
        Caption = '<>'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = Button6Click
        ExplicitLeft = 12
        ExplicitTop = 8
      end
    end
    object EditURL: TMemo
      AlignWithMargins = True
      Left = 34
      Top = 6
      Width = 489
      Height = 62
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
      WantReturns = False
      ExplicitLeft = 112
      ExplicitWidth = 369
      ExplicitHeight = 49
    end
  end
  object grdpnlSleepAndKey: TGridPanel
    Left = 0
    Top = 74
    Width = 556
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
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
        Control = grdpnlHotKey
        Row = 0
      end
      item
        Column = 1
        Control = grdpnlSleep
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 6
    ExplicitLeft = 3
    ExplicitTop = 25
    ExplicitWidth = 550
    object grdpnlHotKey: TGridPanel
      Left = 3
      Top = 3
      Width = 201
      Height = 25
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAuto
          Value = 50.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 27.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Label4
          Row = 0
        end
        item
          Column = 1
          Control = EditHotKey
          Row = 0
        end
        item
          Column = 2
          Control = Button7
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      DesignSize = (
        201
        25)
      object Label4: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 59
        Height = 13
        Caption = #1043#1086#1088#1103#1095#1072#1103' '#1082#1083'.'
      end
      object EditHotKey: THotKey
        Left = 67
        Top = 2
        Width = 105
        Height = 21
        Anchors = []
        HotKey = 0
        Modifiers = []
        TabOrder = 0
      end
      object Button7: TButton
        AlignWithMargins = True
        Left = 177
        Top = 3
        Width = 21
        Height = 19
        Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
        Caption = '<>'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = Button7Click
      end
    end
    object grdpnlSleep: TGridPanel
      Left = 400
      Top = 3
      Width = 153
      Height = 27
      Align = alRight
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAuto
          Value = 50.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 27.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Label6
          Row = 0
        end
        item
          Column = 1
          Control = SESleep
          Row = 0
        end
        item
          Column = 2
          Control = Button8
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      ExplicitLeft = 394
      DesignSize = (
        153
        27)
      object Label6: TLabel
        Left = 0
        Top = 7
        Width = 31
        Height = 13
        Anchors = []
        Caption = #1055#1072#1091#1079#1072
        ExplicitLeft = 272
        ExplicitTop = 131
      end
      object SESleep: TSpinEdit
        AlignWithMargins = True
        Left = 34
        Top = 3
        Width = 89
        Height = 22
        Anchors = []
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object Button8: TButton
        AlignWithMargins = True
        Left = 129
        Top = 3
        Width = 21
        Height = 21
        Hint = #1055#1086' '#1091#1084#1086#1083#1095#1085#1080#1102
        Anchors = []
        Caption = '<>'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = Button8Click
      end
    end
  end
end
