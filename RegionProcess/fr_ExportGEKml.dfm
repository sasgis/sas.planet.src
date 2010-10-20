object frExportGEKml: TfrExportGEKml
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
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
      Left = 376
      Top = 0
      Width = 75
      Height = 277
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblZooms: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 69
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
        ExplicitWidth = 57
      end
      object chkAllZooms: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 257
        Width = 69
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 0
        OnClick = chkAllZoomsClick
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 19
        Width = 69
        Height = 235
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        ExplicitTop = 16
        ExplicitHeight = 241
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 277
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitLeft = -3
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 370
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = #1050#1072#1088#1090#1072
        ExplicitLeft = 12
        ExplicitTop = 6
        ExplicitWidth = 31
      end
      object chkNotSaveNotExists: TCheckBox
        Left = 3
        Top = 60
        Width = 370
        Height = 17
        Align = alTop
        Caption = #1053#1077' '#1089#1086#1093#1088#1072#1085#1103#1090#1100' '#1087#1091#1090#1080' '#1082' '#1085#1077#1089#1091#1097#1077#1089#1090#1074#1091#1102#1097#1080#1084' '#1090#1072#1081#1083#1072#1084
        TabOrder = 0
        ExplicitTop = 66
      end
      object chkUseRelativePath: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 43
        Width = 370
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = #1054#1090#1085#1086#1089#1080#1090#1077#1083#1100#1085#1099#1081' '#1087#1091#1090#1100' '#1082' '#1090#1072#1081#1083#1072#1084
        Checked = True
        State = cbChecked
        TabOrder = 1
        ExplicitTop = 37
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 19
        Width = 370
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 2
        ExplicitTop = 16
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
    object lblTargetFile: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 86
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitLeft = 6
      ExplicitTop = 6
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 92
      Top = 3
      Width = 335
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 89
      ExplicitWidth = 338
    end
    object btnSelectTargetFile: TButton
      Left = 427
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object dlgSaveKML: TSaveDialog
    DefaultExt = 'kml'
    Filter = 'KML |*.kml'
    Left = 184
    Top = 136
  end
end
