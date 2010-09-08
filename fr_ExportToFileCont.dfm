object frExportToFileCont: TfrExportToFileCont
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  Visible = False
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
        Left = 3
        Top = 3
        Width = 69
        Height = 13
        Align = alTop
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
        ExplicitLeft = 7
        ExplicitTop = 5
        ExplicitWidth = 57
      end
      object chkAllZooms: TCheckBox
        Left = 3
        Top = 257
        Width = 69
        Height = 17
        Align = alBottom
        Caption = #1042#1089#1077
        TabOrder = 0
        OnClick = chkAllZoomsClick
        ExplicitLeft = 23
        ExplicitTop = 24
        ExplicitWidth = 41
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 16
        Width = 69
        Height = 241
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        ExplicitLeft = 32
        ExplicitTop = 37
        ExplicitWidth = 59
        ExplicitHeight = 210
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
      ExplicitWidth = 317
      object lblMap: TLabel
        Left = 3
        Top = 3
        Width = 370
        Height = 13
        Align = alTop
        Caption = #1050#1072#1088#1090#1072
        ExplicitWidth = 31
      end
      object lblNamesType: TLabel
        Left = 3
        Top = 37
        Width = 370
        Height = 13
        Align = alTop
        Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1091#1095#1072#1089#1090#1082#1072' '#1074' '#1092#1086#1088#1084#1072#1090
        ExplicitWidth = 207
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 16
        Width = 370
        Height = 21
        Align = alTop
        Style = csDropDownList
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 311
      end
      object cbbNamesType: TComboBox
        Left = 3
        Top = 50
        Width = 370
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 1
        Text = 'SAS.'#1055#1083#1072#1085#1077#1090#1072
        Items.Strings = (
          'GoogleMV'
          'SAS.'#1055#1083#1072#1085#1077#1090#1072
          'ES1.95'
          'GMT (GlobalMapper >=10.02)')
        ExplicitWidth = 311
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
      Left = 3
      Top = 3
      Width = 86
      Height = 21
      Align = alLeft
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 89
      Top = 3
      Width = 338
      Height = 21
      Align = alClient
      TabOrder = 0
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
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Left = 40
    Top = 128
  end
end
