object frExportToFileCont: TfrExportToFileCont
  Left = 0
  Top = 0
  Width = 552
  Height = 180
  Align = alClient
  TabOrder = 0
  Visible = False
  ExplicitWidth = 451
  ExplicitHeight = 304
  object pnlCenter: TPanel
    Left = 0
    Top = 32
    Width = 552
    Height = 148
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 451
    ExplicitHeight = 272
    object pnlRight: TPanel
      Left = 418
      Top = 0
      Width = 134
      Height = 148
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 317
      ExplicitHeight = 272
      DesignSize = (
        134
        148)
      object lblZooms: TLabel
        Left = 7
        Top = 5
        Width = 57
        Height = 13
        Caption = #1052#1072#1089#1096#1090#1072#1073#1099':'
      end
      object chkAllZooms: TCheckBox
        Left = 23
        Top = 24
        Width = 41
        Height = 17
        Caption = #1042#1089#1077
        TabOrder = 0
        OnClick = chkAllZoomsClick
      end
      object chklstZooms: TCheckListBox
        Left = 70
        Top = 2
        Width = 59
        Height = 139
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 1
        ExplicitHeight = 263
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 418
      Height = 148
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 317
      ExplicitHeight = 272
      DesignSize = (
        418
        148)
      object lblMap: TLabel
        Left = 8
        Top = 5
        Width = 132
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = #1050#1072#1088#1090#1072
        ExplicitWidth = 31
      end
      object lblNamesType: TLabel
        Left = 8
        Top = 49
        Width = 316
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1091#1095#1072#1089#1090#1082#1072' '#1074' '#1092#1086#1088#1084#1072#1090
        ExplicitWidth = 207
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 22
        Width = 412
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 386
      end
      object cbbNamesType: TComboBox
        Left = 3
        Top = 68
        Width = 412
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 1
        Text = 'SAS.'#1055#1083#1072#1085#1077#1090#1072
        Items.Strings = (
          'GoogleMV'
          'SAS.'#1055#1083#1072#1085#1077#1090#1072
          'ES1.95'
          'GMT (GlobalMapper >=10.02)')
        ExplicitWidth = 386
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 552
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 451
    DesignSize = (
      552
      32)
    object lblTargetFile: TLabel
      Left = 8
      Top = 6
      Width = 86
      Height = 13
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
    end
    object edtTargetFile: TEdit
      Left = 100
      Top = 3
      Width = 420
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 526
      Top = 3
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
      ExplicitLeft = 440
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Left = 40
    Top = 128
  end
end
