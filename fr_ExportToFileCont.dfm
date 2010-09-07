object frExportToFileCont: TfrExportToFileCont
  Left = 0
  Top = 0
  Width = 485
  Height = 285
  Align = alClient
  TabOrder = 0
  Visible = False
  ExplicitWidth = 435
  ExplicitHeight = 435
  object pnlCenter: TPanel
    Left = 0
    Top = 32
    Width = 485
    Height = 253
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 28
    ExplicitWidth = 451
    ExplicitHeight = 276
    object pnlRight: TPanel
      Left = 351
      Top = 0
      Width = 134
      Height = 253
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 415
      ExplicitHeight = 329
      DesignSize = (
        134
        253)
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
        Width = 57
        Height = 243
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 1
        ExplicitHeight = 319
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 351
      Height = 253
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 64
      ExplicitTop = 2
      ExplicitWidth = 615
      ExplicitHeight = 330
      DesignSize = (
        351
        253)
      object lblMap: TLabel
        Left = 8
        Top = 5
        Width = 337
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = #1050#1072#1088#1090#1072
        ExplicitWidth = 601
      end
      object lblNamesType: TLabel
        Left = 8
        Top = 49
        Width = 337
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1076#1077#1083#1077#1085#1085#1086#1075#1086' '#1091#1095#1072#1089#1090#1082#1072' '#1074' '#1092#1086#1088#1084#1072#1090
        ExplicitWidth = 601
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 22
        Width = 342
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        ExplicitWidth = 606
      end
      object cbbNamesType: TComboBox
        Left = 3
        Top = 68
        Width = 342
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
        ExplicitWidth = 606
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 485
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 435
    DesignSize = (
      485
      32)
    object lblTargetFile: TLabel
      Left = 8
      Top = 6
      Width = 86
      Height = 13
      Caption = #1050#1091#1076#1072' '#1089#1086#1093#1088#1072#1085#1103#1090#1100':'
    end
    object edtTargetFile: TEdit
      Left = 96
      Top = 3
      Width = 358
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 422
    end
    object btnSelectTargetFile: TButton
      Left = 460
      Top = 3
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
      ExplicitLeft = 524
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Left = 40
    Top = 128
  end
end
