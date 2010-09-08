object frExportAUX: TfrExportAUX
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 32
    Width = 451
    Height = 272
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 317
      Height = 272
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        317
        272)
      object lblMap: TLabel
        Left = 1
        Top = 5
        Width = 31
        Height = 13
        Caption = #1050#1072#1088#1090#1072
      end
      object cbbMap: TComboBox
        Left = 44
        Top = 2
        Width = 267
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pnlRight: TPanel
      Left = 317
      Top = 0
      Width = 134
      Height = 272
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object lblZoom: TLabel
        Left = 7
        Top = 5
        Width = 49
        Height = 13
        Caption = #1052#1072#1089#1096#1090#1072#1073':'
      end
      object cbbZoom: TComboBox
        Left = 62
        Top = 2
        Width = 51
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      451
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
      Width = 324
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 426
      Top = 3
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object dlgTargetFileSelect: TSaveDialog
    DefaultExt = 'aux'
    Filter = 'AUX |*.aux'
    Left = 312
    Top = 64
  end
end
