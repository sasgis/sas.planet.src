object frExportRMapsSQLite: TfrExportRMapsSQLite
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 451
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 368
      Height = 277
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 362
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Map'
        ExplicitWidth = 370
      end
      object chkReplaceExistingTiles: TCheckBox
        Left = 3
        Top = 62
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Replace existing tiles'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkForceDropTarget: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 45
        Width = 362
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Recreate target database if exists'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object pnlMap: TPanel
        Left = 3
        Top = 19
        Width = 362
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
      end
    end
    object PnlZoom: TPanel
      Left = 368
      Top = 0
      Width = 83
      Height = 277
      Align = alRight
      BevelOuter = bvNone
      BiDiMode = bdLeftToRight
      ParentBiDiMode = False
      TabOrder = 1
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
      Width = 41
      Height = 18
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 380
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
  object dlgSaveSQLite: TSaveDialog
    DefaultExt = 'sqlitedb'
    Filter = 'sqlitedb|*.sqlitedb'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 184
    Top = 136
  end
end
