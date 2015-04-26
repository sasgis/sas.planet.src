object frExportGEKml: TfrExportGEKml
  Left = 0
  Top = 0
  Width = 400
  Height = 120
  Align = alClient
  Constraints.MinHeight = 120
  Constraints.MinWidth = 400
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 400
    Height = 93
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlZoom: TPanel
      Left = 325
      Top = 0
      Width = 75
      Height = 93
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 325
      Height = 93
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 319
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Map'
      end
      object chkNotSaveNotExists: TCheckBox
        Left = 3
        Top = 59
        Width = 319
        Height = 17
        Align = alTop
        Caption = 'Don'#39't store references to non-existent tiles'
        TabOrder = 0
      end
      object chkUseRelativePath: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 42
        Width = 319
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Relative path to tiles'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object pnlMap: TPanel
        Left = 3
        Top = 16
        Width = 319
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 400
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
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 329
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 376
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
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 184
    Top = 136
  end
end
