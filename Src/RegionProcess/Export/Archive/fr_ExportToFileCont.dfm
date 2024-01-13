object frExportToFileCont: TfrExportToFileCont
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  Constraints.MinHeight = 120
  Constraints.MinWidth = 451
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 451
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlZoom: TPanel
      Left = 376
      Top = 0
      Width = 75
      Height = 277
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 277
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblNamesType: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 44
        Width = 370
        Height = 15
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Export selection to format:'
      end
      object pnlMap: TPanel
        Left = 3
        Top = 3
        Width = 370
        Height = 38
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object lblMap: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 370
          Height = 15
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = 'Map/Overlay layer:'
        end
      end
      object pnlCacheTypes: TPanel
        Left = 3
        Top = 59
        Width = 370
        Height = 21
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
      end
      object pnlArchiveWriteConfig: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 83
        Width = 370
        Height = 184
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
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
    TabOrder = 0
    object lblTargetFile: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Alignment = taRightJustify
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      AlignWithMargins = True
      Left = 47
      Top = 3
      Width = 377
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
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
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 104
    Top = 104
  end
end
