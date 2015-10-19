object frExportRMapsSQLite: TfrExportRMapsSQLite
  Left = 0
  Top = 0
  Width = 550
  Height = 230
  Align = alClient
  Constraints.MinHeight = 230
  Constraints.MinWidth = 550
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 550
    Height = 203
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 467
      Height = 203
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 461
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Map'
      end
      object lblOverlay: TLabel
        Left = 3
        Top = 39
        Width = 461
        Height = 13
        Align = alTop
        Caption = 'Overlay layer:'
        ExplicitWidth = 69
      end
      object chkReplaceExistingTiles: TCheckBox
        Left = 3
        Top = 95
        Width = 461
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
        Top = 78
        Width = 461
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
        Top = 16
        Width = 461
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
      end
      object chkDirectTilesCopy: TCheckBox
        Left = 3
        Top = 129
        Width = 461
        Height = 17
        Align = alTop
        Caption = 'Direct tiles copy (without any modifications)'
        TabOrder = 3
        OnClick = chkDirectTilesCopyClick
      end
      object pnlOverlay: TPanel
        Left = 3
        Top = 52
        Width = 461
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 4
      end
      object pnlImageFormat: TPanel
        Left = 3
        Top = 146
        Width = 461
        Height = 48
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 5
        object lblJpgQulity: TLabel
          Left = 156
          Top = 6
          Width = 90
          Height = 13
          Caption = 'Quality (for JPEG):'
          Layout = tlCenter
        end
        object lblImageFormat: TLabel
          Left = 0
          Top = 6
          Width = 69
          Height = 13
          Caption = 'Image format:'
          Layout = tlCenter
        end
        object lblCompression: TLabel
          Left = 312
          Top = 6
          Width = 113
          Height = 13
          Caption = 'Compression (for PNG):'
          Layout = tlCenter
        end
        object seJpgQuality: TSpinEdit
          Left = 156
          Top = 22
          Width = 150
          Height = 22
          MaxValue = 100
          MinValue = 1
          TabOrder = 0
          Value = 75
        end
        object cbbImageFormat: TComboBox
          Left = 0
          Top = 22
          Width = 150
          Height = 21
          ItemHeight = 13
          TabOrder = 1
          Text = 'JPEG'
          OnChange = cbbImageFormatChange
          Items.Strings = (
            'Auto'
            'JPEG'
            'BMP'
            'GIF'
            'PNG (Indexed Colors)'
            'PNG (TrueColor)'
            'PNG (TrueColor + Alpha)')
        end
        object seCompression: TSpinEdit
          Left = 312
          Top = 22
          Width = 150
          Height = 22
          MaxValue = 9
          MinValue = 0
          TabOrder = 2
          Value = 2
        end
      end
      object chkUsePrevZoom: TCheckBox
        Left = 3
        Top = 112
        Width = 461
        Height = 17
        Align = alTop
        Caption = 'Use tiles from lower zooms (on unavalible tile)'
        TabOrder = 6
        OnClick = chkUsePrevZoomClick
      end
    end
    object PnlZoom: TPanel
      Left = 467
      Top = 0
      Width = 83
      Height = 203
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
    Width = 550
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
      Width = 479
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 526
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
    Left = 416
    Top = 272
  end
end
