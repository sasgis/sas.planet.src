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
      object lblOverlay: TLabel
        Left = 3
        Top = 42
        Width = 362
        Height = 13
        Align = alTop
        Caption = 'Overlay layer'
        ExplicitWidth = 65
      end
      object chkReplaceExistingTiles: TCheckBox
        Left = 3
        Top = 98
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Replace existing tiles'
        Checked = True
        State = cbChecked
        TabOrder = 0
        ExplicitTop = 62
      end
      object chkForceDropTarget: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 81
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
        ExplicitTop = 45
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
      object chkDirectTilesCopy: TCheckBox
        Left = 3
        Top = 115
        Width = 362
        Height = 17
        Align = alTop
        Caption = 'Direct tiles copy (without overlay and reprojection)'
        TabOrder = 3
        OnClick = chkDirectTilesCopyClick
        ExplicitTop = 80
        ExplicitWidth = 97
      end
      object pnlOverlay: TPanel
        Left = 3
        Top = 55
        Width = 362
        Height = 23
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 4
        ExplicitLeft = 0
        ExplicitTop = 60
      end
      object pnlImageFormat: TPanel
        Left = 3
        Top = 129
        Width = 362
        Height = 48
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 5
        object lblJpgQulity: TLabel
          Left = 151
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
          Left = 302
          Top = 6
          Width = 113
          Height = 13
          Caption = 'Compression (for PNG):'
          Layout = tlCenter
        end
        object seJpgQuality: TSpinEdit
          Left = 151
          Top = 22
          Width = 145
          Height = 22
          MaxValue = 100
          MinValue = 1
          TabOrder = 0
          Value = 75
        end
        object cbbImageFormat: TComboBox
          Left = 0
          Top = 22
          Width = 145
          Height = 21
          ItemHeight = 13
          TabOrder = 1
          Text = 'JPEG'
          Items.Strings = (
            'Auto'
            'JPEG'
            'BMP'
            'GIF'
            'PNG (8 bit)'
            'PNG (24 bit)'
            'PNG (32 bit)')
        end
        object seCompression: TSpinEdit
          Left = 302
          Top = 22
          Width = 145
          Height = 22
          MaxValue = 9
          MinValue = 0
          TabOrder = 2
          Value = 2
        end
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
    Left = 416
    Top = 272
  end
end
