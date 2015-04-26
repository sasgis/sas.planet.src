object frExportToOgf2: TfrExportToOgf2
  Left = 0
  Top = 0
  Width = 540
  Height = 210
  Align = alClient
  Constraints.MinHeight = 210
  Constraints.MinWidth = 540
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 467
    Height = 158
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 467
      Height = 158
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 24
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alCustom
        Caption = 'Map:'
      end
      object lblHyb: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 43
        Width = 69
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alCustom
        Caption = 'Overlay layer:'
      end
      object lblImageFormat: TLabel
        Left = 3
        Top = 110
        Width = 88
        Height = 13
        Caption = 'OGF2 tiles format:'
        Layout = tlCenter
      end
      object lblTileRes: TLabel
        Left = 154
        Top = 110
        Width = 103
        Height = 13
        Caption = 'OGF2 tiles resolution:'
        Layout = tlCenter
      end
      object lblJpgQulity: TLabel
        Left = 305
        Top = 110
        Width = 90
        Height = 13
        Caption = 'Quality (for JPEG):'
        Layout = tlCenter
      end
      object cbbImageFormat: TComboBox
        Left = 3
        Top = 129
        Width = 145
        Height = 21
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 0
        Text = 'JPEG'
        Items.Strings = (
          'BMP'
          'PNG'
          'JPEG')
      end
      object cbbTileRes: TComboBox
        Left = 154
        Top = 129
        Width = 145
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 1
        Text = '128x128 pix'
        OnChange = cbbTileResChange
        Items.Strings = (
          '128x128 pix'
          '256x256 pix')
      end
      object chkUsePrevZoom: TCheckBox
        Left = 3
        Top = 86
        Width = 369
        Height = 17
        Caption = 'Use tiles from lower zooms (on unavalible tile)'
        TabOrder = 2
      end
      object seJpgQuality: TSpinEdit
        Left = 305
        Top = 129
        Width = 145
        Height = 22
        MaxValue = 100
        MinValue = 1
        TabOrder = 3
        Value = 75
      end
      object pnlMap: TPanel
        Left = 3
        Top = 22
        Width = 461
        Height = 23
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        TabOrder = 4
      end
      object pnlHyb: TPanel
        Left = 3
        Top = 56
        Width = 461
        Height = 23
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        TabOrder = 5
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 540
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
      Alignment = taRightJustify
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 469
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 516
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object pnlZoom: TPanel
    Left = 467
    Top = 27
    Width = 73
    Height = 158
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 3
    Constraints.MinWidth = 56
    TabOrder = 2
    object lblZoom: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 67
      Height = 13
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Align = alTop
      AutoSize = False
      Caption = 'Zoom:'
    end
    object cbbZoom: TComboBox
      Left = 3
      Top = 16
      Width = 67
      Height = 21
      Align = alTop
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbZoomChange
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 185
    Width = 540
    Height = 25
    Align = alBottom
    Alignment = taLeftJustify
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 3
    object lblStat: TLabel
      Left = 0
      Top = 0
      Width = 540
      Height = 13
      Align = alTop
      Caption = '_'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 336
    Top = 272
  end
end
