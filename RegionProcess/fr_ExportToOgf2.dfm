object frExportToOgf2: TfrExportToOgf2
  Left = 0
  Top = 0
  Width = 486
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  ExplicitWidth = 451
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 413
    Height = 252
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 395
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 413
      Height = 252
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitWidth = 395
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
        Left = 144
        Top = 110
        Width = 103
        Height = 13
        Caption = 'OGF2 tiles resolution:'
        Layout = tlCenter
      end
      object lblJpgQulity: TLabel
        Left = 285
        Top = 110
        Width = 90
        Height = 13
        Caption = 'Quality (for JPEG):'
        Layout = tlCenter
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 19
        Width = 410
        Height = 21
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
      end
      object cbbHyb: TComboBox
        Left = 3
        Top = 59
        Width = 410
        Height = 21
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 1
      end
      object cbbImageFormat: TComboBox
        Left = 3
        Top = 129
        Width = 135
        Height = 21
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 2
        Text = 'JPEG'
        Items.Strings = (
          'BMP'
          'PNG'
          'JPEG')
      end
      object cbbTileRes: TComboBox
        Left = 144
        Top = 129
        Width = 135
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 3
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
        TabOrder = 4
      end
      object seJpgQuality: TSpinEdit
        Left = 285
        Top = 128
        Width = 135
        Height = 22
        MaxValue = 100
        MinValue = 1
        TabOrder = 5
        Value = 75
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 486
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitWidth = 451
    object lblTargetFile: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alLeft
      Alignment = taRightJustify
      Caption = 'Save to:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 415
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 380
    end
    object btnSelectTargetFile: TButton
      Left = 462
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
      ExplicitLeft = 427
    end
  end
  object pnlZoom: TPanel
    Left = 413
    Top = 27
    Width = 73
    Height = 252
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 3
    Constraints.MinWidth = 56
    TabOrder = 2
    ExplicitLeft = 378
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
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 72
    end
    object cbbZoom: TComboBox
      Left = 3
      Top = 19
      Width = 67
      Height = 21
      Align = alTop
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbZoomChange
      ExplicitWidth = 50
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 279
    Width = 486
    Height = 25
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitWidth = 451
    object lblStat: TLabel
      Left = 3
      Top = 3
      Width = 7
      Height = 13
      Align = alCustom
      Anchors = [akLeft, akBottom]
      Caption = '_'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
      ExplicitTop = 5
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
