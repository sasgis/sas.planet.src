object frExportToOgf2: TfrExportToOgf2
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 395
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 395
      Height = 277
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 20
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alCustom
        Caption = 'Map'
      end
      object lblStat: TLabel
        Left = 3
        Top = 46
        Width = 6
        Height = 13
        Caption = '_'
        Layout = tlCenter
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 19
        Width = 369
        Height = 21
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
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
    TabOrder = 1
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
  object pnlZoom: TPanel
    Left = 395
    Top = 27
    Width = 56
    Height = 277
    Align = alRight
    BevelOuter = bvNone
    BorderWidth = 3
    Constraints.MinWidth = 56
    TabOrder = 2
    object lblZoom: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 50
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
      Width = 50
      Height = 21
      Align = alTop
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbZoomChange
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
