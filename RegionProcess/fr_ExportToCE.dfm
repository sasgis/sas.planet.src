object frExportToCE: TfrExportToCE
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
    Width = 451
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlRight: TPanel
      Left = 376
      Top = 0
      Width = 75
      Height = 277
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lblZooms: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 69
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Zooms:'
        ExplicitWidth = 35
      end
      object chkAllZooms: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 257
        Width = 69
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = 'All'
        TabOrder = 0
        OnClick = chkAllZoomsClick
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 19
        Width = 69
        Height = 235
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        OnDblClick = chklstZoomsDblClick
      end
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
      TabOrder = 1
      DesignSize = (
        376
        277)
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
      object LMapName: TLabel
        Left = 3
        Top = 46
        Width = 50
        Height = 13
        Caption = 'Map Name'
      end
      object LComent: TLabel
        Left = 3
        Top = 77
        Width = 45
        Height = 13
        Caption = 'Comment'
      end
      object lVolSize: TLabel
        Left = 271
        Top = 3
        Width = 99
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Max volume size, Mb'
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 19
        Width = 262
        Height = 21
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbMapChange
      end
      object EMapName: TEdit
        Left = 96
        Top = 47
        Width = 274
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object EComent: TEdit
        Left = 96
        Top = 74
        Width = 274
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object cbbMaxVolSize: TComboBox
        Left = 271
        Top = 19
        Width = 99
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemHeight = 13
        ItemIndex = 2
        TabOrder = 2
        Text = '1024'
        Items.Strings = (
          '256'
          '512'
          '1024'
          '2048'
          '')
      end
      object SaveRecoverInfo: TCheckBox
        Left = 3
        Top = 101
        Width = 367
        Height = 17
        Caption = 'Save recover informaiton'
        TabOrder = 4
      end
      object LFoldersName: TListBox
        Left = 271
        Top = 101
        Width = 99
        Height = 97
        ItemHeight = 13
        TabOrder = 5
        Visible = False
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
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = '.d00'
    Filter = '*.d00|*.d00|All (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 336
    Top = 248
  end
end
