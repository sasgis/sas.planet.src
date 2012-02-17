object frExportToJNX: TfrExportToJNX
  Left = 0
  Top = 0
  Width = 451
  Height = 254
  Align = alClient
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  ExplicitHeight = 304
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 451
    Height = 227
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 277
    object pnlRight: TPanel
      Left = 376
      Top = 0
      Width = 75
      Height = 227
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitHeight = 277
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
        Top = 207
        Width = 69
        Height = 17
        Margins.Left = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alBottom
        Caption = 'All'
        Enabled = False
        TabOrder = 0
        OnClick = chkAllZoomsClick
        ExplicitTop = 257
      end
      object chklstZooms: TCheckListBox
        Left = 3
        Top = 19
        Width = 69
        Height = 185
        OnClickCheck = chklstZoomsClickCheck
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        ExplicitHeight = 235
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 227
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitHeight = 277
      DesignSize = (
        376
        227)
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
      object LProductID: TLabel
        Left = 3
        Top = 46
        Width = 51
        Height = 20
        Align = alCustom
        Caption = 'Product ID'
      end
      object LProductName: TLabel
        Left = 3
        Top = 72
        Width = 67
        Height = 13
        Caption = 'Product Name'
      end
      object LMapName: TLabel
        Left = 3
        Top = 96
        Width = 50
        Height = 21
        Caption = 'Map Name'
      end
      object LZOrder: TLabel
        Left = 3
        Top = 155
        Width = 42
        Height = 19
        Caption = 'Z-Order:'
      end
      object LVersion: TLabel
        Left = 3
        Top = 123
        Width = 60
        Height = 14
        Caption = 'JNX version:'
      end
      object lblCompress: TLabel
        Left = 305
        Top = 3
        Width = 65
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Compression:'
      end
      object cbbMap: TComboBox
        Left = 3
        Top = 19
        Width = 296
        Height = 21
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 16
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbMapChange
      end
      object EProductName: TEdit
        Left = 96
        Top = 69
        Width = 274
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object EMapName: TEdit
        Left = 96
        Top = 93
        Width = 274
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object v3: TRadioButton
        Tag = 3
        Left = 96
        Top = 123
        Width = 41
        Height = 23
        Caption = 'v3'
        Checked = True
        TabOrder = 3
        TabStop = True
        OnClick = versionselect
      end
      object v4: TRadioButton
        Tag = 4
        Left = 143
        Top = 123
        Width = 38
        Height = 23
        Caption = 'v4'
        TabOrder = 4
        OnClick = versionselect
      end
      object EZorder: TSpinEdit
        Left = 96
        Top = 151
        Width = 85
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 5
        Value = 30
      end
      object EJpgQuality: TSpinEdit
        Left = 305
        Top = 19
        Width = 65
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 100
        MinValue = 10
        TabOrder = 6
        Value = 95
      end
      object EProductID: TComboBox
        AlignWithMargins = True
        Left = 96
        Top = 46
        Width = 274
        Height = 21
        AutoDropDown = True
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 7
        Items.Strings = (
          '0 - BirdsEye'
          '2 - BirdsEye Select EIRE'
          '3 - BirdsEye Select Deutschland'
          '4 - BirdsEye Select Great Britain'
          '5 - BirdsEye Select France'
          '6 - BirdsEye Select Kompass - Switzerland'
          '7 - BirdsEye Select Kompass - Austria + East Alps'
          '8 - USGS Quads (BirdsEye TOPO, U.S. and Canada)'
          '9 - NRC TopoRama (BirdsEye TOPO, U.S. and Canada)')
      end
      object TreeView1: TTreeView
        Left = 184
        Top = 116
        Width = 186
        Height = 105
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        HideSelection = False
        HotTrack = True
        Indent = 19
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
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
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 336
    Top = 272
  end
end
