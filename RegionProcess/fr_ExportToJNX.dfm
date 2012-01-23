object frExportToJNX: TfrExportToJNX
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
      ExplicitLeft = 3
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
      object LProductID: TLabel
        Left = 3
        Top = 46
        Width = 51
        Height = 13
        Align = alCustom
        Caption = 'Product ID'
      end
      object LProductName: TLabel
        Left = 3
        Top = 74
        Width = 67
        Height = 13
        Caption = 'Product Name'
      end
      object LMapName: TLabel
        Left = 3
        Top = 102
        Width = 50
        Height = 13
        Caption = 'Map Name'
      end
      object LZOrder: TLabel
        Left = 3
        Top = 154
        Width = 42
        Height = 13
        Caption = 'Z-Order:'
      end
      object LVersion: TLabel
        Left = 3
        Top = 128
        Width = 60
        Height = 13
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
        Left = 87
        Top = 74
        Width = 283
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'SAS Palnet'
      end
      object EMapName: TEdit
        Left = 87
        Top = 101
        Width = 283
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object v3: TRadioButton
        Tag = 3
        Left = 87
        Top = 128
        Width = 50
        Height = 17
        Caption = 'v3'
        Checked = True
        TabOrder = 3
        TabStop = True
        OnClick = versionselect
      end
      object v4: TRadioButton
        Tag = 4
        Left = 143
        Top = 128
        Width = 38
        Height = 17
        Caption = 'v4'
        TabOrder = 4
        OnClick = versionselect
      end
      object EZorder: TSpinEdit
        Left = 87
        Top = 151
        Width = 94
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
        Enabled = False
        MaxValue = 100
        MinValue = 10
        TabOrder = 6
        Value = 95
      end
      object EProductID: TComboBox
        Left = 87
        Top = 46
        Width = 283
        Height = 21
        AutoDropDown = True
        Style = csDropDownList
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
