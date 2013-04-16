object frmMapTypeEdit: TfrmMapTypeEdit
  Left = 198
  Top = 305
  BorderIcons = [biSystemMenu]
  ClientHeight = 461
  ClientWidth = 450
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 458
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 424
    Width = 450
    Height = 37
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object btnByDefault: TButton
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 105
      Height = 23
      Align = alLeft
      Caption = 'All by default'
      TabOrder = 0
      OnClick = btnByDefaultClick
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 288
      Top = 6
      Width = 75
      Height = 23
      Align = alRight
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 1
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 369
      Top = 6
      Width = 75
      Height = 23
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
  end
  object pnlSeparator: TPanel
    Left = 0
    Top = 307
    Width = 450
    Height = 63
    Align = alBottom
    BevelEdges = []
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object CheckBox1: TCheckBox
      AlignWithMargins = True
      Left = 6
      Top = 3
      Width = 438
      Height = 17
      Margins.Top = 0
      Align = alTop
      Caption = 'Add menu separator line after this map'
      TabOrder = 0
    end
    object CheckEnabled: TCheckBox
      AlignWithMargins = True
      Left = 6
      Top = 43
      Width = 438
      Height = 17
      Margins.Top = 0
      Align = alTop
      Caption = 'Map enabled'
      TabOrder = 1
    end
    object chkDownloadEnabled: TCheckBox
      AlignWithMargins = True
      Left = 6
      Top = 23
      Width = 438
      Height = 17
      Margins.Top = 0
      Align = alTop
      Caption = 'Download enabled'
      TabOrder = 2
    end
  end
  object pnlCacheType: TPanel
    Left = 0
    Top = 274
    Width = 450
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    object lblCacheType: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 55
      Height = 21
      Align = alLeft
      Caption = 'Cache type'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object CBCacheType: TComboBox
      AlignWithMargins = True
      Left = 67
      Top = 6
      Width = 350
      Height = 21
      Align = alClient
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'By default'
        'GoogleMV'
        'SAS.Planet'
        'EarthSlicer 1.95'
        'GlobalMapper Tiles'
        'BerkeleyDB'
        'BerkeleyDB (Versioned)'
        'DBMS'
        'RAM')
    end
    object btnResetCacheType: TButton
      AlignWithMargins = True
      Left = 423
      Top = 6
      Width = 21
      Height = 21
      Hint = 'By default'
      Align = alRight
      Caption = '<>'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnResetCacheTypeClick
    end
  end
  object pnlParentItem: TPanel
    Left = 0
    Top = 241
    Width = 450
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 3
    object lblSubMenu: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 78
      Height = 21
      Align = alLeft
      Caption = 'Parent submenu'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object EditParSubMenu: TEdit
      AlignWithMargins = True
      Left = 90
      Top = 6
      Width = 327
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnResetSubMenu: TButton
      AlignWithMargins = True
      Left = 423
      Top = 6
      Width = 21
      Height = 21
      Hint = 'By default'
      Align = alRight
      Caption = '<>'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnResetSubMenuClick
    end
  end
  object pnlCacheName: TPanel
    Left = 0
    Top = 208
    Width = 450
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 4
    object lblFolder: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 61
      Height = 21
      Align = alLeft
      Caption = 'Cache folder'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object EditNameinCache: TEdit
      AlignWithMargins = True
      Left = 73
      Top = 6
      Width = 344
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnResetFolder: TButton
      AlignWithMargins = True
      Left = 423
      Top = 6
      Width = 21
      Height = 21
      Hint = 'By default'
      Align = alRight
      Caption = '<>'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnResetFolderClick
    end
  end
  object pnlUrl: TPanel
    Left = 0
    Top = 20
    Width = 450
    Height = 64
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 5
    object lblUrl: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 19
      Height = 52
      Align = alLeft
      Caption = 'URL'
      ExplicitHeight = 13
    end
    object pnlUrlRight: TPanel
      Left = 420
      Top = 3
      Width = 27
      Height = 58
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnResetUrl: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 21
        Height = 21
        Hint = 'By default'
        Align = alTop
        Caption = '<>'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnResetUrlClick
      end
    end
    object EditURL: TMemo
      AlignWithMargins = True
      Left = 31
      Top = 6
      Width = 386
      Height = 52
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
      WantReturns = False
    end
  end
  object grdpnlSleepAndKey: TGridPanel
    Left = 0
    Top = 175
    Width = 450
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = grdpnlHotKey
        Row = 0
      end
      item
        Column = 1
        Control = grdpnlSleep
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end
      item
        SizeStyle = ssAuto
      end>
    TabOrder = 6
    object grdpnlHotKey: TGridPanel
      Left = 3
      Top = 3
      Width = 222
      Height = 25
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 55.000000000000000000
        end
        item
          SizeStyle = ssAuto
          Value = 100.000000000000000000
        end
        item
          SizeStyle = ssAuto
          Value = 50.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 1
          Control = EditHotKey
          Row = 0
        end
        item
          Column = 2
          Control = btnResetHotKey
          Row = 0
        end
        item
          Column = 0
          Control = lblHotKey
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      DesignSize = (
        222
        25)
      object EditHotKey: THotKey
        Left = 55
        Top = 2
        Width = 86
        Height = 21
        Anchors = []
        HotKey = 0
        Modifiers = []
        TabOrder = 0
      end
      object btnResetHotKey: TButton
        AlignWithMargins = True
        Left = 144
        Top = 3
        Width = 21
        Height = 19
        Hint = 'By default'
        Caption = '<>'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnResetHotKeyClick
      end
      object lblHotKey: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 6
        Width = 34
        Height = 13
        Anchors = [akLeft]
        Caption = 'Hotkey'
      end
    end
    object grdpnlSleep: TGridPanel
      Left = 294
      Top = 3
      Width = 153
      Height = 27
      Align = alRight
      BevelOuter = bvNone
      ColumnCollection = <
        item
          SizeStyle = ssAuto
          Value = 50.000000000000000000
        end
        item
          Value = 100.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 27.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = lblPause
          Row = 0
        end
        item
          Column = 1
          Control = SESleep
          Row = 0
        end
        item
          Column = 2
          Control = btnResetPause
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      DesignSize = (
        153
        27)
      object lblPause: TLabel
        Left = 0
        Top = 7
        Width = 29
        Height = 13
        Anchors = []
        Caption = 'Pause'
      end
      object SESleep: TSpinEdit
        AlignWithMargins = True
        Left = 33
        Top = 3
        Width = 89
        Height = 22
        Anchors = []
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
      object btnResetPause: TButton
        AlignWithMargins = True
        Left = 129
        Top = 3
        Width = 21
        Height = 21
        Hint = 'By default'
        Anchors = []
        Caption = '<>'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnResetPauseClick
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 20
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 7
    object lblZmpName: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 20
      Height = 14
      Align = alLeft
      Caption = 'Zmp'
      ExplicitHeight = 13
    end
    object edtZmp: TEdit
      AlignWithMargins = True
      Left = 29
      Top = 3
      Width = 418
      Height = 14
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
    end
  end
  object pnlVersion: TPanel
    Left = 0
    Top = 142
    Width = 450
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 8
    object lblVersion: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 35
      Height = 21
      Align = alLeft
      Caption = 'Version'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object btnResetVersion: TButton
      AlignWithMargins = True
      Left = 423
      Top = 6
      Width = 21
      Height = 21
      Hint = 'By default'
      Align = alRight
      Caption = '<>'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnResetVersionClick
    end
    object edtVersion: TEdit
      AlignWithMargins = True
      Left = 47
      Top = 6
      Width = 370
      Height = 21
      Align = alClient
      TabOrder = 1
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 84
    Width = 450
    Height = 58
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 9
    object lblHeader: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 35
      Height = 46
      Align = alLeft
      Caption = 'Header'
      ExplicitHeight = 13
    end
    object pnlHeaderReset: TPanel
      Left = 420
      Top = 3
      Width = 27
      Height = 52
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnResetHeader: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 21
        Height = 21
        Hint = 'By default'
        Align = alTop
        Caption = '<>'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnResetHeaderClick
      end
    end
    object mmoHeader: TMemo
      AlignWithMargins = True
      Left = 47
      Top = 6
      Width = 370
      Height = 46
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object pnlDownloaderState: TPanel
    Left = 0
    Top = 370
    Width = 450
    Height = 54
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 10
    object lblDownloaderState: TLabel
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 75
      Height = 42
      Align = alLeft
      Caption = 'Download state'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object mmoDownloadState: TMemo
      Left = 84
      Top = 3
      Width = 363
      Height = 48
      Align = alClient
      ReadOnly = True
      TabOrder = 0
    end
  end
end
