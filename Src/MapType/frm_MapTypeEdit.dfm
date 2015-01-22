object frmMapTypeEdit: TfrmMapTypeEdit
  Left = 198
  Top = 305
  ClientHeight = 446
  ClientWidth = 561
  Color = clBtnFace
  Constraints.MinHeight = 435
  Constraints.MinWidth = 460
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 153
    Top = 0
    Height = 389
    ExplicitLeft = 184
    ExplicitTop = 160
    ExplicitHeight = 100
  end
  object PageControl1: TPageControl
    Left = 156
    Top = 0
    Width = 405
    Height = 389
    ActivePage = tsInternet
    Align = alClient
    TabOrder = 0
    object tsInternet: TTabSheet
      Caption = 'Internet'
      ImageIndex = 8
      object pnlDownloaderState: TPanel
        Left = 0
        Top = 216
        Width = 397
        Height = 70
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object lblDownloaderState: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 6
          Width = 385
          Height = 13
          Align = alTop
          Caption = 'Download state:'
          Layout = tlCenter
          ExplicitWidth = 79
        end
        object mmoDownloadState: TMemo
          AlignWithMargins = True
          Left = 6
          Top = 22
          Width = 388
          Height = 48
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alTop
          ReadOnly = True
          TabOrder = 0
        end
      end
      object pnlHeader: TPanel
        Left = 0
        Top = 67
        Width = 397
        Height = 70
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 1
        object lblHeader: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 385
          Height = 13
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Custom HTTP Headers:'
          ExplicitWidth = 111
        end
        object pnlHeaderReset: TPanel
          Left = 367
          Top = 16
          Width = 27
          Height = 51
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
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
          Left = 6
          Top = 19
          Width = 358
          Height = 45
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
      object pnlUrl: TPanel
        Left = 0
        Top = 0
        Width = 397
        Height = 67
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 2
        object lblUrl: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 385
          Height = 13
          Margins.Top = 0
          Margins.Bottom = 1
          Align = alTop
          Caption = 'Base part of request URL:'
          ExplicitWidth = 125
        end
        object pnlUrlRight: TPanel
          Left = 367
          Top = 17
          Width = 27
          Height = 47
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
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
          Left = 6
          Top = 17
          Width = 358
          Height = 44
          Margins.Top = 0
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 0
          WantReturns = False
        end
      end
      object pnlVersion: TPanel
        Left = 0
        Top = 137
        Width = 397
        Height = 48
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 3
        object lblVersion: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 385
          Height = 13
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Map Version:'
          Layout = tlCenter
          ExplicitWidth = 62
        end
        object btnResetVersion: TButton
          AlignWithMargins = True
          Left = 370
          Top = 19
          Width = 21
          Height = 23
          Hint = 'By default'
          Align = alRight
          Caption = '<>'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btnResetVersionClick
        end
        object edtVersion: TEdit
          AlignWithMargins = True
          Left = 6
          Top = 19
          Width = 358
          Height = 23
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 21
        end
      end
      object pnlSleep: TPanel
        Left = 0
        Top = 185
        Width = 397
        Height = 31
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 4
        object lblPause: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 6
          Width = 29
          Height = 19
          Align = alLeft
          Caption = 'Pause'
          ExplicitHeight = 13
        end
        object btnResetPause: TButton
          AlignWithMargins = True
          Left = 133
          Top = 3
          Width = 21
          Height = 21
          Hint = 'By default'
          Caption = '<>'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = btnResetPauseClick
        end
        object SESleep: TSpinEdit
          AlignWithMargins = True
          Left = 38
          Top = 3
          Width = 89
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
      end
      object chkDownloadEnabled: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 341
        Width = 388
        Height = 17
        Margins.Left = 6
        Align = alBottom
        Caption = 'Download enabled'
        TabOrder = 5
      end
    end
    object tsOthers: TTabSheet
      Caption = 'Cache and Other'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlCacheName: TPanel
        Left = 0
        Top = 0
        Width = 397
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object lblFolder: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 6
          Width = 61
          Height = 13
          Align = alLeft
          Caption = 'Cache folder'
          Layout = tlCenter
        end
        object EditNameinCache: TEdit
          AlignWithMargins = True
          Left = 73
          Top = 6
          Width = 264
          Height = 21
          Align = alClient
          TabOrder = 0
        end
        object btnResetFolder: TButton
          AlignWithMargins = True
          Left = 370
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
        object BtnSelectPath: TButton
          Tag = 2
          AlignWithMargins = True
          Left = 343
          Top = 6
          Width = 21
          Height = 21
          Align = alRight
          Caption = '...'
          TabOrder = 2
          OnClick = BtnSelectPathClick
        end
      end
      object pnlCacheType: TPanel
        Left = 0
        Top = 33
        Width = 397
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 1
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
          Width = 297
          Height = 21
          Align = alClient
          Style = csDropDownList
          DropDownCount = 10
          ItemHeight = 0
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
            'RAM'
            'MobileAtlas')
        end
        object btnResetCacheType: TButton
          AlignWithMargins = True
          Left = 370
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
        Top = 66
        Width = 397
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 2
        object lblSubMenu: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 6
          Width = 78
          Height = 13
          Align = alLeft
          Caption = 'Parent submenu'
          Layout = tlCenter
        end
        object EditParSubMenu: TEdit
          AlignWithMargins = True
          Left = 90
          Top = 6
          Width = 274
          Height = 21
          Align = alClient
          TabOrder = 0
        end
        object btnResetSubMenu: TButton
          AlignWithMargins = True
          Left = 370
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
      object grdpnlHotKey: TGridPanel
        Left = 0
        Top = 122
        Width = 397
        Height = 25
        Align = alTop
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
        TabOrder = 3
        DesignSize = (
          397
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
          Left = 6
          Top = 6
          Width = 34
          Height = 13
          Margins.Left = 6
          Anchors = [akLeft]
          Caption = 'Hotkey'
          ExplicitTop = 7
        end
      end
      object chkBoxSeparator: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 102
        Width = 388
        Height = 17
        Margins.Left = 6
        Align = alTop
        Caption = 'Add menu separator line after this map'
        TabOrder = 4
      end
      object CheckEnabled: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 341
        Width = 388
        Height = 17
        Margins.Left = 6
        Align = alBottom
        Caption = 'Map enabled'
        TabOrder = 5
      end
    end
    object tsParams: TTabSheet
      Caption = 'Params.txt'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object tsGetURLScript: TTabSheet
      Caption = 'GetURLScript.txt'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object tsInfo: TTabSheet
      Caption = 'Info.txt'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 409
    Width = 561
    Height = 37
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
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
      Left = 399
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
      Left = 480
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
  object tvMenu: TTreeView
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 153
    Height = 387
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 2
    Align = alLeft
    HideSelection = False
    HotTrack = True
    Indent = 19
    ReadOnly = True
    RowSelect = True
    ShowButtons = False
    ShowRoot = False
    TabOrder = 2
    OnClick = tvMenuClick
    OnCollapsing = tvMenuCollapsing
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 389
    Width = 561
    Height = 20
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object lblZmpName: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 24
      Height = 14
      Align = alLeft
      Caption = 'ZMP:'
      ExplicitHeight = 13
    end
    object edtZmp: TEdit
      AlignWithMargins = True
      Left = 33
      Top = 3
      Width = 525
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
end
