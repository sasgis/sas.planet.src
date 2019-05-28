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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 153
    Top = 0
    Height = 389
  end
  object PageControl1: TPageControl
    Left = 156
    Top = 0
    Width = 405
    Height = 389
    ActivePage = tsInternet
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object tsInternet: TTabSheet
      Caption = 'Internet'
      ImageIndex = 8
      TabVisible = False
      object pnlDownloaderState: TPanel
        Left = 0
        Top = 269
        Width = 397
        Height = 70
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 6
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
        Top = 90
        Width = 397
        Height = 70
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 2
        object lblHeader: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 3
          Width = 385
          Height = 13
          Margins.Top = 0
          Align = alTop
          Caption = 'Custom HTTP Headers:'
          ExplicitWidth = 111
        end
        object pnlHeaderReset: TPanel
          Left = 367
          Top = 19
          Width = 27
          Height = 48
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
          Left = 6
          Top = 22
          Width = 358
          Height = 42
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 1
        end
      end
      object pnlUrl: TPanel
        Left = 0
        Top = 23
        Width = 397
        Height = 67
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 1
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
        Top = 160
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
          Align = alTop
          Caption = 'Map Version:'
          Layout = tlCenter
          ExplicitWidth = 62
        end
        object btnResetVersion: TButton
          AlignWithMargins = True
          Left = 370
          Top = 22
          Width = 21
          Height = 20
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
          Top = 22
          Width = 358
          Height = 20
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 21
        end
      end
      object pnlSleep: TPanel
        Left = 0
        Top = 208
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
          Width = 53
          Height = 19
          Align = alLeft
          Caption = 'Pause, ms:'
          ExplicitHeight = 13
        end
        object btnResetPause: TButton
          AlignWithMargins = True
          Left = 146
          Top = 3
          Width = 21
          Height = 21
          Hint = 'By default'
          Caption = '<>'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btnResetPauseClick
        end
        object SESleep: TSpinEdit
          AlignWithMargins = True
          Left = 65
          Top = 3
          Width = 75
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
        end
      end
      object chkDownloadEnabled: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 3
        Width = 388
        Height = 17
        Margins.Left = 6
        Align = alTop
        Caption = 'Download enabled'
        TabOrder = 0
      end
      object flwpnlMaxConnectToServerCount: TFlowPanel
        Left = 0
        Top = 239
        Width = 397
        Height = 30
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        Padding.Top = 2
        TabOrder = 5
        object lblMaxConnectToServerCount: TLabel
          AlignWithMargins = True
          Left = 6
          Top = 8
          Width = 200
          Height = 13
          Margins.Left = 6
          Margins.Top = 6
          Caption = 'Max concurrent http(s)-requests number:'
        end
        object seMaxConnectToServerCount: TSpinEdit
          AlignWithMargins = True
          Left = 212
          Top = 5
          Width = 75
          Height = 22
          MaxValue = 64
          MinValue = 1
          TabOrder = 0
          Value = 1
        end
        object btnResetMaxConnect: TButton
          AlignWithMargins = True
          Left = 293
          Top = 5
          Width = 21
          Height = 21
          Hint = 'By default'
          Caption = '<>'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btnResetMaxConnectClick
        end
      end
    end
    object tsOthers: TTabSheet
      Caption = 'Cache and Other'
      ImageIndex = 5
      TabVisible = False
      object lblCacheState: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 183
        Width = 391
        Height = 13
        Align = alTop
        Caption = 'Cache state:'
        ExplicitWidth = 62
      end
      object pnlCacheName: TPanel
        Left = 0
        Top = 0
        Width = 397
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 1
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
          TabOrder = 2
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
          TabOrder = 1
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
        object pnlCacheTypesList: TPanel
          AlignWithMargins = True
          Left = 67
          Top = 6
          Width = 297
          Height = 21
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
        end
      end
      object pnlParentItem: TPanel
        Left = 0
        Top = 241
        Width = 397
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 5
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
        Top = 297
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
        TabOrder = 7
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
        Top = 277
        Width = 388
        Height = 17
        Margins.Left = 6
        Align = alTop
        Caption = 'Add menu separator line after this map'
        TabOrder = 6
      end
      object CheckEnabled: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 325
        Width = 388
        Height = 17
        Margins.Left = 6
        Align = alTop
        Caption = 'Map enabled'
        TabOrder = 0
      end
      object chkCacheReadOnly: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 69
        Width = 388
        Height = 17
        Margins.Left = 6
        Align = alTop
        Caption = 'Cache is Read-Only'
        TabOrder = 3
        OnClick = chkAccessClick
      end
      object grpCacheAccess: TGroupBox
        AlignWithMargins = True
        Left = 0
        Top = 92
        Width = 397
        Height = 85
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Cache abilities'
        TabOrder = 4
        object chkReadAccess: TCheckBox
          Tag = 1
          Left = 6
          Top = 16
          Width = 187
          Height = 17
          Caption = 'Allow Read'
          TabOrder = 0
          OnClick = chkAccessClick
        end
        object chkScanAccess: TCheckBox
          Tag = 2
          Left = 6
          Top = 39
          Width = 187
          Height = 17
          Caption = 'Allow Scan'
          TabOrder = 1
          OnClick = chkAccessClick
        end
        object chkAddAccess: TCheckBox
          Tag = 3
          Left = 199
          Top = 16
          Width = 187
          Height = 17
          Caption = 'Allow Add'
          TabOrder = 2
          OnClick = chkAccessClick
        end
        object chkDeleteAccess: TCheckBox
          Tag = 4
          Left = 199
          Top = 39
          Width = 187
          Height = 17
          Caption = 'Allow Delete'
          TabOrder = 3
          OnClick = chkAccessClick
        end
        object chkReplaceAccess: TCheckBox
          Tag = 5
          Left = 199
          Top = 62
          Width = 187
          Height = 17
          Caption = 'Allow Replace'
          TabOrder = 4
          OnClick = chkAccessClick
        end
      end
      object mmoCacheState: TMemo
        Left = 0
        Top = 199
        Width = 397
        Height = 42
        Align = alTop
        ReadOnly = True
        TabOrder = 8
      end
    end
    object tsParams: TTabSheet
      Caption = 'Params.txt'
      ImageIndex = 1
      TabVisible = False
    end
    object tsGetURLScript: TTabSheet
      Caption = 'GetURLScript.txt'
      ImageIndex = 2
      TabVisible = False
    end
    object tsInfo: TTabSheet
      Caption = 'Info.txt'
      ImageIndex = 3
      TabVisible = False
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
    TabOrder = 3
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
    TabOrder = 0
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
    TabOrder = 2
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
