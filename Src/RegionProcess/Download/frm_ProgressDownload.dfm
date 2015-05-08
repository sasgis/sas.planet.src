object frmProgressDownload: TfrmProgressDownload
  Left = 226
  Top = 306
  Caption = 'Please wait...'
  ClientHeight = 262
  ClientWidth = 324
  Color = clBtnFace
  Constraints.MinHeight = 243
  Constraints.MinWidth = 336
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 318
    Height = 238
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    ExplicitHeight = 245
    object mmoLog: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 312
      Height = 83
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      ExplicitHeight = 90
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 208
      Width = 318
      Height = 30
      Margins.Top = 0
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 215
      object btnClose: TButton
        AlignWithMargins = True
        Left = 230
        Top = 3
        Width = 85
        Height = 24
        Align = alRight
        Caption = 'Quit'
        TabOrder = 0
        OnClick = btnCloseClick
      end
      object btnPause: TButton
        AlignWithMargins = True
        Left = 139
        Top = 3
        Width = 85
        Height = 24
        Align = alRight
        Caption = 'Pause'
        TabOrder = 1
        OnClick = btnPauseClick
      end
      object TBXOperationsToolbar: TTBXToolbar
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 111
        Height = 24
        Align = alLeft
        Images = frmMain.MenusImageList
        ShrinkMode = tbsmWrap
        TabOrder = 2
        object tbtmSelect: TTBItem
          ImageIndex = 44
          OnClick = tbtmSelectClick
          Caption = ''
          Hint = 'Selection Manager'
        end
        object tbtmZoom: TTBItem
          ImageIndex = 43
          OnClick = tbtmZoomClick
          Caption = ''
          Hint = 'Fit to Screen'
        end
        object tbtmGotoMap: TTBItem
          ImageIndex = 0
          OnClick = tbtmGotoMapClick
          Caption = ''
          Hint = ''
        end
        object tbtmSave: TTBSubmenuItem
          DisplayMode = nbdmImageAndText
          DropdownCombo = True
          ImageIndex = 25
          OnClick = tbtmSaveClick
          Caption = ''
          Hint = 'Save current session'
          object tbtmMark: TTBItem
            ImageIndex = 17
            OnClick = tbtmMarkClick
            Caption = 'Store selection as Poligon'
            Hint = ''
          end
        end
      end
    end
    object pnlProgress: TPanel
      AlignWithMargins = True
      Left = 2
      Top = 174
      Width = 314
      Height = 17
      Margins.Left = 2
      Margins.Top = 0
      Margins.Right = 2
      Margins.Bottom = 0
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitTop = 181
    end
    object pnlToProcess: TPanel
      Left = 0
      Top = 89
      Width = 318
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitTop = 96
      object lblToProcess: TLabel
        Left = 0
        Top = 0
        Width = 112
        Height = 17
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Process not more than:'
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object lblToProcessValue: TLabel
        Left = 312
        Top = 0
        Width = 6
        Height = 17
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
    end
    object pnlProcessed: TPanel
      Left = 0
      Top = 106
      Width = 318
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 4
      ExplicitTop = 113
      object lblProcessed: TLabel
        Left = 0
        Top = 0
        Width = 78
        Height = 17
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Processed total:'
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object lblProcessedValue: TLabel
        Left = 312
        Top = 0
        Width = 6
        Height = 17
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
    end
    object pnlDownloaded: TPanel
      Left = 0
      Top = 123
      Width = 318
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      ExplicitTop = 130
      object lblDownloaded: TLabel
        Left = 0
        Top = 0
        Width = 88
        Height = 17
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Downloaded total:'
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object lblDownloadedValue: TLabel
        Left = 312
        Top = 0
        Width = 6
        Height = 17
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
    end
    object pnlSizeToFinish: TPanel
      Left = 0
      Top = 157
      Width = 318
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 6
      ExplicitTop = 164
      object lblSizeToFinish: TLabel
        Left = 0
        Top = 0
        Width = 105
        Height = 17
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Approx. to download:'
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object lblSizeToFinishValue: TLabel
        Left = 312
        Top = 0
        Width = 6
        Height = 17
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
    end
    object pnlTimeToFinish: TPanel
      Left = 0
      Top = 140
      Width = 318
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 7
      ExplicitTop = 147
      object lblTimeToFinish: TLabel
        Left = 0
        Top = 0
        Width = 75
        Height = 17
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Time remaining:'
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
      object lblTimeToFinishValue: TLabel
        Left = 312
        Top = 0
        Width = 6
        Height = 17
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
        ExplicitHeight = 13
      end
    end
    object chkAutoCloseWhenFinish: TCheckBox
      Left = 0
      Top = 191
      Width = 318
      Height = 17
      Align = alBottom
      Caption = 'Close this window once finished'
      TabOrder = 8
      ExplicitTop = 198
    end
  end
  object tbxStatusBar: TTBXStatusBar
    Left = 0
    Top = 244
    Width = 324
    Height = 18
    Panels = <
      item
        Size = 30
        Tag = 0
      end
      item
        Size = 180
        Tag = 0
      end
      item
        Size = 100
        Tag = 0
      end>
    UseSystemFont = False
    ExplicitTop = 246
  end
  object SaveSessionDialog: TSaveDialog
    DefaultExt = '*.sls'
    Filter = 'Download session (*.sls)|*.sls'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 288
    Top = 152
  end
  object UpdateTimer: TTimer
    OnTimer = UpdateTimerTimer
    Left = 256
    Top = 152
  end
end
