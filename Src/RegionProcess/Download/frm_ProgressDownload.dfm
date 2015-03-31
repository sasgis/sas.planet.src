object frmProgressDownload: TfrmProgressDownload
  Left = 226
  Top = 306
  Caption = 'Please wait...'
  ClientHeight = 239
  ClientWidth = 328
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
    Width = 322
    Height = 233
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    OnResize = Panel1Resize
    object mmoLog: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 316
      Height = 78
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object pnlBottom: TPanel
      Left = 0
      Top = 203
      Width = 322
      Height = 30
      Margins.Top = 0
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 1
      object btnClose: TButton
        AlignWithMargins = True
        Left = 254
        Top = 3
        Width = 65
        Height = 24
        Align = alRight
        Caption = 'Quit'
        TabOrder = 0
        OnClick = btnCloseClick
      end
      object btnPause: TButton
        AlignWithMargins = True
        Left = 173
        Top = 3
        Width = 75
        Height = 24
        Align = alRight
        Caption = 'Pause'
        TabOrder = 1
        OnClick = btnPauseClick
      end
      object btnMinimize: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 60
        Height = 24
        Align = alLeft
        Caption = 'Minimize'
        TabOrder = 2
        OnClick = btnMinimizeClick
      end
      object TBXOperationsToolbar: TTBXToolbar
        Left = 67
        Top = 3
        Width = 100
        Height = 24
        Images = frmMain.MenusImageList
        ShrinkMode = tbsmWrap
        TabOrder = 3
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
      Left = 5
      Top = 169
      Width = 312
      Height = 17
      Margins.Left = 5
      Margins.Top = 0
      Margins.Right = 5
      Margins.Bottom = 0
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
    object pnlToProcess: TPanel
      Left = 0
      Top = 84
      Width = 322
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object lblToProcess: TLabel
        Left = 0
        Top = 0
        Width = 112
        Height = 13
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Process not more than:'
        ParentBiDiMode = False
        Layout = tlCenter
      end
      object lblToProcessValue: TLabel
        Left = 316
        Top = 0
        Width = 6
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
      end
    end
    object pnlProcessed: TPanel
      Left = 0
      Top = 101
      Width = 322
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 4
      object lblProcessed: TLabel
        Left = 0
        Top = 0
        Width = 78
        Height = 13
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Processed total:'
        ParentBiDiMode = False
        Layout = tlCenter
      end
      object lblProcessedValue: TLabel
        Left = 316
        Top = 0
        Width = 6
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
      end
    end
    object pnlDownloaded: TPanel
      Left = 0
      Top = 118
      Width = 322
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      object lblDownloaded: TLabel
        Left = 0
        Top = 0
        Width = 88
        Height = 13
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Downloaded total:'
        ParentBiDiMode = False
        Layout = tlCenter
      end
      object lblDownloadedValue: TLabel
        Left = 316
        Top = 0
        Width = 6
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
      end
    end
    object pnlSizeToFinish: TPanel
      Left = 0
      Top = 152
      Width = 322
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 6
      object lblSizeToFinish: TLabel
        Left = 0
        Top = 0
        Width = 105
        Height = 13
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Approx. to download:'
        ParentBiDiMode = False
        Layout = tlCenter
      end
      object lblSizeToFinishValue: TLabel
        Left = 316
        Top = 0
        Width = 6
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
      end
    end
    object pnlTimeToFinish: TPanel
      Left = 0
      Top = 135
      Width = 322
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 7
      object lblTimeToFinish: TLabel
        Left = 0
        Top = 0
        Width = 75
        Height = 13
        Align = alLeft
        BiDiMode = bdLeftToRight
        Caption = 'Time remaining:'
        ParentBiDiMode = False
        Layout = tlCenter
      end
      object lblTimeToFinishValue: TLabel
        Left = 316
        Top = 0
        Width = 6
        Height = 13
        Align = alRight
        Alignment = taRightJustify
        BiDiMode = bdLeftToRight
        Caption = '  '
        ParentBiDiMode = False
        Layout = tlCenter
      end
    end
    object chkAutoCloseWhenFinish: TCheckBox
      Left = 0
      Top = 186
      Width = 322
      Height = 17
      Align = alBottom
      Caption = 'Close this window once finished'
      TabOrder = 8
    end
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
