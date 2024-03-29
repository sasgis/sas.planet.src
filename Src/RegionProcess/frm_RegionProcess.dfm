object frmRegionProcess: TfrmRegionProcess
  Left = 234
  Top = 298
  Caption = 'Selection Manager'
  ClientHeight = 450
  ClientWidth = 600
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 580
  ParentFont = True
  PopupMode = pmExplicit
  Position = poMainFormCenter
  ShowHint = True
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 413
    Width = 600
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object btnStart: TButton
      AlignWithMargins = True
      Left = 438
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Start'
      Default = True
      TabOrder = 2
      OnClick = btnStartClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 519
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = btnCancelClick
    end
    object tbxtlbrOperations: TTBXToolbar
      AlignWithMargins = True
      Left = 329
      Top = 6
      Width = 100
      Height = 25
      Margins.Right = 6
      Align = alRight
      Images = frmMain.MenusImageList
      ShrinkMode = tbsmWrap
      TabOrder = 1
      object tbtmCopyBbox: TTBItem
        Hint = 'Copy BBOX coordinates'
        ImageIndex = 28
        OnClick = tbtmCopyBboxClick
      end
      object tbtmSaveToMarksDb: TTBItem
        Hint = 'Store selection as Poligon'
        ImageIndex = 17
        OnClick = tbtmSaveToMarksDbClick
      end
      object tbtmZoom: TTBItem
        Hint = 'Fit to Screen'
        ImageIndex = 43
        OnClick = tbtmZoomClick
      end
      object tbtmSaveToFile: TTBItem
        Hint = 'Save selection info to file'
        ImageIndex = 25
        OnClick = tbtmSaveToFileClick
      end
    end
    object tbxtlbrDontClose: TTBXToolbar
      Left = 4
      Top = 6
      Width = 25
      Height = 24
      Align = alCustom
      Images = frmMain.MenusImageList
      ShrinkMode = tbsmWrap
      TabOrder = 0
      object tbtmDontClose: TTBItem
        AutoCheck = True
        Hint = 'Do not close this window after start'
        ImageIndex = 46
      end
    end
  end
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 413
    Align = alClient
    TabOrder = 1
  end
  object dlgSaveSelection: TSaveDialog
    DefaultExt = '*.hlg'
    Filter = 'Selections|*.hlg'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 32
    Top = 48
  end
end
