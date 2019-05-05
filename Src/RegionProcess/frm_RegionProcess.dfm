object frmRegionProcess: TfrmRegionProcess
  Left = 234
  Top = 298
  Caption = 'Selection Manager'
  ClientHeight = 426
  ClientWidth = 572
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 580
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 389
    Width = 572
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object Button1: TButton
      AlignWithMargins = True
      Left = 410
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Start'
      Default = True
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button3: TButton
      AlignWithMargins = True
      Left = 491
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = Button3Click
    end
    object TBXOperationsToolbar: TTBXToolbar
      Left = 304
      Top = 6
      Width = 100
      Height = 24
      Align = alCustom
      Images = frmMain.MenusImageList
      ShrinkMode = tbsmWrap
      TabOrder = 1
      object tbtmCopyBbox: TTBItem
        ImageIndex = 28
        OnClick = tbtmCopyBboxClick
        Caption = ''
        Hint = 'Copy BBOX coordinates'
      end
      object tbtmSave: TTBItem
        ImageIndex = 17
        OnClick = tbtmSaveClick
        Caption = ''
        Hint = 'Store selection as Poligon'
      end
      object tbtmZoom: TTBItem
        ImageIndex = 43
        OnClick = tbtmZoomClick
        Caption = ''
        Hint = 'Fit to Screen'
      end
      object tbtmMark: TTBItem
        ImageIndex = 25
        OnClick = tbtmMarkClick
        Caption = ''
        Hint = 'Save selection info to file'
      end
    end
    object TBXDontClose: TTBXToolbar
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
        ImageIndex = 46
        Caption = ''
        Hint = 'Do not close this window after start'
      end
    end
  end
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 572
    Height = 389
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 192
    ExplicitTop = 128
    ExplicitWidth = 185
    ExplicitHeight = 41
  end
  object SaveSelDialog: TSaveDialog
    DefaultExt = '*.hlg'
    Filter = 'Selections|*.hlg'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 32
    Top = 48
  end
end
