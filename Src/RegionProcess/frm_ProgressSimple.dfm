object frmProgressSimple: TfrmProgressSimple
  Left = 207
  Top = 161
  BorderStyle = bsToolWindow
  BorderWidth = 3
  Caption = 'Please wait...'
  ClientHeight = 44
  ClientWidth = 319
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object MemoInfo: TMemo
    Left = 0
    Top = 0
    Width = 244
    Height = 28
    Align = alClient
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      '')
    ReadOnly = True
    TabOrder = 0
    OnChange = MemoInfoChange
  end
  object pnlProgress: TPanel
    Left = 0
    Top = 28
    Width = 319
    Height = 16
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
  end
  object TBXOperationsToolbar: TTBXToolbar
    Left = 244
    Top = 0
    Width = 75
    Height = 28
    Align = alRight
    Images = frmMain.MenusImageList
    ShrinkMode = tbsmWrap
    TabOrder = 1
    object tbtmSelect: TTBItem
      Hint = 'Selection Manager'
      ImageIndex = 44
      OnClick = tbtmSelectClick
    end
    object tbtmZoom: TTBItem
      Hint = 'Fit to Screen'
      ImageIndex = 43
      OnClick = tbtmZoomClick
    end
    object tbtmDontClose: TTBItem
      AutoCheck = True
      Hint = 'Do not close this window when finish'
      ImageIndex = 46
    end
  end
end
