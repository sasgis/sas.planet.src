object frmRegionProcess: TfrmRegionProcess
  Left = 234
  Top = 298
  Caption = 'Selection Manager'
  ClientHeight = 323
  ClientWidth = 572
  Color = clBtnFace
  Constraints.MinHeight = 343
  Constraints.MinWidth = 580
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 572
    Height = 286
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    TabWidth = 92
    object TabSheet1: TTabSheet
      Caption = 'Download'
    end
    object TabSheet2: TTabSheet
      Tag = 1
      Caption = 'Stitch'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabSheet3: TTabSheet
      Tag = 2
      Caption = 'Generate'
      ImageIndex = 2
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabSheet4: TTabSheet
      Tag = 3
      Caption = 'Delete'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabSheet5: TTabSheet
      Tag = 4
      Caption = 'Export'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
    object TabSheet6: TTabSheet
      Tag = 5
      Caption = 'Copy'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
    end
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 286
    Width = 572
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object Button1: TButton
      AlignWithMargins = True
      Left = 410
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Start'
      Default = True
      TabOrder = 0
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
      TabOrder = 1
      OnClick = Button3Click
    end
    object CBCloseWithStart: TCheckBox
      AlignWithMargins = True
      Left = 6
      Top = 6
      Width = 398
      Height = 25
      Align = alClient
      Caption = 'Close this window after start'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object TBXOperationsToolbar: TTBXToolbar
      Left = 329
      Top = 6
      Width = 75
      Height = 24
      Align = alCustom
      Images = frmMain.MenusImageList
      ShrinkMode = tbsmWrap
      TabOrder = 3
      object tbtmSave: TTBItem
        ImageIndex = 17
        OnClick = tbtmSaveClick
        Caption = ''
        Hint = ''
      end
      object tbtmZoom: TTBItem
        ImageIndex = 43
        OnClick = tbtmZoomClick
        Caption = ''
        Hint = ''
      end
      object tbtmMark: TTBItem
        ImageIndex = 25
        OnClick = tbtmMarkClick
        Caption = ''
        Hint = ''
      end
    end
  end
  object SaveSelDialog: TSaveDialog
    DefaultExt = '*.hlg'
    Filter = 'Selections|*.hlg'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 32
    Top = 48
  end
end
