object frmFavoriteMapSetManager: TfrmFavoriteMapSetManager
  Left = 0
  Top = 0
  Caption = 'Favorite Map Set Manager'
  ClientHeight = 492
  ClientWidth = 632
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 461
    Width = 632
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 609
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 554
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = btnCancelClick
      ExplicitLeft = 531
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 473
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = btnOkClick
      ExplicitLeft = 450
    end
  end
  object pnlMapSets: TPanel
    Left = 0
    Top = 0
    Width = 632
    Height = 461
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 609
  end
end
