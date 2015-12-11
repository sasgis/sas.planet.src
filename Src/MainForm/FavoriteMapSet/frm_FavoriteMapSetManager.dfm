object frmFavoriteMapSetManager: TfrmFavoriteMapSetManager
  Left = 0
  Top = 0
  Caption = 'Favorites Manager'
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
    object btnOk: TButton
      AlignWithMargins = True
      Left = 554
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
      ExplicitLeft = 473
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
  end
end
