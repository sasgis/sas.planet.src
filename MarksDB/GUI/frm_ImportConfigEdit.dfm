object frmImportConfigEdit: TfrmImportConfigEdit
  Left = 245
  Top = 207
  Caption = 'Import Parameters'
  ClientHeight = 383
  ClientWidth = 503
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMarksGeneralOptions: TPanel
    Left = 0
    Top = 0
    Width = 503
    Height = 352
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 352
    Width = 503
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 425
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 344
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Start'
      TabOrder = 1
      OnClick = btnOkClick
    end
  end
end
