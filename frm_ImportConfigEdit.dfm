object frmImportConfigEdit: TfrmImportConfigEdit
  Left = 245
  Top = 207
  BorderStyle = bsDialog
  Caption = 'Import Parameters'
  ClientHeight = 385
  ClientWidth = 505
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOk: TButton
    Left = 341
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 422
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
  end
  object pnlMarksGeneralOptions: TPanel
    Left = 0
    Top = 26
    Width = 505
    Height = 320
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
  end
end
