object frmJpegImportConfigEdit: TfrmJpegImportConfigEdit
  Left = 245
  Top = 207
  Caption = 'Jpeg Import Parameters'
  ClientHeight = 286
  ClientWidth = 503
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlOptions: TPanel
    Left = 0
    Top = 0
    Width = 503
    Height = 255
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 352
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 255
    Width = 503
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 352
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
