object frmCopyBboxTmplEditor: TfrmCopyBboxTmplEditor
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Edit templates'
  ClientHeight = 256
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 219
    Width = 449
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object btnApply: TButton
      AlignWithMargins = True
      Left = 287
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Apply'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnApplyClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 368
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object mmoTmpl: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 443
    Height = 213
    Align = alClient
    TabOrder = 0
  end
end
