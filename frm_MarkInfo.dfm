object frmMarkInfo: TfrmMarkInfo
  Left = 0
  Top = 0
  Caption = 'frmMarkInfo'
  ClientHeight = 270
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object mmoInfo: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 337
    Height = 234
    Align = alClient
    ReadOnly = True
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 240
    Width = 343
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnClose: TButton
      AlignWithMargins = True
      Left = 265
      Top = 3
      Width = 75
      Height = 24
      Align = alRight
      Cancel = True
      Caption = 'Close'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
