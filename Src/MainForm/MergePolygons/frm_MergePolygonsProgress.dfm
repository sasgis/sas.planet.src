object frmMergePolygonsProgress: TfrmMergePolygonsProgress
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Please wait...'
  ClientHeight = 42
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lblProgress: TLabel
    Left = 8
    Top = 14
    Width = 3
    Height = 13
  end
  object btnAbort: TButton
    Left = 216
    Top = 8
    Width = 101
    Height = 25
    Align = alCustom
    Anchors = [akTop, akRight]
    Caption = 'Abort'
    TabOrder = 0
    OnClick = btnAbortClick
  end
end
