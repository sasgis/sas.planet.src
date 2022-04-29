object frmElevationMetaWriterProgress: TfrmElevationMetaWriterProgress
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Please wait...'
  ClientHeight = 43
  ClientWidth = 347
  Color = clBtnFace
  CustomTitleBar.CaptionAlignment = taCenter
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesktopCenter
  OnClose = FormClose
  OnHide = FormHide
  TextHeight = 13
  object lblProgress: TLabel
    Left = 8
    Top = 14
    Width = 3
    Height = 13
  end
  object btnCancel: TButton
    Left = 246
    Top = 10
    Width = 93
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
    ExplicitLeft = 224
    ExplicitTop = 35
  end
  object tmrProgress: TTimer
    Enabled = False
    Interval = 150
    Left = 168
    Top = 8
  end
end
