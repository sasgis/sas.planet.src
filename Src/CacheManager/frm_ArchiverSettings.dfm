object frmArchiverSettings: TfrmArchiverSettings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Archiver Settings'
  ClientHeight = 148
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 13
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 338
    Height = 111
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlBottom: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 117
    Width = 338
    Height = 28
    Margins.Top = 0
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnApply: TButton
      Left = 176
      Top = 3
      Width = 75
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Caption = 'Apply'
      TabOrder = 0
      OnClick = btnApplyClick
    end
    object btnCancel: TButton
      Left = 255
      Top = 3
      Width = 75
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
