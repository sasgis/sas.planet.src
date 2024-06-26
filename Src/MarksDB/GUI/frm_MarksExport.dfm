object frmMarksExport: TfrmMarksExport
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  ClientHeight = 170
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    422
    170)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFormat: TLabel
    Left = 8
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Format:'
  end
  object lblDest: TLabel
    Left = 8
    Top = 54
    Width = 41
    Height = 13
    Caption = 'Save to:'
  end
  object cbbFormat: TComboBox
    Left = 8
    Top = 27
    Width = 406
    Height = 21
    Align = alCustom
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = cbbFormatChange
  end
  object edtDest: TEdit
    Left = 8
    Top = 73
    Width = 375
    Height = 21
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 1
  end
  object btnDest: TButton
    Left = 389
    Top = 73
    Width = 25
    Height = 21
    Align = alCustom
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 2
    OnClick = btnDestClick
  end
  object chkFilePerMark: TCheckBox
    Left = 8
    Top = 105
    Width = 406
    Height = 17
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Separate file for each placemark'
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 339
    Top = 137
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = btnCancelClick
  end
  object btnRun: TButton
    Left = 258
    Top = 137
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Run'
    TabOrder = 5
    OnClick = btnRunClick
  end
  object btnConfig: TTBXButton
    Left = 8
    Top = 136
    Width = 26
    Height = 26
    Hint = 'Format settings'
    Anchors = [akLeft, akBottom]
    ButtonStyle = bsFlat
    ImageIndex = 20
    Images = frmMain.MenusImageList
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    OnClick = btnConfigClick
  end
  object dlgSave: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 304
    Top = 56
  end
end
