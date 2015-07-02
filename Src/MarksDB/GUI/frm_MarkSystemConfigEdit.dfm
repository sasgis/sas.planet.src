object frmMarkSystemConfigEdit: TfrmMarkSystemConfigEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add Marks Database'
  ClientHeight = 296
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object grpDBType: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 357
    Height = 50
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Database type'
    TabOrder = 0
    ExplicitWidth = 347
    object cbbDbType: TComboBox
      AlignWithMargins = True
      Left = 12
      Top = 18
      Width = 333
      Height = 21
      Margins.Left = 10
      Margins.Right = 10
      Align = alClient
      TabOrder = 0
      OnChange = cbbDbTypeChange
      ExplicitLeft = 10
      ExplicitTop = 16
      ExplicitWidth = 286
    end
  end
  object grpFile: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 66
    Width = 357
    Height = 48
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'File name'
    TabOrder = 1
    ExplicitTop = 73
    object edtFileName: TEdit
      Left = 12
      Top = 16
      Width = 309
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnOpenFile: TButton
      Left = 327
      Top = 16
      Width = 21
      Height = 21
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnOpenFileClick
    end
  end
  object grpDisplayName: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 120
    Width = 357
    Height = 48
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Name (on the list)'
    TabOrder = 2
    ExplicitLeft = 18
    ExplicitTop = 74
    object edtDisplayName: TEdit
      Left = 12
      Top = 16
      Width = 333
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object grpOptions: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 174
    Width = 357
    Height = 83
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Options'
    TabOrder = 3
    object lblUserName: TLabel
      Left = 12
      Top = 16
      Width = 333
      Height = 13
      Caption = 'User Name:'
    end
    object edtUserName: TEdit
      Left = 12
      Top = 35
      Width = 333
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object chkReadOnly: TCheckBox
      Left = 12
      Top = 62
      Width = 333
      Height = 17
      Caption = 'Read-Only'
      TabOrder = 1
    end
  end
  object btnOk: TButton
    Left = 163
    Top = 263
    Width = 100
    Height = 25
    Caption = 'Apply'
    TabOrder = 4
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 269
    Top = 263
    Width = 100
    Height = 25
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object dlgOpenDb: TOpenDialog
    Left = 16
    Top = 264
  end
end
