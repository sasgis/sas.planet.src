object frmMarkSystemConfigEdit: TfrmMarkSystemConfigEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Add Marks Database'
  ClientHeight = 294
  ClientWidth = 402
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 410
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
    Width = 382
    Height = 50
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Database type'
    TabOrder = 0
    ExplicitWidth = 390
    object cbbDbType: TComboBox
      AlignWithMargins = True
      Left = 12
      Top = 18
      Width = 358
      Height = 21
      Margins.Left = 10
      Margins.Right = 10
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbDbTypeChange
      ExplicitWidth = 366
    end
  end
  object grpFile: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 66
    Width = 382
    Height = 48
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'File name'
    TabOrder = 1
    ExplicitWidth = 390
    object edtFileName: TEdit
      Left = 12
      Top = 16
      Width = 334
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 342
    end
    object btnOpenFile: TButton
      Left = 352
      Top = 16
      Width = 21
      Height = 21
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnOpenFileClick
      ExplicitLeft = 360
    end
  end
  object grpDisplayName: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 120
    Width = 382
    Height = 48
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Name (on the list)'
    TabOrder = 2
    ExplicitWidth = 390
    object edtDisplayName: TEdit
      Left = 12
      Top = 16
      Width = 358
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 366
    end
  end
  object grpOptions: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 174
    Width = 382
    Height = 83
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    TabOrder = 3
    ExplicitWidth = 390
    object lblUserName: TLabel
      Left = 12
      Top = 10
      Width = 56
      Height = 13
      Caption = 'User Name:'
    end
    object lblPass: TLabel
      Left = 190
      Top = 10
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object edtUserName: TEdit
      Left = 12
      Top = 29
      Width = 162
      Height = 21
      TabOrder = 0
    end
    object chkReadOnly: TCheckBox
      Left = 12
      Top = 56
      Width = 358
      Height = 17
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Read-Only'
      TabOrder = 1
      ExplicitWidth = 366
    end
    object edtPass: TEdit
      Left = 190
      Top = 29
      Width = 162
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
    object chkShowPass: TCheckBox
      Left = 358
      Top = 30
      Width = 17
      Height = 17
      Hint = 'Show/Hide Password'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = chkShowPassClick
    end
  end
  object btnOk: TButton
    Left = 188
    Top = 263
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    TabOrder = 4
    OnClick = btnOkClick
    ExplicitLeft = 186
  end
  object btnCancel: TButton
    Left = 294
    Top = 263
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
    ExplicitLeft = 292
  end
  object dlgOpenDb: TOpenDialog
    Left = 16
    Top = 264
  end
end
