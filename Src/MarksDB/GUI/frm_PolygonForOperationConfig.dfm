object frmPolygonForOperationConfig: TfrmPolygonForOperationConfig
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Generator Options'
  ClientHeight = 126
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblRadius: TLabel
    Left = 8
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Radius, m'
  end
  object lblShape: TLabel
    Left = 8
    Top = 51
    Width = 30
    Height = 13
    Caption = 'Shape'
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 96
    Width = 318
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 47
    ExplicitWidth = 454
    object btnOk: TButton
      Left = 155
      Top = 3
      Width = 75
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      TabOrder = 0
      OnClick = btnOkClick
      ExplicitLeft = 274
    end
    object btnCancel: TButton
      Left = 236
      Top = 3
      Width = 75
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
      ExplicitLeft = 354
    end
  end
  object cbbShapeType: TComboBox
    Left = 8
    Top = 67
    Width = 302
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object edtRadius: TEdit
    Left = 8
    Top = 24
    Width = 302
    Height = 21
    TabOrder = 2
    Text = '100'
  end
end
