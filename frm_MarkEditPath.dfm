object frmMarkEditPath: TfrmMarkEditPath
  Left = 187
  Top = 189
  Caption = 'Add New Path'
  ClientHeight = 327
  ClientWidth = 328
  Color = clBtnFace
  Constraints.MinHeight = 271
  Constraints.MinWidth = 336
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object chkVisible: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 276
    Width = 322
    Height = 17
    Align = alBottom
    Caption = 'Show on map'
    TabOrder = 4
    ExplicitTop = 279
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 328
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblCategory: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 58
      Height = 19
      Align = alLeft
      Caption = 'Category:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object CBKateg: TComboBox
      AlignWithMargins = True
      Left = 67
      Top = 3
      Width = 258
      Height = 21
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      Text = 'New category'
    end
  end
  object pnlName: TPanel
    Left = 0
    Top = 25
    Width = 328
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblName: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 23
      Height = 21
      Align = alLeft
      Caption = 'Name:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtName: TEdit
      AlignWithMargins = True
      Left = 32
      Top = 3
      Width = 293
      Height = 21
      Align = alClient
      TabOrder = 0
    end
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 52
    Width = 328
    Height = 191
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitHeight = 194
  end
  object flwpnlStyle: TFlowPanel
    Left = 0
    Top = 243
    Width = 328
    Height = 30
    Align = alBottom
    AutoSize = True
    AutoWrap = False
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 246
    object lblLineColor: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 26
      Height = 13
      Align = alLeft
      Caption = 'Color'
      Layout = tlCenter
    end
    object clrbxLineColor: TColorBox
      AlignWithMargins = True
      Left = 35
      Top = 3
      Width = 38
      Height = 22
      Selected = clRed
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object btnLineColor: TSpeedButton
      AlignWithMargins = True
      Left = 79
      Top = 3
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnLineColorClick
    end
    object lblWidth: TLabel
      AlignWithMargins = True
      Left = 102
      Top = 3
      Width = 40
      Height = 13
      Caption = 'Width'
      Layout = tlCenter
    end
    object seWidth: TSpinEdit
      AlignWithMargins = True
      Left = 148
      Top = 3
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 1
      Value = 2
    end
    object lblTransp: TLabel
      AlignWithMargins = True
      Left = 195
      Top = 3
      Width = 85
      Height = 13
      Caption = 'Opacity %'
      Layout = tlCenter
    end
    object SEtransp: TSpinEdit
      AlignWithMargins = True
      Left = 286
      Top = 3
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 35
    end
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 296
    Width = 328
    Height = 31
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 5
    ExplicitTop = 299
    object btnOk: TButton
      AlignWithMargins = True
      Left = 173
      Top = 3
      Width = 73
      Height = 23
      Align = alRight
      Caption = 'Add'
      Default = True
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 252
      Top = 3
      Width = 73
      Height = 23
      Hint = 'Cancel'
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object ColorDialog1: TColorDialog
    Left = 56
    Top = 280
  end
end
