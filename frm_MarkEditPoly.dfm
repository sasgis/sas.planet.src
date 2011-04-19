object frmMarkEditPoly: TfrmMarkEditPoly
  Left = 360
  Top = 40
  Caption = 'Add new polygon'
  ClientHeight = 348
  ClientWidth = 327
  Color = clBtnFace
  Constraints.MinHeight = 375
  Constraints.MinWidth = 335
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
    Top = 297
    Width = 321
    Height = 17
    Align = alBottom
    Caption = 'Show on map'
    TabOrder = 5
    ExplicitTop = 300
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 317
    Width = 327
    Height = 31
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 6
    ExplicitTop = 320
    object btnOk: TButton
      AlignWithMargins = True
      Left = 172
      Top = 3
      Width = 73
      Height = 23
      Align = alRight
      Caption = 'Add'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 251
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
  object pnlFill: TPanel
    Left = 0
    Top = 245
    Width = 327
    Height = 49
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitTop = 248
    object lblFill: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 321
      Height = 13
      Align = alTop
      Caption = 'Completion:'
      ExplicitWidth = 64
    end
    object flwpnlFill: TFlowPanel
      Left = 0
      Top = 19
      Width = 327
      Height = 30
      Align = alTop
      AutoSize = True
      AutoWrap = False
      BevelEdges = [beBottom]
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
      object lblFillColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 26
        Height = 13
        Caption = 'Color'
      end
      object clrbxFillColor: TColorBox
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 38
        Height = 22
        Selected = clWhite
        Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
        ItemHeight = 16
        TabOrder = 0
      end
      object btnFillColor: TSpeedButton
        AlignWithMargins = True
        Left = 79
        Top = 3
        Width = 17
        Height = 22
        Caption = '...'
        OnClick = btnFillColorClick
      end
      object lblFillTransp: TLabel
        AlignWithMargins = True
        Left = 102
        Top = 3
        Width = 85
        Height = 13
        Caption = 'Opacity %'
      end
      object seFillTransp: TSpinEdit
        AlignWithMargins = True
        Left = 193
        Top = 3
        Width = 41
        Height = 22
        MaxValue = 100
        MinValue = 0
        TabOrder = 1
        Value = 80
      end
    end
  end
  object pnlLine: TPanel
    Left = 0
    Top = 198
    Width = 327
    Height = 47
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 201
    object lblLine: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 321
      Height = 13
      Align = alTop
      Caption = 'Line:'
      ExplicitWidth = 35
    end
    object flwpnlLine: TFlowPanel
      Left = 0
      Top = 19
      Width = 327
      Height = 28
      Align = alTop
      AutoSize = True
      AutoWrap = False
      BevelOuter = bvNone
      TabOrder = 0
      object lblLineColor: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 26
        Height = 13
        Caption = 'Color'
      end
      object clrbxLineColor: TColorBox
        AlignWithMargins = True
        Left = 35
        Top = 3
        Width = 38
        Height = 22
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
      object lblLineWidth: TLabel
        AlignWithMargins = True
        Left = 102
        Top = 3
        Width = 40
        Height = 13
        Caption = 'Width'
      end
      object seLineWidth: TSpinEdit
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
      object lblLineTransp: TLabel
        AlignWithMargins = True
        Left = 195
        Top = 3
        Width = 85
        Height = 13
        Caption = 'Opacity %'
      end
      object seLineTransp: TSpinEdit
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
  end
  object pnlDescription: TPanel
    Left = 0
    Top = 52
    Width = 327
    Height = 146
    Align = alClient
    BevelEdges = [beTop, beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitHeight = 149
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 327
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
      ExplicitHeight = 13
    end
    object CBKateg: TComboBox
      AlignWithMargins = True
      Left = 67
      Top = 3
      Width = 257
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
    Width = 327
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
      ExplicitHeight = 13
    end
    object edtName: TEdit
      AlignWithMargins = True
      Left = 32
      Top = 3
      Width = 292
      Height = 21
      Align = alClient
      TabOrder = 0
    end
  end
  object ColorDialog1: TColorDialog
    Left = 88
    Top = 264
  end
end
