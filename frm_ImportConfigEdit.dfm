object frmImportConfigEdit: TfrmImportConfigEdit
  Left = 245
  Top = 207
  BorderStyle = bsDialog
  Caption = 'Import Parameters'
  ClientHeight = 385
  ClientWidth = 505
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object lblCategory: TLabel
    Left = 8
    Top = 10
    Width = 58
    Height = 13
    Caption = 'Category:'
  end
  object CBKateg: TComboBox
    Left = 72
    Top = 8
    Width = 425
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object grpPoint: TGroupBox
    Left = 8
    Top = 64
    Width = 489
    Height = 81
    Caption = 'Placemark parameters'
    TabOrder = 1
    object lblPointTextColor: TLabel
      Left = 128
      Top = 24
      Width = 64
      Height = 13
      Caption = 'Text color'
    end
    object lblPointShadowColor: TLabel
      Left = 128
      Top = 48
      Width = 53
      Height = 13
      Caption = 'Shadow color'
    end
    object lblPointFontSize: TLabel
      Left = 256
      Top = 24
      Width = 78
      Height = 13
      Caption = 'Font size'
    end
    object lblPointIconSize: TLabel
      Left = 256
      Top = 48
      Width = 74
      Height = 13
      Caption = 'Icon size'
    end
    object lblPointTextTransp: TLabel
      Left = 392
      Top = 24
      Width = 85
      Height = 13
      Caption = 'Opacity %'
    end
    object btnPointTextColor: TSpeedButton
      Left = 230
      Top = 21
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnPointTextColorClick
    end
    object btnPointShadowColor: TSpeedButton
      Left = 230
      Top = 45
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnPointShadowColorClick
    end
    object lblPointIcon: TLabel
      Left = 16
      Top = 32
      Width = 37
      Height = 13
      Caption = 'Icon'
    end
    object clrbxPointTextColor: TColorBox
      Left = 192
      Top = 21
      Width = 38
      Height = 22
      Selected = clYellow
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object sePointFontSize: TSpinEdit
      Left = 344
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 0
      TabOrder = 1
      Value = 11
    end
    object clrbxPointShadowColor: TColorBox
      Left = 192
      Top = 45
      Width = 38
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 2
    end
    object sePointIconSize: TSpinEdit
      Left = 344
      Top = 45
      Width = 41
      Height = 22
      MaxValue = 64
      MinValue = 1
      TabOrder = 3
      Value = 32
    end
    object sePointTextTransp: TSpinEdit
      Left = 432
      Top = 43
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 4
      Value = 35
    end
    object cbbPointIcon: TComboBox
      Left = 64
      Top = 21
      Width = 57
      Height = 44
      Hint = 'Select image'
      Style = csOwnerDrawVariable
      ItemHeight = 38
      ParentShowHint = False
      ShowHint = False
      TabOrder = 5
      OnDrawItem = cbbPointIconDrawItem
    end
  end
  object grpLine: TGroupBox
    Left = 8
    Top = 176
    Width = 489
    Height = 57
    Caption = 'Path parameters'
    TabOrder = 2
    object lblLineColor: TLabel
      Left = 88
      Top = 24
      Width = 26
      Height = 13
      Caption = 'Color'
    end
    object lblLineWidth: TLabel
      Left = 194
      Top = 24
      Width = 40
      Height = 13
      Caption = 'Width'
    end
    object lblLineTransp: TLabel
      Left = 298
      Top = 24
      Width = 85
      Height = 13
      Caption = 'Opacity %'
    end
    object btnLineColor: TSpeedButton
      Left = 155
      Top = 21
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnLineColorClick
    end
    object clrbxLineColor: TColorBox
      Left = 117
      Top = 21
      Width = 38
      Height = 22
      Selected = clRed
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object seLineWidth: TSpinEdit
      Left = 237
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 1
      Value = 2
    end
    object seLineTransp: TSpinEdit
      Left = 384
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 35
    end
  end
  object grpPoly: TGroupBox
    Left = 8
    Top = 264
    Width = 489
    Height = 81
    Caption = 'Polygon parameters'
    TabOrder = 3
    object lblPolyLineColor: TLabel
      Left = 88
      Top = 24
      Width = 26
      Height = 13
      Caption = 'Color'
    end
    object lblPolyLineWidth: TLabel
      Left = 194
      Top = 24
      Width = 40
      Height = 13
      Caption = 'Width'
    end
    object lblPolyLineTransp: TLabel
      Left = 298
      Top = 24
      Width = 85
      Height = 13
      Caption = 'Opacity %'
    end
    object btnPolyLineColor: TSpeedButton
      Left = 155
      Top = 21
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnPolyLineColorClick
    end
    object lblPolyFillColor: TLabel
      Left = 88
      Top = 48
      Width = 26
      Height = 13
      Caption = 'Color'
    end
    object lblPolyFillTransp: TLabel
      Left = 194
      Top = 48
      Width = 85
      Height = 13
      Caption = 'Opacity %'
    end
    object btnPolyFillColor: TSpeedButton
      Left = 155
      Top = 45
      Width = 17
      Height = 22
      Caption = '...'
      OnClick = btnPolyFillColorClick
    end
    object lblPolyLine: TLabel
      Left = 8
      Top = 22
      Width = 35
      Height = 13
      Caption = 'Line:'
    end
    object lblPolyFill: TLabel
      Left = 8
      Top = 46
      Width = 64
      Height = 13
      Caption = 'Fill:'
    end
    object clrbxPolyLineColor: TColorBox
      Left = 117
      Top = 21
      Width = 38
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object sePolyLineWidth: TSpinEdit
      Left = 237
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 1
      Value = 2
    end
    object sePolyLineTransp: TSpinEdit
      Left = 384
      Top = 21
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 2
      Value = 35
    end
    object clrbxPolyFillColor: TColorBox
      Left = 117
      Top = 45
      Width = 38
      Height = 22
      Selected = clWhite
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 3
    end
    object sePolyFillTransp: TSpinEdit
      Left = 280
      Top = 45
      Width = 41
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 4
      Value = 80
    end
  end
  object btnOk: TButton
    Left = 424
    Top = 352
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 4
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 344
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object chkPointIgnore: TCheckBox
    Left = 8
    Top = 40
    Width = 129
    Height = 17
    Caption = 'Ignore placemarks'
    TabOrder = 6
  end
  object chkLineIgnore: TCheckBox
    Left = 8
    Top = 152
    Width = 129
    Height = 17
    Caption = 'Ignore paths'
    TabOrder = 7
  end
  object chkPolyIgnore: TCheckBox
    Left = 8
    Top = 240
    Width = 153
    Height = 17
    Caption = 'Ignore polygons'
    TabOrder = 8
  end
  object ColorDialog1: TColorDialog
    Left = 472
    Top = 32
  end
end
