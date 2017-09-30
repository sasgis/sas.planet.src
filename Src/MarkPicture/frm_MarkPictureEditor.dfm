object frmMarkPictureEditor: TfrmMarkPictureEditor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Icon Editor'
  ClientHeight = 303
  ClientWidth = 499
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblMousePosition: TLabel
    Left = 10
    Top = 273
    Width = 3
    Height = 13
  end
  object imgIcon: TImage32
    Left = 8
    Top = 8
    Width = 256
    Height = 256
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    RepaintMode = rmOptimizer
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
    OnMouseDown = imgIconMouseDown
    OnMouseMove = imgIconMouseMove
    OnMouseLeave = imgIconMouseLeave
  end
  object btnApply: TButton
    Left = 285
    Top = 270
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    TabOrder = 1
    OnClick = btnApplyClick
  end
  object btnCancel: TButton
    Left = 391
    Top = 270
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object grpAnchor: TGroupBox
    Left = 270
    Top = 8
    Width = 221
    Height = 256
    Caption = 'Anchor'
    TabOrder = 3
    object lblAnchorX: TLabel
      Left = 15
      Top = 52
      Width = 10
      Height = 13
      Caption = 'X:'
    end
    object lblAnchorY: TLabel
      Left = 15
      Top = 101
      Width = 10
      Height = 13
      Caption = 'Y:'
    end
    object cbbAnchorType: TComboBox
      Left = 12
      Top = 24
      Width = 197
      Height = 21
      Style = csDropDownList
      DropDownCount = 11
      TabOrder = 0
      OnChange = cbbAnchorTypeChange
    end
    object seAnchorX: TSpinEdit
      Left = 12
      Top = 72
      Width = 197
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 0
      OnChange = seAnchorXYChange
    end
    object seAnchorY: TSpinEdit
      Left = 12
      Top = 121
      Width = 197
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 2
      Value = 0
      OnChange = seAnchorXYChange
    end
  end
end
