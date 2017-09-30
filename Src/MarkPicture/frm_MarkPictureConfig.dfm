object frmMarkPictureConfig: TfrmMarkPictureConfig
  Left = 0
  Top = 0
  Caption = 'Icons Settings'
  ClientHeight = 387
  ClientWidth = 603
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 495
    Top = 354
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = btnCloseClick
  end
  object pnlCaptions: TPanel
    Left = 0
    Top = 0
    Width = 603
    Height = 18
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object lblIcon: TLabel
      Left = 8
      Top = 5
      Width = 21
      Height = 13
      Caption = 'Icon'
    end
    object lblName: TLabel
      Left = 96
      Top = 5
      Width = 27
      Height = 13
      Caption = 'Name'
    end
    object lblAnchor: TLabel
      Left = 533
      Top = 5
      Width = 34
      Height = 13
      Align = alCustom
      Anchors = [akTop, akRight]
      BiDiMode = bdRightToLeft
      Caption = 'Anchor'
      ParentBiDiMode = False
    end
  end
  object lstItems: TListBox
    Left = 0
    Top = 24
    Width = 603
    Height = 324
    Style = lbOwnerDrawFixed
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 32
    TabOrder = 0
    OnDblClick = lstItemsDblClick
    OnDrawItem = lstItemsDrawItem
    OnKeyDown = lstItemsKeyDown
  end
  object btnEdit: TButton
    Left = 8
    Top = 354
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akLeft, akBottom]
    Caption = 'Edit'
    TabOrder = 2
    OnClick = btnEditClick
  end
end
