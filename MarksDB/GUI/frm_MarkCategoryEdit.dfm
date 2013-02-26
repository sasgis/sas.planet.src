object frmMarkCategoryEdit: TfrmMarkCategoryEdit
  Left = 208
  Top = 318
  BorderStyle = bsDialog
  Caption = 'Add New Category'
  ClientHeight = 140
  ClientWidth = 295
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 30
    Width = 289
    Height = 13
    Align = alTop
    Caption = 'Visible on zooms:'
  end
  object Bevel5: TBevel
    Left = 0
    Top = 100
    Width = 295
    Height = 9
    Align = alBottom
    Shape = bsBottomLine
  end
  object CBShow: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 80
    Width = 289
    Height = 17
    Align = alBottom
    Caption = 'Show on map'
    TabOrder = 0
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 109
    Width = 295
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 219
      Top = 3
      Width = 73
      Height = 25
      Hint = 'Cancel'
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 140
      Top = 3
      Width = 73
      Height = 25
      Align = alRight
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
    object btnSetAsTemplate: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 23
      Height = 25
      Hint = 'Set as default'
      Align = alLeft
      Caption = '~'
      Default = True
      TabOrder = 2
      OnClick = btnSetAsTemplateClick
    end
  end
  object flwpnlZooms: TFlowPanel
    Left = 0
    Top = 43
    Width = 295
    Height = 34
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Label3: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 22
      Height = 13
      Caption = 'from'
    end
    object EditS1: TSpinEdit
      AlignWithMargins = True
      Left = 31
      Top = 3
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 0
      Value = 3
    end
    object Label4: TLabel
      AlignWithMargins = True
      Left = 78
      Top = 3
      Width = 10
      Height = 13
      Caption = 'to'
    end
    object EditS2: TSpinEdit
      AlignWithMargins = True
      Left = 94
      Top = 3
      Width = 41
      Height = 22
      MaxValue = 24
      MinValue = 1
      TabOrder = 1
      Value = 18
    end
  end
  object pnlName: TPanel
    Left = 0
    Top = 0
    Width = 295
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 27
      Height = 13
      Align = alLeft
      Caption = 'Name'
      Layout = tlCenter
    end
    object EditName: TEdit
      AlignWithMargins = True
      Left = 36
      Top = 3
      Width = 256
      Height = 21
      Align = alClient
      TabOrder = 0
    end
  end
end
