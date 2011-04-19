object frmShortCutEdit: TfrmShortCutEdit
  Left = 669
  Top = 266
  BorderStyle = bsDialog
  Caption = 'Edit hotkey'
  ClientHeight = 81
  ClientWidth = 189
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poDesktopCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 183
    Height = 44
    Align = alClient
    Caption = 'Define hotkey'
    TabOrder = 0
    ExplicitLeft = 8
    object HotKey: THotKey
      AlignWithMargins = True
      Left = 8
      Top = 16
      Width = 167
      Height = 19
      Modifiers = []
      TabOrder = 0
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 50
    Width = 189
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 172
    ExplicitWidth = 201
    object btnClear: TSpeedButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 25
      Align = alLeft
      Caption = 'No'
      Flat = True
      OnClick = btnClearClick
      ExplicitLeft = 8
      ExplicitTop = 6
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 121
      Top = 3
      Width = 65
      Height = 25
      Align = alRight
      Caption = 'Apply'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 128
      ExplicitTop = 16
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 50
      Top = 3
      Width = 65
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 56
      ExplicitTop = 6
    end
  end
end
