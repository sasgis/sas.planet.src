object frmProgressSimple: TfrmProgressSimple
  Left = 207
  Top = 161
  BorderStyle = bsToolWindow
  BorderWidth = 3
  Caption = 'Please wait...'
  ClientHeight = 43
  ClientWidth = 323
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object MemoInfo: TMemo
    Left = 0
    Top = 0
    Width = 323
    Height = 26
    Align = alClient
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      '')
    ReadOnly = True
    TabOrder = 0
    OnChange = MemoInfoChange
  end
  object pnlProgress: TPanel
    Left = 0
    Top = 26
    Width = 323
    Height = 17
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
  end
end
