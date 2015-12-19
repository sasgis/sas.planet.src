object frmMarkInfo: TfrmMarkInfo
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Mark Info'
  ClientHeight = 333
  ClientWidth = 401
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object splDesc: TSplitter
    Left = 0
    Top = 94
    Width = 401
    Height = 4
    Cursor = crVSplit
    Align = alTop
  end
  object mmoInfo: TMemo
    Left = 0
    Top = 0
    Width = 401
    Height = 94
    Align = alTop
    ReadOnly = True
    TabOrder = 0
  end
  object embdwbDesc: TEmbeddedWB
    Left = 0
    Top = 98
    Width = 401
    Height = 235
    Align = alClient
    TabOrder = 1
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    ControlData = {
      4C000000722900004A1800000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
