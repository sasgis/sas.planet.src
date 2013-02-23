object frmMarkInfo: TfrmMarkInfo
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Mark Info'
  ClientHeight = 333
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    ExplicitTop = 8
  end
  object embdwbDesc: TEmbeddedWB
    Left = 0
    Top = 98
    Width = 401
    Height = 235
    Align = alClient
    TabOrder = 1
    Silent = False
    DisableCtrlShortcuts = 'N'
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    About = ' EmbeddedWB http://bsalsa.com/'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    ExplicitLeft = 72
    ExplicitTop = 128
    ExplicitHeight = 181
    ControlData = {
      4C000000722900004A1800000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
