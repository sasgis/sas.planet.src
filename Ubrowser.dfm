object Fbrowser: TFbrowser
  Left = 517
  Top = 199
  Width = 566
  Height = 334
  BorderStyle = bsSizeToolWin
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object EmbeddedWB1: TEmbeddedWB
    Left = 0
    Top = 0
    Width = 558
    Height = 300
    Align = alClient
    TabOrder = 0
    DownloadOptions = [DownloadImages, DownloadVideos, DownloadBGSounds]
    UserInterfaceOptions = []
    About = ' Embedded Web Browser from: http://bsalsa.com/'
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    UserAgent = ' Embedded Web Browser from: http://bsalsa.com/'
    ControlData = {
      4C000000D8380000DE2000000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
