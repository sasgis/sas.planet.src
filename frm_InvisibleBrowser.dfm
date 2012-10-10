object frmInvisibleBrowser: TfrmInvisibleBrowser
  Left = 0
  Top = 0
  ClientHeight = 509
  ClientWidth = 845
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object WebBrowser1: TEmbeddedWB
    Left = 0
    Top = 0
    Width = 845
    Height = 509
    Align = alClient
    TabOrder = 0
    DisableCtrlShortcuts = 'N'
    DownloadOptions = [DownloadImages, DownloadVideos]
    UserInterfaceOptions = [EnablesFormsAutoComplete, EnableThemes]
    OnAuthenticate = WebBrowser1Authenticate
    About = ' EmbeddedWB http://bsalsa.com/'
    EnableMessageHandler = False
    DisableErrors.EnableDDE = False
    DisableErrors.fpExceptions = False
    DisableErrors.ScriptErrorsSuppressed = False
    DialogBoxes.ReplaceCaption = False
    DialogBoxes.ReplaceIcon = False
    PrintOptions.Margins.Left = 19.050000000000000000
    PrintOptions.Margins.Right = 19.050000000000000000
    PrintOptions.Margins.Top = 19.050000000000000000
    PrintOptions.Margins.Bottom = 19.050000000000000000
    PrintOptions.HTMLHeader.Strings = (
      '<HTML></HTML>')
    PrintOptions.Orientation = poPortrait
    UserAgent = 
      'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.' +
      '50727)'
    ControlData = {
      4C000000A4100000550D00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
end
